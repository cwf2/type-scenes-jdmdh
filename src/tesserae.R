# required packages

library("XML")
library("stringi")
library("data.table")
library("parallel")

# constants

# a set of useful regular expressions
tesre <- new.env()
  tesre$nonword <- "\\W+"
  tesre$word <- "\\w+"
  tesre$pbound <- "([:;\\.!\\?]+[”\"]?\\s*)"
  tesre$notseen <- "::::"

# functions

standardize <- function(s) {
  # standardize latin orthography

  s <- stri_trans_nfkd(s)
  s <- stri_trans_tolower(s)
  s <- gsub("[^[:alpha:]]+", "", s)
  s <- chartr("jv", "iu", s)
  return(s)
}

parse.phrase <- function(text) {
  # process a single phrase

  wtokens <- unlist(strsplit(x=text, split=tesre$nonword))
  ptokens <- unlist(strsplit(x=text, split=tesre$word))
  if (wtokens[1] == "") {
    wtokens <- wtokens[-1]
  }
  finalp <- ""
  if (length(wtokens) < length(ptokens)) {
    finalp <- ptokens[length(ptokens)]
    ptokens <- ptokens[-length(ptokens)]
  }

  tokens <- c(rbind(ptokens, wtokens), finalp)
  types <- c(rep(c("P", "W"), length(wtokens)), "P")
  not.empty <- tokens != "" & ! is.na(tokens)

  data.table(
    display = tokens[not.empty],
    form = standardize(tokens[not.empty]),
    type = types[not.empty]
  )
}


parse.unit <- function(node) {
  # parse a single TextUnit xml node

  unitid <- as.integer(xmlGetAttr(node, "id")) + 1
  textunit <- xmlValue(node)

  delimited <- gsub(tesre$pbound, paste("\\1", tesre$notseen, sep=""), textunit, perl=T)
  phrases <- unlist(strsplit(delimited, tesre$notseen))

  rbindlist(
    lapply(phrases, function(p) {
      parse.phrase(p)[, unitid := unitid][]
    })
  )
}


ingest.text <- function(file) {
  # process a whole text

  # create an environment (i.e. pseudo-object)
  # to hold this text and its features
  text.object <- new.env()

  # store the original file name
  assign("file", file, envir=text.object)

  # read the xml
  cat("Reading", file, "\n")
  xml <- xmlParse(file=file)
  units <- getNodeSet(xml, "//TextUnit")

  # get the loci
  loc <- mclapply(units, function(node) {xmlGetAttr(node, "loc")}, mc.cores=ncores)
  assign("loc", loc, envir=text.object)

  # get the text
  token.table <- rbindlist(mclapply(units, parse.unit, mc.cores=ncores))

  token.table[, `:=`(
    phraseid = cumsum(c(
      1,
      stri_detect_regex(token.table[2:nrow(token.table), display], tesre$pbound)
    )),
    tokenid = .I,
    type = factor(type, levels = c("W", "P"))
  )]

  setkey(token.table, tokenid)

  assign("tokens", token.table, envir=text.object)
  return(text.object)
}


add.column <- function(text) {
  # build index for a text

  forms <- unique(text$tokens[type=="W", form])

  assign("index.form", new.env(hash=T, size=length(forms)), envir=text)

  f <- function(form, tokenid) {
    assign(form, c(tokenid), envir=text$index.form)
  }

  text$tokens[type=="W", f(form, tokenid), by=form, ]

  # calculate frequencies
  assign("freq.form", feature.frequencies(text$index.form), envir=text)
}


feat <- function(form, feature) {
  # return indexable features for a given form

  switch(class(feature),
    `environment` = mget(form, envir=feature, ifnotfound=list(identity)),
    `function` = feature(form)
  )
}


freq <- Vectorize(function(form, freq) {
  # look up frequency for a given form in a table

  return(get(form, envir=freq))
}, vectorize.args="form")


add.col.feature <- function(text, feat.name, feat.dict) {
  # index a text for a given featureset

  cat("Extracting features\n")

  feat.to.form <- data.table(
    form = ls(text$index.form)
  )[,
    feat := feat(form, feat.dict)
  ][,
    .(feat=unlist(feat)), by=form
  ][,
    .(form=list(form)), by=feat
  ]

  cat("Indexing\n")

  index.feature <- new.env(hash=T, size=nrow(feat.to.form))
  assign(paste("index", feat.name, sep="."), index.feature, envir=text)

  assignment.function <- Vectorize(function(feat, form) {
    if ( nchar(feat) < 1) { cat ("zero-length:", form, "\n")}

    assign(feat,
           value = unlist(unname(mget(form, envir=text$index.form))),
           envir = index.feature
    )
  })

  do.call(assignment.function, feat.to.form)

  # frequencies
  assign(paste("freq", feat.name, sep="."), feature.frequencies(index.feature), envir=text)
}


feature.frequencies <- function(index.feature) {
  # calculate frequencies for a feature

  features <- ls(index.feature)
  freq <- new.env(hash=T, size=length(features))

  cat ("Calculating", length(features), "feature tallies\n")
  total <- sum(
    sapply(features, function(f) {
      assign(f, length(get(f, envir=index.feature)), envir=freq)
    })
  )

  cat ("Converting to frequencies\n")
  for (f in features) {
    assign(f, get(f, envir=freq)/total, envir=freq)
  }

  assign(".__TOTAL__", total, envir=freq)

  return(freq)
}


feature.stoplist <- function(freq.list, n) {
  # generate a stoplist based on feature frequency table

  feat.uniq <- unique(unlist(lapply(freq.list, ls)))
  mean.freq <- new.env(hash=T, size=length(feat.uniq))
  for (f in feat.uniq) {
    assign(f, 0, envir=mean.freq)
  }

  for (freq in freq.list) {
    for (f in ls(freq)) {
      freq.working <- get(f, envir=mean.freq)
      freq.this <- get(f, envir=freq)
      assign(f, freq.working + freq.this/length(freq.list), envir=mean.freq)
    }
  }

  sorted <- sort(sapply(ls(mean.freq), get, envir=mean.freq), decreasing=T)

  stoplist <- character(0)

  if (n>0) {
    n = min(n, length(sorted))
    stoplist <- names(sorted[1:n])
  }

  return(stoplist)
}


score <- function(s.tokenid, t.tokenid, s.text, t.text, pb=NA){
  # generate a score for a given match
  #  assumes tokenid is key on the two token tables

  if (class(pb) == "txtProgressBar") {
    on.exit(setTxtProgressBar(pb, getTxtProgressBar(pb)+1))
  }

  s.tokenid <- unique(s.tokenid)
  t.tokenid <- unique(t.tokenid)

  s.invfreq <- sapply(s.tokenid, function(tokenid) {
    1/freq(s.text$tokens[tokenid, form], s.text$freq.form)
  })
  t.invfreq <- sapply(t.tokenid, function(tokenid) {
    1/freq(t.text$tokens[tokenid, form], t.text$freq.form)
  })

  s.endpoints <- s.tokenid[order(s.invfreq, decreasing=T)][1:2]
  t.endpoints <- t.tokenid[order(t.invfreq, decreasing=T)][1:2]

  s.dist <- sum(s.text$tokens[seq(s.endpoints[1], s.endpoints[2]), type]=="W")
  t.dist <- sum(t.text$tokens[seq(t.endpoints[1], t.endpoints[2]), type]=="W")

  log(sum(s.invfreq, t.invfreq)/(s.dist + t.dist))
}


tess.search <- function(s.text, t.text,
  feat.name="form", stop=10, ncores=2) {
  # perform a comparison of two previously-indexed texts

  cat("Source:", s.text$file, "\n")
  cat("Target:", t.text$file, "\n")
  cat("Feature:", feat.name, "\n")

  s.feat <- get(paste("index", feat.name, sep="."), envir=s.text)
  t.feat <- get(paste("index", feat.name, sep="."), envir=t.text)

  s.freq <- get(paste("freq", feat.name, sep="."), envir=s.text)
  t.freq <- get(paste("freq", feat.name, sep="."), envir=t.text)

  stoplist <- feature.stoplist(list(s.freq, t.freq), n=stop)
  cat("Stoplist:", paste(stoplist, collapse=" "), "\n")

  features <- setdiff(intersect(ls(s.feat), ls(t.feat)), stoplist)

  cat("Generating links\n")
  cat("Ncores:", ncores, "\n")

  result <- rbindlist(
    mclapply(features, function(feature) {
      expand.grid(
        s.tokenid=get(feature, envir=s.feat),
        t.tokenid=get(feature, envir=t.feat),
        feature=feature
      )
    }, mc.cores=ncores)
  )

  cat("Appending unit ids\n")
  result <- cbind(result,
    s.unitid=s.text$tokens[result$s.tokenid, unitid],
    t.unitid=t.text$tokens[result$t.tokenid, unitid]
  )

  cat ("Checking minimal match criteria:\n")
  # for each pair of phrases, make sure at least two source tokens
  cat (" [1/3] at least 2 source tokens\n")
  setkey(result, s.unitid, t.unitid, s.tokenid)
  m <- result[! duplicated(result), .N, by=.(s.unitid, t.unitid)]
  m <- m[N>1, .(s.unitid, t.unitid)]
  result <- result[m]

  # make sure at least two target tokens
  cat (" [2/3] at least 2 target tokens\n")
  setkey(result, s.unitid, t.unitid, t.tokenid)
  m <- result[! duplicated(result), .N, by=.(s.unitid, t.unitid)]
  m <- m[N>1, .(s.unitid, t.unitid)]
  result <- result[m]

  # make sure at least 2 features
  cat (" [3/3] at least 2 features\n")
  setkey(result, s.unitid, t.unitid, feature)
  m <- result[! duplicated(result), .N, by=.(s.unitid, t.unitid)]
  m <- m[N>1, .(s.unitid, t.unitid)]
  result <- result[m]

  return(result)
}


build.stem.cache <- function(file) {
  # read a stem dictionary in csv format

  cat("Loading stems dictionary\n")
  stems <- fread(file, header=F, select=c(1,3))
  setnames(stems, 1:2, c("form", "feat"))

  cat("Standardizing orthography\n")
  stems[,form:=standardize(form),]
  stems[,feat:=standardize(feat),]

  cat("Removing NAs, duplicates\n")
  setkey(stems, form, feat)
  stems <- unique(stems)
  stems <- na.omit(stems)
  stems <- stems[! ""]

  cat("Initializing hash\n")
  nstems <- length(unique(stems$feat))
  index <- new.env(hash=T, size=nstems)
  pb <- txtProgressBar(min=1, max=nstems, style=3)

  cat("Populating hash\n")
  foo <- function(form, feat) {
    assign(form, c(feat), envir=index)
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
  stems[, foo(form, feat), by=form]

  close(pb)
  return(index)
}

patch.latin.stems <- function(stems) {
  # Remove any other lemma that competes with "sum"
  #  - necessary to mimic behaviour of online Tesserae

  cat("Removing stems that compete with \"sum\"\n")

  forms <- ls(stems)

  pb <- txtProgressBar(min=1, max=length(forms), style=3)
  lapply(forms, function(form) {
    if ("sum" %in% get(form, envir=stems)) {
      assign(form, "sum", envir=stems)
    }
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  })
  close(pb)

  return(invisible(stems))
}

build.stem.table <- function(file, resolve = NA) {
  # read a stem dictionary in csv format

  cat("Loading stems dictionary\n")
  stems <- fread(file, header = F, select = c(1,3),  na.strings = NULL)
  setnames(stems, 1:2, c("form", "feat"))

  cat("Standardizing orthography\n")
  stems[, form := standardize(form),]
  stems[, feat := standardize(feat),]

  cat("Removing NAs, duplicates\n")
  stems <- unique(stems)
  setkey(stems, form)

  if (! is.na(resolve)) {
    cat("Resolver: Reading form frequencies\n")
    freq <- fread(resolve, skip = 1)
    setnames(freq, old=c("V1", "V2"), new=c("form", "count"))
    setkey(freq, form)

    cat("Resolver: Resolving duplicates\n")
    stems <- stems[freq, nomatch=0][, .(form, count = sum(count)), by = feat][, .(feat = feat[which.max(count)]), by = form]
    setkey(stems, form)
  }

  return(stems)
}


build.stem.cache2 <- function(file, resolve = NA) {
  # read a stem dictionary in csv format

  # read a stem dictionary in csv format

  cat("Loading stems dictionary\n")
  stems <- fread(file, header=F, select=c(1,3))
  setnames(stems, 1:2, c("form", "feat"))

  cat("Standardizing orthography\n")
  stems[,form:=standardize(form),]
  stems[,feat:=standardize(feat),]

  cat("Removing NAs, duplicates\n")
  setkey(stems, form, feat)
  stems <- unique(stems)
  stems <- na.omit(stems)
  stems <- stems[! ""]

  if (! is.na(resolve)) {
    cat("Resolver: Reading form frequencies\n")
    freq <- fread(resolve, skip = 1)
    setnames(freq, old=c("V1", "V2"), new=c("form", "count"))
    setkey(freq, form)

    cat("Resolver: Resolving duplicates\n")
    stems <- stems[freq, nomatch=0][, .(form, count = sum(count)), by = feat][, .(feat = feat[which.max(count)]), by = form]
    setkey(stems, form)
  }

  cat("Initializing hash\n")
  nstems <- length(unique(stems$feat))
  index <- new.env(hash=T, size=nstems)
  pb <- txtProgressBar(min=1, max=nstems, style=3)

  cat("Populating hash\n")
  foo <- function(form, feat) {
    assign(form, c(feat), envir=index)
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
  stems[, foo(form, feat), by=form]

  close(pb)
  return(index)
}

build.stemmer.table.old <- function(stems.file, resolve = NA) {
  stems <- build.stem.table(stems.file, resolve)

  function(form) {
    res <- stems[form, .(feat = list(feat)), by = .EACHI][, feat]
    ifelse(is.na(res), form, res)
  }
}

build.stemmer.table <- function(stems.file, resolve = NA) {
  stems <- build.stem.cache2(stems.file, resolve)

  function(form) {
    mget(form, stems, ifnotfound=list(identity))
  }
}
