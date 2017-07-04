# Thematic work on Valerius Flaccus
#  - for DH 2016

#
# packages
#

library(data.table) # keep
library(parallel)
library(tm) # keep
library(topicmodels)
library(mclust)

source(file.path("src", "tesserae.R"))

#
# functions
#

# keep
load.file <- function(file) {
  # read a single text and produce a data.table

  cat("Reading", file, "... ")

  # parse the Tesserae XML file
  doc <- xmlParseDoc(file, encoding="UTF-8")

  # get author and work from Tesserae text id
  tess.id <- xmlGetAttr(getNodeSet(doc, "/TessDocument")[[1]], "id")
  auth <- sub("\\..*", "", tess.id)
  work <- sub(".*\\.", "", tess.id)

  # print a tally when done
  count <- 0
  on.exit(cat(count, "lines\n"))

  # process lines, build a data.table
  rbindlist(
    xpathApply(doc, "//TextUnit", function(node) {
      count <<- count + 1

      ll = xmlGetAttr(node, "loc")
      vv <- xmlValue(node)
      if (is.na(vv)) { cat("NA at", ll, "\n")}

      data.table(
        auth = auth,
        work = work,
        loc = xmlGetAttr(node, "loc"),
        verse = xmlValue(node)
      )
    })
  )[,
    unitid := .I
  ]
}

# keep
load.corpus <- function(index.file) {
  # load a set of Tesserae texts and construct a corpus

  cat("Loading corpus from", index.file, "\n")

  files <- scan(index.file, what="character", sep="\n")
  cat("Reading", length(files), "files\n")

  setkey(
    rbindlist(lapply(files, load.file))[, pk:=.I],
    work, loc, pk
  )
}

# keep
make.samples <- function(corpus, sample.size=50, offset = 0, stem=identity, rand=T) {
  # generate a new data table made of samples

  if(rand) {
    ord <- corpus[, sample(.I)]
  } else {
    ord <- corpus[,order(pk)]
  }

  corpus[ord,
    .(pk, verse, int.grp=ceiling((.I - offset)/sample.size))
  ][,
    .(
      pk = list(pk),
      tok = list(unname(unlist(
        stem(
          standardize(
            unlist(stri_extract_all_charclass(verse, "\\p{Letter}"))
          )))))),
    by = int.grp
  ][,
    .(pk, tok)
  ]
}


cluster.series <- function(x, k = 5, nreps = 10, cores = NA) {
  # generate a series of k-means clusterings

  cat("Generating", nreps, "classifications with k =", k, "\n")

  inner.function <- function(i) {
    t0 <- Sys.time()
    on.exit(
      cat(
        paste(" - [", i, "/", nreps, "]", sep=""),
        "...",
        difftime(Sys.time(), t0, units = "min"),
        "minutes\n")
    )
    kmeans(x, k)$cluster
  }

  if(is.na(cores)) {
    return(do.call(cbind, lapply(1:nreps, inner.function)))
  } else {
    return(do.call(cbind, mclapply(1:nreps, inner.function, mc.cores = cores)))
  }
}


load.scenes <- function(file, author) {
  # read human-generated scene demarcations

  cat("Loading benchmark themes from", file, "\n")

  auth.start <- match(author, la.verse$auth)

  getverses <- Vectorize(function(Start, End) {
    paste(
      unlist(
        la.stemmer(
          standardize(
            unlist(
              strsplit(la.verse[Start:End]$verse, " ")
            )
          )
        )
      ), collapse = " "
    )
  })

  bench <- data.table(read.table(file, sep="\t", header=T, as.is=T, quote="", fill=T))[,.(Loc, Description, Type)]
  bench[, Loc := sub("\\(?(\\d),(\\d+)\\s*-\\s*(\\d+)\\)?", "\\1.\\2-\\1.\\3", Loc)]
  bench[, Loc := sub("\\(?(\\d),(\\d+)\\s*\\)?$", "\\1.\\2-\\1.\\2", Loc)]
  bench[, Start := match(sub("-.*", "", Loc), la.verse[auth==author, loc]) + auth.start - 1]
  bench[, End := match(sub(".*-", "", Loc), la.verse[auth==author, loc]) + auth.start - 1]
  bench[, Text := getverses(Start, End)]

  return(bench)
}


remapClassToRef <- function(class, ref) {
  cmap <- mapClass(a = ref, b = class)

  new.class <- class

  for (i in names(cmap$bTOa)) {
    new.class[class == i] <- cmap$bTOa[i]
  }

  return(unlist(new.class))
}

# keep
get.stop <- function(tokens, n=25, hapax.exclude=T) {
  count <- sort(table(unlist(tokens)), decreasing=T)

  top <- 1:n

  hapax <- c()
  if (hapax.exclude) hapax <- which(count==1)

  names(count[union(top, hapax)])
}

# keep
get.mask <- function(tok, l=3, r=3) {
  ntok = sapply(tok, length)

  return(
    ntok > mean(ntok) - l * sd(ntok) &
    ntok < mean(ntok) + r * sd(ntok)
  )
}



# keep
feat.tfidf <- function(tok) {
  cat("Calculating TF-IDF values\n")

  as.matrix(DocumentTermMatrix(
    x = VCorpus(VectorSource(
      sapply(tok, paste, collapse=" ")
      )),
    control = list(
      wordLengths=c(1,Inf),
      weighting = weightTfIdf
    )
  ))
}

feat.lda <- function(tok, n) {
  cat("Calculating", n, "topics\n")

  dtm.tf <- DocumentTermMatrix(
    x = VCorpus(VectorSource(
      sapply(tok, paste, collapse=" ")
    )),
    control = list(wordLengths=c(1,Inf))
  )

  slot(LDA(dtm.tf, k = n), "gamma")
}

# keep
auth.sig <- function(x, auth) {
  cat("Calculating mean author signals\n")
  do.call(rbind, by(x, auth, colMeans)) - colMeans(x)
}

# keep
auth.adj <- function(x, auth, auth.sig) {
  cat("Calculating author-adjusted features\n")

  do.call(rbind,
    lapply(levels(auth), function(this.auth) {
      t(t(x[auth==this.auth,]) - auth.sig[this.auth,])
    })
  )
}


load.training <- function(file) {
  training <- fread(file)[,
    .(
      sampleid = paste("t", .I, sep=""),
      auth = factor(Oeuvre),
      work = factor(Oeuvre),
      lstart = paste(DébChant, DébVers, sep="."),
      lend = paste(FinChant, FinVers, sep="."),
      class = factor(stri_extract_first_words(Type)),
      weight = as.integer(! stri_detect_fixed(Type, "\\?"))
    )
  ]

  levels(training$work) <- list(
    metamorphoses = "O",
    aeneid = "V",
    argonautica = "VF",
    punica = "SI",
    thebaid = "ST",
    bellum_civile = "L"
  )

  levels(training$auth) <- list(
    ovid = "O",
    vergil = "V",
    valerius_flaccus = "VF",
    silius_italicus = "SI",
    statius = "ST",
    lucan = "L"
  )

  levels(training$class) <- list(
    battle = "Bataille",
    tempest = "Tempête"
  )

  return(training)
}


get.training.sample.tokens <- function(samples, corpus,
  stem=identity, stoplist=character(0)){

  data.table(
    start = corpus[.(work=samples$work, loc=samples$lstart), pk],
    end = corpus[.(work=samples$work, loc=samples$lend), pk]
  )[,
    tok := do.call(Vectorize(function(start, end) {
      corpus[
        pk >= start & pk <= end,
        setdiff(
          unlist(unname(
            stem(standardize(
              unlist(stri_extract_all_charclass(verse, "\\p{Letter}"))
            ))
          )),
          stoplist
        )
      ]
    }), .(start, end))
    ][,
      ntok := sapply(tok, length)
    ]
}

extract.pk.from.samples <- function(samples, corpus) {
  # resort by author, locus
  oldkeys <- key(corpus)
  if (! identical(oldkeys, c("auth", "pk"))) {
    setkey(corpus, auth, loc)
  }

  start <- corpus[.(auth=samples$auth, loc=samples$lstart), pk]
  end <- corpus[.(auth=samples$auth, loc=samples$lend), pk]

  # put back original keys
  if (! identical(oldkeys, key(corpus))) {
    setkeyv(corpus, oldkeys)
  }

  mapply(seq, from=start, to=end)
}

line.class.from.training <- function(samples, corpus) {
  pks <- extract.pk.from.samples(samples, corpus)
  data.frame(pk=unlist(pks), class=rep(samples$class, sapply(pks, length)))
}

sample.test <- function(
  # run a series of classifications
    corpus,
    sample.size = 50,
    offset = 0,
    stem = list(label="stem", FUN=la.stemmer),
    feature = list(label="tfidf", FUN=feat.tfidf),
    nreps = 15,
    nclusters = c(2:12),
    auth = F,
    pc = NA,
    ncores = NA,
    subsample = 1,
    output.base.dir = "output"
  ) {

  # note time for benchmarking

  t0 <- Sys.time()
  on.exit(
    cat(
      sample.size,
      "/",
      offset,
      ":",
      difftime(
        Sys.time(), t0, units = "min"
      ),
      "minutes\n"
    )
  )

  # create a directory for output; clean if exists

  output.dir <- file.path(output.base.dir,
    paste(
      stem$label,
      ifelse(auth, "adj", "unadj"),
      pc,
      paste("sub", round(subsample * 100, 0), sep=""),
      sep = "-"
    ),
    paste(
      sample.size,
      sprintf("%02i", offset),
      sep = "-"
    )
  )
  if(dir.exists(output.dir)) {
    unlink(output.dir, recursive = T)
  }
  dir.create(output.dir, recursive = T)

  # Generate samples

  cat("Generating samples: size =", sample.size, "; offset =", offset, "\n")
  samples <- by(
    data = corpus,
    INDICES = corpus$auth,
    FUN = make.samples,
    sample.size = sample.size,
    stem = stem$FUN,
    rand = F,
    offset = offset
  )
  samples <- rbindlist(samples)[, `:=`(
    sampleid = .I,
    auth = factor(rep(names(samples), sapply(samples, nrow)))
  )]

  # Remove stopwords

  stoplist <- samples[, get.stop(tok)]
  samples[, tok := sapply(tok, setdiff, y=stoplist)]

  # Mask samples with extreme sizes

  samples <- samples[get.mask(tok)]

  # Optional subsampling

  if (subsample < 1) {
    nsamples <- ceiling(nrow(samples) * subsample)
    cat("Subsampling:", nsamples, "samples\n")
    samples <- samples[sample(.N, nsamples)]
  }

  # Extract feature vectors

  feat <- feature$FUN(samples$tok)

  # Optional PCA

  if (! is.na(pc) & pc > 0) {
    cat("Calculating", pc, "principal components\n")

    feat <- prcomp(feat)$x[,1:pc]
  }


  # Optional authorship adjustment

  if (auth) {
    auth.centers <- auth.sig(feat, auth=samples$auth)
    feat <- auth.adj(feat, auth=samples$auth, auth.sig=auth.centers)
  }

  # Classify

  lapply(nclusters, function(k) {

    output.file <- file.path(output.dir, paste("k", k, sep="-"))

    cat("Generating", nreps, "classifications with k =", k, "\n")

    cluster.one.rep <- function(i, lab="") {
      # do one k-means clustering,
      # return a vector of classes
      #
      # * NB: one class per verse line:
      #  - i.e. repeat sample class for each verse in sample

      cat(paste("\t", lab, " [", i, "/", nreps, "]\n", sep=""))

      sample.class <- kmeans(feat, k, iter.max=20)$cluster

      verse.class <- rep(NA, nrow(corpus))
      verse.class[unlist(samples$pk)] <- rep(
        x = sample.class,
        times = lapply(samples$pk, length)
      )

      return(verse.class)
    }

    write.table(file = output.file,
      if(is.na(ncores)) {
        do.call(cbind, lapply(1:nreps, cluster.one.rep,
          lab=paste(sample.size, offset, k, sep=":")))
      } else {
        do.call(cbind, mclapply(1:nreps, cluster.one.rep,
          lab=paste(sample.size, offset, k, sep=":"), mc.cores = ncores))
      }
    )
  })
}

# ari.metaclust <- function(ari, nrep) {
# 	x <- matrix(0, nrow=nrep, ncol=nrep)
# 	x[lower.tri(x, diag=F)] <- 1 - ari
# 	hclust(as.dist(x))
# }

ari <- function(sample.size, k.range, dir.input) {
  cat("Calculating ari\n")
  do.call(cbind,
    mclapply(k.range, function(k) {
      cat("\tsize =", sample.size, "k =", k, "\n")
      x <- do.call(cbind,
        lapply(seq(0, sample.size-5, 5), function(offset) {
          read.table(file.path(
            dir.input,
            sprintf("%02i-%02i", sample.size, offset),
            paste("k", k, sep="-"))
          )
        })
      )
      apply(combn(ncol(x), 2), 2, function(i) {
        adjustedRandIndex(x[,i[1]], x[,i[2]])
      })
    }, mc.cores=ncores)
  )
}


alt.ari <- function(sample.size, k.range, dir.input, ncores=0) {
  cat("Calculating ari per offset\n")
  do.call(cbind,
    mclapply(k.range, function(k) {
      cat("\tsize =", sample.size, "k =", k, "\n")
      sapply(seq(0, sample.size-5, 5), function(offset) {
        x <- read.table(file.path(
          dir.input,
          sprintf("%02i-%02i", sample.size, offset),
          paste("k", k, sep="-")
        ))
        apply(combn(ncol(x), 2), 2, function(i) {
          adjustedRandIndex(x[,i[1]], x[,i[2]])
        })
      })
    }, mc.cores=ncores)
  )
}
