# Thematic work on Valerius Flaccus
#  - for DH 2016

#
# packages
#

library(data.table)
library(parallel)
library(tm)
library(mclust)

source(file.path("src", "tesserae.R"))

#
# functions
#

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


load.corpus <- function(index.file) {
  # load a set of Tesserae texts and construct a corpus

  cat("Loading corpus from", index.file, "\n")

  files <- scan(index.file, what="character", sep="\n")
  cat("Reading", length(files), "files\n")

  setkey(
    rbindlist(lapply(files, load.file))[, pk:=.I],
    work, pk
  )
}

make.samples <- function(corpus, sample.size=50, offset = 0, rand=F) {
  # generate a new data table made of samples

  if(rand) {
    ord <- corpus[, sample(.I)]
  } else {
    ord <- corpus[,order(pk)]
  }

  corpus[ord,
    .(pk, tok, int.grp=ceiling((.I - offset)/sample.size))
  ][,
    .(
      pk = list(pk),
      tok = list(unlist(tok))),
    by = int.grp
  ][,
    .(pk, tok)
  ]
}

get.stop <- function(tokens, n=25, hapax.exclude=T) {
  # build a stoplist from a feature count

  count <- sort(table(unlist(tokens)), decreasing=T)

  mfw <- c()

  if (n > 0) {
     mfw <- c(1:n)
  }

  hapax <- c()
  if (hapax.exclude) {
    hapax <- which(count==1)
  }

  stopids <- union(mfw, hapax)

  names(count[stopids])
}

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
