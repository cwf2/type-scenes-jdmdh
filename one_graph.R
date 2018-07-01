# load functions
source(file.path("src", "refactored.R"))

# set parameters
sample.size <- 30
offset <- 0
n.stop <- 0
exclude.hapax <- FALSE

#
# main
#


# load latin stemmer
la.stemmer <- build.stemmer.table(
  stems.file = file.path("data", "tesserae", "la.lexicon.csv"),
  resolve = file.path("data", "tesserae", "la.word.freq")
)


# load latin corpus
corpus <- load.corpus(file.path("data", "la.index.txt"))

# tokenize and stem
cat("Tokenizing and stemming\n")
corpus[,
  tok := .(list(unname(unlist(
    la.stemmer(standardize(
      unlist(stri_extract_all_charclass(verse, "\\p{Letter}"))
    ))
  )))),
  by = pk
]

# sample
cat("Sampling: size =", sample.size, 'and offset =', offset, '\n')
samples <- by(
  data=corpus,
  INDICES = corpus$auth,
  FUN = make.samples,
  sample.size = sample.size,
  offset = offset
)

samples <- rbindlist(samples)[, `:=`(
  sampleid = .I,
  auth = factor(rep(names(samples), sapply(samples, nrow)))
)]

# write samples to file


# Remove stopwords
cat('Removing', n.stop, 'stopwords.')
if (exclude.hapax == T) {
  cat(' Removing hapax legomena.')
}
cat('\n')
stoplist <- samples[, get.stop(tok, n.stop, exclude.hapax)]
cat('Stoplist:', paste(stoplist, sep=', '))
samples[, tok := sapply(tok, setdiff, y=stoplist)]

# Extract feature vectors

feat <- feat.tfidf(samples$tok)

# Optional PCA

pca <- prcomp(feat)$x[,1:2]


# plot

plot(pca, col=samples$auth, pch=1, lwd=3, cex=2)
legend('topright', legend=levels(samples$auth), col=1:6, pch=1, cex=2)
