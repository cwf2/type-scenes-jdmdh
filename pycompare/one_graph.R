# load functions
source(file.path("src", "refactored.R"))

# libraries
library(ggplot2)
library(RColorBrewer)
library(jsonlite)

# set parameters
sample.size <- 30
offset <- 0
n.stop <- 0
exclude.hapax <- FALSE

# export data at various stages
#  - set to NA to skip export
export.file.lemmata <- 'rlemmata.json'
export.file.tfidf <- 'rtfidf.csv'
export.file.labels <- 'rlabels.txt'

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

# Export lemmata to json
if (! is.na(export.file.lemmata)) {
  cat('Exporting lemmata to', export.file.lemmata, '\n')
  write(toJSON(corpus[,.(auth,tok)]), file=export.file.lemmata)
}


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

# Export samples to file
if (! is.na(export.file.labels)) {
  cat('Exporting sample labels to', export.file.labels, '\n')
  write(as.character(samples$auth),
    file=export.file.labels,
    sep='\n')
}


# Remove stopwords
cat('Calculating', n.stop, 'stopwords.')
if (exclude.hapax == T) {
  cat(' Calculating hapax legomena.')
}
cat('\n')
stoplist <- samples[, get.stop(tok, n.stop, exclude.hapax)]
if (length(stoplist) > 0) {
  cat('Applying stoplist of', length(stoplist), 'tokens \n')
  samples[, tok := sapply(tok, setdiff, y=stoplist)]
}

# Extract feature vectors
feat <- feat.tfidf(samples$tok)

# Export feature vectors to file
if (! is.na(export.file.tfidf)) {
  cat('Exporting TF-IDF features to', export.file.tfidf, '\n')
  write.csv(feat, file=export.file.tfidf, row.names=F, fileEncoding="utf-8")
}

# Optional PCA
cat('Performing PCA\n')
pca <- prcomp(feat)$x[,1:2]
pca[,1] <- 0 - pca[,1]

# plot
cat('Plotting\n')
png(file='r_output.png', width=800, height=500)
ggplot(data.frame(pca, auth=samples$auth), aes(x=PC1, y=PC2)) +
   geom_point(aes(color=auth), size=3) +
   scale_colour_brewer(palette = "Set1") +
   labs(title='R lems + R pipeline')
dev.off()

#
# graph each author separately
#

# generate labels

loci <- corpus[order(pk)][pk %in% unlist(lapply(samples$pk, min)), loc]

for (auth in levels(samples$auth)) {
	pdf(file=paste('r_pca_', auth, '.pdf', sep=''), width=8, height=5, pointsize=8)
	plot(
		pca[samples$auth==auth,],
		type = 'n',
		main = paste('R', '-', auth)
	)
	text(
		pca[samples$auth==auth,],
		labels = loci[samples$auth==auth]
	)
	dev.off()
}



