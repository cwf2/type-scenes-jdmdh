#
# libraries
#

source(file.path("src", "type-scenes.R"))

#
# global variables
#

k.range <- 2:15
size.range <- c(40, 50, 60, 70)
subsample.range <- c(0.8, 0.9, 1)
auth <- T
ncores <- 3
output.dir <- file.path('data', 'class')

#
# main
#

# make warnings appear immediately
#  - so we can (sort of) see which kmeans calls don't converge
options(warn=1)

# seed for the JDMDH article is my birthday
set.seed(19810111)

# load latin stemmer
la.stemmer <- build.stemmer.table(
  stems.file = file.path("data", "tesserae", "la.lexicon.csv"),
  resolve = file.path("data", "tesserae", "la.word.freq")
)


# load latin corpus
corpus <- load.corpus(file.path("data", "la.index.txt"))


# loop the experiment
for (size in size.range) {
  for (sub in subsample.range) {
    offset.range = seq(from = 0, to = size-5, by = 5)
    mclapply(offset.range, function(offset) {
      sample.test(
        corpus = corpus,
        sample.size = size,
        offset = offset,
        nclusters = k.range,
        nreps = 15,
        subsample = sub,
        auth = auth,
        pc = 500,
        output.base.dir = output.dir
      )
    }, mc.cores=ncores)
  }
}
