#
# Libraries
#

source(file.path("src", "type-scenes.R"))

#
# Global values
#

k.range <- 2:15
size.range <- c(40, 50, 60, 70)
data.dir <- file.path("data")
labels <- c(
  'stem-adj-500-sub80',
  'stem-adj-500-sub90',
  'stem-adj-500-sub100')
ncores <- 3

#
# Functions
#

ari.one.size <- function(
  sample.size,
  k.range,
  data.dir,
	label,
  n.cores) {

  dir.output <- file.path(data.dir, "ari-by-offset", label)
  dir.input <- file.path(data.dir, "class", label)

  # create output directory
  if(! dir.exists(dir.output)) {
    dir.create(dir.output, recursive = T)
  }

  write.table(
    x = alt.ari(sample.size, k.range, dir.input, n.cores),
    file = file.path(dir.output, sample.size)
  )
}


#
# Main
#

for (label in labels) {
  cat("label:", label, "\n")
  for (sample.size in size.range) {
    ari.one.size(
      sample.size = sample.size,
      k.range = k.range,
      data.dir = data.dir,
			label = label,
			n.cores = ncores
    )
  }
}

warnings()
