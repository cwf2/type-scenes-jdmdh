#
# libraries
#

source(file.path("src", "type-scenes.R"))

#
# global values
#

k.range <- 2:15
size.range <- c(40, 50, 60, 70)
input.dir <- file.path("output")
plot.dir <- "plot"
labels <- c(
  'stem-adj-500-sub80',
  'stem-adj-500-sub90',
  'stem-adj-500-sub100')


#
# Functions
#

plotfile <- function(file, plot.fun, ...) {
  pdf(
    file = file,
    width = 10,
    height = 5,
    pointsize = 10)
  plot.fun(...)
  dev.off()
}

plot.ari <- function(ari, k.range, sample.size, label) {
  # reshape the data: concatenate columns by k
  n = nrow(ari) * ncol(ari)/length(k.range)
  grouped.ari <- data.frame(split(unlist(ari), rep(k.range, each=n)))

  boxplot(grouped.ari, names=k.range)
  title(
    xlab = "k",
    ylab = "adjusted rand index",
    main = paste(label, "\nsample size =", sample.size)
  )
}

plot.ari.by.offset <- function(ari, k.range, sample.size, label) {
  offset.range <- (seq(ncol(ari)/length(k.range)) - 1) * 5
  palette(rainbow(max(6,length(offset.range))))
  k.color = sapply(k.range, function(x) ifelse(x %% 2, "grey90", "white"))

  bp <- boxplot(ari,
    plot = F
  )
  plot.new()
  plot.window(
    xlim = c(0, ncol(ari)),
    ylim = c(0,1)
  )
  rect(
    xleft = seq(from=1, to=ncol(ari), by=length(offset.range)) - 0.5,
    ybottom = par()$usr[3],
    xright = seq(from=length(offset.range), to=ncol(ari),
                  by=length(offset.range)) + 0.5,
    ytop = par()$usr[4],
    col = k.color,
    border = k.color
  )
  bxp(bp,
    add = T,
    boxfill = rep(seq_along(offset.range), times=ncol(ari)/length(k.range)),
    pars = list(xaxt="n")
  )
  mtext(
    text = k.range,
    side = 1,
    line = 1,
    at = seq(from=length(offset.range)/2, to=ncol(ari), by=length(offset.range))
  )
  legend("topright",
    legend = offset.range,
    fill = 1:length(offset.range)
  )
  title(
    xlab = "k",
    ylab = "adjusted rand index",
    main = paste(label, "\nsample size =", sample.size)
  )
}

plot.mean.ari.by.offset <- function(ari, k.range, sample.size, label) {
  offset.range <- (seq(ncol(ari)/length(k.range)) - 1) * 5
  ari.mean <- apply(ari, 2, mean)

  plot.new()
  plot.window(
    xlim = range(k.range),
    ylim = c(0,1)
  )
  axis(1)
  axis(2)
  title(
    xlab = "k",
    ylab = "mean adjusted rand index",
    main = paste(label, "\nsample size =", sample.size)
  )
  palette(rainbow(max(6,length(offset.range))))

  for (offset_i in seq_along(offset.range)) {
    sel <- 1:ncol(ari) %% length(offset.range) == offset_i %% length(offset.range)
    lines(k.range, ari.mean[sel], col=offset_i)
  }

  legend("topright", legend=offset.range, col=1:length(offset.range), lty=1)
}

#
# Main
#

for (label in labels) {
  for (sample.size in size.range) {

    # load the ari data
    ari <- read.table(
      file = file.path(input.dir, label, "ari-by-offset", sample.size)
    )

    # boxplot of all ari values by k
    plotfile(
      file = file.path(plot.dir,
        paste("ari_by_k", label, sample.size, "pdf", sep=".")),
      plot.fun = plot.ari,
      ari = ari,
      k.range = k.range,
      sample.size = sample.size,
      label = label
    )

    # boxplot of ari values by offset, k
    plotfile(
      file = file.path(plot.dir,
        paste("ari_by_offset", label, sample.size, "pdf", sep=".")),
      plot.fun = plot.ari.by.offset,
      ari = ari,
      k.range = k.range,
      sample.size = sample.size,
      label = label
    )

    # mean ari by offset, k
    plotfile(
      file = file.path(plot.dir,
        paste("ari_mean_by_offset", label, sample.size, "pdf", sep=".")),
      plot.fun = plot.mean.ari.by.offset,
      ari = ari,
      k.range = k.range,
      sample.size = sample.size,
      label = label
    )
  }
}
