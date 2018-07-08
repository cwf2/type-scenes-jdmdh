plot(-pca[,1], pca[,2], col=samples$auth, cex=3, pch=20)
legend('topleft', legend=levels(samples$auth), col=1:6, pch=20, cex=3)
identify(-pca[,1], pca[,2], labels=corpus[order(pk)][pk %in% unlist(lapply(samples$pk, min)), loc])


id.sample <- function(id) {
   corpus[pk %in% samples[id, range(pk)],
      paste(unique(auth), unique(work), paste(loc, collapse='-'))]
}
