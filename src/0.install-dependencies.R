# CRAN repository of choice
repos <- "http://cran.rstudio.com"

# package dependencies for jdmdh scripts
dependencies <- c(
    "XML",
    "data.table",
    "mclust",
    "stringi",
    "tm",
    "topicmodels"
)

# install required packages
update.packages(repos=repos, ask=T)
install.packages(pkgs=dependencies, repos=repos)
