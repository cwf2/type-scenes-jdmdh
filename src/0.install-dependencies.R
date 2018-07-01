# package dependencies for jdmdh scripts
dependencies <- c(
    "XML",
    "data.table",
    "mclust",
    "stringi",
    "tm",
    "topicmodels"
)

# CRAN repository of choice
repos <- "http://cran.rstudio.com"


# Path to site-wide and user R libraries
lib_site <- Sys.getenv('R_LIBS_SITE')[1]
lib_user <- Sys.getenv('R_LIBS_USER')[1]

# install required packages
update.packages(repos=repos, ask=F)

try (
install.packages(pkgs=dependencies, repos=repos,
  lib=lib_site)
)
