library(jsonlite)
source(file.path("src", "refactored.R"))

# load latin stemmer
la.stemmer <- build.stemmer.table(
  stems.file = file.path("data", "tesserae", "la.lexicon.csv"),
  resolve = file.path("data", "tesserae", "la.word.freq")
)


# load latin corpus
corpus <- load.corpus(file.path("data", "la.index.txt"))

# tokenize and stem
cat("Tokenizing and stemming")
corpus[,
  tok := .(list(unname(unlist(
    la.stemmer(standardize(
      unlist(stri_extract_all_charclass(verse, "\\p{Letter}"))
    ))
  )))),
  by = pk
]

# export to json
export.file <- 'rlemmata.json'
cat('Exporting', export.file, '\n')
write(toJSON(corpus[,.(auth,tok)]), file=export.file)
