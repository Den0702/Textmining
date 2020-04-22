#włączenie bibliotek
library(tm)

#zmiana katalogu roboczego
workDir <- "D:\\Polska\\Edukacja_w_Polsce\\Uniwersytet ekonomiczny\\Studia magisterskie\\Przetwarzanie jezyka naturalnego\\Textmining"
setwd(workDir)

#definicja katalogów projektu
# . - to jest katalog roboczy, a nie bieżący
inputDir <- ".\\data"
outputDir <- ".\\results"
scriptsDir <- ".\\scripts"
workspaceDir <- ".\\workspaces"

#utworzenie katalogu wyjściowego 
dir.create(outputDir, showWarnings = FALSE)
dir.create(workspaceDir, showWarnings = FALSE)

#utworzenie korpusu dokumentów - chcemy do input dir dokleić ścieżkę dostępu, ale ta zmienna zawiera tylko adres
corpusDir <- paste(inputDir, "\\",  "Literatura - streszczenia - przetworzone", sep = "")

#VCorpus - tworzy obiekt korpusu dokumentów
corpus <- VCorpus(
  DirSource(
    corpusDir,
    pattern = "*.txt",
    encoding = "UTF-8"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

cutExtentions <- function(document){
  #chcemy metadaną o id dokumentu podmienić na
  meta(document, "id") <- gsub(pattern = "\\.txt$", "", meta(document, "id"))#gsub nie zmienia tekstu podawanego jako parametr,
  #dlatego, jeśli chcemy zachować zmianę, to musimy z powrotem przypisać (<-)
  return(document)
}

corpus <- tm_map(corpus, cutExtentions)

#utworzenie macierzy częstości
tdmTfAll <- TermDocumentMatrix(corpus)
dtmTfAll <- DocumentTermMatrix(corpus)
tdmTfidfAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)

tdmBinAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
tdmTfBounds <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
tdmTfidfBounds <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
#tdm zmienilismy na dtm, zeby nie transponowac tdm do dtm(?bo miacierz zawierala bardzo duzo wymiarow(slow)?)
dtmTfidfBounds <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

tdmTfAllMatrix <- as.matrix(tdmTfAll)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
tdmTfidfAllMatrix <- as.matrix(tdmTfidfAll)
tdmBinAllMatrix <- as.matrix(tdmBinAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfidfBoundsMatrix <- as.matrix(tdmTfidfBounds)
dtmTfidfBoundsMatrix <- as.matrix(dtmTfidfBounds)

#eksport macierzy do pliku .csv
matrixFile <- paste(
  outputDir,
  "\\",
  "tdmTfidfBounds.csv",
  sep = ""
)
write.table(tdmTfidfBoundsMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

