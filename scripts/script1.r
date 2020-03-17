#włączenie bibliotek
library(tm)

#zmiana katalogu roboczego
workDir <- "D:\\Polska\\Edukacja_w_Polsce\\Uniwersytet ekonomiczny\\Studia magisterskie\\Przetwarzanie jezyka naturalnego\\Textmining_copy"
setwd(workDir)

#definicja katalogów projektu
# . - to jest katalog roboczy, a nie bieżący
inputDir <- ".\\data"
outputDir <- ".\\results"
scriptDir <- ".\\scripts"
workspaceDir <- ".\\workspaces"

#utworzy katalog 
dir.create(outputDir, showWarnings = FALSE)

#utworzenie korpusu dokumentów - chcemy do input dir dokleić ścieżkę dostępu, ale ta zmienna zawiera tylko adres
corpusDir <- paste(inputDir, "\\",  "Literatura - streszczenia - oryginal", sep = "")

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
View(corpus)

# wstepne przetwarzanie 
corpus <- tm_map(corpus, removeNumbers) # wykonuje na korpusie daną transformację
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower)) # tolower może działać i na zawartości i na metadanych, jednak my nie chcemy, żeby np. nazwa pliku była sprowadzona do małych liter 
# definicja lokalizacji zawierającej stoplistę          # my chcemy tylko zawartość dokumentu sprowadzić do małych liter
stoplistFile <- paste(                                 
  inputDir,                                            # funkcja paste zwraca string
  "\\", 
  "stopwords_pl.txt", 
  sep = "")

stoplist <- readLines(# wczytuje po kolei linijki z pliku do listy(nie do ciągłego tekstu)
  stoplistFile,
  encoding = "UTF-8"
)

corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace) # stripWhitespace jest na końcu, bo mogą zostać nadmiarowe "białe znaki"
# bo funkcja removeWords zamienia usuwane przez siebie znaki na spacje

















