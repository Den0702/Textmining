#włączenie bibliotek
library(tm)
library(hunspell)
library (stringr)
#zmiana katalogu roboczego
workDir <- "D:\\Polska\\Edukacja_w_Polsce\\Uniwersytet ekonomiczny\\Studia magisterskie\\Przetwarzanie jezyka naturalnego\\Textmining"
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

remove_char <- content_transformer(
  function(x, pattern, replacement) 
    gsub(pattern, replacement, x)
  )

#usunięcie "em dash" i 3/4 z tekstów
corpus <- tm_map(corpus, remove_char, intToUtf8(8722), "")
corpus <- tm_map(corpus, remove_char, intToUtf8(190), "")

#lematyzacja - sprowadzenie do formy
polish <- dictionary(lang = "pl_PL")

lemmatize <- function(text){
  simple_text <- str_trim(as.character(text[1]))
  parsed_text <- strsplit(simple_text, split = " ")
  new_text_vec <- hunspell_stem(parsed_text[[1]], dict = polish)
  for (i in 1:length(new_text_vec)){
    if(length(new_text_vec[[i]]) == 0) new_text_vec[i] <- parsed_text[[1]][i]
    if(length(new_text_vec[[i]]) > 1) new_text_vec[i] <- new_text_vec[[i]][1]
  }
  new_text <- paste(new_text_vec, collapse = " ")
  return(new_text)
}

corpus <- tm_map(corpus, content_transformer(lemmatize))
#bedziemy siegac metadanych w pliku
cut_extentions <- function(document){
  meta(document, "id") <- gsub(pattern = "\\.txt$", "", meta(document, "id"))
  return(document)
}

corpus <- tm_map(corpus, cut_extentions)

preprocessed_dir <- paste(
  outputDir,
  "\\",
  "Literatura - streszczenia - przetworzone",
  sep = ""
)
dir.create(preprocessed_dir, showWarnings = FALSE)
writeCorpus(corpus, path = preprocessed_dir)
#writeLines(as.character(corpus[[1]]))











