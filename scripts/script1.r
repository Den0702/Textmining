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
scriptsDir <- ".\\scripts"
workspaceDir <- ".\\workspaces"

#utworzy katalog 
dir.create(outputDir, showWarnings = FALSE)
dir.create(workspaceDir, showWarnings = FALSE)

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

#funcja obudowana w content_transformer, korzystamy z lambda-funkcji
removeChar <- content_transformer(
  function(x, pattern, replacement) 
    gsub(pattern, replacement, x)#zamieniamy pattern na replacement
  )

#usunięcie "em dash" i 3/4 z tekstów
corpus <- tm_map(corpus, removeChar, intToUtf8(8722), "")
corpus <- tm_map(corpus, removeChar, intToUtf8(190), "")

#lematyzacja - sprowadzenie do formy
polish <- dictionary(lang = "pl_PL")

lemmatize <- function(text){
  simpleText <- str_trim(as.character(text[1]))#as.character - to co przychodzi jako text konwertuje to na string
                            #str_trim -usuwanie w ten sposób białych znaków od poczatku do konca - dobra praktyka
  parsedText <- strsplit(simpleText, split = " ")#parsedText - lista, dzielimy tekst na tokeny?, gdzie spacja jest czynnikiem separujacym
                                              #dzieli słowa z tekstu do wektora(każdy element to pojedyńcze słowo)
  newTextVec <- hunspell_stem(parsedText[[1]], dict = polish)#newTextVec - przechowuje formy podstawowe poszczególnych słów wektora
  for (i in 1:length(newTextVec)){                   #hunspell_stem - zwraca listę słów w formie podstawowej
    if(length(newTextVec[[i]]) == 0) newTextVec[i] <- parsedText[[1]][i]#kiedy mamy jedną formę podstawową, to bierzemy oryginalne
    if(length(newTextVec[[i]]) > 1) newTextVec[i] <- newTextVec[[i]][1]#newTextVec[[i]]) > 1 - więcje niż 1 forma podstawowa, 
                                                                              #to bieżemy pierwszy element z (każdej?)listy
  }
  newText <- paste(newTextVec, collapse = " ")#łączymy słowa do jednego stringu używając spacji jako łącznika
  return(newText)                              #czyli konwertuje wektor do tekstu
}

corpus <- tm_map(corpus, content_transformer(lemmatize))#jest przekazywany tekst, nie dokument
#bedziemy siegac metadanych w pliku
#pozbywamy się rozszerzeń, bo one potem i tak zostaną dodane
cutExtentions <- function(document){
  #chcemy metadaną o id dokumentu podmienić na
  meta(document, "id") <- gsub(pattern = "\\.txt$", "", meta(document, "id"))#gsub nie zmienia tekstu podawanego jako parametr,
  #dlatego, jeśli chcemy zachować zmianę, to musimy z powrotem przypisać
  return(document)
}

corpus <- tm_map(corpus, cutExtentions)

#eksport przetworzonego korpusu do plików tekstowych
preprocessedDir <- paste(
  outputDir,
  "\\",
  "Literatura - streszczenia - przetworzone",
  sep = ""
)
dir.create(preprocessedDir, showWarnings = FALSE)
writeCorpus(corpus, path = preprocessedDir)
#writeLines(as.character(corpus[[1]]))











