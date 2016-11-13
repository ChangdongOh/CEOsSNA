TEXTFILE = "data/pg100.txt"
if (!file.exists(TEXTFILE)) {
download.file("http://www.gutenberg.org/cache/epub/100/pg100.txt", destfile = TEXTFILE)
}

shakespeare = readLines(TEXTFILE)
length(shakespeare)
head(shakespeare)


shakespeare = shakespeare[-(1:173)]
shakespeare = shakespeare[-(124195:length(shakespeare))]
shakespeare = paste(shakespeare, collapse = " ")


shakespeare = strsplit(shakespeare, "<<[^>]*>>")[[1]]

dramatis.personae <- grep("Dramatis Personae", shakespeare, ignore.case = TRUE)

shakespeare = shakespeare[-dramatis.personae]

library(tm)

doc.vec <- VectorSource(shakespeare)
doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)

doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, content_transformer(removePunctuation))
doc.corpus <- tm_map(doc.corpus, content_transformer(removeNumbers))
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))


library(SnowballC)
doc.corpus <- tm_map(doc.corpus, stemDocument)
doc.corpus <- tm_map(doc.corpus, stripWhitespace)

inspect(doc.corpus[8])

TDM <- DocumentTermMatrix(doc.corpus)
