library(stringr)

fl<-unique(readLines(file('Data/Merged.txt')))
speechdata=unique(paste(fl, collapse=' '))


byyear=list()
splited<-unique(str_split(speechdata,'-----------------------------------')[[1]])[-1]
for(i in splited){
  print(str_split(i,'\\.')[[1]][1])
  index=as.integer(str_extract(i, '\\d{4}'))-2002
  sentences=str_split(i,'\\.')[[1]][-1]
  tryCatch({
    sentences=c(sentences,byyear[[index]])
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  byyear[[index]]=sentences
}

texts=str_replace_all(byyear[[6]], '[^A-Za-z]+',' ')

library(KoNLP)
library(tm)
library(tokenizers)

cps<-Corpus(VectorSource(texts))
tdmt1<-Sys.time()

tokenizer=function(texts){
  token<-as.character()
  for(i in texts){
    d <- as.character(i)
    splited<-unlist(str_split(d," "))
    for(j in splited){
      j<-as.character(j)
      
    }
  tokenize_ngrams(texts, n=1)
}

tdm=TermDocumentMatrix(cps,
                       control=list(tokenize=tokenizer,
                                    removeNumbers=T,
                                    removePunctuation=T,
                                    wordLengths=c(3, 15),
                                    stopwords = stopwords("en"),
                                    weightiing=weightTfIdf
                       ))


tdmt2<-Sys.time()
tdmt<-tdmt2-tdmt1
tdmt