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




graphmaker <- function(year){
  texts=str_replace_all(byyear[[year-2002]], '[^A-Za-z]+',' ')
  
  library(tm)
  library(SnowballC)
  
  
  cps<-Corpus(VectorSource(texts))
  
  
  cps <- tm_map(cps, content_transformer(tolower))
  cps <- tm_map(cps, content_transformer(removePunctuation))
  cps <- tm_map(cps, content_transformer(removeNumbers))
  cps <- tm_map(cps, removeWords, stopwords("english"))

  
  tdm=TermDocumentMatrix(cps)
  mat<-as.matrix(tdm)
  mat2<-mat %*% t(mat)
  gra <- graph.adjacency(mat2, weighted=TRUE, mode="undirected")
  gra<-simplify(gra)
  density=graph.density(gra)
  transitivity=transitivity(gra)
  distance=mean_distance(gra)
  
  tdm=TermDocumentMatrix(cps,
                         control=list(weightiing=weightTfIdf,
                                      bounds=list(global=c(floor(length(cps)*0.04), Inf)
                                      )))
  
  
  mat<-as.matrix(tdm)
  mat2<-mat %*% t(mat)
  
  library(igraph)
  
  gra <- graph.adjacency(mat2, weighted=TRUE, mode="undirected")
  gra<-simplify(gra)
  
  btw<-betweenness(gra)
  btw.score<-round(btw)
  btw.colors<-rev(heat.colors(max(btw.score)))
  V(gra)$color<-btw.colors[btw.score]
  V(gra)$size<-degree(gra)
  
  png(file=paste0('energy',year,'.png'),height=1200,width=1600)
  
  plot.igraph(gra,
              #layout=layout.fruchterman.reingold.grid,
              main=paste0('Energy Industry ',year,' Network'),
              sub=paste0('Graph Density ', density, ' Clustering Coefficients ', transitivity, ' Mean Distance ', distance),
              rescale=T,
              #vertex.frame.color='white',
              vertex.label.color='black',
              vertex.color=V(gra)$color,
              edge.width=(E(gra)$weight-1)*3,
              vertex.size=V(gra)$size/(sum(V(gra)$size)/400),
              edge.color='gray'
              #edge.mode=??
  )
  
  dev.off()
}


for(i in 2003:2014){
  graphmaker(i)
}





texts=str_replace_all(byyear[[5]], '[^A-Za-z]+',' ')


library(tm)
library(SnowballC)


cps<-Corpus(VectorSource(texts))


cps <- tm_map(cps, content_transformer(tolower))
cps <- tm_map(cps, content_transformer(removePunctuation))
cps <- tm_map(cps, content_transformer(removeNumbers))
cps <- tm_map(cps, removeWords, stopwords("english"))

tdmt1<-Sys.time()


tdm=TermDocumentMatrix(cps,
                       control=list(weightiing=weightTfIdf,
                                    bounds=list(global=c(4, Inf)
                       )))


tdmt2<-Sys.time()
tdmt<-tdmt2-tdmt1
tdmt

mat<-as.matrix(tdm)
mat2<-mat %*% t(mat)

library(igraph)

gra <- graph.adjacency(mat2, weighted=TRUE, mode="undirected")
gra<-simplify(gra)

btw<-betweenness(gra)
btw.score<-round(log(btw+1))
btw.colors<-rev(heat.colors(max(btw.score)))
V(gra)$color<-btw.colors[btw.score]
V(gra)$size<-degree(gra)


#png(file='minjoo.png',height=1200,width=1600)
#set.seed(1200)

plot.igraph(gra,
            #layout=layout.fruchterman.reingold.grid,
            #main='새누리다 논평 단어 네트워크',
            rescale=T,
            #vertex.frame.color='white',
            vertex.label.color='black',
            vertex.color=V(gra)$color,
            edge.width=E(gra)$weight,
            vertex.size=V(gra)$size,
            edge.color='gray'
            #edge.mode=??
)

#dev.off()



V(gra)$label <- V(gra)$name
V(gra)$degree <- degree(gra)

# plot layout fruchterman.reingold
layout1 <- layout.fruchterman.reingold(gra)
plot(gra, layout=layout1, vertex.size=5, 
     vertex.label.color="darkred")

