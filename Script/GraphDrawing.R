

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

