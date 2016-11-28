

read_data<-function(category, year){
  
  library(stringr)
  fl<-unique(readLines(file(paste0('Data/',as.character(category),'/Merged.txt'))))
  speechdata=unique(paste(fl, collapse=' '))
  
  splited<-unique(str_split(speechdata,'-----------------------------------')[[1]])[-1]
  splited2=as.character()
  for(i in splited){
    if(str_detect(i, '-- Supplementary|없음|No message')!=TRUE & nchar(i)>400){
      splited2=c(splited2, i)  
    }
  }
  splited<-splited2
  splited<-str_replace_all(splited, '[-]+ [\\s\\S]{1,15}([\\d]{4})', '\\1')
  byyear=vector('list',11)
  for(i in splited){
    print(str_split(i,'\\.')[[1]][1])
    if(str_detect(i, '\\d{4}')){
      index=as.integer(str_extract(i, '\\d{4}'))-2003
    }
    if(index > 0){
      sentences=str_split(i,'\\.')[[1]][-1]
      byyear[[index]]=sentences
    }
  }
  
  return(byyear[[year-2003]])
}

graph <- function(texts){
  
  library(tm)
  library(SnowballC)
  
  
  cps<-Corpus(VectorSource(texts))
  
  
  cps <- tm_map(cps, content_transformer(tolower))
  cps <- tm_map(cps, content_transformer(removePunctuation))
  cps <- tm_map(cps, content_transformer(removeNumbers))
  cps <- tm_map(cps, removeWords, c(stopwords("english"),'also'))
  
  cps =tm_map(cps, stemDocument)
  
  require(igraph)
  
  tdm=TermDocumentMatrix(cps,
                         control=list(weightiing=weightTfIdf))
  mat<-as.matrix(tdm)
  mat2<-mat %*% t(mat)
  gra <- graph.adjacency(mat2, weighted=TRUE, mode="undirected")
  nodes=vcount(gra)
  edges=ecount(gra)
  density=graph.density(gra)
  transitivity=transitivity(gra)
  meandistance=mean_distance(gra)
  comm<-multilevel.community(gra)
  numcom=length(comm)
  mod<-modularity(comm)
  bigcom3<-as.numeric(sort(sizes(comm), decreasing=T)[1:3]/nodes)
  
  return(c(nodes, edges, density, transitivity, meandistance, numcom, mod, bigcom3))
}

category=c('Energy','Food and Beverage','Health Management',
           'Industrial Goods and Services','Technology','Telecommunication','Utility')
total=vector('list',7)
names(total)=category
for(i in category){
  result=data.frame(matrix(ncol=10, nrow=3))
  names(result)=c('Nodes','Edges','Density','Clustering Coefficient','Mean Distance','Number of Communities','Modularity','Size of 1st Community','Size of 2nd Community','Size of 3rd Community')
  row.names(result)=c('Before Recession','Recession','After Recession')
  beforedata=as.character()
  for(j in 2004:2006){
    beforedata=c(beforedata, read_data(i, j))
  }
  recdata=as.character()
  for(j in 2007:2009){
    recdata=c(recdata, read_data(i, j))
  }
  afterdata=as.character()
  for(j in 2010:2014){
    afterdata=c(afterdata,read_data(i, j))
  }
  result[1,]=graph(beforedata)
  result[2,]=graph(recdata)
  result[3,]=graph(afterdata)
  write.csv(result, paste0('Data/',i,'.csv'))
  total[[i]]=result
}



