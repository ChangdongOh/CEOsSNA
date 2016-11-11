library(docxtractr)
library(stringr)

fl<-readLines(file('Data/Total CEO msg_2013.txt'))
speech=c(speech, fl)
speechdata=paste(speech[2:length(speech)], collapse=' ')

byyear=list()
for(i in 2003:2014){
 for(j in speechdata){
   if(str_detect(speechdata[1],'------ [a-zA-Z+]')) & ){
     byyear[[k]]=c(byyear[[k]], j)
   }
 }
}