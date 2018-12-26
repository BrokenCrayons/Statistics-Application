library(rJava)
library(Rwordseg) 
library(RColorBrewer)
library(jiebaR)
library(wordcloud)

# Organize Data -> add T to "Type" to last row, delete ALL "S" of "Type"
### 将excel中的文本按trial和condition和parent-child分入不同的变量中

# freeplay <- read.csv('~/Desktop/UCB 2017 Fall/Research/RA for ucb/Baby Center_Minxuan/freeplay/freeplay scripts_new.csv')
freeplay <- read.csv("/Users/wujiawen/Desktop/UCB 2017 Fall/Research/RA for ucb/Baby Center_Minxuan/freeplay/freeplay scripts_new2 copy.csv")
str(freeplay)

is.factor(freeplay$Infant.vocalizations)
freeplay$Infant.vocalizations<-as.character(freeplay$Infant.vocalizations)
freeplay$Referent<-as.character(freeplay$Referent)
##计算事件
i<-1
Sample <-c()
Parent <- c()
Parent_text_c1 <- c()
Baby_text_c1 <- c()
Parent_text_c2 <- c()
Baby_text_c2 <- c()
c1_time <-c()
c2_time<- c()
parent_p <-" "
infant_p <-" "
# demo <- paste(Vocalizations[6:9],collapse = " ")
for(i in 1:7141){
  print(i)
  ## Sample
  if(i==1) {Sample <- c(Sample,freeplay$Infant.vocalizations[1]) 
  condition <- "c1"}
  if (grepl(pattern = "T",x = freeplay$Type[i]) && !(i==1)){ 
    if (!(i==7141)){Sample <- c(Sample,freeplay$Infant.vocalizations[i])}
    if(grepl(pattern = "c1",x = condition)){
      Parent_text_c1 <- c(Parent_text_c1,parent_p)
      Baby_text_c1 <- c(Baby_text_c1, infant_p)
      c1_time <- c(c1_time, freeplay$Exit[i-1] )################ end - begin
      Parent_text_c2 <- c(Parent_text_c2, " ")
      Baby_text_c2 <- c(Baby_text_c2, " ")
      c2_time <- c(c2_time, " ")
    }
    if(grepl(pattern = "c2",x = condition)){
      Parent_text_c2 <- c(Parent_text_c2,parent_p)
      Baby_text_c2 <- c(Baby_text_c2, infant_p)
      c2_time <- c(c2_time,freeplay$Exit[i-1]-time_mark  )################ end - begin
    }
    condition <- "c1"
    parent_p <-" "
    infant_p <-" "
  }
  # condition shift
  if (grepl(pattern = "Condition shift",x = freeplay$Others[i])){
    Parent_text_c1 <- c(Parent_text_c1,parent_p)
    Baby_text_c1 <- c(Baby_text_c1, infant_p)
    c1_time <- c(c1_time, freeplay$Exit[i])################ end - begin
    time_mark <- freeplay$Exit[i]
    condition <- "c2"
    parent_p <-" "
    infant_p <-" "
  }
  ## Character
  if(grepl(pattern = "Dad",x = freeplay$Referent[i])){ 
    Parent <- c(Parent, freeplay$Referent[i])
  }
  if(grepl(pattern = "Mom",x = freeplay$Referent[i])){ 
    Parent <- c(Parent, freeplay$Referent[i])
  }
  if(grepl(pattern = "Grandpa",x = freeplay$Referent[i])){ 
    Parent <- c(Parent, freeplay$Referent[i])
  }
  if(grepl(pattern = "Grandma",x = freeplay$Referent[i])){ 
    Parent <- c(Parent, freeplay$Referent[i])
  }
  ## c1,c2
  if(grepl(pattern = "arent",x = freeplay$Parent.vocalizations[i])){ 
    parent_p <- paste(parent_p,freeplay$Vocalizations[i],collapse = " ")
  }
  if(grepl(pattern = "nfant",x = freeplay$Infant.vocalizations[i])){ 
    infant_p <- paste(infant_p,freeplay$Vocalizations[i],collapse = " ")
  }
}

## 统计词频
countwords<- function(list_of_words){
  a<- lapply(X = list_of_words, FUN=segmentCN)
  b <- unlist(summary(a))
  word_count <- b[,1]
  c <- lapply(X = a,FUN = table)
  sd <-unlist(lapply(X = c,FUN = sd))
  type <- unlist(lapply(X = c,FUN = length))
  return(data.frame(word_count,sd,type))
}

pc1 <- countwords(Parent_text_c1)
bc1 <- countwords(Baby_text_c1)
pc2 <- countwords(Parent_text_c2)
bc2 <- countwords(Baby_text_c2)


result_rawfreeplay_light <- data.frame(Sample,pc1,bc1,pc2,bc2)
result_rawfreeplay <- data.frame(Sample,Parent, Parent_text_c1, pc1, Baby_text_c1, bc1, c1_time, Parent_text_c2, pc2,Baby_text_c2, bc2, c2_time )
Parent[66] # Only 65 items of parents in the dataset

write.csv(x = result_rawfreeplay_light,file = '~/Desktop/UCB 2017 Fall/Research/RA for ucb/Baby Center_Minxuan/freeplay/freeplay_raw_result_new.xlsx' )
"/Users/wujiawen/Desktop/UCB 2017 Fall/Research/RA for ucb/Baby Center_Minxuan/freeplay/freeplay_raw_result_03152018.xls"
## wordcloud for fun
cloud <-c()
for(i in 1:67){
  cloud <- paste(cloud,result_rawfreeplay$Parent_text_c1[i],collapse = " ")
  cloud <- paste(cloud,result_rawfreeplay$Parent_text_c2[i],collapse = " ")
}
myfile.words<-unlist(lapply(X = cloud,FUN = segmentCN)) 
myfile.freq <- table(unlist(myfile.words))
myfile.freq<- rev(sort(myfile.freq))
myfile.freq<- data.frame(word=names(myfile.freq), freq=myfile.freq)
mycolors<- brewer.pal(8,"Dark2")
wordcloud(myfile.freq$word,myfile.freq$freq.Freq,random.order=FALSE,random.color=FALSE,colors=mycolors,family ="GB18030 Bitmap") 


a<- lapply(X = Baby_text_c1, FUN=segmentCN)
b <- unlist(summary(a))
word_count <- b[,1]
c <- lapply(X = a,FUN = table)
sd <-unlist(lapply(X = c,FUN = sd))
type <- unlist(lapply(X = c,FUN = length))

length(c[1])
