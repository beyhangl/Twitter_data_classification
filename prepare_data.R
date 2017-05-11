#####SEEING TURK TELEKOM'S  MOST FREQUENT WORDS ON TWITTER
library("twitteR")
library("ROAuth")
library(devtools)
library("tm")
library("SnowballC")
library("stringr")

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

setup_twitter_oauth("xxx", "xxx", "xxx-xxx", "xxx")
tweets <- searchTwitter("xxxx", n=7000, lang="tr", since="2008-01-01")
tweets.df <- twListToDF(tweets)
mydata=data.frame(tweets.df$text)
write.table(mydata, "test123.csv", row.names=FALSE, sep=",")
dat = readLines("test123.csv")
mydata<-iconv(dat, to='ASCII//TRANSLIT')

corpus <- Corpus(VectorSource(iconv(mydata, 'UTF-8', 'ASCII')))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c("boktan", stopwords("english"),"rt"))
corpus <- tm_map(corpus, stemDocument)
corpus_clean <- tm_map(corpus, PlainTextDocument)
mydata=data.frame(text = sapply(corpus_clean, as.character), stringsAsFactors = FALSE)

mydata<-iconv(dat, to='ASCII//TRANSLIT')
length(mydata)

require("tm")
for(j in 1:length(mydata)){
  splitted<-as.list(strsplit(mydata[j], " ")[[1]])
  test=grep('http', splitted, value=TRUE)
  if(!identical(test, character(0))){ 
    test<-gsub('"', '',test)
    mydata[j]<-removeWords((mydata[j]),test)}
  for (i in 1:11){
  test=grep(i, splitted, value=TRUE)
  if(!identical(test, character(0))){ 
    test<-gsub('"', '',test)
    mydata[j]<-removeWords((mydata[j]),test)}
  }
}
#data<-sapply(data,function(row) iconv(row, "latin1", "ASCII", sub=""))
#data[2]=0
#colnames(data) <- c("text", "sentiment")
mydata<-data.frame(mydata)
mydata[mydata==""]<-NA
mydata<-data.frame(mydata)
mydata<-mydata[complete.cases(mydata),]
corpus <- Corpus(VectorSource(iconv(mydata, 'UTF-8', 'ASCII')))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)
corpus_clean <- tm_map(corpus, PlainTextDocument)
mydata=data.frame(text = sapply(corpus_clean, as.character), stringsAsFactors = FALSE)
mydata=data.frame(mydata)
mydata[,2]<-0

colnames(mydata) <- c("text", "sentiment")
bob <- lapply(mydata, as.character)
mydata <- mydata[nchar(bob$text)>12 , ]
library(qdapRegex)
rm_white(mydata)
mydata <- mydata[!apply(is.na(mydata) | mydata == "", 1, all),]

write.table(mydata, "non.csv", row.names=FALSE, sep=",")

