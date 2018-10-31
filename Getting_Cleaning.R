

## Loading all necessary packages
if(require(tidyverse)==FALSE)(install.packages("tidyverse")); library(tidyverse)
if(require(downloader)==FALSE)(install.packages("downloader")); library(downloader)
if(require(tm)==FALSE)(install.packages("tm")); library(tm)
if(require(stringi)==FALSE)(install.packages("stringi")); library(stringi)
if(require(NLP)==FALSE)(install.packages("NLP")); library(NLP)
if(require(openNLP)==FALSE)(install.packages("openNLP")); library(openNLP)
if(require(RWeka)==FALSE)(install.packages("RWeka")); library(RWeka)
if(require(RWekajars)==FALSE)(install.packages("RWekajars")); library(RWekajars)
if(require(SnowballC)==FALSE)(install.packages("SnowballC")); library(SnowballC)
if(require(quanteda)==FALSE)(install.packages("quanteda")); library(quanteda)
if(require(qdap)==FALSE)(install.packages("qdap")); library(qdap)
if(require(qdapDictionaries)==FALSE)(install.packages("qdapDictionaries")); library(qdapDictionaries)
if(require(wordcloud)==FALSE)(install.packages("wordcloud")); library(wordcloud)
if(require(RColorBrewer)==FALSE)(install.packages("RColorBrewer")); library(RColorBrewer)


## Downloading the data and unzipping the folder
## Check if directory already exists?
if(!file.exists("./projectData")){
  dir.create("./projectData")
}
Url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
## Check if zip has already been downloaded in projectData directory?
if(!file.exists("./projectData/Coursera-SwiftKey.zip")){
  download.file(Url,destfile="./projectData/Coursera-SwiftKey.zip",mode = "wb")
}
## Check if zip has already been unzipped?
if(!file.exists("./projectData/final")){
  unzip(zipfile="./projectData/Coursera-SwiftKey.zip",exdir="./projectData")
}

#We'll focus on english language
path_english<-file.path("./projectData/final" , "en_US")
list.files(path_english)

#There are three files (blogs,news and twitter)
#Since the data is downlaoded, let's read it using readline (reading line by line)
#setting up location and read it in binary mode
blogsLocation <- file(paste0(path_english,"/en_US.blogs.txt"), open="rb")
#reading the data
blogs <- readLines(blogsLocation, encoding = "UTF-8", skipNul=TRUE)

#Same steps for news and twitter data
newsLocation <- file(paste0(path_english,"/en_US.news.txt"), open = "rb") 
news <- readLines(newsLocation, encoding = "UTF-8", skipNul=TRUE)

twitterLocation <- file(paste0(path_english,"/en_US.twitter.txt"), open = "rb") # open for reading in binary mode
twitter <- readLines(twitterLocation, encoding = "UTF-8", skipNul=TRUE)


#Before sampling let's a first view on these 3 files.
## Number of lines
lB<- length(blogs) 
lN <- length(news)  
lT <- length(twitter) 

## Counting the Words
sB<- sum(stri_count_words(blogs))
sN<- sum(stri_count_words(news))  
sT <- sum(stri_count_words(twitter)) 

# mean number of words per line
mB<- mean(stri_count_words(blogs))
mN<- mean(stri_count_words(news))
mT <- mean(stri_count_words(twitter))


## Size of Files
sizeBlogs<- object.size(blogs)/1024^2 
sizeNews<- object.size(news)/1024^2 
sizeTwitter<- object.size(twitter)/1024^2 

summary<-data.frame(dataSource = c("blogs", "news", "twitter"),
                    Lines = c(lB,lN,lT),
                    NrWords = c(sB,sN,sT),
                    MeanNrWords = c(mB,mN,mT),
                    Size= c(sizeBlogs,sizeNews, sizeTwitter))


## Sampling the data and create a unified file. In order to reduce the size and because NLP algorithm are 
## computing consuming and our memory is limited let's sample them$

#Since we are sampling (and there's some randomness in samling) we'll set seet to enable
#reproducibility

set.seed(1604)
sTwitter <- sample(twitter, size = 50, replace = TRUE)
sBlogs <- sample(blogs, size = 50, replace = TRUE)
sNews <- sample(news, size = 50, replace = TRUE)
sampleTotal <- c(sTwitter, sBlogs, sNews)
length(sampleTotal)
writeLines(sampleTotal, "./projectData/dataFinalSample.txt")


#Let's clean the data, todo it we 're ging to use tm package which 
#has several built in functions enabling to easily clean text

fileCon<-file("./projectData/dataFinalSample.txt")
text<- readLines(fileCon)
corpus <- VCorpus(VectorSource(text))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
# Removing URL and Mails
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
# Converting to Lower
corpus <- tm_map(corpus, content_transformer(tolower))
# Removing stopwords (the,..)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Removing numbers
corpus <- tm_map(corpus, content_transformer(removeNumbers))
# WhiteSpace
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(removePunctuation), preserve_intra_word_dashes=TRUE)
# Twitter tags
tt<-function(x) gsub("RT |via", "", x)
corpus<- tm_map(corpus, content_transformer(tt))

# Twitter Usernames
tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
corpus<- tm_map(corpus, content_transformer(tun))
#remove numbers and punctuation

numb<-function(x) gsub('[[:digit:]]+', '', x)
corpus<-tm_map(corpus,content_transformer(numb))
punct<-function(x) gsub('[[:punct:]]+', '', x)
corpus<-tm_map(corpus,content_transformer(punct))
eq<-function(x) gsub("=", "", x)
corpus<-tm_map(corpus,content_transformer(eq))
alpha<-function(x) gsub("([[:alpha:]])\1+", "",x)
corpus<-tm_map(corpus,content_transformer(alpha))
# Text stemming
corpus <- tm_map(corpus, stemDocument)

# Transforming into term docuemnt matrix to process first basic analysis
tdm <- TermDocumentMatrix(corpus)

#Analysis based on 
v <- sort(rowSums(as.matrix(tdm)),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,5)
g1 <- ggplot(data=d[1:5,], aes(x = word, y = freq))+
  geom_bar(stat="identity") + coord_flip() + ggtitle("top 5 words")

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




#Let's convert and save for further purposes
corpus <- tm_map(corpus, PlainTextDocument)

#Let's save it
saveRDS(corpus, file = "./projectData/cleanedCorpus.RData")


#Let's load it
cleanCorpus<-readRDS("./projectData/cleanedCorpus.RData")

fcorpus<-data.frame(text=unlist(sapply(cleanCorpus,`[`, "content")),stringsAsFactors = FALSE)

corpusq<-corpus(fcorpus)


bigram<-quanteda::tokens(x=corpusq, what =c("word"), remove_numbers = TRUE,
                          remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                          remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE,
                          ngrams = 2, concatenator = " ")


bigFreq<-dfm(bigram)

bigFreq <- data.frame(words = featnames(bigFreq), freq= colSums(bigFreq), 
                       row.names = NULL, sbingsAsFactors = FALSE)
bigFreq<- bigFreq[order(bigFreq$freq,decreasing = TRUE),]

plotBig <- ggplot(bigFreq[1:20,], aes(reorder(words, -freq), freq)) +
  labs(x = "bi-gram", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1)) +
  geom_bar(stat = "identity")





trigram<-quanteda::tokens(x=corpusq, what =c("word"), remove_numbers = TRUE,
                         remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                         remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE,
                         ngrams = 3, concatenator = " ")


trigFreq<-dfm(trigram)

trigFreq <- data.frame(words = featnames(trigFreq), freq= colSums(trigFreq), 
                      row.names = NULL, stringsAsFactors = FALSE)
trigFreq<- trigFreq[order(trigFreq$freq,decreasing = TRUE),]

plotTrig <- ggplot(trigFreq[1:20,], aes(reorder(words, -freq), freq)) +
  labs(x = "tri-gram", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1)) +
  geom_bar(stat = "identity")


quadram<-quanteda::tokens(x=corpusq, what =c("word"), remove_numbers = TRUE,
                          remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                          remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE,
                          ngrams = 4, concatenator = " ")


quadFreq<-dfm(quadram)

quadFreq <- data.frame(words = featnames(quadFreq), freq= colSums(quadFreq), 
                       row.names = NULL, squadngsAsFactors = FALSE)
quadFreq<- quadFreq[order(quadFreq$freq,decreasing = TRUE),]

plotQuad <- ggplot(quadFreq[1:20,], aes(reorder(words, -freq), freq)) +
  labs(x = "quad-gram", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1)) +
  geom_bar(stat = "identity")











top<-topfeatures(v,50)
l<-data.frame(top)
bigram$words <- as.character(bigram$words)
str2 <- strsplit(bigram$words,split=" ")
bigram <- transform(bigram, 
                    one = sapply(str2,"[[",1),   
                    two = sapply(str2,"[[",2))
bigram <- data.frame(word1 = bigram$one,word2 = bigram$two,freq = bigram$freq,stringsAsFactors=FALSE)

## saving files 
write.csv(bigram[bigram$freq > 1,],"bigram.csv",row.names=F)
bigram <- read.csv("bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"bigram.RData")






source("multiplotFunction.R")

multiplot(plotBig,plotTrig,plotQuad,cols=2)
