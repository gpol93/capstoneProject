

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
sTwitter <- sample(twitter, size = 5000, replace = TRUE)
sBlogs <- sample(blogs, size = 5000, replace = TRUE)
sNews <- sample(news, size = 5000, replace = TRUE)
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

findAssocs(tdm, terms = "will", corlimit = 0.3)


#Let's convert and save for further purposes
corpus <- tm_map(corpus, PlainTextDocument)

#Let's save it
saveRDS(corpus, file = "./finaldata/cleanedCorpus.RData")


#Nased on the final corpus let's generate the n-grams

bigram <-NGramTokenizer(corpus, Weka_control(min = 2, max = 2))
trigram <- NGramTokenizer(corpus, Weka_control(min = 3, max = 3))
quadgram <- NGramTokenizer(corpus, Weka_control(min = 4, max = 4))
fivegram<-NGramTokenizer(corpus, Weka_control(min = 5, max = 5))


bigram<-data.frame(table(bigram))
trigram<-data.frame(table(trigram))

gramPlot <- function(data, label) {
  ggplot(data[1:30,], aes(reorder(word, -freq), freq)) +
    labs(x = label, y = "Frequency") +
    theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1)) +
    geom_bar(stat = "identity", fill = I("blue"))
}

bigramPLot<-gramPlot(bigram)