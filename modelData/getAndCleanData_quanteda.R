################################################################################
#
#
# 
################################################################################
setwd("~/Desktop/capstone/modelData")

# clean up workspace and console
rm(list=ls(all=TRUE))
cat("\014")

# increase rJava/RWeka Java heap-size using JVM -Xmx flag
#options(java.parameters="-Xmx5g")

# load libraries
#library(tm)
# library(magrittr)

#library(RWeka)
#library(slam)
################################################################################
# setup script
dataPortion <- 0.01
minN <- 1
maxN <- 1
set.seed(1)

# set path, file-names etc.
dataURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
dataFileZip <-"Coursera-SwiftKey.zip"
dataFileString <- paste("dataString-",gsub("\\.","-",dataPortion),".rds",sep="") 
dataFileDir <- "./dataFilesRaw"
dataFileZipPath <- "final/en_US/"
dataFiles <- c("en_US.blogs.txt",
               "en_US.news.txt",
               "en_US.twitter.txt")
################################################################################
# download data file
if(!file.exists(dataFileZip)){
  download.file(dataURL,destfile=dataFileZip)
}

# create data directory and unzip
if(!file.exists(dataFileDir)){
  dir.create(dataFileDir)
  unzip("Coursera-SwiftKey.zip",
        files=paste(dataFileZipPath,dataFilesRaw,sep=""),
        junkpaths=TRUE,
        exdir=dataFileDir) 
}

# read text-files, draw sample, combine, clean-up and save corpus as .rds
if(!file.exists(dataFileString)){
  print(paste("----->","reading data files",Sys.time()))
  dataRawCollected <- c()
  for(file in dataFiles){
    print(paste("->",file,Sys.time()))
    conn <- file(paste(dataFileDir,"/",file,sep=""), open="rb")
    dataRawFile <- readLines(conn, encoding="UTF-8",skipNul=TRUE)
    close(conn)
    numberLines <- floor(length(dataRawFile)*dataPortion)
    dataRawSample <- sample(dataRawFile,size=numberLines,replace=FALSE)
    dataRawCollected <- c(dataRawCollected,dataRawSample)
    rm(dataRawFile,dataRawSample)
  }
  
  dataString <- paste(dataRawCollected,collapse=", ")
  
  print(paste("----->","applying regex",Sys.time()))  
  dataString <- gsub("_","",dataString)
  # print(paste("->","lowercase",Sys.time())) 
  # dataString <- tolower(dataString)
  # gsub("[^[:alpha:]]", " ", .)
  #print(paste("->","remove special words",Sys.time())) 
  # dataString <- gsub("#\\w+ *", "", dataString)
  # dataString <- gsub("@\\w+ *", "", dataString)
  # print(paste("->","remove URLs",Sys.time())) 
  dataString <- gsub("http[[:alnum:]]*", "", dataString)
  dataString <- gsub("www[[:alnum:]]*", "", dataString)
  # print(paste("->","remove junk words",Sys.time())) 
  dataString <- gsub("([[:alpha:]])\1+", "", dataString)
  
  # print(paste("->","removing stopwords",Sys.time())) 
  #   corpus <-   tm_map(corpus, removeWords, stopwords("english"))
  print(paste("----->","saving corpus string",Sys.time()))
  saveRDS(dataString,dataFileString)
  rm(dataString)
}

require(quanteda)
print(paste("----->","loading corpus string from ",dataFileString,Sys.time()))
dataString <- readRDS(dataFileString)

for(n in minN:maxN){
  fileName <- paste("freqDict",n,".rds",sep="")
  print(paste("----->",fileName,Sys.time()))
  print(paste("->","tokenizing",Sys.time()))
  dataTokens <- tokenize(toLower(dataString), 
                         removePunct=TRUE,
                         removeNumbers=TRUE,
                         removeSeparators=TRUE,
                         removeTwitter=TRUE,
                         #stem=FALSE,
                         verbose=TRUE,
                         #ignoredFeatures=stopwords("english"),
                         ngrams=n)
  rm(dataString)
  print(paste("->","building DFM",fileName,Sys.time()))
  dataDFM <- dfm(dataTokens)
  print(paste("->","extracting data from DFM",fileName,Sys.time()))
  nGramFreq <- dataDFM@x
  nGrams <- features(dataDFM)
  #nGrams <- dataDFM@Dimnames$features
  rm(dataDFM)
  nGramWords <- strsplit(as.character(nGrams),split="_")
  
  print(paste("->","building dataframe",fileName,Sys.time()))
  nGramFreqDict <- as.data.frame(matrix(unlist(nGramWords), ncol=n, byrow = TRUE),
                                 stringsAsFactors = FALSE)
  nGramFreqDict <- cbind(nGramFreqDict,nGramFreq)
  rm(nGramWords,nGramFreq)
  names(nGramFreqDict) <- c((paste("token_",c(1:n),sep="")),"nGramFrequency")
  print(paste("->","saving dataframe",fileName,Sys.time()))
  nGramFreqDict <- nGramFreqDict[order(nGramFreqDict$nGramFrequency,decreasing=TRUE),]
  saveRDS(nGramFreqDict,fileName)
  rm(nGramFreqDict)
  
  # print(paste("----->","building",fileName,Sys.time()))
  # print(paste("->","tdm tokenizing",fileName,Sys.time()))
  # tokenizer <- function(x) NGramTokenizer(x, Weka_control(min=n, max=n))
  # tdm <- TermDocumentMatrix(corpus, control=list(tokenize=tokenizer))
  # #inspect(tdm)
  # print(paste("->","stripping frequencies and strings",fileName,Sys.time()))
  # nGramFreq <- row_sums(tdm)
  # rm(tdm)
  # nGramStrings <- names(nGramFreq)
  # names(nGramFreq) <- NULL
  # print(paste("->","strings to words",fileName,Sys.time()))
  # nGramWords <- strsplit(as.character(nGramStrings),split=" ")
  # rm(nGramStrings)
  # print(paste("->","storing in dataframe",fileName,Sys.time()))
  # nGramFreqDict <- as.data.frame(matrix(unlist(nGramWords), ncol=n, byrow = TRUE),
  #                                stringsAsFactors = FALSE)
  # nGramFreqDict <- cbind(nGramFreqDict,nGramFreq)
  # names(nGramFreqDict) <- c((paste("token_",c(1:n),sep="")),"nGramFrequency")
  # print(paste("->","saving dataframe",fileName,Sys.time()))
  # nGramFreqDict <- nGramFreqDict[order(nGramFreqDict$nGramFrequency,decreasing=TRUE),]
  # saveRDS(nGramFreqDict,fileName)
  # rm(nGramWords,nGramFreqDict)
}
