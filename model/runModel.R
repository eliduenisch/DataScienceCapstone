################################################################################
#
#
# 
###############################################################################

# clean up workspace and console
rm(list=ls(all=TRUE))
cat("\014")
setwd("~/Desktop/capstone/model")
###############################
dataPath <- "../modelData/"
# freqDict1 <- readRDS(paste(dataPath,"freqDict1.Rds",sep=""))
freqDict2 <- readRDS(paste(dataPath,"freqDict2.rds",sep=""))
freqDict3 <- readRDS(paste(dataPath,"freqDict3.rds",sep=""))
freqDict4 <- readRDS(paste(dataPath,"freqDict4.rds",sep=""))
###############################
        freqDict4[2,1] = "abso"
        freqDict4[2,2] = "killer"
        freqDict4[2,5] = 10
        freqDict3[2,1] = "abso"
        freqDict3[2,2] = "killer"
        freqDict3[2,4] = 10
        freqDict2[2,1] = "abso"
        freqDict2[2,2] = "killer"
        freqDict2[2,3] = 10

inWords <- "bla gaga absot Killer "
inWords <- "you are "
# print(inWords)

###############################
inWords <- trimws(inWords)
inWords <- trimws(gsub("[^[:alpha:]]", " ", inWords))
inWords <- tolower(inWords)
inWords <- unlist(strsplit(inWords, split=" +"))
# print(inWords)
###############################


res <- data.frame(prediction=c(),frequency=c())
########## prediction using 4-grams
n <- 3
if(length(inWords)>=n){
  print("-----> 4-gram")
  
  inWords <- tail(inWords,n)
  print(inWords)
  idx <- TRUE
  for(i in 1:n){
    idx <- (idx & freqDict4[,i]==inWords[i])
  }
  if(any(idx)){
    resFd <- freqDict4[idx,c((n+1),ncol(freqDict4))]
    names(resFd) <- c("prediction","freqency")
    rownames(resFd) <- NULL
    res <- rbind(res,resFd)
  }
  
  print(res)
}
########## prediction using 3-grams
n <- 2
if(length(inWords)>=n & nrow(res)==0){
  print("-----> 3-gram")
  
  inWords <- tail(inWords,n)
  print(inWords)
  idx <- TRUE
  for(i in 1:n){
    idx <- (idx & freqDict3[,i]==inWords[i])
  }
  if(any(idx)){
    resFd <- freqDict3[idx,c((n+1),ncol(freqDict3))]
    names(resFd) <- c("prediction","freqency")
    rownames(resFd) <- NULL
    res <- rbind(res,resFd)
  }

  print(res)
}
########## prediction using 2-grams
n <- 1
if(length(inWords)>=n & nrow(res)==0){
  print("-----> 2-gram")
  
  inWords <- tail(inWords,n)
  print(inWords)
  idx <- TRUE
  for(i in 1:n){
    idx <- (idx & freqDict2[,i]==inWords[i])
  }
  if(any(idx)){
    resFd <- freqDict2[idx,c((n+1),ncol(freqDict2))]
    names(resFd) <- c("prediction","freqency")
    rownames(resFd) <- NULL
    res <- rbind(res,resFd)
  }
  
  print(res)
}
########## prediction using 1-grams
# n <- 1
# if(length(inWords)>=n & nrow(res)==0){
#   print("-----> 2-gram")
#   
#   inWords <- tail(inWords,n)
#   print(inWords)
#   idx <- TRUE
#   for(i in 1:n){
#     idx <- (idx & freqDict2[,i]==inWords[i])
#   }
#   if(any(idx)){
#     resFd <- freqDict2[idx,c((n+1),ncol(freqDict2))]
#     names(resFd) <- c("prediction","freqency")
#     rownames(resFd) <- NULL
#     res <- rbind(res,resFd)
#   }
#   
#   print(res)
# }
########## process results
# res <- res[!duplicated(res),]
# print(res)
# print(res[,2])
print("-----> overall")
res <- res[order(res[,2],decreasing=TRUE),]
res <- head(res,5)
print(res)
