library(shiny)
#library(DT)

freqDict1 <- readRDS("freqDict1prob.rds")
freqDict2 <- readRDS("freqDict2.rds")
freqDict3 <- readRDS("freqDict3.rds")
freqDict4 <- readRDS("freqDict4.rds")
colNames <- c("word","probability")

shinyServer(function(input, output, session) {

  
  predictNextWord = function(inWords){

    ########## process user input
    inWords <- trimws(inWords)
    inWords <- trimws(gsub("[^[:alpha:]]", " ", inWords))
    inWords <- tolower(inWords)
    inWords <- unlist(strsplit(inWords, split=" +"))
    inWords <- tail(inWords,3)
    
    ########## check empty input
    if(length(inWords)==0)return(NULL)
    
    ########## the result
    res <- NULL
    
    ########## prediction using 4-grams
    n <- 3
    if(length(inWords)>=n){
      print("-----> 4-gram")
      
      inWords <- tail(inWords,n)
      # print(inWords)
      idx <- TRUE
      for(i in 1:n){
        idx <- (idx & freqDict4[,i]==inWords[i])
      }
      if(any(idx)){

        pCond <- 1
        for(word in inWords){
          idx <- freqDict1$word==word
          pWord <- freqDict1[idx,3]
          pCond <- pCond*pWord
        }
        
        predWords <- freqDict4[idx,(n+1)]
        predWordsProb <- c()
        for(word in predWords){predWordsProb <- c(predWordsProb,pCond*freqDict1[freqDict1$word==word,3])}
        res <- data.frame(word=predWords,probability=predWordsProb)
        # resFd <- freqDict4[idx,c((n+1),ncol(freqDict4))]
        # 
        # names(resFd) <- colNames
        # resFd$probability <- resFd$probability/nrow(freqDict4)
        # 
        # rownames(resFd) <- NULL
        # res <- rbind(res,resFd)
      }
      
      #print(res)
    }
    ########## prediction using 3-grams
    n <- 2
    if(length(inWords)>=n & is.null(res)){
      print("-----> 3-gram")
      
      inWords <- tail(inWords,n)
      # print(inWords)
      idx <- TRUE
      for(i in 1:n){
        idx <- (idx & freqDict3[,i]==inWords[i])
      }
      if(any(idx)){
        
        pCond <- 1
        for(word in inWords){
          idx <- freqDict1$word==word
          pWord <- freqDict1[idx,3]
          pCond <- pCond*pWord
        }
        
        predWords <- freqDict3[idx,(n+1)]
        predWordsProb <- c()
        for(word in predWords){predWordsProb <- c(predWordsProb,pCond*freqDict1[freqDict1$word==word,3])}
        res <- data.frame(word=predWords,probability=predWordsProb)
      }
      
      #print(res)
    }
    ########## prediction using 2-grams
    n <- 1
    if(length(inWords)>=n & is.null(res)){
      print("-----> 2-gram")
      
      inWords <- tail(inWords,n)
      # print(inWords)
      idx <- TRUE
      for(i in 1:n){
        idx <- (idx & freqDict2[,i]==inWords[i])
      }
      if(any(idx)){

        pCond <- 1
        for(word in inWords){
          idx <- freqDict1$word==word
          pWord <- freqDict1[idx,3]
          pCond <- pCond*pWord
        }
        
        predWords <- freqDict2[idx,(n+1)]
        predWordsProb <- c()
        for(word in predWords){predWordsProb <- c(predWordsProb,pCond*freqDict1[freqDict1$word==word,3])}
        res <- data.frame(word=predWords,probability=predWordsProb)
      }
      
      #print(res)
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
    
    ########## check empty result
    if(is.null(res))return(res)
    
    
    res <- res[order(res[,2],decreasing=TRUE),]
    res <- head(res,5)
    rownames(res) <- NULL
    
    return(res)
  }
  
  output$predictions = renderDataTable(
    predictNextWord(input$userInput),
    # colnames = c("word","probability"),
    # rownames=NULL, 
    # selection = 'none',
    options=list(
                  bInfo=FALSE,
                  searching=FALSE,
                  paging=FALSE,
                  # selection='none',
                  pageLength=5
    )
  )
  
  output$context = renderDataTable(
    values$df,
    # colnames = c("word","frequency","probability"),
    # rownames=NULL, 
    # selection = 'none',
    options=list(
      bInfo=FALSE,
      searching=FALSE,
      paging=FALSE
    )
  )
  
  
  values <- reactiveValues()
  values$df <- data.frame(word=c(),frequency=c(),probability=c())
 
  values$lastUserInputLength <- 0


  
  observeEvent(input$userInput, {
    
    inWords <- isolate(input$userInput)
    inWords <- trimws(inWords)
    inWords <- trimws(gsub("[^[:alpha:]]", " ", inWords))
    inWords <- tolower(inWords)
    inWords <- unlist(strsplit(inWords, split=" +"))
    word <- tail(inWords,1)
    if(length(word)==0 || nchar(word)<3)return(NULL)
    if(values$lastUserInputLength<length(inWords)){
      idxCC <- 0
      isolate({
        rows <- nrow(values$df)
        if(rows>0){
          for(i in 1:rows){
            w<-values$df[[i,1]]
            if(w==word){
              idxCC<-i
              values$df[nrow(values$df),2]<-values$df[nrow(values$df),2]+1
              values$df[nrow(values$df),3]<-values$df[nrow(values$df),3]*2
              }
          }
        }
     })
     print(idxCC)
      print("new")
      idx <- freqDict1$word==word
      if(idxCC==0 && sum(idx)>0){
        pWord <- freqDict1[idx,3]
        isolate(values$df[nrow(values$df)+1,1]<-word)
        isolate(values$df[nrow(values$df),2]<-1)
        isolate(values$df[nrow(values$df),3]<-pWord)
      }
    
    } 
    isolate(values$lastUserInputLength <- length(inWords))
})
 
  
})