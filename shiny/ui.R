library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Next-word prediction using Katz's backoff model extended with user-generated context"),
  
  sidebarLayout(
    
    sidebarPanel(
      includeMarkdown("docs.Rmd")
    ),
    
    mainPanel(
          h3("Enter a phrase:"),
          textInput(inputId="userInput",label="",value=""),
          hr(),
          h4("Predicted next word (Top-5 in decreasing order):"),
          dataTableOutput("predictions"),
          hr(),
          h4("Context corpus from collected input (words with increased probabilities for next predictions):"),
          dataTableOutput("context")
          
  )
)
))