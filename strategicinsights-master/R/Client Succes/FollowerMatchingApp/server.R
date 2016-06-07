library(shiny)
source("FollowerMatching.R")
cat("\nhere 1")
options(shiny.maxRequestSize=500*1024^2)
shinyServer(function(input, output) {
  output$outputtable1 <- renderTable({
      cat("\nhere 3")
      infile1 <- input$file1
      if (is.null(infile1))
        return(NULL)
      something1 <- read.csv(infile1$datapath, header=TRUE)
      cat("\nhere 4")
      infile2 <- input$file2
      if (is.null(infile2))
        return(NULL)
      something2 <- read.csv(infile2$datapath, header=TRUE)
      cat("\nhere 5")
      outputtable1 <- FollowerMatching(something1,something2)
      cat("\nhere 6")
      return(outputtable1)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("dump", '.csv', sep='') },
    content = function(file) {
      cat("\nhere 3")
      infile1 <- input$file1
      if (is.null(infile1))
        return(NULL)
      something1 <- read.csv(infile1$datapath, header=TRUE)
      cat("\nhere 4")
      infile2 <- input$file2
      if (is.null(infile2))
        return(NULL)
      something2 <- read.csv(infile2$datapath, header=TRUE)
      cat("\nhere 5")
      outputtable1 <- FollowerMatching(something1,something2)
      write.csv(outputtable1, file)
    })
})
