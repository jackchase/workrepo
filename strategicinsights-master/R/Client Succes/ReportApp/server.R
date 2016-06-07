library(shiny)
source("reporttemplate.R")
cat("\nhere 1")
options(shiny.maxRequestSize=500*1024^2)
shinyServer(function(input, output) {
  cat("\nhere 2")
  output$outputtable1 <- renderTable({
    cat("\nhere 3")
      infile <- input$file1
      
      if (is.null(infile))
        return(NULL)
      outputtable1 <- makeGenderTable(infile)
      outputtable1
  })
  output$outputtable2 <- renderTable({
    cat("\nhere5")
      infile <- input$file1
      if (is.null(infile))
        return(NULL)
      outputtable2 <- makefinaltable(infile)
      outputtable2
  })
  output$outputtable3 <- renderTable({
    cat("\nhere6")
    infile <- input$file1
    if (is.null(infile))
      return(NULL)
    outputtable3 <- makeMaleKlout(infile)
    outputtable3
  })
  output$outputtable4 <- renderTable({
    cat("\nhere7")
    infile <- input$file1
    if (is.null(infile))
      return(NULL)
    outputtable4 <- makeFemaleKlout(infile)
    outputtable4
  })
})
