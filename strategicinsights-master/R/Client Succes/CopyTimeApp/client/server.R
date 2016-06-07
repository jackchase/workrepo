#server.R

library(shiny)
source("AllDataManipulation_App_Benchmarks_C.R")

AD <- read.csv("All_Data_04.06.csv", header=TRUE)

clients <- as.matrix(unique(AD$client))
verticals <- as.matrix(unique(AD$vertical))

# This code is run once
shinyServer(
  function(input, output) {
    #this code is run once per visit
    
    output$table <- renderTable({      
      #this code is run on submit
      cat("\n------------------START--------------------")  
      cat("\nVertical: ",input$client)
      cat("\nGoal: ",input$goal)
      cat("\nBenchmark: ", input$benchmark)
      
      
      outputMatrix <- doallthecrap(input$client, input$goal, input$benchmark)
      cat("\n------------------END--------------------") 
      outputMatrix
    })
    output$refTable <- renderTable({
      something <- input
      refTable <- refTableFun(something)
      return(refTable)
     
      
    })
    output$plot1 <- renderPlot({      
      #this code is run on submit
      something1 <- input
      cat("\n------------------START--------------------")  
      
      punchcard <- doallthecrap1(something1)
      cat("\n------------------END--------------------") 
      return(punchcard)
      
    })
    
    
  }
)


