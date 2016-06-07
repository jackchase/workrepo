#server.R

library(shiny)
source("AllDataManipulation_App.R")

clients <- as.matrix(unique(AD$client))
verticals <- as.matrix(unique(AD$vertical))

# This code is run once
shinyServer(
  function(input, output) {
    #this code is run once per visit
     
    output$table <- renderTable({      
      #this code is run on submit
      cat("\n------------------START--------------------")  
      cat("\nVertical: ",input$vertical)
      cat("\nGoal: ",input$goal)
      cat("\nBenchmark: ", input$benchmark)
      
      
      outputMatrix <- doallthecrap(input$vertical, input$goal, input$benchmark)
      cat("\n------------------END--------------------") 
      outputMatrix
    
    })
    output$refTable <- renderTable({
      
      refMatrix <- refTableFun()
      refMatrix
      
    })
            
 }
)

 

