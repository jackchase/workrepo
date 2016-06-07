library(shiny)
shiny.maxRequestSize=30*1024^2
shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values',
                         'text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      submitButton(text = "Go Go Baby Go")
    ),
    mainPanel(
      tableOutput("outputtable1"),
      tableOutput("outputtable2"),
      tableOutput("outputtable3"),
      tableOutput("outputtable4")

    )
  )
))
