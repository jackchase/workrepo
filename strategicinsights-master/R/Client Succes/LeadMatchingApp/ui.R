library(shiny)
shiny.maxRequestSize=30*1024^2
shinyUI(fluidPage(
  titlePanel("Lead Matching"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values',
                         'text/plain', 
                         '.csv')),
      helpText("Files must be uploaded with headers that say Engaged, Graph, and Leads. Any other headers will not work."),
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
      submitButton(text = "Go Go Baby Go"),
      downloadButton('downloadData', 'Give me CSV')
    ),
    mainPanel(
      tableOutput("outputtable1")

    )
  )
))
