library(shiny)
shiny.maxRequestSize=300*1024^2
shinyUI(fluidPage(
  titlePanel("Follower Matching"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Current Followers',
                accept=c('text/csv', 
                         'text/comma-separated-values',
                         'text/plain', 
                         '.csv')),
      fileInput('file2', 'Pre Insightpool Followers',
                accept=c('text/csv', 
                         'text/comma-separated-values',
                         'text/plain', 
                         '.csv')),
      helpText("Files must be uploaded with only ONE header that is exactly twitter_handle and must be filetypes of CSV"),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
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
