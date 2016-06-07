# ui.R

AD <- read.csv("../AllData_Worksheet.csv", header=TRUE)
#BM <- .0084


clients <- as.matrix(unique(AD$client))
clients <- sort(clients)
verticals <- as.matrix(unique(AD$vertical))
imps <- AD$impressions
linkImps <- AD$impressions [AD$link == "1"]
RTs <- AD$retweets
replies <- AD$replies
favs <- AD$favorites
clicks <- AD$url.clicks
engmnt <- clicks + favs + replies + RTs
overallER <- sum(engmnt)/sum(imps)
overallCTR <- sum(clicks)/sum(linkImps)

shinyUI(fluidPage(
  titlePanel("Insightpool SI tool (by Client)"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter in the appropriate information below. The chart to the right will display the probability of meeting or exceeding the specified benchmark:"),
    
      selectInput("client", 
                  label = "Choose client",
                  choices = c(sort(clients), "OVERALL"),
                  selected = clients[2]),
  
      selectInput("goal", 
                  label = "Choose goal",
                  choices = c("", "Engagement Rate", "CTR"),
                  selected = "Engagement Rate"),
            
      numericInput("benchmark", 
                   label = "Enter benchmark",
                   value = .0085, step = .001),
      
      submitButton("submit")
      #actionButton("submit", label = "Submit, yo")
            
       ),
    
    mainPanel(
  
   
      tableOutput("table"),
      tableOutput("refTable")
            
      
            )
  )
))