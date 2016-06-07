library(stringr)

leadMatch <- function(input) {
  LM <- input
  cat("\n\tin function1 : ")
  stripEngaged <- str_replace_all(LM$Engaged, "[^[:alnum:]]", "") 
  engaged<- tolower(stripEngaged) 
  cat("\n\tin function2 : ")
  stripGraph <- str_replace_all(LM$Graph, "[^[:alnum:]]", "")
  graph<- tolower(stripGraph)
  cat("\n\tin function3 : ")
  stripLeads <- str_replace_all(LM$Leads, "[^[:alnum:]]", "") 
  leads <- tolower(stripLeads)
  cat("\n\tin function4 : ")
  # match
  match1 <- intersect(engaged, leads)
  match2 <- intersect(graph, leads)
  nrows1 <- length(match1)
  nrows2 <- length(match2)
  nrows <-nrows1+nrows2+1
  cat("\n\tin function5 : ")
  total1 <- merge(match1, match2, by=0, all = TRUE)
  colnames(total1) <- c("Number", "First Degree", "Second Degree")
  total2 <- total1[,-c(1)]
  total2 <- as.matrix(total2)
  return(total2)
}
