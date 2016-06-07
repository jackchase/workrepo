library(stringr)
library(xtable)
FollowerMatching <- function(input1,input2) {
  LM <- input1
  LMP <- input2
  cat("\n\tin function1 : ")
  stripatl <- str_replace_all(LM$twitter_handle, "[^[:alnum:]]", "") 
  atl <- tolower(stripatl) 
  cat("\n\tin function2 : ")
  stripatlp <- str_replace_all(LMP$twitter_handle, "[^[:alnum:]]", "")
  atlp <- tolower(stripatlp)
  totallist <- c(atl,atlp)
  x <- which(duplicated(totallist))
  y <- which(duplicated(totallist, fromLast = TRUE))
  totallist[x] <- ""
  totallist[y] <- ""
  totallist <- as.data.frame(totallist)
  cat("\n\tin function3 : ")
  df <- totallist[-which(totallist == ""), ]
  cat("\n\tin function4 : ")
  df <- as.matrix(df)
  cat("\n\tin function5 : ")
  colnames(df)[1] <- "twitter_handles"
  cat("\n\tin function6 : ")
  cat("\n\tin function7 : ")
  return(df)
}
