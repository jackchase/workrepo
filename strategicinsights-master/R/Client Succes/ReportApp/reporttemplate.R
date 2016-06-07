library(dplyr)
library(tidyr)
library(stringi)
library(stringr)

makeGenderTable <- function(input) {
  cat("\n\tin function1 : ")
  datapath = input$datapath
  LM <- read.csv(file = datapath, header = TRUE)
  cat("\n\tafter read")
  males <- LM$gender == "m"
  malecount <- length(males[males==TRUE])
  
  females <- LM$gender == "f"
  femalecount <- length(females[females==TRUE])
  
  total_gender <- malecount+femalecount
  
  male_perc <- round(malecount/total_gender * 100)
  female_perc <- round(femalecount/total_gender * 100)

  gendertable <- matrix(c(male_perc, female_perc), nrow=1, byrow=TRUE)
  colnames(gendertable) <- c("Males", "Females")
  gendertable <- data.frame(gendertable)
  return(gendertable)
}

makefinaltable <- function(input) {
  datapath = input$datapath
  LM <- read.csv(file = datapath, header = TRUE)
  male_avg_fol <- round(mean(LM$followers_count[LM$gender == "m"], na.rm = TRUE))
  male_avg_tweets <- round(mean(LM$statuses_count[LM$gender == "m"], na.rm = TRUE))
  female_avg_fol <- round(mean(LM$followers_count[LM$gender == "f"], na.rm = TRUE))
  female_avg_tweets <- round(mean(LM$statuses_count[LM$gender =="f"], na.rm = TRUE))
  finaltable <- matrix(c(male_avg_fol,male_avg_tweets,female_avg_fol,female_avg_tweets), nrow=1, byrow=TRUE)
  
  colnames(finaltable) <- c("Male Average Followers","Male Average Tweets","Female Average Followers","Female Average Tweets")
  return(finaltable)
}

makeMaleKlout <- function(input) {
  datapath = input$datapath
  cat("in loop male")
  LM <- read.csv(file = datapath, header = TRUE)
  male_klout <- (LM$klout_topics[LM$gender == "m"])
  male_klout <- data.frame(male_klout)
  colnames(male_klout) <- c("klout_topics")
  
  male_klout <- tbl_df(male_klout)
  
  male_klout <- mutate(male_klout, klout_topics=stri_replace_all_fixed(klout_topics, c("[", "]"), "", vectorize_all=FALSE))
  male_klout <- mutate(male_klout, klout_topics=stri_replace_all_fixed(klout_topics, "\"", ""))
  male_klout <- separate(male_klout, klout_topics, into=c("mt1", "mt2", "mt3", "mt4", "mt5"), sep=", ", extra="drop")
  
  topicsmale <- bind_rows(
    select(male_klout, MaleTopics=mt1),
    select(male_klout, MaleTopics=mt2),
    select(male_klout, MaleTopics=mt3),
    select(male_klout, MaleTopics=mt4),
    select(male_klout, MaleTopicsmt=mt5))
  
  MaleKloutTopics <- topicsmale %>% count(MaleTopics, sort=TRUE)
  MaleKloutTopics25 <- MaleKloutTopics[1:25,1:2]
  MaleKloutTopics <- data.frame(MaleKloutTopics25)
  return(MaleKloutTopics)
}

makeFemaleKlout <- function(input) {
  cat("trying to get in female")
  datapath = input$datapath
  cat("in loop female")
  LM <- read.csv(file = datapath, header = TRUE)
  female_klout <- (LM$klout_topics[LM$gender == "f"])
  female_klout <- data.frame(female_klout)
  colnames(female_klout) <- c("klout_topics")
  
  female_klout <- tbl_df(female_klout)
  
  female_klout <- mutate(female_klout, klout_topics=stri_replace_all_fixed(klout_topics, c("[", "]"), "", vectorize_all=FALSE))
  female_klout <- mutate(female_klout, klout_topics=stri_replace_all_fixed(klout_topics, "\"", ""))
  female_klout <- separate(female_klout, klout_topics, into=c("kt1", "kt2", "kt3", "kt4", "kt5"), sep=", ", extra="drop")
  
  topicsfemale <- bind_rows(
    select(female_klout, FemaleTopics=kt1),
    select(female_klout, FemaleTopics=kt2),
    select(female_klout, FemaleTopics=kt3),
    select(female_klout, FemaleTopics=kt4),
    select(female_klout, FemaleTopics=kt5))
  
  FemaleKloutTopics = topicsfemale %>% count(FemaleTopics, sort=TRUE)
  FemaleKloutTopics25 <- FemaleKloutTopics[1:25,1:2]
  FemaleKloutTopics <- data.frame(FemaleKloutTopics25)
  return(FemaleKloutTopics)
}
