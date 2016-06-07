library(RPostgreSQL)
#install.packages("RPostgreSQL")

library(dplyr)
setwd("./strategicinsights")
INTERACTION_USER_NAME = 8
INTERACTION_CONTENT = 9
TWITTER_USER_SCREEN_NAME = 56
TWITTER_USER_FOLLOWER_COUNT = 51
TWITTER_RETWEETED_SCREEN_NAME = 89
TWITTER_RETWEET_FOLLOWER_COUNT = 68
INTERACTION_CREATED_DATE = 10
START_DATE = as.Date("2015-02-07")
END_DATE = as.Date("2015-02-14")

#influencerList <- read.csv(file = "./Nokia/Nokia Influencers.csv")
db <- src_postgres(host="192.168.1.119", user="thoth")
influencerList <- read.csv(file = "./Nokia/LSInfluencers.csv")


influencerList[,1] <- sort(influencerList[,1])


#ds004 == Nokia_5 (retweet data)
#ds005 == Nokiashare (global share of voice)
#ds006 == Nokia Influencer Standard 

#ds004 - RETWEETS
#twitter_retweet_user_screen_name = user that RTs influencer
#twitter_retweet_user_followers_count = RTers follower count
#twitter_retweeted_user_screen_name = influencer that has been RTd

#ds005 - GLOBAL
#twitter_user_followers_count = all contributors' reach

#ds006 - INFLUENCERS
#twitter_user_screen_name = influencer username
#twitter.user.followers_count = influencer follower count

#ds1 <- tbl(db, "ds009")
#retweetData <- ds1 %>% collect() 
#rdMatrix <- as.matrix(retweetData)
#rdMatrix <- rdMatrix[!is.na(rdMatrix[,1]),]
# retweetData2 <- retweetData1[order(retweetData1$interaction_created_at),]
# retweetData <- retweetData2[138:259,]

# ds5 <- tbl(db, "ds011")
# retweetFeb1 <- ds5 %>% collect() 



ds2 <- tbl(db, "ds014")
globalData <- collect(ds2)
gdMatrix <- as.matrix(globalData)
gdMatrix <- gdMatrix[!is.na(gdMatrix[,1]),]
gdMatrix[,TWITTER_USER_SCREEN_NAME] <- tolower(gdMatrix[,TWITTER_USER_SCREEN_NAME])
gdMatrix[,INTERACTION_USER_NAME] <- tolower(gdMatrix[,INTERACTION_USER_NAME])
gdMatrix[,TWITTER_RETWEETED_SCREEN_NAME] <- tolower(gdMatrix[,TWITTER_RETWEETED_SCREEN_NAME])
# globalData2 <- globalData1[order(globalData1$interaction_created_at),]
# globalData <- globalData2[307000:477199,]
#testGlobal <- gdMatrix[gdMatrix[,TWITTER_USER_SCREEN_NAME] == "alvetica",]
#testGlobal <- testGlobal[!is.na(testGlobal[,1]),]
#View(testGlobal)

#ds3 <- tbl(db, "ds010")
#influencerData <- collect(ds3)
#idMatrix <- as.matrix(influencerData)
# influencerData2 <- influencerData1[order(influencerData1$interaction_created_at),]
# influencerData <- influencerData2[114:260,]
#test <- idMatrix[idMatrix[,TWITTER_USER_SCREEN_NAME] == "tednguyen",]
#View(test)
# ds4 <- tbl(db, "ds012")
# infFeb1 <- ds4 %>% collect()



# retweetData <- tbl(db, "ds004") %>% collect()
# globalData <- tbl(db, "ds005") %>% collect()
# influencerData <- tbl(db, "ds006") %>% collect()


######################

library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

#influencerFC1 <- read.csv("Nokia/Nokia Influencers.csv", header = TRUE)
#influencerFC <- influencerFC1$Handles
#influencerDS <- unique(influencerData$twitter_user_screen_name)

# StripInfluencersFC <- str_replace_all(influencerFC, "[^[:alnum:]]", "") 
# influencerFC<- tolower(StripInfluencersFC) 
# 
# StripInfluencersDS <- str_replace_all(influencerDS, "[^[:alnum:]]", "") 
# influencerDS<- tolower(StripInfluencersDS) 

########################
#----filter by date----# 
########################
#  gdDates <- as.Date(gdMatrix[,INTERACTION_CREATED_DATE])
#  gdMatrix <- gdMatrix[gdDates<=START_DATE,]



#influencers <- unique(influencerData$twitter_user_screen_name)
outputMatrix <- matrix(ncol= 5, nrow=(nrow(influencerList)))
colnames(outputMatrix) = c("Influencer","Number of Tweets", "Total Imps", "Number of RTs", "RT Imps")

influencerMatrix <- matrix(ncol = 2, nrow = 0)
for (index in 1:nrow(influencerList)) {
  influencerName = tolower(influencerList[index,1])
  numOfTweets <- 0
  totalImp <- 0
  numOfRTs <- 0
  rtImps <- 0
  
  
  allTweets <- gdMatrix[gdMatrix[,TWITTER_USER_SCREEN_NAME] == influencerName,]
  allTweets <- allTweets[!is.na(allTweets[,1]),]
  numOfTweets <- nrow(allTweets)
  if(is.null(numOfTweets)) {
    allTweets <- t(allTweets)
    numOfTweets <- nrow(allTweets)
  }
  if(numOfTweets > 0) {
    totalImp <- sum(as.numeric(allTweets[,TWITTER_USER_FOLLOWER_COUNT]))
    littleMatrix <- matrix(ncol = 2, nrow = numOfTweets)
    for (tweetIndex in 1:numOfTweets) {
      littleMatrix[tweetIndex, 1] <- influencerName
      littleMatrix[tweetIndex, 2] <- allTweets[tweetIndex, INTERACTION_CONTENT]
    }
    influencerMatrix <- rbind(influencerMatrix, littleMatrix)
  }
  allRTs <- gdMatrix[gdMatrix[,TWITTER_RETWEETED_SCREEN_NAME] == influencerName,]
  allRTs <- allRTs[!is.na(allRTs[,1]),]
  numOfRTs <- nrow(allRTs)
  if(is.null(numOfRTs)) {
    allRTs <- t(allRTs)
    numOfRTs <- nrow(allRTs)
  }
  if (numOfRTs > 0) {
    rtImps <- sum(as.numeric(allRTs[,TWITTER_RETWEET_FOLLOWER_COUNT]))
  }
  cat("\n", influencerName, " : ", numOfTweets, " : ", totalImp, " : ", numOfRTs, " : ", rtImps)
  outputMatrix[index, 1] <- influencerName
  outputMatrix[index, 2] <- numOfTweets
  outputMatrix[index, 3] <- totalImp
  outputMatrix[index, 4] <- numOfRTs
  outputMatrix[index, 5] <- rtImps
}

colnames(influencerMatrix) = c("Influencer","Message")

NokiaOutputV2 <- file("NokiaOutputV2Weekly.csv", open="w")
write.csv(file=NokiaOutputV2, x=influencerMatrix)
write.csv(file=NokiaOutputV2, x=outputMatrix)
closeAllConnections()



# imps <- influencerData$twitter.user.followers_count[!is.na(match(Username, mat))]
# username <- Username[!is.na(match(Username, mat))]
# total <- cbind(username, as.vector(tweets),imps)
# colnames(total) <- c("Username","Tweets", "Impressions")
# testFile <- file("NokiaOutput.csv", open="w")
# write.csv(file=testFile, x=total)
# write.csv(file=testFile, x=influencerMatrix)
# closeAllConnections()
