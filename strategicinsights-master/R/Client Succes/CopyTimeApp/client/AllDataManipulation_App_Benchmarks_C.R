#setwd("/Users/brentducote/strategicinsights/Benchmarks/by client")
AD <- read.csv("All_Data_04.06.csv", header=TRUE)
# clients <- as.matrix(unique(AD$client))
# verticals <- as.matrix(unique(AD$vertical))
# 
# ####  FROM APP
# 
# # goal = "Engagement Rate"
# # client= "Leviton"
# # benchmark = .0085

doallthecrap <- function(client,goal,benchmark) { 
  
  #variables 0s included (for rates)
  imps <- AD$impressions
  linkImps <- AD$impressions
  RTs <- AD$retweets
  replies <- AD$replies
  favs <- AD$favorites
  clicks <- AD$url.clicks
  engmnt <- clicks + favs + replies + RTs
  overallER <- sum(engmnt, na.rm = TRUE)/sum(imps, na.rm = TRUE)
  overallCTR <- sum(clicks, na.rm = TRUE)/sum(linkImps, na.rm = TRUE)
  
  
  
  AD <- AD[AD$client == client,]
  
  
  clients <- as.matrix(unique(AD$client))
  verticals <- as.matrix(unique(AD$vertical))
  
  
  TEST_START_INDEX = 20
  TEST_END_INDEX = 35
  tests = colnames(AD[TEST_START_INDEX:TEST_END_INDEX])
  
  outputMatrix = matrix(ncol= 4, nrow=(length(tests)))
  colnames(outputMatrix) = c("Test Name", "With", "Without", "% Diff")
  outputMatrix[,1] = tests
  
  
  ############################# ER test 
  if (exists("goal") && goal == "Engagement Rate"){
    cat("ER TRUE");
    
    withVsWithout <- function (dataCol) {  
      
      withX= (RTs[dataCol == 1] + replies[dataCol == 1] + favs[dataCol == 1] + clicks[dataCol == 1]) / imps[dataCol == 1]
      withX[imps[dataCol==1]==0] <- NA
      withX <- na.omit(withX)
      
      withoutX= (RTs[dataCol == 0] + replies[dataCol == 0] + favs[dataCol == 0] + clicks[dataCol == 0]) / imps[dataCol == 0]
      withoutX[imps[dataCol==0]==0] <- NA
      withoutX <- na.omit(withoutX)
      
      WithXGlobal <<- withX
      WithoutXGlobal <<- withoutX
      
      meanWithX = (sum(RTs[dataCol == "1"], na.rm = TRUE) + sum(replies[dataCol == "1"], na.rm = TRUE) + sum(favs[dataCol == "1"], na.rm = TRUE) + sum(clicks[dataCol == "1"], na.rm = TRUE)) / sum(imps[dataCol == "1"], na.rm = TRUE)
      withXsd = sd(withX)
      
      meanWithoutX = (sum(RTs[dataCol == "0"], na.rm = TRUE) + sum(replies[dataCol == "0"], na.rm = TRUE) + sum(favs[dataCol == "0"], na.rm = TRUE) + sum(clicks[dataCol == "0"], na.rm = TRUE)) / sum(imps[dataCol == "0"], na.rm = TRUE)
      withoutXsd = sd(withoutX)
      
      zero1<<- length(WithXGlobal[WithXGlobal==0])/length(WithXGlobal)
      zero2<<- length(WithoutXGlobal[WithoutXGlobal==0])/length(WithoutXGlobal)
      
      Xpd = abs(meanWithX-meanWithoutX)/((meanWithX+meanWithoutX)/2)
      
      outputTable <- matrix(c(meanWithX,withXsd,zero1,meanWithoutX,withoutXsd,zero2,Xpd,"",""),ncol=3,byrow=TRUE)
      colnames(outputTable) <- c("Engagement Rate", "Std. Dev", "% of Zeroes")
      rownames(outputTable) <- c("With", "Without","Percent Difference")
      
      return(outputTable)
    }
    rowIndex = 1
    
    for (loopIndex in seq(from = TEST_START_INDEX, length.out = length(tests), by = 1)) {     
      outputTable <- withVsWithout(AD[,loopIndex])
      
      calculatePDF <- function(arrayOfValues) {
        interval = .0001
        list = seq(0,.05,interval)
        
        PDFmatrix = matrix(ncol=2,nrow=length(list)-1)
        PDFmatrix[,1] = list[1:length(list)-1]
        PDFmatrix[,2] = 0
        for (index in 2:length(list)) {
          preValue = list[index-1]
          currentValue = list[index]
          
          #PDF
          PDF =  length(arrayOfValues[arrayOfValues > preValue & arrayOfValues <= currentValue])/length(arrayOfValues[arrayOfValues>0])
          
          PDFmatrix[index-1,2] = PDF
          
        }
        return(PDFmatrix)
      }
      
      convertPDFtoCDF <- function(pdfMatrix) {
        cdfMatrix = matrix(ncol = 2, nrow = length(pdfMatrix[,1]))
        cdfMatrix[,1] = pdfMatrix[,1]
        summation = 0
        
        for (index in 1:length(pdfMatrix[,1])) {
          summation = summation + pdfMatrix[index,2]
          cdfMatrix[index,2] = summation
        }
        
        return(cdfMatrix)
      } 
      
      pdfMatrixERWith = calculatePDF(WithXGlobal)
      cdfMatrixERWith = convertPDFtoCDF(pdfMatrixERWith)
      pdfMatrixERWithout = calculatePDF(WithoutXGlobal)
      cdfMatrixERWithout = convertPDFtoCDF(pdfMatrixERWithout)
      
      cdfMatrixERWith1 = floor(cdfMatrixERWith*10000)/10000
      cdfMatrixERWithout1 = floor(cdfMatrixERWithout*10000)/10000
      
      cdfERBMWith = cdfMatrixERWith1[cdfMatrixERWith1[,1] == abs(benchmark),2]
      cdfERBMWithout = cdfMatrixERWithout1[,2][cdfMatrixERWithout1[,1] == abs(benchmark)]
      prob1With = (1- cdfERBMWith)
      prob1Without = (1-cdfERBMWithout)
      prob2With = prob1With*(1-as.numeric(outputTable[1,3]))
      prob2Without = prob1Without*(1-as.numeric(outputTable[2,3]))
      probPD = (prob2With-prob2Without)/((prob2With+prob2Without)/2)
      
      prob2With1 = round(prob2With*100,2)
      prob2Without1 = round(prob2Without*100,2)
      probPD1 = round(probPD*100,2)
      
      outputMatrix[rowIndex,2] = prob2With1
      outputMatrix[rowIndex,3] = prob2Without1
      outputMatrix[rowIndex,4] = probPD1
      rowIndex = rowIndex + 1
      
      #   CDFatBMapp <- CDFatBM[,3]
      #   CDFatBMapp <- cbind(c("With","Without","% diff"),c("","",""), CDFatBMapp)
      #   YN <- ifelse(CDFatBMapp[1,3]>CDFatBMapp[2,3],"Y","N")
      #   NY <- ifelse(CDFatBMapp[2,3]>CDFatBMapp[1,3],"Y","N")
      #   CDFatBMapp[1,2] <- YN
      #   CDFatBMapp[2,2] <- NY
      #   CDFatBMapp[3,2] <- "---"
      #   colnames(CDFatBMapp) <- c("TEST NAME","Y/N", "CDF (%)")
      #   
      
      # colnames(CDFatBM) <- c("CDF at Benchmark", "Probability >= to BM", "Probability >= BM *with zeroes")
      
    }
  }
  
  ############################# CTR test
  if (exists("goal") && goal == "CTR"){
    
    #cat("CTR TRUE");
    cat("loop restart")
    
    withVsWithoutCTR <- function (dataCol) {  
      withX= clicks[dataCol == 1]/ linkImps[dataCol == 1]
      withX[linkImps[dataCol==1]==0] <- NA
      withX <- na.omit(withX)
      
      
      withoutX= clicks[dataCol == 0] / linkImps[dataCol == 0]
      withoutX[linkImps[dataCol==0]==0] <- NA
      withoutX <- na.omit(withoutX)
      
      
      WithXGlobal <<- withX
      WithoutXGlobal <<- withoutX
      
      
      meanWithX = sum(clicks[dataCol == "1"], na.rm = TRUE) / sum(na.omit(linkImps[dataCol == "1"], na.rm = TRUE))
      withXsd = sd(withX)
      
      meanWithoutX = sum(clicks[dataCol == "0"], na.rm = TRUE) / sum(na.omit(linkImps[dataCol == "0"], na.rm = TRUE))
      withoutXsd = sd(withoutX)
      
      zero1<<- length(WithXGlobal[WithXGlobal==0])/length(WithXGlobal)
      zero2<<- length(WithoutXGlobal[WithoutXGlobal==0])/length(WithoutXGlobal)
      
      Xpd = abs(meanWithX-meanWithoutX)/((meanWithX+meanWithoutX)/2)
      
      outputTable <- matrix(c(meanWithX,withXsd,zero1,meanWithoutX,withoutXsd,zero2,Xpd,"",""),ncol=3,byrow=TRUE)
      colnames(outputTable) <- c("CTR", "Std. Dev", "% of Zeroes")
      rownames(outputTable) <- c("With", "Without","Percent Difference")
      
      return(outputTable)
    }
    
    rowIndex = 1
    for (loopIndex in seq(from = TEST_START_INDEX, length.out = length(tests), by = 1)) {     
      outputTable <- withVsWithoutCTR(AD[,loopIndex])  
      
      
      calculatePDF <- function(arrayOfValues) {
        interval = .0001
        list = seq(0,.05,interval)
        
        PDFmatrix = matrix(ncol=2,nrow=length(list)-1)
        PDFmatrix[,1] = list[1:length(list)-1]
        PDFmatrix[,2] = 0
        for (index in 2:length(list)) {
          preValue = list[index-1]
          currentValue = list[index]
          
          #PDF
          PDF =  length(arrayOfValues[arrayOfValues > preValue & arrayOfValues <= currentValue])/length(arrayOfValues[arrayOfValues>0])
          PDFmatrix[index-1,2] = PDF
          
        }
        return(PDFmatrix)
      }
      
      convertPDFtoCDF <- function(pdfMatrix) {
        cdfMatrix = matrix(ncol = 2, nrow = length(pdfMatrix[,1]))
        cdfMatrix[,1] = pdfMatrix[,1]
        summation = 0
        for (index in 1:length(pdfMatrix[,1])) {
          summation = summation + pdfMatrix[index,2]
          cdfMatrix[index,2] = summation
        }
        return(cdfMatrix)
      } 
      
      pdfMatrixERWith = calculatePDF(WithXGlobal)
      cdfMatrixERWith = convertPDFtoCDF(pdfMatrixERWith)
      pdfMatrixERWithout = calculatePDF(WithoutXGlobal)
      cdfMatrixERWithout = convertPDFtoCDF(pdfMatrixERWithout)
      
      cdfMatrixERWith1 = floor(cdfMatrixERWith*10000)/10000
      cdfMatrixERWithout1 = floor(cdfMatrixERWithout*10000)/10000
      
      cdfERBMWith = cdfMatrixERWith1[,2][cdfMatrixERWith1[,1] == abs(benchmark)]
      cdfERBMWithout = cdfMatrixERWithout1[,2][cdfMatrixERWithout1[,1] == abs(benchmark)]
      prob1With = (1- cdfERBMWith)
      prob1Without = (1-cdfERBMWithout)
      prob2With = prob1With*(1-as.numeric(outputTable[1,3]))
      prob2Without = prob1Without*(1-as.numeric(outputTable[2,3]))
      probPD = (prob2With-prob2Without)/((prob2With+prob2Without)/2)
      
      prob2With1 = round(prob2With*100,2)
      prob2Without1 = round(prob2Without*100,2)
      probPD1 = round(probPD*100,2)
      
      outputMatrix[rowIndex,2] = prob2With1
      outputMatrix[rowIndex,3] = prob2Without1
      outputMatrix[rowIndex,4] = probPD1
      rowIndex = rowIndex + 1
      
      #   CDFatBMapp <- CDFatBM[,3]
      #   CDFatBMapp <- cbind(c("With","Without","% diff"),c("","",""), CDFatBMapp)
      #   YN <- ifelse(CDFatBMapp[1,3]>CDFatBMapp[2,3],"Y","N")
      #   NY <- ifelse(CDFatBMapp[2,3]>CDFatBMapp[1,3],"Y","N")
      #   CDFatBMapp[1,2] <- YN
      #   CDFatBMapp[2,2] <- NY
      #   CDFatBMapp[3,2] <- "---"
      #   colnames(CDFatBMapp) <- c("TEST NAME","Y/N", "CDF (%)")
      #   
      
      # colnames(CDFatBM) <- c("CDF at Benchmark", "Probability >= to BM", "Probability >= BM *with zeroes")
      
    }
  }
  
  b2cCTR = sum(clicks[AD$vertical == "B2C"], na.rm = TRUE) / sum(imps[AD$vertical == "B2C"], na.rm = TRUE)
  b2cER = sum(sum(clicks[AD$vertical == "B2C"], na.rm = TRUE) + sum(RTs[AD$vertical == "B2C"], na.rm = TRUE) + sum(replies[AD$vertical == "B2C"], na.rm = TRUE) + sum(favs[AD$vertical == "B2C"], na.rm = TRUE)) / sum(imps[AD$vertical == "B2C"], na.rm = TRUE)
  b2bCTR = sum(clicks[AD$vertical == "B2B"], na.rm = TRUE) / sum(imps[AD$vertical == "B2B"], na.rm = TRUE)
  b2bER = sum(sum(clicks[AD$vertical == "B2B"], na.rm = TRUE) + sum(RTs[AD$vertical == "B2B"], na.rm = TRUE) + sum(replies[AD$vertical == "B2B"], na.rm = TRUE) + sum(favs[AD$vertical == "B2B"], na.rm = TRUE)) / sum(imps[AD$vertical == "B2B"], na.rm = TRUE)
  mediaCTR = sum(clicks[AD$vertical == "Media"], na.rm = TRUE) / sum(imps[AD$vertical == "Media"], na.rm = TRUE)
  mediaER = sum(sum(clicks[AD$vertical == "Media"], na.rm = TRUE) + sum(RTs[AD$vertical == "Media"], na.rm = TRUE) + sum(replies[AD$vertical == "Media"], na.rm = TRUE) + sum(favs[AD$vertical == "Media"], na.rm = TRUE)) / sum(imps[AD$vertical == "Media"], na.rm = TRUE)
  sportsCTR = sum(clicks[AD$vertical == "Sports"], na.rm = TRUE) / sum(imps[AD$vertical == "Sports"], na.rm = TRUE)
  sportsER = sum(sum(clicks[AD$vertical == "Sports"], na.rm = TRUE) + sum(RTs[AD$vertical == "Sports"], na.rm = TRUE) + sum(replies[AD$vertical == "Sports"], na.rm = TRUE) + sum(favs[AD$vertical == "Sports"], na.rm = TRUE)) / sum(imps[AD$vertical == "Sports"], na.rm = TRUE)
  overallCTR = sum(clicks)/sum(imps)
  overallCTR <- sum(clicks, na.rm = TRUE)/sum(linkImps, na.rm = TRUE)
  overallER <- sum(engmnt, na.rm = TRUE)/sum(imps, na.rm = TRUE)
  
  
  refMatrix = matrix(c(round(b2cCTR*100,4),round(b2cER*100,4),round(b2bCTR*100,4),round(b2bER*100,4),round(mediaCTR*100,4),round(mediaER*100,4),round(sportsCTR*100,4),round(sportsER*100,4),round(overallCTR*100,4),round(overallER*100,4)), ncol= 2, nrow=5,byrow = TRUE)
  colnames(refMatrix) = c("CTR (%)", "ER (%)")
  rownames(refMatrix) = c("B2C", "B2B", "Media", "Sports", "OVERALL")
  
  return(outputMatrix)
}





refTableFun <- function (input){
  AD <- read.csv("All_Data_04.06.csv", header=TRUE)
  clients <- as.matrix(unique(AD$client))
  
  cat("\nIn function")
  something <- input$client
  print(something)
  AD <- read.csv("All_Data_04.06.csv", header=TRUE)
  if (something == "OVERALL"){
    cat("\nOverall selected")
    AD <- read.csv("All_Data_04.06.csv", header=TRUE)
  }else{
    AD <- AD[AD$client == something,]
    cat("\n something selected")
  }
  library(tidyr)
  library(ggplot2)
  library(plotrix)
  
  imps <- AD$impressions
  linkImps <- AD$impressions [AD$link == "1"]
  RTs <- AD$retweets
  replies <- AD$replies
  favs <- AD$favorites
  clicks <- AD$url.clicks
  engmnt <- clicks + favs + replies + RTs
  overallER <- sum(engmnt)/sum(imps)
  
  #### variables ####
  
  imps <- AD$impressions
  linkImps <- AD$impressions[AD$link == 1]
  clicks <- AD$url.clicks
  favs <- AD$favorites
  RTs <- AD$retweets
  replies <- AD$replies
  followBacks <- na.omit(AD$followed_back_at)
  clients <- as.matrix(unique(AD$client))
  
  ## clicks ##
  
  CTR <- clicks[AD$link==1]/linkImps
  CTRperNonZeroes <- (length(CTR)-length(CTR[CTR == "0"]))/length(CTR)
  CTRs <- c(CTR*CTRperNonZeroes)
  CTRs[CTRs == "0"] <- NA
  CTRs <- na.omit(CTRs)
  CTRpercentiles <- as.matrix((quantile(CTRs, c(.25,.5,.75)))*100)
  colnames(CTRpercentiles) <- c("iCTR")
  ####################
  CRpercentiles = NULL
  
  for (index in 1:length(clients)) {
    
    currentClient <- clients[index] 
    AD1 <- AD[AD$client == currentClient,]
    CR <- sum(AD1$url.clicks, na.rm = TRUE) / length(AD1$url.clicks)
    CRpercentiles[index] = CR
  }
  
  CRpercentiles <- as.matrix((quantile(CRpercentiles, c(.25,.5,.75), na.rm = TRUE))*100)
  colnames(CRpercentiles) <- c("CR")
  Clicks <- cbind(CRpercentiles,CTRpercentiles)
  
  
  
  ## acquisition rate ##
  ARmatrix = NULL
  ARpercentiles = NULL
  
  for (index in 1:length(clients)) {
    
    currentClient <- clients[index] 
    AD1 <- AD[AD$client == currentClient,]
    AR <- length(na.omit(AD1$followed_back_at))/(nrow(AD1[is.na(AD1$followed_at) == FALSE,]))
    
    ARmatrix[index] = AR
    
  }
  
  ARpercentiles <- as.matrix((quantile(ARmatrix, c(.25,.5,.75), na.rm = TRUE))*100)
  colnames(ARpercentiles) <- c("Acquistion Rate")
  ARmatrix <- na.omit(cbind(clients,ARmatrix))
  
  
  ## rt rate ##
  
  iRTR <- RTs/imps
  iRTRperNonZeroes <- (length(iRTR)-length(iRTR[iRTR == "0"]))/length(iRTR)
  iRTRs <- c(iRTR*iRTRperNonZeroes)
  iRTRs[iRTRs == "0"] <- NA
  iRTRs <- na.omit(iRTRs)
  iRTRpercentiles <- as.matrix((quantile(iRTRs, c(.25,.5,.75)))*100)
  
  colnames(iRTRpercentiles) <- c("iRTR")
  
  ####################
  RTRpercentiles = NULL
  
  for (index in 1:length(clients)) {
    
    currentClient <- clients[index] 
    AD1 <- AD[AD$client == currentClient,]
    RTR <- sum(AD1$retweets, na.rm = TRUE) / length(AD1$retweets)
    RTRpercentiles[index] = RTR
    
  }
  
  RTRpercentiles <- as.matrix((quantile(RTRpercentiles, c(.25,.5,.75), na.rm = TRUE))*100)
  colnames(RTRpercentiles) <- c("RTR")
  Retweets <- cbind(RTRpercentiles,iRTRpercentiles)
  
  ## fav rate ##
  
  iFR <- favs/imps
  iFRperNonZeroes <- (length(iFR)-length(iFR[iFR == "0"]))/length(iFR)
  iFRs <- c(iFR*iFRperNonZeroes)
  iFRs[iFRs == "0"] <- NA
  iFRs <- na.omit(iFRs)
  iFRpercentiles <- as.matrix((quantile(iFRs, c(.25,.5,.75)))*100)
  colnames(iFRpercentiles) <- c("iFR")
  ####################
  FRpercentiles = NULL
  
  for (index in 1:length(clients)) {
    
    currentClient <- clients[index] 
    AD1 <- AD[AD$client == currentClient,]
    FR <- sum(AD1$favorites, na.rm = TRUE) / length(AD1$favorites)
    FRpercentiles[index] = FR
  }
  
  FRpercentiles <- as.matrix((quantile(FRpercentiles, c(.25,.5,.75), na.rm = TRUE))*100)
  colnames(FRpercentiles) <- c("FR")
  
  Favorites <- cbind(FRpercentiles,iFRpercentiles)
  
  ## reply rate ##
  
  iRR <- replies/imps
  iRRperNonZeroes <- (length(iRR)-length(iRR[iRR == "0"]))/length(iRR)
  iRRs <- c(iRR*iRRperNonZeroes)
  iRRs[iRRs == "0"] <- NA
  iRRs <- na.omit(iRRs)
  iRRpercentiles <- as.matrix((quantile(iRRs, c(.25,.5,.75)))*100)
  colnames(iRRpercentiles) <- c("iRR")
  
  ####################
  RRpercentiles = NULL
  
  for (index in 1:length(clients)) {
    
    currentClient <- clients[index] 
    AD1 <- AD[AD$client == currentClient,]
    RR <- sum(AD1$replies, na.rm = TRUE) / length(AD1$replies)
    RRpercentiles[index] = RR
    
  }
  
  RRpercentiles <- as.matrix((quantile(RRpercentiles, c(.25,.5,.75), na.rm = TRUE))*100)
  colnames(RRpercentiles) <- c("RR")
  Replies <- cbind(RRpercentiles,iRRpercentiles)
  
  
  ## engmt rate ##
  
  iER <- (replies+favs+clicks+RTs)/imps
  iERperNonZeroes <- (length(iER)-length(iER[iER == "0"]))/length(iER)
  iERs <- c(iER*iERperNonZeroes)
  iERs[iERs == "0"] <- NA
  iERs <- na.omit(iERs)
  iERpercentiles <- as.matrix((quantile(iERs, c(.25,.5,.75)))*100)
  
  colnames(iERpercentiles) <- c("iER")
  ####################
  ERpercentiles = NULL
  
  for (index in 1:length(clients)) {
    
    currentClient <- clients[index] 
    AD1 <- AD[AD$client == currentClient,]
    ER <- sum(AD1$replies,AD1$url.clicks,AD1$favorites,AD1$retweets, na.rm=TRUE) / length(AD1$replies)
    ERpercentiles[index] = ER
  }
  
  ERpercentiles <- as.matrix((quantile(ERpercentiles, c(.25,.5,.75), na.rm = TRUE))*100)
  colnames(ERpercentiles) <- c("ER")
  
  
  Engagements <- cbind(ERpercentiles,iERpercentiles)
  
  refTable <- cbind(Clicks, Retweets, Favorites, Replies, Engagements, ARpercentiles)
  return(refTable)
}





doallthecrap1 <- function(input) {
  AD <- read.csv("All_Data_04.06.csv", header=TRUE)
  verticals <- as.matrix(unique(AD$vertical))
  
  cat("\nIn function")
  something1 <- input$client
  print(something1)
  AD <- read.csv("All_Data_04.06.csv", header=TRUE)
  if (something1 == "OVERALL"){
    cat("\nOverall selected")
    AD <- read.csv("All_Data_04.06.csv", header=TRUE)
  }else{
    AD <- AD[AD$client == something1,]
    cat("\n something1 selected")
  }
  library(tidyr)
  library(ggplot2)
  library(plotrix)
  
  imps <- AD$impressions
  linkImps <- AD$impressions [AD$link == "1"]
  RTs <- AD$retweets
  replies <- AD$replies
  favs <- AD$favorites
  clicks <- AD$url.clicks
  engmnt <- clicks + favs + replies + RTs
  overallER <- sum(engmnt)/sum(imps)
  
  # hours of the day (0-23)
  hoursOfDay <- seq(0:23)
  
  # # from data
  rawTimeDate <- AD$tweet.time
  rawTimeDate <- as.POSIXlt(rawTimeDate)
  
  hours <-as.numeric(format(rawTimeDate, format='%H')) # 0 = midnight
  date <- as.Date(rawTimeDate)
  dayOfWeek <- as.matrix(rawTimeDate$wday)
  
  
  timesER = engmnt / imps
  timesER[imps == 0] <- NA
  timesER[engmnt == 0] <- NA
  timezoneDf <- data.frame(date,hours,timesER,dayOfWeek)
  
  start <- as.Date("2014-03-08")
  end <- as.Date("2014-11-01")
  
  minusFour <- timezoneDf[timezoneDf[,1]<= end,]
  minusFive <- timezoneDf[timezoneDf[,1] > end,]
  
  ######## Reassign -4 ########
  minusFour[,2][minusFour[,2] == 0] <- 20
  minusFour[,2][minusFour[,2] == 1] <- 21
  minusFour[,2][minusFour[,2] == 2] <- 22
  minusFour[,2][minusFour[,2] == 3] <- 23
  minusFour[,2][minusFour[,2] == 4] <- 0
  minusFour[,2][minusFour[,2] == 5] <- 1
  minusFour[,2][minusFour[,2] == 6] <- 2
  minusFour[,2][minusFour[,2] == 7] <- 3
  minusFour[,2][minusFour[,2] == 8] <- 4
  minusFour[,2][minusFour[,2] == 9] <- 5
  minusFour[,2][minusFour[,2] == 10] <- 6
  minusFour[,2][minusFour[,2] == 11] <- 7
  minusFour[,2][minusFour[,2] == 12] <- 8
  minusFour[,2][minusFour[,2] == 13] <- 9
  minusFour[,2][minusFour[,2] == 14] <- 10
  minusFour[,2][minusFour[,2] == 15] <- 11
  minusFour[,2][minusFour[,2] == 16] <- 12
  minusFour[,2][minusFour[,2] == 17] <- 13
  minusFour[,2][minusFour[,2] == 18] <- 14
  minusFour[,2][minusFour[,2] == 19] <- 15
  minusFour[,2][minusFour[,2] == 20] <- 16
  minusFour[,2][minusFour[,2] == 21] <- 17
  minusFour[,2][minusFour[,2] == 22] <- 18
  minusFour[,2][minusFour[,2] == 23] <- 19
  ######## Reassign -5 ########
  minusFive[,2][minusFive[,2] == 0] <- 19
  minusFive[,2][minusFive[,2] == 1] <- 20
  minusFive[,2][minusFive[,2] == 2] <- 21
  minusFive[,2][minusFive[,2] == 3] <- 22
  minusFive[,2][minusFive[,2] == 4] <- 23
  minusFive[,2][minusFive[,2] == 5] <- 0
  minusFive[,2][minusFive[,2] == 6] <- 1
  minusFive[,2][minusFive[,2] == 7] <- 2
  minusFive[,2][minusFive[,2] == 8] <- 3
  minusFive[,2][minusFive[,2] == 9] <- 4
  minusFive[,2][minusFive[,2] == 10] <- 5
  minusFive[,2][minusFive[,2] == 11] <- 6
  minusFive[,2][minusFive[,2] == 12] <- 7
  minusFive[,2][minusFive[,2] == 13] <- 8
  minusFive[,2][minusFive[,2] == 14] <- 9
  minusFive[,2][minusFive[,2] == 15] <- 10
  minusFive[,2][minusFive[,2] == 16] <- 11
  minusFive[,2][minusFive[,2] == 17] <- 12
  minusFive[,2][minusFive[,2] == 18] <- 13
  minusFive[,2][minusFive[,2] == 19] <- 14
  minusFive[,2][minusFive[,2] == 20] <- 15
  minusFive[,2][minusFive[,2] == 21] <- 16
  minusFive[,2][minusFive[,2] == 22] <- 17
  minusFive[,2][minusFive[,2] == 23] <- 18
  
  ###########
  tzAdjustedMatrix <- (rbind(minusFour,minusFive))
  
  DoWmatrix <- unique(sort(tzAdjustedMatrix[,4]))
  DoWnames <- c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat")
  DoWmatrix <- cbind(DoWmatrix,DoWnames)
  
  #create the matrix to dump the data
  DoWname = NULL
  
  for (index in 1:nrow(tzAdjustedMatrix)) {
    
    #for each row in the tzMat
    
    #match the value in column ,4 to the row in dowMat
    dow <- tzAdjustedMatrix[index,4]
    
    #pull the string in ,2 of dowMat
    name <- DoWmatrix[dow+1,2]
    #name <- DoWmatrix[DoWMatrix[,1]==dow,2]
    #assign string to the above matrix
    DoWname[index] = name
    
  }
  
  
  tzAdjustedMatrix <- cbind(tzAdjustedMatrix,DoWname)
  hours <- tzAdjustedMatrix[,2]
  
  # Boxplot
  DoWdf <- data.frame(tzAdjustedMatrix[,3], tzAdjustedMatrix[,4])
  boxplot(tzAdjustedMatrix[,3]~tzAdjustedMatrix[,4], main = "Client ERs by Day of Week")
  
  adjustedMatrix2 <- tzAdjustedMatrix[!is.na(tzAdjustedMatrix[,3]),]
  TOD <- seq(0:23)-1
  
  # total elements per hour per day from total
  
  # perTime = matrix(ncol=7, nrow =24)
  # colnames(perTime) <- c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat")
  # rownames(perTime) <- c("midnight","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","noon","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm")
  # 
  # for (day in 1:7) {
  # dayOweek <- adjustedMatrix2[adjustedMatrix2[,4] == day-1,] 
  #     
  #   for (hour in 1:24) {
  #     ct <- length(dayOweek[dayOweek[,2] == hour-1,3])
  #     perTime[hour,day] <- ct/(length(adjustedMatrix2[,3]))
  #   
  #                       }
  # 
  #                   }
  
  
  # count of elements per hour per day
  
  countTime = matrix(ncol=7, nrow =24)
  colnames(countTime) <- c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat")
  rownames(countTime) <- c("midnight","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","noon","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm")
  
  for (day in 1:7) {
    dayOweek <- adjustedMatrix2[adjustedMatrix2[,4] == day-1,] 
    
    for (hour in 1:24) {
      ct <- length(dayOweek[dayOweek[,2] == hour-1,3])
      
      if (ct < 10) {
        ct = "sample too small :("
      }
      
      countTime[hour,day] <- ct
      
    }
    
  }
  
  # median of ERs per hour per day
  
  medERs = matrix(ncol=7, nrow =25)
  colnames(medERs) <- c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat")
  #rownames(medERs) <- (TOD)
  rownames(medERs) <- c("midnight","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","noon","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm","Median ER by Day")
  
  for (day in 1:7) {
    dayOweek <- adjustedMatrix2[adjustedMatrix2[,4] == day-1,] 
    
    for (hour in 1:24) {
      ct <- median(dayOweek[dayOweek[,2] == hour-1,3])
      
      if (length(dayOweek[dayOweek[,2] == hour-1,3]) < 10) {
        ct = NA
        
      }
      
      medERs[hour,day] <- ct*100
      
    }
    medERs[25,day] <- median(medERs[,day], na.rm = TRUE)
  }
  
  
  
  
  # mean of ERs per hour per day
  
  # meanERs = matrix(ncol=7, nrow =24)
  # colnames(meanERs) <- c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat")
  # rownames(meanERs) <- c("midnight","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","noon","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm")
  
  
  # for (day in 1:7) {
  #   dayOweek <- adjustedMatrix2[adjustedMatrix2[,4] == day-1,] 
  #   
  #   for (hour in 1:24) {
  #     ct <- mean(dayOweek[dayOweek[,2] == hour-1,3])
  #     meanERs[hour,day] <- ct
  #     
  #   }
  #   
  # }
  
  
  
  
  # median of ERs per hour per day (25th column omitted)
  
  medERs1 = matrix(ncol=7, nrow =24)
  colnames(medERs1) <- c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat")
  #rownames(medERs1) <- (TOD)
  rownames(medERs1) <- c("midnight","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","noon","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm")
  
  for (day in 1:7) {
    dayOweek <- adjustedMatrix2[adjustedMatrix2[,4] == day-1,] 
    
    for (hour in 1:24) {
      ct <- median(dayOweek[dayOweek[,2] == hour-1,3])
      
      if (length(dayOweek[dayOweek[,2] == hour-1,3]) < 10) {
        ct = NA
        
      }
      
      medERs1[hour,day] <- ct*100
      
    }
  }
  
  
  
  
  
  # plot(countTime, medERs,  xlim = c(0,10))
  # plot(countTime, medERs1)
  
  # medERs <- as.data.frame(medERs)
  # data.frame(expand.grid(weekday = c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"), time = 0:23, value=medERs1))
  # ggplot(medERs, aes(day, weekday, size=value)) + geom_point(shape=21, fill="blue")
  
  
  
  
  
  # lines(lowess(countTime,medERs), col="blue") # lowess line (x,y)
  # abline(lm(countTime~medERs), col="red") 
  # 
  # write.csv(medERs,file= "ToD vs DoW.csv")
  
  
  
  
  
  pctable<-data.frame(expand.grid(0:23,c("sat","fri","thurs","wed","tues","mon","sun")))
  colnames(pctable) <- c("hour","day")
  pctable$value=as.vector(medERs1)
  punchcard <- ggplot(pctable, aes(hour,day, size=value)) + geom_point(shape=21, fill="blue") + scale_x_continuous(breaks= 0:23) + scale_size_continuous(range=c(0,17)) + theme_light(base_size = 20, base_family = "")
  
  return(punchcard)
}



