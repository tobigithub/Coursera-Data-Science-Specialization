# Write a function calledrankallthat takes two arguments: an outcome name 
# (outcome) and a hospital ranking (num).  The function reads the
# outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified num.  
# For example the function call rankall("heart attack", "best") would return a
# data frame containing the names of the hospitals thatare the best in their 
# respective states for 30-day heart attack death rates.  The function should 
# return a value for every state (some may be NA). The first column in the data 
# frame is named hospital, which contains the hospital name, and the second column 
# is named state, which contains the 2-character abbreviation for the state name. 
# Hospitals that do not have data on a particular outcome should be excluded
# from the set of hospitals when deciding the rankings.

rankall <- function(outcome, num = "best") {
      ## Read outcome data
      outc <- read.csv("outcome-of-care-measures.csv")
      states <- unique(outc$State)
      outcomes <- c("heart attack" , "heart failure" , "pneumonia")
      
      ## Check that state and outcome are valid
      if (!state %in% states) stop("invalid state")
      if (!outcome %in% outcomes) stop("invalid outcome")
      
      ## For each state, find the hospital of the given rank
      state <- levels(unique(outc[,7]))
      hospital=rep(NA,length(state))
      result=cbind(hospital, state)
      
      for (i in 1:length(state)){
            if (outcome=="heart attack"){
                  best=worst=0
                  tempdata <- outc[outc$State==state[i],][,c(11,2)]
                  tempdata[,1]=as.numeric(levels(tempdata[,1])[(tempdata[,1])])
                  tempdata[,2]=as.character(levels(tempdata[,2])[(tempdata[,2])])
                  ranking <- tempdata[ do.call(order, tempdata) ,]
                  ranking <- ranking[which(complete.cases(ranking)),]
                  if (num=="best") {num=1; best=1}
                  if (num=="worst") {num=length(ranking[,1]); worst=1}
                  if (num>length(ranking[,1])) result[i,1]=NA
                  if (num<=length(ranking[,1])) result[i,1]=ranking[num,2]
                  if (worst==1) num="worst"
                  if (best==1) num="best"
            }
            
            if (outcome=="heart failure"){
                  best=worst=0
                  tempdata <- outc[outc$State==state[i],][,c(17,2)]
                  tempdata[,1]=as.numeric(levels(tempdata[,1])[(tempdata[,1])])
                  tempdata[,2]=as.character(levels(tempdata[,2])[(tempdata[,2])])
                  ranking <- tempdata[ do.call(order, tempdata) ,]
                  ranking <- ranking[which(complete.cases(ranking)),]
                  if (num=="best") {num=1; best=1}
                  if (num=="worst") {num=length(ranking[,1]); worst=1}
                  if (num>length(ranking[,1])) result[i,1]=NA
                  if (num<=length(ranking[,1])) result[i,1]=ranking[num,2]
                  if (worst==1) num="worst"
                  if (best==1) num="best"
            }
            
            if (outcome=="pneumonia"){
                  best=worst=0
                  tempdata <- outc[outc$State==state[i],][,c(23,2)]
                  tempdata[,1]=as.numeric(levels(tempdata[,1])[(tempdata[,1])])
                  tempdata[,2]=as.character(levels(tempdata[,2])[(tempdata[,2])])
                  ranking <- tempdata[ do.call(order, tempdata) ,]
                  ranking <- ranking[which(complete.cases(ranking)),]
                  if (num=="best") {num=1; best=1}
                  if (num=="worst") {num=length(ranking[,1]); worst=1}
                  if (num>length(ranking[,1])) result[i,1]=NA
                  if (num<=length(ranking[,1])) result[i,1]=ranking[num,2]
                  if (worst==1) num="worst"
                  if (best==1) num="best"
            }
      }
      ## Return a data frame with the hospital names and the (abbreviated) state name
      return(as.data.frame(result))
      
}
