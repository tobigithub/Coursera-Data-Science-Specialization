# Write a function called rankhospital that takes three arguments: the 2-character 
# abbreviated name of astate (state), an outcome (outcome), and the ranking of a
# hospital in that state for that outcome (num).The function reads the
# outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example,
# the call rankhospital("MD", "heart failure", 5) would return a character vector
# containing the name of the hospital with the 5th lowest 30-day death rate for 
# heart failure. The num argument can take values "best", "worst", or an integer
# indicating the ranking (smaller numbers are better). If the number given by num is 
# larger than the number of hospitals in that state, then the function should 
# return NA. Hospitals that do not have data on a particular outcome should be
# excluded from the set of hospitals when deciding the rankings. Handling ties:
# It may occur that multiple hospitals have the same 30-day mortality rate for a
# given cause of death. In those cases ties should be broken by using the hospital
# name. 

rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      outc <- read.csv("outcome-of-care-measures.csv")
      states <- unique(outc$State)
      outcomes <- c("heart attack" , "heart failure" , "pneumonia")
      
      ## Check that state and outcome are valid
      if (!state %in% states) stop("invalid state")
      if (!outcome %in% outcomes) stop("invalid outcome")
      
      ## Return hospital name in that state with the given rank
      if (outcome=="heart attack"){
            tempdata <- outc[outc$State==state,][,c(11,2)]
            tempdata[,1]=as.numeric(levels(tempdata[,1])[(tempdata[,1])])
            tempdata[,2]=as.character(levels(tempdata[,2])[(tempdata[,2])])
            ranking <- tempdata[ do.call(order, tempdata) ,]
            ranking <- ranking[which(complete.cases(ranking)),]
            if (num=="best") num=1
            if (num=="worst") num=length(ranking[,1])
            if (num>length(ranking[,1])) return(NA)
            else return(ranking[num,2])
      }
      
      if (outcome=="heart failure"){
            tempdata <- outc[outc$State==state,][,c(17,2)]
            tempdata[,1]=as.numeric(levels(tempdata[,1])[(tempdata[,1])])
            tempdata[,2]=as.character(levels(tempdata[,2])[(tempdata[,2])])
            ranking <- tempdata[ do.call(order, tempdata) ,]
            ranking <- ranking[which(complete.cases(ranking)),]
            if (num=="best") num=1
            if (num=="worst") num=length(ranking[,1])
            if (num>length(ranking[,1])) return(NA)
            else return(ranking[num,2])
      }
      
      if (outcome=="pneumonia"){
            tempdata <- outc[outc$State==state,][,c(23,2)]
            tempdata[,1]=as.numeric(levels(tempdata[,1])[(tempdata[,1])])
            tempdata[,2]=as.character(levels(tempdata[,2])[(tempdata[,2])])
            ranking <- tempdata[ do.call(order, tempdata) ,]
            ranking <- ranking[which(complete.cases(ranking)),]
            if (num=="best") num=1
            if (num=="worst") num=length(ranking[,1])
            if (num>length(ranking[,1])) return(NA)
            else return(ranking[num,2])
      }
      
}
