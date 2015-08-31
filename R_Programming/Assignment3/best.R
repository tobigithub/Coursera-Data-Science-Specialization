# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv ﬁle and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the speciﬁed outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.

# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the ﬁrst hospital in that set should be chosen 
# (i.e. if hospitals “b”, “c”, and “f” are tied for best, then hospital “b” should be returned).

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message “invalid outcome”.

best <- function(state, outcome) {
    ## Read outcome data
    outc <- read.csv("outcome-of-care-measures.csv")
    states <- unique(outc$State)
    outcomes <- c("heart attack" , "heart failure" , "pneumonia")
    
    if (!state %in% states) stop("invalid state")
    if (!outcome %in% outcomes) stop("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day deaths
    if (outcome=="heart attack"){
        tempdata <- outc[outc$State==state,][,c(11,2)]
        tempdata[,1]=as.numeric(levels(tempdata[,1])[(tempdata[,1])])
        minimum = min(tempdata[,1], na.rm=T)
        hospitals <- as.character(tempdata[which(tempdata[,1]==minimum),][,2])
        return(sort(hospitals))
    }
    
    if (outcome=="heart failure"){
        tempdata <- outc[outc$State==state,][,c(17,2)]
        tempdata[,1]=as.numeric(levels(tempdata[,1])[(tempdata[,1])])
        minimum = min(tempdata[,1], na.rm=T)
        hospitals <- as.character(tempdata[which(tempdata[,1]==minimum),][,2])
        return(sort(hospitals))
    }
    
    if (outcome=="pneumonia"){
        tempdata <- outc[outc$State==state,][,c(23,2)]
        tempdata[,1]=as.numeric(levels(tempdata[,1])[(tempdata[,1])])
        minimum = min(tempdata[,1], na.rm=T)
        hospitals <- as.character(tempdata[which(tempdata[,1]==minimum),][,2])
        return(sort(hospitals))
    }
}
