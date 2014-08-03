## this script returns the best outcome in the specified state

best <- function(state, outcome) {
         ## read in state, outcome data
         outData <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
         outData[,11] <- as.numeric(outData[,11])
         outData[,17] <- as.numeric(outData[,17])
         outData[,23] <- as.numeric(outData[,23])
         
         ## check input values
         if(!(state %in% outData$State)) {
           stop("invalid state")
         }
         outcomeNames <- c("heart attack", "heart failure", "pneumonia")
         if(!(outcome %in% outcomeNames)) {
           stop("invalid outcome")
         }
        
         ## match inputs to state with minimum values
         if(match(outcome, "heart attack", nomatch=0) !=0){
                  result <- character()
                  resultFrame <- outData[outData$State == state,]
                  value <- min(resultFrame[,11], na.rm = TRUE)
                  selector <- resultFrame[,11] == value
                  ## get rid of NAs
                  selector <- selector & !is.na(selector)
                  result <- resultFrame$Hospital.Name[selector]
                  return(result)       
         }
         if(match(outcome, "heart failure", nomatch=0) !=0){
                  result <- character()
                  resultFrame <- outData[outData$State == state,]
                  value <- min(resultFrame[,17], na.rm = TRUE)
                  selector <- resultFrame[,17] == value
                  ## get rid of NAs
                  selector <- selector & !is.na(selector)
                  result <- resultFrame$Hospital.Name[selector]
                  return(result)      
         }
         if(match(outcome, "pneumonia", nomatch=0) !=0){
                  result <- character()
                  resultFrame <- outData[outData$State == state,]
                  value <- min(resultFrame[,23], na.rm = TRUE)
                  selector <- resultFrame[,23] == value
                  ## get rid of NAs
                  selector <- selector & !is.na(selector)
                  result <- resultFrame$Hospital.Name[selector]
                  return(result)      
         }
}