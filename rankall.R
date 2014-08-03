## this script returns all hospitals that have a given ranking in their
## home states.

rankall <- function(outcome, num = "best") {
         ## read in outcome, rank data
         outData <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
         
         outcomeNames <- c("heart attack", "heart failure", "pneumonia")
         if(!(outcome %in% outcomeNames)) {
           stop("invalid outcome")
         }
         
         ## Set list of states to be used. 
         statenames <- as.character(levels(as.factor(outData$State)))
         ## this list has length 54. It includes all 50 states, plus
         ## District Of Columbia (DC), Guam (GU), and Puerto Rico (PR). 
         
         ## Restrict data to specified state:
         
         resultFrame <- outData[outData$State == state,]
         
         ##charNumVector <- as.character(1:length(resultFrame$State))
         ## testNumChar <- c("best", "worst", charNumVector)
         ## if(!(num %in% testNumChar)) {
         ##  stop("NA")
         ##}
         
         resultVector <- numeric()
         hospitalVector <- resultFrame$Hospital.Name
         
         ## match outcomes to correct columns
         if(match(outcome, "heart attack", nomatch=0) !=0){

                  ## the following coercion introduces NAs. Could use suppressWarnings()
                  resultFrame[,11] <- as.numeric(resultFrame[,11])                 
                  resultVector <- resultFrame[,11]
         }
         
         if(match(outcome, "heart failure", nomatch=0) !=0){
      
                  resultFrame[,17] <- as.numeric(resultFrame[,17])
                  resultVector <- resultFrame[,17]
         }
         
         if(match(outcome, "pneumonia", nomatch=0) !=0){
                  
                  resultFrame[,23] <- as.numeric(resultFrame[,23])
                  resultVector <- resultFrame[,23]
                      
         }
         
         twoFrame <- data.frame(hospitalVector, resultVector, stringsAsFactors = FALSE)
         ordered <- twoFrame[order(twoFrame$resultVector, twoFrame$hospitalVector),]
         ## need to split ordered into the NA/nonNA parts
         ordered$isna <- is.na(ordered$resultVector)
         splitted <- split(ordered, as.factor(ordered$isna))
         finalFrame <- as.data.frame(splitted[1])
         names(finalFrame) <- c("Hospital", "Result", "ISNA")
         if(num == "best") {
                 result <- finalFrame$Hospital[1]
                 return(result)
         }
         if(num == "worst") {
                result <- finalFrame$Hospital[length(finalFrame$Hospital)]
                return(result)
         } 
         if(num %in% 1:length(finalFrame$Hospital)) {
                result <- finalFrame$Hospital[num]
                return(result)
         }
         if(num > length(finalFrame$Hospital)) {
                result <- finalFrame$Hospital[num]
                result
         }
        
        
}