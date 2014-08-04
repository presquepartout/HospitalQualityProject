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
         
         ## match outcomes to correct columns
         if(match(outcome, "heart attack", nomatch=0) !=0){
           
           ## the following coercion introduces NAs. Could use suppressWarnings()
           resultVector <- as.numeric(outData[,11])                 
         }
         
         if(match(outcome, "heart failure", nomatch=0) !=0){
           
           resultVector <- as.numeric(outData[,17])   
         }
         
         if(match(outcome, "pneumonia", nomatch=0) !=0){
           
           resultVector <- as.numeric(outData[,23])          
         }
         
         
         workingFrame <- data.frame(outData$State
                                    , outData$Hospital.Name
                                    , resultVector)
         ## when you cobble together vectors into a frame the 
         ## names are long. 
         
         names(workingFrame) <- c("state", "hospital", "result")
         workingFrame$state <- as.character(workingFrame$state)
         workingFrame$hospital <- as.character(workingFrame$hospital)
         ## order the data frame by state. 
         
         workingSort <- workingFrame[order(workingFrame$state, workingFrame$result),]
         splitWork <- split(workingSort, as.factor(workingSort$state))
         statelength <- length(names(splitWork))
         hospital <- character(statelength)
         
        for (i in 1:statelength) {
            dF <- as.data.frame(splitWork[i])
            names(dF) <- c("state", "hospital", "result")
            if(match(num, c("best", "worst")) == 1) {
                  hospital[i] <- dF$hospital[1]
              } else {
                if(match(num, c("best", "worst")) == 2) {
                  index = sum(!is.na(df$hospital))
                  hospital[i] <- dF$hospital[index]
                  } else {        
                    hospital[i] <- dF$hospital[as.integer(num)]
                  }}
           
         }
         state <- names(splitWork)
         endFrame <- data.frame(hospital,state)
         return(endFrame)
         
       
   
}
         