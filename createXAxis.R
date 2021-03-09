#----------------------- createXAxis.R --------------------------------------#
# M. L. Muradian
# HFF
# Function used by plotCleanSondeDat.R to create a user-specified x-axis 
#
# Written: Nov 30, 2015
# Last updated:  
##########################################################################

## The call:
# cleanSondeDat <- createXAxis(cleanSondeDat, fromXAxis, toXAxis, fromData, toData)

createXAxis <- function(cleanSondeDat, fromXAxis, toXAxis, fromData, toData){
  
  fromXAxis <- as.POSIXct(fromXAxis, tz="UTC")
  toXAxis   <- as.POSIXct(toXAxis, tz="UTC") 
  
  if(fromData > fromXAxis){ 
    ## then the axis needs to begin before the data
    fillDateSeq <- seq(fromXAxis, fromData - 900, by = 900)
    fillDF <- data.frame(fillDateSeq,
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))) )
    
    colnames(fillDF) <- names(cleanSondeDat)
    
    cleanSondeDat <- data.frame(rbind(fillDF, cleanSondeDat))
    
  }
  
  if(toXAxis > toData){ 
    ## then the axis needs to end after the data
    fillDateSeq <- seq(toData + 900, toXAxis, by = 900)
    fillDF <- data.frame(fillDateSeq,
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))), 
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))),
                         as.numeric(rep(NA, length(fillDateSeq))) )
    
    colnames(fillDF) <- names(cleanSondeDat)
    
    cleanSondeDat <- data.frame(rbind(cleanSondeDat, fillDF))
    
  }
  
  return(cleanSondeDat)
}

