#----------------------------- getHabContext_test.R -------------------------------------------#
## M.L. Muradian
## HFF


getHabContext <- function(z = NA, zAvgDMn = NA, data.dir = NA){
  
  # cat("getHab location is ", data.dir)
  shades <- list()
  n <- length(z)
  days <- unique(as.Date(time(z)))
  # n <- length(cleanSondeDat$Temp_C)
  # use data.dir to determine whether we use Rainbow or Brown trout threshold
  if(data.dir == "PS" | data.dir == "SA" | data.dir == "LSF"){
    
    # Use habitat thresholds for Brown trout (only for SA and PS sites)
    temp <- 10
    # spawning to emergence period for Browns, made-up year
    s2e <- seq( as.Date("Oct 1", format = "%b %d"), as.Date("Dec 31", format = "%b %d"), by = 1)
    s2e <- c(s2e, seq( as.Date("Jan 1", format = "%b %d"), as.Date("Feb 28", format = "%b %d"), by = 1))
    s2e <- format(s2e, "%b %d")
    # convert days to "month day" - hopefully this works with multiple years?! Need to test.
    md <- format(days, "%b %d")
    # check for overlap, these are the indecies for the spawn-to-emergence period
    spInd <- which(md %in% intersect(md, s2e)) 
    tempFac <- rep(1, length(days)) # indicates whether need temps for adults/juv/fry (1) or embryos (2)
    tempFac[spInd] <- 2
    # plot(tempFac)
    #
    # Temperature: 
    A <- matrix(c(0,1,1,0, 1,12,12,1, 12,19,19,12, 19,27,27,19, 27,30,30,27),5,4)  # juveniles & adults
    shades[[1]] <- c(rgb(1,1,0, alpha = .9),
                     rgb(.5,.9,.25, alpha = .3), 
                     rgb(.5,.9,.25, alpha = .65), 
                     rgb(.9, 1, .5, alpha = .55),
                     # rgb(.5,.9,.25, alpha = .3), 
                     rgb(1,0,0, alpha = .45)) # a place holder so that vectors in shades are of same 
    # length (plots off panel)
    B <- matrix(c(0,2,2,0, 2,13,13,2, 13,15,15,13, 15,20,20,15, 20,27,27,20),5,4) # embryos
    shades[[2]] <- c(rgb(.5,.9,.25, alpha = .3), 
                     rgb(.5,.9,.25, alpha = .65), 
                     rgb(.9, 1, .5, alpha = .55),
                     # rgb(.5,.9,.25, alpha = .3), 
                     rgb(1,1,0, alpha = .9),
                     rgb(1,0,0, alpha = .45))
    #
    # DO:
    C <- matrix(c(0,3,3,0, 3,5,5,3, 5,12,12,5, 12,16,16,12),4,4) # adults, warm water > 10 C;  
    # corresponds to 3 in DO.Fac
    D <- matrix(c(0,3,3,0, 3,5,5,3, 5,9,9,5, 9,16,16,9),4,4)     # adults, cold water <= 10 C; 
    # corresponds to 3 in DO.Fac
    E <- matrix(c(0,6,6,0, 6,9,9,6, 9,11,11,9, 11,16,16,11),4,4) # embryos;                    
    # corresponds to 3 in DO.Fac
    G <- matrix(c(-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9),4,4) # (F = FALSE);         
    # corresponds to 3 in DO.Fac
  }else{
    # Use habitat thresholds for Rainbow trout, Also, find the period of overlap between 
    # the plotted time series and the spawning-to-emergence period for Rainbows, spInd,
    # which contains the indices (in days) that need to be coded for embryos (3) in tempFac
    
    temp <- 15
    # spawning to emergence period for Rainbows, made-up year
    s2e <- seq( as.Date("Apr 1", format = "%b %d"), as.Date("Jun 1", format = "%b %d"), by = 1)
    s2e <- format(s2e, "%b %d")
    # convert days to "month day"
    md <- format(days, "%b %d")
    # check for overlap, these are the indices for the spawn-to-emergence period
    spInd <- which(md %in% intersect(md, s2e)) 
    tempFac <- rep(1, length(days)) # indicates whether need temps for adults/juv/fry (1) or embryos (2)
    tempFac[spInd] <- 2
    # Temperature:
    A <- matrix(c(0,1,1,0, 1,12,12,1, 12,20,20,12, 20,23,23,20, 23,25,25,23, 25,27,27,25),6,4) 
    # juveniles & adults
    shades[[1]] <- c(rgb(1,1,0, alpha = .9),
                     rgb(.5,.9,.25, alpha = .3),
                     rgb(.5,.9,.25, alpha = .65), 
                     rgb(.9, 1, .5, alpha = .55),
                     # rgb(.5,.9,.25, alpha = .3), # make this one a yellowish green?
                     rgb(1,1,0, alpha = .9),
                     rgb(1,0,0, alpha = .45))
    B <- matrix(c(0,4,4,0, 4,7,7,4, 7,12,12,7, 12,15,15,12, 15,20,20,15, 20,27,27,20),6,4) # embryos
    shades[[2]] <- c(rgb(1,1,0, alpha = .9),
                     rgb(.5,.9,.25, alpha = .3), 
                     rgb(.5,.9,.25, alpha = .65),
                     rgb(.9, 1, .5, alpha = .55),
                     # rgb(.5,.9,.25, alpha = .3),
                     rgb(1,1,0, alpha = .9),
                     rgb(1,0,0, alpha = .45))
    
    # DO:
    C <- matrix(c(0,3,3,0, 3,5,5,3, 5,9,9,5, 9,16,16,9),4,4)     # adults, warm water > 15 C;  
    # corresponds to 3 in DO.Fac
    D <- matrix(c(0,3,3,0, 3,5,5,3, 5,7,7,5, 7,16,16,7),4,4)     # adults, cold water <= 15 C; 
    # corresponds to 4 in DO.Fac
    E <- matrix(c(0,6,6,0, 6,9,9,6, 9,11,11,9, 11,16,16,11),4,4) # embryos;                    
    # corresponds to 5 in DO.Fac
    G <- matrix(c(-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9),4,4) # (F = FALSE);         
    # corresponds to 6 in DO.Fac
  }
  
  ###########################################################
  yset <- list(A = A, B = B, C = C, D = D, E = E, G = G)
  ###########################################################
  
  ######  Here is the chunk with any changes  ######
  ##################################################
  # calculate average daily temperatures and use to determine DO requirements
  # create a vector to hold factor for whether cool or warm
  zAvgTemp <- apply.daily(z, colMeans, na.rm=T) # z created on line 63
  # apply.daily calculates non-overlapping intervals, right aligned (no control over this 
  #  that I can find) vs rollmean, which calculates a rolling average!
  
  # Drop times, dates only
  time(zAvgTemp) <- format(time(zAvgTemp), "%Y-%m-%d")

  # Now work on the Average Daily Mean data
  
  # If site is USF or LSF, create a simple time series based on what happened in 2020 
  # until these sites have a few years of data and have their own Avg Daily Means
  if(data.dir == "USF"){
    # create the reference time series (estimate based on 2020) for USF
    zRef <- zoo(x = rep(12, 366), 
                order.by = as.POSIXlt(seq(as.Date("01-01-2020", format = "%m-%d"), 
                                          as.Date("12-31-2020", format = "%m-%d"), by = 1), 
                                      tz = "UTC") )
    time(zRef) <- format(time(zRef), "%m-%d")
    # Add in segement when temps are above 15 deg C at USF (Rainbows)
    hotSeg <- format(seq(as.Date("08-15-2020", format = "%m-%d"), 
                  as.Date("10-01-2020", format = "%m-%d"), by = 1), "%m-%d")
    coredata(zRef)[which(time(zRef) %in% intersect(time(zRef), hotSeg))] <- 16
    # Create time series of data chunk times populated with reference temp values
    zAvgDMn <- zoo(x = rep(NA, length(time(zAvgTemp)) ), 
                   order.by = as.POSIXlt(time(zAvgTemp), tz = "UTC") )
    time(zAvgDMn) <- format(time(zAvgDMn), "%m-%d")
    coredata(zAvgDMn) <- coredata(zRef)[which(time(zRef) %in% intersect(time(zAvgDMn),time(zRef)))]
    time(zAvgDMn) <- as.POSIXlt(time(zAvgTemp), tz = "UTC")
    # Now calc a 7-day rolling average
    zRollMn <- rollapply(zAvgDMn, 7, mean, align = "right", fill = "extend", partial = T) 
    message(zRollMn)
    
  } else if(data.dir == "LSF"){ 
    # create the reference time series (estimate based on 2020) for LSF
    zRef <- zoo(x = rep(8, 366), 
                order.by = as.POSIXlt(seq(as.Date("01-01-2020", format = "%m-%d"), 
                                          as.Date("12-31-2020", format = "%m-%d"), by = 1), 
                                      tz = "UTC") )
    time(zRef) <- format(time(zRef), "%m-%d")
    # Add in segement when temps are above 10 deg C at LSF (Browns), the segment based on
    # 2020 temperatures is June 1 to Nov 1. However, I only code it for June 1 to Sept 30 since
    # Brown trout spawning begins Oct 1 and the thresholds will switch to eggs/larvae Oct 1
    hotSeg <- format(seq(as.Date("06-01-2020", format = "%m-%d"), 
                         as.Date("09-30-2020", format = "%m-%d"), by = 1), "%m-%d")
    coredata(zRef)[which(time(zRef) %in% intersect(time(zRef), hotSeg))] <- 12
    # Create time series of data chunk times populated with reference temp values
    zAvgDMn <- zoo(x = rep(NA, length(time(zAvgTemp)) ), 
                   order.by = as.POSIXlt(time(zAvgTemp), tz = "UTC") )
    time(zAvgDMn) <- format(time(zAvgDMn), "%m-%d")
    coredata(zAvgDMn) <- coredata(zRef)[which(time(zRef) %in% intersect(time(zAvgDMn),time(zRef)))]
    time(zAvgDMn) <- as.POSIXlt(time(zAvgTemp), tz = "UTC")
    # Now calc a 7-day rolling average
    zRollMn <- rollapply(zAvgDMn, 7, mean, align = "right", fill = "extend", partial = T) 
    message(zRollMn)
    
  } else {
    # All other sites have avgDMn data in their cleanFiles that this handles
    # Collapse to just the Avg Daily Mean data at noon of each day
    zAvgDMn <- zAvgDMn[which(!is.na(coredata(zAvgDMn)))]
    # Drop times, dates only 
    time(zAvgDMn) <- format(time(zAvgDMn), "%Y-%m-%d")
    # If not same length, take what's in zAvgTemp 
    # NOTE: This will happen when data (z) begin anytime after noon within a day
    # zAvgDMn <- zAvgDMn[-1]
    new.Zoo <- merge(zAvgTemp, zAvgDMn, all = TRUE)
    # new.Zoo$zAvgTemp; new.Zoo$zAvgDMn
    
    # Fill in any missing daily avg values in temperature data with values from the Average 
    # Daily Mean data (AvgDMn)
    new.Zoo$zAvgTemp[which(is.na(new.Zoo$zAvgTemp))] <- new.Zoo$zAvgDMn[which(is.na(new.Zoo$zAvgTemp))]
    
    # Cut off leading NAs, if any
    zAvgTemp_2 <- na.trim(new.Zoo$zAvgTemp, sides = "left")
    # Now calc a 7-day rolling average
    zRollMn <- rollapply(zAvgTemp_2, 7, mean, align = "right", fill = "extend", partial = T) 
    # last 2 arguments allow for missing values in the middle and ends of the time series
    message(zRollMn)
  }
  
  ############  To here  #####################################
  ############################################################
  
  # Alright, when the data is run through createXAxis, the last day ends on 23:30 instead of 23:45... :(
  # It's for plotting labels, but it's a pain...
  nZAT <- length(zAvgTemp) # number days
  # DO factor based on temperature and spawning period: 3 for warm temps; 4 for cool temps; 5 for spawn-
  # to-emergence 
  DO.Fac <- rep(6, nZAT) # indicates whether warm or cool, corresponds to yset above
  # an NA in the zRollMn will remain case 6, which will print NA in the polygons.
  DO.Fac[which(zRollMn > temp)] <- 3 # warm temps
  DO.Fac[which(zRollMn <= temp)] <- 4 # cool temps
  # here, need to assign a 3 to whatever days are within spawn-to-emergence period, and rest of code 
  #  should work?
  DO.Fac[spInd] <- 5 
  
  # plot(zAvgTemp); abline(h = 15)
  # plot(zRollMn); abline(h = 15) # rolling mean creates a gap of 6 days on either side of a data gap...
  # plot(DO.Fac)
  
  # DO.Diff <- rep(0, nZAT) # for testing
  
  DO.Diff <- diff(DO.Fac)
  
  # indices of breaks
  endPtsDO <- c(1, which(DO.Diff != 0) + 1, nZAT)
  # plot(DO.Diff)
  
  # The break identifies the last element in each period (period 1, 2, or 3)
  # Find those dates that match the dates of the breaks and these divide up the space along the 
  # x axis for use in the polygon plotting.
  breakDatesDO <- as.POSIXlt(c(time(z)[1],
                               time(zRollMn[which(DO.Diff != 0)]),
                               time(z)[n]), tz = "UTC")
  
  # If DO.Diff contains only zeros, then breaks will only be the first and last element
  # of the plotting horizon, and so will always be at least length 2
  
  nEndPtsDO <- length(breakDatesDO)
  
  #################################################################################################
  # Build it for Temp, too.
  tempDiff <- diff(tempFac)
  
  # indices of breaks
  endPtsT <- c(1, which(tempDiff != 0) + 1, nZAT)
  # plot(tempDiff)
  
  # The break identifies the last element in each period (period 1, 2, or 3)
  # Find those dates that match the dates of the breaks and these divide up the space along the 
  # x axis for use in the polygon plotting.
  breakDatesT <- as.POSIXlt(c(time(z)[1],
                              time(zRollMn[which(tempDiff != 0)]),
                              time(z)[n]), tz = "UTC")
  
  nEndPtsT <- length(breakDatesT)
  
  # I may actually be able to cut tempVec and xset, they aren't used anywhere.
  # k <- 2
  # tempVec <- c(as.numeric(c(breakDates[1], breakDates[1], breakDates[2], breakDates[2])))
  # 
  # while(k < nEndPts){
  #   tempVec <- c(tempVec, 
  #                as.numeric(c(breakDates[k], breakDates[k], breakDates[k+1], breakDates[k+1])) )
  #   k <- k + 1
  # }
  # 
  # xset <- matrix(tempVec, 4, (nEndPts-1)) # each column will be a polygon arguement... is this use?
  # # Now, I'll have to call the polygon function as many times as (nEndPts - 1)
  
  return(list(yset = yset, shades = shades,
              breakDatesDO = breakDatesDO, DO.Fac = DO.Fac, endPtsDO = endPtsDO, nEndPtsDO = nEndPtsDO, 
              breakDatesT = breakDatesT, tempFac = tempFac, endPtsT = endPtsT, nEndPtsT = nEndPtsT))
  # l <- list(yset = yset, shades = shades,
  #           breakDatesDO = breakDatesDO, DO.Fac = DO.Fac, endPtsDO = endPtsDO, nEndPtsDO = nEndPtsDO, 
  #           breakDatesT = breakDatesT, tempFac = tempFac, endPtsT = endPtsT, nEndPtsT = nEndPtsT)
  
  # plot(0,type='n',axes=FALSE,ann=FALSE)
}

