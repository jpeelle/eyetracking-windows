

#---- Import libraries and set options ----

# Time bins are 16.6667 ms (100/6) long. All times below in milliseconds.
startTimes <- seq(from = 800, to = 2200, by = 100/6)
windowLengths <- seq(from = 400, to = 1800, by = 100/6)

# What do we call the collated save file? (date appended automatically)
saveFileName <- "start800-2200_length400-1800"


#---- loop through to combine into one big data frame ----

foreach(i = 1:length(startTimes)) %do% {
  
  for(j in 1:length(windowLengths)) {
    dataFile <- file.path(outDir, sprintf("%g_%g.csv", startTimes[i], windowLengths[j]))
    dfTmp = read.csv(dataFile, header=TRUE, sep=",")
    
    # add in current conditions
    dfTmp[, "startTime"] <- startTimes[i]
    dfTmp[, "windowLength"] <- windowLengths[j]
    
    # If the very first one, use that as a template; otherwise, rbind
    if (i==1 && j==1) {
      dfAll <- dfTmp
    } else {
      dfAll <- rbind(dfAll, dfTmp)
    }
    
  } # window lengths
} # start times


# save
write.csv(dfAll, file = sprintf("%s_%s.csv", saveFileName, strftime(Sys.time(), format = "%Y-%m-%d-%H%M")))

