# Master script for looping through time windows and 
# running statistical models, saving outputs to text files.
#
# Plots of models are in plotData.R (assumes a saved dataframe)


#---- Import libraries and set options ----

library(ggplot2)
library(lme4)
library(broom)
library(doParallel)
library(foreach)


# What do we call the collated save file? (date appended automatically)
saveFileName <- "start800-2200_length400-1800"

# Time bins are 16.6667 ms (100/6) long. All times below in milliseconds.
startTimes <- seq(from = 800, to = 2200, by = 100/6)
windowLengths <- seq(from = 400, to = 1800, by = 100/6)

# For parallel processing
numCores <- 14

# ---- setup ----
script.dir <- dirname(sys.frame(1)$ofile)
outDir <- file.path(script.dir, "outputs")


# get data

df <- read.csv(file.path("data", "VanEngen2020.csv"), header=TRUE, sep=",")


#---- plot overall timecourse ----

p <- ggplot(df, aes(TimeMS, PercentTarget, fill=Group, linetype=Condition)) + 
  theme_bw(base_size = 10)+
  ylab("Percent fixations to target") + 
  xlab("Time (ms)") +
  ggtitle("Fixations to target words from carrier phrase onset") +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha=.2) +
  stat_summary(aes(y=PercentTarget, linetype=Condition), fun=mean, geom="line") +
  theme(legend.position="bottom")

p + theme(
  plot.title = element_text(size=14, face="bold.italic"),
  axis.title.x = element_text(size=14, face="bold"),
  axis.title.y = element_text(size=14, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)

show(p)



#---- model parameters that don't change with time ----


#time selection






t <- poly(unique(df$TimePoint), 3)

df[,paste("ot", 1:3, sep="")] <- 
  t[df$TimePoint, 1:3]

#sum coding
df$Group <- C(df$Group, sum)
df$SNR <- C(df$SNR, sum)
df$Condition <- C(df$Condition, sum)

#contrasts(df$Group)
#contrasts(df$SNR) 
#contrasts(df$Condition)


#---- loop through start/length combinations and run model ----


cl <- makeCluster(numCores)
registerDoParallel(cores=numCores)

#for (thisStart in startTimes) {

foreach(i = 1:length(startTimes)) %dopar% {
  
  for(thisLength in windowLengths) {
    outFile <- file.path(outDir, sprintf("%i_%i.csv", startTimes[i], thisLength))
    
    # If the file doesn't exist, do the work and save it
    if(file.exists(outFile)) {
      
      print(sprintf("%s already exists, skipping.", outFile))
      
    } else {
      
      print(sprintf("%s not found, running model.", outFile))
      
      # Select datasubset based on time parameters
      dfTrimmed <- df[df$TimeMS > thisStart,]
      dfTrimmed <- dfTrimmed[dfTrimmed$TimeMS < (thisStart + thisLength),]
      dfTrimmed$TimePoint <- dfTrimmed$TimePoint - dfTrimmed$TimePoint[1] + 1

      # polynomial predictors
      t <- poly(unique(dfTrimmed$TimePoint), 3)

      dfTrimmed[,paste("ot", 1:3, sep="")] <- t[dfTrimmed$TimePoint, 1:3]

      #sum coding
      dfTrimmed$Group <- C(dfTrimmed$Group, sum)
      dfTrimmed$SNR <- C(dfTrimmed$SNR, sum)
      dfTrimmed$Condition <- C(dfTrimmed$Condition, sum)

      contrasts(dfTrimmed$Group)
      contrasts(dfTrimmed$SNR)
      contrasts(dfTrimmed$Condition)

      m <- glmer(cbind(WordsTarget, WordsTotal-WordsTarget) ~
                   (ot1+ot2+ot3)*(Group*SNR*Condition) +
                   (ot1+ot2 | Subject) +
                   (ot1+ot2+ot3 | Subject:Condition),
                 data=dfTrimmed,
                 family=binomial,
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)))

      # convert model info to data frame and write to file
      s <- tidy(m)
      write.csv(s, file = outFile)
      
    } # end of if/else for file existing

  } # window lengths

} # %dopar%
stopCluster(cl)




#---- loop through to combine into one big data frame ----

foreach(i = 1:length(startTimes)) %do% {
  
  for(j in 1:length(windowLengths)) {
    dataFile <- file.path(outDir, sprintf("%i_%i.csv", startTimes[i], windowLengths[j]))
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

