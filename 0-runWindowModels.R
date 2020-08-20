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


# Time bins are 16.6667 ms (100/6) long. All times below in milliseconds.
startTimes <- seq(from = 700, to = 2200, by = 100/6)
windowLengths <- seq(from = 300, to = 1800, by = 100/6)

# For parallel processing
numCores <- 14

# ---- setup ----
script.dir <- dirname(sys.frame(1)$ofile)
outDir <- file.path(script.dir, "outputs")


# get data

df <- read.csv(file.path("eyetracking-data", "VanEngen2020.csv"), header=TRUE, sep=",")


#---- plot overall timecourse ----

p <- ggplot(df, aes(TimeMS, PercentTarget, fill=Group, linetype=Condition)) + 
  theme_classic(base_size = 10) +
  ylab("Percent fixations to target") + 
  xlab("Time (ms)") +
  annotate("rect", xmin = 1300, xmax = 2300, ymin = -1, ymax = 100, alpha = .2) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha=.2) +
  stat_summary(aes(y=PercentTarget, linetype=Condition), fun=mean, geom="line") +
  scale_fill_manual(values = c("red", "gray50")) +
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

foreach(i = 1:length(startTimes)) %dopar% {
  
  for(thisLength in windowLengths) {
    outFile <- file.path(outDir, sprintf("%g_%g.csv", startTimes[i], thisLength))
    
    # If the file doesn't exist, do the work and save it
    if(file.exists(outFile)) {
      
      print(sprintf("%s already exists, skipping.", outFile))
      
    } else {
      
      print(sprintf("%s not found, running model.", outFile))
      
      # Select datasubset based on time parameters
      dfTrimmed <- df[df$TimeMS > startTimes[i],]
      dfTrimmed <- dfTrimmed[dfTrimmed$TimeMS < (startTimes[i] + thisLength),]
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

# To combine all of these individual models files into a big dataframe,
# use 1-combineWindowModels.R