# Master script for looping through time windows and 
# running statistical models, saving outputs to text files.


#---- Import libraries and set options ----

library(ggplot2)
library(lme4)
library(doParallel)
library(foreach)

# Time bins are 16.6667 ms long. All times below in milliseconds.

startTimes <- c(1000, 1500, 2000)
windowLengths <- c(100, 600, 1100)

numCores <- 9

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

dataFreq[,paste("ot", 1:3, sep="")] <- 
  t[dataFreq$TimePoint, 1:3]

#sum coding
dataFreq$Group <- C(dataFreq$Group, sum)
dataFreq$SNR <- C(dataFreq$SNR, sum)
dataFreq$Condition <- C(dataFreq$Condition, sum)

contrasts(dataFreq$Group)
contrasts(dataFreq$SNR) 
contrasts(dataFreq$Condition)

#full model

AFNCube8=glmer(cbind(WordsTarget, WordsTotal-WordsTarget) ~
                 (ot1+ot2+ot3)*(Group*SNR*Condition)+ 
                 (ot1+ot2+ot3 |Subject) + 
                 (ot1+ot2+ot3 |Subject:Condition),
               data=dataFreq, 
               family=binomial,
               control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)))

summary(AFNCube8)


#---- loop through start/length combinations and run model ----


cl <- makeCluster(numCores)
registerDoParallel(cl, cores=numCores)

#for (thisStart in startTimes) {

foreach(i = 1:length(startTimes)) %dopar% 
  print(sprintf("hi %d", i))
  
  for (thisLength in windowLengths) {
    
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
                   (ot1+ot2+ot3 | Subject) +
                   (ot1+ot2+ot3 | Subject:Condition),
                 data=dfTrimmed,
                 family=binomial,
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)))

      s <- summary(m)
      
      # write summary variables to file
      
      
      
    } # end of if/else for file existing

  } # window lengths

# (parallel processing with foreach ended here)


