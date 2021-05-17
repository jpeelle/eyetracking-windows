# Scaled-down copy of 0-runWindowModels.R to make sure time
# selection is working properly (thanks Reviewer 2!)*
#
# * no really, tha'ts not sarcastic, thank you




# Time bins are 16.6667 ms (100/6) long. All times below in milliseconds.
startTimes <- seq(from = 700, to = 2200, by = 100/6)
windowLengths <- seq(from = 300, to = 1800, by = 100/6)


# ---- setup ----
script.dir <- dirname(sys.frame(1)$ofile)
outDir <- file.path(script.dir, "outputs")


# get data

df <- read.csv(file.path("eyetracking-data", "VanEngen2020.csv"), header=TRUE, sep=",")


# sum coding
df$Group <- C(as.factor(df$Group), sum)
df$SNR <- C(as.factor(df$SNR), sum)
df$Condition <- C(as.factor(df$Condition), sum)


#---- loop through start/length combinations and run model ----


#for(i = 1:length(startTimes)) {
for(i in 1) {
  for(thisLength in windowLengths) {
      
      # Select data subset based on time parameters
      dfTrimmed <- df[df$TimeMS >= startTimes[i],]
      dfTrimmed <- dfTrimmed[dfTrimmed$TimeMS <= (startTimes[i] + thisLength + .1),]
      dfTrimmed$TimePoint <- dfTrimmed$TimePoint - dfTrimmed$TimePoint[1] + 1

      
      print(sprintf("start: %.2f, end: %.2f • min: %.2f, max: %.2f\n", startTimes[i], startTimes[i] + thisLength, min(dfTrimmed$TimeMS), max(dfTrimmed$TimeMS)))
      


  } # window lengths

}
