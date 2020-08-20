# Loop through some time bins to plot the selection


#---- Import libraries and set options ----

library(ggplot2)
library(lme4)
library(grid)
#library(broom)
#library(doParallel)
#library(foreach)


# Time bins are 16.6667 ms (100/6) long. All times below in milliseconds.
startTime <- 1300
windowLengths <- seq(from = 300, to = 1800, by = 100/6)
numCores <- 14


# ---- setup ----
script.dir <- dirname(sys.frame(1)$ofile)
outDir <- file.path(script.dir, "movieimg")


# get data

df <- read.csv(file.path("eyetracking-data", "VanEngen2020.csv"), header=TRUE, sep=",")


#---- plot overall timecourse ----


for(i in 1:length(windowLengths)) {
  
  thisLength <- windowLengths[i]
  
  endTime <- startTime + thisLength
  
  outFile <- file.path(outDir, sprintf("%g_%.2f.png", startTime, endTime))
  
  
  # load the right stats file for this combination and rename columns
  dfstats <- read.csv(file.path("outputs", sprintf("%g_%g.csv", startTime, thisLength)))
  
  dfstats$term <- as.factor(dfstats$term)
  
  dfstats$term <- recode(dfstats$term,
                       ot1 = "linear",
                       ot2 = "quadratic",
                       ot3 = "cubic",
                       Group1 = "age",
                       SNR1 = "noise",
                       Condition1 = "frequency",
                       "ot1:Group1" = "linear:age",
                       "ot1:SNR1" = "linear:noise",
                       "ot1:Condition1" = "linear:frequency",
                       "Group1:Condition1" = "age:frequency",
                       "Group1:SNR1" = "age:noise",
                       "Group1:SNR1:Condition1" = "age:noise:frequency")
  
  
  p <- ggplot(df, aes(TimeMS, PercentTarget, fill=Group, linetype=Condition)) + 
    theme_classic(base_size = 12) +
    ggtitle(sprintf("Start: %g, End: %.2f", startTime, endTime)) + 
    ylab("Percent fixations to target") + 
    xlab("Time (ms)") +
    annotate("rect", xmin = startTime, xmax = endTime, ymin = -1, ymax = 100, alpha = .2) +
    stat_summary(fun.data=mean_se, geom="ribbon", alpha=.2) +
    stat_summary(aes(y=PercentTarget, linetype=Condition), fun=mean, geom="line") +
    scale_fill_manual(values = c("red", "gray50")) +
    theme(legend.position=c(.85, .35)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # p + theme(
  #   plot.title = element_text(size=40, face="bold.italic"),
  #   axis.title.x = element_text(size=40, face="bold"),
  #   axis.title.y = element_text(size=40, face="bold"),
  #   legend.title = element_text(size=26),
  #   legend.text = element_text(size=26)
  # )
  

  
  
  cond <- c("linear", "quadratic", "cubic", "age", "noise", "frequency", "linear:age", "linear:noise", "linear:frequency")
  
  dfstats <- dfstats %>% filter(term %in% cond)
  
  pp <- ggplot(data = dfstats, aes(x=term, y=p.value)) + 
    geom_bar(stat="identity", color = "white", fill = "lightblue") +
    geom_hline(yintercept = .05, linetype = "dotted", color = "blue", size = 0.5) +
    annotate(geom="text", x = 1.5, y = .08, label="p = .05", color = "blue") +
    theme_classic(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylim(0, 1) +
    ylab("p value")
  
  
  
  pest <- ggplot(data = dfstats, aes(x=term, y=estimate)) + 
    geom_bar(stat="identity", color = "white", fill = "lightgreen") +
    theme_classic(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylim(-3, 14) +
    ylab("parameter estimate")
  
  
  ggp <- ggarrange(p, ggarrange(pp, pest, ncol = 2), nrow = 2)
  
  png(filename = outFile,
      width = 800, height = 800, units = "px", pointsize = 12,
      bg = "white", res = NA)
  
  show(ggp)

  dev.off()
  
} # going through window lengths


#---- Use convert command, part of imagemagick, to create a gif ----
# imagemagick available on mac via homebrew (http://brew.sh)

movie_speed = 10
cmd = paste0('convert -antialias -delay 1x', movie_speed, ' ', outDir, '/*.png ', 'timebins.gif')
system(cmd)
