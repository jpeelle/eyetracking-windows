

#---- Import libraries and set options ----

library(ggplot2)
library(ggpubr)
library(dplyr)

startTime <- 1300
windowLengths <- seq(from = 800, to = 1200, by = 100/2)
windowLengthMin <- min(windowLengths)
windowLengthMax <- max(windowLengths)

script.dir <- dirname(sys.frame(1)$ofile)

dataFileName <- "start700-2200_length300-1800_2020-04-29-2156.csv"


dfAll <- read.csv(dataFileName, header=TRUE, sep=",")

dfAll$windowLength <- as.numeric(dfAll$windowLength)
dfAll$startTime <- as.numeric(dfAll$startTime)

dfAll$term <- as.factor(dfAll$term)

dfAll$term <- recode(dfAll$term,
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


# First filter for start time; then for durations; then for condition
cond <- c("age", "noise", "frequency", "linear:age", "linear:noise", "linear:frequency")

df <- dfAll[dfAll$startTime==1300,]
df <- df %>% filter(windowLength %in% windowLengths)
df <- df %>% filter(term %in% cond)

#---- plot parameter estimates and p values grouped by effect, colored by windowLength ----

df$windowLength <- as.factor(df$windowLength)

p1 <- ggbarplot(df, x = "term", y = "estimate", order = cond,
                color = "windowLength", fill = "windowLength", 
                palette = "RdBu",
                position = position_dodge2(padding = 0.2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("parameter estimate")

p2 <- ggbarplot(df, x = "term", y = "p.value", order = cond,
                color = "windowLength", fill = "windowLength", 
                palette = "RdBu",
                position = position_dodge2(padding = 0.2)) +
  geom_hline(yintercept = .05, linetype = "dotted", color = "blue", size = 0.5) +
  annotate(geom="text", x = 1, y = .075, label="p = .05", color = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("p value")

ggarrange(p1, p2, ncol = 1, nrow = 2)

