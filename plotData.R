
#---- Import libraries and set options ----

library(ggplot2)
library(ggpubr)

dataFileName <- "start700-2200_length300-1800_2020-04-29-2156.csv"

#---- read in data we want ----

dfAll <- read.csv(dataFileName, header=TRUE, sep=",")


#---- plot ----

termsToPlot <- c("ot1", "ot2", "ot3")

# filter rows

par(mfrow=c(length(termsToPlot),2))

for(termInd in  1:length(termsToPlot)) {
  
  thisTerm = termsToPlot[termInd]
  
  # filter to just get rows for current term
  dfTmp <- dfAll[dfAll$term==thisTerm,]
  
  p <- ggplot(dfTmp, aes(startTime, windowLength, fill = p.value)) +
    geom_tile() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal() + 
    scale_fill_viridis_c(direction = -1, option = "plasma") +  # plasma, viridis
    xlab("Start Time (ms)") +
    ylab("Window Length (ms)") +
    ggtitle(thisTerm)
  
  
  
}
