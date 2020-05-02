
#---- Import libraries and set options ----

library(ggplot2)
library(ggpubr)

dataFileName <- "start700-2200_length300-1800_2020-04-29-2156.csv"

#---- read in data we want ----

dfAll <- read.csv(dataFileName, header=TRUE, sep=",")

# replace ot1, ot2, ot3 with more friendly linear, quadratic, cubic
# TODO


#---- plot ----

termsToPlot <- c("ot1", "ot2", "ot3")

# filter rows

par(mfrow=c(2, length(termsToPlot)))

plist <- c()


# From https://stackoverflow.com/questions/31993704/storing-ggplot-objects-in-a-list-from-within-loop-in-r

# plot_data_column = function (data, column) {
#   ggplot(data, aes_string(x = column)) +
#     geom_histogram(fill = "lightgreen") +
#     xlab(column)
# }
# 
# myplots <- lapply(colnames(data2), plot_data_column, data = data2)

plotpvalues = function (data, thisTerm) {
  dfTmp <- data[data$term==thisTerm,]
  
  ggplot(dfTmp, aes(startTime, windowLength, fill = p.value)) +
    geom_tile() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal() + 
    scale_fill_viridis_c(direction = -1, option = "plasma") +  # plasma, viridis
    xlab("Start Time (ms)") +
    ylab("Window Length (ms)") +
    ggtitle(thisTerm)
}

plotestimate = function (data, thisTerm) {
  dfTmp <- data[data$term==thisTerm,]
  
  ggplot(dfTmp, aes(startTime, windowLength, fill = estimate)) +
    geom_tile() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal() + 
    scale_fill_viridis_c(direction = -1, option = "plasma") +  # plasma, viridis
    xlab("Start Time (ms)") +
    ylab("Window Length (ms)") +
    ggtitle(thisTerm)
}

plotsP <- lapply(termsToPlot, plotpvalues, data = dfAll)
plotsEstimate <- lapply(termsToPlot, plotestimate, data = dfAll)

plotsAll <- c(plotsEstimate, plotsP)
ggarrange(plotlist = plotsAll, nrow = 2, ncol = 3)
