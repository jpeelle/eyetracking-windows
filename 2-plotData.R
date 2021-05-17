
#---- Import libraries and set options ----

library(ggplot2)
library(ggpubr)
library(dplyr)
library(viridis)

dataFileName <- "start700-2200_length300-1800_2021-05-16-0642.csv"

#---- read in data we want ----

dfAll <- read.csv(dataFileName, header=TRUE, sep=",")

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


#---- plot main effects of time ----

termsToPlot <- c("linear", "quadratic", "cubic")

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
    geom_raster() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal() + 
    scale_fill_viridis(option = "turbo", begin = 0, end = 1, limits = c(0, 1)) +
    #scale_fill_gradient(low = "white", high = "gray", limits = c(0, 1)) +
    xlab("Start Time (ms)") +
    ylab("Window Length (ms)") +
    ggtitle(thisTerm) +
    theme(legend.position="none") +
    NULL
}

plotestimate = function (data, thisTerm) {
  dfTmp <- data[data$term==thisTerm,]
  
  yyz <- max(c(max(dfTmp$estimate), abs(min(dfTmp$estimate))))
  yyx <- 0-yyz
  
  print(yyx)
  print(yyz)
  
  ggplot(dfTmp, aes(startTime, windowLength, fill = estimate)) +
    geom_raster() + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal() + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(yyx, yyz)) +
    xlab("Start Time (ms)") +
    ylab("Window Length (ms)") +
    ggtitle(thisTerm) +
    theme(legend.position="none") +
    NULL
}

plotsP <- lapply(termsToPlot, plotpvalues, data = dfAll)
plotsEstimate <- lapply(termsToPlot, plotestimate, data = dfAll)

plotsAll <- c(plotsEstimate, plotsP)
ggarrange(plotlist = plotsAll, nrow = 2, ncol = 3)







#---- plot main effects of age, noise, and frequency ----

termsToPlot <- c("age", "noise", "frequency")

# filter rows

par(mfrow=c(2, length(termsToPlot)))

plist <- c()

plotpvalues = function (data, thisTerm) {
  dfTmp <- data[data$term==thisTerm,]
  
  ggplot(dfTmp, aes(startTime, windowLength, fill = p.value)) +
    geom_raster() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal() + 
    scale_fill_viridis(option = "turbo", begin = 0, end = 1, limits = c(0, 1)) +
    #scale_fill_gradient(low = "white", high = "gray") +
    xlab("Start Time (ms)") +
    ylab("Window Length (ms)") +
    ggtitle(thisTerm) +
    theme(legend.position="none") +
    NULL
}

plotestimate = function (data, thisTerm) {
  dfTmp <- data[data$term==thisTerm,]
  
  yyz <- max(c(max(dfTmp$estimate), abs(min(dfTmp$estimate))))
  yyx <- 0-yyz
  
  print(yyx)
  print(yyz)
  
  ggplot(dfTmp, aes(startTime, windowLength, fill = estimate)) +
    geom_raster() + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal() + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    xlab("Start Time (ms)") +
    ylab("Window Length (ms)") +
    ggtitle(thisTerm) +
    theme(legend.position="none") +
    NULL
}

plotsP <- lapply(termsToPlot, plotpvalues, data = dfAll)
plotsEstimate <- lapply(termsToPlot, plotestimate, data = dfAll)

plotsAll <- c(plotsEstimate, plotsP)
ggarrange(plotlist = plotsAll, nrow = 2, ncol = 3)




#---- plot linear effects of age, noise and frequency ----

termsToPlot <- c("linear:age", "linear:noise", "linear:frequency")

# filter rows

par(mfrow=c(2, length(termsToPlot)))

plist <- c()

plotpvalues = function (data, thisTerm) {
  dfTmp <- data[data$term==thisTerm,]
  
  ggplot(dfTmp, aes(startTime, windowLength, fill = p.value)) +
    geom_raster() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal() + 
    scale_fill_viridis(option = "turbo", begin = 0, end = 1, limits = c(0, 1)) +
    #scale_fill_gradient(low = "white", high = "gray") +
    xlab("Start Time (ms)") +
    ylab("Window Length (ms)") +
    ggtitle(thisTerm) +
    theme(legend.position="none") +
    NULL
}

plotestimate = function (data, thisTerm) {
  dfTmp <- data[data$term==thisTerm,]
  
  yyz <- max(c(max(dfTmp$estimate), abs(min(dfTmp$estimate))))
  yyx <- 0-yyz
  
  print(yyx)
  print(yyz)
  
  ggplot(dfTmp, aes(startTime, windowLength, fill = estimate)) +
    geom_raster() + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal() + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    xlab("Start Time (ms)") +
    ylab("Window Length (ms)") +
    ggtitle(thisTerm) +
    theme(legend.position="none") +
    NULL
}

plotsP <- lapply(termsToPlot, plotpvalues, data = dfAll)
plotsEstimate <- lapply(termsToPlot, plotestimate, data = dfAll)

plotsAll <- c(plotsEstimate, plotsP)
ggarrange(plotlist = plotsAll, nrow = 2, ncol = 3)



#---- plot more effects ----

termsToPlot <- c("age:noise", "age:frequency", "age:noise:frequency")

# filter rows

par(mfrow=c(2, length(termsToPlot)))

plist <- c()

plotpvalues = function (data, thisTerm) {
  dfTmp <- data[data$term==thisTerm,]
  
  ggplot(dfTmp, aes(startTime, windowLength, fill = p.value)) +
    geom_raster() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal() + 
    scale_fill_viridis(option = "turbo", begin = 0, end = 1, limits = c(0, 1)) +
    #scale_fill_gradient(low = "white", high = "gray") +
    xlab("Start Time (ms)") +
    ylab("Window Length (ms)") +
    ggtitle(thisTerm) +
    theme(legend.position="none") +
    NULL
}

plotestimate = function (data, thisTerm) {
  dfTmp <- data[data$term==thisTerm,]
  
  yyz <- max(c(max(dfTmp$estimate), abs(min(dfTmp$estimate))))
  yyx <- 0-yyz
  
  print(yyx)
  print(yyz)
  
  ggplot(dfTmp, aes(startTime, windowLength, fill = estimate)) +
    geom_raster() + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal() + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    xlab("Start Time (ms)") +
    ylab("Window Length (ms)") +
    ggtitle(thisTerm) +
    theme(legend.position="none") +
    NULL
}

plotsP <- lapply(termsToPlot, plotpvalues, data = dfAll)
plotsEstimate <- lapply(termsToPlot, plotestimate, data = dfAll)

plotsAll <- c(plotsEstimate, plotsP)
ggarrange(plotlist = plotsAll, nrow = 2, ncol = 3)



