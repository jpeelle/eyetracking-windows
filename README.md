# eyetracking-windows

Looking at different time windows of analysis for a visual world eye tracking
paradigm.

## Motivation

The time window to-be-modeled is a critically important part of time series analysis, but seldom receives direct study. We set out to empirically test how the time window selection (i.e., the data to be modeled) affect parameter estimates and p values in the context of a real visual world eye tracking study of spoken word recognition. Further details are found in the paper.


## Files

To run the analysis reported in the paper, run these files in order:

1. `0-runWindowModels.R` which runs statistical models for all unique combinations of time window start and duration (using parallel processing). The results from each model are saved in their own .csv file. If the program stops or crashes in the middle, it should pick up where it left off.

2. `1-combineWindowModels.R` combines individual .csv files from the first step into one big .csv file suitable for reading into a data frame.

3. `2-plotData.R` plots key aspects of the combined data (loaded from any file, e.g., one saved from `1-combineWindowModels.R`).



## Reference
