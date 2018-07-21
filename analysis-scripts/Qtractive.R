#### Tractive forces analysis for each stream ####
source("./analysis-scripts/QDeoth.R")

st_temps <- read.csv("./stream-data/stream_temps.csv",T)

#calculate tractive forces for all streams at all dates
 ##create long dataframe by subsetting the columns with certain data types