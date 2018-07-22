setwd("C:/Users/Jim/Documents/Projects/Iceland/Disturbance/Substrate")

##load libraries
library(ggplot2)
library(chron)
library(gridExtra)
library(zoo)
library(scales)
library(GGally)
library(plyr)
library(data.table)
library(corrplot)
library(reshape2)
theme_set(theme_bw(20))

## read in the Q all discharge file
Q_all <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Stream Discharge/All Q/Q_all.csv")


Q_all$Pd <- as.POSIXct(paste(Q_all$Date), format = "%d-%m-%y", tz = "UTC")
