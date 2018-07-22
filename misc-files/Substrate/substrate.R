###### Script for Sediment size distributions  #######

####   Setting working drive  ####

setwd("C:/Users/Jim/Documents/Projects/Iceland/Disturbance/Substrate")
###  Loading the required packages  ###
library(reshape2)
library(tidyr)
library(plyr)
library(ggplot2)

theme_set(theme_bw(20))
### Load in the substrate file  ####
sediment <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Disturbance/Substrate/Substrate Size.csv", T)
max(sediment$Size)
####  Function for plotting ecdf with line on ggplot  ####

ecdf.plot <- function(DATA)
{

x = sort(unique(DATA[,2]))
vals = cumsum(tabulate(match(DATA[,2] , unique(DATA[,2]))))/length(DATA[,2])
df = data.frame(x, vals)
ggplot(df, aes(x = vals, y = x)) + geom_point(size = 3, shape = 19, colour = "#999999") + geom_line(lwd = 1.2) +
	labs(x = "Cumulative Frequency", y = "Particle Size (mm)") + scale_y_continuous(limits = c(0,266)) +
  annotate("text", x = 0.12, y = 250, label = as.character(levels(droplevels(DATA$Stream))))
}

unique(levels(sediment$Stream))
sed_st14 = ecdf.plot(sediment[which(sediment$Stream == "ST14"),]);sed_st14
sed_st13 = ecdf.plot(sediment[which(sediment$Stream == "ST13"),])
sed_st17 = ecdf.plot(sediment[which(sediment$Stream == "ST17"),])
sed_st1 = ecdf.plot(sediment[which(sediment$Stream == "ST1"),])
sed_st5 = ecdf.plot(sediment[which(sediment$Stream == "ST5"),])
sed_st6 = ecdf.plot(sediment[which(sediment$Stream == "ST6"),])
sed_st9 = ecdf.plot(sediment[which(sediment$Stream == "ST9"),])
sed_st11u = ecdf.plot(sediment[which(sediment$Stream == "ST11 U"),])
sed_st11d = ecdf.plot(sediment[which(sediment$Stream == "ST11 D"),])

stream_list = list(sed_st14, sed_st13, sed_st17, sed_st1, sed_st5, sed_st6, sed_st9,
                   sed_st11d, sed_st11u)

png(file = "sediment-size-dist.png", res = 150, height = 15, width = 10, units = 'in')
gridExtra::grid.arrange(sed_st14, sed_st13, sed_st17, sed_st1, sed_st5, sed_st6, sed_st9,
                        sed_st11d, sed_st11u, ncol = 2)
dev.off()

unique(droplevels(sediment[which(sediment$Stream == "ST14"),]))

x = sort(unique(st14[,2]))
vals = cumsum(tabulate(match(st14[,2], unique(st14[,2]))))/length(st14[,2])
plot(vals, x)

####  

st14 <- sediment[which(sediment$Stream == "ST14"),]
cum14 <- ggplot(st14, aes(Size)) + stat_ecdf(geom = "step", lwd = 1.5) +
	scale_x_continuous(limits = c(0, 300)) +
	labs(y = "Particle Size (mm)", x = "Cumulative Frequency"); cum14

st13 <- sediment[which(sediment$Stream == "ST13"),]
cum13 <- ggplot(st13, aes(Size)) + stat_ecdf(geom = "step", lwd = 1.5) +
	scale_x_continuous(limits = c(0, 300)) +
	labs(y = "Particle Size (mm)", x = "Cumulative Frequency"); cum13

st17 <- sediment[which(sediment$Stream == "ST17"),]
cum17 <- ggplot(st17, aes(Size)) + stat_ecdf(geom = "step", lwd = 1.5)  +
	scale_x_continuous(limits = c(0, 300)) +
	labs(y = "Particle Size (mm)", x = "Cumulative Frequency"); cum17

st1 <- sediment[which(sediment$Stream == "ST1"),]
cum1 <- ggplot(st1, aes(Size)) + stat_ecdf(geom = "step", lwd = 1.5)  +
	scale_x_continuous(limits = c(0, 300)) +
	labs(y = "Particle Size (mm)", x = "Cumulative Frequency"); cum1

st5 <- sediment[which(sediment$Stream == "ST5"),]
cum5 <- ggplot(st5, aes(Size)) + stat_ecdf(geom = "step", lwd = 1.5)  +
	scale_x_continuous(limits = c(0, 300)) +
	labs(y = "Particle Size (mm)", x = "Cumulative Frequency"); cum5

st6 <- sediment[which(sediment$Stream == "ST6"),]
cum6 <- ggplot(st6, aes(Size)) + stat_ecdf(geom = "step", lwd = 1.5)  +
	scale_x_continuous(limits = c(0, 300)) +
	labs(y = "Particle Size (mm)", x = "Cumulative Frequency"); cum17

st8 <- sediment[which(sediment$Stream == "ST8"),]
cum8 <- ggplot(st8, aes(Size)) + stat_ecdf(geom = "step", lwd = 1.5)  +
	scale_x_continuous(limits = c(0, 300)) +
	labs(y = "Particle Size (mm)", x = "Cumulative Frequency"); cum8

st9 <- sediment[which(sediment$Stream == "ST9"),]
cum9 <- ggplot(st9, aes(Size)) + stat_ecdf(geom = "step", lwd = 1.5)  +
	scale_x_continuous(limits = c(0, 300)) +
	labs(y = "Particle Size (mm)", x = "Cumulative Frequency"); cum9

st11D <- sediment[which(sediment$Stream == "ST11 D"),]
cum11D <- ggplot(st11D, aes(Size)) + stat_ecdf(geom = "step", lwd = 1.5)  +
	scale_x_continuous(limits = c(0, 300)) +
	labs(y = "Particle Size (mm)", x = "Cumulative Frequency"); cum11D

st11U <- sediment[which(sediment$Stream == "ST11 U"),]
cum11U <- ggplot(st11U, aes(Size)) + stat_ecdf(geom = "step", lwd = 1.5)  +
	scale_x_continuous(limits = c(0, 300)) +
	labs(y = "Particle Size (mm)", x = "Cumulative Frequency"); cum11U


x = sort(unique(st14$Size))

vals = cumsum(tabulate(match(st14$Size, unique(st14$Size))))/length(st14$Size)


plot(Fn)
