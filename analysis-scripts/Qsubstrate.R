###### Script for Sediment size distributions, Relative bed stability, and 
######  breaking down the metrics into individual files. #######
source("./analysis-scripts/Qtractive.R")
###  Loading the required packages  ###
library(stringr)
theme_set(theme_bw(20))
### Load in the substrate file  ####
sediment <- read.csv(file = "./stream-data/substrate-size.csv", T, stringsAsFactors = F)
st1.fix = which(sediment$Stream == "ST1")
sediment[st1.fix, "Stream"] = "st1"
st5.fix = which(sediment$Stream == "ST5")
sediment[st5.fix, "Stream"] = "st5"
st6.fix = which(sediment$Stream == "ST6")
sediment[st6.fix, "Stream"] = "st6"
st8.fix = which(sediment$Stream == "ST8")
sediment[st8.fix, "Stream"] = "st8"
st9.fix = which(sediment$Stream == "ST9")
sediment[st9.fix, "Stream"] = "st9"
st11L.fix = which(sediment$Stream == "ST11 D")
sediment[st11L.fix, "Stream"] = "st11L"
st11U.fix = which(sediment$Stream == "ST11 U")
sediment[st11U.fix, "Stream"] = "st11U"
st13.fix = which(sediment$Stream == "ST13")
sediment[st13.fix, "Stream"] = "st13"
st14.fix = which(sediment$Stream == "ST14")
sediment[st14.fix, "Stream"] = "st14"
st17.fix = which(sediment$Stream == "ST17")
sediment[st17.fix, "Stream"] = "st17"
hver.fix = which(sediment$Stream == "Hver")
sediment[hver.fix, "Stream"] = "hver"
sediment = data.frame(unclass(sediment))

max(sediment$Size)
levels(sediment$Stream)
##calculate relative bed stability
pres_allhr$st1_RBS = (0.155*sqrt(median(sediment$Size[which(sediment$Stream == "st1")])))/(0.7*pres_allhr$st1_vel)
#hist(pres_allhr$st1_RBS)
#ggplot(pres_allhr, aes(x = Pd, y = st1_RBS)) + geom_point(size = 2)+ geom_hline(yintercept = 1)
length(which(pres_allhr$st1_RBS <= 1))
mean(pres_allhr$st1_RBS, na.rm = T)
pres_allhr$st5_RBS = (0.155*sqrt(median(sediment$Size[which(sediment$Stream == "st5")])))/(0.7*pres_allhr$st5_vel)
#hist(pres_allhr$st5_RBS)
#ggplot(pres_allhr, aes(x = Pd, y = st5_RBS)) + geom_point(size = 2)+ geom_hline(yintercept = 1)
length(which(pres_allhr$st5_RBS <= 1))
mean(pres_allhr$st5_RBS, na.rm = T)
pres_allhr$st6_RBS = (0.155*sqrt(median(sediment$Size[which(sediment$Stream == "st6")])))/(0.7*pres_allhr$st6_vel)
#hist(pres_allhr$st6_RBS)
#ggplot(pres_allhr, aes(x = Pd, y = st6_RBS)) + geom_point(size = 2) + geom_hline(yintercept = 1)
length(which(pres_allhr$st6_RBS <= 1))
mean(pres_allhr$st6_RBS, na.rm = T)
pres_allhr$st8_RBS = (0.155*sqrt(33.8))/(0.7*pres_allhr$st8_vel)
#hist(pres_allhr$st8_RBS)
#ggplot(pres_allhr, aes(x = Pd, y = st8_RBS)) + geom_point(size = 2)+ geom_hline(yintercept = 1)
length(which(pres_allhr$st8_RBS < 1))
mean(pres_allhr$st8_RBS, na.rm = T)
pres_allhr$st9_RBS = (0.155*sqrt(median(sediment$Size[which(sediment$Stream == "st9")])))/(0.7*pres_allhr$st9_vel)
#hist(pres_allhr$st9_RBS)
#ggplot(pres_allhr, aes(x = Pd, y = st9_RBS)) + geom_point(size = 2)+ geom_hline(yintercept = 1)
length(which(pres_allhr$st11L_RBS <= 1))
mean(pres_allhr$st9_RBS, na.rm = T)
pres_allhr$st11L_RBS = (0.155*sqrt(median(sediment$Size[which(sediment$Stream == "st11L")])))/(0.7*pres_allhr$st11L_vel)
#hist(pres_allhr$st11L_RBS)
#ggplot(pres_allhr, aes(x = Pd, y = st11L_RBS)) + geom_point(size = 2)+ geom_hline(yintercept = 1)
length(which(pres_allhr$st11L_RBS <= 1))
mean(pres_allhr$st11L_RBS, na.rm = T)
pres_allhr$st11U_RBS = (0.155*sqrt(median(sediment$Size[which(sediment$Stream == "st11U")])))/(0.7*pres_allhr$st11U_vel)
#hist(pres_allhr$st11U_RBS)
#ggplot(pres_allhr, aes(x = Pd, y = st11U_RBS)) + geom_point(size = 2)+ geom_hline(yintercept = 1)
length(which(pres_allhr$st11U_RBS <= 1))
mean(pres_allhr$st11U_RBS, na.rm = T)
pres_allhr$st13_RBS = (0.155*sqrt(median(sediment$Size[which(sediment$Stream == "st13")])))/(0.7*pres_allhr$st13_vel)
#hist(pres_allhr$st13_RBS)
#ggplot(pres_allhr, aes(x = Pd, y = st13_RBS)) + geom_point(size = 2)+ geom_hline(yintercept = 1)
length(which(pres_allhr$st13_RBS <= 1))
mean(pres_allhr$st13_RBS, na.rm = T)
pres_allhr$st14_RBS = (0.155*sqrt(median(sediment$Size[which(sediment$Stream == "st14")])))/(0.7*pres_allhr$st14_vel)
#hist(pres_allhr$st14_RBS)
#ggplot(pres_allhr, aes(x = Pd, y = st14_RBS)) + geom_point(size = 2)+ geom_hline(yintercept = 1)
length(which(pres_allhr$st14_RBS <= 1))
mean(pres_allhr$st14_RBS, na.rm = T)
pres_allhr$st17_RBS = (0.155*sqrt(median(sediment$Size[which(sediment$Stream == "st17")])))/(0.7*pres_allhr$st17_vel)
#hist(pres_allhr$st17_RBS)
#ggplot(pres_allhr, aes(x = Pd, y = st17_RBS)) + geom_point(size = 2)+ geom_hline(yintercept = 1)
length(which(pres_allhr$st17_RBS <= 1))
mean(pres_allhr$st17_RBS, na.rm = T)
pres_allhr$Hver_RBS = (0.155*sqrt(11.6))/(0.7*pres_allhr$Hver_vel)
#hist(pres_allhr$Hver_RBS)
#ggplot(pres_allhr, aes(x = Pd, y = Hver_RBS)) + geom_point(size = 2)+ geom_hline(yintercept = 1)
length(which(pres_allhr$Hver_RBS <= 1))
mean(pres_allhr$Hver_RBS, na.rm = T)

####  
Q_allhr = data.frame(Pd = pres_allhr$Pd, pres_allhr[str_detect(names(pres_allhr), "_Q")])
temp_allhr = data.frame(Pd = pres_allhr$Pd, pres_allhr[str_detect(names(pres_allhr), "_tempC")])
depth_allhr = data.frame(Pd = pres_allhr$Pd, pres_allhr[str_detect(names(pres_allhr), "_depthe")])
width_allhr = data.frame(Pd = pres_allhr$Pd, pres_allhr[str_detect(names(pres_allhr), "_width")])
tforce_allhr = data.frame(Pd = pres_allhr$Pd, pres_allhr[str_detect(names(pres_allhr), "_tforce")])
RBS_allhr = data.frame(Pd = pres_allhr$Pd, pres_allhr[str_detect(names(pres_allhr), "_RBS")])

write.csv(Q_allhr, file = "./stream-data/Q_allhr.csv", row.names = F)
write.csv(temp_allhr, file = "./stream-data/temp_allhr.csv", row.names = F)
write.csv(depth_allhr, file = "./stream-data/depth_allhr.csv", row.names = F)
write.csv(width_allhr, file = "./stream-data/width_allhr.csv", row.names = F)
write.csv(tforce_allhr, file = "./stream-data/tforce_allhr.csv", row.names = F)
write.csv(RBS_allhr, file = "./stream-data/RBS_allhr.csv", row.names = F)

###

rm(list = ls()[!ls() %in% c("st_temps","Q_allhr","depth_allhr","RBS_allhr","sediment","tforce_allhr","temp_allhr")])
