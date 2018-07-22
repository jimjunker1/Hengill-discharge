#### Tractive forces analysis for each stream ####
source("./analysis-scripts/QDepth.R")

st_temps <- read.csv("./stream-data/stream_temps1.csv",T,stringsAsFactors = F)
st11L.fix = which(st_temps$Stream == "ST11D")
st_temps[st11L.fix, "Stream"] = "ST11L";rm(st11L.fix)
st_temps = data.frame(unclass(st_temps))
#calculate tractive forces for all streams at all dates
#need to estimate the hydraulic radius across all Q's and streams
# to calculate hydraulic radius (Depth[m]*width[m])/(2*Depth[m] + width[m])

pres_allhr$st1_Rh = (pres_allhr$st1_depthe*(pres_allhr$st1_width/100))/(2*pres_allhr$st1_depthe + (pres_allhr$st1_width/100))
ggplot(pres_allhr, aes(x = st1_depthe, y = st1_Rh)) + geom_point() + geom_abline(intercept = 0, slope = 1)
pres_allhr$st5_Rh = (pres_allhr$st5_depthe*(pres_allhr$st5_width/100))/(2*pres_allhr$st5_depthe + (pres_allhr$st5_width/100))
plot(pres_allhr$st5_depthe, pres_allhr$st5_Rh);abline(a=0,b=1)
pres_allhr$st6_Rh = (pres_allhr$st6_depthe*(pres_allhr$st6_width/100))/(2*pres_allhr$st6_depthe + (pres_allhr$st6_width/100))
plot(pres_allhr$st6_depthe, pres_allhr$st6_Rh);abline(a=0,b=1)
pres_allhr$st8_Rh = (pres_allhr$st8_depthe*(pres_allhr$st8_width/100))/(2*pres_allhr$st8_depthe + (pres_allhr$st8_width/100))
plot(pres_allhr$st8_depthe, pres_allhr$st8_Rh);abline(a=0,b=1)
pres_allhr$st9_Rh = (pres_allhr$st9_depthe*(pres_allhr$st9_width/100))/(2*pres_allhr$st9_depthe + (pres_allhr$st9_width/100))
plot(pres_allhr$st9_depthe, pres_allhr$st9_Rh);abline(a=0,b=1)
pres_allhr$st11L_Rh = (pres_allhr$st11L_depthe*(pres_allhr$st11L_width/100))/(2*pres_allhr$st11L_depthe + (pres_allhr$st11L_width/100))
plot(pres_allhr$st11L_depthe, pres_allhr$st11L_Rh);abline(a=0,b=1)
pres_allhr$st11U_Rh = (pres_allhr$st11U_depthe*(pres_allhr$st11U_width/100))/(2*pres_allhr$st11U_depthe + (pres_allhr$st11U_width/100))
plot(pres_allhr$st11U_depthe, pres_allhr$st11U_Rh);abline(a=0,b=1)
pres_allhr$st13_Rh = (pres_allhr$st13_depthe*(pres_allhr$st13_width/100))/(2*pres_allhr$st13_depthe + (pres_allhr$st13_width/100))
plot(pres_allhr$st13_depthe, pres_allhr$st13_Rh);abline(a=0,b=1)
pres_allhr$st14_Rh = (pres_allhr$st14_depthe*(pres_allhr$st14_width/100))/(2*pres_allhr$st14_depthe + (pres_allhr$st14_width/100))
plot(pres_allhr$st14_depthe, pres_allhr$st14_Rh);abline(a=0,b=1)
pres_allhr$st17_Rh = (pres_allhr$st17_depthe*(pres_allhr$st17_width/100))/(2*pres_allhr$st17_depthe + (pres_allhr$st17_width/100))
plot(pres_allhr$st17_depthe, pres_allhr$st17_Rh);abline(a=0,b=1)
pres_allhr$Hver_Rh = (pres_allhr$Hver_depthe*(pres_allhr$Hver_width/100))/(2*pres_allhr$Hver_depthe + (pres_allhr$Hver_width/100))
plot(pres_allhr$Hver_depthe, pres_allhr$Hver_Rh);abline(a=0,b=1)

#calculate the tractive forces on each stream using the Rh series
#units are N/m^2 --remove 9.807 to get to kg/m^2
pres_allhr$st1_tforce = 1000*9.807*unique(st_temps$Slope[which(st_temps$Stream == "ST1")]/100)*pres_allhr$st1_Rh
pres_allhr$st5_tforce = 1000*9.807*unique(st_temps$Slope[which(st_temps$Stream == "ST5")]/100)*pres_allhr$st5_Rh
pres_allhr$st6_tforce = 1000*9.807*(st_temps$Slope[which(st_temps$Stream == "ST6")]/100)*pres_allhr$st6_Rh
pres_allhr$st8_tforce = 1000*9.807*unique(st_temps$Slope[which(st_temps$Stream == "ST8")]/100)*pres_allhr$st8_Rh
pres_allhr$st9_tforce = 1000*9.807*unique(st_temps$Slope[which(st_temps$Stream == "ST9")]/100)*pres_allhr$st9_Rh
pres_allhr$st11L_tforce = 1000*9.807*unique(st_temps$Slope[which(st_temps$Stream == "ST11L")]/100)*pres_allhr$st11L_Rh
pres_allhr$st11U_tforce = 1000*9.807*unique(st_temps$Slope[which(st_temps$Stream == "ST11U")]/100)*pres_allhr$st11U_Rh
pres_allhr$st13_tforce = 1000*9.807*unique(st_temps$Slope[which(st_temps$Stream == "ST13")]/100)*pres_allhr$st13_Rh
pres_allhr$st14_tforce = 1000*9.807*unique(st_temps$Slope[which(st_temps$Stream == "ST14")]/100)*pres_allhr$st14_Rh
pres_allhr$st17_tforce = 1000*9.807*unique(st_temps$Slope[which(st_temps$Stream == "ST17")]/100)*pres_allhr$st17_Rh
pres_allhr$Hver_tforce = 1000*9.807*unique(st_temps$Slope[which(st_temps$Stream == "Hver")]/100)*pres_allhr$Hver_Rh

#critical force approximates the force to move sediment of x(mm): e.g. tforce of 16
#is necessary to move sediment of 16mm
hist(pres_allhr$st1_tforce)
ggplot(pres_allhr, aes(x = st1_tforce)) + geom_histogram(aes(y = ..density..))
hist(pres_allhr$st5_tforce)
hist(pres_allhr$st6_tforce)
hist(pres_allhr$st8_tforce)
hist(pres_allhr$st9_tforce)
hist(pres_allhr$st11L_tforce)
hist(pres_allhr$st11U_tforce)
hist(pres_allhr$st13_tforce)
hist(pres_allhr$st14_tforce)
hist(pres_allhr$st17_tforce)
hist(pres_allhr$Hver_tforce)

#### create average velocity measures ####
pres_allhr$st1_vel = pres_allhr$st1_length/pres_allhr$st1_tt.s
hist(pres_allhr$st1_vel);mean(pres_allhr$st1_vel, na.rm = T)
pres_allhr$st5_vel = pres_allhr$st5_length/pres_allhr$st5_tt.s
hist(pres_allhr$st5_vel);mean(pres_allhr$st5_vel, na.rm = T)
pres_allhr$st6_vel = pres_allhr$st6_length/pres_allhr$st6_tt.s
hist(pres_allhr$st6_vel);mean(pres_allhr$st6_vel, na.rm = T)
pres_allhr$st8_vel = pres_allhr$st8_length/pres_allhr$st8_tt.s
hist(pres_allhr$st8_vel);mean(pres_allhr$st8_vel, na.rm = T)
pres_allhr$st9_vel = pres_allhr$st9_length/pres_allhr$st9_tt.s
hist(pres_allhr$st9_vel);mean(pres_allhr$st9_vel, na.rm = T)
pres_allhr$st11L_vel = pres_allhr$st11L_length/pres_allhr$st11L_tt.s
hist(pres_allhr$st11L_vel);mean(pres_allhr$st11L_vel, na.rm = T)
pres_allhr$st11U_vel = pres_allhr$st11U_length/pres_allhr$st11U_tt.s
hist(pres_allhr$st11U_vel);mean(pres_allhr$st11U_vel, na.rm = T)
pres_allhr$st13_vel = pres_allhr$st13_length/pres_allhr$st13_tt.s
hist(pres_allhr$st13_vel);mean(pres_allhr$st13_vel, na.rm = T)
pres_allhr$st14_vel = pres_allhr$st14_length/pres_allhr$st14_tt.s
hist(pres_allhr$st14_vel);mean(pres_allhr$st14_vel, na.rm = T)
pres_allhr$st17_vel = pres_allhr$st17_length/pres_allhr$st17_tt.s
hist(pres_allhr$st17_vel);mean(pres_allhr$st17_vel, na.rm = T)
pres_allhr$Hver_vel = pres_allhr$Hver_length/pres_allhr$Hver_tt.s
hist(pres_allhr$Hver_vel);mean(pres_allhr$Hver_vel, na.rm = T)

write.csv(pres_allhr, file = "Q_trac_allhr.csv", row.names = F)

rm(list = ls()[!ls() %in% c("pres_allhr", "Q", "st_temps")])

#Tractive forces equation: pgRS; p = 1000 kg/m3, g = 9.81 m/s, R = hydraulic radius (m),
# S = gradient of the energy line (slope?? units??)


