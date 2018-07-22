#### Tractive forces analysis for each stream ####
source("./analysis-scripts/QDepth.R")

st_temps <- read.csv("./stream-data/stream_temps1.csv",T,stringsAsFactors = F)
st11L.fix = which(st_temps$Stream == "ST11D")
st_temps[st11L.fix, "Stream"] = "ST11L"
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
#units are kg/m^2
pres_allhr$st1_tforce = 10000*unique(st_temps$Slope[which(st_temps$Stream == "ST1")]/100)*pres_allhr$st1_Rh
pres_allhr$st5_tforce = 10000*unique(st_temps$Slope[which(st_temps$Stream == "ST5")]/100)*pres_allhr$st5_Rh
pres_allhr$st6_tforce = 10000*unique(st_temps$Slope[which(st_temps$Stream == "ST6")]/100)*pres_allhr$st6_Rh
pres_allhr$st8_tforce = 10000*unique(st_temps$Slope[which(st_temps$Stream == "ST8")]/100)*pres_allhr$st8_Rh
pres_allhr$st9_tforce = 10000*unique(st_temps$Slope[which(st_temps$Stream == "ST9")]/100)*pres_allhr$st9_Rh
pres_allhr$st11L_tforce = 10000*unique(st_temps$Slope[which(st_temps$Stream == "ST11L")]/100)*pres_allhr$st11L_Rh
pres_allhr$st11U_tforce = 10000*unique(st_temps$Slope[which(st_temps$Stream == "ST11U")]/100)*pres_allhr$st11U_Rh
pres_allhr$st13_tforce = 10000*unique(st_temps$Slope[which(st_temps$Stream == "ST13")]/100)*pres_allhr$st13_Rh
pres_allhr$st14_tforce = 10000*unique(st_temps$Slope[which(st_temps$Stream == "ST14")]/100)*pres_allhr$st14_Rh
pres_allhr$st17_tforce = 10000*unique(st_temps$Slope[which(st_temps$Stream == "ST17")]/100)*pres_allhr$st17_Rh
pres_allhr$Hver_tforce = 10000*unique(st_temps$Slope[which(st_temps$Stream == "Hver")]/100)*pres_allhr$Hver_Rh

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

  ##create long dataframe by subsetting the columns with certain data types
Q_allhr = data.frame(Pd = pres_allhr$Pd, pres_allhr[str_detect(names(pres_allhr), "_Q")])
Depth_allhr
width_allhr

#Tractive forces equation: pgRS; p = 1000 kg/m3, g = 9.81 m/s, R = hydraulic radius (m),
# S = gradient of the energy line (slope?? units??)


