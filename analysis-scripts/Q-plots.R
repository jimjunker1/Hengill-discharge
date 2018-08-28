#Q return interval plots
####  Function for plotting ecdf with line on ggplot  ####
f <-  function(u){
  which.min(abs(as.numeric(u$vals) - 0.5))
}
ecdf.plot <- function(DATA)
{
  DATA = DATA[!is.na(DATA)]
  x = sort(unique(DATA, na.rm = T))
  vals = cumsum(tabulate(match(DATA , unique(DATA))))/length(DATA)
  df = data.frame(x, vals)
  ggplot(df, aes(x = vals, y = log10(x))) + geom_point(size = 3, shape = 19, colour = "#999999") + geom_line(lwd = 1.2) +
    labs(x = "Cumulative Frequency", y = "log10(Q (L/s))") + scale_y_continuous(limits = c(NA,NA)) +
    geom_hline(yintercept = log10(as.numeric(df$x[f(df)]))) +
    annotate("text", -Inf, Inf, label = as.character(paste("Q50 (L/s)= ",round(df$x[f(df)],1), sep = "")), hjust = 0, vjust = 1)
}

#Q_st1 = ecdf.plot(pres_allhr[,'st1_Q']);Q_st1
#Q_st5 = ecdf.plot(pres_allhr[,'st5_Q']);Q_st5
#Q_st6 = ecdf.plot(pres_allhr[,'st6_Q']);Q_st6
#Q_st8 = ecdf.plot(pres_allhr[,'st8_Q']);Q_st8
#Q_st9 = ecdf.plot(pres_allhr[,'st9_Q']);Q_st9
#Q_st11L = ecdf.plot(pres_allhr[,'st11L_Q']);Q_st11L
#Q_st11U = ecdf.plot(pres_allhr[,'st11U_Q']);Q_st11U
#Q_st13 = ecdf.plot(pres_allhr[,'st13_Q']);Q_st13
#Q_st14 = ecdf.plot(pres_allhr[,'st14_Q']);Q_st14
#Q_st17 = ecdf.plot(pres_allhr[,'st17_Q']);Q_st17
#Q_hver = ecdf.plot(pres_allhr[,'Hver_Q']);Q_hver


#stream_list = list(sed_st14, sed_st13, sed_st17, sed_st1, sed_st5, sed_st6, sed_st9,
#                   sed_st11l, sed_st11u)

#png(file = "./stream-data/sediment-size-dist.png", res = 150, height = 15, width = 10, units = 'in')
#gridExtra::grid.arrange(sed_st14, sed_st13, sed_st17, sed_st1, sed_st5, sed_st6, sed_st9,
#                        sed_st11l, sed_st11u, ncol = 2)
#dev.off()
#pull in file from Nick Bond on Github to determine recurrence interval
#library(gh)
#gh('/repos/nickbond/hydrostats/contents/R/partial.series.R',.send_headers = c(Accept = "application/vnd.github.v3.raw"),.destfile = "./analysis-scripts/parial.series.R")

##trying to get recurrence intervals to estimate bankfull Q = 1.5 y return interval
source("./analysis-scripts/partial.series.R")
#debugonce(partial.series)
Q1 = pres_allhr[,c("Pd","st1_Q")]
Q1ps = partial.series(flow.ts = Q1, ari = 1.5, plot = F);Q1ps
Q1 %>% filter(st1_Q >= Q1ps[[4]])

Q5 = pres_allhr[,c("Pd","st5_Q")]
Q5ps = partial.series(flow.ts = Q5, ari = 1.5, plot = F);Q5ps
Q5 %>% filter(st5_Q > Q5ps[[4]])

Q6 = pres_allhr[,c("Pd","st6_Q")]
Q6ps = partial.series(flow.ts = Q6, ari = 1.5, plot = F);Q6ps

Q8 = pres_allhr[,c("Pd","st8_Q")]
Q8ps = partial.series(flow.ts = Q8, ari = 1.5, plot = F);Q8ps

Q9 = pres_allhr[,c("Pd","st9_Q")]
Q9ps = partial.series(flow.ts = Q9, ari = 1.5, plot = F);Q9ps

Q11L = pres_allhr[,c("Pd","st11L_Q")]
Q11Lps = partial.series(flow.ts = Q11L, ari = 1.5, plot = F);Q11Lps

Q11U = pres_allhr[,c("Pd","st11U_Q")]
Q11Ups = partial.series(flow.ts = Q11U, ari = 1.5, plot = F);Q11Ups

Q14 = pres_allhr[,c("Pd","st14_Q")]
Q14ps = partial.series(flow.ts = Q14, ari = 1.5, plot = F);Q14ps

Q17 = pres_allhr[,c("Pd","st17_Q")]
Q17ps = partial.series(flow.ts = Q17, ari = 1.5, plot = F);Q17ps

Qhver = pres_allhr[,c("Pd","Hver_Q")]
Qhverps = partial.series(flow.ts = Qhver, ari = 1.5, plot = F);Qhverps

streams = c('st1','st5','st6','st8','st9','st11L','st11U','st13','st14','st17','hver')
Qcrit = c(Q1ps[[4]],Q5ps[[4]],Q6ps[[4]],Q8ps[[4]],Q9ps[[4]],Q11Lps[[4]],Q11Ups[[4]],14.6,Q14ps[[4]],
          Q17ps[[4]],Qhverps[[4]])

Qcrit_BOM = data.frame(Stream = streams, Qcrit = Qcrit)

rm(list = ls()[!ls() %in% c("st_temps","Q_allhr","depth_allhr","RBS_allhr","sediment","tforce_allhr","temp_allhr", "ins_allhr", "vel_allhr","pres_allhr","Qcrit_BOM")])