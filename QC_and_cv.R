require(data.table)

input.folder <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/A_IDASw/A_incubation_filtered_360_corrected"
flux <- paste0(input.folder, "/A_filtered360_corrected.dat")
flux <- fread(flux)

data <- copy(flux)
incubation <- "A"
source("parameter_thresholds_asAincubation.R")


# flow
par(mfrow=c(2,5))
boxplot(data[, flow], main = "flow")
boxplot(data[, MEANflow], main = "MEANflow")

boxplot(data[, exhaust], main = "exhaust")

boxplot(data[, p_flow], main = "p_flow")
boxplot(data[, MEANp_flow], main = "MEANp_flow")




boxplot(data[, flowSD], main = "flowSD")
boxplot(data[, exhaustSD], main = "exhaustSD")


sd(data$flow)
sd(data$MEANflow)
sd(data$exhaust)
sd(data$p_flow)
sd(data$MEANp_flow)

summary(mean(data$flow) - data$flow)

summary(data$flow)
summary(data$MEANflow)
summary(data$exhaust)
summary(data$p_flow)
summary(data$MEANp_flow)

# QCL
par(mfrow=c(1,1))

boxplot(data[!is.na(CO2), rg_CO2_ppm], main = "rg_CO2_ppm")
boxplot(data[!is.na(CO2), sd_CO2_ppm], main = "sd_CO2_ppm")
boxplot(data[!is.na(CO2), 100*sd_CO2_ppm / CO2_ppm], main = "cv_CO2_ppm")
mean(data[!is.na(CO2), 100*sd_CO2_ppm / CO2_ppm]>0.15)

boxplot(data[!is.na(CH4), rg_CH4_ppb], main = "rg_CH4_ppb")
boxplot(data[!is.na(CH4), sd_CH4_ppb], main = "sd_CH4_ppb")
boxplot(data[!is.na(CH4), 100*sd_CH4_ppb / CH4_ppb], main = "cv_CH4_ppb")
mean(data[!is.na(CH4), 100*sd_CH4_ppb / CH4_ppb]>0.15)

boxplot(data[!is.na(N2O), rg_N2O_ppb], main = "rg_N2O_ppb")
boxplot(data[!is.na(N2O), sd_N2O_ppb], main = "sd_N2O_ppb")
boxplot(data[!is.na(N2O), 100*sd_N2O_ppb / N2O_ppb], main = "cv_N2O_ppb")
mean(data[!is.na(N2O), 100*sd_N2O_ppb / N2O_ppb]>0.15)

################################################################################
#QCL cal2 cv
input.folder <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/A_IDASw"
meas <- paste0(input.folder, "/A_incubation_full_60_corrected/A_full60_corrected_stdev.dat")
meas.val <- paste0(input.folder, "/A_incubation_full_60_corrected/A_full60_corrected_value.dat")
meas.rg <- paste0(input.folder, "/A_incubation_full_60_corrected/A_full60_corrected_max.dat")


meas <- fread(meas)
meas <- meas[bin_WORK == "0", ]
meas <- meas[epoch_time%%360==0,]

meas.val <- fread(meas.val)
meas.val <- meas.val[bin_WORK == "0", ]
meas.val <- meas.val[epoch_time%%360==0,]

meas.rg <- fread(meas.rg)
meas.rg <- meas.rg[bin_WORK == "0", ]
meas.rg <- meas.rg[epoch_time%%360==0,]

my360 <- copy(meas.val)


my360[, sd_N2O_ppb:= meas$sd_N2O_ppb]
my360[, sd_CO2_ppm:= meas$sd_CO2_ppm]
my360[, sd_CH4_ppb:= meas$sd_CH4_ppb]
my360[, sd_H2O_ppm:= meas$sd_H2O_ppm]

my360[, rg_N2O_ppb:= meas.rg$rg_N2O_ppb]
my360[, rg_CO2_ppm:= meas.rg$rg_CO2_ppm]
my360[, rg_CH4_ppb:= meas.rg$rg_CH4_ppb]
my360[, rg_H2O_ppm:= meas.rg$rg_H2O_ppm]

my360[, cv.N2O:= 100 * sd_N2O_ppb/N2O_ppb]
my360[, cv.CO2:= 100 * sd_CO2_ppm/CO2_ppm]
my360[, cv.CH4:= 100 * sd_CH4_ppb/CH4_ppb]
my360[, cv.H2O:= 100 * sd_H2O_ppm/H2O_ppm]

my360[, crg.N2O:= 100 * rg_N2O_ppb/N2O_ppb]
my360[, crg.CO2:= 100 * rg_CO2_ppm/CO2_ppm]
my360[, crg.CH4:= 100 * rg_CH4_ppb/CH4_ppb]
my360[, crg.H2O:= 100 * rg_H2O_ppm/H2O_ppm]

# get cal2 values (the problem is that ch4 is really high, 4000ppb, so sd is high)
cal2 <- copy(my360)
cal2 <- cal2[bin_INC2 == "1-0-00-0000-0000" 
             & bin_INC_valve == 1 
             & bin_WORK == "0"
             ,]
# 
# cal2[, cv.N2O:= 100 * sd_N2O_ppb/N2O_ppb]
# cal2[, cv.CO2:= 100 * sd_CO2_ppm/CO2_ppm]
# cal2[, cv.CH4:= 100 * sd_CH4_ppb/CH4_ppb]
# cal2[, cv.H2O:= 100 * sd_H2O_ppm/H2O_ppm]


boxplot(cal2[, cv.N2O], main = "cal2 cv.N2O")
boxplot(cal2[, cv.CO2], main = "cal2 cv.CO2")
boxplot(cal2[, cv.CH4], main = "cal2 cv.CH4")
boxplot(cal2[, cv.H2O], main = "cal2 cv.H2O")

boxplot(cal2[cv.N2O <0.3, cv.N2O], main = "cal2 cv.N2O")
boxplot(cal2[cv.N2O <0.3, cv.CO2], main = "cal2 cv.CO2")
boxplot(cal2[cv.N2O <0.3, cv.CH4], main = "cal2 cv.CH4")
boxplot(cal2[cv.N2O <0.3, cv.H2O], main = "cal2 cv.H2O")

boxplot(cal2[, cv.N2O], main = "cal2 cv.N2O", ylim=c(0,0.5))
boxplot(cal2[, cv.CO2], main = "cal2 cv.CO2", ylim=c(0,0.5))
boxplot(cal2[, cv.CH4], main = "cal2 cv.CH4", ylim=c(0,0.5))
boxplot(cal2[, cv.H2O], main = "cal2 cv.H2O", ylim=c(0,0.5))

boxplot(cal2[, crg.N2O], main = "cal2 crg.N2O", ylim=c(0,1))
boxplot(cal2[, crg.CO2], main = "cal2 crg.CO2", ylim=c(0,1))
boxplot(cal2[, crg.CH4], main = "cal2 crg.CH4", ylim=c(0,1))
boxplot(cal2[, crg.H2O], main = "cal2 crg.H2O", ylim=c(0,1))

#QCL AIR cv


# get 360 AIR values
air <- my360[bin_INC1 == "0-1-0-0000-0000"| bin_INC2 == "0-1-00-0000-0000", ]

air <- my360[bin_INC1 == "0-1-0-0000-0000" & bin_INC_valve == 10 |
                     bin_INC2 == "0-1-00-0000-0000" & bin_INC_valve == 1, ]

# get 360 AIR values
myfluxes <- sort(unique(c(cal2$epoch_time, air$epoch_time)))
flux <- my360[! epoch_time %in% myfluxes,]

boxplot(air[, flow], main = "flow")
par(mfrow=c(1,4))

boxplot(air[cv.N2O <1, cv.N2O], main = "air cv.N2O")
boxplot(air[cv.N2O <1, cv.CO2], main = "air cv.CO2")
boxplot(air[cv.N2O <1, cv.CH4], main = "air cv.CH4")
boxplot(air[cv.N2O <1, cv.H2O], main = "air cv.H2O")

boxplot(air[, cv.N2O], main = "air cv.N2O", ylim=c(0,0.5))
boxplot(air[, cv.CO2], main = "air cv.CO2", ylim=c(0,0.5))
boxplot(air[, cv.CH4], main = "air cv.CH4", ylim=c(0,0.5))
boxplot(air[, cv.H2O], main = "air cv.H2O", ylim=c(0,0.5))

qnorm(0.9995)/(qnorm(0.75)-qnorm(0.5))
qnorm(0.99975)/(qnorm(0.75)-qnorm(0.5))

par(mfrow=c(1,3))
boxplot(cal2[, cv.N2O], main = "air cv.N2O", ylim=c(0,0.5), range = 2.44)
boxplot(cal2[, cv.CO2], main = "air cv.CO2", ylim=c(0,0.5), range = 2.44)
boxplot(cal2[, cv.CH4], main = "air cv.CH4", ylim=c(0,0.5), range = 2.44)

boxplot(air[, cv.N2O], main = "air cv.N2O", ylim=c(0,0.5), range = 2.44)
boxplot(air[, cv.CO2], main = "air cv.CO2", ylim=c(0,0.5), range = 2.44)
boxplot(air[, cv.CH4], main = "air cv.CH4", ylim=c(0,0.5), range = 2.44)

median(air[, cv.N2O], na.rm=T) + 5.160577*( quantile(air[, cv.N2O], probs=0.75, na.rm=T)[[1]] - quantile(air[, cv.N2O], probs=0.25, na.rm=T)[[1]] )
median(air[, cv.CO2], na.rm=T) + 5.160577*( quantile(air[, cv.CO2], probs=0.75, na.rm=T)[[1]] - quantile(air[, cv.CO2], probs=0.25, na.rm=T)[[1]] )
median(air[, cv.CH4], na.rm=T) + 5.160577*( quantile(air[, cv.CH4], probs=0.75, na.rm=T)[[1]] - quantile(air[, cv.CH4], probs=0.25, na.rm=T)[[1]] )

mytime2nd <- 1398882240 + (1403253720 - 1398882240)/3
median(air[epoch_time>mytime2nd, cv.N2O], na.rm=T) + 5.160577*( quantile(air[epoch_time>mytime2nd, cv.N2O], probs=0.75, na.rm=T)[[1]] - quantile(air[epoch_time>mytime2nd, cv.N2O], probs=0.25, na.rm=T)[[1]] )
median(air[epoch_time>mytime2nd, cv.CO2], na.rm=T) + 5.160577*( quantile(air[epoch_time>mytime2nd, cv.CO2], probs=0.75, na.rm=T)[[1]] - quantile(air[epoch_time>mytime2nd, cv.CO2], probs=0.25, na.rm=T)[[1]] )
median(air[epoch_time>mytime2nd, cv.CH4], na.rm=T) + 5.160577*( quantile(air[epoch_time>mytime2nd, cv.CH4], probs=0.75, na.rm=T)[[1]] - quantile(air[epoch_time>mytime2nd, cv.CH4], probs=0.25, na.rm=T)[[1]] )

boxplot(air[, cv.N2O], main = "air cv.N2O", ylim=c(0,0.1), range = 2.439271)
boxplot(air[, cv.CO2], main = "air cv.CO2", ylim=c(0,0.1), range = 2.439271)
boxplot(air[, cv.CH4], main = "air cv.CH4", ylim=c(0,0.1), range = 2.439271)

boxplot(air[, cv.N2O], main = "air cv.N2O")
boxplot(air[, cv.CO2], main = "air cv.CO2")
boxplot(air[, cv.CH4], main = "air cv.CH4")
boxplot(air[, cv.H2O], main = "air cv.H2O")

boxplot(air[, crg.N2O], main = "air crg.N2O", ylim=c(0,1))
boxplot(air[, crg.CO2], main = "air crg.CO2", ylim=c(0,1))
boxplot(air[, crg.CH4], main = "air crg.CH4", ylim=c(0,1))
boxplot(air[, crg.H2O], main = "air crg.H2O", ylim=c(0,1))

with(air, plot(N2O_ppb~cv.N2O))
par(mfrow=c(2,4))

with(air, plot(N2O_ppb~cv.N2O, xlim= c(0, 0.5)))
with(air, plot(CO2_ppm~cv.CO2, xlim= c(0, 0.5)))
with(air, plot(CH4_ppb~cv.CH4, xlim= c(0, 0.5)))
with(air, plot(H2O_ppm~cv.H2O, xlim= c(0, 2)))

with(flux, plot(N2O_ppb~cv.N2O, xlim= c(0, 0.5)))
with(flux, plot(CO2_ppm~cv.CO2, xlim= c(0, 0.5)))
with(flux, plot(CH4_ppb~cv.CH4, xlim= c(0, 0.5)))
with(flux, plot(H2O_ppm~cv.H2O, xlim= c(0, 2)))

with(cal2, plot(N2O_ppb~cv.N2O, xlim= c(0, 0.5)))
with(cal2, plot(CO2_ppm~cv.CO2, xlim= c(0, 0.5)))
with(cal2, plot(CH4_ppb~cv.CH4, xlim= c(0, 0.5)))
with(cal2, plot(H2O_ppm~cv.H2O, xlim= c(0, 2)))

with(air[epoch_time>mytime2nd,], plot(N2O_ppb~cv.N2O, xlim= c(0, 0.5)))
with(air[epoch_time>mytime2nd,], plot(CO2_ppm~cv.CO2, xlim= c(0, 0.5)))
with(air[epoch_time>mytime2nd,], plot(CH4_ppb~cv.CH4, xlim= c(0, 0.5)))
with(air[epoch_time>mytime2nd,], plot(H2O_ppm~cv.H2O, xlim= c(0, 2)))

with(flux[epoch_time>mytime2nd,], plot(N2O_ppb~cv.N2O, xlim= c(0, 0.5)))
with(flux[epoch_time>mytime2nd,], plot(CO2_ppm~cv.CO2, xlim= c(0, 0.5)))
with(flux[epoch_time>mytime2nd,], plot(CH4_ppb~cv.CH4, xlim= c(0, 0.5)))
with(flux[epoch_time>mytime2nd,], plot(H2O_ppm~cv.H2O, xlim= c(0, 2)))

with(cal2[epoch_time>mytime2nd,], plot(N2O_ppb~cv.N2O, xlim= c(0, 0.5)))
with(cal2[epoch_time>mytime2nd,], plot(CO2_ppm~cv.CO2, xlim= c(0, 0.5)))
with(cal2[epoch_time>mytime2nd,], plot(CH4_ppb~cv.CH4, xlim= c(0, 0.5)))
with(cal2[epoch_time>mytime2nd,], plot(H2O_ppm~cv.H2O, xlim= c(0, 2)))



mean(air$cv.N2O >0.3, na.rm=T)
mean(my360$cv.N2O >0.3, na.rm=T)
mean(flux$cv.N2O >0.3, na.rm=T)

mean(air$cv.CO2 >0.3, na.rm=T)
mean(my360$cv.CO2 >0.3, na.rm=T)
mean(flux$cv.CO2 >0.3, na.rm=T)

mean(air$cv.CH4 >0.3, na.rm=T)
mean(my360$cv.CH4 >0.3, na.rm=T)
mean(flux$cv.CH4 >0.3, na.rm=T)


quantile(cal2$cv.N2O, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(air$cv.N2O, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(flux$cv.N2O, na.rm=T, prob=c(.75, .9, .95, .99))

quantile(cal2$cv.CO2, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(air$cv.CO2, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(flux$cv.CO2, na.rm=T, prob=c(.75, .9, .95, .99))

quantile(cal2$cv.CH4, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(air$cv.CH4, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(flux$cv.CH4, na.rm=T, prob=c(.75, .9, .95, .99))


mean(air$cv.N2O >0.1, na.rm=T)
mean(flux$cv.N2O >0.1, na.rm=T)

mean(air$cv.CO2 >0.1, na.rm=T)
mean(flux$cv.CO2 >0.1, na.rm=T)

mean(air$cv.CH4 >0.1, na.rm=T)
mean(flux$cv.CH4 >0.1, na.rm=T)

quantile(cal2$crg.N2O, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(air$crg.N2O, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(flux$crg.N2O, na.rm=T, prob=c(.75, .9, .95, .99))

quantile(cal2$crg.CO2, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(air$crg.CO2, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(flux$crg.CO2, na.rm=T, prob=c(.75, .9, .95, .99))

quantile(cal2$crg.CH4, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(air$crg.CH4, na.rm=T, prob=c(.75, .9, .95, .99))
quantile(flux$crg.CH4, na.rm=T, prob=c(.75, .9, .95, .99))

mean(air[epoch_time>mytime2nd,cv.N2O] >0.106, na.rm=T)
mean(flux[epoch_time>mytime2nd,cv.N2O] >0.106, na.rm=T)

mean(air[epoch_time>mytime2nd,cv.CO2] >0.114, na.rm=T)
mean(flux[epoch_time>mytime2nd,cv.CO2] >0.114, na.rm=T)

mean(air[epoch_time>mytime2nd,cv.CH4] >0.133, na.rm=T)
mean(flux[epoch_time>mytime2nd,cv.CH4] >0.133, na.rm=T)


mean(air[,cv.N2O] >0.106, na.rm=T)
mean(flux[,cv.N2O] >0.106, na.rm=T)

mean(air[,cv.CO2] >0.114, na.rm=T)
mean(flux[,cv.CO2] >0.114, na.rm=T)

mean(air[,cv.CH4] >0.133, na.rm=T)
mean(flux[,cv.CH4] >0.133, na.rm=T)
# 
mean(air[,cv.N2O] >0.089, na.rm=T)
mean(flux[,cv.N2O] >0.089, na.rm=T)

mean(air[,cv.CO2] >0.098, na.rm=T)
mean(flux[,cv.CO2] >0.098, na.rm=T)

mean(air[,cv.CH4] >0.133, na.rm=T)
mean(flux[,cv.CH4] >0.133, na.rm=T)




mean(cal2$crg.N2O >0.3950182, na.rm=T)
mean(air$crg.N2O >0.3950182, na.rm=T)
mean(flux$crg.N2O >0.3950182, na.rm=T)

mean(air$crg.CO2 >0.4244916, na.rm=T)
mean(flux$crg.CO2 >0.4244916, na.rm=T)

mean(air$crg.CH4 >0.2681504, na.rm=T)
mean(flux$crg.CH4 >0.2681504, na.rm=T)


# rg
par(mfrow=c(2,4))
with(air, plot(N2O_ppb~crg.N2O))
with(air, plot(CO2_ppm~crg.CO2))
with(air, plot(CH4_ppb~crg.CH4))
with(air, plot(H2O_ppm~crg.H2O))

with(my360, plot(N2O_ppb~crg.N2O))
with(my360, plot(CO2_ppm~crg.CO2))
with(my360, plot(CH4_ppb~crg.CH4))
with(my360, plot(H2O_ppm~crg.H2O))



with(air, plot(N2O_ppb~crg.N2O, xlim= c(0, 5)))
with(air, plot(CO2_ppm~crg.CO2, xlim= c(0, 5)))
with(air, plot(CH4_ppb~crg.CH4, xlim= c(0, 5)))
with(air, plot(H2O_ppm~crg.H2O, xlim= c(0, 20)))

with(my360, plot(N2O_ppb~crg.N2O, xlim= c(0, 5)))
with(my360, plot(CO2_ppm~crg.CO2, xlim= c(0, 5)))
with(my360, plot(CH4_ppb~crg.CH4, xlim= c(0, 5)))
with(my360, plot(H2O_ppm~crg.H2O, xlim= c(0, 20)))

with(air, plot(N2O_ppb~crg.N2O, xlim= c(0, 1)))
with(air, plot(CO2_ppm~crg.CO2, xlim= c(0, 1)))
with(air, plot(CH4_ppb~crg.CH4, xlim= c(0, 1)))
with(air, plot(H2O_ppm~crg.H2O, xlim= c(0, 20)))

with(flux, plot(N2O_ppb~crg.N2O, xlim= c(0, 1)))
with(flux, plot(CO2_ppm~crg.CO2, xlim= c(0, 1)))
with(flux, plot(CH4_ppb~crg.CH4, xlim= c(0, 1)))
with(flux, plot(H2O_ppm~crg.H2O, xlim= c(0, 20)))

mean(air$crg.N2O >0.3, na.rm=T)
mean(my360$crg.N2O >0.3, na.rm=T)

mean(air$crg.CO2 >0.3, na.rm=T)
mean(my360$crg.CO2 >0.3, na.rm=T)

mean(air$crg.CH4 >0.3, na.rm=T)
mean(my360$crg.CH4 >0.3, na.rm=T)



mean(air$crg.N2O >0.5, na.rm=T)
mean(my360$crg.N2O >0.5, na.rm=T)

mean(air$crg.CO2 >0.5, na.rm=T)
mean(my360$crg.CO2 >0.5, na.rm=T)

mean(air$crg.CH4 >0.5, na.rm=T)
mean(my360$crg.CH4 >0.5, na.rm=T)

par(mfrow=c(1,3))
with(air, plot(rg_N2O_ppb~epoch_time, ylim = c(0, 20)))
with(air, plot(rg_CO2_ppm~epoch_time, ylim = c(0, 20)))
with(air, plot(rg_CH4_ppb~epoch_time, ylim = c(0, 20)))

with(air, plot(cv.N2O~epoch_time, ylim = c(0, 1)))
with(air, plot(cv.CO2~epoch_time, ylim = c(0, 1)))
with(air, plot(cv.CH4~epoch_time, ylim = c(0, 1)))

mean(air$crg.N2O, na.rm=T)
mean(my360$crg.N2O , na.rm=T)
mean(flux$crg.N2O , na.rm=T)
crgN2O <- mean(air$crg.N2O, na.rm=T)
mean(flux$crg.N2O > 3.29*crgN2O, na.rm=T)
3.29*crgN2O

mean(air$crg.CO2 , na.rm=T)
mean(my360$crg.CO2 , na.rm=T)
crgCO2 <- mean(air$crg.CO2, na.rm=T)
mean(flux$crg.CO2 > 3.29*crgCO2, na.rm=T)
3.29*crgCO2

mean(air$crg.CH4 , na.rm=T)
mean(my360$crg.CH4 , na.rm=T)
crgCH4 <- mean(air$crg.CH4, na.rm=T)
mean(flux$crg.CH4 > 3.29*crgCH4, na.rm=T)
3.29*crgCH4


mean(air$cv.N2O, na.rm=T)
mean(my360$cv.N2O , na.rm=T)
mean(flux$cv.N2O , na.rm=T)
cvN2O <- mean(air$cv.N2O, na.rm=T)
mean(flux$cv.N2O > 3.29*cvN2O, na.rm=T)
3.29*cvN2O

mean(air$cv.CO2 , na.rm=T)
mean(my360$cv.CO2 , na.rm=T)
cvCO2 <- mean(air$cv.CO2, na.rm=T)
mean(flux$cv.CO2 > 3.29*cvCO2, na.rm=T)
3.29*cvCO2

mean(air$cv.CH4 , na.rm=T)
mean(my360$cv.CH4 , na.rm=T)
cvCH4 <- mean(air$cv.CH4, na.rm=T)
mean(flux$cv.CH4 > 3.29*cvCH4, na.rm=T)
3.29*cvCH4



mean(flux$cv.N2O > 3.29*cvN2O | flux$crg.N2O > 3.29*crgN2O, na.rm=T)
mean(flux$cv.CO2 > 3.29*cvCO2 | flux$crg.CO2 > 3.29*crgCO2, na.rm=T)
mean(flux$cv.CH4 > 3.29*cvCH4 | flux$crg.CH4 > 3.29*crgCH4, na.rm=T)

mean(flux$cv.N2O > 3.29*cvN2O | flux$crg.N2O > 3.29*crgN2O |
     flux$cv.CO2 > 3.29*cvCO2 | flux$crg.CO2 > 3.29*crgCO2 |
     flux$cv.CH4 > 3.29*cvCH4 | flux$crg.CH4 > 3.29*crgCH4
     , na.rm=T)

mean(flux$cv.N2O > 0.1 | flux$crg.N2O > 3.29*crgN2O |
             flux$cv.CO2 > 3.29*cvCO2 | flux$crg.CO2 > 3.29*crgCO2 |
             flux$cv.CH4 > 3.29*cvCH4 | flux$crg.CH4 > 3.29*crgCH4
     , na.rm=T)


crgN2O <- mean(air$crg.N2O, na.rm=T)
crgN2O.sd <- sd(air$crg.N2O, na.rm=T)
crgN2O + 3.29*crgN2O.sd

mytime <- 1403253720- (1403253720 - 1398882240)/3

crgN2O_last3rd <- mean(air[epoch_time>mytime,crg.N2O], na.rm=T)
crgN2O.sd_last3rd <- sd(air[epoch_time>mytime,crg.N2O], na.rm=T)
crgN2O_last3rd + 3.29*crgN2O.sd_last3rd
