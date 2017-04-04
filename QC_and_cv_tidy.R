require(data.table)
myplots <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/_all_fluxes/thresholds"

input.folder <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/A_IDASw/A_incubation_filtered_360_corrected"
flux <- paste0(input.folder, "/A_filtered360_corrected.dat")
flux <- fread(flux)

data <- copy(flux)
incubation <- "A"
source("parameter_thresholds_asAincubation.R")

################################################################################
### flow
myplot <- paste0(myplots, "/A_incubation_flow_parameters.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(2,5))
boxplot(data[, flow], main = "flow")
boxplot(data[, MEANflow], main = "MEANflow")

boxplot(data[, exhaust], main = "exhaust")

boxplot(data[, p_flow], main = "p_flow")
boxplot(data[, MEANp_flow], main = "MEANp_flow")



boxplot(data[, flowSD], main = "flowSD")
boxplot(data[, exhaustSD], main = "exhaustSD")
dev.off()


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

################################################################################
################################################################################

################################################################################
### QCL (my360; measured 360 s values)
input.folder <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/A_IDASw"
meas <- paste0(input.folder, "/A_incubation_full_60_corrected/A_full60_corrected_stdev.dat")
meas.val <- paste0(input.folder, "/A_incubation_full_60_corrected/A_full60_corrected_value.dat")
meas.rg <- paste0(input.folder, "/A_incubation_full_60_corrected/A_full60_corrected_max.dat")
meas.min <- paste0(input.folder, "/A_incubation_full_60_corrected/A_full60_corrected_min.dat")

meas <- fread(meas)
meas <- meas[bin_WORK == "0", ]
meas <- meas[epoch_time%%360==0,]

meas.val <- fread(meas.val)
meas.val <- meas.val[bin_WORK == "0", ]
meas.val <- meas.val[epoch_time%%360==0,]

meas.rg <- fread(meas.rg)
meas.rg <- meas.rg[bin_WORK == "0", ]
meas.rg <- meas.rg[epoch_time%%360==0,]

meas.min <- fread(meas.min)
meas.min <- meas.min[bin_WORK == "0", ]
meas.min <- meas.min[epoch_time%%360==0,]

# 
my360 <- copy(meas.val)

my360[, flowSD:= meas$flow]
my360[, flowMIN:= meas.min$flow]
my360[, flowMAX:= meas.rg$flow]
my360[, exhaustSD:= meas$exhaust]
my360[, exhaustMIN:= meas.min$exhaust]
my360[, exhaustMAX:= meas.rg$exhaust]

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

### cal2
# get cal2 values (the problem is that ch4 is really high, 4000ppb, so sd is high)
cal2 <- copy(my360)
cal2 <- cal2[bin_INC2 == "1-0-00-0000-0000" 
             & bin_INC_valve == 1 
             & bin_WORK == "0"
             ,]

### AIR (tank air)
data <- my360
source("parameter_thresholds_asAincubation_justFlow.R")
my360<- copy(data)

air <- my360[bin_INC1 == "0-1-0-0000-0000"| bin_INC2 == "0-1-00-0000-0000", ]

air <- my360[bin_INC1 == "0-1-0-0000-0000" & bin_INC_valve == 10 |
                     bin_INC2 == "0-1-00-0000-0000" & bin_INC_valve == 1, ]

### CORE concetrations
myfluxes <- sort(unique(c(cal2$epoch_time, air$epoch_time)))
flux <- my360[! epoch_time %in% myfluxes,]

################################################################################
### cv (sd/mean)
qnorm(0.99975)/(qnorm(0.75)-qnorm(0.5))
myrange <- qnorm(0.99975)/(qnorm(0.75)-qnorm(0.25))

# have a lokk to QCL performance; consider cal2, air and fluxes
myplot <- paste0(myplots, "/A_incubation_cv_timeseries_N2O.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(cv.N2O ~epoch_time, ylim=c(0,0.5), main= "cal2 (N2O cv)"))
with(air, plot(cv.N2O ~epoch_time, ylim=c(0,0.5), main= "air (N2O cv)"))
with(flux, plot(cv.N2O ~epoch_time, ylim=c(0,0.5), main= "flux (N2O cv)"))

dev.off()
# 

myplot <- paste0(myplots, "/A_incubation_cv_timeseries_CO2.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(cv.CO2 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CO2 cv)"))
with(air, plot(cv.CO2 ~epoch_time, ylim=c(0,0.5), main= "air (CO2 cv)"))
with(flux, plot(cv.CO2 ~epoch_time, ylim=c(0,0.5), main= "flux (CO2 cv)"))

dev.off()

# 
myplot <- paste0(myplots, "/A_incubation_cv_timeseries_CH4.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(cv.CH4 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CH4 cv)"))
with(air, plot(cv.CH4 ~epoch_time, ylim=c(0,0.5), main= "air (CH4 cv)"))
with(flux, plot(cv.CH4 ~epoch_time, ylim=c(0,0.5), main= "flux (CH4 cv)"))

dev.off()

# we take just the last part were QCL performs well for cal2, air
mytime2nd <- 1398882240 + (1403253720 - 1398882240)/3
mytime2nd <- 1400500000

raw.cal2 <- copy(cal2)
raw.air <- copy(air)
raw.flux <- copy(flux)

cal2 <- cal2[epoch_time > mytime2nd]
air <- air[epoch_time > mytime2nd]
flux <- flux[epoch_time > mytime2nd]

# check boxplots
myplot <- paste0(myplots, "/A_incubation_cv_boxplots_testTime.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(2,4))

boxplot(cal2[, cv.N2O], main = "cal2 cv.N2O", ylim=c(0,0.5), range= myrange)
boxplot(cal2[, cv.CO2], main = "cal2 cv.CO2", ylim=c(0,0.5), range= myrange)
boxplot(cal2[, cv.CH4], main = "cal2 cv.CH4", ylim=c(0,0.5), range= myrange)
boxplot(cal2[, cv.H2O], main = "cal2 cv.H2O", ylim=c(0,5), range= myrange)

boxplot(air[, cv.N2O], main = "air cv.N2O", ylim=c(0,0.5), range= myrange)
boxplot(air[, cv.CO2], main = "air cv.CO2", ylim=c(0,0.5), range= myrange)
boxplot(air[, cv.CH4], main = "air cv.CH4", ylim=c(0,0.5), range= myrange)
boxplot(air[, cv.H2O], main = "air cv.H2O", ylim=c(0,5), range= myrange)

dev.off()
# get threshold values
thr.N2O.cv <- median(air[, cv.N2O], na.rm=T) +
        5.160577*(
                quantile(air[, cv.N2O], probs=0.75, na.rm=T)[[1]]
                - quantile(air[, cv.N2O], probs=0.25, na.rm=T)[[1]] 
        )
thr.CO2.cv <- median(air[, cv.CO2], na.rm=T) +
        5.160577*(
                quantile(air[, cv.CO2], probs=0.75, na.rm=T)[[1]]
                - quantile(air[, cv.CO2], probs=0.25, na.rm=T)[[1]] 
        )
thr.CH4.cv <- median(air[, cv.CH4], na.rm=T) +
        5.160577*(
                quantile(air[, cv.CH4], probs=0.75, na.rm=T)[[1]] 
                - quantile(air[, cv.CH4], probs=0.25, na.rm=T)[[1]] 
        )
thr.N2O.cv
thr.CO2.cv
thr.CH4.cv


# plot it
myplot <- paste0(myplots, "/A_incubation_cv_vs_CONC__air_flux.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(2,3))

with(air, plot(N2O_ppb~cv.N2O, main= "air (cv)", xlim= c(0, 0.5))); abline(v=thr.N2O.cv, col="red")
with(air, plot(CO2_ppm~cv.CO2, main= "air (cv)", xlim= c(0, 0.5))); abline(v=thr.CO2.cv, col="red")
with(air, plot(CH4_ppb~cv.CH4, main= "air (cv)", xlim= c(0, 0.5))); abline(v=thr.CH4.cv, col="red")

with(flux, plot(N2O_ppb~cv.N2O, main= "flux (cv)", xlim= c(0, 0.5))); abline(v=thr.N2O.cv, col="red")
with(flux, plot(CO2_ppm~cv.CO2, main= "flux (cv)", xlim= c(0, 0.5))); abline(v=thr.CO2.cv, col="red")
with(flux, plot(CH4_ppb~cv.CH4, main= "flux (cv)", xlim= c(0, 0.5))); abline(v=thr.CH4.cv, col="red")

dev.off()
# 
myplot <- paste0(myplots, "/A_incubation_cv_vs_CONC__cal2_air.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(2,3))

with(cal2, plot(N2O_ppb~cv.N2O, main= "cal2 (cv)", xlim= c(0, 0.5))); abline(v=thr.N2O.cv, col="red")
with(cal2, plot(CO2_ppm~cv.CO2, main= "cal2 (cv)", xlim= c(0, 0.5))); abline(v=thr.CO2.cv, col="red")
with(cal2, plot(CH4_ppb~cv.CH4, main= "cal2 (cv)", xlim= c(0, 0.5))); abline(v=thr.CH4.cv, col="red")

with(air, plot(N2O_ppb~cv.N2O, main= "air (cv)", xlim= c(0, 0.5))); abline(v=thr.N2O.cv, col="red")
with(air, plot(CO2_ppm~cv.CO2, main= "air (cv)", xlim= c(0, 0.5))); abline(v=thr.CO2.cv, col="red")
with(air, plot(CH4_ppb~cv.CH4, main= "air (cv)", xlim= c(0, 0.5))); abline(v=thr.CH4.cv, col="red")

dev.off()
# check deleted values in test 
mean(cal2$cv.N2O >thr.N2O.cv, na.rm=T)
mean(air$cv.N2O >thr.N2O.cv, na.rm=T)
mean(flux$cv.N2O >thr.N2O.cv, na.rm=T)
mean(flux$cv.N2O >thr.N2O.cv, na.rm=T) / mean(air$cv.N2O >thr.N2O.cv, na.rm=T)

mean(cal2$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(air$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(flux$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(flux$cv.CO2 >thr.CO2.cv, na.rm=T) / mean(air$cv.CO2 >thr.CO2.cv, na.rm=T)

mean(cal2$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(air$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(flux$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(flux$cv.CH4 >thr.CH4.cv, na.rm=T) / mean(air$cv.CH4 >thr.CH4.cv, na.rm=T)


################################################################################
### crg (rg/mean)

# have a lokk to QCL performance; consider cal2, air and fluxes
myplot <- paste0(myplots, "/A_incubation_crg_timeseries_N2O.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(crg.N2O ~epoch_time, ylim=c(0,0.5), main= "cal2 (N2O crg)"))
with(air, plot(crg.N2O ~epoch_time, ylim=c(0,0.5), main= "air (N2O crg)"))
with(flux, plot(crg.N2O ~epoch_time, ylim=c(0,0.5), main= "flux (N2O crg)"))

dev.off()
# 

myplot <- paste0(myplots, "/A_incubation_crg_timeseries_CO2.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(crg.CO2 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CO2 crg)"))
with(air, plot(crg.CO2 ~epoch_time, ylim=c(0,0.5), main= "air (CO2 crg)"))
with(flux, plot(crg.CO2 ~epoch_time, ylim=c(0,0.5), main= "flux (CO2 crg)"))

dev.off()

# 
myplot <- paste0(myplots, "/A_incubation_crg_timeseries_CH4.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(crg.CH4 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CH4 crg)"))
with(air, plot(crg.CH4 ~epoch_time, ylim=c(0,0.5), main= "air (CH4 crg)"))
with(flux, plot(crg.CH4 ~epoch_time, ylim=c(0,0.5), main= "flux (CH4 crg)"))

dev.off()

# to be consistent, we consider the same time span, although there is no problem with rg

# check boxplots
myplot <- paste0(myplots, "/A_incubation_crg_boxplots_testTime.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(2,4))

boxplot(cal2[, crg.N2O], main = "cal2 crg.N2O", ylim=c(0,0.5), range= myrange)
boxplot(cal2[, crg.CO2], main = "cal2 crg.CO2", ylim=c(0,0.5), range= myrange)
boxplot(cal2[, crg.CH4], main = "cal2 crg.CH4", ylim=c(0,0.5), range= myrange)
boxplot(cal2[, crg.H2O], main = "cal2 crg.H2O", ylim=c(0,20), range= myrange)

boxplot(air[, crg.N2O], main = "air crg.N2O", ylim=c(0,0.5), range= myrange)
boxplot(air[, crg.CO2], main = "air crg.CO2", ylim=c(0,0.5), range= myrange)
boxplot(air[, crg.CH4], main = "air crg.CH4", ylim=c(0,0.5), range= myrange)
boxplot(air[, crg.H2O], main = "air crg.H2O", ylim=c(0,20), range= myrange)

dev.off()
# get threshold values
thr.N2O.crg <- median(air[, crg.N2O], na.rm=T) +
        5.160577*(
                quantile(air[, crg.N2O], probs=0.75, na.rm=T)[[1]]
                - quantile(air[, crg.N2O], probs=0.25, na.rm=T)[[1]] 
        )
thr.CO2.crg <- median(air[, crg.CO2], na.rm=T) +
        5.160577*(
                quantile(air[, crg.CO2], probs=0.75, na.rm=T)[[1]]
                - quantile(air[, crg.CO2], probs=0.25, na.rm=T)[[1]] 
        )
thr.CH4.crg <- median(air[, crg.CH4], na.rm=T) +
        5.160577*(
                quantile(air[, crg.CH4], probs=0.75, na.rm=T)[[1]] 
                - quantile(air[, crg.CH4], probs=0.25, na.rm=T)[[1]] 
        )
thr.N2O.crg
thr.CO2.crg
thr.CH4.crg

# plot it
myplot <- paste0(myplots, "/A_incubation_crg_vs_CONC__air_flux.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(2,3))

with(air, plot(N2O_ppb~crg.N2O, main= "air (crg)", xlim= c(0, 0.5))); abline(v=thr.N2O.crg, col="red")
with(air, plot(CO2_ppm~crg.CO2, main= "air (crg)", xlim= c(0, 0.5))); abline(v=thr.CO2.crg, col="red")
with(air, plot(CH4_ppb~crg.CH4, main= "air (crg)", xlim= c(0, 0.5))); abline(v=thr.CH4.crg, col="red")

with(flux, plot(N2O_ppb~crg.N2O, main= "flux (crg)", xlim= c(0, 0.5))); abline(v=thr.N2O.crg, col="red")
with(flux, plot(CO2_ppm~crg.CO2, main= "flux (crg)", xlim= c(0, 0.5))); abline(v=thr.CO2.crg, col="red")
with(flux, plot(CH4_ppb~crg.CH4, main= "flux (crg)", xlim= c(0, 0.5))); abline(v=thr.CH4.crg, col="red")

dev.off()
# 
myplot <- paste0(myplots, "/A_incubation_crg_vs_CONC__cal2_air.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(2,3))

with(cal2, plot(N2O_ppb~crg.N2O, main= "cal2 (crg)", xlim= c(0, 0.5))); abline(v=thr.N2O.crg, col="red")
with(cal2, plot(CO2_ppm~crg.CO2, main= "cal2 (crg)", xlim= c(0, 0.5))); abline(v=thr.CO2.crg, col="red")
with(cal2, plot(CH4_ppb~crg.CH4, main= "cal2 (crg)", xlim= c(0, 0.5))); abline(v=thr.CH4.crg, col="red")

with(air, plot(N2O_ppb~crg.N2O, main= "air (crg)", xlim= c(0, 0.5))); abline(v=thr.N2O.crg, col="red")
with(air, plot(CO2_ppm~crg.CO2, main= "air (crg)", xlim= c(0, 0.5))); abline(v=thr.CO2.crg, col="red")
with(air, plot(CH4_ppb~crg.CH4, main= "air (crg)", xlim= c(0, 0.5))); abline(v=thr.CH4.crg, col="red")

dev.off()

# check deleted values in test 
mean(cal2$crg.N2O >thr.N2O.crg, na.rm=T)
mean(air$crg.N2O >thr.N2O.crg, na.rm=T)
mean(flux$crg.N2O >thr.N2O.crg, na.rm=T)
mean(flux$crg.N2O >thr.N2O.crg, na.rm=T) / mean(air$crg.N2O >thr.N2O.crg, na.rm=T)

mean(cal2$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(air$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(flux$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(flux$crg.CO2 >thr.CO2.crg, na.rm=T) / mean(air$crg.CO2 >thr.CO2.crg, na.rm=T)

mean(cal2$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(air$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(flux$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(flux$crg.CH4 >thr.CH4.crg, na.rm=T) / mean(air$crg.CH4 >thr.CH4.crg, na.rm=T)


################################################################################
### cv & crg total test

################################
# A incubation
### cv
# check deleted values in test 
mean(raw.cal2$cv.N2O >thr.N2O.cv, na.rm=T)
mean(raw.air$cv.N2O >thr.N2O.cv, na.rm=T)
mean(raw.flux$cv.N2O >thr.N2O.cv, na.rm=T)
mean(raw.flux$cv.N2O >thr.N2O.cv, na.rm=T) / mean(raw.air$cv.N2O >thr.N2O.cv, na.rm=T)

mean(raw.cal2$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(raw.air$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(raw.flux$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(raw.flux$cv.CO2 >thr.CO2.cv, na.rm=T) / mean(raw.air$cv.CO2 >thr.CO2.cv, na.rm=T)

mean(raw.cal2$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(raw.air$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(raw.flux$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(raw.flux$cv.CH4 >thr.CH4.cv, na.rm=T) / mean(raw.air$cv.CH4 >thr.CH4.cv, na.rm=T)

### crg
# check deleted values in test 
mean(raw.cal2$crg.N2O >thr.N2O.crg, na.rm=T)
mean(raw.air$crg.N2O >thr.N2O.crg, na.rm=T)
mean(raw.flux$crg.N2O >thr.N2O.crg, na.rm=T)
mean(raw.flux$crg.N2O >thr.N2O.crg, na.rm=T) / mean(raw.air$crg.N2O >thr.N2O.crg, na.rm=T)

mean(raw.cal2$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(raw.air$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(raw.flux$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(raw.flux$crg.CO2 >thr.CO2.crg, na.rm=T) / mean(raw.air$crg.CO2 >thr.CO2.crg, na.rm=T)

mean(raw.cal2$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(raw.air$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(raw.flux$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(raw.flux$crg.CH4 >thr.CH4.crg, na.rm=T) / mean(raw.air$crg.CH4 >thr.CH4.crg, na.rm=T)

# B incubation
################################################################################
### QCL (my360; measured 360 s values)
input.folder <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/B_IDASw"
meas <- paste0(input.folder, "/B_incubation_full_60_corrected/B_full60_corrected_stdev.dat")
meas.val <- paste0(input.folder, "/B_incubation_full_60_corrected/B_full60_corrected_value.dat")
meas.rg <- paste0(input.folder, "/B_incubation_full_60_corrected/B_full60_corrected_max.dat")
meas.min <- paste0(input.folder, "/B_incubation_full_60_corrected/B_full60_corrected_min.dat")

meas <- fread(meas)
meas <- meas[bin_WORK == "0", ]
meas <- meas[epoch_time%%360==0,]

meas.val <- fread(meas.val)
meas.val <- meas.val[bin_WORK == "0", ]
meas.val <- meas.val[epoch_time%%360==0,]

meas.rg <- fread(meas.rg)
meas.rg <- meas.rg[bin_WORK == "0", ]
meas.rg <- meas.rg[epoch_time%%360==0,]

meas.min <- fread(meas.min)
meas.min <- meas.min[bin_WORK == "0", ]
meas.min <- meas.min[epoch_time%%360==0,]

# 
my360 <- copy(meas.val)

my360[, flowSD:= meas$flow]
my360[, flowMIN:= meas.min$flow]
my360[, flowMAX:= meas.rg$flow]
my360[, exhaustSD:= meas$exhaust]
my360[, exhaustMIN:= meas.min$exhaust]
my360[, exhaustMAX:= meas.rg$exhaust]

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

### cal2
# get cal2 values (the problem is that ch4 is really high, 4000ppb, so sd is high)
cal2 <- copy(my360)
cal2 <- cal2[bin_INC2 == "1-0-00-0000-0000" 
             & bin_INC_valve == 1 
             & bin_WORK == "0"
             ,]

### AIR (tank air)
data <- my360
incubation <- "B"
source("parameter_thresholds_asAincubation_justFlow.R")
my360<- copy(data)

air <- my360[bin_INC1 == "0-1-0-0000-0000"| bin_INC2 == "0-1-00-0000-0000", ]

air <- my360[bin_INC1 == "0-1-0-0000-0000" & bin_INC_valve == 10 |
                     bin_INC2 == "0-1-00-0000-0000" & bin_INC_valve == 1, ]

### CORE concetrations
myfluxes <- sort(unique(c(cal2$epoch_time, air$epoch_time)))
flux <- my360[! epoch_time %in% myfluxes,]

### cv plots timeseries
# have a lokk to QCL performance; consider cal2, air and fluxes
myplot <- paste0(myplots, "/B_incubation_cv_timeseries_N2O.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(cv.N2O ~epoch_time, ylim=c(0,0.5), main= "cal2 (N2O cv)"))
with(air, plot(cv.N2O ~epoch_time, ylim=c(0,0.5), main= "air (N2O cv)"))
with(flux, plot(cv.N2O ~epoch_time, ylim=c(0,0.5), main= "flux (N2O cv)"))

dev.off()
# 

myplot <- paste0(myplots, "/B_incubation_cv_timeseries_CO2.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(cv.CO2 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CO2 cv)"))
with(air, plot(cv.CO2 ~epoch_time, ylim=c(0,0.5), main= "air (CO2 cv)"))
with(flux, plot(cv.CO2 ~epoch_time, ylim=c(0,0.5), main= "flux (CO2 cv)"))

dev.off()

# 
myplot <- paste0(myplots, "/B_incubation_cv_timeseries_CH4.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(cv.CH4 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CH4 cv)"))
with(air, plot(cv.CH4 ~epoch_time, ylim=c(0,0.5), main= "air (CH4 cv)"))
with(flux, plot(cv.CH4 ~epoch_time, ylim=c(0,0.5), main= "flux (CH4 cv)"))

dev.off()

### crg plots timeseries
# have a lokk to QCL performance; consider cal2, air and fluxes
myplot <- paste0(myplots, "/B_incubation_crg_timeseries_N2O.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(crg.N2O ~epoch_time, ylim=c(0,0.5), main= "cal2 (N2O crg)"))
with(air, plot(crg.N2O ~epoch_time, ylim=c(0,0.5), main= "air (N2O crg)"))
with(flux, plot(crg.N2O ~epoch_time, ylim=c(0,0.5), main= "flux (N2O crg)"))

dev.off()
# 

myplot <- paste0(myplots, "/B_incubation_crg_timeseries_CO2.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(crg.CO2 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CO2 crg)"))
with(air, plot(crg.CO2 ~epoch_time, ylim=c(0,0.5), main= "air (CO2 crg)"))
with(flux, plot(crg.CO2 ~epoch_time, ylim=c(0,0.5), main= "flux (CO2 crg)"))

dev.off()

# 
myplot <- paste0(myplots, "/B_incubation_crg_timeseries_CH4.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(crg.CH4 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CH4 crg)"))
with(air, plot(crg.CH4 ~epoch_time, ylim=c(0,0.5), main= "air (CH4 crg)"))
with(flux, plot(crg.CH4 ~epoch_time, ylim=c(0,0.5), main= "flux (CH4 crg)"))

dev.off()

################################################################################
### cv
# check deleted values
mean(cal2$cv.N2O >thr.N2O.cv, na.rm=T)
mean(air$cv.N2O >thr.N2O.cv, na.rm=T)
mean(flux$cv.N2O >thr.N2O.cv, na.rm=T)
mean(flux$cv.N2O >thr.N2O.cv, na.rm=T) / mean(air$cv.N2O >thr.N2O.cv, na.rm=T)

mean(cal2$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(air$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(flux$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(flux$cv.CO2 >thr.CO2.cv, na.rm=T) / mean(air$cv.CO2 >thr.CO2.cv, na.rm=T)

mean(cal2$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(air$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(flux$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(flux$cv.CH4 >thr.CH4.cv, na.rm=T) / mean(air$cv.CH4 >thr.CH4.cv, na.rm=T)

### crg
# check deleted values 
mean(cal2$crg.N2O >thr.N2O.crg, na.rm=T)
mean(air$crg.N2O >thr.N2O.crg, na.rm=T)
mean(flux$crg.N2O >thr.N2O.crg, na.rm=T)
mean(flux$crg.N2O >thr.N2O.crg, na.rm=T) / mean(air$crg.N2O >thr.N2O.crg, na.rm=T)

mean(cal2$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(air$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(flux$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(flux$crg.CO2 >thr.CO2.crg, na.rm=T) / mean(air$crg.CO2 >thr.CO2.crg, na.rm=T)

mean(cal2$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(air$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(flux$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(flux$crg.CH4 >thr.CH4.crg, na.rm=T) / mean(air$crg.CH4 >thr.CH4.crg, na.rm=T)

# C incubation
################################################################################
### QCL (my360; measured 360 s values)
input.folder <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/C_IDASw"
meas <- paste0(input.folder, "/C_incubation_full_60_corrected/C_full60_corrected_stdev.dat")
meas.val <- paste0(input.folder, "/C_incubation_full_60_corrected/C_full60_corrected_value.dat")
meas.rg <- paste0(input.folder, "/C_incubation_full_60_corrected/C_full60_corrected_max.dat")
meas.min <- paste0(input.folder, "/C_incubation_full_60_corrected/C_full60_corrected_min.dat")

meas <- fread(meas)
meas <- meas[bin_WORK == "0", ]
meas <- meas[epoch_time%%360==0,]

meas.val <- fread(meas.val)
meas.val <- meas.val[bin_WORK == "0", ]
meas.val <- meas.val[epoch_time%%360==0,]

meas.rg <- fread(meas.rg)
meas.rg <- meas.rg[bin_WORK == "0", ]
meas.rg <- meas.rg[epoch_time%%360==0,]

meas.min <- fread(meas.min)
meas.min <- meas.min[bin_WORK == "0", ]
meas.min <- meas.min[epoch_time%%360==0,]

# 
my360 <- copy(meas.val)

my360[, flowSD:= meas$flow]
my360[, flowMIN:= meas.min$flow]
my360[, flowMAX:= meas.rg$flow]
my360[, exhaustSD:= meas$exhaust]
my360[, exhaustMIN:= meas.min$exhaust]
my360[, exhaustMAX:= meas.rg$exhaust]

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

### cal2
# get cal2 values (the problem is that ch4 is really high, 4000ppb, so sd is high)
cal2 <- copy(my360)
cal2 <- cal2[bin_INC2 == "1-0-00-0000-0000" 
             & bin_INC_valve == 1 
             & bin_WORK == "0"
             ,]

### AIR (tank air)
data <- my360
incubation <- "C"
source("parameter_thresholds_asAincubation_justFlow.R")
my360<- copy(data)

air <- my360[bin_INC1 == "0-1-0-0000-0000"| bin_INC2 == "0-1-00-0000-0000", ]

air <- my360[bin_INC1 == "0-1-0-0000-0000" & bin_INC_valve == 10 |
                     bin_INC2 == "0-1-00-0000-0000" & bin_INC_valve == 1, ]

### CORE concetrations
myfluxes <- sort(unique(c(cal2$epoch_time, air$epoch_time)))
flux <- my360[! epoch_time %in% myfluxes,]

### cv plots timeseries
# have a lokk to QCL performance; consider cal2, air and fluxes
myplot <- paste0(myplots, "/C_incubation_cv_timeseries_N2O.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(cv.N2O ~epoch_time, ylim=c(0,0.5), main= "cal2 (N2O cv)"))
with(air, plot(cv.N2O ~epoch_time, ylim=c(0,0.5), main= "air (N2O cv)"))
with(flux, plot(cv.N2O ~epoch_time, ylim=c(0,0.5), main= "flux (N2O cv)"))

dev.off()
# 

myplot <- paste0(myplots, "/C_incubation_cv_timeseries_CO2.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(cv.CO2 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CO2 cv)"))
with(air, plot(cv.CO2 ~epoch_time, ylim=c(0,0.5), main= "air (CO2 cv)"))
with(flux, plot(cv.CO2 ~epoch_time, ylim=c(0,0.5), main= "flux (CO2 cv)"))

dev.off()

# 
myplot <- paste0(myplots, "/C_incubation_cv_timeseries_CH4.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(cv.CH4 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CH4 cv)"))
with(air, plot(cv.CH4 ~epoch_time, ylim=c(0,0.5), main= "air (CH4 cv)"))
with(flux, plot(cv.CH4 ~epoch_time, ylim=c(0,0.5), main= "flux (CH4 cv)"))

dev.off()

### crg plots timeseries
# have a lokk to QCL performance; consider cal2, air and fluxes
myplot <- paste0(myplots, "/C_incubation_crg_timeseries_N2O.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(crg.N2O ~epoch_time, ylim=c(0,0.5), main= "cal2 (N2O crg)"))
with(air, plot(crg.N2O ~epoch_time, ylim=c(0,0.5), main= "air (N2O crg)"))
with(flux, plot(crg.N2O ~epoch_time, ylim=c(0,0.5), main= "flux (N2O crg)"))

dev.off()
# 

myplot <- paste0(myplots, "/C_incubation_crg_timeseries_CO2.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(crg.CO2 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CO2 crg)"))
with(air, plot(crg.CO2 ~epoch_time, ylim=c(0,0.5), main= "air (CO2 crg)"))
with(flux, plot(crg.CO2 ~epoch_time, ylim=c(0,0.5), main= "flux (CO2 crg)"))

dev.off()

# 
myplot <- paste0(myplots, "/C_incubation_crg_timeseries_CH4.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(3,1))
with(cal2, plot(crg.CH4 ~epoch_time, ylim=c(0,0.5), main= "cal2 (CH4 crg)"))
with(air, plot(crg.CH4 ~epoch_time, ylim=c(0,0.5), main= "air (CH4 crg)"))
with(flux, plot(crg.CH4 ~epoch_time, ylim=c(0,0.5), main= "flux (CH4 crg)"))

dev.off()
################################################################################
### cv
# check deleted values
mean(cal2$cv.N2O >thr.N2O.cv, na.rm=T)
mean(air$cv.N2O >thr.N2O.cv, na.rm=T)
mean(flux$cv.N2O >thr.N2O.cv, na.rm=T)
mean(flux$cv.N2O >thr.N2O.cv, na.rm=T) / mean(air$cv.N2O >thr.N2O.cv, na.rm=T)

mean(cal2$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(air$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(flux$cv.CO2 >thr.CO2.cv, na.rm=T)
mean(flux$cv.CO2 >thr.CO2.cv, na.rm=T) / mean(air$cv.CO2 >thr.CO2.cv, na.rm=T)

mean(cal2$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(air$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(flux$cv.CH4 >thr.CH4.cv, na.rm=T)
mean(flux$cv.CH4 >thr.CH4.cv, na.rm=T) / mean(air$cv.CH4 >thr.CH4.cv, na.rm=T)

### crg
# check deleted values
mean(cal2$crg.N2O >thr.N2O.crg, na.rm=T)
mean(air$crg.N2O >thr.N2O.crg, na.rm=T)
mean(flux$crg.N2O >thr.N2O.crg, na.rm=T)
mean(flux$crg.N2O >thr.N2O.crg, na.rm=T) / mean(air$crg.N2O >thr.N2O.crg, na.rm=T)

mean(cal2$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(air$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(flux$crg.CO2 >thr.CO2.crg, na.rm=T)
mean(flux$crg.CO2 >thr.CO2.crg, na.rm=T) / mean(air$crg.CO2 >thr.CO2.crg, na.rm=T)

mean(cal2$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(air$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(flux$crg.CH4 >thr.CH4.crg, na.rm=T)
mean(flux$crg.CH4 >thr.CH4.crg, na.rm=T) / mean(air$crg.CH4 >thr.CH4.crg, na.rm=T)
