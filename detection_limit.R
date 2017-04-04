Sys.setenv(TZ='UTC') #--> very important

require(data.table)
input.folder <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/A_IDASw"
tank <- paste0(input.folder, "/A_incubation_TANK_corrected.dat")
tank.sd <- paste0(input.folder, "/A_incubation_TANKsd_corrected.dat")
meas <- paste0(input.folder, "/A_incubation_full_60_corrected/A_full60_corrected_stdev.dat")
meas.val <- paste0(input.folder, "/A_incubation_full_60_corrected/A_full60_corrected_value.dat")


tank <- fread(tank)
tank.sd <- fread(tank.sd)
meas <- fread(meas)
meas <- meas[bin_WORK == "0", ]
meas <- meas[epoch_time%%360==0,]

meas.val <- fread(meas.val)
meas.val <- meas.val[bin_WORK == "0", ]
meas.val <- meas.val[epoch_time%%360==0,]

# get cal2 values (the problem is that ch4 is really high, 4000ppb, so sd is high)
cal2 <- copy(meas)
cal2 <- cal2[bin_INC2 == "1-0-00-0000-0000" 
             & bin_INC_valve == 1 
             & bin_WORK == "0"
             ,]

boxplot(cal2[!is.na(sd_N2O_ppb) & sd_N2O_ppb < 10,sd_N2O_ppb], main = " cal2 sd_N2O_ppb")
boxplot(cal2[!is.na(sd_CO2_ppm) & sd_CO2_ppm < 10,sd_CO2_ppm], main = "cal2 sd_CO2_ppm")
boxplot(cal2[!is.na(sd_CH4_ppb) & sd_CH4_ppb < 10,sd_CH4_ppb], main = "cal2 sd_CH4_ppb")

plot(meas.val[time %in% cal2$time, N2O_ppb], main = "sd_N2O_ppb")

# get 360 AIR values
meas <- meas[bin_INC1 == "0-1-0-0000-0000"| bin_INC2 == "0-1-00-0000-0000", ]

meas <- meas[bin_INC1 == "0-1-0-0000-0000" & bin_INC_valve == 10 |
             bin_INC2 == "0-1-00-0000-0000" & bin_INC_valve == 1, ]

boxplot(meas[!is.na(sd_N2O_ppb) & sd_N2O_ppb < 10,sd_N2O_ppb], main = "sd_N2O_ppb")
boxplot(meas[!is.na(sd_CO2_ppm) & sd_CO2_ppm < 10,sd_CO2_ppm], main = "sd_CO2_ppm")
boxplot(meas[!is.na(sd_CH4_ppb) & sd_CH4_ppb < 10,sd_CH4_ppb], main = "sd_CH4_ppb")



# n2o
plot(meas.val[!is.na(N2O_ppb), sort(N2O_ppb)], main = "N2O_ppb")
plot(meas.val[!is.na(N2O_ppb) & N2O_ppb < 500,sort(N2O_ppb)], main = "N2O_ppb")
plot(meas.val[!is.na(N2O_ppb) & N2O_ppb < 370,sort(N2O_ppb)], main = "N2O_ppb")
plot(meas.val[!is.na(N2O_ppb) & N2O_ppb > 370,sort(N2O_ppb)], main = "N2O_ppb")
plot(meas.val[!is.na(N2O_ppb) & N2O_ppb < 450 & N2O_ppb > 370,sort(N2O_ppb)], main = "N2O_ppb")
plot(meas.val[!is.na(N2O_ppb) & N2O_ppb < 400 & N2O_ppb > 370,sort(N2O_ppb)], main = "N2O_ppb")

plot(meas.val[!is.na(N2O_ppb) & N2O_ppb < 400 & N2O_ppb > 370, sort(N2O_ppb)], main = "N2O_ppb")
mytimes<- meas.val[!is.na(N2O_ppb) & N2O_ppb < 400 & N2O_ppb > 370, sort(time)]
plot(meas[!is.na(sd_N2O_ppb), sort(sd_N2O_ppb)], main = "sd_N2O_ppb")
plot(meas[!is.na(sd_N2O_ppb) & time %in% mytimes, sort(sd_N2O_ppb)], main = "sd_N2O_ppb")

plot(meas[!is.na(sd_N2O_ppb) & time %in% mytimes, sort(sd_N2O_ppb)], main = "sd_N2O_ppb")
abline(h = quantile(meas[!is.na(sd_N2O_ppb) & time %in% mytimes, sd_N2O_ppb], probs = c(0.5,0.75,0.95, 0.99)), col="red")
quantile(meas[!is.na(sd_N2O_ppb) & time %in% mytimes, sd_N2O_ppb], probs = 75:100/100)
quantile(meas[!is.na(sd_N2O_ppb) & time %in% mytimes, sd_N2O_ppb], probs = c(0.5,0.75,0.95, 0.99))
mean(meas[!is.na(sd_N2O_ppb) & time %in% mytimes, sd_N2O_ppb])

plot(meas[!is.na(sd_N2O_ppb) & time %in% mytimes, sd_N2O_ppb], main = "sd_N2O_ppb", ylim = c(0, 2))

# co2
plot(meas.val[!is.na(CO2_ppm), sort(CO2_ppm)], main = "CO2_ppm")
plot(meas.val[!is.na(CO2_ppm) & CO2_ppm < 450,sort(CO2_ppm)], main = "CO2_ppm")
plot(meas.val[!is.na(CO2_ppm) & CO2_ppm > 500,sort(CO2_ppm)], main = "CO2_ppm")

plot(meas.val[!is.na(CO2_ppm) & CO2_ppm < 550 & CO2_ppm > 440, sort(CO2_ppm)], main = "CO2_ppm")
mytimes<- meas.val[!is.na(CO2_ppm) & CO2_ppm < 550 & CO2_ppm > 440, sort(time)]
plot(meas[!is.na(sd_CO2_ppm), sort(sd_CO2_ppm)], main = "sd_CO2_ppm")
plot(meas[!is.na(sd_CO2_ppm) & time %in% mytimes, sort(sd_CO2_ppm)], main = "sd_CO2_ppm")

mytimes<- meas[!is.na(sd_CO2_ppm) & time %in% mytimes & sd_CO2_ppm <0.5, time]
plot(meas[!is.na(sd_CO2_ppm), sort(sd_CO2_ppm)], main = "sd_CO2_ppm")
plot(meas[!is.na(sd_CO2_ppm) & time %in% mytimes, sort(sd_CO2_ppm)], main = "sd_CO2_ppm")

plot(meas[!is.na(sd_CO2_ppm) & time %in% mytimes, sort(sd_CO2_ppm)], main = "sd_CO2_ppm")
abline(h = quantile(meas[!is.na(sd_CO2_ppm) & time %in% mytimes, sd_CO2_ppm], probs = c(0.5,0.75,0.95, 0.99)), col="red")
quantile(meas[!is.na(sd_CO2_ppm) & time %in% mytimes, sd_CO2_ppm], probs = 75:100/100)
quantile(meas[!is.na(sd_CO2_ppm) & time %in% mytimes, sd_CO2_ppm], probs = c(0.5,0.75,0.95, 0.99))
mean(meas[!is.na(sd_CO2_ppm) & time %in% mytimes, sd_CO2_ppm])

plot(meas[!is.na(sd_CO2_ppm) & time %in% mytimes, sd_CO2_ppm], main = "sd_CO2_ppm", ylim=c(0,2))


# ch4
plot(meas.val[!is.na(CH4_ppb), sort(CH4_ppb)], main = "CH4_ppb")
plot(meas.val[!is.na(CH4_ppb) & CH4_ppb < 2100,sort(CH4_ppb)], main = "CH4_ppb")
plot(meas.val[!is.na(CH4_ppb) & CH4_ppb < 2300,sort(CH4_ppb)], main = "CH4_ppb")

plot(meas.val[!is.na(CH4_ppb) & CH4_ppb < 2110 & CH4_ppb > 2030,sort(CH4_ppb)], main = "CH4_ppb")
mytimes<- meas.val[!is.na(CH4_ppb) & CH4_ppb < 2110 & CH4_ppb > 2030, sort(time)]
plot(meas[!is.na(sd_CH4_ppb), sort(sd_CH4_ppb)], main = "sd_CH4_ppb")
plot(meas[!is.na(sd_CH4_ppb) & time %in% mytimes, sort(sd_CH4_ppb)], main = "sd_CH4_ppb")

plot(meas[!is.na(sd_CH4_ppb) & time %in% mytimes, sort(sd_CH4_ppb)], main = "sd_CH4_ppb")
abline(h = quantile(meas[!is.na(sd_CH4_ppb) & time %in% mytimes, sd_CH4_ppb], probs = c(0.5,0.75,0.95, 0.99)), col="red")
quantile(meas[!is.na(sd_CH4_ppb) & time %in% mytimes, sd_CH4_ppb], probs = 75:100/100)
quantile(meas[!is.na(sd_CH4_ppb) & time %in% mytimes, sd_CH4_ppb], probs = c(0.5,0.75,0.95, 0.99))
mean(meas[!is.na(sd_CH4_ppb) & time %in% mytimes, sd_CH4_ppb])

plot(meas[!is.na(sd_CH4_ppb) & time %in% mytimes, sd_CH4_ppb], main = "sd_CH4_ppb")




# tank
boxplot(tank.sd[!is.na(TANK_CLD_NO_sd),TANK_CLD_NO_sd], main = "TANK_CLD_NO_sd")
points(mean(tank.sd$TANK_CLD_NO_sd, na.rm=T), col="red", pch=19)
quantile(tank.sd$TANK_CLD_NO_sd, na.rm=T, probs = c(0.5,0.75,0.95, 0.99))
mean(tank.sd$TANK_CLD_NO_sd, na.rm=T)
sum(!is.na(tank.sd$TANK_CLD_NO_sd))

boxplot(tank.sd[!is.na(TANK_N2O_ppb_sd),TANK_N2O_ppb_sd], main = "TANK_N2O_ppb_sd")
points(mean(tank.sd$TANK_N2O_ppb, na.rm=T), col="red", pch=19)
quantile(tank.sd$TANK_N2O_ppb, na.rm=T, probs = c(0.5,0.75,0.95, 0.99))
mean(tank.sd$TANK_N2O_ppb, na.rm=T)
sum(!is.na(tank.sd$TANK_N2O_ppb))

boxplot(tank.sd[!is.na(TANK_CO2_ppm_sd),TANK_CO2_ppm_sd], main = "TANK_CO2_ppm_sd")
points(mean(tank.sd$TANK_CO2_ppm, na.rm=T), col="red", pch=19)
quantile(tank.sd$TANK_CO2_ppm, na.rm=T, probs = c(0.5,0.75,0.95, 0.99))
mean(tank.sd$TANK_CO2_ppm, na.rm=T)
sum(!is.na(tank.sd$TANK_CO2_ppm))

boxplot(tank.sd[!is.na(TANK_CH4_ppb_sd),TANK_CH4_ppb_sd], main = "TANK_CH4_ppb_sd")
points(mean(tank.sd$TANK_CH4_ppb, na.rm=T), col="red", pch=19)
quantile(tank.sd$TANK_CH4_ppb, na.rm=T, probs = c(0.5,0.75,0.95, 0.99))
mean(tank.sd$TANK_CH4_ppb, na.rm=T)
sum(!is.na(tank.sd$TANK_CH4_ppb))



tank[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
tank.sd[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
meas[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]



# tank values (for No comensation point)
boxplot(tank[!is.na(TANK_CLD_NO) & TANK_CLD_NO < 10,TANK_CLD_NO], main = " tank TANK_CLD_NO")
boxplot(tank[!is.na(sd_CO2_ppm) & sd_CO2_ppm < 10,sd_CO2_ppm], main = "tank sd_CO2_ppm")
boxplot(tank[!is.na(sd_CH4_ppb) & sd_CH4_ppb < 10,sd_CH4_ppb], main = "tank sd_CH4_ppb")



################################################################################
meas.val <- meas.val[epoch_time %in% meas$epoch_time,]
meas.val[, dev.NO:= CLD_NO - TANK_CLD_NO]
meas.val[, dev.N2O:= N2O_ppb - TANK_N2O_ppb]
meas.val[, dev.CO2:= CO2_ppm - TANK_CO2_ppm]
meas.val[, dev.CH4:= CH4_ppb - TANK_CH4_ppb]


data <- copy(meas.val)
source("parameter_thresholds.R")
source("flux.R")
# NO
boxplot(data[!is.na(dev.NO) & abs(dev.NO) < 20, dev.NO], main = "dev.NO")
quantile(data[, dev.NO], na.rm=T, probs= c(0.5, 0.75, 90:100/100))
LOD.NO <- 3.29 * mean(tank.sd$TANK_CLD_NO_sd, na.rm=T)
LOD.NO <- 3.48 * mean(tank.sd$TANK_CLD_NO_sd, na.rm=T)
flux(LOD.NO,compound ="NO", flow=333)

mean(data[, dev.NO]>LOD.NO, na.rm=T)
with(data, plot(TANK_CLD_NO ~ dev.NO))

LOD.NO.np <- median(tank.sd$TANK_CLD_NO_sd, na.rm=T) +
        5.160577*( quantile(tank.sd$TANK_CLD_NO_sd, probs=0.75, na.rm=T)[[1]] -
                           quantile(tank.sd$TANK_CLD_NO_sd, probs=0.25, na.rm=T)[[1]] )
mean(data[, dev.NO]>LOD.NO.np, na.rm=T)

boxplot(tank.sd$TANK_CLD_NO_sd, na.rm =T, main = "TANK.NO.sd")
plot(tank.sd$TANK_CLD_NO_sd, na.rm =T, main = "TANK.NO.sd")


# N2O
boxplot(data[!is.na(dev.N2O) & abs(dev.N2O) < 20, dev.N2O], main = "dev.N2O")
quantile(data[, dev.N2O], na.rm=T, probs= c(0.5, 0.75, 90:100/100))
LOD.N2O <- 3.29 * mean(tank.sd$TANK_N2O_ppb_sd, na.rm=T)
LOD.N2O <- 3.48 * mean(tank.sd$TANK_N2O_ppb_sd, na.rm=T)
flux(LOD.N2O,compound ="N2O", flow=333)

mean(data[, dev.N2O]>LOD.N2O, na.rm=T)
with(data, plot(TANK_N2O_ppb ~ dev.N2O))

LOD.N2O.np <- median(tank.sd$TANK_N2O_ppb_sd, na.rm=T) +
        5.160577*( quantile(tank.sd$TANK_N2O_ppb_sd, probs=0.75, na.rm=T)[[1]] -
                           quantile(tank.sd$TANK_N2O_ppb_sd, probs=0.25, na.rm=T)[[1]] )
mean(data[, dev.N2O]>LOD.N2O.np, na.rm=T)

# CO2
boxplot(data[!is.na(dev.CO2) & abs(dev.CO2) < 20, dev.CO2], main = "dev.CO2")
quantile(data[, dev.CO2], na.rm=T, probs= c(0.5, 0.75, 90:100/100))
LOD.CO2 <- 3.29 * mean(tank.sd$TANK_CO2_ppm_sd, na.rm=T)
LOD.CO2 <- 3.48 * mean(tank.sd$TANK_CO2_ppm_sd, na.rm=T)
flux(LOD.CO2,compound ="CO2", flow=333)

mean(data[, dev.CO2]>LOD.CO2, na.rm=T)
with(data, plot(TANK_CO2_ppm ~ dev.CO2))

LOD.CO2.np <- median(tank.sd$TANK_CO2_ppm_sd, na.rm=T) +
        5.160577*( quantile(tank.sd$TANK_CO2_ppm_sd, probs=0.75, na.rm=T)[[1]] -
                           quantile(tank.sd$TANK_CO2_ppm_sd, probs=0.25, na.rm=T)[[1]] )
mean(data[, dev.CO2]>LOD.CO2.np, na.rm=T)

# CH4
boxplot(data[!is.na(dev.CH4) & abs(dev.CH4) < 20, dev.CH4], main = "dev.CH4")
quantile(data[, dev.CH4], na.rm=T, probs= c(0.5, 0.75, 90:100/100))
LOD.CH4 <- 3.29 * mean(tank.sd$TANK_CH4_ppb_sd, na.rm=T)
LOD.CH4 <- 3.48 * mean(tank.sd$TANK_CH4_ppb_sd, na.rm=T)
flux(LOD.CH4,compound ="CH4", flow=333)

mean(data[, dev.CH4]>LOD.CH4, na.rm=T)
with(data, plot(TANK_CH4_ppb ~ dev.CH4))

LOD.CH4.np <- median(tank.sd$TANK_CH4_ppb_sd, na.rm=T) +
        5.160577*( quantile(tank.sd$TANK_CH4_ppb_sd, probs=0.75, na.rm=T)[[1]] -
                           quantile(tank.sd$TANK_CH4_ppb_sd, probs=0.25, na.rm=T)[[1]] )
mean(data[, dev.CH4]>LOD.CH4.np, na.rm=T)
