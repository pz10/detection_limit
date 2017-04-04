require(data.table)

input.folder <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/_all_fluxes/Acid_trap_detection_limit"
std <- paste0(input.folder, "/Acid_trap_detection_limit.dat")
std <- fread(std)

boxplot(std[,est.0], main = "blank estimation")
abline(h=0, col="red")

boxplot(std[,est.1], main = "1.0 estimation")
abline(h=1, col="red")

boxplot(std[,est.2], main = "1.5 estimation")
abline(h=1.5, col="red")

boxplot(std[,est.3], main = "2.0 estimation")
abline(h=2, col="red")

boxplot(std[,est.4], main = "2.5 estimation")
abline(h=2.5, col="red")

boxplot(std[,est.5], main = "3.5 estimation")
abline(h=3.5, col="red")

boxplot(std[,est.6], main = "5.0 estimation")
abline(h=5, col="red")

###
boxplot(std[,est.0], main = "blank estimation")
abline(h=0, col="red")

sd(std[,est.0])
summary(std[,est.0])
sd(std[,est.0])*3.29*100

sd(std[,est.1])
summary(std[,est.1])

sd(std[,est.2])
summary(std[,est.2])

sd(std[,est.3])
summary(std[,est.3])

sd(std[,est.4])
summary(std[,est.4])

sd(std[,est.5])
summary(std[,est.5])

sd(std[,est.6])
summary(std[,est.6])

detect <- sd(std[,est.0])*3.29*100
area <- pi*(0.126/2)^2

NH3flux <- 200 #µg-N m-2 h-1
core.flux <- NH3flux*area


detect/core.flux # time [h] to detect the NH3 volatilization from the core

