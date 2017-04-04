Sys.setenv(TZ='UTC') #--> very important

require(data.table)
input.folder <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting"
A <- paste0(input.folder, "/A_IDASw/A_fluxes/A_fluxes.dat")
B <- paste0(input.folder, "/B_IDASw/B_fluxes/B_fluxes.dat")
C <- paste0(input.folder, "/C_IDASw/C_fluxes/C_fluxes.dat")
D <- paste0(input.folder, "/D_IDASw/D_fluxes/D_fluxes.dat")

acid <- paste0(input.folder, "/_all_fluxes/acid_trap/cumNH3_inTrap_nocorrected.dat")


A <- fread(A)
B <- fread(B)
C <- fread(C)
D <- fread(D)

fluxes <- rbindlist(list(A, B, C, D))

# valid measuremnst
sum(!is.na(fluxes$NO))
sum(!is.na(fluxes$N2O))
sum(!is.na(fluxes$CO2))
sum(!is.na(fluxes$CH4))

NO.f <- sum(!is.na(fluxes[days>0 & days<44, NO]))/44/18/4
N2O.f <- sum(!is.na(fluxes[days>0 & days<44, N2O]))/44/18/4
CO2.f <- sum(!is.na(fluxes[days>0 & days<44, CO2]))/44/18/4
CH4.f <- sum(!is.na(fluxes[days>0 & days<44, CH4]))/44/18/4

CH4.f.ABC <- sum(!is.na(rbindlist(list(A,B,C))[days>0 & days<44, CH4]))/44/18/3

NO.f
N2O.f
CO2.f
CH4.f
CH4.f.ABC

mean(c(NO.f, N2O.f, CO2.f))


# max
max(fluxes$NO, na.rm=T)
max(fluxes$N2O, na.rm=T)
max(fluxes$CO2, na.rm=T)
# max(A$CH4, na.rm=T)
# max(B$CH4, na.rm=T)
max(C$CH4, na.rm=T)

boxplot(fluxes$NO)
boxplot(fluxes$N2O)
boxplot(fluxes$CO2)

summary(fluxes$NO, na.rm=T)
summary(fluxes$N2O, na.rm=T)
summary(fluxes$CO2, na.rm=T)
summary(c(A$CH4, B$CH4, C$CH4), na.rm=T)



################################################################################
acid <- fread(acid)

acid[, pre.cumN:= c(NA, cumN)]
acid[, cumEmission:= cumN - pre.cumN]
acid <- acid[days>0 & days < 14,]

boxplot(acid$cumEmission)
summary(acid$cumEmission, na.rm=T)
