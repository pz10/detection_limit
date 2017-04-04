Sys.setenv(TZ='UTC') #--> very important

require(data.table)
input.folder <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting"
tank.A <- paste0(input.folder, "/A_IDASw/A_incubation_TANK_corrected.dat")
tank.B <- paste0(input.folder, "/B_IDASw/B_incubation_TANK_corrected.dat")
tank.C <- paste0(input.folder, "/C_IDASw/C_incubation_TANK_corrected.dat")

# get valid tank codes
flux.A <- paste0(input.folder, "/A_IDASw/A_fluxes/A_fluxes.dat")
flux.B <- paste0(input.folder, "/B_IDASw/B_fluxes/B_fluxes.dat")
flux.C <- paste0(input.folder, "/C_IDASw/C_fluxes/C_fluxes.dat")

codes.A <- paste0(input.folder, "/A_IDASw/A_incubation_filtered_360_corrected/A_filtered360_corrected.dat")
codes.B <- paste0(input.folder, "/B_IDASw/B_incubation_filtered_360_corrected/B_filtered360_corrected.dat")
codes.C <- paste0(input.folder, "/C_IDASw/C_incubation_filtered_360_corrected/C_filtered360_corrected.dat")

flux.A <- fread(flux.A)
flux.B <- fread(flux.B)
flux.C <- fread(flux.C)

codes.A <- fread(codes.A)
codes.B <- fread(codes.B)
codes.C <- fread(codes.C)

times.A <- sort(unique( flux.A[!is.na(NO), time] ))
codes.A <- sort(unique( codes.A[time %in% times.A, TANKcode] ))  

times.B <- sort(unique( flux.B[!is.na(NO), time] ))
codes.B <- sort(unique( codes.B[time %in% times.B, TANKcode] )) 

times.C <- sort(unique( flux.C[!is.na(NO), time] ))
codes.C <- sort(unique( codes.C[time %in% times.C, TANKcode] )) 

# 
tank.A <- fread(tank.A)
tank.B <- fread(tank.B)
tank.C <- fread(tank.C)
to.delete <- names(tank.B)[! names(tank.B) %in% names(tank.A)]
tank.B[, (to.delete):=NULL]
tank.C[, (to.delete):=NULL]

tank.A <- tank.A[TANKcode %in% codes.A,]
tank.B <- tank.B[TANKcode %in% codes.B,]
tank.C <- tank.C[TANKcode %in% codes.C,]


# tank values (for No comensation point)
boxplot(tank.A[!is.na(TANK_CLD_NO),TANK_CLD_NO], main = " tank.A TANK_CLD_NO")
boxplot(tank.B[!is.na(TANK_CLD_NO),TANK_CLD_NO], main = " tank.B TANK_CLD_NO")
boxplot(tank.C[!is.na(TANK_CLD_NO),TANK_CLD_NO], main = " tank.C TANK_CLD_NO")

tank <- rbindlist(list(tank.A, tank.B, tank.C))
boxplot(tank[!is.na(TANK_CLD_NO),TANK_CLD_NO], main = " tank TANK_CLD_NO")
quantile(tank[,TANK_CLD_NO], na.rm=T, probs= c(0.5, 0.75, 0.9, 0.95, 0.99))

mean(tank[!is.na(TANK_CLD_NO),TANK_CLD_NO] > 1)
mean(tank[!is.na(TANK_CLD_NO),TANK_CLD_NO] > 5)

mean(tank.A[!is.na(TANK_CLD_NO),TANK_CLD_NO] > 1)
mean(tank.A[!is.na(TANK_CLD_NO),TANK_CLD_NO] > 5)

mean(tank.B[!is.na(TANK_CLD_NO),TANK_CLD_NO] > 1)
mean(tank.B[!is.na(TANK_CLD_NO),TANK_CLD_NO] > 5)

mean(tank.C[!is.na(TANK_CLD_NO),TANK_CLD_NO] > 1)
mean(tank.C[!is.na(TANK_CLD_NO),TANK_CLD_NO] > 5)

# write file
mydata <- copy(annual.summary)
file <- paste0(output_path, "/torrejon_1992_2011_annual_summaries.dat")
write.table(mydata, file= file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
