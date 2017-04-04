### flow

# A_incubation
myplot <- paste0(myplots, "/A_incubation_flow_parameters.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(2,5))
boxplot(A_flux[, flow], main = "flow")
boxplot(A_flux[, MEANflow], main = "MEANflow")

boxplot(A_flux[, exhaust], main = "exhaust")

boxplot(A_flux[, p_flow], main = "p_flow")
boxplot(A_flux[, MEANp_flow], main = "MEANp_flow")



boxplot(A_flux[, flowSD], main = "flowSD")
boxplot(A_flux[, exhaustSD], main = "exhaustSD")
boxplot(A_flux[, flowMAX], main = "flowMAX")
boxplot(A_flux[, flowMIN], main = "flowMIN")

dev.off()


# B_incubation
myplot <- paste0(myplots, "/B_incubation_flow_parameters.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(2,5))
boxplot(B_flux[, flow], main = "flow")
boxplot(B_flux[, MEANflow], main = "MEANflow")

boxplot(B_flux[, exhaust], main = "exhaust")

boxplot(B_flux[, p_flow], main = "p_flow")
boxplot(B_flux[, MEANp_flow], main = "MEANp_flow")



boxplot(B_flux[, flowSD], main = "flowSD")
boxplot(B_flux[, exhaustSD], main = "exhaustSD")
boxplot(B_flux[, flowMAX], main = "flowMAX")
boxplot(B_flux[, flowMIN], main = "flowMIN")

dev.off()

# C_incubation
myplot <- paste0(myplots, "/C_incubation_flow_parameters.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 2.5 )

par(mfrow=c(2,5))
boxplot(C_flux[, flow], main = "flow")
boxplot(C_flux[, MEANflow], main = "MEANflow")

boxplot(C_flux[, exhaust], main = "exhaust")

boxplot(C_flux[, p_flow], main = "p_flow")
boxplot(C_flux[, MEANp_flow], main = "MEANp_flow")



boxplot(C_flux[, flowSD], main = "flowSD")
boxplot(C_flux[, exhaustSD], main = "exhaustSD")
boxplot(C_flux[, flowMAX], main = "flowMAX")
boxplot(C_flux[, flowMIN], main = "flowMIN")

dev.off()

mean(A_flux[, MEANflow]<330 | A_flux[, MEANflow]>335)
mean(A_flux[, flow]<330 | A_flux[, flow]>335)
mean(A_flux[, MEANp_flow]<330 | A_flux[, MEANp_flow]>335)
mean(A_flux[, flowSD]>0.75)
mean(A_flux[, flowMIN]<330)
mean(A_flux[, flowMAX]>335)

mean(B_flux[, MEANflow]<330 | B_flux[, MEANflow]>335)
mean(B_flux[, flow]<330 | B_flux[, flow]>335)
mean(B_flux[, MEANp_flow]<330 | B_flux[, MEANp_flow]>335)
mean(B_flux[, flowSD]>0.75)
mean(B_flux[, flowMIN]<330)
mean(B_flux[, flowMAX]>335)

mean(C_flux[, MEANflow]<330 | C_flux[, MEANflow]>335)
mean(C_flux[, flow]<330 | C_flux[, flow]>335)
mean(C_flux[, MEANp_flow]<330 | C_flux[, MEANp_flow]>335)
mean(C_flux[, flowSD]>0.75)
mean(C_flux[, flowMIN]<330)
mean(C_flux[, flowMAX]>335)


mean(flux$flow)
sd(flux$flow)

mean(flux$MEANflow)
sd(flux$MEANflow)

mean(flux$flowSD)
sd(flux$flowSD)
