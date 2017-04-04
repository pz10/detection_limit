### threshold file
# threshold.file <- paste0("G:/BioAtmo/zuazo-p/RAW data/_drying_wetting/_extra/incubation_thresholds.dat")
# thr <- fread(input = threshold.file)
thr <- fread("incubation_thresholds.dat")
to.delete <- grep("A", names(thr))
to.delete <- c(1,2,3,to.delete)
to.delete <- names(thr)[-to.delete]

set(thr, j = to.delete, value=NULL)
setnames(thr, names(thr), c("target", "order", "parameter", "min", "max"))

min <- thr[!is.na(min), list(target, parameter, min)]
max <- thr[!is.na(max), list(target, parameter, max)]

min <- min[parameter %in% c("flow", "MEANflow", "exhaust",  "CLD_Temp", "CLD_Press", "MEANp_flow")]
max <- max[parameter %in% c("flow", "MEANflow", "exhaust",  "CLD_Temp", "CLD_Press")]


################################################################################
#global flow values
check <- nrow(data)

min.global <- min[target=="GLOBAL" | target=="variable2",]
if(nrow(min.global) > 0){
        
        for(i in 1:nrow(min.global)){
                data <- data[get(min.global$parameter[i]) > min.global$min[i],]
        }
}
###
max.global <- max[target=="GLOBAL" | target=="variable2",]
if(nrow(max.global) > 0){
        
        for(i in 1:nrow(max.global)){
                data <- data[get(max.global$parameter[i]) < max.global$max[i],]
        }
}
check1 <- nrow(data)
print(paste( format(100*(1-check1/check),digits=2) , " % of values deleted due to flow-related problems"))

################################################################################
# CLD
check <- sum(!is.na(data$CLD_NO))

mytarget <- c("CLD_NO")
###
min.CLD <- min[target=="CLD",]
if(nrow(min.CLD) > 0){
        
        for(i in 1:nrow(min.CLD)){
                data[get(min.CLD$parameter[i]) < min.CLD$min[i], I(mytarget):=NA]
        }
}
###
max.CLD <- max[target=="CLD",]
if(nrow(max.CLD) > 0){
        
        for(i in 1:nrow(max.CLD)){
                data <- data[get(max.CLD$parameter[i]) > max.CLD$max[i], I(mytarget):=NA]
        }
}
check1 <- sum(!is.na(data$CLD_NO))
print(paste( format(100*(1-check1/check),digits=2) , " % of NO values deleted due to CLD malfunction"))
################################################################################
# ################################################################################
# # QCLN2O
# check <- sum(!is.na(data$N2O))
# mytarget <- c("N2O", "mN2O")
# ###
# max.QCLN2O <- max[target=="QCLN2O",]
# if(nrow(max.QCLN2O) > 0){
#         
#         for(i in 1:nrow(max.QCLN2O)){
#                 data <- data[get(max.QCLN2O$parameter[i]) > max.QCLN2O$max[i], I(mytarget):=NA]
#         }
# }
# check1 <- sum(!is.na(data$N2O))
# print(paste( format(100*(1-check1/check),digits=2) , " % of QCL-N2O values deleted due to funny N2O rg or sd"))
# ##################################
# # QCLCO2
# check <- sum(!is.na(data$CO2))
# 
# mytarget <- c("CO2", "mCO2")
# ###
# max.QCLCO2 <- max[target=="QCLCO2",]
# if(nrow(max.QCLCO2) > 0){
#         
#         for(i in 1:nrow(max.QCLCO2)){
#                 data <- data[get(max.QCLCO2$parameter[i]) > max.QCLCO2$max[i], I(mytarget):=NA]
#         }
# }
# check1 <- sum(!is.na(data$CO2))
# print(paste( format(100*(1-check1/check),digits=2) , " % of QCL-CO2 values deleted due to funny CO2 rg or sd"))
# ##################################
# # QCLCH4
# check <- sum(!is.na(data$CH4))
# 
# mytarget <- c("CH4", "mCH4")
# ###
# max.QCLCH4 <- max[target=="QCLCH4",]
# if(nrow(max.QCLCH4) > 0){
#         
#         for(i in 1:nrow(max.QCLCH4)){
#                 data <- data[get(max.QCLCH4$parameter[i]) > max.QCLCH4$max[i], I(mytarget):=NA]
#         }
# }
# check1 <- sum(!is.na(data$CH4))
# print(paste( format(100*(1-check1/check),digits=2) , " % of QCL-CH4 values deleted due to funny CH4 rg or sd"))
# ################################################################################
# ################################################################################
# # LiCO2
# check <- sum(!is.na(data$Li840CO2))
# 
# mytarget <- c("Li840CO2", "mLi840CO2")
# ###
# max.LiCO2 <- max[target=="LiCO2",]
# if(nrow(max.LiCO2) > 0){
#         
#         for(i in 1:nrow(max.LiCO2)){
#                 data <- data[get(max.LiCO2$parameter[i]) > max.LiCO2$max[i], I(mytarget):=NA]
#         }
# }
# check1 <- sum(!is.na(data$Li840CO2))
# print(paste( format(100*(1-check1/check),digits=2) , " % of QCL-Li840CO2 values deleted due to funny Li840CO2 rg or sd"))
# ################################################################################