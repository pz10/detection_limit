# thr <- fread(input = threshold.file)
thr <- fread("incubation_thresholds.dat")
to.delete <- grep(incubation, names(thr))
to.delete <- c(1,2,3,to.delete)
to.delete <- names(thr)[-to.delete]

set(thr, j = to.delete, value=NULL)
setnames(thr, names(thr), c("target", "order", "parameter", "min", "max"))

min <- thr[!is.na(min), list(target, parameter, min)]
max <- thr[!is.na(max), list(target, parameter, max)]


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