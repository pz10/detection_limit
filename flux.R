flux <- function(Cout, Cin=0,compound = NULL, flow=NULL, temp=20,  ofmolecule = FALSE, device = "QCL"){
        #applied formula
        #Flux(X)[mass/m2/h] =   (X core - X tank) *
        #                       coef(X) *
        #                       flow [ccm]*60/(10^6) *
        #                       293.15 / (T[°C]+273.15) *
        #                       1 / (0.0124689812420979[m2])
        
        ###    compounds -->  c("CO2", "H2O", "N2O", "NO", "NH3", "CH4")
        
        #       A)ofmolecule = FALSE (default)
        #                                       coef
        #         Co2[ppm] --> mg-C/m2/h        # 0.4991760134  [mg-C/m3]
        #         H2O[ppm] --> g-H2O/m2/h       # 0.7487320181  [mg-H2O/m3] as in ofmolecule = TRUE
        
        #         N2O[ppb] --> µg-N/m2/h        # 1.1642633096  [µg-N/m3]
        #         NO[ppb] --> µg-N/m2/h         # 0.5821316548  [µg-N/m3]
        #         NH3[ppb] --> µg-N/m2/h        # 0.5821316548  [µg-N/m3]
        #         CH4[ppb] --> µg-C/m2/h        # 0.4991760134  [µg-C/m3]
        
        
        #       B)ofmolecule = TRUE
        #                                       coef.ofmol
        #         Co2[ppm] --> mg-CO2/m2/h      # 1.8290970841  [mg-CO2/m3]
        #         H2O[ppm] --> g-H2O/m2/h       # 0.7487320181  [mg-H2O/m3]
        
        #         N2O[ppb] --> µg-N2O/m2/h      # 1.8292217669  [µg-N2O/m3]
        #         NO[ppb] --> µg-NO/m2/h        # 1.2472438876  [µg-NO/m3]
        #         NH3[ppb] --> µg-NH3/m2/h      # 0.7078244135  [µg-NH3/m3]
        #         CH4[ppb] --> µg-CH4/m2/h      # 0.6666375194  [µg-CH4/m3]
        
        #       device --> QCL (default) or PICARRO
        #       just to use specific FLOW METER value (i.e.channel 1010 or 1030)
        
        #############################
        #error message
        if(length(Cout) == 0) stop( "PLEASE SET Concentration out of chamber in ppm (CO2) or ppb (rest); Cin = X ", call. = FALSE)
        if(length(compound) == 0) stop( "PLEASE SET compound -->  (CO2, H2O, N2O, NO, NH3, CH4) ", call. = FALSE)
        if(length(flow) == 0) stop( "PLEASE SET flow rate in mL min-1; flow = ", call. = FALSE)
        
        ### set core area [m2]
        core_area <- 0.0124689812420979
        
        ### set target depending on compound
        target <- which(c("CO2", "H2O", "N2O", "NO", "NH3", "CH4", "Li840CO2") == compound)
        
        ### select coef
        if(ofmolecule == TRUE){
                coef <- c(1.8290970841, 0.7487320181, 1.8292217669, 1.2472438876, 0.7078244135, 0.6666375194, 1.8290970841)
        }else{
                #default
                coef <- c(0.4991760134, 0.7487320181, 1.1642633096, 0.5821316548, 0.5821316548, 0.4991760134, 0.4991760134)   
        }
        
        ### select CORE and TANK measurements
        if (device == "QCL"){
                tank <- c("TANK_CO2_ppm", "TANK_H2O_ppm", "TANK_N2O_ppb", "TANK_CLD_NO", "TANK_NH3_ppb", "TANK_CH4_ppb", "TANK_Li_CO2")
                core <- c("CO2_ppm", "H2O_ppm", "N2O_ppb", "CLD_NO", "NH3_ppb", "CH4_ppb", "Li_CO2") 
        }else if (device == "PICARRO") {
                tank <- c("TANK_PICARROCO2", "TANK_PICARROH2O", "TANK_PICARRON2O", "TANK_CLD_NO", "TANK_PICARRONH3", "TANK_PICARROCH4", "TANK_Li_CO2")
                core <- c("PICARROCO2", "PICARROH2O", "PICARRON2O", "CLD_NO", "PICARRONH3", "PICARROCH4", "Li_CO2")
        }
        
        
#         ### select FLOW METER
#         if (device == "QCL") {
#                 flow = data[,"flow", with=FALSE]
#         }else if (device == "PICARRO") {
#                 flow = data[,"p_flow", with=FALSE]
#         }
        
        ### flux calculation
        if(compound == "NH3"){
                fluxes = (
                        (Cout - Cin ) *
                                coef[target] *
                                (flow * 60/(10^6)) *
                                (293.15 / (temp + 273.15) ) *
                                (1 / core_area)
                )
        }else{
                fluxes = (
                        (Cout - Cin ) *
                                coef[target] *
                                (flow * 60/(10^6)) *
                                (293.15 / (temp + 273.15) ) *
                                (1 / core_area)
                )
        }
        
        fluxes
        
}
