#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# DYNAMIC MODEL
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

library(RDynState5NAInodiscquota)
load("5spp_dynstate_Inew.RData")
#2012 mean catch data

detach("package:RDynState5NAI", unload=TRUE)
dim <- c(4,4); dimnames <- list(option=c("VIOTB","VIIOTB", "VIIIabdOTB", "VIIIabdPTB"),season=as.character(1:4))

#2012 effort
effort  <- array(c(13,0,59,69,71,0,48,38,12,0,14,58,12,0,66,50), dim=dim, dimnames=dimnames)

macPrice    <- array(c(0,  709, 0, 1898, 0,  700, 0,  669),dim=dim,dimnames=dimnames)
hkePrice    <- array(c(0, 2525, 0, 2847, 0, 1879, 0, 2347),dim=dim,dimnames=dimnames)
lezPrice    <- array(c(0, 5312, 0, 4564, 0, 4898, 0, 5055),dim=dim,dimnames=dimnames)
homPrice    <- array(c(0,  742, 0, 1014, 0, 1274, 0, 1669),dim=dim,dimnames=dimnames)
othersPrice <- array(c(  0, 3001,   0, 2279,   0, 2807,   0, 3297),dim=dim,dimnames=dimnames)

# apply(mean_price_other, c(2), mean, na.rm = TRUE)
# 1        2        3        4 
# 3001.157 2278.688 2806.889 3297.455 
# hom<- mean_price_main_spp[mean_price_main_spp$spp=="HOM",]
# hom<- array(c(hom$value), dim=c(4,10), dimnames=list(season=as.character(1:4),year=as.character(2003:2012)))
# apply(hom, c(1), mean, na.rm = TRUE)

#-------------------------------------------------------------------------------
# subsection{Scenario 1: Hake and Mackarel will be the most probably choke species, they need to swap quota
# Although this scenario allows to discards, therefore there should not be any Unconstrained quotum for discarding
#-------------------------------------------------------------------------------
#LndQuotaMAC= 73 / 11 vessels = 7
#DisQuotaMAC= sum(catchMean(mac))- 7 = 289.6552
#LndQuotaHKE= 5919/ 11 = 538 
#DISQuotaHKE= sum(catchMean(hke))- 538 = 370.955 

control <- DynState.control(spp1LndQuota= 45,  spp2LndQuota= 538, #tn
                            spp1LndQuotaFine= 1E9, spp2LndQuotaFine=1E9, # tn
                            fuelUse =1 , 
                            fuelPrice = 1240, # euros/day
                            landingCosts= 121, # euros/tn
                            gearMaintenance= 271,
                            addNoFishing=TRUE, increments=40,
                            spp1DiscardSteps=7, spp2DiscardSteps=7,
		            interpolationdistance=12, simNumber=200, numThreads=16)

z <- DynState(mac, hke, lez, hom, others,macPrice, hkePrice, lezPrice, homPrice, othersPrice,effort, control)


save(z, file = "scenario1.RData")
