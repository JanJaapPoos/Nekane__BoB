###############################################################################
# BoB, data exploration
# Fleet dynamics
# IJmuiden, IMARES
# 20th July 2016
###############################################################################

#==============================================================================
# libraries
#==============================================================================
library(nekaneR)
library(plyr)
library(reshape2)
library(devtools)
library(FLCore)
library(ggplotFL)
library (FLFishery)

#==============================================================================
# Read data
#==============================================================================
y<-2003:2012

# Catch at age for the Basque fleet (FLFishery=flf), Stock data for the selected stocks (FLBiol=stk_bio),
# years, order of the species (mac, hom, hke, lez, anf),  fishmort and 
# international catches for the last assessment (lez and mon in 2005, others in 2012) 

load("~/Dropbox/BoB/DynState/July_2016/data/others_finAL.RData")
load("~/Dropbox/BoB/DynState/July_2016/data/FLFishery_Basque_BoB.RData")
species<- names(flf)

# Ang[[5]] and lez[[4]] in area VI equal to zero

landings.wt(flf[[4]][,,,,"VI"])[] <- 0
discards.wt(flf[[4]][,,,,"VI"])[] <- 0
landings.wt(flf[[5]][,,,,"VI"])[] <- 0
discards.wt(flf[[5]][,,,,"VI"])[] <- 0

#number_vessels[,,"PTB"] <- number_vessels[,,"PTB"] *2
#effort_days[,,"PTB"]    <- effort_days[,,"PTB"] *2
# the effort and number of vessels for PTB are already in total days and vessels

#-------------------------------------------------------------------------------
# Manipulating data for using it in the model
#-------------------------------------------------------------------------------

dimnames<-list(len=c('<MCRS','>MCRS'), year=c(2003:2012),unit=c("OTB","PTB"),
              season=as.character(1:4),area=c("VI","VII","VIIIabd"))

#------------------------------------------------------------------------------
# CPUE (catch by vessel), catch weight (landings+ discards) by gear, season, year, day, vessel
#------------------------------------------------------------------------------

CPUE_2012<-FLQuants(mac=FLQuant(, dimnames=dimnames), 
               hom=FLQuant(, dimnames=dimnames), 
               hke=FLQuant(, dimnames=dimnames),
               lez=FLQuant(, dimnames=dimnames), 
               anf=FLQuant(, dimnames=dimnames),
               others=FLQuant(, dimnames=dimnames)) 

for (i in 1:(length(species)+1)){  
      
      if (i<6){
            
            catch.wt <- (landings.wt(flf[[i]])+ discards.wt(flf[[i]]))/1000
            catch.wt[catch.wt==0]<- NA
            units(catch.wt)<-"t"    
            CPUE1  <- catch.wt  %/% number_vessels[,as.character(y)]
            #(effort_days[,as.character(y)] * number_vessels[,as.character(y)])#      
      }
      
      if (i==6){
            # Others is already in tonnes and in the proper format. <MCRS is the discard fraction,
            # while >MCRS is the landing fraction for the group of species selected            
            CPUE1  <- others %/% number_vessels[,as.character(y)]      
      }
            
      CPUE1[is.nan(CPUE1)| is.infinite(CPUE1)|CPUE1==0] <- NA     
      units(CPUE1)<-"t/vessel"     
      CPUE_2012 [[i]] <-CPUE1
}

CPUE_2012 <-   lapply(CPUE_2012,function (x) {
            units(x) <- "t/vessel"
            return (x)
            })

CPUE_2012 <- CPUE_2012 [-5]
# Remove the element 5 from the FLQuants --> ANGLERFISH
# Now it is considered under 'OTHERS' species
species<- names(CPUE_2012)

#==============================================================================
## Structure for Jan Jaap's model
#==============================================================================
                         
# Estimate the mean and the standard deviation of the time series by vessel
  catchmean_2012 <-   lapply(CPUE_2012, '[',j=10)
  catchmean_2012 <-   lapply(catchmean_2012,function (x) {
      quant(x) <- "cat"
      units(x) <- "t/vessel"
      return (x)
      })

#variance divided by the number of years to estimate the variance of the mean 
  sigma <- lapply(lapply(CPUE_2012, '[',j=10), function (x) sqrt(yearVars(x)/1))
  sigma <-   lapply(sigma,function (x) {
      x[is.na(x)]<- 0.001
      quant(x) <- "cat"
      units(x) <- "t/vessel"
      return (x)
      })

#==============================================================================
# DYNAMIC MODEL
#==============================================================================

library(RDynState5NAI)
#load("~/Documents/BoB/DynState/data/5spp_dynstate.RData")

mac <- hke <-  hom <- lez <- others <- new("DynStateInput")

sp<-c(mac=mac,hom=hom,hke=hke,lez=lez,others=others)

# OTB gear and PTB in area VIIIabd... for each time step a vessel chooses the gear (OTB, PTB)
# and the fishing ground based on the optimal choice given the vessel's state
#-------------------------------------------------------------------------------

dimnames<-list(len=c('<MCRS','>MCRS'),year="unique",unit="unique", 
               season=as.character(1:4),area=c("VIOTB","VIIOTB","VIIIabdOTB","VIIIabdPTB"))

catchmean2 <-FLQuants(mac=FLQuant(, dimnames=dimnames), 
                      hom=FLQuant(, dimnames=dimnames), 
                      hke=FLQuant(, dimnames=dimnames),
                      lez=FLQuant(, dimnames=dimnames),
                      others=FLQuant(, dimnames=dimnames))
sigma2 <-catchmean2

for(i in 1:5){  
      
      catchmean2 [[i]][,,,,c("VIOTB","VIIOTB","VIIIabdOTB")] <-catchmean_2012[[i]][,,"OTB",] 
      catchmean2 [[i]][,,,,"VIIIabdPTB"] <-catchmean_2012[[i]][,,"PTB",,'VIIIabd']
      catchMean(sp[[i]])  <- catchmean2[[i]]
      catchMean(sp[[i]])[is.na(catchMean(sp[[i]]))]<-0
      catchMean(sp[[i]])[catchMean(sp[[i]])=="Inf"]<-0
      catchMean(sp[[i]])[catchMean(sp[[i]])=="NaN"]<-0
      catchMean(sp[[i]])<- catchMean(sp[[i]])[drop=T]
      names(dimnames(catchMean(sp[[i]])))<- c('cat','season', 'option')
      
      sigma2 [[i]][,,,,c("VIOTB","VIIOTB","VIIIabdOTB")] <-sigma[[i]][,,"OTB",] 
      sigma2 [[i]][,,,,"VIIIabdPTB"] <-sigma[[i]][,,"PTB",,'VIIIabd']
      catchSigma(sp[[i]]) <- sigma2[[i]]
      catchSigma(sp[[i]])[is.na(catchSigma(sp[[i]]))]<-0
      catchSigma(sp[[i]])[catchSigma(sp[[i]])=="Inf"]<-0
      catchSigma(sp[[i]])[catchSigma(sp[[i]])=="NaN"]<-0
      catchSigma(sp[[i]])<- catchSigma(sp[[i]])[drop=T]
      names(dimnames(catchSigma(sp[[i]])))<-c("cat","season","option")
      
      }

mac    <- sp[[1]]
hom    <- sp[[2]]
hke    <- sp[[3]]
lez    <- sp[[4]]
others <- sp[[5]]

save(CPUE_2012, species, flf, number_vessels,catchmean_2012,
     effort_trips, effort_days, mac, hke, hom, lez, others,
     file="~/Dropbox/BoB/DynState/2017/data/5spp_dynstate_modelvalidation.RData") 


