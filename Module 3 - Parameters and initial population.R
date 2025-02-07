# Module 3 - Parameters and initial population

# Description: 
# This module sets the parameters for the models, such as production time, post production time and others. 
# There are some additional packages that might need to be install before running this modele. Please see "Additional packages" section

# Use:
# Make sure the packages have been installed successfully
# Press "Ctrl+A" to select all the code and then press "run" or "Ctrl+Enter" to create the water cycle of a virtual mine.
# Depending on the assumptions imposed in the Ready to Reclaim (RTR) period, one should manipulate the following variables:
# NPostPeriods (Line 262)        ### The value must be 10 if NPostPeriodsPitlake is 20 (RTR = 10)
# NPostPeriodsPitLake (Line 263) ### The value must be 10 if NPostPeriods is 20 (RTR = 20)

### Additional packages ###

#install.packages("deSolve")
#install.packages("doParallel")
#install.packages("doRNG")
#install.packages("rootSolve")
#install.packages("lattice")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("forcats")
#install.packages("minqa")
#install.packages("mosek")
#install.packages("pkgbuild")
#install.packages("matrixStats")
#install.packages("reshape2")
# install.packages("jsonlite", type = "source")
# install.packages("devtools")


##### MAKE SURE RMOSEK IS INTALLED #####
# See document "Rmosek Installation guide"


#---------------------------------------

library(Matrix)
suppressWarnings(library(Rmosek))
library(deSolve)
library(doParallel)
library(doRNG)
library(rootSolve)
library(GA)
library(SciViews)
library(minqa)
library(SciViews)
library(rootSolve)
library("dplyr")
library(reshape2)
library(matrixStats)
library(ggplot2)
library(Rmosek)

#getOption("max.print")
#options(max.print = 3000)
#Machine numerical limit GHchanges
epsilon= 1e-8 #.Machine$double.eps

###### Global Parameters ######################
##### Conversion Factors ######
LperM3<-1000
MGperTonne<-1e9
M3perL=1000  # M3 per liter
KGperTonne<-1000
Mm3Yr_to_m3sec= 1e6/(24*60*60*365)     # conversion factor from millions m3 per year to m3 per sec
Mm3_6mo_to_m3sec=1e6/(24*60*60*365*0.5) # conversion for six months

## Discount rate
d_rate= 0.06

###########Discount Factor#####
DiscRate=1/(1+d_rate)
DiscR<-DiscRate^(1:Nperiods)


##### Algorithm Constraint penalties #######
pen_lin=20
pen_quad=20

##### Wage and day assumptions #######
EX_12 = 0.99
PPP_12_CAD = 1.245
Wage_batea = 80   #Batea wage CAD
Wage_source = 48.02 #Source wage USD
Wage_batea = 80   #Batea wage CAD
day_y = 350 # days in a year

##### Expansion factors ####
factors = c(3,3.42,1,1)
factors = c(3,3.42,1) #GHchange does factors have 4 parameters in the previous line.  NOte: I'm concerned with multiplying 3*3.42 Perhaps we need to be careful how to apply these factors and to which costs we apply them to.
factors=c(1,3.42,1)
factors = c(3,1,1) 
###### End Global Parameters ###########


#####################SET UP Water balance and model - These are scalable to Bitumen Production level ######################################    
BitumenProduction=58056400/1e6  # million barrels per year (This corresponds to Muskeg River Mine)
rawOretonnes_perbarrel_bit<-1.6  # see OSPW concentrations.R  

#Production stream numbers
RawOreTonnesPerDay=10604#tonnes per hour  From Oilsands Magazine
RawOreTonnesPerYear=RawOreTonnesPerDay*355*24 # assumed 355 days of production per year
RawOreTonnesPerYear=rawOretonnes_perbarrel_bit*BitumenProduction #million of tonnes per year

# parameters relevant to NAs that end up in OSPW from raw ore
RawOre_NA_Conc=200 #mg/Kg  B. Zhao, R. Currie and H. Mian 2012.   Catalogue of Analytical Methods for Naphthenic Acids Related to Oil Sands Operations. Northern Alberta Institute of Technology. Oilsand Research and Information Network.
RawOre_Bitumen=0.11
RawOre_Water=0.04
RawOre_Minerals=0.85
NA_perFresh_wa=0.5  #mg/L concentration of NAs in river water <1mg/L



######################################################################
# Per unit Water use parameter Setup based on Muskeg River Mine

#Water Balances/flows from the processing plant###################################################
UtilityLosses<-c(0.2,0.2)
CoolingLosses<-c(0.76,0.61)
LossToProduct<-c(0.0,0.0)
UtilityToRecyclePond<-c(1.78,1.7)
OutsideWaterOrFroth<-c(4.43,4.51)  # consider to be recycled water but it's different because in this case it's froth - does not go directly into production->goes into recycle pond
wafresh_toOreProcessUtils<-c(8.78,10.32)
warecycle_toOreProcessUtils<-c(84.79,88.77) # millionM3 Based on Muskeg river mine 2019 - the 4.51 actually comes from the Jackpine Mine processing (it's Froth water but we don't want to model this outside input for now.)
ConnateWaterFromOre<-c(4.81,4.94)  #2019  # 2018 4.81
WaToTailings1<-c(101.5,102)

# Unaccounted for water in the processing facility balance, this is how much net water to/from the MRM Ore Prep faciltity is unaccounted for in 2019 (ASSUMPTION: LETS ASSUME THAT THIS IS THE NET CHANGE IN TAILINGS POND WATER.)
UnAccountedWaterLoss<-wafresh_toOreProcessUtils+warecycle_toOreProcessUtils+ConnateWaterFromOre+OutsideWaterOrFroth-WaToTailings1-UtilityLosses-CoolingLosses-LossToProduct-UtilityToRecyclePond
WaToTailings<-WaToTailings1+UnAccountedWaterLoss  

UtilityWaterTotalInProcess<-UtilityLosses+CoolingLosses+UtilityToRecyclePond  # OUr indication of water going into utilities is measured by what comes out of utilities
TotWaToProcessUtilities<-wafresh_toOreProcessUtils+warecycle_toOreProcessUtils+OutsideWaterOrFroth;  # Water going through the production process bAsed on Muskeg river mine water balance in Million m3 per year 2019
WaToProcess<-TotWaToProcessUtilities-UtilityWaterTotalInProcess  # Water going through the production process bAsed on Muskeg river mine water balance in Million m3 per year 2019
#WaToProcess<-88.77+10.32-0.2-0.61-1.7
FreshWaToUtilities<-UtilityWaterTotalInProcess  # we assume these are equivalent - utilities use fresh water - ASSUMPTION  
FreshWatToOreProcess<-mean(wafresh_toOreProcessUtils-FreshWaToUtilities)
warecycle_OreProcess<-(WaToProcess-FreshWatToOreProcess)
print(c(warecycle_OreProcess,mean(warecycle_toOreProcessUtils)),digits=21)

# compute processor balance from raw data from water balance
ProcessorBalance=wafresh_toOreProcessUtils+warecycle_toOreProcessUtils+OutsideWaterOrFroth+ConnateWaterFromOre-WaToTailings-UtilityToRecyclePond-UtilityLosses-CoolingLosses-LossToProduct
print(c(ProcessorBalance,UnAccountedWaterLoss),digits=21)  #
# compute balance based on means I computed  # note this includes the outside water from froth but in the model we assume it comes from fresh or recycled and that outside water = 0
ProcessorBalanceMean<-mean(WaToProcess+UtilityWaterTotalInProcess+ConnateWaterFromOre-WaToTailings-UtilityToRecyclePond-UtilityLosses-CoolingLosses-LossToProduct)
ProcessorBalanceMean
##########################################################
#      Precip, nat. surface         interception of runoff&   runoff&dewatering     
#      runoff + FFT dewatering      release to environment    to tailings pond       
# 2018        4.81                         1.91               2.24                      
# 2019        6.41*                        3.52               2.89                   
#  * this also includes dewatering - I think the 2018 number also has dewatering in it because the runoff number of 2.24 is less
# I'm going to assume that the amount of FFT dewatering is verys small relative to the runoff and precip
wa_runoff_dewatering<-c(4.16,6.41) #2019 #2018  wa_runoff_dewatering=4.16
wa_estimated_dewatering<-c(0,2.25)  # note 2019 dewatering must be less than 2.89 which is runoff to pond - it cannot be in the 3.52 runoff to the environment. I estimate 2.25 by assuming actual runnoff is the same in 2018 and 2019 - 4.16
wa_cap_runoff_env<-c(1.91,3.52)  #2019  #2018 wa_cap_runoff_env=1.91
wa_runoff_toPond<-wa_runoff_dewatering-wa_estimated_dewatering-wa_cap_runoff_env  #2019 #2.24 runoff in 2018   # this can be controlled to some extent by interception
EstimatedMaxRunoff<-wa_runoff_dewatering-wa_estimated_dewatering

wa_evaporation<-c(1.47,1.4)     #2019 evaporation from ponds
wa_mine_depressurization<-c(0.92,0.53)  #2019   # note this could go to either the tailings pond or into recycling pond. High salt concentration.
wa_FFTPoreSpace<-c(27.69,23.88)   #2019
wa_recycled<-c(75.53,80.14)
wa_Potable_ClearInventory<-c(0.03,0) 
TailingsPondBalance=WaToTailings+wa_Potable_ClearInventory-UnAccountedWaterLoss+wa_runoff_toPond+wa_estimated_dewatering+wa_mine_depressurization-wa_FFTPoreSpace-wa_evaporation-wa_recycled   #2019
101.5+2.24+0.92+0.03-27.69-75.53-1.47

Rate_PoreWater_To_ClearWater<-0.03  # Need to check this parameter # from salt loading model
Maximum_PoreWater_ToPits<-1/3   # comes from Salt loading model.

# the per unit water parameter derived from the above total mine numbers (this is water going into hte process but not connate water)
ConnateWater_perRawOretonne<-mean(ConnateWaterFromOre/RawOreTonnesPerYear)  # m3 water/tonne ore
#prop_fresh_water<-FreshWatToOreProcess/(warecycle_toOreProcessing+wafresh_toOreProcessing)  # the denominator paramters no longer present above so I've commented out - DELETE
wa_per_bitbarrel<-mean(WaToProcess/BitumenProduction) # m3/barrel bitumen
wa_per_RawOretonnes<-mean(WaToProcess/RawOreTonnesPerYear)    # m3/tonne ore
wa_per_bitbarrel/wa_per_RawOretonnes
wa_Utilities_bitbarrel<-mean(UtilityWaterTotalInProcess/BitumenProduction) # water per barrel going from  fresh wate to the Ore prep utlities Based on Muskeg River mine - 1.7mM3 goes to recycle pond, minus 0.2 and 0.61 to utilties losses and cooling losses
Wa_UtilToRec_bitbarrel<-mean(UtilityToRecyclePond/(BitumenProduction)) #  water per barrel diverted back to the recycle pond from the Ore prep utlities Based on Muskeg River mine - 1.7mM3 goes to recycle pond, minus 0.2 and 0.61 to utilties losses and cooling loss
wa_connate_bitbarrel<-mean(ConnateWaterFromOre/BitumenProduction)
wa_depress_bitbarrel<-mean(wa_mine_depressurization)/BitumenProduction # Depressurization Water
wa_OSPW_Eff_bitbarrel<-mean(WaToTailings/BitumenProduction)
wa_ProcOut_bitbarrel<-mean(UtilityLosses+CoolingLosses+LossToProduct)/BitumenProduction
#Ratios 
wa_out_water_in_ratio<-mean((WaToProcess+ConnateWaterFromOre)/WaToProcess)  # water out-to-in ratio is greater than 1 because connate water is added in the process.
WaTailings_ov_Process<-mean(WaToTailings/(WaToProcess+ConnateWaterFromOre))
RecycWater_ov_WaTail<-mean(warecycle_OreProcess/WaToProcess)
wa_OSPW_Eff_bitbarrel/wa_per_bitbarrel

prop_ClearWa_toPoreWa<-mean(1-wa_FFTPoreSpace/WaToTailings)  # my estimate based on Muskeg River mine
wa_poreWa_perBit<-mean(wa_FFTPoreSpace/(BitumenProduction)) #m3 water trapped per bitumen barrel produced
wa_poreWa_perRawOre<-mean(wa_FFTPoreSpace/RawOreTonnesPerYear) #m3 water trapped per raw ore produced
wa_poreWa_perRawOre/wa_per_RawOretonnes
wa_poreWa_perBit/wa_per_bitbarrel

wa_ClearInv_perBit<-mean(WaToTailings-wa_FFTPoreSpace)/(BitumenProduction)
wa_ClearInv_perRawOre<-mean(WaToTailings-wa_FFTPoreSpace)/(RawOreTonnesPerYear)

wa_ClearInv_per_wainput<-mean((WaToTailings-wa_FFTPoreSpace)/WaToTailings)
wa_ClearInv_per_wainput

net_water_runoff_evap<-mean(wa_runoff_toPond+wa_estimated_dewatering-wa_evaporation)  # this is really a decision by the company - we know this because they release some of the captured runoff water into the river. 





##############End SET UP Water balance and model - These are escalable to Bitumen Production level ######################################  

#######################################################################################
# Set up pollution accumulation parameters for mine water flow system
InitPollutants<-c(0,0,0,0,0) #salts, chlorides, NAs, cadmium, ammonia

# Change this in the cost functions
Sa=1
Cl=2
Na=3
Cd=4
Ammonia=5
PolNames<-c("TDS","Cl","Na","Cd","Am")
# Pollutants coming from connate water
Pol_ConnWater_mgL<-c(0,0,0,.0000,0)   #mg/L # Don't know which would be the concentration here (straight from the ore)
Pol_ConnWater_tMm3<-Pol_ConnWater_mgL/MGperTonne*LperM3*1e6  #1e6 m3 per million m3,  units are tonnes per million m3
# Pollutants coming from raw ore via the production process
RawOreNAConc=RawOre_NA_Conc*.07
Pol_Ore_mgKg<-c(500,0.2*500,RawOreNAConc,0.0025,4)   #mg of pollutants coming from the processing of raw ore
Pol_BitProcess_tonnesbarrel<-Pol_Ore_mgKg*KGperTonne*rawOretonnes_perbarrel_bit/MGperTonne  # in tonnes pollution per barrel
Pol_BitProcess_tonnesMbar<-Pol_BitProcess_tonnesbarrel*1e6
# Pollutants coming from mine depressurization water
Pol_DepressWater_mgL<-c(500,0.2*500,0,0.00,0)     #mg/L
# Pollutants coming from river water 170TDS and 6 chlorides is from Allen W 2008. 
Pol_RiverWater_mgL<-c(170,6,0.5,0.00,0)  # calibrated to give an increase in TDS of about 75mg/liter/year from Allen W. 2008 (2000-2500mg/L after)
# chorides syncrude: 75-550 mg/L from 1980-2003 or (540-75)/23=20.22mg/L/yr  Suncor: (80-20)/18=3.333 mg/L/Yr 

Pol_DepressWater_tMm3<-Pol_DepressWater_mgL/MGperTonne*LperM3*1e6
Pol_RiverWater_tMm3<-Pol_RiverWater_mgL/MGperTonne*LperM3*1e6  # tonnes per million m3
Pol_RunoffWater_mgL<-c(0,0,0,0,0) # Small level for TDS? - All at the lower end 
Pol_RunoffWater_tMm3<-Pol_RunoffWater_mgL/MGperTonne*LperM3*1e6  

NADecayRate<-log(1/0.5)/13  # based on a 13 year half-life for all forms of NAs (half-lives are between 12.3-13.6)
NADecayRate<-NADecayRate+0
Pol_DecayRates<-matrix(c(0,0,NADecayRate,0,0),ncol=5);  colnames(Pol_DecayRates)<-c("TDS","Cl","Na","Cd","Am")
######################## Set up pollution accumulation parameters  ######################################


################# Set up Decision Variables and end pit lake parameters ########################################
# Enumerate the decision variables
# set up model size - time horizon
Nperiods=70
NPostPeriods= 10       ### The value must be 10 if NPostPeriodsPitlake is 20 (RTR = 10)
NPostPeriodsPitLake=20 ### The value must be 10 if NPostPeriods is 20 (RTR = 20)
NProdPeriods=Nperiods-NPostPeriods-NPostPeriodsPitLake
NMinePeriods<-NProdPeriods+NPostPeriods
Last_Year_Treatment = Nperiods-NPostPeriodsPitLake # Constraint for when to treat following the regulation period (Ready to Reclaim period) 

#Mine ends at year 40

if (DecisionVariables[3]+DecisionVariables[4]>Last_Year_Treatment) {DecisionsVariables[4]=Last_Year_Treatment-DecisionVariables[4]}
T<-trunc(DecisionVariables[3])+1 
TreaT <- min(trunc(DecisionVariables[3]+DecisionVariables[4]+1),Last_Year_Treatment)

# Creating 
ActiveFFT_Dewatering<-rep(0,Nperiods)
ActiveFFT_Dewatering[1:(Nperiods-NPostPeriods-NPostPeriodsPitLake)]<-rep(wa_estimated_dewatering[2],(Nperiods-NPostPeriods-NPostPeriodsPitLake))
netwater_runoffevap<-rep(net_water_runoff_evap,Nperiods)
netwater_runoffevap<-rep(wa_runoff_toPond[1],Nperiods)

netwater_runoffevap[1:(Nperiods-NPostPeriods-NPostPeriodsPitLake)]<-wa_runoff_toPond[2]
netrunoffevap_plus_activeDewatering<-ActiveFFT_Dewatering+netwater_runoffevap  #Below we further adjust this so that the netrunoff is higher - maximum observed in data we have

# we also see that the company adjusts the water going to the river depending on how much dewatering there is.
# treatment start times govern any treatment 
MaxStartTime=50
ub_WatTreat<-c(rep(20,Nperiods))  #c(rep(20,Nperiods))
ub_WatTreat[(NMinePeriods+1):Nperiods]=0
  

# End pitlake parameters

#PitLakeFlowThrough<-0.1/Mm3Yr_to_m3sec/PitLakeVolume  # Mm3Yr - note we'll treat this as a constraint for now
ub_PitLake<-c(1000,70)
lb_PitLake<-c(0,0)

# Pitlake treatment decisions and constraints
MaxStorageVolume<- 20
MaxStorageVolume_Penalty<-20 # for penalizing or costing additional above ground OSPW storage capacity
 StorVol_LinPen=pen_lin
 StorVol_QuadPen=pen_quad

Pond_reduction_rate<-0.001
MaxPitLakeVolume<- 90 #Mm3 (Doesn't necessarily ends up in the lake) - See if it is a hard constraint
TargetQuantityWaterEndPitLake<-15 # target volume - in the model as a lower bound

PitLakeFlowThrough=2.25    # possibilities: 2.25 is the same as wa_runoff_toPond[1] calculated above  or 4.16=max(EstimatedMaxRunoff) also calculated from above    #netrunoffevap_plus_activeDewatering # Lower bound 
netrunoffevap_plus_activeDewatering[NProdPeriods+1:(NPostPeriods+NPostPeriodsPitLake)]<-PitLakeFlowThrough   # adjust net runoff vector - calculated above so that is matches this PitLakeFlowThrough
PitLakeFlowThrough_m3sec<-PitLakeFlowThrough*Mm3Yr_to_m3sec # How much water that is untreated that is not being used in the system - which will go to a treatment

PitLake_Vol_Stabi_Time = 65 # Pitlake volume stabilization time
MaxProdWaterFromRiver=20
PitLake_Pond_Constraints<-c(MaxPitLakeVolume,TargetQuantityWaterEndPitLake,PitLakeFlowThrough,MaxStorageVolume,PitLake_Vol_Stabi_Time,MaxProdWaterFromRiver,Pond_reduction_rate)
PL_Treatment=0  # =0 no treatment (just flowthrough), 1=wetland, 2=MBR_GAC #to override continuous variable in the algorithm

#Set up final Decision variable vectors and names
#GHChanges
DecisionVariables=c(0,0,0,0,0,0,0,0)  
Var_Names<-c(paste0(c("Inflow_S","Inflow_W","STreat","Dtreat","End_WTT","Wet_Tdays","NA_Target","Technology")))


#1 Inflow_S = OSPW inflow in Summer (Mm3) [,10] Mm3
#2 Inflow_W = OSPW inflow in Winter (Mm3) [,10] Mm3
#3 STreat = Start treatment time (Year) [,50] year
#4 DTreat = Duration of treatment (Years) [,60] year
#5 End_WTT = End Wetland treatment time in day for endpitlake days
#6 Wet_Tdays = Wetland treatment days (days)  [,80] days
#7 NA_Target = End of pipe NAs target (Target) [,G_a]
#8 Technology = 0-1 OZ, 1-2 RO, 2-3 WL_BC


################################## End Set up Decision Variables   ################################################



################### Set up Effluent output limits and parameters  ############################
RiverConc_mgL<-c(170,6,0.5,0.00,0.00)   #mg/L or ug/L ?  TDS,Chloride,NAs,Cadmium, ammonia
RunoffConc_mgL<-c(0,0,0,0,0)            #mg/L # There could be many assumptions here
# G_a<-c(3000,860,10,0.0023,5.6)         # mg/L Acute guidelines (END of PIPE) in m3/L Sources: Chloride from Loading model by Tammy, and the others TDS Canadian guidelines, Cadmium from Tammy, NA don't know 
# G_c<-c(1000,250,2,0.00009,1.54)          # mg/L  Chronic guidelines 
# G_a<-c(3000,860,5,0.1,20)         # mg/L Acute guidelines (END of PIPE) in m3/L Sources: Chloride from Loading model by Tammy, and the others TDS Canadian guidelines, Cadmium from Tammy, NA don't know 
# G_c<-c(1000,250,1,0.05,10)          # mg/L  Chronic guidelines 
# G_a<-c(3000,860,10,0.1,20)         # mg/L Acute guidelines (END of PIPE) in m3/L Sources: Chloride from Loading model by Tammy, and the others TDS Canadian guidelines, Cadmium from Tammy, NA don't know 
# G_c<-c(1000,250,2,0.05,10)          # mg/L  Chronic guidelines 

G_c<-c(2500,250,2,0.05,15) #defaults targets ## I change the TDS from 1000 to 2500 pending questioning Mohamed as these were for agriculture and for OSPW they might vary since they not consider TDS a problem
G_a<-c(3000,860,30,0.1,20) #defaults initial

#G_a<-c(3000,860,20,0.01,5)         # mg/L Acute guidelines (END of PIPE) in m3/L Sources: Chloride from Loading model by Tammy, and the others TDS Canadian guidelines, Cadmium from Tammy, NA don't know 
#G_c<-c(500,230,10,0.004,1)  
ConstrainWeights<-matrix(rep(c(1,10,100,100000,100),Nperiods),byrow=TRUE,ncol=5)  # for effluents
WetlandConstraintWeight<-10
Q_s_W=100   # m3/s (instream flow) winter  (ice covered)
Q_s_S=289   # m3/s (instream flow) summer  (ice free/open water)
ff=c(0.1,0.1,0.1,0.1,0.1) # factor mixing
CV=.6
n_a=1
n_c=4 # (Sample size)
S_Var=log((CV^2/n_c)+1)
Var=log((CV^2)+1)
z_95=1.642
z_98=2.054
z_99=2.236
# adjusted G_a for probability
G_a<-c(3000,860,20,0.1,20)    
G_a=LTA_c(G_a,Var,z_99)

#Be careful not call G_a somewhere else by itself (only call it once)
####################End Set up Effluent output limits and parameters ##############################



####### Set up the RMOSEK problem for the mine water flows  ###############
# Call this function to set up the initial water flow problem to be called inside waterCostModel
WaterTreatVec = c(rep(0,10),rep(9,50),rep(0,10))

Mine_Results<-CallMineWaterSystemFn(Nperiods=Nperiods,NPostPeriods=NPostPeriods,waterTreatmentVector=WaterTreatVec,ub_WatTreat=ub_WatTreat,PitLake_Pond_Constraints=PitLake_Pond_Constraints,ObjectFnCoefs=c(400,-0.03156905,-1,2.5,100,-0.03156905)) #from -5 to -0.01
conic_OSPW_prob<-Mine_Results$conic_OSPW_prob
ConsRowNames<-rownames(Mine_Results$AMatbc$AMat)
rownames(conic_OSPW_prob$A)
# Mine_Results2<-WaterSystemFn2(conic_OSPW_prob=conic_OSPW_prob,waterTreatmentVector=WaterTreatVec) # check it

# Constraint by limiting the tailing ponds capacity needed (Look it up)
# Consider the chronic and accute for TDS

VarNames<-Mine_Results$VarNames

VarNames[(Nperiods*6+1):(Nperiods*7)] #ft
conic_OSPW_prob$bx[,(Nperiods*6+1):(Nperiods*7)]
conic_OSPW_prob$c

Mine_Results$PondWater
Mine_Results$message
Mine_Results$ObjFn
Mine_Results$solsta
Mine_Results$prosta
Mine_Results$BitumenProduction
Mine_Results$freshWaterRiver
Mine_Results$WaterRecycledFromPond
Mine_Results$PitLakeOutflow
Mine_Results$freshWaterToPond
Mine_Results$FullLinMatrixWithSoln
t(noquote(rbind(VarNames,Mine_Results$conic_OSPW_prob$bx)))
noquote(cbind(VarNames[1:(70*5)],Mine_Results$FullSoln[1:(70*5)]),) #actual numbers in the solution
noquote(cbind(VarNames[(70*5):(70*10)],Mine_Results$FullSoln[(70*5):(70*10)]))

posobjfunction <- which(conic_OSPW_prob$c!=0) #creates an index
noquote(cbind(VarNames[posobjfunction],conic_OSPW_prob$c[posobjfunction])) #See costs


length(Mine_Results$FullLinMatrixWithSoln[1,])

Mine_Results$FullLinMatrixWithSoln 

#printSpMatrix(t(Mine_Results$FullLinMatrixWithSoln[954:957,Nperiods*0+1:(Nperiods)]),col.names=TRUE,digits=3) #limits and solution for parts blx bux solution
#printSpMatrix(Mine_Results$FullLinMatrixWithSoln[954:957,Nperiods*11+1:(Nperiods)],col.names=TRUE,digits=3) #Print bits of the matrix
#ColumnSet<-c(1:1892)
#printSpMatrix(rbind(Mine_Results$FullLinMatrixWithSoln[373:393,ColumnSet],
 #             Mine_Results$FullLinMatrixWithSoln[954:957,ColumnSet]),col.names=TRUE,digits=3)

#conic_OSPW_prob$bx[1:2,1:Nperiods] #Get the bounds for each variable (Water Pond)

###### End set up the RMOSEK problem #################################

Neg_WaterCostModel<-function(DecisionVariables) { #negative function for WCM (minimize a negative cost)
  return(-WaterCostModel2(DecisionVariables,optimiz=TRUE))
}
Call_WaterCostModel<-function(DecisionVariables) {
  return(WaterCostModel2(DecisionVariables,optimiz=TRUE,FixedStartTime=NULL))
}

###########################################################
## SET UP THIS FIRST before calling Water Cost Model
## Should have no problem running Water Cost Model 2
###########################################################

# Then go through WaterCostModel 2 line by line to see that it runs properly, to then (when everything is ok) run it in here.


###### First call to Water Cost Model with intial solution for testing purposes   ##########
#WCR<-WaterCostModel(DecisionVariables,optimiz=FALSE)
DecisionVariables<-c(6,3,20,40,10,10,1,1) #
WCR<-WaterCostModel2(DecisionVariables,optimiz=FALSE)
warnings()


##################### End of module 3  ######


