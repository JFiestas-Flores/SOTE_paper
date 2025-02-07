# Module 4 - Estimations For Study 
### MAIN ASSUMPTION: NPostPeriods = 20 (RTR == 20)


# Description:  
# This module estimates the costs for the selected technology and the assumption in the RTR periods
# The model only estimates the cost for each technology separately and then provides a set of results that should be stored in a directory
# This module should only be run after module 1, 2 and 3 were succesfully implemented.
# The module first estimates the a "No technology" or "Natural treatment" as a benchmark
# Then it presents the estimations for each of the different technologies:  Ozonation (OZ), Reverse Osmosis (RO) and Wetlands and Biochar (WL_BC)

# Default assumptions on Ready to Reclaim (RTR) period 
# NPostPeriods = 20       
# NPostPeriodsPitLake=10  

# Post estimation
# Once the estimations are done, the user should have one r file (.rData) for each treatment. Inside each file the user
# will find the objects containing the following results:

# OptDecVariables_ : Decision variables obtained after the iterations and that produce the ultimate objective functions
# ObjFunction_ : Treatment cost
# WCR_ : Water cycle
# penalty_ : Penaly value. If the value is higher than 0.05 then the objective function should be rejected,

# The number of objects are estimated for each target. That is, if the user estimates the costs for n targets, they will find nx4 objects in the file 

# Use:
# You can select all the file ("Ctrl+A") and run it ("Ctrl + Enter") after making sure all the 


# Set directory to store results
setwd ("C:/Model_Results") # This is just an example, one should set them to a know directory.
suppressWarnings(library(GA))

# In the saving files the first number stands for target and the second for end of treatment time

#---------------------------------------

##### Parameters for No technology ("Natural treatment") #####

# Set the number of iterations per run (maxiter)
MAX = 20 # The runs used in the Paper considers 20 iterations in this section

############ Parameter that can be manipulated ##############
# The current values are the ones used in the Paper

# Max production from River
Pond_reduction_rate=0.05  # (rratio)
MaxProdWaterFromRiver=20 # Maximum water extracted from the river
MaxStorageVolume = 25 # Change it to 25
MaxPitLakeVolume = 120 # Should be a
TargetQuantityWaterEndPitLake= 100 # 15 This changes then you can have more water to the pitlake
MaxProdWaterFromRiver = 20 # 

# Intereste rate
d_rate= 0.06

# Wetland parameter
wetland_t = 80 # or 0 for no treatment scenario

# End pitlake constraints
PitLake_Pond_Constraints<-c(MaxPitLakeVolume,TargetQuantityWaterEndPitLake,PitLakeFlowThrough,MaxStorageVolume,PitLake_Vol_Stabi_Time,MaxProdWaterFromRiver,Pond_reduction_rate)

# Mine results parameter
Mine_Results<-CallMineWaterSystemFn(Nperiods=Nperiods,NPostPeriods=NPostPeriods,waterTreatmentVector=WaterTreatVec,ub_WatTreat=ub_WatTreat,PitLake_Pond_Constraints=PitLake_Pond_Constraints,
                                    ObjectFnCoefs=c(3500,-0.03156905,-1,2.5,100,-0.03156905)) 
conic_OSPW_prob<-Mine_Results$conic_OSPW_prob
ConsRowNames<-rownames(Mine_Results$AMatbc$AMat)


## Pollution limits
targets <- c(20) # Assumed to the "Natural treatment" so the model has no constraints

#TDS Targets
TDS_target = 0
target_tds= c(2500)

# Penalty factors
p_factor <- 40 # The penalty factos is applied when the technologies cannot reach the pollution targets. This will be reflected in the costs


# No treatment (NT) iteration ##############################

tb_up=1 # upper tech bound
tb_lw=0.01

for (j in targets) {
  NAtarget=j 
  
  for (i in target_tds) {
    TDS_target=i
    mod_factor = p_factor # High penalty for all scenarios
    pen_lin=400*mod_factor
    pen_quad=2000 
    
    {
      upbound=c(20,20,50,50,wetland_t,80,NAtarget,tb_up)
      lwbound=c(0,0,0,49.9,0,0,0,tb_lw)
      
      #Chronic and Accute Limits
      
      G_a=c(TDS_target,860,NAtarget,10,50)
      G_c=c(500,250,0.95,0.05,15.00) #0.75 changed to 1, 0.95
      
      Startpop = 50 # Number for the starting population
      StartPopulation<-matrix(0,ncol=8,nrow=Startpop) #nrow was 100
      StartPopulation[1,]=c(5,5,35,50,10,10,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[2,]=c(18.9,0,31,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[3,]=c(9,9,35,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[4,]=c(0,0,49,50,50,50,NAtarget,runif(1,tb_lw,tb_up))
      StartPopulation[5,]=c(6,3,20,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[6,]=c(3,2,35,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[7,]=c(3,2,10,50,0,10,NAtarget,runif(1,tb_lw,tb_up))
      
      # Reset the size for it to work To run it faster
      StartPopulation[8:Startpop,1]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,2]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,3]<-runif(Startpop-7,5,50)
      StartPopulation[8:Startpop,4]<-rep(50,Startpop-7)
      StartPopulation[8:Startpop,5]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,6]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,7]<-rep(NAtarget,Startpop-7)
      StartPopulation[8:Startpop,8]<-runif(Startpop-7,tb_lw,tb_up)
      set.seed(20211121)
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,  suggestions=StartPopulation, #Take this out for wetland =0 
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      StartPopulation[1,]<-DecisionVariables
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound, suggestions=StartPopulation, #Take this out for wetland =0 
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      WCR<-WaterCostModel2(DecisionVariables,optimiz=FALSE)
      
      #We can use this to substract to any other technologies to obtain the marginal cost of the technology (vs not treating anything)
      
      penalty_ratio= sum(WCR$Effluent$SummerPenalty_EffLim+WCR$Effluent$WinterPenalty_EffLims+
                           WCR$Effluent$Summer_acute_Penalty_EffLims+WCR$Effluent$Winter_acute_Penalty_EffLims+
                           WCR$PitLake$Penalty_EffLims+WCR$PitLake$acute_Penalty_EffLims+WCR$MineResults$Bit_cost)/WCR$ObjectiveFunction
    }
    
    assign(paste0("OptDecVariables_NATarget",j,"_TDS",i,"_NT_Post20"),DecisionVariables)
    assign(paste0("WCR_NT",j,i,"_Post20"),WCR)
    assign(paste0("ObjFunction_NATarget",j,"_TDS",i,"_NT_Post20"),WCR$ObjectiveFunction)
    assign(paste0("penalty_ratio_NT",j,i,"_Post20"),penalty_ratio)
  }
}


# Saving results
{
  # When RTR is 10 years
  save (
    OptDecVariables_NATarget20_TDS2500_NT_Post20, #_NOwet #Med_wet
    ObjFunction_NATarget20_TDS2500_NT_Post20, #_NOwet #Med_wet
    WCR_NT202500_Post20, #_NOwet #Med_wet
    penalty_ratio_NT202500_Post20, #_NOwet #Med_wet
    
    file="Results NT RTR 20 rratio 0.05.rData") # Name for the data. It indicates:
  # "Type of result, No Technology (NT), RTR period (10 years) and the reduction of water in the pond per year (0.05)
  
}




###############################################################

# Parameters for treatment technologies  ##############################
# Iterations
MAX = 100 # The Paper uses 100 iterations. We changed it here to reduce the running times

# NAs targets
targets <- c(10,5,3,4,2,1) 

# TDS targets
target_tds= c(2500) # 2000, 2500


# Ozonation (OZ) iteration  ##############################
tb_up=2 # upper tech bound
tb_lw=1.01

for (j in targets) {
  NAtarget=j 
  
  
  for (i in target_tds) {
    TDS_target=i
    mod_factor = p_factor # High penalty for all scenarios
    pen_lin=400*mod_factor
    pen_quad=2000 
    
    {
      upbound=c(20,20,50,50,80,80,NAtarget,tb_up)
      lwbound=c(0,0,0,49.9,0,0,0,tb_lw)
      
      #Chronic and Accute Limits
      
      G_a=c(TDS_target,860,NAtarget,10,50)
      G_c=c(500,250,0.95,0.05,15.00) #0.75 changed to 1, 0.95
      
      Startpop = 50 # Number for the starting population
      StartPopulation<-matrix(0,ncol=8,nrow=Startpop) #nrow was 100
      StartPopulation[1,]=c(5,5,35,50,10,10,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[2,]=c(18.9,0,31,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[3,]=c(9,9,35,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[4,]=c(0,0,49,50,50,50,NAtarget,runif(1,tb_lw,tb_up))
      StartPopulation[5,]=c(6,3,20,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[6,]=c(3,2,35,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[7,]=c(3,2,10,50,0,10,NAtarget,runif(1,tb_lw,tb_up))
      
      # Reset the size for it to work To run it faster
      StartPopulation[8:Startpop,1]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,2]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,3]<-runif(Startpop-7,5,50)
      StartPopulation[8:Startpop,4]<-rep(50,Startpop-7)
      StartPopulation[8:Startpop,5]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,6]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,7]<-rep(NAtarget,Startpop-7)
      StartPopulation[8:Startpop,8]<-runif(Startpop-7,tb_lw,tb_up)
      set.seed(20211121)
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      StartPopulation[1,]<-DecisionVariables
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      WCR<-WaterCostModel2(DecisionVariables,optimiz=FALSE)
      
      
      #WCR$ObjectiveFunction
      
      #We can use this to substract to any other technologies to obtain the marginal cost of the technology (vs not treating anything)
      
      penalty_ratio= sum(WCR$Effluent$SummerPenalty_EffLim+WCR$Effluent$WinterPenalty_EffLims+
                           WCR$Effluent$Summer_acute_Penalty_EffLims+WCR$Effluent$Winter_acute_Penalty_EffLims+
                           WCR$PitLake$Penalty_EffLims+WCR$PitLake$acute_Penalty_EffLims+WCR$MineResults$Bit_cost)/WCR$ObjectiveFunction
    }
    
    assign(paste0("OptDecVariables_NATarget",j,"_TDS",i,"_OZ_Post20"),DecisionVariables)
    assign(paste0("WCR_OZ",j,i,"_Post20"),WCR)
    assign(paste0("ObjFunction_NATarget",j,"_TDS",i,"_OZ_Post20"),WCR$ObjectiveFunction)
    assign(paste0("penalty_ratio_OZ",j,i,"_Post20"),penalty_ratio)
    
  }
}

# Saving: The amount of objects will depend in the amount of targets
{
  save (
    ### Save for target = 10
    OptDecVariables_NATarget10_TDS2500_OZ_Post20,
    ObjFunction_NATarget10_TDS2500_OZ_Post20,
    WCR_OZ102500_Post20,
    penalty_ratio_OZ102500_Post20,
    
    ### Save for target = 5
    OptDecVariables_NATarget5_TDS2500_OZ_Post20,
    ObjFunction_NATarget5_TDS2500_OZ_Post20,
    WCR_OZ52500_Post20,
    penalty_ratio_OZ52500_Post20,
    
    ### Save for target = 4
    OptDecVariables_NATarget4_TDS2500_OZ_Post20,
    ObjFunction_NATarget4_TDS2500_OZ_Post20,
    WCR_OZ42500_Post20,
    penalty_ratio_OZ42500_Post20,
    
    ### Save for target = 3
    OptDecVariables_NATarget3_TDS2500_OZ_Post20,
    ObjFunction_NATarget3_TDS2500_OZ_Post20,
    WCR_OZ32500_Post20,
    penalty_ratio_OZ32500_Post20,
    
    ### Save for target = 2
    OptDecVariables_NATarget2_TDS2500_OZ_Post20,
    ObjFunction_NATarget2_TDS2500_OZ_Post20,
    WCR_OZ22500_Post20,
    penalty_ratio_OZ22500_Post20,
    
    ### Save for target = 1
    OptDecVariables_NATarget1_TDS2500_OZ_Post20,
    ObjFunction_NATarget1_TDS2500_OZ_Post20,
    WCR_OZ12500_Post20,
    penalty_ratio_OZ12500_Post20,
    
    file="Results OZ RTR 20 rratio 0.05.rData")
  # "Type of result, Ozonation (OZ), RTR period (10 years) and the reduction of water in the pond per year (0.05)
}

##############################################################

# Reverse Osmosis (RO) iteration  ##############################
tb_up=4 # upper tech bound
tb_lw=3.01

for (j in targets) {
  NAtarget=j  
  
  for (i in target_tds) {
    TDS_target=i
    mod_factor = p_factor # High penalty for all scenarios
    pen_lin=400*mod_factor
    pen_quad=2000 
    
    {
      upbound=c(20,20,50,50,80,80,NAtarget,tb_up)
      lwbound=c(0,0,0,49.9,0,0,0,tb_lw)
      
      #Chronic and Accute Limits
      
      G_a=c(TDS_target,860,NAtarget,10,50)
      G_c=c(500,250,0.95,0.05,15.00) #0.75 changed to 1, 0.95
      
      Startpop = 50 # Number for the starting population
      StartPopulation<-matrix(0,ncol=8,nrow=Startpop) #nrow was 100
      StartPopulation[1,]=c(5,5,35,50,10,10,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[2,]=c(18.9,0,31,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[3,]=c(9,9,35,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[4,]=c(0,0,49,50,50,50,NAtarget,runif(1,tb_lw,tb_up))
      StartPopulation[5,]=c(6,3,20,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[6,]=c(3,2,35,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[7,]=c(3,2,10,50,0,10,NAtarget,runif(1,tb_lw,tb_up))
      
      # Reset the size for it to work To run it faster
      StartPopulation[8:Startpop,1]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,2]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,3]<-runif(Startpop-7,5,50)
      StartPopulation[8:Startpop,4]<-rep(50,Startpop-7)
      StartPopulation[8:Startpop,5]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,6]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,7]<-rep(NAtarget,Startpop-7)
      StartPopulation[8:Startpop,8]<-runif(Startpop-7,tb_lw,tb_up)
      set.seed(20211121)
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      StartPopulation[1,]<-DecisionVariables
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      WCR<-WaterCostModel2(DecisionVariables,optimiz=FALSE)
      
      #WCR$ObjectiveFunction
      
      #We can use this to substract to any other technologies to obtain the marginal cost of the technology (vs not treating anything)
      
      penalty_ratio= sum(WCR$Effluent$SummerPenalty_EffLim+WCR$Effluent$WinterPenalty_EffLims+
                           WCR$Effluent$Summer_acute_Penalty_EffLims+WCR$Effluent$Winter_acute_Penalty_EffLims+
                           WCR$PitLake$Penalty_EffLims+WCR$PitLake$acute_Penalty_EffLims+WCR$MineResults$Bit_cost)/WCR$ObjectiveFunction
    }
    
    assign(paste0("OptDecVariables_NATarget",j,"_TDS",i,"_RO_Post20"),DecisionVariables)
    assign(paste0("WCR_RO",j,i,"_Post20"),WCR)
    assign(paste0("ObjFunction_NATarget",j,"_TDS",i,"_RO_Post20"),WCR$ObjectiveFunction)
    assign(paste0("penalty_ratio_RO",j,i,"_Post20"),penalty_ratio)
    
  }
}

# Saving: The amount of objects will depend in the amount of targets
{
  save (
    ### Save for target = 10
    OptDecVariables_NATarget10_TDS2500_RO_Post20,
    ObjFunction_NATarget10_TDS2500_RO_Post20,
    WCR_RO102500_Post20,
    penalty_ratio_RO102500_Post20,
    
    ### Save for target = 5
    OptDecVariables_NATarget5_TDS2500_RO_Post20,
    ObjFunction_NATarget5_TDS2500_RO_Post20,
    WCR_RO52500_Post20,
    penalty_ratio_RO52500_Post20,
    
    ### Save for target = 4
    OptDecVariables_NATarget4_TDS2500_RO_Post20,
    ObjFunction_NATarget4_TDS2500_RO_Post20,
    WCR_RO42500_Post20,
    penalty_ratio_RO42500_Post20,
    
    ### Save for target = 3
    OptDecVariables_NATarget3_TDS2500_RO_Post20,
    ObjFunction_NATarget3_TDS2500_RO_Post20,
    WCR_RO32500_Post20,
    penalty_ratio_RO32500_Post20,
    
    ### Save for target = 2
    OptDecVariables_NATarget2_TDS2500_RO_Post20,
    ObjFunction_NATarget2_TDS2500_RO_Post20,
    WCR_RO22500_Post20,
    penalty_ratio_RO22500_Post20,
    
    ### Save for target = 1
    OptDecVariables_NATarget1_TDS2500_RO_Post20,
    ObjFunction_NATarget1_TDS2500_RO_Post20,
    WCR_RO12500_Post20,
    penalty_ratio_RO12500_Post20,
    
    file="Results RO RTR 20 rratio 0.05.rData")
  # "Type of result, Reverse Osmosis (RO), RTR period (10 years) and the reduction of water in the pond per year (0.05)
  
}

##############################################################

# Wetland and Biochar (WL_BC) iterations ###############################
tb_up=3 # upper tech bound
tb_lw=2.01

for (j in targets) {
  NAtarget=j 
  
  for (i in target_tds) {
    TDS_target=i
    mod_factor = p_factor # High penalty for all scenarios
    pen_lin=400*mod_factor
    pen_quad=2000 
    
    {
      upbound=c(20,20,50,50,80,80,NAtarget,tb_up)
      lwbound=c(0,0,0,49.9,0,0,0,tb_lw)
      
      #Chronic and Accute Limits
      
      G_a=c(TDS_target,860,NAtarget,10,50)
      G_c=c(500,250,0.95,0.05,15.00) #0.75 changed to 1, 0.95
      
      Startpop = 50 # Number for the starting population
      StartPopulation<-matrix(0,ncol=8,nrow=Startpop) #nrow was 100
      StartPopulation[1,]=c(5,5,35,50,10,10,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[2,]=c(18.9,0,31,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[3,]=c(9,9,35,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[4,]=c(0,0,49,50,50,50,NAtarget,runif(1,tb_lw,tb_up))
      StartPopulation[5,]=c(6,3,20,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[6,]=c(3,2,35,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[7,]=c(3,2,10,50,0,10,NAtarget,runif(1,tb_lw,tb_up))
      
      # Reset the size for it to work To run it faster
      StartPopulation[8:Startpop,1]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,2]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,3]<-runif(Startpop-7,5,50)
      StartPopulation[8:Startpop,4]<-rep(50,Startpop-7)
      StartPopulation[8:Startpop,5]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,6]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,7]<-rep(NAtarget,Startpop-7)
      StartPopulation[8:Startpop,8]<-runif(Startpop-7,tb_lw,tb_up)
      set.seed(20211121)
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      StartPopulation[1,]<-DecisionVariables
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      WCR<-WaterCostModel2(DecisionVariables,optimiz=FALSE)
      
      #WCR$ObjectiveFunction
      
      #We can use this to substract to any other technologies to obtain the marginal cost of the technology (vs not treating anything)
      
      penalty_ratio= sum(WCR$Effluent$SummerPenalty_EffLim+WCR$Effluent$WinterPenalty_EffLims+
                           WCR$Effluent$Summer_acute_Penalty_EffLims+WCR$Effluent$Winter_acute_Penalty_EffLims+
                           WCR$PitLake$Penalty_EffLims+WCR$PitLake$acute_Penalty_EffLims+WCR$MineResults$Bit_cost)/WCR$ObjectiveFunction
      
      
    }
    
    assign(paste0("OptDecVariables_NATarget",j,"_TDS",i,"_WL_BC_Post20"),DecisionVariables)
    assign(paste0("WCR_WL_BC",j,i,"_Post20"),WCR)
    assign(paste0("ObjFunction_NATarget",j,"_TDS",i,"_WL_BC_Post20"),WCR$ObjectiveFunction)
    assign(paste0("penalty_ratio_WL_BC",j,i,"_Post20"),penalty_ratio)
    
  }
}


# Saving: The amount of objects will depend in the amount of targets
{
  save (
    ### Save for target = 10
    OptDecVariables_NATarget10_TDS2500_WL_BC_Post20,
    ObjFunction_NATarget10_TDS2500_WL_BC_Post20,
    WCR_WL_BC102500_Post20,
    penalty_ratio_WL_BC102500_Post20,
    
    ### Save for target = 5    
    OptDecVariables_NATarget5_TDS2500_WL_BC_Post20,
    ObjFunction_NATarget5_TDS2500_WL_BC_Post20,
    WCR_WL_BC52500_Post20,
    penalty_ratio_WL_BC52500_Post20,
    
    ### Save for target = 4
    OptDecVariables_NATarget4_TDS2500_WL_BC_Post20,
    ObjFunction_NATarget4_TDS2500_WL_BC_Post20,
    WCR_WL_BC42500_Post20,
    penalty_ratio_WL_BC42500_Post20,
    
    ### Save for target = 3
    OptDecVariables_NATarget3_TDS2500_WL_BC_Post20,
    ObjFunction_NATarget3_TDS2500_WL_BC_Post20,
    WCR_WL_BC32500_Post20,
    penalty_ratio_WL_BC32500_Post20,
    
    ### Save for target = 2
    OptDecVariables_NATarget2_TDS2500_WL_BC_Post20,
    ObjFunction_NATarget2_TDS2500_WL_BC_Post20,
    WCR_WL_BC22500_Post20,
    penalty_ratio_WL_BC22500_Post20,
    
    ### Save for target = 1
    OptDecVariables_NATarget1_TDS2500_WL_BC_Post20,
    ObjFunction_NATarget1_TDS2500_WL_BC_Post20,
    WCR_WL_BC12500_Post20,
    penalty_ratio_WL_BC12500_Post20,
    
    file="Results WL_BC _Post20 rratio 0.05.rData")
  # "Type of result, Wetland Biochar (WL_BC), RTR period (10 years) and the reduction of water in the pond per year (0.05)
  
}


##################### End of module 4 - NPostPeriods = 20 (RTR == 20)  ######