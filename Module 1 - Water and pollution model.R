# Module 1 - Water and pollution model

# Description: 
# This module models the water cycle for a virtual oil sands mine in Northern Alberta by considering the changes in the water
# balance during production and post production periods. Additionally, it calculates the changes in pollutants concentration.
# At the moment it only considers changes in Naphtenics Acids (NAs) and Total Dissolved Solids (TDS), but is capable to
# also incorporate changes in Aluminium, Chloride and Cadmium.


# Use:
# Press "Ctrl+A" to select all the code and then press "run" or "Ctrl+Enter" to create the water cycle of a virtual mine.
# There is no need for further action in this module

#---------------------------------------

#rm(list=ls()) # Uncomment this line in case you need to restart the whole process.

CreateVarNames<-function(Nperiods) {
  # create Variable Names
  #VarNames<-rep("",(Nperiods*NVariTypes+NPostPeriods*NVariTypesPostProd+4+NPeriods))
  NVariTypes=27
  VarNames<-rep("",(Nperiods*NVariTypes))
  for(i in 1:Nperiods) {
    # first set of variables focus on the water stock and flows from and to the tailings pond
    # Column set 1 Stock: Total Clear Water in POnd
    VarNames[i]=paste0("W",i)
    # Column Set 2 Stock: Total water trapped in pore water FFT
    VarNames[Nperiods+i]=paste0("Wp",i) 
    # Column set 3 recycled water to recycled water pond from the clear part of the tailings pond
    VarNames[Nperiods*2+i]=paste0("wr",i)
    # Column set 4 OSPW water from the bitumen production process water to the clear water portion of the tailings pond
    VarNames[Nperiods*3+i]=paste0("pc",i)
    # column set 5 OSPW water trapped in pore water from the bitumen production process water - trapped in FFT
    VarNames[Nperiods*4+i]=paste0("pf",i)
    # Column set 6 water flowing from FFT to clear water in the tailings pond 
    VarNames[Nperiods*5+i]=paste0("fc",i)
    # Column set 7 active treatment of FFT to extract water from the flow or stock of FFT
    VarNames[Nperiods*6+i]=paste0("ft",i)
    # Column set 8 mine depressurization water flowing into the tailings pond
    VarNames[Nperiods*7+i]=paste0("dc",i)
    # column set 9 net evaporation and runoff water 
    VarNames[Nperiods*8+i]=paste0("er",i)
    # Column set 10 water treated from tailings pond - early treatment before closure!
    VarNames[Nperiods*9+i]=paste0("wt",i)
    # Column set 11 water returned to tailings pond after treatment.
    VarNames[Nperiods*10+i]=paste0("tw",i)
    # Column set 12 water to the environment via the pit lake to the environment (could also go to treament) -constrained to zero
    VarNames[Nperiods*11+i]=paste0("te",i)
    # column set 13 water from river to the pond ->> this is really for post production dilution and pit lake
    VarNames[Nperiods*12+i]=paste0("fw",i)
    # 2nd set of variables focus on the water stock and flows from and to the raw water pond
    # Column set 14 Stock: Total water in raw water pond
    VarNames[Nperiods*13+i]=paste0("Wf",i)
    # Column set 15 river water drawn from the river
    VarNames[Nperiods*14+i]=paste0("wf",i)
    # Column set 16 water from raw fresh water pond to bitumen process
    VarNames[Nperiods*15+i]=paste0("fp",i)
    # Column set 17 Total water demand (note this should be just a constant most of the time because it's precalculated)
    VarNames[Nperiods*16+i]=paste0("wd",i)
    #Column set 17 connate water from the bitumen process
    VarNames[Nperiods*17+i]=paste0("co",i)
    # Column set 17 water from raw fresh water pond to the recycle pond
    VarNames[Nperiods*18+i]=paste0("fr",i)
    # column set 18 water from raW fresh water pond to the utilities
    VarNames[Nperiods*19+i]=paste0("fu",i)
    # 3rd set of variables focus on the water stock and flows from and to recycled water pond
    # Column set 19 Stock: Total water in recycled water pond
    VarNames[Nperiods*20+i]=paste0("Wr",i)
    # Flows - above we defined 2 flows into the recycled water pond -> wr from the tailings pond, and fr from the fresh water pond 
    # Column set 20 water from recycle pond to production
    VarNames[Nperiods*21+i]=paste0("rp",i)
    # column set 21 water from utilities to recycle pond
    VarNames[Nperiods*22+i]=paste0("ur",i)
    # column set 22 water from outside to the recycle pond (usually froth from another production facility)
    VarNames[Nperiods*23+i]=paste0("or",i)
    # 4th set of variables are investment variables and bitumen production demand
    # column set 23 investment in tailings pond 
    VarNames[Nperiods*24+i]=paste0("Iw",i)
    # column set 24 bitument production
    VarNames[Nperiods*25+i]=paste0("Bp",i)
    # column set 25 cost of recycling water in production
    VarNames[Nperiods*26+i]=paste0("Cr",i) # Cost of recycling water in production
    
  }

  # Additional Variables = put here temporarily before integrating into above so I don't have to change matrix setup for other variables and constraints
  #for(i in 1:NPostPeriods) {
    # stock variable for water trapped in FFT pore water
  #  VarNames[NProdPeriodVars+NPostPeriods*21+4+i]=paste0("Wp",i) 
    
  #}  
  return(VarNames)
} 
CreateVarNames2<-function(Nperiods) {
  # create Variable Names
  #VarNames<-rep("",(Nperiods*NVariTypes+NPostPeriods*NVariTypesPostProd+4+NPeriods))
  VarNames<-rep("",(Nperiods*7))
  for(i in 1:Nperiods) {
    # first set of variables focus on the water stock and flows from and to the tailings pond
    # Column set 1 Stock: Total Clear Water in POnd
    VarNames[i]=paste0("W",i)
    # Column Set 2 Stock: Total water trapped in pore water FFT
    VarNames[Nperiods+i]=paste0("Wp",i) 
    # Column set 3 recycled water to recycled water pond from the clear part of the tailings pond
    VarNames[Nperiods*2+i]=paste0("wr",i)
    # Column set 4 OSPW water from the bitumen production process water to the clear water portion of the tailings pond
    VarNames[Nperiods*3+i]=paste0("pc",i)
    # Column set 8 mine depressurization water flowing into the tailings pond
    VarNames[Nperiods*4+i]=paste0("dc",i)
    # column set 9 net evaporation and runoff water 
    VarNames[Nperiods*5+i]=paste0("er",i)
    # Column set 10 water treated from tailings pond
    VarNames[Nperiods*6+i]=paste0("wt",i)
    return(VarNames)
  }
  
} 
#Function fills in the linear constraint matrix (AMat), the RHS for the linear constraints (bc, and the objective function row (obj))
Fill_AMat_bc_obj<-function(Nperiods,NPostPeriods,NPostPeriodsPitLake,VarNames=VarNames,BitumenProduction,wa_per_bitbarrel,penalty=1000000,ObjectFnCoefs=c(10,-0.01,-1,2.5,5,-1),waterTreatmentVector=rep(0,Nperiods),ub_WatTreat=rep(Inf,Nperiods),PitLake_Pond_Constraints=c(10000,0,0,0,65,20,1)) {
  
  NProdPeriods<-Nperiods-NPostPeriods-NPostPeriodsPitLake
  #Bitumen production 
  
  #Extract Water Flow variables
  NProdConstrTypes=11
  NConstrTypes=7
  NVariTypes=27 
  ConstrMarkers<-rep(0,NProdConstrTypes+NConstrTypes)
  VariableMarkers<-seq(1,(NVariTypes*Nperiods),Nperiods)
  #print("Here 1")
  Npitlake_lvlconstraints = Nperiods-PitLake_Pond_Constraints[5]
  AMat<-Matrix(0,nrow=NProdPeriods*NProdConstrTypes+Nperiods*NConstrTypes+Npitlake_lvlconstraints,ncol=(Nperiods*NVariTypes))
  #print("Here 1.1")
  RowNames<-rep("",NProdConstrTypes*NProdPeriods+Nperiods*NConstrTypes+Npitlake_lvlconstraints)
  #print("Here 1.2 ")
  colnames(AMat)<-VarNames
  # Set up Objective function row
  #print("Here 2")
  ObjRow<-rep(0,(Nperiods*NVariTypes))
  #ObjectFnCoefs=c(RevenueForBitumen,-costperunitfeshwater,-1 for cost of water recyclinig)
  DiscR<-DiscRate^(1:Nperiods)
  ObjRow[Nperiods*25+1:NProdPeriods]=ObjectFnCoefs[1]*DiscR[1:NProdPeriods]  #net value of bitumen production
  ObjRow[Nperiods*14+1:NProdPeriods]=ObjectFnCoefs[2]*DiscR[1:NProdPeriods]  # water draw from the river is a low cost item $0.036/m3
  ObjRow[Nperiods*26+1:NProdPeriods]=ObjectFnCoefs[3]*DiscR[1:NProdPeriods]    # pick up the recycled water cost for conic part
  ObjRow[Nperiods*2+1:NProdPeriods]=ObjectFnCoefs[4]*DiscR[1:NProdPeriods]     #linear benefits of recycling
  ObjRow[Nperiods*12+1:Nperiods]=ObjectFnCoefs[6]*DiscR[1:Nperiods]         # water draw from the river for the filling the pond for pit lake $0.036/m3
  
  ObjRow[Nperiods*9+1:Nperiods]=ObjectFnCoefs[5]*DiscR     # benefits of treatment: We are just using this to ensure that water treatment is maximized while being less than values in the WaterTreatmentVector (which is the maximum) 
  
  #print("Here 3")
  #set up right hand side constraints
  bc<-Matrix(0,nrow=NProdPeriods*NProdConstrTypes+Nperiods*NConstrTypes+Npitlake_lvlconstraints,ncol=2)
  #print("Here 4")
  
  # set up variable lower and upper bounds
  bx<-rbind(blx=rep(0,Nperiods*NVariTypes),bux=rep(0,Nperiods*NVariTypes))
  
  # pick up bounds for variables in the cones not picked up here\
  bx[2,Nperiods*26+1:NProdPeriods]<-Inf  # costs of recycling water


  #print("Here 5")
  # Place coefficients into the linear coefficient matrices
  # Constraint set 1: bitumen production
  SetNum=0; StartRow=0
  ConstrMarkers[SetNum+1]<-StartRow+1
  for(i in 1:NProdPeriods) { #i=1
    AMat[i,i+Nperiods*25]=1
    RowNames[i]=paste0("Bp",i)
    rownames(AMat)<-RowNames
  }
  BitumenProd<-rep(0,NProdPeriods)
  BitumenProd[1]<-0.5*BitumenProduction
  BitumenProd[2:(NProdPeriods)]<-BitumenProduction
  bc[1:NProdPeriods,]<-cbind(rep(0,NProdPeriods),BitumenProd) #Bitumen ranges old
  bx[2,Nperiods*25+1:NProdPeriods]<-Inf
  
  # Constraint set 2: Water Demand from bitumen production
  SetNum=1
  StartRow<-NProdPeriods*SetNum
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:NProdPeriods,Nperiods*25+1:NProdPeriods]<-diag((-wa_per_bitbarrel),NProdPeriods) #Total water required from the process in millions of m3 of water (from bitumen production)
  AMat[StartRow+1:NProdPeriods,Nperiods*21+1:NProdPeriods]<-diag(NProdPeriods) #recycle water
  AMat[StartRow+1:NProdPeriods,Nperiods*15+1:NProdPeriods]<-diag(NProdPeriods) #fresh water
  bc[StartRow+1:NProdPeriods,]<-cbind(rep(0,NProdPeriods),rep(0,NProdPeriods))
  RowNames[StartRow+1:NProdPeriods]=paste0("wd",1:NProdPeriods) #water demand
  rownames(AMat)<-RowNames
  bx[2,Nperiods*21+1:NProdPeriods]<-Inf
  bx[2,Nperiods*15+1:NProdPeriods]<-Inf
  
  # Constraint set 3: Raw water pond constraint (Note: We want this to include potential after production draw fro mthe river)
  SetNum=2
  StartRow<-SetNum*NProdPeriods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:Nperiods,Nperiods*14+1:Nperiods]<-(-diag(Nperiods)) # set up draw from river
  AMat[StartRow+1:Nperiods,Nperiods*13+1:Nperiods]<-diag(Nperiods)  # stock of water in the raw water pond
  for(i in 2:Nperiods) {
    AMat[StartRow+i,Nperiods*13+i-1]<-(-1)
  }
  AMat[StartRow+1:Nperiods,Nperiods*15+1:Nperiods]<-diag(Nperiods)
  AMat[StartRow+1:Nperiods,Nperiods*18+1:Nperiods]<-diag(Nperiods)
  AMat[StartRow+1:Nperiods,Nperiods*19+1:Nperiods]<-diag(Nperiods)
  RowNames[StartRow+1:Nperiods]=paste0("Wf",1:Nperiods) #fresh water pond
  rownames(AMat)<-RowNames
  bc[StartRow+1:Nperiods,]<-cbind(rep(0,Nperiods),rep(0,Nperiods))
  bx[2,Nperiods*13+1:(Nperiods)]<-0 #Total water in raw water pond
  bx[2,Nperiods*14+1:Nperiods]<-PitLake_Pond_Constraints[6]   # fresh water from river
  bx[2,Nperiods*15+1:NProdPeriods]<- Inf  # Fresh water to process 
  bx[2,Nperiods*18+1:Nperiods]<-0 # water from raw fresh water pond to the recycle pond
  bx[2,Nperiods*19+1:NProdPeriods]<-Inf # water from utilities
  
  #Constraint set 4: Raw water pond is X% (I choose 5) less than the total demand for water .05*102
  # (Is Wt raw water in the pond?)
  SetNum=3
  StartRow<-StartRow+Nperiods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:Nperiods,Nperiods*13+1:Nperiods]<-diag(Nperiods)
  bc[StartRow+1:Nperiods,]<-cbind(rep(0,Nperiods),rep(wa_per_bitbarrel*BitumenProduction*0.05))
  RowNames[StartRow+1:Nperiods]=paste0("Wfc",1:Nperiods)
  rownames(AMat)<-RowNames
  bx[2,Nperiods*13+1:(Nperiods)]<-0
  
  #Constraint set 5: Water demand for utilities (Setting a ratio omega)
  SetNum=4
  StartRow=StartRow+Nperiods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:NProdPeriods,Nperiods*19+1:NProdPeriods]<-diag(NProdPeriods) # water from raW fresh water pond to the utilities
  AMat[StartRow+1:NProdPeriods,Nperiods*25+1:NProdPeriods]<-diag(-wa_Utilities_bitbarrel,NProdPeriods) #bitument production
  bc[StartRow+1:NProdPeriods,]<-cbind(rep(0,NProdPeriods),rep(0,NProdPeriods))
  RowNames[StartRow+1:NProdPeriods]=paste0("fu",1:NProdPeriods)
  rownames(AMat)<-RowNames
  bx[2,Nperiods*19+1:NProdPeriods]<-Inf
  bx[2,Nperiods*25+1:NProdPeriods]<-Inf
  
  #Constraint set 6: Water flow from production to pond (Is this covered by 1-rho)
  SetNum=5
  StartRow=StartRow+NProdPeriods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:NProdPeriods,Nperiods*25+1:NProdPeriods]<-diag(-wa_ClearInv_perBit,NProdPeriods)  #bitument production
  AMat[StartRow+1:NProdPeriods,Nperiods*3+1:NProdPeriods]<-diag(NProdPeriods) #OSPW water from the bitumen production process water to the clear water portion of the tailings pond
  bc[StartRow+1:NProdPeriods,]<-cbind(rep(0,NProdPeriods),rep(0,NProdPeriods))
  RowNames[StartRow+1:NProdPeriods]=paste0("pc",1:NProdPeriods)
  rownames(AMat)<-RowNames
  bx[2,Nperiods*3+1:NProdPeriods]<-Inf
  bx[2,Nperiods*25+1:NProdPeriods]<-Inf 
  
  ## constrtaint set 7: water flow from production to FFT 
  SetNum=6
  StartRow=StartRow+NProdPeriods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:NProdPeriods,Nperiods*25+1:NProdPeriods]<-diag(-wa_poreWa_perBit,NProdPeriods) #How is controlled from production
  AMat[StartRow+1:NProdPeriods,Nperiods*4+1:NProdPeriods]<-diag(NProdPeriods) ## Flow of water going to fft
  bc[StartRow+1:NProdPeriods,]<-cbind(rep(0,NProdPeriods),rep(0,NProdPeriods))
  RowNames[StartRow+1:NProdPeriods]=paste0("pf",1:NProdPeriods)
  rownames(AMat)<-RowNames
  bx[2,Nperiods*4+1:NProdPeriods]<-Inf
  bx[2,Nperiods*25+1:NProdPeriods]<-Inf

  ## constrtaint set 8: water balance in tailings pond
  SetNum=7
  StartRow=StartRow+NProdPeriods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:Nperiods,1:Nperiods]<-diag(Nperiods)
  for(i in 2:Nperiods) {
    AMat[StartRow+i,i-1]<-(-1)
  }
  AMat[StartRow+1:Nperiods,Nperiods*3+1:Nperiods]<-diag(-1,Nperiods) # water from bitument production
  AMat[StartRow+1:Nperiods,Nperiods*2+1:Nperiods]<-diag(Nperiods)   # water to recycling stream 
  AMat[StartRow+1:Nperiods,Nperiods*5+1:Nperiods]<-diag(-1,Nperiods)  #water coming out of FFT
  AMat[StartRow+1:Nperiods,Nperiods*8+1:Nperiods]<-diag(-1,Nperiods)   # water added by runoff and evaporation (net positive)
  AMat[StartRow+1:Nperiods,Nperiods*7+1:Nperiods]<-diag(-1,Nperiods)   # water added from depressurization water
  AMat[StartRow+1:Nperiods,Nperiods*9+1:Nperiods]<-diag(1,Nperiods)  # water going to treatment
  AMat[StartRow+1:Nperiods,Nperiods*12+1:Nperiods]<-diag(-1,Nperiods)  # water from river straight to the pond - really for end pit lake
  AMat[StartRow+1:Nperiods,Nperiods*11+1:Nperiods]<-diag(1,Nperiods)   # water going from Pond to environment - for pit lakes! this is pit lake outflow! So it will be constrained to zero until the pit lake at closure
  bc[StartRow+1:Nperiods,]<-cbind(rep(0,Nperiods),rep(0,Nperiods))
  RowNames[StartRow+1:Nperiods]=paste0("Wb",1:Nperiods)
  rownames(AMat)<-RowNames
  # Bounds for the maximum amount of water in pond
  PitLake_upper_bounds <- rep(PitLake_Pond_Constraints[4],NProdPeriods)
  PitLake_upper_bounds <- PitLake_upper_bounds-seq(0,NProdPeriods-1,1)*(PitLake_Pond_Constraints[7]) 
    bx[2,1:NProdPeriods]<-PitLake_upper_bounds  # This is Max volume for tailings during production
  bx[2,(NProdPeriods+1):Nperiods]<-PitLake_Pond_Constraints[1]  # This is MaxPitLakeVolume after production
  #bx[1,(NProdPeriods+1):Nperiods]<-PitLake_Pond_Constraints[2]  # This is TargetQuantityWaterEndPitLake It's a lowerbound
  bx[1,(NProdPeriods+NPostPeriods+1):Nperiods]<-PitLake_Pond_Constraints[2]  # This is TargetQuantityWaterEndPitLake It's a lowerbound for the end state pit lake
  
    
  bx[2,Nperiods*3+1:NProdPeriods]<-Inf
  bx[2,Nperiods*2+1:NProdPeriods]<-Inf  #Note difference here with recycled water  - it ends after production!!!!
  bx[1,Nperiods*8+1:Nperiods]<-netrunoffevap_plus_activeDewatering #fix the net runnoff variable
  bx[2,Nperiods*8+1:Nperiods]<-netrunoffevap_plus_activeDewatering
  bx[2,Nperiods*7+1:NProdPeriods]<-Inf
  bx[2,Nperiods*12+NProdPeriods+1:(NPostPeriods+NPostPeriodsPitLake)]<- PitLake_Pond_Constraints[1] # Set the maximum flow of water into lake as same as upperbound
  bx[2,Nperiods*11+NMinePeriods+1:NPostPeriodsPitLake]<-PitLake_Pond_Constraints[3] #Inf
  #bx[2,Nperiods*11+NProdPeriods+1:(Nperiods-NProdPeriods-NPostPeriodsPitLake)]<-  # this allows for water pumping into the pond to establish Target PitLake volume
  bx[1,Nperiods*11+NMinePeriods+1:NPostPeriodsPitLake]<- PitLake_Pond_Constraints[3]  # This is PitLake FlowThrough - lower bound 
  
  #Constraint set 9: water from mine depressurization (beta)
  SetNum=8
  StartRow=StartRow+Nperiods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:NProdPeriods,Nperiods*25+1:NProdPeriods]<-diag(-wa_depress_bitbarrel,NProdPeriods)
  AMat[StartRow+1:NProdPeriods,Nperiods*7+1:NProdPeriods]<-diag(NProdPeriods)
  bc[StartRow+1:NProdPeriods,]<-cbind(rep(0,NProdPeriods),rep(0,NProdPeriods))
  RowNames[StartRow+1:NProdPeriods]=paste0("dc",1:NProdPeriods)
  rownames(AMat)<-RowNames

  #Constraint set 10: Recycled water pond balance (not including water from outisde the system)
  SetNum=9
  StartRow=StartRow+NProdPeriods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:Nperiods,Nperiods*20+1:Nperiods]<-diag(Nperiods)
  for(i in 2:Nperiods) {
    AMat[StartRow+i,Nperiods*20+i-1]<-(-1)
  }
  AMat[StartRow+1:Nperiods,Nperiods*2+1:Nperiods]<-(-diag(Nperiods)) # recycled water from the tailings pond
  AMat[StartRow+1:Nperiods,Nperiods*18+1:Nperiods]<-(-diag(Nperiods)) # fresh water from the raw water pond to the recylced water pond
  AMat[StartRow+1:Nperiods,Nperiods*21+1:Nperiods]<-(diag(Nperiods))  # recycled water to the bitumen production process
  AMat[StartRow+1:Nperiods,Nperiods*22+1:Nperiods]<-(-diag(Nperiods)) # water from utilities to the recyled water pond
  AMat[StartRow+1:Nperiods,Nperiods*23+1:Nperiods]<-(-diag(Nperiods)) # water from outside system (froth from anothyer facility)
  bc[StartRow+1:Nperiods,]<-cbind(rep(0,Nperiods),rep(0,Nperiods))
  bx[2,Nperiods*20+1:Nperiods]<-0
  bx[2,Nperiods*2+1:NProdPeriods]<-Inf  #only lasts as long as production periods
  bx[2,Nperiods*21+1:NProdPeriods]<-Inf
  bx[2,Nperiods*22+1:NProdPeriods]<-Inf
  bx[1:2,Nperiods*23+1:Nperiods]<-0   #OutsideWaterOrFroth
  bx[2,Nperiods*18+1:Nperiods]<-0 
  RowNames[StartRow+1:Nperiods]=paste0("Wr",1:Nperiods)
  rownames(AMat)<-RowNames
  
  #Constraint set 11: water from utilities to recycling pond 
  SetNum=10
  StartRow=StartRow+Nperiods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:NProdPeriods,Nperiods*25+1:NProdPeriods]<-diag(-Wa_UtilToRec_bitbarrel,NProdPeriods) # bitument production
  AMat[StartRow+1:NProdPeriods,Nperiods*22+1:NProdPeriods]<-diag(NProdPeriods) # water from utilities to recycle pond
  bc[StartRow+1:NProdPeriods,]<-cbind(rep(0,NProdPeriods),rep(0,NProdPeriods))
  RowNames[StartRow+1:NProdPeriods]=paste0("ur",1:NProdPeriods)
  rownames(AMat)<-RowNames
  
  #Constraint set 12: recycled water must be less that 99% of water used
  SetNum=11
  StartRow=StartRow+NProdPeriods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:NProdPeriods,Nperiods*15+1:NProdPeriods]<-diag(-0.99,NProdPeriods) # maximum 99% recycled
  AMat[StartRow+1:NProdPeriods,Nperiods*18+1:NProdPeriods]<-diag(-0.99,NProdPeriods)  # water from raw fresh water pond to the recycle pond
  AMat[StartRow+1:NProdPeriods,Nperiods*2+1:NProdPeriods]<-(diag(0.01,NProdPeriods)) # recycled water from the tailings pond
  bc[StartRow+1:NProdPeriods,]<-cbind(rep(-Inf,NProdPeriods),rep(0,NProdPeriods))
  RowNames[StartRow+1:NProdPeriods]=paste0("rW",1:NProdPeriods)
  rownames(AMat)<-RowNames
  
  #Constraint set 13: balance of water trapped in FFT (pore water)
  SetNum=12
  StartRow=StartRow+NProdPeriods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:Nperiods,Nperiods*4+1:Nperiods]<-diag(-1,Nperiods) # pf = water trapped in pore water from the bitumen production process water - trapped in FFT
  AMat[StartRow+1:Nperiods,Nperiods+1:Nperiods]<-diag(1,Nperiods) # Wp = stock Total water trapped in pore water FFT
  AMat[StartRow+1:Nperiods,Nperiods*5+1:Nperiods]<-diag(1,Nperiods) # fc = water flowing from FFT to clear water in the tailings pond 
  for(i in 2:Nperiods) { 
    AMat[StartRow+i,Nperiods+i-1]<-(-1) # Account to the fact that the pore water starts at t=2 (can only start if there is a flow from the previous period)
  }
  RowNames[StartRow+1:Nperiods]=paste0("Wf",1:Nperiods)
  rownames(AMat)<-RowNames
  bx[2,Nperiods+1:Nperiods]<-Inf
  bx[2,Nperiods*4+1:NProdPeriods]<-Inf
  
 
  #Constraint Set 14: connate water from extracted during processing 
  SetNum=13
  StartRow=StartRow+Nperiods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:NProdPeriods,Nperiods*25+1:NProdPeriods]<-diag(-wa_connate_bitbarrel,NProdPeriods)  # bitument production
  AMat[StartRow+1:NProdPeriods,Nperiods*17+1:NProdPeriods]<-diag(NProdPeriods) # connate water from the bitumen process
  bc[StartRow+1:NProdPeriods,]<-cbind(rep(0,NProdPeriods),rep(0,NProdPeriods))
  RowNames[StartRow+1:NProdPeriods]=paste0("co",1:NProdPeriods)
  rownames(AMat)<-RowNames
  bx[2,Nperiods*17+1:NProdPeriods]<-Inf
  bx[2,Nperiods*25+1:NProdPeriods]<-Inf
  
  #Constraint Set 15: recycled water cannot be greater than the rate at which water is coming in from the processing plant (What is the difference to the .99wt?)
  SetNum=14
  StartRow=StartRow+NProdPeriods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:NProdPeriods,Nperiods*2+1:NProdPeriods]<-diag(-1/8,NProdPeriods)  #wr
  AMat[StartRow+1:NProdPeriods,1:NProdPeriods]<-diag(1,NProdPeriods)  #pc  process water to clear water portion
  bc[StartRow+1:NProdPeriods,]<-cbind(rep(0,NProdPeriods),rep(Inf,NProdPeriods))
  RowNames[StartRow+1:NProdPeriods]=paste0("Wwr",1:NProdPeriods)
  rownames(AMat)<-RowNames
  
  
  #Constraint Set 16: Adding up total water demand from recycled + fresh
  SetNum=15
  StartRow=StartRow+NProdPeriods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:NProdPeriods,Nperiods*16+1:NProdPeriods]<-diag(NProdPeriods)    # wd - total water demand
  AMat[StartRow+1:NProdPeriods,Nperiods*15+1:NProdPeriods]<-diag(-1,NProdPeriods) # fresh water to bitumen production process
  AMat[StartRow+1:NProdPeriods,Nperiods*21+1:NProdPeriods]<-diag(-1,NProdPeriods) # recycled water to process
  bc[StartRow+1:NProdPeriods,]<-cbind(rep(0,NProdPeriods),rep(0,NProdPeriods))
  RowNames[StartRow+1:NProdPeriods]=paste0("wd",1:NProdPeriods)
  rownames(AMat)<-RowNames
  bx[2,Nperiods*16+1:NProdPeriods]<-Inf
  
  #Constraint Set 17: water treated must be less than 0.95 of water in the pond
  SetNum=16
  StartRow=StartRow+NProdPeriods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:Nperiods,Nperiods*9+1:Nperiods]<-diag(1,Nperiods)    # wd - total water demand
  AMat[StartRow+1:Nperiods,1:Nperiods]<-diag(-0.95,Nperiods) # fresh water to bitumen production process
  bc[StartRow+1:Nperiods,]<-cbind(rep(-Inf,Nperiods),rep(0,Nperiods))
  # bounds on the water treatment variable that are passed into the function
  bx[2,Nperiods*9+1:Nperiods]<-ub_WatTreat

  RowNames[StartRow+1:Nperiods]=paste0("tW",1:Nperiods)
  rownames(AMat)<-RowNames
  
  #Constraint Set 18: pore water flowing from trapped pore water stock to clear water pond is less than or equal to Percentage of trapped water 
  Rate_PoreWater_To_ClearWater<-0.03  # salt loading model
  Maximum_PoreWater_ToPits<-1/3  
  MaxTimePoreWaterMigratesToFree<-log(1-Maximum_PoreWater_ToPits)/-(Rate_PoreWater_To_ClearWater)
  ti<-trunc(MaxTimePoreWaterMigratesToFree+1)
  PoreWaterTransferVector<-exp(-Rate_PoreWater_To_ClearWater*seq(0,ti-1))-exp(-Rate_PoreWater_To_ClearWater*seq(1,ti))
  sum(PoreWaterTransferVector)
  SetNum=17
  StartRow=StartRow+Nperiods
  ConstrMarkers[SetNum+1]<-StartRow+1
  t=2
  AMat[StartRow+1:Nperiods,Nperiods*5+1:Nperiods]<-diag(1,Nperiods)   # water flowing from FFT back to clear water pond inventory
  for(t in 1:Nperiods) { #Work down colums and rows simulteously
    AMat[StartRow+t:min(Nperiods,t+ti-1),Nperiods*4+t]<-(-PoreWaterTransferVector[1:min(ti,Nperiods-t+1)])
  }

  bc[StartRow+1:Nperiods,]<-cbind(rep(0,Nperiods),cbind(rep(0,Nperiods)))
  bx[2,Nperiods*5+1:Nperiods]<-Inf #TailingPondMaxVolume*(Rate_PoreWater_To_ClearWater*1.1) #multiply by 1.1 here to ensure we're never at this bound - maximum it shoudl be is the rateofflow*Porewater
  RowNames[StartRow+1:Nperiods]=paste0("rpw",1:Nperiods) # rate of pore water movement back to clear water inventory
  rownames(AMat)<-RowNames

  #Constraint Set 19: Water released from pit lake must be such that the water in the pit lake stays the same each year in the Pitlake period
  SetNum=18
  
  StartRow=StartRow+Nperiods
  ConstrMarkers[SetNum+1]<-StartRow+1
  AMat[StartRow+1:Npitlake_lvlconstraints,PitLake_Pond_Constraints[5]+1:Npitlake_lvlconstraints]=diag(1,(Npitlake_lvlconstraints))
  for(i in 1:Npitlake_lvlconstraints) {
    AMat[StartRow+i,PitLake_Pond_Constraints[5]+i-1]<-(-1)
  }
  RowNames[StartRow+1:Npitlake_lvlconstraints]=paste0("piL",1:  Npitlake_lvlconstraints)
  rownames(AMat)<-RowNames
  bc[StartRow+1:Npitlake_lvlconstraints,]<-cbind(rep(0,  Npitlake_lvlconstraints),rep(0,  Npitlake_lvlconstraints))
  
  
  AMatbc<-list()
  AMatbc$AMat<-AMat
  AMatbc$bc<-bc
  AMatbc$bx<-bx
  AMatbc$ObjRow<-ObjRow
  AMatbc$ConstrMarkers<-ConstrMarkers
  AMatbc$VariableMarkers<-VariableMarkers
  return(AMatbc)
} 

ConstructCones<-function(Nperiods,NPostPeriods,NPostPeriodsPitLake,VarNames=VarNames,Theta=1,rho=50) {
  NVariTypes=27 
  NProdPeriods<-Nperiods-NPostPeriods-NPostPeriodsPitLake
  NConeTypes<-NProdPeriods
  length(VarNames)
  FRowNames<-rep("",(NProdPeriods)*3) 
  FMat<-Matrix(0,nrow=(NProdPeriods*3),ncol=(NVariTypes*Nperiods))
  colnames(FMat)<-VarNames
  gvecs<-Matrix(rep(0,(NProdPeriods*3)),nrow=(NProdPeriods*3),ncol=1)

  # Cone Set #1 Cost of treating water from the tailing ponds
  Cones <- matrix(rep(list("PPOW", 3, c(1/rho,1-1/rho)),(NProdPeriods)), ncol=(NProdPeriods),nrow=3)
  rownames(Cones) <- c("type","dim","conepar")
  for(i in 1:NProdPeriods) {  #i=2
    FMat[(i-1)*3+1,26*Nperiods+i]=1 #cost of recycling water in production
    FMat[(i-1)*3+2,16*Nperiods+i]=1 #Total water demand (note this should be just a constant most of the time because it's precalculated)
    FMat[(i-1)*3+3,2*Nperiods+i]=Theta #recycled water to recycled water pond from the clear part of the tailings pond
    gvecs[(i-1)*3+1:3]<-c(0,0,0)
    FRowNames[((i-1)*3+1):(i*3)]<-paste0("CoWr",i)
  }
  ConeList<-list()
  ConeList$FMat<-FMat
  ConeList$gvecs<-gvecs
  ConeList$Cones<-Cones
  return(ConeList)
}


CallMineWaterSystemFn<-function(DiscRate=0.96,
                                # Set up a "restricted" Mosek problem where we "FIX" the proportion of water treated in the tailings pond
                                # This is necessary in order to impose the following constraint PropWaterTreated=(WaterTreated/TotalWaterInPond)=(SaltRemoved/TotalSaltInPond)=(NAsRemoved/TotalNAsInPond)
                                # Parameters setting up the SIZE of the problem
                                Nperiods=8, #  numer total periods
                                # NVariTypes=27, # number of variables types in production periods
                                # NConstrTypes=18, #number of constraint types in production periods
                                NPostPeriods=4, # postproduction periods
                                # NVariTypesPostProd=21, # number of variable type in  post production periods
                                # NPostConstrTypes=14,
                                #  EndingConcConstraints=2,
                                #  EndWaterInPondConstraints=1,
                                #  EndingConcVariables=4,
                                #  NEndConcConstraints=2, # number of ending constraints in concentration of water at the end.  
                                #  MaxFreshWaterWithdrawal=11,
                                #  TargetQuantityWaterEndPitLake=0.1,
                                ObjectFnCoefs=c(1000,-0.03156905,-1,2.5,100,-0.03156905), # (To keep it in bounds)
                                # #ObjectFnCoefs=c(RevenueForBitumen,-costperunitfeshwater,-1 for cost of water recycling conic portion,2.5 for benefit of water recycling,5 Benefit of water treatment,-costperunitfeshwater)
                                # 10 is for bitumen to always produce (highest)
                                # The 5 is second best to always treat when indicated (all that is possible)
                                # -costperunitfeshwater,-1 for cost of water recycling conic portion,Benefit of water treatment                            
                              VarNames=CreateVarNames(Nperiods), # You can call a function in the argument list
                                waterTreatmentVector=rep(0,Nperiods), #default
                                ub_WatTreat=rep(Inf,Nperiods),
                              PitLake_Pond_Constraints=c(10000,0,0,0,65,20,1)) 
{
  
  
  AMatbc<-Fill_AMat_bc_obj(Nperiods,NPostPeriods,NPostPeriodsPitLake,VarNames=VarNames,BitumenProduction,wa_per_bitbarrel=wa_per_bitbarrel,ObjectFnCoefs=ObjectFnCoefs,waterTreatmentVector=WaterTreatVec,ub_WatTreat=ub_WatTreat,PitLake_Pond_Constraints=PitLake_Pond_Constraints) 
  #printSpMatrix(rbind(cbind(AMatbc$AMat,AMatbc$bc),
  #                    cbind(AMatbc$bx,matrix(0,2,2)))[949:952,],col.names=TRUE,digits=3)
  ConeData<-ConstructCones(Nperiods,NPostPeriods,NPostPeriodsPitLake,VarNames=VarNames,Theta=1,rho=3)
  
  conic_OSPW_prob<-list(sense="max")
  conic_OSPW_prob$bx<-AMatbc$bx
  conic_OSPW_prob$bc<-as.matrix(t(AMatbc$bc))  #[1:2,1:12]
  conic_OSPW_prob$A<-AMatbc$AMat#[1:12,]
  conic_OSPW_prob$c<-AMatbc$ObjRow
  conic_OSPW_prob$F<-ConeData$FMat
  conic_OSPW_prob$g<-as.numeric(ConeData$gvecs)
  conic_OSPW_prob$cones<-ConeData$Cones
  
  # solve the problem
  r <- mosek(conic_OSPW_prob, list(soldetail=1,verbose=5))
  r$sol$itr$pobjval
  
  Obj_Row<-Matrix(c(AMatbc$ObjRow,0,0),nrow=1)
  rownames(Obj_Row)<-c("Obj")
  soln_row<-Matrix(c(round(r$sol$itr$xx,3),0,0),nrow=1)
  rownames(soln_row)<-c("sol")
  Obj_A_bc_xx2<-rbind(Obj_Row,
                      rep(0,length(AMatbc$ObjRow)+2),
                      cbind(conic_OSPW_prob$A,t(conic_OSPW_prob$bc)),
                      rep(0,length(AMatbc$ObjRow)+2),
                      cbind(conic_OSPW_prob$bx,matrix(0,2,2)),
                      rep(0,length(AMatbc$ObjRow)+2),
                      soln_row)
  Results<-list()
  Results$FullLinMatrixWithSoln<-Obj_A_bc_xx2

  Results$FullSoln<-matrix(r$sol$itr$xx,ncol=1,dimnames=list(VarNames,c("Soln")))
  Results$PondWater<-matrix(r$sol$itr$xx[1:Nperiods],ncol=1,dimnames=list(VarNames[1:Nperiods],c("WaPond")))
  Results$WaterRecycledFromPond<-matrix(r$sol$itr$xx[Nperiods*2+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*2+1:Nperiods],c("wr")))
  Results$ConnateWater<-matrix(r$sol$itr$xx[Nperiods*17+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*17+1:Nperiods],c("co")))
  Results$MineDepWater<-matrix(r$sol$itr$xx[Nperiods*7+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*8+1:Nperiods],c("dc")))
  Results$freshWaterRiver<-matrix(r$sol$itr$xx[Nperiods*14+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*14+1:Nperiods],c("wf")))
  Results$freshWaterToPond<-matrix(r$sol$itr$xx[Nperiods*12+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*12+1:Nperiods],c("fw")))
  Results$BitumenProduction<-matrix(r$sol$itr$xx[Nperiods*25+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*25+1:Nperiods],c("Bp")))
  Results$Watertreated<-matrix(r$sol$itr$xx[Nperiods*9+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*9+1:Nperiods],c("wt")))
  Results$PitLakeOutflow<-matrix(r$sol$itr$xx[Nperiods*11+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*11+1:Nperiods],c("te")))
  Results$ObjFn<-r$sol$itr$pobjval
  Results$message<-r$response$msg
  Results$code<-r$response$code
  Results$prosta<-r$sol$itr$prosta
  Results$solsta<-r$sol$itr$solsta
  Results$conic_OSPW_prob<-conic_OSPW_prob
  Results$VarNames<-VarNames
  Results$AMatbc<-AMatbc
  return(Results)
}  

# column set 25 cost of recycling water in production
#VarNames[Nperiods*26+i]=paste0("Cr",i) # Cost of recycling water in production

CallMineWaterSystemFn2<-function(conic_OSPW_prob=conic_OSPW_prob,waterTreatmentVector=rep(0,Nperiods)) 
{
  conic_OSPW_prob$bx[2,Nperiods*9+1:Nperiods]<-waterTreatmentVector #Edits the problem by changing the Water treatment vector bound

  #take the max needed treatment * x%
  r <- mosek(conic_OSPW_prob, list(soldetail=1,verbose=0))
  Results<-list()
  length(r$sol$itr$xx)
  length(VarNames)
  Results$FullSoln<-matrix(r$sol$itr$xx,ncol=1,dimnames=list(VarNames,c("Soln")))
  Results$PondWater<-matrix(r$sol$itr$xx[1:Nperiods],ncol=1,dimnames=list(VarNames[1:Nperiods],c("WaPond")))
  Results$PoreWater<-matrix(r$sol$itr$xx[Nperiods*1+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods+1:Nperiods],c("WaPore")))
  Results$WaterRecycledFromPond<-matrix(r$sol$itr$xx[Nperiods*2+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*2+1:Nperiods],c("wr")))
  Results$ConnateWater<-matrix(r$sol$itr$xx[Nperiods*17+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*17+1:Nperiods],c("co")))
  Results$MineDepWater<-matrix(r$sol$itr$xx[Nperiods*7+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*8+1:Nperiods],c("dc")))
  Results$freshWaterRiver<-matrix(r$sol$itr$xx[Nperiods*14+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*14+1:Nperiods],c("wf")))
  Results$BitumenProduction<-matrix(r$sol$itr$xx[Nperiods*25+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*25+1:Nperiods],c("Bp")))
  Results$Watertreated<-matrix(r$sol$itr$xx[Nperiods*9+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*9+1:Nperiods],c("wt")))
  Results$PitLakeOutflow<-matrix(r$sol$itr$xx[Nperiods*11+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*11+1:Nperiods],c("te")))
  Results$freshWaterToPond<-matrix(r$sol$itr$xx[Nperiods*12+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*12+1:Nperiods],c("fw")))
  Results$FreshWaterToRecyclePond<-matrix(r$sol$itr$xx[Nperiods*18+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*18+1:Nperiods],c("fr")))
  Results$FreshwaterToProcess<-matrix(r$sol$itr$xx[Nperiods*15+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*15+1:Nperiods],c("fp")))
  Results$RecycleToProcess<-matrix(r$sol$itr$xx[Nperiods*21+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*21+1:Nperiods],c("rp")))
  Results$ProdToPondOSPW<-matrix(r$sol$itr$xx[Nperiods*3+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*3+1:Nperiods],c("pc")))
  Results$ProdToPoreOSPW<-matrix(r$sol$itr$xx[Nperiods*4+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*4+1:Nperiods],c("pf")))
  Results$PoreWaterToPond<-matrix(r$sol$itr$xx[Nperiods*5+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*5+1:Nperiods],c("fc")))
  Results$NetRunoffEvap<-matrix(r$sol$itr$xx[Nperiods*8+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*8+1:Nperiods],c("er")))
  Results$RawWaterPond<-matrix(r$sol$itr$xx[Nperiods*13+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*13+1:Nperiods],c("Wf")))
  Results$WaterDemand<-matrix(r$sol$itr$xx[Nperiods*16+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*16+1:Nperiods],c("wd")))
  Results$FreshWaterToUtilities<-matrix(r$sol$itr$xx[Nperiods*19+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*19+1:Nperiods],c("fu")))
  Results$RecycledWaterPond<-matrix(r$sol$itr$xx[Nperiods*20+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*20+1:Nperiods],c("Wr")))
  Results$UtiltoRecyle<-matrix(r$sol$itr$xx[Nperiods*22+1:Nperiods],ncol=1,dimnames=list(VarNames[Nperiods*22+1:Nperiods],c("ur")))
  Results$Duals_lwBnd<-matrix(r$sol$itr$slc,ncol=1,dimnames=list(rownames(conic_OSPW_prob$A),"dl"))
  Results$const_rhs<-matrix(r$sol$itr$xc,ncol=1,dimnames=list(rownames(conic_OSPW_prob$A),"rhs"))
  Results$ObjFn<-r$sol$itr$pobjval
  Results$WaterPumping<-1e6*(r$sol$itr$xx[Nperiods*14+1:NProdPeriods]%*%conic_OSPW_prob$c[Nperiods*14+1:NProdPeriods]+r$sol$itr$xx[Nperiods*12+1:Nperiods]%*%conic_OSPW_prob$c[Nperiods*12+1:Nperiods])
  Results$WaterPumping_production<-1e6*(r$sol$itr$xx[Nperiods*14+1:NProdPeriods]%*%conic_OSPW_prob$c[Nperiods*14+1:NProdPeriods])
  Results$WaterPumping_RTR<-1e6*(r$sol$itr$xx[Nperiods*12+1:Nperiods]%*%conic_OSPW_prob$c[Nperiods*12+1:Nperiods])
  Results$Bit_cost <- 1e6*sum(conic_OSPW_prob$c[Nperiods*25+1:40]*(conic_OSPW_prob$bc[2,1:40]-r$sol$itr$xx[Nperiods*25+1:40])) 
  
  Results$message<-r$response$msg
  Results$code<-r$response$code
  Results$prosta<-r$sol$itr$prosta
  Results$solsta<-r$sol$itr$solsta
  Obj_Row<-Matrix(c(conic_OSPW_prob$c,0,0),nrow=1)
  rownames(Obj_Row)<-c("Obj")
  soln_row<-Matrix(c(round(r$sol$itr$xx,3),0,0),nrow=1)
  rownames(soln_row)<-c("sol")
  Obj_A_bc_xx2<-rbind(Obj_Row,
                      rep(0,length(Obj_Row)),
                      cbind(conic_OSPW_prob$A,t(conic_OSPW_prob$bc)),
                      rep(0,length(Obj_Row)),
                      cbind(conic_OSPW_prob$bx,matrix(0,2,2)),
                      rep(0,length(Obj_Row)),
                      soln_row)
  Results$FullLinMatrixWithSoln<-Obj_A_bc_xx2
  #Results$conic_OSPW_prob<-conic_OSPW_prob
  return(Results)
} 


################ Effluent limit Functions ################
{
  WLA_c <- function(G_c,Q_e,Q_s,ff,Cs,Nperiods) {
    matrix(rep(G_c,Nperiods),ncol=5,byrow=TRUE)+(1/Q_e)%*%((Q_s*ff)*(G_c-Cs))
  }

  LTA_c <- function(WLA_c_r,S_Var, z_98) {
    WLA_c_r*exp(0.5*S_Var-z_98*(S_Var)^(1/2)) 
  }
  
  AML_c <- function(LTA_c_r,S_Var, z_95) {
    LTA_c_r*exp(z_95*(S_Var)^(1/2)-0.5*S_Var) 
  }
  
  MDL_c <- function(LTA_c_r,Var, z_95) {
    LTA_c_r*exp(z_99*(Var)^(1/2)-0.5*Var) 
  }
}


################### Limits2
{

  Limits2 <- function(G_c,Qe,Nperiods,Q_s) {
    lim2=array(0,dim=c(Nperiods,length(G_c),5))
    lim2[,,1]=WLA_c(G_c,Qe,Q_s,ff,RiverConc_mgL,Nperiods)
    lim2[,,2]=LTA_c(lim2[,,1],S_Var,z_98)
    lim2[,,3]=AML_c(lim2[,,2],S_Var, z_95)
    lim2[,,4]=MDL_c(lim2[,,2],Var, z_99)
    return(lim2)
  }
}


################ Set up Treatments Costs and treatment effects for the treatments represented in the model ###############

## Parallel processing (allowed by Rmosek)
## https://askanydifference.com/difference-between-microsoft-r-open-and-r-studio-with-table/

Init_Pollution_de<-function(t,P_state,P_parameters){
  with(as.list(c(P_state,P_parameters)),{
    #rate of change
    dP.TDS<-b.TDS-a1*P.TDS+pwtr*PWo.TDS
    dP.Cl<-b.Cl-a2*P.Cl+pwtr*PWo.Cl
    dP.Na<-b.Na-a3*P.Na+pwtr*PWo.Na
    dP.Cd<-b.Cd-a4*P.Cd+pwtr*PWo.Cd
    dP.Am<-b.Am-a5*P.Am+pwtr*PWo.Am
    dW<-c
    dWo<-co
    dPWo.TDS<-bo.TDS-pwtr*PWo.TDS
    dPWo.Cl<-bo.Cl-pwtr*PWo.Cl
    dPWo.Na<-bo.Na-pwtr*PWo.Na
    dPWo.Cd<-bo.Cd-pwtr*PWo.Cd
    dPWo.Am<-bo.Am-pwtr*PWo.Am
    
    #return rate of change
    list(c(dP.TDS,dP.Cl,dP.Na,dP.Cd,dP.Am,dW,dWo,dPWo.TDS,dPWo.Cl,dPWo.Na,dPWo.Cd,dPWo.Am))
  })
}

Pollution_de<-function(t,P_state,P_parameters){ #Differential equation
  with(as.list(c(P_state,P_parameters)),{
    #rate of change #More complex for n+1 periods
    dP.TDS<-b.TDS-br*P.TDS/W-a1*P.TDS+pwtr*PWo.TDS-Tr*P.TDS/W-PLOF*P.TDS/W
    dP.Cl<-b.Cl-br*P.Cl/W-a2*P.Cl+pwtr*PWo.Cl-Tr*P.Cl/W-PLOF*P.Cl/W
    dP.Na<-b.Na-br*P.Na/W-a3*P.Na+pwtr*PWo.Na-Tr*P.Na/W-PLOF*P.Na/W
    dP.Cd<-b.Cd-br*P.Cd/W-a4*P.Cd+pwtr*PWo.Cd-Tr*P.Cd/W-PLOF*P.Cd/W
    dP.Am<-b.Am-br*P.Am/W-a5*P.Am+pwtr*PWo.Am-Tr*P.Am/W-PLOF*P.Am/W
    dW<-c
    dWo<-co
    dPWo.TDS<-bo.TDS+br*P.TDS/W-pwtr*PWo.TDS
    dPWo.Cl<-bo.Cl+br*P.Cl/W-pwtr*PWo.Cl
    dPWo.Na<-bo.Na+br*P.Na/W-pwtr*PWo.Na
    dPWo.Cd<-bo.Cd+br*P.Cd/W-pwtr*PWo.Cd
    dPWo.Am<-bo.Am+br*P.Am/W-pwtr*PWo.Am
    
    #return rate of change
    list(c(dP.TDS,dP.Cl,dP.Na,dP.Cd,dP.Am,dW,dWo,dPWo.TDS,dPWo.Cl,dPWo.Na,dPWo.Cd,dPWo.Am))
  })
  
}

# 70 year MODEL

#1 Inflow_S = OSPW inflow in Summer (Mm3) [,10] Mm3
#2 Inflow_W = OSPW inflow in Winter (Mm3) [,10] Mm3
#3 STreat = Start treatment time (Year) [,50] year
#4 DTreat = Duration of treatment (Years) [,60] year
#5 End treatment time = Wetland treatment day for endpitlake [,80] days
#6 Wet_Tdays = Wetland treatment days (days)  [,80] days
#7 End of pipe NAs target (Target) [,G_a]
#8 Technology = 0-1 no early treatment tech, 1-2 OZ, 2-3 WL_BC, 3-4 RO, 



#DecisionVariables=OptDecVariables_NATarget20_TDS2500_NT_Post10
WaterCostModel2<-function(Decisions=DecisionVariables,optimiz=TRUE,FixedStartTime=NULL) {
  Technology = Decisions[8]
  #if (Decisions[3]+Decisions[4]>Last_Year_Treatment) {
  #  Decisions[4]=Last_Year_Treatment-Decisions[3] #GHchange this used to be -Decisions[4]
  #}
  if(Technology>1) {
    #T<-trunc(DecisionVariables[3])+1 # Time to starts (T is in years) 
    #GHchange this line is incorrect because refers to the variable passed into Decisions not Decisions
    T<-min(trunc(Decisions[3])+1,Last_Year_Treatment)  # GHchange
    TreaT<-Last_Year_Treatment  #min(trunc(Decisions[3]+Decisions[4]+1),Last_Year_Treatment) #GHchange
  
    TreatVec_S<-rep(0,Nperiods)
    TreatVec_W<-rep(0,Nperiods)
    TreatVec_S[T:TreaT]<-Decisions[1] 
    TreatVec_W[T:TreaT]<-Decisions[2] 
    Tf<-T-Decisions[3]
    TreatVec_S[T]<-Tf*Decisions[1]
    TreatVec_W[T]<-Tf*Decisions[2]
    if(Technology>2 & Technology<=3) {TreatVec_W<-rep(0,70)} 
    # GHchange We need to zero out the winter treatment vector for biochar-wetland before calling CallMineWaterSystemFn2
  } else {
    TreatVec_S<-rep(0,Nperiods)
    TreatVec_W<-rep(0,Nperiods)
  }
  
  WaterTreatVec<-TreatVec_S+TreatVec_W

  Pitlake_startyears = Last_Year_Treatment+1 #(70- (20 - max(TreaT-50,0)))+1
  #GHChanges  # I moved the five lines from here after the call to CallMineWaterSystemFn2
  
  MineResults<-CallMineWaterSystemFn2(conic_OSPW_prob,waterTreatmentVector=WaterTreatVec)
  MineResults$prosta
  
  if(MineResults$prosta=="PRIMAL_INFEASIBLE"){
    print(noquote(c("Decisions",Decisions)))
    print(WaterTreatVec)
    stop("Error: CallMineWaterSystem is Infeasible!")
  }
  TreatVec_S_End<-rep(0,Nperiods)
  TreatVec_W_End<-rep(0,Nperiods)
  TreatVec_S_End[Pitlake_startyears:Nperiods]<-MineResults$PitLakeOutflow[Pitlake_startyears:Nperiods]   #(min(Pitlake_startyears-Nperiods,20)) # GHChanges should be at the end of treatment until the end of periods or max 20 years
  TreatVec_W_End[Pitlake_startyears:Nperiods]<-0 # should be at the end of treatment (Lenght of treatment)
  
  WaterTreatVec_End=TreatVec_S_End+TreatVec_W_End
  
  MineResults$freshWaterRiver_m3sec<-MineResults$freshWaterRiver*Mm3Yr_to_m3sec  

# Calculate Flows of NAs and Salts(Chorides),cadmium, etc
  sapply(Pol_ConnWater_tMm3,"*",MineResults$ConnateWater)
  TotNewPollutants<-(sapply(Pol_ConnWater_tMm3,"*",MineResults$ConnateWater)+ #New input each year
                       sapply(Pol_DepressWater_tMm3,"*",MineResults$MineDepWater)+
                       sapply(Pol_RiverWater_tMm3,"*",MineResults$freshWaterRiver)+
                       sapply(Pol_BitProcess_tonnesMbar,"*",MineResults$BitumenProduction)+ #Has all the pollutants
                       sapply(Pol_RunoffWater_tMm3,"*",MineResults$NetRunoffEvap)+
                       sapply(Pol_RiverWater_tMm3,"*",MineResults$freshWaterToPond)) #No Cd and Am
  #cbind(TotNewPollutants,MineResults$freshWaterRiver)
  colnames(TotNewPollutants)<-PolNames
  PollToPond<-TotNewPollutants*wa_ClearInv_per_wainput
  
  ## Evaluate changes in the runoff water

  PollToPore<-TotNewPollutants*(1-wa_ClearInv_per_wainput) # this is just what's new being added from the ore processing - doesn't account for concentration of polllution already accumulated in the ponds
  
  TotPollutants<-matrix(0,nrow=Nperiods,ncol=length(PolNames),dimnames=list(paste0("per",1:Nperiods),PolNames))
  TotPoreWaPoll<-matrix(0,nrow=Nperiods,ncol=length(PolNames),dimnames=list(paste0("per",1:Nperiods),PolNames))
  concPollutants<-TotPollutants
  concPoreWaPoll<-TotPoreWaPoll
  RatePoreWaterToClearWater<-MineResults$PoreWaterToPond/MineResults$PoreWater

  FractProcessWaFlowToPoreWater<-MineResults$ProdToPoreOSPW/(MineResults$ProdToPondOSPW+MineResults$ProdToPoreOSPW)
  FractProcessWaFlowToPoreWater<-ifelse(is.na(FractProcessWaFlowToPoreWater),0,FractProcessWaFlowToPoreWater)
  
  # differential equations for total pollutants and concentrations
  times<-seq(0,0.1,by=0.1)
  P_out<-ode(y=c(P.TDS=0,P.Cl=0,P.Na=0,P.Cd=0,P.Am=0,W=0,Wo=0,PWo.TDS=0,PWo.Cl=0,PWo.Na=0,PWo.Cd=0,PWo.Am=0),
             times=times,
             func=Init_Pollution_de,
             parms=c(b=PollToPond[1,],bo=PollToPore[1,],
                     a=Pol_DecayRates,
                     Tr=0,
                     pwtr=RatePoreWaterToClearWater[1],
                     c=MineResults$PondWater[1],
                     co=MineResults$PoreWater[1])) # Initializing
  Tot_Poll<-as.vector(P_out[length(P_out[,1]),c(2:6,9:13)])
  
  # differential equations for total pollutants and concentrations
  times<-seq(0.1,1,by=0.9)
  
  # Wo is 10% of the water
  P_out<-ode(y=c(P.TDS=Tot_Poll[1],P.Cl=Tot_Poll[2],P.Na=Tot_Poll[3],P.Cd=Tot_Poll[4],P.Am=Tot_Poll[5],W=(0.1*MineResults$PondWater[1]),
                 Wo=0.1*MineResults$PoreWater[1],PWo.TDS=Tot_Poll[6],PWo.Cl=Tot_Poll[7],PWo.Na=Tot_Poll[8],PWo.Cd=Tot_Poll[9],PWo.Am=Tot_Poll[10]),
             times=times,
             func=Pollution_de,
             parms=c(b=PollToPond[1,],bo=PollToPore[1,],
                     br=FractProcessWaFlowToPoreWater[1]*MineResults$WaterRecycledFromPond[1],
                     a=Pol_DecayRates,
                     Tr=MineResults$Watertreated[1],
                     PLOF=MineResults$PitLakeOutflow[1],
                     pwtr=RatePoreWaterToClearWater[1],
                     c=MineResults$PondWater[1]*0.9,
                     co=MineResults$PoreWater[1]*0.9))
  TotPollutants[1,]<-P_out[length(P_out[,1]),2:6]
  TotPollutants[1,]<-ifelse(TotPollutants[1,]<0,0,TotPollutants[1,])
  concPollutants[1,]<-TotPollutants[1,]/MineResults$PondWater[1]  
  TotPoreWaPoll[1,]<-P_out[length(P_out[,1]),9:13]
  TotPoreWaPoll[1,]<-ifelse(TotPoreWaPoll[1,]<0,0,TotPoreWaPoll[1,])
  concPoreWaPoll[1,]<-TotPoreWaPoll[1,]/MineResults$PoreWater[1]
  times<-seq(0,1,by=1)
  for(t in 2:Nperiods ) {    # t=2 Nperiods  t=41   Nperiods
  
    
    #MineResults$WaterRecycledFromPond/MineResults$PondWater
    P_out<-ode(y=c(P=TotPollutants[t-1,],W=MineResults$PondWater[t-1], # Ordinary Differential Equation
                   Wo=MineResults$PoreWater[t-1],PWo=TotPoreWaPoll[t-1,]),
                    times=times,
                    func=Pollution_de,  # Needed function
                    parms=c(b=PollToPond[t,],bo=PollToPore[t,],
                       br=FractProcessWaFlowToPoreWater[t]*MineResults$WaterRecycledFromPond[t],
                       a=Pol_DecayRates,
                       Tr=MineResults$Watertreated[t],
                       PLOF=MineResults$PitLakeOutflow[t],
                       pwtr=RatePoreWaterToClearWater[t],
                       c=(MineResults$PondWater[t]-MineResults$PondWater[t-1]),
                       co=(MineResults$PoreWater[t]-MineResults$PoreWater[t-1])))
    
    TotPollutants[t,]<-P_out[length(P_out[,1]),2:6]     
    #ifelse(a!=0,b_ov_a+C*exp(a*1),TotNewPollutants[t,]+TotPollutants[t-1,])
    TotPollutants[t,]<-ifelse(TotPollutants[t,]<0,0,TotPollutants[t,])
    
    #### End of new way of getting TotPollutants[,t] ######3
    
    concPollutants[t,]<-TotPollutants[t,]/(MineResults$PondWater[t])   
    
    TotPoreWaPoll[t,]<-P_out[length(P_out[,1]),9:13]
    TotPoreWaPoll[t,]<-ifelse(TotPoreWaPoll[t,]<0,0,TotPoreWaPoll[t,])
    concPoreWaPoll[t,]<-TotPoreWaPoll[t,]/MineResults$PoreWater[t]
    
  }
  
  Pollution<-list()
  Pollution$TotPollutants<-TotPollutants
  Pollution$concPollutants<-concPollutants
  Pollution$TotPoreWaPoll<-TotPoreWaPoll
  Pollution$concPoreWaPoll<-concPoreWaPoll


  Effluent<-list()
  Effluent$Summer_Treat_Flow<-TreatVec_S*ifelse(WaterTreatVec!=0,MineResults$Watertreated/WaterTreatVec,0) #Multiply by 2 to obtain the half year flow # GHChange - put this back the way it was
  Effluent$Winter_Treat_Flow<-TreatVec_W*ifelse(WaterTreatVec!=0,MineResults$Watertreated/WaterTreatVec,0) #Prevent any possible situation where is not exactly the same as the water vector
  Effluent$Total_Treat_Flow<-MineResults$Watertreated  

  # Water treatment corrected here
  ### Calculate effluent flow from the treatment and the concentrations 

  if(Technology<=1) {
    Effluent$Summer_PostTreatConc = matrix(0, nrow = Nperiods,ncol = 5) 
    Effluent$Winter_PostTreatConc = matrix(0, nrow = Nperiods,ncol = 5) 
    Effluent$Summer_Eff_Flow=rep(0,Nperiods)
    Effluent$Winter_Eff_Flow=rep(0,Nperiods)
  } else if (Technology <= 2) { 

    tech_treat<-OZ_TreatmentEffect(T=T,TreaT=TreaT,NATarget=NAtarget,TreatVec_S=Effluent$Summer_Treat_Flow,TreatVec_W=Effluent$Winter_Treat_Flow,Pollconc=concPollutants,parms=OZ_Parameters)
    Effluent$Summer_PostTreatConc = tech_treat$OZPollconc_S 
    Effluent$Winter_PostTreatConc = tech_treat$OZPollconc_W 
    Effluent$Summer_Eff_Flow=tech_treat$we_S 
    Effluent$Winter_Eff_Flow=tech_treat$we_W
    
  } else if (Technology<=3) { 
    
    tech_treat  <- BCpre_TreatmentEffect(T=T,TreaT=TreaT,NATarget=NAtarget,WL_TreatTime=Decisions[6],BioChar=TRUE, 
                                         TreatVec_S=Effluent$Summer_Treat_Flow,Pollconc=concPollutants,parms=WL_Parameters,parms2=BCpre_Parameters)
    Effluent$Summer_PostTreatConc = tech_treat$Postconc
    Effluent$Winter_PostTreatConc = matrix(0, nrow = Nperiods,ncol = 5) 
    Effluent$Summer_Eff_Flow<-tech_treat$Effluent_Flow
    Effluent$Winter_Eff_Flow<-rep(0,Nperiods)

  } else { # Needs to be updated
    #Decisions=Decisions[1:7], 
    tech_treat <- RO_TreatmentEffect(T=T,TreaT=TreaT,NATarget=NAtarget,TreatVec_S=Effluent$Summer_Treat_Flow, TreatVec_W=Effluent$Winter_Treat_Flow,Pollconc=concPollutants,parms=RO_Parameters)
    Effluent$Summer_PostTreatConc = tech_treat$ROPollconc_S
    Effluent$Winter_PostTreatConc = tech_treat$ROPollconc_W
    Effluent$Summer_Eff_Flow=tech_treat$we_S #GHChanges pulling out the final effluent flows
    Effluent$Winter_Eff_Flow=tech_treat$we_W
  }
  Effluent$Total_Eff_Flow<-Effluent$Summer_Eff_Flow+Effluent$Winter_Eff_Flow
  ## Pollution concentration is built in mine results, assumes the amount of water is treated is taken out

  Effluent$G_a<-G_a
  
  Effluent$Summer_Eff_Flow_m3Sec<-Effluent$Summer_Eff_Flow*2*Mm3Yr_to_m3sec  #GHchanges *2*ifelse(WaterTreatVec!=0,MineResults$Watertreated/WaterTreatVec,0) #Multiply by 2 to obtain the half year flow
  Effluent$Winter_Eff_Flow_m3Sec<-Effluent$Winter_Eff_Flow*2*Mm3Yr_to_m3sec  #GHchanges  *2*ifelse(WaterTreatVec!=0,MineResults$Watertreated/WaterTreatVec,0) #Prevent any possible situation where is not exactly the same as the water vector
  Effluent$Total_Eff_Flow_m3Sec<-Effluent$Total_Eff_Flow*Mm3Yr_to_m3sec  
  
  #Calculate Chronic limits for the effluent flow that we have: Eff_Flow_m3Sec
  
  Effluent$SummerEff_Flow_Limits<-Limits2(G_c,Qe=matrix(Effluent$Summer_Eff_Flow_m3Sec[1:Nperiods],ncol=1),Nperiods,Q_s=Q_s_S)[,,2]
  Effluent$WinterEff_Flow_Limits<-Limits2(G_c,Qe=matrix(Effluent$Winter_Eff_Flow_m3Sec[1:Nperiods],ncol=1),Nperiods,Q_s=Q_s_W)[,,2]
  
  Effluent$SummerEffLim_Violations<-pmax(Effluent$Summer_PostTreatConc-Effluent$SummerEff_Flow_Limits,0)
  cbind(Effluent$Summer_PostTreatConc,Effluent$SummerEff_Flow_Limits,Effluent$Summer_Eff_Flow,Effluent$Summer_Eff_Flow_m3sec)
  Effluent$SummerPenalty_EffLims<-sum(pen_lin*Effluent$SummerEffLim_Violations*ConstrainWeights+pen_quad*(Effluent$SummerEffLim_Violations*ConstrainWeights)^2)
  
  Effluent$WinterEffLim_Violations<-pmax(Effluent$Winter_PostTreatConc-Effluent$WinterEff_Flow_Limits,0)
  Effluent$WinterPenalty_EffLims<-sum(pen_lin*Effluent$WinterEffLim_Violations*ConstrainWeights+pen_quad*(Effluent$WinterEffLim_Violations*ConstrainWeights)^2)
  
  # Calculate the penalties for accute violations (For LTA)
  Effluent$Summer_acute_EffLim_Violations<-pmax(Effluent$Summer_PostTreatConc-matrix(rep(G_a,Nperiods),ncol=length(G_a),byrow=TRUE),0)
  Effluent$Summer_acute_violations_x_flow<-Effluent$Summer_acute_EffLim_Violations*ConstrainWeights*matrix(rep(Effluent$Summer_Eff_Flow_m3Sec,5),ncol=5)
  Effluent$Summer_acute_Penalty_EffLims<-sum(pen_lin*Effluent$Summer_acute_violations_x_flow+pen_quad*Effluent$Summer_acute_violations_x_flow^2)
  
  Effluent$Winter_acute_EffLim_Violations<-pmax(Effluent$Winter_PostTreatConc-matrix(rep(G_a,Nperiods),ncol=length(G_a),byrow=TRUE),0)
  Effluent$Winter_acute_violations_x_flow<-Effluent$Winter_acute_EffLim_Violations*ConstrainWeights*matrix(rep(Effluent$Winter_Eff_Flow_m3Sec,5),ncol=5)
  Effluent$Winter_acute_Penalty_EffLims<-sum(pen_lin*Effluent$Winter_acute_violations_x_flow+pen_quad*Effluent$Winter_acute_violations_x_flow^2)
  
  MineResults$CostPen_StorageVol<-pmax(MineResults$PondWater[1:NProdPeriods]- MaxStorageVolume_Penalty,0)
  MineResults$MaxStorageVolume_Penalty<-sum((StorVol_LinPen*MineResults$CostPen_StorageVol+StorVol_QuadPen*MineResults$CostPen_StorageVol^2)*DiscR[1:NProdPeriods])

  
  ### cost calculations 
    if(Technology<=1) {
      tech_cost=list()
      tech_cost$Disc_TotalCosts=0
      tech_cost$NoTechnology=TRUE
    } else if (Technology <= 2) {

      tech_cost <- OZ_CostFn(Decisions=Decisions,OZ_Parameters,treat=tech_treat,exp_factor=factors)  
    } else if (Technology<=3) {
      tech_cost <- BCp_CostFn(T=T,TreaT=TreaT,WL_TreatTime=Decisions[6],BioChar=TRUE,parms=WL_Parameters,parms2=BCpre_Parameters,treat=tech_treat,exp_factor=factors,TruckCost=0)
      #tech_cost$Disc_TotalCosts
      #tech_cost$m3_cost
    } else {
      tech_cost <- RO_CostFn(Decisions=Decisions,parms=RO_Parameters,treat=tech_treat,exp_factor=factors)
    }
  
  PitLake<-list()

  # Pitlake wetlands 
  PitLake$PitLakeOutflow<-MineResults$PitLakeOutflow

  EndWet_treat<-BCpre_TreatmentEffect(T=(NMinePeriods+1),TreaT=Nperiods,NATarget=Decisions[7],WL_TreatTime=Decisions[5],BioChar=FALSE, 
                        TreatVec_S=MineResults$PitLakeOutflow,Pollconc=concPollutants,parms=WL_Parameters2,parms2=BCpre_Parameters)
  
  PitLake$outflowConc<-EndWet_treat$WLPostconc_BC
  PitLake$PitLakeOutflow<-MineResults$PitLakeOutflow[Last_Year_Treatment:Nperiods]
  
  EndWet_treat$Targetconc
  
  PitLake$Eff_Conc_S <-EndWet_treat$Postconc
  PitLake$Eff_Flow_S<-EndWet_treat$Effluent_Flow
  PitLake$Eff_Flow_S_m3Sec=PitLake$Eff_Flow_S*2*Mm3Yr_to_m3sec
  PitLake$EndWet_treat<-EndWet_treat  
  EndWet_Cost<-BCp_CostFn(T=(NMinePeriods+1),TreaT=Nperiods,WL_TreatTime=Decisions[5],BioChar=FALSE,parms=WL_Parameters2,parms2=BCpre_Parameters,treat=EndWet_treat,exp_factor=factors,TruckCost=0)
  PitLake$DiscCapCosts<- EndWet_Cost$Disc_TotalCosts  
 
  # Modifications:
  Effluent_Con_PL = matrix(rep(0,Nperiods*5),ncol=5,byrow=TRUE)
  Effluent_Con_PL[(Nperiods-NPostPeriodsPitLake+1):(Nperiods),] =  matrix(rep(G_a,NPostPeriodsPitLake),ncol=5,byrow=TRUE)  

  PitLake$acute_EffLim_violations<-pmax(PitLake$Eff_Conc_S-Effluent_Con_PL,0)
  PitLake$acute_Penalty_EffLims<-10000*sum(pen_lin*PitLake$acute_EffLim_violations+pen_quad*PitLake$acute_EffLim_violations^2)
  
  PitLake$Eff_Flow_Limits<-Limits2(G_c,Qe=matrix(PitLake$Eff_Flow_S_m3Sec[(Nperiods-NPostPeriodsPitLake+1):(Nperiods)],ncol=1),NPostPeriodsPitLake,Q_s_S)[,,2]
  
  PitLake$EffLim_Violations<-pmax(PitLake$Eff_Conc_S[(Nperiods-NPostPeriodsPitLake+1):(Nperiods),]-PitLake$Eff_Flow_Limits,0)
  PitLake$Penalty_EffLims<-10000*sum(pen_lin*PitLake$EffLim_Violations+pen_quad*PitLake$EffLim_Violations^2)
  
  if(optimiz==TRUE) { #equals true
    return(tech_cost$Disc_TotalCosts+ 
           PitLake$DiscCapCost+ 
           MineResults$MaxStorageVolume_Penalty-MineResults$WaterPumping+MineResults$Bit_cost+
           Effluent$SummerPenalty_EffLims+Effluent$WinterPenalty_EffLims+
           Effluent$Summer_acute_Penalty_EffLims+Effluent$Winter_acute_Penalty_EffLim+PitLake$acute_Penalty_EffLims+PitLake$Penalty_EffLims)
  } else {
    WaterCostResults<-list()
    WaterCostResults$MineResults<-MineResults
    WaterCostResults$Costs<-tech_cost
    WaterCostResults$ObjectiveFunction<-(tech_cost$Disc_TotalCosts+ # homogenize the total cost structure+sum(PitLake$DiscOperCost)
                                           PitLake$DiscCapCost+     
                                           MineResults$MaxStorageVolume_Penalty-MineResults$WaterPumping+ # MineResults$Bit_cost is out + #WaterPumping is substracted as it comes from the maximization process
                                           Effluent$SummerPenalty_EffLims+Effluent$WinterPenalty_EffLims+
                                           Effluent$Summer_acute_Penalty_EffLims+Effluent$Winter_acute_Penalty_EffLim+PitLake$acute_Penalty_EffLims+PitLake$Penalty_EffLims)
    WaterCostResults$Pollution<-Pollution
    WaterCostResults$Effluent<-Effluent
    WaterCostResults$PitLake<-PitLake
  }
  return(WaterCostResults)
}


type="real-valued"
GA_minqa_bobyqa<-function (type = c("binary", "real-valued", "permutation"), 
          fitness, fitnessMin,..., lower, upper, nBits, population = gaControl(type)$population, 
          selection = gaControl(type)$selection, crossover = gaControl(type)$crossover, 
          mutation = gaControl(type)$mutation, popSize = 50, pcrossover = 0.8, 
          pmutation = 0.1, elitism = base::max(1, round(popSize * 
                                                          0.05)), updatePop = FALSE, postFitness = NULL, maxiter = 100, 
          run = maxiter, maxFitness = Inf, names = NULL, suggestions = NULL, 
          optim = FALSE, optimArgs = list(poptim = 0.05, 
                                          pressel = 0.5, control = list(fnscale = -1, maxit = 100)), 
          keepBest = FALSE, parallel = FALSE, monitor = if (interactive()) gaMonitor else FALSE, 
          seed = NULL) 
{
  call <- match.call()
  type <- match.arg(type, choices = eval(formals(ga)$type))
  if (!is.function(population)) 
    population <- get(population)
  if (!is.function(selection)) 
    selection <- get(selection)
  if (!is.function(crossover)) 
    crossover <- get(crossover)
  if (!is.function(mutation)) 
    mutation <- get(mutation)
  if (missing(fitness)) {
    stop("A fitness function must be provided")
  }
  if (!is.function(fitness)) {
    stop("A fitness function must be provided")
  }
  if (popSize < 10) {
    warning("The population size is less than 10.")
  }
  if (maxiter < 1) {
    stop("The maximum number of iterations must be at least 1.")
  }
  if (elitism > popSize) {
    stop("The elitism cannot be larger that population size.")
  }
  if (pcrossover < 0 | pcrossover > 1) {
    stop("Probability of crossover must be between 0 and 1.")
  }
  if (is.numeric(pmutation)) {
    if (pmutation < 0 | pmutation > 1) {
      stop("If numeric probability of mutation must be between 0 and 1.")
    }
    else if (!is.function(population)) {
      stop("pmutation must be a numeric value in (0,1) or a function.")
    }
  }
  callArgs <- list(...) #callArgs<-list()
  if (any("min" %in% names(callArgs))) {
    lower <- callArgs$min
    callArgs$min <- NULL
    warning("'min' arg is deprecated. Use 'lower' instead.")
  }
  if (any("max" %in% names(callArgs))) {
    upper <- callArgs$max
    callArgs$max <- NULL
    warning("'max' arg is deprecated. Use 'upper' instead.")
  }
  if (missing(lower) & missing(upper) & missing(nBits)) {
    stop("A lower and upper range of values (for 'real-valued' or 'permutation' GA) or nBits (for 'binary' GA) must be provided!")
  }
  switch(type, binary = {
    nBits <- as.vector(nBits)[1]
    lower <- upper <- NA
    nvars <- nBits
    if (is.null(names)) names <- paste0("x", 1:nvars)
  }, `real-valued` = {
    lnames <- names(lower)
    unames <- names(upper)
    lower <- as.vector(lower)
    upper <- as.vector(upper)
    nBits <- NA
    if (length(lower) != length(upper)) stop("lower and upper must be vector of the same length!")
    nvars <- length(upper)
    if (is.null(names) & !is.null(lnames)) names <- lnames
    if (is.null(names) & !is.null(unames)) names <- unames
    if (is.null(names)) names <- paste0("x", 1:nvars)
  }, permutation = {
    lower <- as.vector(lower)[1]
    upper <- as.vector(upper)[1]
    nBits <- NA
    nvars <- length(seq.int(lower, upper))
    if (is.null(names)) names <- paste0("x", 1:nvars)
  })
  if (is.null(suggestions)) {
    suggestions <- matrix(nrow = 0, ncol = nvars)
  } else {
    if (is.vector(suggestions)) {
      if (nvars > 1) 
        suggestions <- matrix(suggestions, nrow = 1)
      else suggestions <- matrix(suggestions, ncol = 1)
    } else {
      suggestions <- as.matrix(suggestions)
    }
    if (nvars != ncol(suggestions)) 
      stop("Provided suggestions (ncol) matrix do not match number of variables of the problem!")
  }
  if (is.logical(monitor)) {
    if (monitor) 
      monitor <- gaMonitor
  }
  if (is.null(monitor)) 
    monitor <- FALSE
  if (optim) {
    optimArgs.default <- eval(formals(ga)$optimArgs)
    optimArgs.default$control[names(optimArgs$control)] <- optimArgs$control
    optimArgs$control <- NULL
    optimArgs.default[names(optimArgs)] <- optimArgs
    optimArgs <- optimArgs.default
    rm(optimArgs.default)
    if (any(optimArgs$method == c("L-BFGS-B", "Brent"))) {
      optimArgs$lower <- lower
      optimArgs$upper <- upper
    }
    else {
      optimArgs$lower <- -Inf
      optimArgs$upper <- Inf
    }
    optimArgs$poptim <- min(max(0, optimArgs$poptim), 1)
    optimArgs$pressel <- min(max(0, optimArgs$pressel), 1)
    optimArgs$control$maxit <- as.integer(optimArgs$control$maxit)
    if (is.null(optimArgs$control$fnscale)) 
      optimArgs$control$fnscale <- -1
    if (optimArgs$control$fnscale > 0) 
      optimArgs$control$fnscale <- -1 * optimArgs$control$fnscale
  }
  if (is.logical(parallel)) {
    if (parallel) {
      parallel <- startParallel(parallel)
      stopCluster <- TRUE
    } else {
      parallel <- stopCluster <- FALSE
    }
  } else {
    stopCluster <- if (inherits(parallel, "cluster")) 
      FALSE
    else TRUE
    parallel <- startParallel(parallel)
  }
  on.exit(if (parallel & stopCluster) stopParallel(attr(parallel, 
                                                        "cluster")))
  `%DO%` <- if (parallel && requireNamespace("doRNG", quietly = TRUE)) {
    doRNG::`%dorng%` 
  } else if (parallel) { 
    `%dopar%`
  } else {`%do%`}
  if (!is.null(seed)) 
    set.seed(seed)
  i. <- NULL
  fitnessSummary <- matrix(as.double(NA), nrow = maxiter, 
                           ncol = 6)
  colnames(fitnessSummary) <- names(gaSummary(rnorm(10)))
  bestSol <- if (keepBest) 
    vector(mode = "list", length = maxiter) else list()

  Fitness <- rep(NA, popSize)
  object <- new("ga", call = call, type = type, lower = lower, 
                upper = upper, nBits = nBits, names = if (is.null(names)) 
                  character()
                else names, popSize = popSize, iter = 0, run = 1, maxiter = maxiter, 
                suggestions = suggestions, population = matrix(), elitism = elitism, 
                pcrossover = pcrossover, pmutation = if (is.numeric(pmutation)) 
                  pmutation
                else NA, optim = optim, fitness = Fitness, summary = fitnessSummary, 
                bestSol = bestSol)
  if (maxiter == 0) 
    return(object)
  Pop <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
  ng <- min(nrow(suggestions), popSize)
  if (ng > 0) {
    Pop[1:ng, ] <- suggestions
  }
  if (popSize > ng) {
    Pop[(ng + 1):popSize, ] <- population(object)[1:(popSize - 
                                                       ng), ]
  }
  object@population <- Pop
  for (iter in seq_len(maxiter)) { #iter=1
    object@iter <- iter
    if (!parallel) {
      for (i in seq_len(popSize)) if (is.na(Fitness[i])) { #i=1
        fit <- do.call(fitness, c(list(Pop[i, ]), callArgs))
        if (updatePop) 
          Pop[i, ] <- attributes(fit)[[1]]
        Fitness[i] <- fit
      }
    } else {
      Fitness <- foreach(i. = seq_len(popSize), .combine = "c") %DO% 
        {
          if (is.na(Fitness[i.])) 
            do.call(fitness, c(list(Pop[i., ]), callArgs))
          else Fitness[i.]
        }
    }
    object@population <- Pop
    object@fitness <- Fitness
    fitnessSummary[iter, ] <- gaSummary(object@fitness)
    object@summary <- fitnessSummary
    if (is.function(monitor)) {
      monitor(object)
    }
    if (optim & (type == "real-valued")) {
      if (optimArgs$poptim > runif(1)) {
        #print(c("optimProbsel call ",popSize,Fitness,optimArgs$pressel))
        i <- sample(1:popSize, size = 1, prob = optimProbsel(Fitness, 
                                                             q = optimArgs$pressel))
        opt <- try(suppressWarnings(do.call(minqa::bobyqa, c(list(fn = fitnessMin, 
                                                                  par = Pop[i, ], 
                                                                  lower = optimArgs$lower, upper = optimArgs$upper, 
                                                                  control = list(iprint=0,maxfun=max(10*length(Pop[i,])^2+1,10000)))))),
                   silent=TRUE)

        if (is.function(monitor)) {
          if (!inherits(opt, "try-error")) 
            cat("\b | Local search =", format((-opt$fval), 
                                              digits = getOption("digits")))
          else cat("\b |", opt[1])
          cat("\n")
        }
        if (!inherits(opt, "try-error")) {
          if(-opt$fval>Fitness[i]) {  
            Pop[i, ] <- opt$par
            Fitness[i] <- (-opt$fval)
          }
        }
        object@population <- Pop
        object@fitness <- Fitness
        fitnessSummary[iter, ] <- gaSummary(object@fitness)
        object@summary <- fitnessSummary
      }
    }
    if (keepBest) {
      object@bestSol[[iter]] <- unique(Pop[Fitness == 
                                             max(Fitness, na.rm = TRUE), , drop = FALSE])
    }
    if (is.function(postFitness)) {
      object <- do.call(postFitness, c(object, callArgs))
      Fitness <- object@fitness
      Pop <- object@population
    }
    if (iter > 1) 
      object@run <- garun(fitnessSummary[seq(iter), 1])
    if (object@run >= run) 
      break
    if (max(Fitness, na.rm = TRUE) >= maxFitness) 
      break
    if (object@iter == maxiter) 
      break
    ord <- order(Fitness, decreasing = TRUE)
    PopSorted <- Pop[ord, , drop = FALSE]
    FitnessSorted <- Fitness[ord]
    if (is.function(selection)) {
      sel <- selection(object)
      Pop <- sel$population
      Fitness <- sel$fitness
    } else {
      sel <- sample(1:popSize, size = popSize, replace = TRUE)
      Pop <- object@population[sel, ]
      Fitness <- object@fitness[sel]
    }
    object@population <- Pop
    object@fitness <- Fitness
    if (is.function(crossover) & pcrossover > 0) {
      nmating <- floor(popSize/2)
      mating <- matrix(sample(1:(2 * nmating), size = (2 * 
                                                         nmating)), ncol = 2)
      for (i in seq_len(nmating)) {
        if (pcrossover > runif(1)) {
          parents <- mating[i, ]
          Crossover <- crossover(object, parents)
          Pop[parents, ] <- Crossover$children
          Fitness[parents] <- Crossover$fitness
        }
      }
      object@population <- Pop
      object@fitness <- Fitness
    }
    pm <- if (is.function(pmutation)) {
      pmutation(object) 
    } else { pmutation}
    if (is.function(mutation) & pm > 0) {
      for (i in seq_len(popSize)) {
        if (pm > runif(1)) {
          Mutation <- mutation(object, i)
          Pop[i, ] <- Mutation
          Fitness[i] <- NA
        }
      }
      object@population <- Pop
      object@fitness <- Fitness
    }
    if (elitism > 0) {
      ord <- order(object@fitness, na.last = TRUE)
      u <- which(!duplicated(PopSorted, margin = 1))
      Pop[ord[1:elitism], ] <- PopSorted[u[1:elitism], 
                                         ]
      Fitness[ord[1:elitism]] <- FitnessSorted[u[1:elitism]]
      object@population <- Pop
      object@fitness <- Fitness
    }
  }
  if (optim & (type == "real-valued")) {
    optimArgs$control$maxit <- rev(optimArgs$control$maxit)[1]
    i <- which.max(object@fitness)
        opt <- try(suppressWarnings(do.call(minqa::bobyqa, c(list(fn = fitnessMin, 
                                                              par = Pop[i, ], 
                                                              lower = optimArgs$lower, upper = optimArgs$upper, 
                                                              control = list(iprint=0))))),
               silent=TRUE)
    
    
    if (is.function(monitor)) {
      if (!inherits(opt, "try-error")) 
        cat("\b | Local search =", format(-opt$fval, 
                                          digits = getOption("digits")))
      else cat("\b |", opt[1])
      cat("\n")
    }
    if (!inherits(opt, "try-error")) {
      if(-opt$fval>Fitness[i]) {  # I put this alteration in here because the original code assume that the local search algorithm gave a better solution than the starting solution and that's not always the case. March 26, 2021
        Pop[i, ] <- opt$par
        Fitness[i] <- (-opt$fval)
      }
    }
  }
  object@summary <- na.exclude(object@summary)
  attr(object@summary, "na.action") <- NULL
  object@fitnessValue <- max(object@fitness, na.rm = TRUE)
  valueAt <- which(object@fitness == object@fitnessValue)
  solution <- object@population[valueAt, , drop = FALSE]
  if (nrow(solution) > 1) {
    eps <- gaControl("eps")
    solution <- unique(round(solution/eps) * eps, margin = 1)
  }
  #colnames(solution) <- parNames(object)
  object@solution <- solution
  if (keepBest) 
    object@bestSol <- object@bestSol[!sapply(object@bestSol, 
                                             is.null)]
  return(object)
}


GA_call_bobyqa_fixedStartTime<-function (type = c("binary", "real-valued", "permutation"), 
                           fitness, fitnessMin,..., lower, upper, nBits, population = gaControl(type)$population, 
                           selection = gaControl(type)$selection, crossover = gaControl(type)$crossover, 
                           mutation = gaControl(type)$mutation, popSize = 50, pcrossover = 0.8, 
                           pmutation = 0.1, elitism = base::max(1, round(popSize * 
                                                                           0.05)), updatePop = FALSE, postFitness = NULL, maxiter = 100, 
                           run = maxiter, maxFitness = Inf, names = NULL, suggestions = NULL, 
                           optim = FALSE, optimArgs = list(poptim = 0.05, 
                                                           pressel = 0.5, control = list(fnscale = -1, maxit = 100)), 
                           keepBest = FALSE, parallel = FALSE, monitor = if (interactive()) gaMonitor else FALSE, 
                           seed = NULL) 
{
  call <- match.call()
  type <- match.arg(type, choices = eval(formals(ga)$type))
  if (!is.function(population)) 
    population <- get(population)
  if (!is.function(selection)) 
    selection <- get(selection)
  if (!is.function(crossover)) 
    crossover <- get(crossover)
  if (!is.function(mutation)) 
    mutation <- get(mutation)
  if (missing(fitness)) {
    stop("A fitness function must be provided")
  }
  if (!is.function(fitness)) {
    stop("A fitness function must be provided")
  }
  if (popSize < 10) {
    warning("The population size is less than 10.")
  }
  if (maxiter < 1) {
    stop("The maximum number of iterations must be at least 1.")
  }
  if (elitism > popSize) {
    stop("The elitism cannot be larger that population size.")
  }
  if (pcrossover < 0 | pcrossover > 1) {
    stop("Probability of crossover must be between 0 and 1.")
  }
  if (is.numeric(pmutation)) {
    if (pmutation < 0 | pmutation > 1) {
      stop("If numeric probability of mutation must be between 0 and 1.")
    }
    else if (!is.function(population)) {
      stop("pmutation must be a numeric value in (0,1) or a function.")
    }
  }
  callArgs <- list(...) #callArgs<-list()
  if (any("min" %in% names(callArgs))) {
    lower <- callArgs$min
    callArgs$min <- NULL
    warning("'min' arg is deprecated. Use 'lower' instead.")
  }
  if (any("max" %in% names(callArgs))) {
    upper <- callArgs$max
    callArgs$max <- NULL
    warning("'max' arg is deprecated. Use 'upper' instead.")
  }
  if (missing(lower) & missing(upper) & missing(nBits)) {
    stop("A lower and upper range of values (for 'real-valued' or 'permutation' GA) or nBits (for 'binary' GA) must be provided!")
  }
  switch(type, binary = {
    nBits <- as.vector(nBits)[1]
    lower <- upper <- NA
    nvars <- nBits
    if (is.null(names)) names <- paste0("x", 1:nvars)
  }, `real-valued` = {
    lnames <- names(lower)
    unames <- names(upper)
    lower <- as.vector(lower)
    upper <- as.vector(upper)
    nBits <- NA
    if (length(lower) != length(upper)) stop("lower and upper must be vector of the same length!")
    nvars <- length(upper)
    if (is.null(names) & !is.null(lnames)) names <- lnames
    if (is.null(names) & !is.null(unames)) names <- unames
    if (is.null(names)) names <- paste0("x", 1:nvars)
  }, permutation = {
    lower <- as.vector(lower)[1]
    upper <- as.vector(upper)[1]
    nBits <- NA
    nvars <- length(seq.int(lower, upper))
    if (is.null(names)) names <- paste0("x", 1:nvars)
  })
  if (is.null(suggestions)) {
    suggestions <- matrix(nrow = 0, ncol = nvars)
  } else {
    if (is.vector(suggestions)) {
      if (nvars > 1) 
        suggestions <- matrix(suggestions, nrow = 1)
      else suggestions <- matrix(suggestions, ncol = 1)
    } else {
      suggestions <- as.matrix(suggestions)
    }
    if (nvars != ncol(suggestions)) 
      stop("Provided suggestions (ncol) matrix do not match number of variables of the problem!")
  }
  if (is.logical(monitor)) {
    if (monitor) 
      monitor <- gaMonitor
  }
  if (is.null(monitor)) 
    monitor <- FALSE
  if (optim) {
    optimArgs.default <- eval(formals(ga)$optimArgs)
    optimArgs.default$control[names(optimArgs$control)] <- optimArgs$control
    optimArgs$control <- NULL
    optimArgs.default[names(optimArgs)] <- optimArgs
    optimArgs <- optimArgs.default
    rm(optimArgs.default)
    if (any(optimArgs$method == c("L-BFGS-B", "Brent"))) {
      optimArgs$lower <- lower
      optimArgs$upper <- upper
    }
    else {
      optimArgs$lower <- -Inf
      optimArgs$upper <- Inf
    }
    optimArgs$poptim <- min(max(0, optimArgs$poptim), 1)
    optimArgs$pressel <- min(max(0, optimArgs$pressel), 1)
    optimArgs$control$maxit <- as.integer(optimArgs$control$maxit)
    if (is.null(optimArgs$control$fnscale)) 
      optimArgs$control$fnscale <- -1
    if (optimArgs$control$fnscale > 0) 
      optimArgs$control$fnscale <- -1 * optimArgs$control$fnscale
  }
  if (is.logical(parallel)) {
    if (parallel) {
      parallel <- startParallel(parallel)
      stopCluster <- TRUE
    } else {
      parallel <- stopCluster <- FALSE
    }
  } else {
    stopCluster <- if (inherits(parallel, "cluster")) 
      FALSE
    else TRUE
    parallel <- startParallel(parallel)
  }
  on.exit(if (parallel & stopCluster) stopParallel(attr(parallel, 
                                                        "cluster")))
  `%DO%` <- if (parallel && requireNamespace("doRNG", quietly = TRUE)) {
    doRNG::`%dorng%` 
  } else if (parallel) { 
    `%dopar%`
  } else {`%do%`}
  if (!is.null(seed)) 
    set.seed(seed)
  i. <- NULL
  fitnessSummary <- matrix(as.double(NA), nrow = maxiter, 
                           ncol = 6)
  colnames(fitnessSummary) <- names(gaSummary(rnorm(10)))
  bestSol <- if (keepBest) 
    vector(mode = "list", length = maxiter) else list()
  
  Fitness <- rep(NA, popSize)
  object <- new("ga", call = call, type = type, lower = lower, 
                upper = upper, nBits = nBits, names = if (is.null(names)) 
                  character()
                else names, popSize = popSize, iter = 0, run = 1, maxiter = maxiter, 
                suggestions = suggestions, population = matrix(), elitism = elitism, 
                pcrossover = pcrossover, pmutation = if (is.numeric(pmutation)) 
                  pmutation
                else NA, optim = optim, fitness = Fitness, summary = fitnessSummary, 
                bestSol = bestSol)
  if (maxiter == 0) 
    return(object)
  Pop <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
  ng <- min(nrow(suggestions), popSize)
  if (ng > 0) {
    Pop[1:ng, ] <- suggestions
  }
  if (popSize > ng) {
    Pop[(ng + 1):popSize, ] <- population(object)[1:(popSize - 
                                                       ng), ]
  }
  object@population <- Pop
  for (iter in seq_len(maxiter)) { #iter=1
    object@iter <- iter
    if (!parallel) {
      for (i in seq_len(popSize)) if (is.na(Fitness[i])) { #i=1
        fit <- do.call(fitness, c(list(Pop[i, ]), callArgs))
        if (updatePop) 
          Pop[i, ] <- attributes(fit)[[1]]
        Fitness[i] <- fit
      }
    } else {
      Fitness <- foreach(i. = seq_len(popSize), .combine = "c") %DO% 
        {
          if (is.na(Fitness[i.])) 
            do.call(fitness, c(list(Pop[i., ]), callArgs))
          else Fitness[i.]
        }
    }
    object@population <- Pop
    object@fitness <- Fitness
    fitnessSummary[iter, ] <- gaSummary(object@fitness)
    object@summary <- fitnessSummary
    if (is.function(monitor)) {
      monitor(object)
    }
    if (optim & (type == "real-valued")) {
      if (optimArgs$poptim > runif(1)) {
        i <- sample(1:popSize, size = 1, prob = optimProbsel(Fitness, 
                                                             q = optimArgs$pressel))

         opt <- try(suppressWarnings(do.call(call_bobyqa_fixedStartTime, c(list(fn = fitnessMin, 
                                                                    start_par = Pop[i, ], 
                                                                    bobyqa_lower = optimArgs$lower, bobyqa_upper = optimArgs$upper, 
                                                                    bobyqa_control = list(iprint=0,maxfun=max(10*length(Pop[i,])^2+1,10000)))))),
                     silent=TRUE)
        
        if (is.function(monitor)) {
          if (!inherits(opt, "try-error")) 
            cat("\b | Local search =", format((-opt$fval), 
                                              digits = getOption("digits")))
          else cat("\b |", opt[1])
          cat("\n")
        }
        if (!inherits(opt, "try-error")) {
          if(-opt$fval>Fitness[i]) {  
            Pop[i, ] <- opt$par
            Fitness[i] <- (-opt$fval)
          }
        }
        object@population <- Pop
        object@fitness <- Fitness
        fitnessSummary[iter, ] <- gaSummary(object@fitness)
        object@summary <- fitnessSummary
      }
    }
    if (keepBest) {
      object@bestSol[[iter]] <- unique(Pop[Fitness == 
                                             max(Fitness, na.rm = TRUE), , drop = FALSE])
    }
    if (is.function(postFitness)) {
      object <- do.call(postFitness, c(object, callArgs))
      Fitness <- object@fitness
      Pop <- object@population
    }
    if (iter > 1) 
      object@run <- garun(fitnessSummary[seq(iter), 1])
    if (object@run >= run) 
      break
    if (max(Fitness, na.rm = TRUE) >= maxFitness) 
      break
    if (object@iter == maxiter) 
      break
    ord <- order(Fitness, decreasing = TRUE)
    PopSorted <- Pop[ord, , drop = FALSE]
    FitnessSorted <- Fitness[ord]
    if (is.function(selection)) {
      sel <- selection(object)
      Pop <- sel$population
      Fitness <- sel$fitness
    } else {
      sel <- sample(1:popSize, size = popSize, replace = TRUE)
      Pop <- object@population[sel, ]
      Fitness <- object@fitness[sel]
    }
    object@population <- Pop
    object@fitness <- Fitness
    if (is.function(crossover) & pcrossover > 0) {
      nmating <- floor(popSize/2)
      mating <- matrix(sample(1:(2 * nmating), size = (2 * 
                                                         nmating)), ncol = 2)
      for (i in seq_len(nmating)) {
        if (pcrossover > runif(1)) {
          parents <- mating[i, ]
          Crossover <- crossover(object, parents)
          Pop[parents, ] <- Crossover$children
          Fitness[parents] <- Crossover$fitness
        }
      }
      object@population <- Pop
      object@fitness <- Fitness
    }
    pm <- if (is.function(pmutation)) {
      pmutation(object) 
    } else { pmutation}
    if (is.function(mutation) & pm > 0) {
      for (i in seq_len(popSize)) {
        if (pm > runif(1)) {
          Mutation <- mutation(object, i)
          Pop[i, ] <- Mutation
          Fitness[i] <- NA
        }
      }
      object@population <- Pop
      object@fitness <- Fitness
    }
    if (elitism > 0) {
      ord <- order(object@fitness, na.last = TRUE)
      u <- which(!duplicated(PopSorted, margin = 1))
      Pop[ord[1:elitism], ] <- PopSorted[u[1:elitism], 
      ]
      Fitness[ord[1:elitism]] <- FitnessSorted[u[1:elitism]]
      object@population <- Pop
      object@fitness <- Fitness
    }
  }
  if (optim & (type == "real-valued")) {
    optimArgs$control$maxit <- rev(optimArgs$control$maxit)[1]
    i <- which.max(object@fitness)

    opt <- try(suppressWarnings(do.call(call_bobyqa_fixedStartTime, c(list(fn = fitnessMin, 
                                                                           start_par = Pop[i, ], 
                                                                           bobyqa_lower = optimArgs$lower, bobyqa_upper = optimArgs$upper, 
                                                                           bobyqa_control = list(iprint=0,maxfun=max(10*length(Pop[i,])^2+1,10000)))))),
               silent=TRUE)

    if (is.function(monitor)) {
      if (!inherits(opt, "try-error")) 
        cat("\b | Local search =", format(-opt$fval, 
                                          digits = getOption("digits")))
      else cat("\b |", opt[1])
      cat("\n")
    }
    if (!inherits(opt, "try-error")) {
      if(-opt$fval>Fitness[i]) {  
        Pop[i, ] <- opt$par
        Fitness[i] <- (-opt$fval)
      }
    }
  }
  object@summary <- na.exclude(object@summary)
  attr(object@summary, "na.action") <- NULL
  object@fitnessValue <- max(object@fitness, na.rm = TRUE)
  valueAt <- which(object@fitness == object@fitnessValue)
  solution <- object@population[valueAt, , drop = FALSE]
  if (nrow(solution) > 1) {
    eps <- gaControl("eps")
    solution <- unique(round(solution/eps) * eps, margin = 1)
  }
  
  object@solution <- solution
  if (keepBest) 
    object@bestSol <- object@bestSol[!sapply(object@bestSol, 
                                             is.null)]
  return(object)
}

##### Code to examine the output of CallMineWaterSystemFn  ######3
Examine_MineWaterSystemFn_Out<-function(){
  VarNames[AMatbc$VariableMarkers]
  rownames(AMatbc$AMat)[AMatbc$ConstrMarkers]
  
  options(scipen=99,max.print=800000)
  printSpMatrix(Obj_A_bc_xx2,col.names=TRUE,digits=3)
  printSpMatrix(ConeData$FMat[1:3,],col.names=TRUE,digits=3)
  
  
  rw<-length(Obj_A_bc_xx2[,1])
  rg<-(rw-4):rw
  rg<-c(1,rg)
  printSpMatrix(t(Obj_A_bc_xx2[rg,])[,],col.names=TRUE,digits=3)
  # ratio wr to wd and rp to wd
  r$sol$itr$xx[Nperiods*2+1:(Nperiods)]/r$sol$itr$xx[Nperiods*16+1:Nperiods]
  r$sol$itr$xx[Nperiods*21+1:Nperiods]/r$sol$itr$xx[Nperiods*16+1:Nperiods]
  
  Soln1<-Obj_A_bc_xx2
  printSpMatrix(t(rbind(Obj_A_bc_xx2[rg,],Soln1[rg,]))[,],col.names=TRUE,digits=3)
  printSpMatrix(t(rbind(MineResults$FullLinMatrixWithSoln[rg,],Soln1[rg,]))[,],col.names=TRUE,digits=3)
  printSpMatrix(MineResults$FullLinMatrixWithSoln,col.names=TRUE,digits=3)
  
  
  MineResults$FullLinMatrixWithSoln
  noquote(cbind(Obj_Row,soln_row))[]
  rownames(AMatbc$A)
  AMatbc$ConstrMarkers
  
  VarNames[AMatbc$VariableMarkers]
  rownames(AMatbc$AMat)[AMatbc$ConstrMarkers]
  constrToShow<-c(8,13)
  nRows<-40
  VarToShow<-c(1,2,3,4,5,6)
  NCols<-40
  
  rowrg<-as.vector(sapply(AMatbc$ConstrMarkers[constrToShow],seq,length.out=nRows))
  rowrg<-c(1,rowrg+2,(nrow(Obj_A_bc_xx2)-4):nrow(Obj_A_bc_xx2))
  colrg<-c(as.vector(sapply(AMatbc$VariableMarkers[VarToShow],seq,length.out=NCols)),(ncol(Obj_A_bc_xx2)-1):ncol(Obj_A_bc_xx2))
  
  noquote(colnames(Obj_A_bc_xx2)[colrg])
  noquote(rownames(Obj_A_bc_xx2)[rowrg])
  
  printSpMatrix(Obj_A_bc_xx2[rowrg,colrg],col.names=TRUE,digits=3)
}

##################### End of module 1  ######


