# Module 2 - Technology cost and effectiveness functions

# Description: 
# This module sets the cost functions of three treatments technologies: Ozonation (OZ), Reverse Osmosis (RO) and Wetlands and Biochar (WL_BC)
# for the production period. In the post production period the model only considers Wetlands and Biochar (WL_BC). 
# The functions transform the costs to 2020 CAD and also considers the effects on each of them in the pollutants

# Use:
# Press "Ctrl+A" to select all the code and then press "run" or "Ctrl+Enter" to create cost and effectiveness functions for each technology.
# There is no need for further action in this module



#---------------------------------------

## Wetland effectiveness
Effectiveness=0.026# 0.026 low / 0.043 medium / 0.056 high

#install.packages("dplyr")
library(SciViews)
library(rootSolve)
library("dplyr")
library(reshape2)
library(matrixStats)
library(ggplot2)

#### Variables from Water, Pollution and Core model ###

### New Decisions vector #
  
  #1 Inflow_S = OSPW inflow in Summer (Mm3) [,10] Mm3
  #2 Inflow_W = OSPW inflow in Winter (Mm3) [,10] Mm3
  #3 STreat = Start treatment time (Year) [,50] year
  #4 DTreat = Duration of treatment (Years) [,60] year
  #5 End treatment time = Wetland treatment day for endpitlake [,80] days
  #6 Wet_Tdays = Wetland treatment days (days)  [,80] days
  #7 End of pipe NAs target (Target) [,G_a]
  #8 Technology = 0-1 OZ, 1-2 RO, 2-3 WL_BC

  DecisionVariables <- c(6,3,10,20,10,3,5,1)
  
  # Recycling ratios are no longer decision variables
  
######### New global parameters #####



## Interest rate
  d_rate=0.06
    
## Expansion factors 
factors = c(3,3.42,1)

#Machine numerical limit GHchanges
epsilon= 1e-8 #.Machine$double.eps
  
  
### Function inputs for testing cost functions
{
    Nperiods = 70 #50 + 10 of regulatory period and + 20 to make sure is reclaimed (assuming it was a pitlake)
    NMinePeriods = 50  
    Lenght_of_Treatment = 20 # Years of treatment for technology and package (Decision variable)
    Last_Year_Treatment = Nperiods-10 # Constraint for when to treat following the regulation period (Ready to Reclaim period) 
    # Make sure that the program deals with a number that goes outside of the contraint. if (start + length > last_year_of_treatment, then last_year_of_treatment)
    Rate =d_rate
    DiscRate=1/d_rate
    DiscR <- DiscRate^((1-1):(Nperiods-1))
    EX_12 = 0.99
    PPP_12_CAD = 1.245
    # We had a different as year 1 had a  0 interest rate, now year 1 has an interest rate of 1
    Wage_batea = 80   #Batea wage CAD
    Wage_source = 48.02 #Source wage USD
    #Year 1 should be 1
    
    day_y = 350 # days in a year
    
    # Timing
    if (DecisionVariables[3]+DecisionVariables[4]>Last_Year_Treatment) {
      DecisionVariables2[4]=Last_Year_Treatment-DecisionVariables[4]
    }
    T<-trunc(DecisionVariables[3])+1 # Time to starts (T is in years)
    TreaT <- min(trunc(DecisionVariables[3]+DecisionVariables[4]+1),Last_Year_Treatment) # Year where treatment ends / Truncation capture the full years

    TreatVec_S<-rep(0,Nperiods)
    TreatVec_W<-rep(0,Nperiods)
    TreatVec_S[T:TreaT]<-DecisionVariables[1] 
    TreatVec_W[T:TreaT]<-DecisionVariables[2] 
    Tf<-T-DecisionVariables[3]
    TreatVec_S[T]<-Tf*DecisionVariables[1]
    TreatVec_W[T]<-Tf*DecisionVariables[2]
    TreaTf<-(DecisionVariables[3]+DecisionVariables[4]+1)-TreaT
    TreatVec_S[TreaT]<-TreaTf*DecisionVariables[1]
    TreatVec_W[TreaT]<-TreaTf*DecisionVariables[2]
    
  WaterTreatVec=TreatVec_S+TreatVec_W
  
  
  # End pit lake flow
  # Temporary 
  
  TreatVec_S_End<-rep(0,Nperiods)
  TreatVec_W_End<-rep(0,Nperiods)
  TreatVec_S_End[Last_Year_Treatment:Nperiods]<-4 
  TreatVec_W_End[Last_Year_Treatment:Nperiods]<-0 
  WaterTreatVec_End=TreatVec_S_End+TreatVec_W_End
   
  }
  
### Pollution vector
{
    PollconcInit = c(2000,800,30.8,0.0001,20)  
  # Initial concentration (TDS, Chlorides (From Pamela's data), NAs, Cadmium, Ammonia) 
    concPollutants<-matrix(0, nrow = Nperiods,ncol = 5)
    concPollutants[T:TreaT,1]<-PollconcInit[1]
    concPollutants[T:TreaT,2]<-PollconcInit[2]
    concPollutants[T:TreaT,3]<-PollconcInit[3]
    concPollutants[T:TreaT,4]<-PollconcInit[4]
    concPollutants[T:TreaT,5]<-PollconcInit[5]
    
    concPollutants_End<-matrix(0, nrow = Nperiods,ncol = 5)
    concPollutants_End[Last_Year_Treatment:Nperiods,1]<-PollconcInit[1]
    concPollutants_End[Last_Year_Treatment:Nperiods,2]<-PollconcInit[2]
    concPollutants_End[Last_Year_Treatment:Nperiods,3]<-PollconcInit[3]
    concPollutants_End[Last_Year_Treatment:Nperiods,4]<-PollconcInit[4]
    concPollutants_End[Last_Year_Treatment:Nperiods,5]<-PollconcInit[5]
  }


# Cons_target = NAs concentration target (mg/L) - Constraint
{  
  Targetconc = c(2500,0,5,0,0)  # Target concentration (TDS, Chlorides, NAs, Cadmium, Ammonia)
  TargetPollutants<-matrix(0, nrow = Nperiods,ncol = 5)
  TargetPollutants[T:TreaT,1]<-Targetconc[1]
  TargetPollutants[T:TreaT,3]<-Targetconc[3]
  
  TargetPollutants_End<-matrix(0, nrow = Nperiods,ncol = 5)
  TargetPollutants_End[Last_Year_Treatment:Nperiods,1]<-Targetconc[1]
  TargetPollutants_End[Last_Year_Treatment:Nperiods,3]<-Targetconc[3]
}

  
  
######### Technologies #####
  
### OZONATION ## (Sharma et al. (2013), BATEA, CONRAD)
{
Setup_OZ_Parameters<-function(interest_rate= d_rate)
  { 
    ################### OZ Specs and parameters #######################
    OZ=list()
     #Conversion Factors# 
    OZ$USgal_m3=264.17 #US gallon per m3
    OZ$m3_USgas=0.003785412 #m3 per US gallon
    OZ$AcrePerHa=2.47105382
    OZ$m2_ft2 = 0.09290304 #m2 per ft2
    
    #Value of Time variables 
    Ammort_Period     = 15 # Years
    OZ$PW = (1-1/(1+interest_rate)^Ammort_Period)/interest_rate# Present worth factor
    
    #Base Case parameters for RO
    InflowYr          = 6000000               #m3/yr 
    InflowDay         = InflowYr/(365)      #m3/day
    InflowHr          = InflowYr/(365*24)   #m3/hr
    InflowSec         = InflowDay/(24*60*60)  #m3/sec
    InfYrM             =  InflowYr/1000000      #Mm3/yr
    
    # Ozonation Design Assumptions: Total equipment cost
    CPI_11 = 224.94 #CPI for 2011 (Average)
    CPI_12 = 229.54 #CPI for 2012 (Average)
    CPI_22 = 291.73 #CPI for 2022 (Average) (until October)
    OZ$Adj_f = CPI_12/CPI_11 #Adjustment factor
    OZ$Adj_f2 = CPI_12/CPI_22 #Adjustment factor
    O_dmg = 2*48 # Ozone dose - mg/L
    O_dkg = O_dmg/1000 # Ozone dose - kg/m3
    O_gc_kg = O_dkg*InflowDay    # Ozone generation capacity - kg/d
    O_gc_lb = O_gc_kg*2.2 # Ozone generation capacity - lb/d
    CT = 15 # Contact time - minutes
    CC_m3 = 104.2 # Contact Chamber Volume - m3
    CC_ft3 = CC_m3*35.3 # Contact Chamber Volume - ft3
    
    OZ$time = 15 # treatment time in minutes
    OZ$coef = 0.865 # to make it close to the linear approximation (change it parameter)
    
    # Water loss rate
    OZ$WaterLossRate= c((3000-2941)/3000,(3000-2941)/3000)   # From DAP - AOP BATEA REPORT
    
    # Recycle Ratio for summer and winter
    OZ$RecycleRatio=c(0,0)
    
    # Assumed reduction
    Infl_Poll_conc<-c(2000,200,44,0.0001,20)   #mg/L
    Effl_Poll_conc<-c(2091,100,0.9,0.00007,15)   #mg/L  

    
    return(OZ)
      }

OZ_Parameters<-Setup_OZ_Parameters(interest_rate= d_rate)
 
### Treatment OZ ###
OZ_TreatmentEffect<-function(T=T,TreaT=TreaT,NATarget=DecisionVariables[7],TreatVec_S=TreatVec_S, TreatVec_W=TreatVec_W,Pollconc=concPollutants, parms=OZ_Parameters) {
    R<-list()
    R$Targetconc= NATarget # Regarding NA Target 

    R$theta_S=1-parms$WaterLossRate[1] ## Assuming different Water loss rate over seasons
    R$theta_W=1-parms$WaterLossRate[2] ## Assuming different Water loss rate over seasons

    ### For capital cost ###
    R$max_Pollc = max(Pollconc[,3]) # max pollution concentration (Initial concentration for capital) (beyond 80 there is no solution)
    R$p_req_max = min(R$Targetconc/R$max_Pollc,1) # End result as compared to initial (to avoid treating when the concentration is lower)
                                                  # Required remaining proportion to treat the maximum concentration for the given target
   ### New formula 
    coeff = -0.0185548873912615 #Dose coefficient
    
    l_p_req = ln(R$p_req_max) # Log of ratio of outcome to initial
    R$o_dose = min(c(l_p_req/coeff,210)) # Initial level to reaction
    
    R$final_Conc= exp(R$o_dose*coeff)*R$max_Pollc
    
    #####

    Req_init_max=0
    #####
    
    if (R$final_Conc>R$Targetconc) {
      Req_init_max = R$Targetconc/exp(R$o_dose*coeff) #cwf
      # wr = (cw*w- cwf*w)/ (cwf-cwr)
      max_wr_s = max(TreatVec_S)*(R$max_Pollc-Req_init_max)/(Req_init_max-R$Targetconc) # Recycled water flow Summer
      max_wr_w = max(TreatVec_W)*(R$max_Pollc-Req_init_max)/(Req_init_max-R$Targetconc) # Recycled water flow Winter
      # wf=w+wr
      max_wf_s = max(TreatVec_S)+ max_wr_s
      max_wf_w = max(TreatVec_W)+ max_wr_w
      # α = wr/θwf
      max_rr_s = max_wr_s/(R$theta_S*max_wf_s)
      max_rr_w = max_wr_w/(R$theta_W*max_wf_w)  
    } else {
      max_rr_s = 0
      max_rr_w = 0
      max_wf_s = max(TreatVec_S) 
      max_wf_w = max(TreatVec_W)
    }
    
    # Maxium required doses 
    R$ratio_OZ = 1 # OGC ratio considering Chris comment regarding the actual amount of Ozone needed to reduce toxicity
    R$max_ogc_lb_S = R$ratio_OZ*(R$o_dose/1000*max_wf_s*(1e6/day_y*2*2.2)) # Maximum Required Ozone generation capacity per day SUMMER #lb/day (Way bigger than the model constraint)
    R$max_ogc_lb_W = R$ratio_OZ*(R$o_dose/1000*max_wf_w*(1e6/day_y*2*2.2)) # Maximum Required Ozone generation capacity per day WINTER #lb/day
    
    
    ### For operational cost ###
    
    # Water treatment vectors #

    # For NAs
    R$max_dose<-matrix(0, nrow = Nperiods,ncol = 1) #Outcome concentration
    R$max_dose[T:TreaT,1] = 210
    
    l_v_p_remain = ln(pmin(R$Targetconc/Pollconc[,3],1)) # Set the reduction target rate for NAs concentration
    
    R$dose_v = pmin(l_v_p_remain/coeff,R$max_dose) # Dose of vectors 
    # after an initial concentration of 37, the dose will be the max
    
    p_remain = Pollconc[,3]*ifelse(Pollconc[,3]>R$Targetconc,exp(R$dose_v*coeff),1) 
    #Concentration remaining per each vector dose if they are higher than the target
    
    Req_init = ifelse(p_remain>R$Targetconc+epsilon,R$Targetconc/exp(R$o_dose*coeff),Pollconc[,3]) #cwf
    
    wr_s = TreatVec_S*(Pollconc[,3]-Req_init)/(Req_init_max-R$Targetconc) # Recycled water flow Summer
    wr_w = TreatVec_W*(Pollconc[,3]-Req_init)/(Req_init_max-R$Targetconc) # Recycled water flow Winter
  
    # wf=w+wr
    wf_s = TreatVec_S+ wr_s
    wf_w = TreatVec_W+ wr_w
    # α = wr/  θwf
    R$rr_S = ifelse(wf_s!=0,wr_s/(R$theta_S*wf_s),0) # Estimates recycling ratio summer
    R$rr_W = ifelse(wf_w!=0,wr_w/(R$theta_S*wf_w),0) # Estimates recycling ratio winter
    
    p_remain_rr = Req_init*ifelse(Pollconc[,3]>R$Targetconc,exp(R$dose_v*coeff),1) # Reached level after each optimal treatment for NAs
        
    #Summer concentration
    R$OZPollconc_S<-matrix(0, nrow = Nperiods,ncol = 5) #Outcome concentration
    R$OZPollconc_S[T:TreaT,1] = parms$ratePoll_reduc[1]*Pollconc[T:TreaT,1]*(1-R$theta_S*R$rr_S[T:TreaT])/(R$theta_S)*(1-parms$ratePoll_reduc[1]*R$rr_S[T:TreaT]) # Reached level after each optimal treatment for TDS
    R$OZPollconc_S[T:TreaT,2] = parms$ratePoll_reduc[2]*Pollconc[T:TreaT,2]*(1-R$theta_S*R$rr_S[T:TreaT])/(R$theta_S)*(1-parms$ratePoll_reduc[2]*R$rr_S[T:TreaT]) # Chloride
    R$OZPollconc_S[T:TreaT,3] = p_remain_rr[T:TreaT] # Reached level after each optimal treatment for NAs
    R$OZPollconc_S[T:TreaT,4] = parms$ratePoll_reduc[4]*Pollconc[T:TreaT,4]*(1-R$theta_S*R$rr_S[T:TreaT])/(R$theta_S)*(1-parms$ratePoll_reduc[4]*R$rr_S[T:TreaT]) # Ammonia
    R$OZPollconc_S[T:TreaT,5] = parms$ratePoll_reduc[5]*Pollconc[T:TreaT,5]*(1-R$theta_S*R$rr_S[T:TreaT])/(R$theta_S)*(1-parms$ratePoll_reduc[5]*R$rr_S[T:TreaT]) # Cadmium
    
    #Winter concentration
    R$OZPollconc_W<-matrix(0, nrow = Nperiods,ncol = 5) #Outcome concentration
    R$OZPollconc_W[T:TreaT,1] = parms$ratePoll_reduc[1]*Pollconc[T:TreaT,1]*(1-R$theta_W*R$rr_W[T:TreaT])/(R$theta_W)*(1-parms$ratePoll_reduc[1]*R$rr_W[T:TreaT]) # Reached level after each optimal treatment for TDS
    R$OZPollconc_W[T:TreaT,2] = parms$ratePoll_reduc[2]*Pollconc[T:TreaT,2]*(1-R$theta_W*R$rr_W[T:TreaT])/(R$theta_W)*(1-parms$ratePoll_reduc[2]*R$rr_W[T:TreaT]) # Chloride
    R$OZPollconc_W[T:TreaT,3] = p_remain_rr[T:TreaT] # Reached level after each optimal treatment for NAs
    R$OZPollconc_W[T:TreaT,4] = parms$ratePoll_reduc[4]*Pollconc[T:TreaT,4]*(1-R$theta_W*R$rr_W[T:TreaT])/(R$theta_W)*(1-parms$ratePoll_reduc[4]*R$rr_W[T:TreaT]) # Ammonia
    R$OZPollconc_W[T:TreaT,5] = parms$ratePoll_reduc[5]*Pollconc[T:TreaT,5]*(1-R$theta_W*R$rr_W[T:TreaT])/(R$theta_W)*(1-parms$ratePoll_reduc[5]*R$rr_W[T:TreaT]) # Cadmium
    
    ## Summer effluent ##
    R$we_S=TreatVec_S*R$theta_S*(1-R$rr_S)/(1-R$rr_S*R$theta_S)  # calculation of the final effluent flow into the river
    R$wtflow_S=TreatVec_S/(1-R$theta_S*R$rr_S) # amount of water going through the treatment system
    R$wr_S=R$theta_S*R$rr_S*R$wtflow_S  # amount of water recycled is the amount of water diverted from the effluent back into the system
    
    ## Winter effluent ##
    R$we_W=TreatVec_W*R$theta_W*(1-R$rr_W)/(1-R$rr_W*R$theta_W)  # calculation of the final effluent flow into the river
    R$wtflow_W=TreatVec_W/(1-R$theta_W*R$rr_W) # amount of water going through the treatment system
    R$wr_W=R$theta_W*R$rr_W*R$wtflow_W  # amount of water recycled is the amount of water diverted from the effluent back into the system

    ## Yearly water on treatment ##
    R$wtflow_T = R$wtflow_S+R$wtflow_W # wf_s + wf_w

    R$OGC_S = R$dose_v/1000*(1e6/day_y*2.2*2)*R$wtflow_S  
    R$OGC_W = R$dose_v/1000*(1e6/day_y*2.2*2)*R$wtflow_W
    
    return(R)
  }

  
### OZ Cost ###
OZ_CostFn <- function(Decisions=DecisionVariables[1:7],parms=OZ_Parameters,treat=OZ_treat,exp_factor=factors[1:3]){
  R=list()
  
    R$Exp_factor = exp_factor[1] # Expansion factor for a Oil Sand company to multiply by cost
    R$Exp_Cap =exp_factor[2] #factor capital cost
    R$Exp_Op =exp_factor[3] #factor ope cost

 
    ## 1. Raw water pumping (TDH = 30.48m  (100 ft))## 
    # For the assumption in the oil sands it seems that the TDH will not exceed 30.48
    {
    R$mgd_f = 0 # mgd capacity floor (from model) // Actual minimum 1 
    R$mgd_l = 200 # mgd capacity limit (from model)
    R$r_mgd = max(c(treat$wtflow_S,treat$wtflow_W))*1e6/(day_y/2)*parms$USgal_m3/1e6 # Required water treated in gallons per day 
    R$a_mgd = max(c(R$mgd_f,min(c(R$r_mgd,R$mgd_l)))) # Allowed mgd per day in the system (if is lower than the floor, the floor will be used)

    R$ScaleFactor_wp = (R$r_mgd/min(R$r_mgd, R$a_mgd))^parms$coef #There will be no scaling if the minimum amount is used
    
    ## Capital Cost (Investment)
    
    R$ProjectCapitalCost_1 = (13060*R$a_mgd+71118)*R$ScaleFactor_wp *parms$Adj_f
    
    ## Operational cost (annual) 
    R$min_mgd_c =  R$mgd_f/1e6*(day_y/2)/parms$USgal_m3*1e6 # Minimum capacity 
    R$max_mgd_c =  R$mgd_l/1e6*(day_y/2)/parms$USgal_m3*1e6 # Maximum capacity 
    
    R$mgd_S = treat$wtflow_S*1e6/(day_y/2)*parms$USgal_m3/1e6 # required for summer in mgd
    R$mgd_W = treat$wtflow_W*1e6/(day_y/2)*parms$USgal_m3/1e6 #required for winter in mgd
    
    R$ScaleFactor_wp_S = ifelse(R$mgd_S>0,(R$mgd_S/pmin(R$mgd_S, R$mgd_l))^parms$coef,0) #Scale factor for operational cost in SUMMER
    R$ScaleFactor_wp_W = ifelse(R$mgd_W>0,(R$mgd_W/pmin(R$mgd_W, R$mgd_l))^parms$coef,0) #Scale factor for operational cost in WINTER
    
    R$t_mgd_S = ifelse(treat$wtflow_S>0,pmax(R$min_mgd_c,pmin(R$mgd_S,R$mgd_l)),0) #truncate to min and max mgd for summer
    R$t_mgd_W = ifelse(treat$wtflow_W>0,pmax(R$min_mgd_c,pmin(R$mgd_W,R$mgd_l)),0) #truncate to min and max mgd for winter
    
    R$ProjectOperatingCost_S_1 = (8979.1*R$t_mgd_S+24960)*(R$ScaleFactor_wp_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
    R$ProjectOperatingCost_W_1 = (8979.1*R$t_mgd_W+24960)*(R$ScaleFactor_wp_W/2*parms$Adj_f) # Total project operation cost WINTER
    
    R$ProjectOperatingCost_1 = (R$ProjectOperatingCost_S_1+R$ProjectOperatingCost_W_1)

    }
      
    ## 2. Ozone Generation Plant and Contact Chambers ## 
    {
    # 2.1 Ozonation Generation
    {
    R$ogc_lb_f = 0 # Ozone generation capacity limit (from model) - Actual is 10
    R$ogc_lb_l = 3500 # Ozone generation capacity limit (from model)
    ## Operational cost (annual) - With Scale factor #Keep this one
    
    ## Capital Cost (Investment)
    R$r_ogc_lb = max(treat$max_ogc_lb_S,treat$max_ogc_lb_W) # Maximum required capacity
    R$a_ogc_lb = max(c(R$ogc_lb_f,min(c(R$r_ogc_lb,R$ogc_lb_l))))   # Allowable Ozone generation capacity per day - mg/L*kg/mg*L/m3*m3/day*lb/kg 
                                                                    # (if its lower than the floor, the floor will be used)
    R$d_ogc_lb = max(c(R$r_ogc_lb-R$a_ogc_lb,0))  # Difference between allowable ozone generation capacity
    
    R$max_wt =  max(c(treat$wtflow_S,treat$wtflow_W))*1e6/(day_y/2) # Max water treated per day
    
     R$ProjectCapitalCost_2.1 = ((0.0002*R$a_ogc_lb^3-1.5794*R$a_ogc_lb^2+4424.4*R$a_ogc_lb+214180)+716.5*R$d_ogc_lb)*parms$Adj_f 
    # 716.5 is the slope of the Capital Cost function 2.1
    
    R$t_OGC_S = ifelse(treat$OGC_S>0,pmax(R$ogc_lb_f,pmin(treat$OGC_S,R$ogc_lb_l)),0) #truncate to the min or max ogc for summer
    R$t_OGC_W = ifelse(treat$OGC_S>0,pmax(R$ogc_lb_f,pmin(treat$OGC_W,R$ogc_lb_l)),0) #truncate to the min or max ogc for winter
    
    ## Operational cost (annual) - With Scale factor #Keep this one
    parms$coef=0.865 # to make it close to the linear approximation (change it parameter)
    R$ScaleFactor_oc_S = ifelse(treat$OGC_S>0,((treat$OGC_S/pmin(treat$OGC_S, R$ogc_lb_l))^parms$coef),0) #Scale factor for operational cost in SUMMER
    R$ScaleFactor_oc_W = ifelse(treat$OGC_W>0,((treat$OGC_W/pmin(treat$OGC_W, R$ogc_lb_l))^parms$coef),0) #Scale factor for operational cost in WINTER
    
    R$ProjectOperatingCost_S_2.1 = (-0.01*R$t_OGC_S^2+367.19*R$t_OGC_S+ifelse(R$t_OGC_S>0,35653,0))*(R$ScaleFactor_oc_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
    R$ProjectOperatingCost_W_2.1 = (-0.01*R$t_OGC_W^2+367.19*R$t_OGC_W+ifelse(R$t_OGC_W>0,35653,0))*(R$ScaleFactor_oc_W/2*parms$Adj_f) # Total project operation cost WINTER
    
    R$ProjectOperatingCost_2.1 = (R$ProjectOperatingCost_S_2.1+R$ProjectOperatingCost_W_2.1) # per year and adjusted by inflation
    }
    
    }
      
    # 2.2 Contact chamber
    {
    R$ccv_f = 0 # Minimum volume for CCV - Actual is 460
    R$ccv_l = 92000 # Maximum volume for CCV
    R$ccv  = max(c(R$ccv_f,parms$time/60/24*R$max_wt*35.3))  # Chamber Capacity Volume (ft3)  (if its lower than the floor, the floor will be used)
    R$ScaleFactor_cc = (R$ccv/min(R$ccv, R$ccv_l))^parms$coef
    
    R$ProjectCapitalCost_2.2 = (6.3332*R$ccv+40147)*R$ScaleFactor_cc*parms$Adj_f 
    
    }  
   
    ## 3. Coagulation, precipitation and flocculation (Dry alum feed system)## 
    {
    ## 3.1 Dry alum feed system
    {
    # Use the dose of 250 mg/L with a 4 Mm3 capacity from Conrad
    dose = 250/1000*max(c(treat$wtflow_S,treat$wtflow_W))*1e6/(day_y/2)*2.2 # (lb/d)  of aluminium
  
    R$f_alu_lb_h = 0 # Alum dose floor (lb/h) - Actual is 5.4
    R$l_alu_lb_h = 5400 # Alum dose limit (lb/h)
    R$r_alu_lb_h = dose/24 # Maximum required capacity per hour (lb/h)
    R$a_alu_lb_h = max(R$f_alu_lb_h/24,min(R$r_alu_lb_h, R$l_alu_lb_h)) # Allowed
    
    R$ScaleFactor_fc = (R$r_alu_lb_h/min(R$r_alu_lb_h, R$a_alu_lb_h))^parms$coef #There will be no scaling if the minimum amount is used
      
    ## Capital Cost (Investment)
    R$ProjectCapitalCost_3.1 = (251.52*R$a_alu_lb_h+74291)*R$ScaleFactor_fc*parms$Adj_f
    
    ## Operational Cost 
    
    R$fc_S = treat$wtflow_S*1e6/(day_y/2)/24
    R$fc_W = treat$wtflow_W*1e6/(day_y/2)/24
    
    parms$coef=0.865 # to make it close to the linear approximation (change it parameter)
    R$ScaleFactor_fc_S = ifelse(  R$fc_S>0,((R$fc_S/pmin(R$fc_S, R$l_alu_lb_h))^parms$coef),0) #Scale factor for operational cost in SUMMER
    R$ScaleFactor_fc_W = ifelse(  R$fc_W>0,((R$fc_W/pmin(R$fc_W, R$l_alu_lb_h))^parms$coef),0) #Scale factor for operational cost in WINTER
    
    R$t_fc_S = ifelse(R$fc_S>0,pmax(R$f_alu_lb_h,pmin(treat$wtflow_S*1e6/(day_y/2)/24,R$l_alu_lb_h)),0) #truncate to min and maximum alum dose for summer feed capacity
    R$t_fc_W = ifelse(R$fc_S>0,pmax(R$f_alu_lb_h,pmin(treat$wtflow_W*1e6/(day_y/2)/24,R$l_alu_lb_h)),0) #truncate to min and maximum alum dose for winter feed capacity
    
    R$ProjectOperatingCost_S_3.1 = (2212.7*R$t_fc_S^(0.2919))*(R$ScaleFactor_fc_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
    R$ProjectOperatingCost_W_3.1 = (2212.7*R$t_fc_W^(0.2919))*(R$ScaleFactor_fc_W/2*parms$Adj_f) # Total project operation cost WINTER
    
    
    R$ProjectOperatingCost_3.1 = (R$ProjectOperatingCost_S_3.1+R$ProjectOperatingCost_W_3.1)
    }
    
    ## 3.2 Rapid mix (G=900/s)  
    {
      Gall_ft3 = 0.1336805556 # ft3 per gallon
      # NEW CHANGE
      volumeb = 0.5*max(R$t_mgd_S,R$t_mgd_W)*1e6/24/60*Gall_ft3  # retention time (minutes) * gpm flow (ft3) 
      # volumeb = 0.5*R$max_mgd_c*1e6/24/60*Gall_ft3  # retention time (30 seconds) * gpm flow (ft3) - OLD
      
    R$f_bvol = 0 # Rapid mix floor (ft3) - Actual is 10
    R$l_bvol = 20000 # Rapid mix limit (ft3)
    R$r_bvol = volumeb # Maximum basin volume (ft3)
    R$a_bvol = max(R$f_bvol,min(R$r_bvol, R$l_bvol)) # Allowed    
      
    R$ScaleFactor_bvol = (R$r_bvol/min(R$r_bvol, R$a_bvol))^parms$coef #There will be no scaling if the minimum amount is used
    
    ## Capital Cost (Investment)
    R$ProjectCapitalCost_3.2 = (0.0002*R$a_bvol^2+58.159*R$a_bvol+30823)*R$ScaleFactor_bvol*parms$Adj_f
    
    ## Operational Cost 
    
    ## NEW change
    R$ProjectOperatingCost_3.2 = rep(0,70)
    R$ProjectOperatingCost_3.2 =  ifelse((R$t_mgd_S+R$t_mgd_W)>0,(37.081*R$a_bvol+18839)*(R$ScaleFactor_bvol*parms$Adj_f),0)  
    
    
    }
        
    ## 3.3 Horizontal paddle systems, G = 50/s
    {  
      # NEW CHANGE
      volume = 90*max(R$t_mgd_S,R$t_mgd_W)*1e6/24/60*Gall_ft3  # retention time (minutes) * gpm flow (ft3) 
      # volume = 90*R$max_mgd_c*1e6/24/60*Gall_ft3  # retention time (minutes) * gpm flow (ft3) 
      # Before we assumed the maximum volume from the 
      
      R$f_bvolh = 0 # Rapid mix floor (ft3) - Actual is 1800
      R$l_bvolh = 1000000 # Rapid mix limit (ft3)
      R$r_bvolh = volume # Maximum basin volume (ft3)
      R$a_bvolh = max(R$f_bvolh,min(R$r_bvolh, R$l_bvolh)) # Allowed    
      
      R$ScaleFactor_bvolh = (R$r_bvolh/min(R$r_bvolh, R$a_bvolh))^parms$coef 
      
      ## Capital Cost (Investment)
      R$ProjectCapitalCost_3.3 = (5.7963*R$a_bvolh+224760)*R$ScaleFactor_bvolh*parms$Adj_f  
      
      ## Operational Cost 
      R$ProjectOperatingCost_3.3 = rep(0,70)
      R$ProjectOperatingCost_3.3 =  ifelse((R$t_mgd_S+R$t_mgd_W)>0,(3e-13*R$a_bvolh^3-5e-7*R$a_bvolh^2+0.3935*R$a_bvolh+6960)*(R$ScaleFactor_bvolh*parms$Adj_f),0)  
    } 
     }
    
    ## 4. Sedimentation (Rectangular clarifiers) ## 
    # Contact time + mixing velocity
    {
      volume = 90*max(R$t_mgd_S,R$t_mgd_W)*1e6/24/60*Gall_ft3  # retention time (minutes) * gpm flow (ft3) 
      surface = volume /9 # ft3/ft = ft2 (ft depth = 3-4.5 m) - 9ft is for the water 
      # x will depend on the amount of sedimentation
      surface/4800
      
      
    R$f_sa = 0        # Minimum surface area ft2 - Actual is 240
    R$l_sa = 4800        # Limit surface area ft2
    R$r_sa = surface     # ft2 assuming a length of 15 m
    R$a_sa = max(c(R$f_sa,min(c(R$r_sa,R$l_sa))))
    R$d_sa = max(c(R$r_sa-R$a_sa,0))
    R$n_tanks = R$r_sa/R$l_sa
    
    ## Capital Cost (Investment)
    
    R$ProjectCapitalCost_4 = ((-0.0029*R$a_sa^2+169.19*R$a_sa+94365)*R$n_tanks)*parms$Adj_f 
    # Total project capital cost adjusted by inflation

    ## Operational cost (annual) 
    R$ProjectOperatingCost_4 = rep(0,70)
    R$ProjectOperatingCost_4 =  ifelse((R$t_mgd_S+R$t_mgd_W)>0,(4.2948*R$a_sa+8283)*(R$n_tanks*parms$Adj_f),0)  
    
    }  
   
    ## 5. Finished water pumping facilities (30.48m - 100ft)## 
    {
      ## Capital Cost (Investment)
      
      R$d_mgd = max(c(R$r_mgd-R$a_mgd,0))
      
      R$ProjectCapitalCost_5 = ((0.9446*R$a_mgd^2+19736*R$a_mgd+118832)+20302.76*R$d_mgd)*parms$Adj_f
      
      ## Operational cost (annual)
      
      # We used the Scale factor and truncated mgd_
      R$ProjectOperatingCost_S_5 = (16555*R$t_mgd_S+23488)*(R$ScaleFactor_wp_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
      R$ProjectOperatingCost_W_5 = (16555*R$t_mgd_W+23488)*(R$ScaleFactor_wp_W/2*parms$Adj_f) # Total project operation cost WINTER
      
      R$ProjectOperatingCost_5 = (R$ProjectOperatingCost_S_5+R$ProjectOperatingCost_W_5)
      
    }
     
    ## 6. In plant pumping## 
    {
    ## Capital Cost (Investment)
    
    # the slope for higher values is 4888180
    R$d_mgd = max(c(R$r_mgd-R$a_mgd,0))
    
    R$ProjectCapitalCost_6 = ((12.288*R$a_mgd^2+12980*R$a_mgd+89161)+4888180*R$d_mgd)*parms$Adj_f
      
    ## Operational cost (annual)
    
    # We used the Scale factor and truncated mgd_
    R$ProjectOperatingCost_S_6 = (12751*R$t_mgd_S+24739)*(R$ScaleFactor_wp_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
    R$ProjectOperatingCost_W_6 = (12751*R$t_mgd_W+24739)*(R$ScaleFactor_wp_W/2*parms$Adj_f) # Total project operation cost WINTER
      
    R$ProjectOperatingCost_6 = (R$ProjectOperatingCost_S_6+R$ProjectOperatingCost_W_6)
      
          }
    
    ## 7. Disposal (Chemical sludge pumping: unthickened sludge) 
     {
    #   ## Capital Cost (Investment)
      R$f_gpm = 0     # gpm floor - Actual is 1
      R$l_gpm = 10000 # gpm limit

      R$dis_ratio = 0.1 # From CONRAD report (Confirm with Pamela)
      R$dis_pum = max(R$f_gpm,min(R$l_gpm,R$max_mgd_c[1]*1e6/24/60*R$dis_ratio)) #Pumping capacity (gpm)


      R$ScaleFactor_dis = (R$dis_pum/min(R$dis_pum, R$max_gpm))^parms$coef #There will be no scaling if the minimum amount is used

      R$ProjectCapitalCost_7 = (2e-6*R$dis_pum^3-0.0258*R$dis_pum^2+183.11*R$dis_pum+94222)*R$ScaleFactor_wp*parms$Adj_f

      ## Operational cost (annual) - With Scale factor #Keep this one

      R$gpm_S = R$mgd_S*1e6/24/60*R$dis_ratio   # gpm in summer
      R$gpm_W = R$mgd_W*1e6/24/60*R$dis_ratio   # gpm in winter

      R$ScaleFactor_dis_S = ifelse(R$gpm_S>0,(R$gpm_S/pmin(R$gpm_S, R$l_gpm))^parms$coef,0) #Scale factor for operational cost in SUMMER
      R$ScaleFactor_dis_W = ifelse(R$gpm_W>0,(R$gpm_W/pmin(R$gpm_W, R$l_gpm))^parms$coef,0) #Scale factor for operational cost in WINTER

      R$t_gpm_S = ifelse(treat$TreatVec_S>0,pmax(R$f_gpm,pmin(R$gpm_S,R$l_gpm)),0) #truncate to min and max gpm for summer
      R$t_gpm_W = ifelse(treat$TreatVec_W>0,pmax(R$f_gpm,pmin(R$gpm_W,R$l_gpm)),0) #truncate to min and max gpm for winter


      # using same scale factor than in pumping

      R$ProjectOperatingCost_S_7 = (4e-7*R$t_mgd_S^3-0.0058*R$t_mgd_S^2+43.748*R$t_mgd_S+ifelse(R$mgd_S>0,11622,0))*(R$ScaleFactor_dis_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
      R$ProjectOperatingCost_W_7 = (4e-7*R$t_mgd_W^3-0.0058*R$t_mgd_W^2+43.748*R$t_mgd_W+ifelse(R$mgd_W>0,11622,0))*(R$ScaleFactor_dis_W/2*parms$Adj_f) # Total project operation cost WINTER

      R$ProjectOperatingCost_7 = (R$ProjectOperatingCost_S_7+R$ProjectOperatingCost_W_7)
    #   
    }
    
    
    ## 8. Management ## 
    {
      ## Capital Cost (Investment)
      
      # Using the Batea report cost, the coefficient is 3971011.313 and 72277.67
      R$ScaleFactor_ma = (R$r_mgd/min(R$r_mgd, R$a_mgd))^parms$coef
      
      R$ProjectCapitalCost_8 = (3971011.313*R$r_mgd^0.5523)*parms$Adj_f
      
      R$ProjectOperatingCost_8 = (72277.67*(R$mgd_S+R$mgd_W)^0.4526)*parms$Adj_f
    }
    
    ## TOTAL EQUIPMENT COST ##
    {
    # Total cost  
    R$Cap_Cost <-  R$ProjectCapitalCost_1+ R$ProjectCapitalCost_2.1+R$ProjectCapitalCost_2.2+ R$ProjectCapitalCost_3.1+ R$ProjectCapitalCost_3.2+R$ProjectCapitalCost_3.3
        R$ProjectCapitalCost_4+ R$ProjectCapitalCost_5 + R$ProjectCapitalCost_6+ R$ProjectCapitalCost_7 + R$ProjectCapitalCost_8
    
    R$OP_Cost <-ifelse(treat$wtflow_T>0,(R$ProjectOperatingCost_1+R$ProjectOperatingCost_2.1+R$ProjectOperatingCost_3.1
        +R$ProjectOperatingCost_3.2+R$ProjectOperatingCost_3.3+R$ProjectOperatingCost_4+R$ProjectOperatingCost_5
        +R$ProjectOperatingCost_6+R$ProjectOperatingCost_7+R$ProjectOperatingCost_8)*DiscR,0)  
    
    R$DisOPCost_Buildtime = sum(R$OP_Cost)*DiscRate^(-Decisions[3])  
      
    R$amo_OP_cost= R$DisOPCost_Buildtime*(1/DiscRate-1)/(1-DiscRate^Decisions[4])    
      
    }
    
    ## TOTAL FACTOR COST ##
    {
     
      R$labor_factor = Wage_batea/(Wage_source*PPP_12_CAD)
      
      R$Exp_Cap_Cost =  R$ProjectCapitalCost_1*(0.81*R$Exp_factor+0.19*R$labor_factor)+ 
                        R$ProjectCapitalCost_2.1*(0.81*R$Exp_factor+0.19*R$labor_factor)+ 
                        R$ProjectCapitalCost_2.2*(0.81*R$Exp_factor+0.19*R$labor_factor)+
                        R$ProjectCapitalCost_3.1*(0.53*R$Exp_factor+0.47*R$labor_factor)+ 
                        R$ProjectCapitalCost_3.2*(0.83*R$Exp_factor+0.17*R$labor_factor)+ 
                        R$ProjectCapitalCost_3.3*(0.70*R$Exp_factor+0.30*R$labor_factor)+
                        R$ProjectCapitalCost_5*(0.90*R$Exp_factor+0.10*R$labor_factor)+                
                        R$ProjectCapitalCost_6*(0.62*R$Exp_factor+0.38*R$labor_factor)+
                        R$ProjectCapitalCost_7*(0.69*R$Exp_factor+0.31*R$labor_factor) 
                       
      
 
      R$Total_Cap_Cost = R$Exp_Cap_Cost*(R$Exp_Cap)+(R$ProjectCapitalCost_4*(0.74+0.26*R$labor_factor)+R$ProjectCapitalCost_8)+(17820000+26730000)*parms$Adj_f
      
      # 17820000 is I&C from the BATEA report
      # 26730000 is Building/Roads from the BATEA report
      
      R$Exp_Op_Cost = ifelse(treat$wtflow_T>0,(R$ProjectOperatingCost_1*(0.78*R$Exp_factor+0.22*R$labor_factor)+
                       R$ProjectOperatingCost_2.1*(0.86*R$Exp_factor+0.16*R$labor_factor)+
                       R$ProjectOperatingCost_3.1*(0.54*R$Exp_factor+0.46*R$labor_factor)+
                       R$ProjectOperatingCost_3.2*(0.84*R$Exp_factor+0.16*R$labor_factor)+
                       R$ProjectOperatingCost_3.3*(0.70*R$Exp_factor+0.30*R$labor_factor)+
                       R$ProjectOperatingCost_5*(0.92*R$Exp_factor+0.08*R$labor_factor)+
                       R$ProjectOperatingCost_6*(0.89*R$Exp_factor+0.11*R$labor_factor)+
                       R$ProjectOperatingCost_7*(0.73*R$Exp_factor+0.27*R$labor_factor))*DiscR,0)
      
      R$Total_Op_Cost = R$Exp_Op_Cost*(R$Exp_Op)+(R$ProjectOperatingCost_4*(0.11+0.89*R$labor_factor)+
                                                       R$ProjectOperatingCost_8*(0.15+0.85*R$labor_factor))
      
      #3.42 is the factor used to obtain the total Capital costs (from BATEA report)

      }
    
  {
    
    if (sum(treat$wtflow_T)>=0.00001) {  
    R$Disc_TotalCosts<- (R$Total_Cap_Cost*DiscRate^(Decisions[3]-1)+sum(R$Total_Op_Cost*DiscR))*(PPP_12_CAD) #In CAD
    
    R$DisCost_Buildtime = R$Disc_TotalCosts*DiscRate^(-Decisions[3])# cost at building time
    
    R$amo_cost= R$DisCost_Buildtime*(1/DiscRate-1)/(1-DiscRate^Decisions[4])#amortization cost per year
    
    R$m3_cost = R$amo_cost/((treat$wtflow_T)*1000000)  #Cost per m3 in CAD
    }else{
      R$Disc_TotalCosts = 0
      R$DisCost_Buildtime = 0
      R$amo_cost = 0
      R$m3_cost = 0
    }
      
  
    return(R)
  

  
  
}
}

}
  
### REVERSE OSMOSIS ### (Sharma et al. (2013), BATEA, CONRAD)
{ 
Setup_RO_Parameters<-function(interest_rate = d_rate)
  { 
    ################### RO Specs and parameters #######################
    RO=list()
    #Conversion Factors
    RO$USgal_m3=264.17 #US gallon per m3
    RO$m3_USgas=0.003785412 #m3 per US gallon
    RO$AcrePerHa=2.47105382
    RO$m2_ft2 = 0.09290304 #m2 per ft2
    
    #Value of Time variables
    Ammort_Period     = 15 # Years
    RO$PW = (1-1/(1+interest_rate)^Ammort_Period)/interest_rate# Present worth factor
    
    
    #Base Case parameters for RO
    InflowYr          = 6000000               #m3/yr 
    InflowDay         = InflowYr/(365)      #m3/day
    InflowHr          = InflowYr/(365*24)   #m3/hr
    InflowSec         = InflowDay/(24*60*60)  #m3/sec
    InfYrM             =  InflowYr/1000000      #Mm3/yr
    
    
    #Reverse Osmeosis Design Assumptions: Total equipment cost
    CPI_11 = 224.94 #CPI for 2011 (Average)
    CPI_12 = 229.54 #CPI for 2012 (Average)
    CPI_22 = 291.73 #CPI for 2022 (Average) (until October)
    RO$Adj_f = CPI_12/CPI_11 #Adjustment factor
    RO$Adj_f2 = CPI_12/CPI_22 #Adjustment factor
    
    RO$time = 15 # treatment time in minutes ?
    RO$coef = 0.865 # to make it close to the linear approximation (change it parameter)
  
    # Water loss rate
    RO$WaterLossRate= c((3000-2941)/3000,(3000-2941)/3000)   # From DAP - AOP BATEA REPORT
    
    # Recycle Ratio for summer and winter
    RO$RecycleRatio=c(0,0)

    
    # Removal
  
    RO$ratePoll_reduc<-c(0.96, 0.94,0.97,1,0.5) # aritmethic reduction, assumption for Cadmium
    
    return(RO)
  }
  
RO_Parameters<-Setup_RO_Parameters(interest_rate= d_rate)
  
  ### RO Treatment ###
RO_TreatmentEffect<-function(T=T,TreaT=TreaT,NATarget=DecisionVariables[7],TreatVec_S=TreatVec_S, TreatVec_W=TreatVec_W,Pollconc=concPollutants,parms=RO_Parameters) {
    R<-list()
    R$theta_S=1-parms$WaterLossRate[1] ## Assuming different Water loss rate over seasons
    R$theta_W=1-parms$WaterLossRate[2] ## Assuming different Water loss rate over seasons
    
    R$Targetconc = NATarget #Decisions[7]
    
    ### For capital cost ###
    p_remain = (1-parms$ratePoll_reduc[3])*Pollconc[,3]
    
    # To obtain cases where recycling is needed
    Req_init = ifelse(p_remain>R$Targetconc,R$Targetconc/(1-parms$ratePoll_reduc[3]),Pollconc[,3]) #cwf
    
    # wr = (cw*w- cwf*w)/ (cwf-cwr)
    wr_s = TreatVec_S*(Pollconc[,3]-Req_init)/(Req_init-R$Targetconc) # Recycled water flow Summer
    wr_w = TreatVec_W*(Pollconc[,3]-Req_init)/(Req_init-R$Targetconc) # Recycled water flow Winter
    # wf=w+wr
    wf_s = TreatVec_S+ wr_s
    wf_w = TreatVec_W+ wr_w
    # α = wr/  θwf
    R$rr_S = ifelse(wf_s!=0,wr_s/(R$theta_S*wf_s),0) 
    R$rr_W = ifelse(wf_w!=0,wr_w/(R$theta_S*wf_w),0)
    
    ## Summer ##
    R$we_S=TreatVec_S*R$theta_S*(1-R$rr_S)/(1-R$rr_S*R$theta_S)  # calculation of the final effluent flow into the river
    R$wtflow_S=TreatVec_S/(1-R$theta_S*R$rr_S) # amount of water going through the treatment system
    R$wr_S=R$theta_S*R$rr_S*R$wtflow_S  # amount of water recycled is the amount of water diverted from the effluent back into the system
    
    ## Winter ##
    R$we_W=TreatVec_W*R$theta_W*(1-R$rr_W)/(1-R$rr_W*R$theta_W)  # calculation of the final effluent flow into the river
    R$wtflow_W=TreatVec_W/(1-R$theta_W*R$rr_W) # amount of water going through the treatment system
    R$wr_W=R$theta_W*R$rr_W*R$wtflow_W  # amount of water recycled is the amount of water diverted from the effluent back into the system
    
    ## Yearly water on treatment ##
    R$wtflow_T = R$wtflow_S+R$wtflow_W
    
    
    #Summer concentration
    R$ROPollconc_S<-matrix(0, nrow = Nperiods,ncol = 5) #Outcome concentration 
    R$ROPollconc_S[T:TreaT,1] = (1-parms$ratePoll_reduc[1])*Pollconc[T:TreaT,1]*(1-R$theta_S*R$rr_S[T:TreaT])/(R$theta_S)/(1-parms$ratePoll_reduc[1]*R$rr_S[T:TreaT]) # Reached level after each optimal treatment for TDS
    R$ROPollconc_S[T:TreaT,2] = (1-parms$ratePoll_reduc[2])*Pollconc[T:TreaT,2]*(1-R$theta_S*R$rr_S[T:TreaT])/(R$theta_S)/(1-parms$ratePoll_reduc[2]*R$rr_S[T:TreaT]) # Chloride
    R$ROPollconc_S[T:TreaT,3] = (1-parms$ratePoll_reduc[3])*Pollconc[T:TreaT,3]*(1-R$theta_S*R$rr_S[T:TreaT])/(R$theta_S)/(1-parms$ratePoll_reduc[3]*R$rr_S[T:TreaT]) # Reached level after each optimal treatment for NAs
    R$ROPollconc_S[T:TreaT,4] = (1-parms$ratePoll_reduc[4])*Pollconc[T:TreaT,4]*(1-R$theta_S*R$rr_S[T:TreaT])/(R$theta_S)/(1-parms$ratePoll_reduc[4]*R$rr_S[T:TreaT]) # Ammonia
    R$ROPollconc_S[T:TreaT,5] = (1-parms$ratePoll_reduc[5])*Pollconc[T:TreaT,5]*(1-R$theta_S*R$rr_S[T:TreaT])/(R$theta_S)/(1-parms$ratePoll_reduc[5]*R$rr_S[T:TreaT]) # Cadmium
    
    #Winter concentration
    R$ROPollconc_W<-matrix(0, nrow = Nperiods,ncol = 5) #Outcome concentration
    R$ROPollconc_W[T:TreaT,1] = (1-parms$ratePoll_reduc[1])*Pollconc[T:TreaT,1]*(1-R$theta_W*R$rr_W[T:TreaT])/(R$theta_W)/(1-parms$ratePoll_reduc[1]*R$rr_W[T:TreaT]) # Reached level after each optimal treatment for TDS
    R$ROPollconc_W[T:TreaT,2] = (1-parms$ratePoll_reduc[2])*Pollconc[T:TreaT,2]*(1-R$theta_W*R$rr_W[T:TreaT])/(R$theta_W)/(1-parms$ratePoll_reduc[2]*R$rr_W[T:TreaT]) # Chloride
    R$ROPollconc_W[T:TreaT,3] = (1-parms$ratePoll_reduc[3])*Pollconc[T:TreaT,3]*(1-R$theta_W*R$rr_W[T:TreaT])/(R$theta_W)/(1-parms$ratePoll_reduc[3]*R$rr_W[T:TreaT]) # Reached level after each optimal treatment for NAs
    R$ROPollconc_W[T:TreaT,4] = (1-parms$ratePoll_reduc[4])*Pollconc[T:TreaT,4]*(1-R$theta_W*R$rr_W[T:TreaT])/(R$theta_W)/(1-parms$ratePoll_reduc[4]*R$rr_W[T:TreaT]) # Ammonia
    R$ROPollconc_W[T:TreaT,5] = (1-parms$ratePoll_reduc[5])*Pollconc[T:TreaT,5]*(1-R$theta_W*R$rr_W[T:TreaT])/(R$theta_W)/(1-parms$ratePoll_reduc[5]*R$rr_W[T:TreaT]) # Cadmium    
    
    return(R)
  }
  
  ### Cost RO ###
  
RO_CostFn <- function(Decisions=DecisionVariables[1:7],parms=RO_Parameters,treat=RO_treat,exp_factor=factors[1:3]){
    R=list()
    R$Exp_factor = exp_factor[1] # Expansion factor for a Oil Sand company to multiply by cost
    R$Exp_Cap =exp_factor[2] #factor capital cost
    R$Exp_Op =exp_factor[3] #factor ope cost
    
    
    ## 1. Raw water pumping (TDH = 30.48m  (100 ft))## 
    {
      R$mgd_f = 1 # mgd capacity floor (from model)
      R$mgd_l = 200 # mgd capacity limit (from model)
      R$r_mgd = max(c(treat$wtflow_S,treat$wtflow_W))*1e6/(day_y/2)*parms$USgal_m3/1e6 # Required water treated in gallons per day 
      R$a_mgd = max(c(R$mgd_f,min(c(R$r_mgd,R$mgd_l)))) # Allowed mgd per day in the system (if is lower than the floor, the floor will be used)
      
      R$ScaleFactor_wp = (R$r_mgd/min(R$r_mgd, R$a_mgd))^parms$coef #There will be no scaling if the minimum amount is used
      ## Capital Cost (Investment)
      
      R$ProjectCapitalCost_1 = (13060*R$a_mgd+71118)*R$ScaleFactor_wp *parms$Adj_f
      
      ## Operational cost (annual) 
      R$min_mgd_c =  R$mgd_f/1e6*(day_y/2)/parms$USgal_m3*1e6 # Minimum capacity 
      R$max_mgd_c =  R$mgd_l/1e6*(day_y/2)/parms$USgal_m3*1e6 # Maximum capacity 
      
      R$mgd_S = treat$wtflow_S*1e6/(day_y/2)*parms$USgal_m3/1e6 # required for summer in mgd
      R$mgd_W = treat$wtflow_W*1e6/(day_y/2)*parms$USgal_m3/1e6 #required for winter in mgd
      
      R$ScaleFactor_wp_S = ifelse(R$mgd_S>0,(R$mgd_S/pmin(R$mgd_S, R$mgd_l))^parms$coef,0) #Scale factor for operational cost in SUMMER
      R$ScaleFactor_wp_W = ifelse(R$mgd_W>0,(R$mgd_W/pmin(R$mgd_W, R$mgd_l))^parms$coef,0) #Scale factor for operational cost in WINTER
      
      R$t_mgd_S = ifelse(treat$wtflow_S>0,pmax(R$min_mgd_c,pmin(R$mgd_S,R$mgd_l)),0) #truncate to min and max mgd for summer
      R$t_mgd_W = ifelse(treat$wtflow_W>0,pmax(R$min_mgd_c,pmin(R$mgd_W,R$mgd_l)),0) #truncate to min and max mgd for winter
      
      R$ProjectOperatingCost_S_1 = (8979.1*R$t_mgd_S+24960)*(R$ScaleFactor_wp_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
      R$ProjectOperatingCost_W_1 = (8979.1*R$t_mgd_W+24960)*(R$ScaleFactor_wp_W/2*parms$Adj_f) # Total project operation cost WINTER
      
      R$ProjectOperatingCost_1 = (R$ProjectOperatingCost_S_1+R$ProjectOperatingCost_W_1)
      
    }
    
    ## 2. Reverse Osmeosis system ## 
    {
      ## Capital Cost (Investment)
      
      R$ProjectCapitalCost_2 = (-0.0007*(R$a_mgd)^2+1246.6*R$a_mgd+2000000)*R$ScaleFactor_wp*parms$Adj_f
      # Seems that I am not using the right unit as the plant capcity is 1000 gpd, and we were using million of mgd!
      R$ProjectCapitalCost_2 = (-0.0007*(R$a_mgd*1000)^2+1246.6*R$a_mgd*1000+2000000)*R$ScaleFactor_wp*parms$Adj_f
      
      
      ## Operational cost (annual) - With Scale factor #Keep this one
      
      # R$ProjectOperatingCost_S_2 = (414135*R$t_mgd_S+ifelse(R$t_mgd_S>0,222705,0))*(R$ScaleFactor_wp_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
      # R$ProjectOperatingCost_W_2 = (414135*R$t_mgd_W+ifelse(R$t_mgd_S>0,222705,0))*(R$ScaleFactor_wp_W/2*parms$Adj_f) # Total project operation cost WINTER
      # 
      R$ProjectOperatingCost_S_2 = (414135*R$t_mgd_S+ifelse(R$t_mgd_S*1000>0,222705,0))*(R$ScaleFactor_wp_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
      R$ProjectOperatingCost_W_2 = (414135*R$t_mgd_W+ifelse(R$t_mgd_S*1000>0,222705,0))*(R$ScaleFactor_wp_W/2*parms$Adj_f) # Total project operation cost WINTER
      
      
      R$ProjectOperatingCost_2 = (R$ProjectOperatingCost_S_2+R$ProjectOperatingCost_W_2)
    }  
    
    ## 3. Coagulation, precipitation and flocculation (Dry alum feed system)## 
    {
      ## 3.1 Dry alum feed system
      {
        # Use the dose of 250 mg/L with a 4 Mm3 capacity from Conrad
        dose = 250/1000*max(c(treat$wtflow_S,treat$wtflow_W))*1e6/(day_y/2)*2.2 # (lb/d)  of aluminium
        
        R$f_alu_lb_h = 0 # Alum dose floor (lb/h) - Actual is 5.4
        R$l_alu_lb_h = 5400 # Alum dose limit (lb/h)
        R$r_alu_lb_h = dose/24 # Maximum required capacity per hour (lb/h)
        R$a_alu_lb_h = max(R$f_alu_lb_h/24,min(R$r_alu_lb_h, R$l_alu_lb_h)) # Allowed
        
        R$ScaleFactor_fc = (R$r_alu_lb_h/min(R$r_alu_lb_h, R$a_alu_lb_h))^parms$coef #There will be no scaling if the minimum amount is used
        
        ## Capital Cost (Investment)
        R$ProjectCapitalCost_3.1 = (251.52*R$a_alu_lb_h+74291)*R$ScaleFactor_fc*parms$Adj_f
        
        ## Operational Cost 
        
        R$fc_S = treat$wtflow_S*1e6/(day_y/2)/24
        R$fc_W = treat$wtflow_W*1e6/(day_y/2)/24
        
        parms$coef=0.865 # to make it close to the linear approximation (change it parameter)
        R$ScaleFactor_fc_S = ifelse(  R$fc_S>0,((R$fc_S/pmin(R$fc_S, R$l_alu_lb_h))^parms$coef),0) #Scale factor for operational cost in SUMMER
        R$ScaleFactor_fc_W = ifelse(  R$fc_W>0,((R$fc_W/pmin(R$fc_W, R$l_alu_lb_h))^parms$coef),0) #Scale factor for operational cost in WINTER
        
        R$t_fc_S = ifelse(R$fc_S>0,pmax(R$f_alu_lb_h,pmin(treat$wtflow_S*1e6/(day_y/2)/24,R$l_alu_lb_h)),0) #truncate to min and maximum alum dose for summer feed capacity
        R$t_fc_W = ifelse(R$fc_S>0,pmax(R$f_alu_lb_h,pmin(treat$wtflow_W*1e6/(day_y/2)/24,R$l_alu_lb_h)),0) #truncate to min and maximum alum dose for winter feed capacity
        
        R$ProjectOperatingCost_S_3.1 = (2212.7*R$t_fc_S^(0.2919))*(R$ScaleFactor_fc_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
        R$ProjectOperatingCost_W_3.1 = (2212.7*R$t_fc_W^(0.2919))*(R$ScaleFactor_fc_W/2*parms$Adj_f) # Total project operation cost WINTER
        
        R$ProjectOperatingCost_3.1 = (R$ProjectOperatingCost_S_3.1+R$ProjectOperatingCost_W_3.1)
      }
      
      ## 3.2 Rapid mix (G=900/s)  
      {
        Gall_ft3 = 0.1336805556 # ft3 per gallon
        volumeb = 0.5*max(R$t_mgd_S,R$t_mgd_W)*1e6/24/60*Gall_ft3  # retention time (minutes) * gpm flow (ft3) 
      
        R$f_bvol = 0 # Rapid mix floor (ft3) - Actual is 10
        R$l_bvol = 20000 # Rapid mix limit (ft3)
        R$r_bvol = volumeb # Maximum basin volume (ft3)
        R$a_bvol = max(R$f_bvol,min(R$r_bvol, R$l_bvol)) # Allowed    
        
        R$ScaleFactor_bvol = (R$r_bvol/min(R$r_bvol, R$a_bvol))^parms$coef #There will be no scaling if the minimum amount is used
        
        ## Capital Cost (Investment)
        R$ProjectCapitalCost_3.2 = (0.0002*R$a_bvol^2+58.159*R$a_bvol+30823)*R$ScaleFactor_bvol*parms$Adj_f
        
        ## Operational Cost 
      
        R$ProjectOperatingCost_3.2 = rep(0,70)
        R$ProjectOperatingCost_3.2 =  ifelse((R$t_mgd_S+R$t_mgd_W)>0,(37.081*R$a_bvol+18839)*(R$ScaleFactor_bvol*parms$Adj_f),0)  
         
      }
      
      ## 3.3 Horizontal paddle systems, G = 50/s
      {  
        
        volume = 90*max(R$t_mgd_S,R$t_mgd_W)*1e6/24/60*Gall_ft3  # retention time (minutes) * gpm flow (ft3) 
        
        R$f_bvolh = 0 # Rapid mix floor (ft3) - Actual is 1800
        R$l_bvolh = 1000000 # Rapid mix limit (ft3)
        R$r_bvolh = volume # Maximum basin volume (ft3)
        R$a_bvolh = max(R$f_bvolh,min(R$r_bvolh, R$l_bvolh)) # Allowed    
        
        R$ScaleFactor_bvolh = (R$r_bvolh/min(R$r_bvolh, R$a_bvolh))^parms$coef 
        
        ## Capital Cost (Investment)
        R$ProjectCapitalCost_3.3 = (5.7963*R$a_bvolh+224760)*R$ScaleFactor_bvolh*parms$Adj_f  
        
        ## Operational Cost 
        R$ProjectOperatingCost_3.3 = rep(0,70)
        R$ProjectOperatingCost_3.3 =  ifelse((R$t_mgd_S+R$t_mgd_W)>0,(3e-13*R$a_bvolh^3-5e-7*R$a_bvolh^2+0.3935*R$a_bvolh+6960)*(R$ScaleFactor_bvolh*parms$Adj_f),0)  
       
              } 
    }
    
    
    ## 5. Finished water pumping facilities (30.48m - 100ft)## 
    {
      ## Capital Cost (Investment)
      
      # the slope for higher values is 20302.76
      R$d_mgd = max(c(R$r_mgd-R$a_mgd,0))
      
      R$ProjectCapitalCost_5 = ((0.9446*R$a_mgd^2+19736*R$a_mgd+118832)+20302.76*R$d_mgd)*parms$Adj_f
      
      ## Operational cost (annual)
      
      # We used the Scale factor and truncated mgd_
      R$ProjectOperatingCost_S_5 = (16555*R$t_mgd_S+23488)*(R$ScaleFactor_wp_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
      R$ProjectOperatingCost_W_5 = (16555*R$t_mgd_W+23488)*(R$ScaleFactor_wp_W/2*parms$Adj_f) # Total project operation cost WINTER
      
      R$ProjectOperatingCost_5 = (R$ProjectOperatingCost_S_5+R$ProjectOperatingCost_W_5)
      
    }
    
    ## 6. In plant pumping## 
    {
      ## Capital Cost (Investment)
      
      # the slope for higher values is 4888180
      R$d_mgd = max(c(R$r_mgd-R$a_mgd,0))
      
      R$ProjectCapitalCost_6 = ((12.288*R$a_mgd^2+12980*R$a_mgd+89161)+4888180*R$d_mgd)*parms$Adj_f
      
      ## Operational cost (annual)
      
      # We used the Scale factor and truncated mgd_
      R$ProjectOperatingCost_S_6 = (12751*R$t_mgd_S+24739)*(R$ScaleFactor_wp_S/2*parms$Adj_f) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
      R$ProjectOperatingCost_W_6 = (12751*R$t_mgd_W+24739)*(R$ScaleFactor_wp_W/2*parms$Adj_f) # Total project operation cost WINTER
      
      R$ProjectOperatingCost_6 = (R$ProjectOperatingCost_S_6+R$ProjectOperatingCost_W_6)
      
    }
    

    ## 8. Transport and Disposal cost## From CONRAD REPORT
    {
      ## 8.1 Transport cost
      
      R$dis_ratio = 0.05 # Disposal generated in the extraction process CONRAD report
      R$truck = 150 # 250 # cost per hour (Couldn't they be company trucks? or this is just gas)
      R$tonnes_truck = 35 # 25 # tonnes in the truck
      R$distance = 350 # 450 km 
      
      R$ProjectOperatingCost_S_8.1= (((R$distance/80*2)+2)/R$tonnes_truck*R$truck)*treat$wtflow_S*1e6*R$dis_ratio*parms$Adj_f
      R$ProjectOperatingCost_W_8.1= (((R$distance/80*2)+2)/R$tonnes_truck*R$truck)*treat$wtflow_W*1e6*R$dis_ratio*parms$Adj_f
      
      # 80 is the average speed (km/h) of the truck (shall this change for winter?)
      # 2 Is the handling hours 
      
      R$ProjectOperatingCost_8.1 = (R$ProjectOperatingCost_S_8.1+R$ProjectOperatingCost_W_8.1)
      
      
      ## 8.2 Disposal
      R$Dis_cost = 17 # 37 # Based on Conrad report
      # 37 is the disposal cost in CAD per m3
      
      R$ProjectOperatingCost_S_8.2= (R$Dis_cost)*treat$wtflow_S*1e6*R$dis_ratio*parms$Adj_f
      R$ProjectOperatingCost_W_8.2= (R$Dis_cost)*treat$wtflow_W*1e6*R$dis_ratio*parms$Adj_f
      
      R$ProjectOperatingCost_8.2 = (R$ProjectOperatingCost_S_8.2+R$ProjectOperatingCost_W_8.2)
      
      }
    
    ## 9. Management ## 
    {
      ## Capital Cost (Investment)
      
      # Using the Batea report cost, the coefficient is 3971011.313 and 72277.67
      R$ScaleFactor_ma = (R$r_mgd/min(R$r_mgd, R$a_mgd))^parms$coef
      
      R$ProjectCapitalCost_9 = (3971011.313*R$r_mgd^0.5523)*parms$Adj_f
      
      R$ProjectOperatingCost_9 = (72277.67*(R$mgd_S+R$mgd_W)^0.4526)*parms$Adj_f
    }
    
    ## TOTAL EQUIPMENT COST ##
    {
      # Total cost  
      R$Cap_Cost <-  R$ProjectCapitalCost_1+ R$ProjectCapitalCost_2+ R$ProjectCapitalCost_3.1+ R$ProjectCapitalCost_3.2+R$ProjectCapitalCost_3.3
      R$ProjectCapitalCost_4+ R$ProjectCapitalCost_5 + R$ProjectCapitalCost_6+ R$ProjectCapitalCost_7 + R$ProjectCapitalCost_8+ R$ProjectCapitalCost_9
      
      R$OP_Cost <-ifelse(treat$wtflow_T>0,(R$ProjectOperatingCost_1+R$ProjectOperatingCost_2+R$ProjectOperatingCost_3.1
                                                +R$ProjectOperatingCost_3.2+R$ProjectOperatingCost_3.3+R$ProjectOperatingCost_4+R$ProjectOperatingCost_5
                                                +R$ProjectOperatingCost_6+R$ProjectOperatingCost_7+R$ProjectOperatingCost_8+R$ProjectOperatingCost_9)*DiscR,0)  
      
      R$DisOPCost_Buildtime = sum(R$OP_Cost)*DiscRate^(-Decisions[3])  
      
      R$amo_OP_cost= R$DisOPCost_Buildtime*(1/DiscRate-1)/(1-DiscRate^Decisions[4])    
      
    }
    
    ## TOTAL FACTOR COST ##
    {
      R$labor_factor = Wage_batea/(Wage_source*PPP_12_CAD)
      
      R$Exp_Cap_Cost =  R$ProjectCapitalCost_1*(0.81*R$Exp_factor+0.19*R$labor_factor)+ 
        R$ProjectCapitalCost_2*(0.94*R$Exp_factor+0.06*R$labor_factor)+ 
        R$ProjectCapitalCost_3.1*(0.53*R$Exp_factor+0.47*R$labor_factor)+ 
        R$ProjectCapitalCost_3.2*(0.83*R$Exp_factor+0.17*R$labor_factor)+ 
        R$ProjectCapitalCost_3.3*(0.70*R$Exp_factor+0.30*R$labor_factor)+         
        R$ProjectCapitalCost_5*(0.90*R$Exp_factor+0.10*R$labor_factor)+                
        R$ProjectCapitalCost_6*(0.62*R$Exp_factor+0.38*R$labor_factor)
        #R$ProjectCapitalCost_7*(0.69*R$Exp_factor+0.31*R$labor_factor)
      

      R$Total_Cap_Cost = R$Exp_Cap_Cost*(R$Exp_Cap)+R$ProjectCapitalCost_9+(17820000+26730000)*parms$Adj_f
      # (R$ProjectCapitalCost_4*(0.74+0.26*R$labor_factor) # Excluded
      # 17820000 is I&C from the BATEA report
      # 26730000 is Building/Roads from the BATEA report
      
      R$Exp_Op_Cost = ifelse(treat$wtflow_T>0,(R$ProjectOperatingCost_1*(0.78*R$Exp_factor+0.22*R$labor_factor)+
                         R$ProjectOperatingCost_2*(0.86*R$Exp_factor+0.16*R$labor_factor)+
                         R$ProjectOperatingCost_3.1*(0.54*R$Exp_factor+0.46*R$labor_factor)+
                         R$ProjectOperatingCost_3.2*(0.84*R$Exp_factor+0.16*R$labor_factor)+
                         R$ProjectOperatingCost_3.3*(0.70*R$Exp_factor+0.30*R$labor_factor)+   
                         R$ProjectOperatingCost_5*(0.92*R$Exp_factor+0.08*R$labor_factor)+
                         R$ProjectOperatingCost_6*(0.89*R$Exp_factor+0.11*R$labor_factor))*DiscR,0)
                         #R$ProjectOperatingCost_7*(0.73*R$Exp_factor+0.27*R$labor_factor)
      
      R$Total_Op_Cost = R$Exp_Op_Cost*(R$Exp_Op)+R$ProjectOperatingCost_9*(0.15+0.85*R$labor_factor)+R$ProjectOperatingCost_8.1+R$ProjectOperatingCost_8.2
      # (R$ProjectOperatingCost_4*(0.11*+0.89*R$labor_factor) Excluded
      
      #2.15 is the factor used to obtain the total Capital costs (from CONRAD report)
      #3.62 is the factor used to obtain the total Operational costs (from CONRAD report)
    
     }
    
    ## TOTAL COST ##
    
   {
     if (sum(treat$wtflow_T)>=0.00001) {
      R$Disc_TotalCosts<- (R$Total_Cap_Cost*DiscRate^(Decisions[3]-1)+sum(R$Total_Op_Cost*DiscR))*(PPP_12_CAD) #In CAD
      
      R$DisCost_Buildtime = R$Disc_TotalCosts*DiscRate^(-Decisions[3])# cost at building time
      
      R$amo_cost= R$DisCost_Buildtime*(1/DiscRate-1)/(1-DiscRate^Decisions[4])#amortization cost per year
      
      R$m3_cost = R$amo_cost/((treat$wtflow_T)*1000000)  #Cost per m3 in CAD # Shouldn't this be the maximum as they can change per year?
     }else{
       R$Disc_TotalCosts = 0
       R$DisCost_Buildtime = 0
       R$amo_cost= 0
       R$m3_cost = 0
     }
      
      
    return(R)
  }

}

}
  
### BIOCHAR + WETLAND ### (Sharma et al. (2013), BATEA, CONRAD)
{  
  # WL Decisions

Setup_Wetland_Parameters<-function(WetlandEffectiveness=Effectiveness) 
{
    WLT<-list()
    #Conversion Factors
    WLT$USgal_m3=264.17 #US gallon per m3
    WLT$m3_USgas=0.003785412 #m3 per US gallon
    WLT$AcrePerHa=2.47105382
    
    #Value of Time variables
    interest_rate     = d_rate
    Ammort_Period     = 15
    
    #Base Case parameters for Wetland
    WLT$InflowYr          = 6000000               #m3/yr assuming operating wetland for 6 months only
    WLT$InflowDay         = WLT$InflowYr/(day_y/2)      #m3/day
    WLT$InflowHr          = WLT$InflowYr/(day_y/2*24)   #m3/hr
    WLT$InflowSec         = WLT$InflowDay/(24*60*60)  #m3/sec
    
    WLT$Mm3Yr_to_m3sec    = 1e6/(24*60*60*365)     # conversion factor from millions m3 per year to m3 per sec
    
    WidthBeds         = 792.48	              #metres  or 2600	ft - determines exposure time of water to the treatment
    Lengthbeds        = 2573.712443	          #metres 8443.938462	ft -  the flow rate determines how large this must be for a given rate of water movement - determined by slope
    AreaTreatment     = 203.9615632	          #Ha 	Referred to as area of cells in report (504 acres)  
    DepthBeds         = 0.42672               #metres  1.4 ft in report
    DepthWater	      = 0.42672	              #meters  unspecified in report - assumed
    DepthGravel	      = 0.42672	              #meters  unspecified in report -assumed
    
    LandAreaHa          = WidthBeds*Lengthbeds/10000     #Land area occupied by treatment
    LandAreaAc          = LandAreaHa*WLT$AcrePerHa           #Area occupied by wetland in acres  - base case should be 504 acres
    VolumeSandGravelBed = LandAreaHa*DepthGravel*10000   #m3
    
    GravelVoidSpace          = 0.607054439
    RatioVolumeGravel        = 1/(1+GravelVoidSpace)                       # ratio of volume of gravel in the total space occupied by the gravel
    VoidSpaceOccupiedByWater = (1-RatioVolumeGravel)*VolumeSandGravelBed   #Void space volume occupied by water (m3)
    TreatmentContactDays     = VoidSpaceOccupiedByWater/WLT$InflowDay         #treatment contact days
    RateFlow_BedLength       = WLT$InflowDay/Lengthbeds                        # m3/(day*bed length)
    WaterMovementPerDay        = WidthBeds/TreatmentContactDays            # meters/day
    
    #Base Case: Other construction parameters
    Earthwork_cu_yds        = 9906000       # cubic yards (corresponds to the 504 acres of treatedment area)
    GravelSandFills         = 2056000      # cubic yards 
    WetlandLines            = 21990000      # Square feet synthetic
    Infl_Ef_Spillways       = 22000         # cubic yards of concrete spillways
    # Base Case: unit costs associated with above paramaters
    UCost_Land                = 500         # $/acre
    UCost_Earthwork           = 8           # $/cu yard
    UCost_GravelSandFills     = 5           # $/cu yard
    UCost_WetlandLines        = 1           # $/squarefeet synthetic
    UCost_Infl_Ef_Spillways   = 500         # $/cu yard
    
    #Other Construction Cost iters (total cost basis)
    # Costs based on Factors/ratios of unit costs 
    TailPondEffBox_Piping     = 0.02        #ratio-Tailing pond effluent box and piping
    WetlandWaterLevelStruct   = 0.1         #ratio- Wetland Water level control structures
    Rip_rap                   = 0.005       # ratio of ? Rip-rap
    PlantingCost              = 0.05        # ratio planting cost for wetland plants
    # Other Site Factored Costs (Ratios)
    Substation                = 0.002
    IandC                     = 0.005
    BuildingRoads             = 0.05
    offPlotPiping             = 0.03
    InsulationWinterization   = 0.005
    Other                     = 0.02        #Other costs (Pain,FireProtection,etc)  
    ProjectMan                = 0.02        # project managment and engineering
    
    
    #Total Base Case Capital Costs 
    TC_ItemLabels<-c("Land","Earthwork","GravelSandFills","WetlandLines","Infl_Ef_Spillways")
    TotalItemCosts<-c(UCost_Land*LandAreaAc,
                      UCost_Earthwork*Earthwork_cu_yds,
                      UCost_GravelSandFills*GravelSandFills,
                      UCost_WetlandLines*WetlandLines,
                      UCost_Infl_Ef_Spillways*Infl_Ef_Spillways)
    SumTotalItemCost<-sum(TotalItemCosts)/1e6 # convert costs to millions of $
    
    TC_RatioBaseLabels<-c("TailBondEffPox_Piping","WetlandWaterLevelStruct","Rip_rap","PlantingCost","Substation","IandC","BuildingRoads",
                          "OffPlotPiping","InsulationWinterization","Other","ProjectMan")
    TotalCost_RatioBased<-c(TailPondEffBox_Piping,
                            WetlandWaterLevelStruct,
                            Rip_rap,
                            PlantingCost,
                            Substation,
                            IandC,
                            BuildingRoads,
                            offPlotPiping,
                            InsulationWinterization,
                            Other,
                            ProjectMan)*SumTotalItemCost  # convert costs to millions of $
    SumTotalCost_RatioBased<-sum(TotalCost_RatioBased)
    TotalCapitalCosts<-SumTotalItemCost+SumTotalCost_RatioBased
    
    
    ##Base Case Operating Costs (Annual)
    Ucost_Labour = 80    # $/hr
    ManHour_Labour = 6570 # hours
    Maintenance   = 0.005   # % of total construction items
    Supplies      = 0.1   # % of maintenance costs
    Laboratory    = 0.1   # % labor costs
    OperCostLabels<-c("Labour","Maintenance","Supplies","Laboratory")
    OperatingCosts<-c(Ucost_Labour*ManHour_Labour,Maintenance*SumTotalItemCost)
    OperatingCosts<-c(OperatingCosts,Supplies*OperatingCosts[2],Laboratory*OperatingCosts[1])
    TotalOperatingCosts<-sum(OperatingCosts)/1e6
    
    ##### Average Total Costs #####
    AnnualizedCapCost<-TotalCapitalCosts*interest_rate/(1-(1+interest_rate)^(-Ammort_Period))
    TotalCosts<-AnnualizedCapCost+TotalOperatingCosts
    AverageTotalCosts<-TotalCosts*1e6/WLT$InflowYr  # Average total costs per m3($/m3) water treated per year
    
    #### Adjusted factors ###
    CPI_11 = 224.94 #CPI for 2011 (Average)
    CPI_12 = 229.54 #CPI for 2012 (Average)
    CPI_22 = 291.73 #CPI for 2022 (Average) (until October)
    WLT$Adj_f = CPI_12/CPI_11 #Adjustment factor
    WLT$Adj_f2 = CPI_12/CPI_22 #Adjustment factor
    
    WLT$coef = 0.865 # to make it close to the linear approximation (change it parameter)
    
    
    #### wetland input and outputs  ####
    RawOSPWTreated=1363 #(DecisionVariables[1]+DecisionVariables[2])*1e6/(day_y/2*24)   #m3/hr (Summer and winter OSPW)
    Precipitation=144     #m3  Total m3 precip over the 504 acres of base project
    Evapotranspiration=288 #m3  Total m3 evapotranspiration over the 504 acres of base project
    Effluent=RawOSPWTreated+Precipitation-Evapotranspiration
    Ratio_Effl_Infl=Effluent/RawOSPWTreated
    Infl_Poll_conc<-c(2000,1,44,20,0.1)   #mg/L
    Effl_Poll_conc<-c(2236,0.5,44*exp(-WetlandEffectiveness*10)/Ratio_Effl_Infl,5,0.05)   #mg/L 25  Assumptions Original 22 or exp(-0.067*10), exp(-0.026*10) or ~23%, 
    ratePoll_reduc<-(-log(Effl_Poll_conc/Infl_Poll_conc*Ratio_Effl_Infl))/TreatmentContactDays # exponential function reduction in NAs
    WaterLossRate=(RawOSPWTreated-Effluent)/TreatmentContactDays/RawOSPWTreated   # linear per day loss of water 
    
    WLT$WaterLossRate<-WaterLossRate
    WLT$RateFlow_BedLength<-RateFlow_BedLength
    WLT$AreaTreatment<-AreaTreatment
    WLT$TotalCapitalCosts<-TotalCapitalCosts
    WLT$TotalOperatingCosts<-TotalOperatingCosts
    WLT$AnnualizedCapCost<-AnnualizedCapCost
    WLT$WaterMovementPerDay<-WaterMovementPerDay
    WLT$ratePoll_reduc<-ratePoll_reduc
    
    
    WLT$PostTreatConc<-(exp(-(WLT$ratePoll_reduc)*c(seq(1,90,1))))/(1-WLT$WaterLossRate*c(seq(1,90,1)))
    # the valley is at 53 days, here is how to find it:
    WLT$Max_TreatTime = which(WLT$PostTreatConc==min(WLT$PostTreatConc))-1 # Max treatment time for Decision 1
    # decision variable is <= end of pipe < Chronic limit
    
    WLT$Min_PostConc<-(exp(-(WLT$ratePoll_reduc)*WLT$Max_TreatTime))/(1-WLT$WaterLossRate*WLT$Max_TreatTime)
    #Add a target concentration bound

    return(WLT)
    
  }
  
WL_Parameters<-Setup_Wetland_Parameters(WetlandEffectiveness=Effectiveness)
  
Setup_BC_Parameters<-function(interest_rate= d_rate){ 
    BC=list()
    #Conversion Factors
    BC$USgal_m3=264.17 #US gallon per m3
    BC$m3_USgas=0.003785412 #m3 per US gallon
    BC$AcrePerHa=2.47105382
    BC$m2_ft2 = 0.09290304 #m2 per ft2
    
    #Value of Time variables
    Ammort_Period     = 15 # Years
    BC$PW = (1-1/(1+interest_rate)^Ammort_Period)/interest_rate# Present worth factor
    
    
    #Base Case parameters for Wetland
    InflowYr          = 6000000               #m3/yr (UPDATE)
    InflowDay         = InflowYr/(365)      #m3/day (UPDATE)
    InflowHr          = InflowYr/(365*24)   #m3/hr (UPDATE)
    InflowSec         = InflowDay/(24*60*60)  #m3/sec (UPDATE)
    InfYrM             =  InflowYr/1000000      #Mm3/yr (UPDATE)
    
    
    # Biochar Design Assumptions: Total equipment cost
    CPI_11 = 224.94 #CPI for 2011 (Average)
    CPI_12 = 229.54 #CPI for 2012 (Average)
    CPI_22 = 291.73 #CPI for 2022 (Average) (until October)
    BC$Adj_f = CPI_12/CPI_11 #Adjustment factor
    BC$Adj_f2 = CPI_12/CPI_22 #Adjustment factor 2
    
    BC$time = 2 # treatment time in hours (UPDATE)
    BC$coef = 0.865 # to make it close to the linear approximation (change it parameter)
    
    BC$price = 110*PPP_12_CAD  
    
    BC$dose = 2 # gr per liter
    
    BC$field=0.85 # Effect of Biochar in the field
    BC$NA_reduc = 0.86 # NAs concentration reduction for 16 mg/L initial concentration and 2g/L
    

    # Balance hours
      
    BC$Thours = 2 # Biochar treatment hours (hours)
      
      return(BC)
}
  
BCpre_Parameters<-Setup_BC_Parameters(interest_rate= d_rate)   

BCpre_TreatmentEffect<-function(T=T,TreaT=TreaT,NATarget=DecisionVariables[7],WL_TreatTime=DecisionVariables[6],BioChar=TRUE, 
           TreatVec_S=Effluent$Summer_Treat_Flow, Pollconc=concPollutants,parms=WL_Parameters,parms2=BCpre_Parameters) {
      R<-list()
      
      R$max_Pollc = max(Pollconc[,3]) # Max concentration
      
      R$TreatTimeV <-rep(0,Nperiods)
      R$TreatTimeV[T:TreaT]= WL_TreatTime  #Decisions[6]

      R$Targetconc = NATarget #Decisions[7] # Target
       
      # Inflow to costs
      R$Inflow = TreatVec_S
   
      # Concentration Wetland alone
      R$WLPostconc <-matrix(0, nrow = Nperiods,ncol = 5)
      R$WLPostconc[T:TreaT,3]<-Pollconc[T:TreaT,3]*((exp(-parms$ratePoll_reduc[3]*R$TreatTimeV[T:TreaT]))/(1-parms$WaterLossRate*R$TreatTimeV[T:TreaT]))
      R$Effluent_Flow<-(1-parms$WaterLossRate*R$TreatTimeV)*R$Inflow
      R$WLPostconc[T:TreaT,1]<-Pollconc[T:TreaT,1]*((exp(-parms$ratePoll_reduc[1]*R$TreatTimeV[T:TreaT]))/(1-parms$WaterLossRate*R$TreatTimeV[T:TreaT]))
      R$WLPostconc[T:TreaT,2]<-Pollconc[T:TreaT,2]*((exp(-parms$ratePoll_reduc[2]*R$TreatTimeV[T:TreaT]))/(1-parms$WaterLossRate*R$TreatTimeV[T:TreaT]))
      R$WLPostconc[T:TreaT,4]<-Pollconc[T:TreaT,4]*((exp(-parms$ratePoll_reduc[4]*R$TreatTimeV[T:TreaT]))/(1-parms$WaterLossRate*R$TreatTimeV[T:TreaT]))
      R$WLPostconc[T:TreaT,5]<-Pollconc[T:TreaT,5]*((exp(-parms$ratePoll_reduc[5]*R$TreatTimeV[T:TreaT]))/(1-parms$WaterLossRate*R$TreatTimeV[T:TreaT]))
      
        R$Pre_BC = R$WLPostconc[,3]-R$Targetconc #Difference between wetland effect alone and target
      
        R$Opt_WetConc <-matrix(0, nrow = Nperiods,ncol = 5)
        R$Opt_WetConc[T:TreaT,3] = ifelse(R$WLPostconc[T:TreaT,3]>R$Targetconc,R$Targetconc/((exp(-parms$ratePoll_reduc[3]*R$TreatTimeV[T:TreaT]))/(1-parms$WaterLossRate*R$TreatTimeV[T:TreaT])),Pollconc[T:TreaT,3]) # Optimal concentration after BC
      
        ## Capital cost inputs
        R$BC_effect= parms2$NA_reduc*parms2$field # Concentration reduction caused by BioChar
      
        R$Opt_BCRedC = matrix(0,Nperiods,1)
        R$Opt_BCRedC[T:TreaT] = Pollconc[T:TreaT,3]-R$Opt_WetConc[T:TreaT,3] # Desire BC reduction in concentration before going to the wetland
      
        R$Opt_BCRed = matrix(0,Nperiods,1)
        R$Opt_BCRed[T:TreaT] = R$Opt_BCRedC[T:TreaT]/Pollconc[T:TreaT,3] # Desire BC reduction in %
      
        R$Opt_BCRema = matrix(0,Nperiods,1)
        R$Opt_BCRema[T:TreaT] = (1-R$Opt_BCRed[T:TreaT])*100

      
        R$BC_dose = matrix(0,Nperiods,1)
        R$BC_dose[T:TreaT] = ifelse(R$Pre_BC[T:TreaT] <=0,0,(ln(R$Opt_BCRema[T:TreaT]/100)*1/-0.47038)^(1/0.85972)*(Pollconc[T:TreaT,3]/100))
      
      # Concentration Wetland and biochar
      
      R$Con_afterBC = matrix(0,Nperiods,ncol=5)
      R$Con_afterBC[T:TreaT,3] = (100*exp(-0.47038*(R$BC_dose[T:TreaT]/(Pollconc[T:TreaT,3]/100))^0.85972))*Pollconc[T:TreaT,3]/100  
      
      R$Con_afterBC[T:TreaT,1] = Pollconc[T:TreaT,1]*(1-0)
      R$Con_afterBC[T:TreaT,2] = Pollconc[T:TreaT,2]*(1-0)
      R$Con_afterBC[T:TreaT,4] = Pollconc[T:TreaT,4]*(1-0)
      R$Con_afterBC[T:TreaT,5] = Pollconc[T:TreaT,5]*(1-0)
      
      R$WLPollconc_BC <-matrix(0, nrow = Nperiods,ncol = 5)
      R$WLPollconc_BC[T:TreaT,1]<- R$Con_afterBC[T:TreaT,1]*((exp(-parms$ratePoll_reduc[1]*R$TreatTimeV[T:TreaT]))/(1-parms$WaterLossRate*R$TreatTimeV[T:TreaT]))
      R$WLPollconc_BC[T:TreaT,2]<-R$Con_afterBC[T:TreaT,2]*((exp(-parms$ratePoll_reduc[2]*R$TreatTimeV[T:TreaT]))/(1-parms$WaterLossRate*R$TreatTimeV[T:TreaT]))
      R$WLPollconc_BC[T:TreaT,3]<-R$Con_afterBC[T:TreaT,3]*((exp(-parms$ratePoll_reduc[3]*R$TreatTimeV[T:TreaT]))/(1-parms$WaterLossRate*R$TreatTimeV[T:TreaT]))    
      R$WLPollconc_BC[T:TreaT,4]<-R$Con_afterBC[T:TreaT,4]*((exp(-parms$ratePoll_reduc[4]*R$TreatTimeV[T:TreaT]))/(1-parms$WaterLossRate*R$TreatTimeV[T:TreaT]))
      R$WLPollconc_BC[T:TreaT,5]<-R$Con_afterBC[T:TreaT,5]*((exp(-parms$ratePoll_reduc[5]*R$TreatTimeV[T:TreaT]))/(1-parms$WaterLossRate*R$TreatTimeV[T:TreaT]))
      
      #Final concentration
      if(BioChar){
        R$Postconc<-R$WLPollconc_BC
      } else {
        R$Postconc<-R$WLPostconc
      }
      R$BioChar=BioChar
      return(R)
  }

tech_treat <- BCpre_TreatmentEffect(T=T,TreaT=TreaT,NATarget=DecisionVariables[7],WL_TreatTime=DecisionVariables[6],BioChar=TRUE, 
                                  TreatVec_S=rep(DecisionVariables[1],70), Pollconc=concPollutants,parms=WL_Parameters,parms2=BCpre_Parameters)
                                    
                                    
#BCp_CostFn <- function(Decisions=DecisionVariables,parms=WL_Parameters,parms2=BCpre_Parameters,treat=BCpre_treat,exp_factor=factors)
BCp_CostFn <- function(T=T,TreaT=TreaT,WL_TreatTime=DecisionVariables[6],BioChar=TRUE,parms=WL_Parameters,parms2=BCpre_Parameters,treat=tech_treat,exp_factor=factors,TruckCost=250) # Add a cost here
    #End = 0 (treatment wetland), 1 (end pitlake)
    {
    R=list()
    
    R$Exp_factor = exp_factor[1] # Expansion factor for a Oil Sand company to multiply by cost
    R$Exp_Cap =exp_factor[2] #factor capital cost
    R$Exp_Op =exp_factor[3] #factor ope cost

    ## 1 Adquisition of Biochar and transport ## 
    {
    
    R$total_BC = ifelse(treat$BC_dose<=0,0,treat$BC_dose/1e6*treat$Inflow*1e6*1e3)  # gr/1e6 (tonnes/L)* million M3*1e3 (L)= Tonnes of biochar per year (Transport will be assume to be annual)   
    R$Total_BC_t = sum(R$total_BC) #Sum of all BC in tonnes
      
    R$truck = TruckCost # 250 # cost per hour (Couldn't they be company trucks? or this is just gas)
    

    R$tonnes_truck = 25 # tonnes in the truck
    R$distance = 50 # km # assuming that there is close plant
    
    R$ProjectOperatingCost_1= ((((R$distance/80*2)+2)/R$tonnes_truck*R$truck)*R$total_BC+R$total_BC*parms2$price)*parms$Adj_f2
  
   }
    

    ## 2. Raw water pumping (TDH = 30.48m  (100 ft))## 
    # For the assumption in the oil sands it seems that the TDH will not exceed 30.48
    {
      R$mgd_f = 1 # mgd capacity floor (from model)
      R$mgd_l = 200 # mgd capacity limit (from model)
      R$r_mgd = max(c(treat$Inflow))*1e6/(day_y/2)*parms$USgal_m3/1e6 # Required max water treated in gallons per day 
      R$a_mgd = max(c(R$mgd_f,min(c(R$r_mgd,R$mgd_l)))) # Allowed mgd per day in the system (if is lower than the floor, the floor will be used)
      
      R$ScaleFactor_wp = (R$r_mgd/min(R$r_mgd, R$a_mgd))^parms$coef #There will be no scaling if the minimum amount is used
      ## Capital Cost (Investment)
      
      if(BioChar){
        R$ProjectCapitalCost_2 = (13060*R$a_mgd+71118)*R$ScaleFactor_wp *parms$Adj_f
      } else {
        R$ProjectCapitalCost_2=0
      }
      ## Operational cost (annual) 
      R$min_mgd_c =  R$mgd_f/1e6*(day_y/2)/parms$USgal_m3*1e6 # Minimum capacity 
      R$max_mgd_c =  R$mgd_l/1e6*(day_y/2)/parms$USgal_m3*1e6 # Maximum capacity 
      
      R$mgd_S =treat$Inflow*1e6/(day_y/2)*parms$USgal_m3/1e6 # required for summer in mgd
      
      R$ScaleFactor_wp_S = ifelse(R$mgd_S>0,(R$mgd_S/pmin(R$mgd_S, R$mgd_l))^parms$coef,0) #Scale factor for operational cost in SUMMER
      
      R$t_mgd_S = ifelse(treat$Inflow>0,pmax(R$min_mgd_c,pmin(R$mgd_S,R$mgd_l)),0) #truncate to min and max mgd for summer
      
      R$ProjectOperatingCost_2 = rep(0,70)
      if(BioChar){
        R$ProjectOperatingCost_2[T:TreaT] = ifelse(treat$Inflow[T:TreaT]>0, (8979.1*R$t_mgd_S+24960)*(R$ScaleFactor_wp_S/2*parms$Adj_f),0) # Total project operation cost SUMMER (divided by 2 as is an annual operational cost)
      } else {
        R$ProjectOperatingCost_2[T:TreaT]=rep(0,(TreaT-T+1))
      }
      
    }
    
    ## 3. Distribution of Biochar - with mixing system  
    {
    
      Gall_ft3 = 0.1336805556 # ft3 per gallon
      # NEW CHANGE
      volumeb = 0.5*max(R$t_mgd_S,R$t_mgd_W)*1e6/24/60*Gall_ft3
      
      #volumeb = 0.5*R$max_mgd_c*1e6/24/60*Gall_ft3  # retention time (30 seconds) * gpm flow (ft3)  Old
      
      R$f_bvol = 0 # Rapid mix floor (ft3) - Actual is 10
      R$l_bvol = 20000 # Rapid mix limit (ft3)
      R$r_bvol = volumeb # Maximum basin volume (ft3)
      R$a_bvol = max(R$f_bvol,min(R$r_bvol, R$l_bvol)) # Allowed    
      
      R$ScaleFactor_bvol = (R$r_bvol/min(R$r_bvol, R$a_bvol))^parms$coef #There will be no scaling if the minimum amount is used
      
      R$volumeb_v = 0.5*R$mgd_S*1e6/24/60*Gall_ft3  # retention time (30 seconds) * gpm flow (ft3) 
      if(BioChar){
        ## Capital Cost (Investment)
        R$ProjectCapitalCost_3 = ifelse(sum(R$total_BC)>0,(0.0002*R$a_bvol^2+58.159*R$a_bvol+30823)*R$ScaleFactor_bvol*parms$Adj_f,0)
        ## Operational Cost 
        R$ProjectOperatingCost_3 =  ifelse(R$total_BC>0,(37.081*R$volumeb_v+ifelse(R$volumeb_v>0,18839,0))*(R$ScaleFactor_bvol*parms$Adj_f),0)  
      } else {
        R$ProjectCapitalCost_3 =0
        R$ProjectOperatingCost_3=rep(0,Nperiods)
      }
    }
    

    ## 4. Wetland cost ##
    {
    # Capital Cost (Build wetland)
    R$TreatTime = min(parms$Max_TreatTime ,WL_TreatTime)
    
    # multiplied by two to account for running this for 1/2 a year - the inflow rate is for the summer period only
    R$Infl_flow_rate_m3sec=treat$Inflow*parms$Mm3Yr_to_m3sec*2
    R$Effl_flow_rate_m3sec=R$Infl_flow_rate_m3sec*(1-R$T*parms$WaterLossRate)  #m3/sec
    # 86400=60*60*24 seconds per day, converts inflow rate in m3/sec to m3/day
    R$Length_beds =R$Infl_flow_rate_m3sec*86400/parms$RateFlow_BedLength  #RateFlow_BedLength       = InflowDay/Lengthbeds       86400=24*60*60                 # m3/(day*bed length),
    
    R$Width_beds=parms$WaterMovementPerDay*R$TreatTime
    R$Area_Beds=R$Length_beds*R$Width_beds/10000  #m3 to ha
    R$ScaleFactor=R$Area_Beds/parms$AreaTreatment
    
    R$ProjectCapitalCost_4=max(c(R$ScaleFactor))*parms$TotalCapitalCosts*1e6   # $/yr

    #Operational Cost
    
    # Water treatment vectors
    R$ProjectOperatingCost_4<-rep(0,Nperiods)
    R$ProjectOperatingCost_4[T:TreaT]<-R$ScaleFactor[T:TreaT]*parms$TotalOperatingCosts*1e6
    }
    
    ## 5. Management ## for BioChar only
    {
    # Using the Batea report cost, the coefficient is 3971011.313 and 72277.67
      if(BioChar) {
        R$ProjectCapitalCost_5 = ifelse(sum(R$total_BC)>0,(3971011.313*R$r_mgd^0.5523)*parms$Adj_f,0)
        R$ProjectOperatingCost_5 = ifelse(R$total_BC>0,(72277.67*(R$mgd_S)^0.4526)*parms$Adj_f,0)
      } else {
         R$ProjectCapitalCost_5=0
         R$ProjectOperatingCost_5=rep(0,Nperiods)
      }
      
    }
    
    ## TOTAL EQUIPMENT COST ##
    {
      # Total cost  
      R$Cap_Cost <- (R$ProjectCapitalCost_2+ R$ProjectCapitalCost_3+ R$ProjectCapitalCost_4+ R$ProjectCapitalCost_5)
      
      R$OP_Cost <-ifelse(treat$Inflow>0,(R$ProjectOperatingCost_1+R$ProjectOperatingCost_2+R$ProjectOperatingCost_3+ R$ProjectCapitalCost_4+ R$ProjectCapitalCost_5)*DiscR,0)  
      
      R$DisOPCost_Buildtime = sum(R$OP_Cost)*DiscRate^(-T)  
      
      R$amo_OP_cost= R$DisOPCost_Buildtime*(1/DiscRate-1)/(1-DiscRate^(TreaT-T))    
      
    }
    
    ## TOTAL FACTOR COST ##
    {
      #R$ProjectCapitalCost_4 = 0
      #R$ProjectOperatingCost_4 = 0
      
      R$labor_factor = Wage_batea/(Wage_source*PPP_12_CAD)
      # Define and justify which costs should be affected by the expansion factors
      #Biochar capital Costs (pumping and distribution)
      R$Exp_Cap_Cost =  R$ProjectCapitalCost_2*(0.53*R$Exp_factor+0.47*R$labor_factor)+
                        R$ProjectCapitalCost_3*(0.83*R$Exp_factor+0.17*R$labor_factor)
      
      #Wetland capital costs 
      if(BioChar){
        if(WL_TreatTime<=0.00001){
          R$Total_Cap_Cost =  R$Exp_Cap_Cost*(R$Exp_Cap)
        }else{
          R$Total_Cap_Cost =  R$Exp_Cap_Cost*(R$Exp_Cap)+(R$ProjectCapitalCost_4+R$ProjectCapitalCost_5)+(17820000+26730000)*parms$Adj_f
        }
      } else {
        if(WL_TreatTime<=0.00001){
          R$Total_Cap_Cost = 0 
        }else{
          R$Total_Cap_Cost = (R$ProjectCapitalCost_4+R$ProjectCapitalCost_5)+(17820000+26730000)*parms$Adj_f
        }
      }
      
      
      # 17820000 is I&C from the BATEA report
      # 26730000 is Building/Roads from the BATEA report
      #biochar operating costs
      R$Exp_Op_Cost = ifelse(treat$Inflow>0,(R$ProjectOperatingCost_2*(0.78*R$Exp_factor+0.22*R$labor_factor)+
                                             R$ProjectOperatingCost_3*(0.84*R$Exp_factor+0.16*R$labor_factor))*DiscR,0)
      
        #Wetland operating costs+biochar operating costs
      ## But if Wetland is not selected... then there is no need to use its costs
      if(BioChar){
        if(WL_TreatTime<=0.00001){
          R$Total_Op_Cost = R$Exp_Op_Cost*(R$Exp_Op)+(R$ProjectOperatingCost_1+R$ProjectOperatingCost_5*(0.15+0.85*R$labor_factor))
        }else{
          R$Total_Op_Cost = R$Exp_Op_Cost*(R$Exp_Op)+(R$ProjectOperatingCost_1+R$ProjectOperatingCost_4+R$ProjectOperatingCost_5*(0.15+0.85*R$labor_factor))
        }
        
      } else {
        if(WL_TreatTime<=0.00001){
          R$Total_Op_Cost =0
        }else{
          R$Total_Op_Cost =R$ProjectOperatingCost_4+R$ProjectOperatingCost_5*(0.15+0.85*R$labor_factor)
        }  
      }    

      }
    
    ## TOTAL COST ##
    # Capital cost should also be discounted to year 0
    # 

    {
      if (sum(treat$Inflow)>=0.00001) {
        
      R$Disc_TotalCosts<-(R$Total_Cap_Cost*DiscRate^T+sum(R$Total_Op_Cost*DiscR))*(PPP_12_CAD) #In CAD
      # All discount happens at the same time
      
      R$DisCost_Buildtime = R$Disc_TotalCosts*DiscRate^(-T)# cost at building time
      # Present value at building time
      
      R$amo_cost= R$DisCost_Buildtime*(1/DiscRate-1)/(1-DiscRate^(TreaT-T))#amortization cost per year
      
      R$m3_cost = R$amo_cost/(mean(treat$Inflow[T:TreaT])*1000000)  #Cost per m3 in CAD (Should be per year?)
      }else{
        R$Disc_TotalCosts =0
        R$DisCost_Buildtime = 0        
        R$amo_cost= 0
        R$m3_cost = 0
      }
    }
    
    return(R)
  }
 
{  
    # WL Decisions
    # Type is the End_type 1, wetland, 2 wet+bc, 3 BC # PENDING
    
Setup_Wetland_Parameters2<-function(WetlandEffectiveness=Effectiveness) # JJFF = Higher effectiveness
{
      WLT<-list()
      #Conversion Factors
      WLT$USgal_m3=264.17 #US gallon per m3
      WLT$m3_USgas=0.003785412 #m3 per US gallon
      WLT$AcrePerHa=2.47105382
      
      #Value of Time variables
      interest_rate     = d_rate
      Ammort_Period     = 15
      
      #Base Case parameters for Wetland
      WLT$InflowYr          = 6000000               #m3/yr assuming operating wetland for 6 months only
      WLT$InflowDay         = WLT$InflowYr/(day_y/2)      #m3/day
      WLT$InflowHr          = WLT$InflowYr/(day_y/2*24)   #m3/hr
      WLT$InflowSec         = WLT$InflowDay/(24*60*60)  #m3/sec
      
      WLT$Mm3Yr_to_m3sec    = 1e6/(24*60*60*365)     # conversion factor from millions m3 per year to m3 per sec
      
      WidthBeds         = 792.48	              #metres  or 2600	ft - determines exposure time of water to the treatment
      Lengthbeds        = 2573.712443	          #metres 8443.938462	ft -  the flow rate determines how large this must be for a given rate of water movement - determined by slope
      AreaTreatment     = 203.9615632	          #Ha 	Referred to as area of cells in report (504 acres)  
      DepthBeds         = 0.42672               #metres  1.4 ft in report
      DepthWater	      = 0.42672	              #meters  unspecified in report - assumed
      DepthGravel	      = 0.42672	              #meters  unspecified in report -assumed
      
      LandAreaHa          = WidthBeds*Lengthbeds/10000     #Land area occupied by treatment
      LandAreaAc          = LandAreaHa*WLT$AcrePerHa           #Area occupied by wetland in acres  - base case should be 504 acres
      VolumeSandGravelBed = LandAreaHa*DepthGravel*10000   #m3
      
      GravelVoidSpace          = 0.607054439
      RatioVolumeGravel        = 1/(1+GravelVoidSpace)                       # ratio of volume of gravel in the total space occupied by the gravel
      VoidSpaceOccupiedByWater = (1-RatioVolumeGravel)*VolumeSandGravelBed   #Void space volume occupied by water (m3)
      TreatmentContactDays     = VoidSpaceOccupiedByWater/WLT$InflowDay         #treatment contact days
      RateFlow_BedLength       = WLT$InflowDay/Lengthbeds                        # m3/(day*bed length)
      WaterMovementPerDay        = WidthBeds/TreatmentContactDays            # meters/day
      
      #Base Case: Other construction parameters
      Earthwork_cu_yds        = 9906000       # cubic yards (corresponds to the 504 acres of treatedment area)
      GravelSandFills         = 2056000      # cubic yards 
      WetlandLines            = 21990000      # Square feet synthetic
      Infl_Ef_Spillways       = 22000         # cubic yards of concrete spillways
      # Base Case: unit costs associated with above paramaters
      UCost_Land                = 500         # $/acre
      UCost_Earthwork           = 8           # $/cu yard
      UCost_GravelSandFills     = 5           # $/cu yard
      UCost_WetlandLines        = 1           # $/squarefeet synthetic
      UCost_Infl_Ef_Spillways   = 500         # $/cu yard
      
      #Other Construction Cost iters (total cost basis)
      # Costs based on Factors/ratios of unit costs 
      WetlandWaterLevelStruct   = 0.1         #ratio- Wetland Water level control structures
      Rip_rap                   = 0.005       # ratio of ? Rip-rap
      PlantingCost              = 0.05        # ratio planting cost for wetland plants
      # Other Site Factored Costs (Ratios)
      IandC                     = 0.005
      ProjectMan                = 0.02        # project managment and engineering
      
      
      #Total Base Case Capital Costs 
      TC_ItemLabels<-c("Land","Earthwork","GravelSandFills","WetlandLines","Infl_Ef_Spillways")
      TotalItemCosts<-c(UCost_Land*LandAreaAc,
                        UCost_Earthwork*Earthwork_cu_yds,
                        UCost_GravelSandFills*GravelSandFills,
                        UCost_WetlandLines*WetlandLines,
                        UCost_Infl_Ef_Spillways*Infl_Ef_Spillways)
      SumTotalItemCost<-sum(TotalItemCosts)/1e6 # convert costs to millions of $
      
      TC_RatioBaseLabels<-c("TailBondEffPox_Piping","WetlandWaterLevelStruct","Rip_rap","PlantingCost","Substation","IandC","BuildingRoads",
                            "OffPlotPiping","InsulationWinterization","Other","ProjectMan")
      TotalCost_RatioBased<-c(WetlandWaterLevelStruct,
                              Rip_rap,
                              PlantingCost,
                              IandC,
                              ProjectMan)*SumTotalItemCost  # convert costs to millions of $
      SumTotalCost_RatioBased<-sum(TotalCost_RatioBased)
      TotalCapitalCosts<-SumTotalItemCost+SumTotalCost_RatioBased
      
      
      ##Base Case Operating Costs (Annual)
      Ucost_Labour = 80    # $/hr
      ManHour_Labour = 6570 # hours
      Maintenance   = 0.005   # % of total construction items
      Supplies      = 0.1   # % of maintenance costs
      Laboratory    = 0.1   # % labor costs
      OperCostLabels<-c("Labour","Maintenance","Supplies","Laboratory")
      OperatingCosts<-c(Ucost_Labour*ManHour_Labour,Maintenance*SumTotalItemCost)
      OperatingCosts<-c(OperatingCosts,Supplies*OperatingCosts[2],Laboratory*OperatingCosts[1])
      TotalOperatingCosts<-sum(OperatingCosts)/1e6
      
      ##### Average Total Costs #####
      AnnualizedCapCost<-TotalCapitalCosts*interest_rate/(1-(1+interest_rate)^(-Ammort_Period))
      TotalCosts<-AnnualizedCapCost+TotalOperatingCosts
      AverageTotalCosts<-TotalCosts*1e6/WLT$InflowYr  # Average total costs per m3($/m3) water treated per year
      
      #### Adjusted factors ###
      CPI_11 = 224.94 #CPI for 2011 (Average)
      CPI_12 = 229.54 #CPI for 2012 (Average)
      CPI_22 = 291.73 #CPI for 2022 (Average) (until October)
      WLT$Adj_f = CPI_12/CPI_11 #Adjustment factor
      WLT$Adj_f2 = CPI_12/CPI_22 #Adjustment factor
      
      WLT$coef = 0.865 # to make it close to the linear approximation (change it parameter)
      
      #### wetland input and outputs  ####
      RawOSPWTreated=1363 # (DecisionVariables[1]+DecisionVariables[2])*1e6/(day_y/2*24)   #m3/hr (Summer and winter OSPW)
      Precipitation=144     #m3  Total m3 precip over the 504 acres of base project
      Evapotranspiration=288 #m3  Total m3 evapotranspiration over the 504 acres of base project
      Effluent=RawOSPWTreated+Precipitation-Evapotranspiration
      Ratio_Effl_Infl=Effluent/RawOSPWTreated
      Infl_Poll_conc<-c(2000,10,44,20,0.1)   #mg/L
      Effl_Poll_conc<-c(2236,5,44*exp(-WetlandEffectiveness*10)/Ratio_Effl_Infl,5,0.05)   #mg/L 25  Assumptions Original 22 or exp(-0.067*10), exp(-0.026*10) or ~23%, 
      ratePoll_reduc<-(-log(Effl_Poll_conc/Infl_Poll_conc*Ratio_Effl_Infl))/TreatmentContactDays # exponential function reduction in NAs
      WaterLossRate=(RawOSPWTreated-Effluent)/TreatmentContactDays/RawOSPWTreated   # linear per day loss of water 
      
      WLT$WaterLossRate<-WaterLossRate
      WLT$RateFlow_BedLength<-RateFlow_BedLength
      WLT$AreaTreatment<-AreaTreatment
      WLT$TotalCapitalCosts<-TotalCapitalCosts
      WLT$TotalOperatingCosts<-TotalOperatingCosts
      WLT$AnnualizedCapCost<-AnnualizedCapCost
      WLT$WaterMovementPerDay<-WaterMovementPerDay
      WLT$ratePoll_reduc<-ratePoll_reduc
      
      WLT$PostTreatConc<-(exp(-(WLT$ratePoll_reduc)*c(seq(1,90,1))))/(1-WLT$WaterLossRate*c(seq(1,90,1)))
      # the valley is at 53 days, here is how to find it:
      WLT$Max_TreatTime = which(WLT$PostTreatConc==min(WLT$PostTreatConc))-1 # Max treatment time for Decision 1
      # decision variable is <= end of pipe < Chronic limit
      
      WLT$Min_PostConc<-(exp(-(WLT$ratePoll_reduc)*WLT$Max_TreatTime))/(1-WLT$WaterLossRate*WLT$Max_TreatTime)
      #Add a target concentration bound
      
      
      return(WLT)
      
    }
    
WL_Parameters2<-Setup_Wetland_Parameters2(WetlandEffectiveness=Effectiveness)
  } 
  
##################### End of module 2  ######


  
  
