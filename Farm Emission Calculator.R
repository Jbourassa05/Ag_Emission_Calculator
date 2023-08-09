# Emissions From Cattle
# Test
# 2019 Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories Chapter V4, Chapter 10.

# Feed Intake: Tier 2 Gross Energy Approach ----

#-------------------------------------------------------------------------------#
# Cold Adjustment Factor
  # Equation 10.2
#-------------------------------------------------------------------------------#

CF.Cold<-function(CF, Ct){
  CF + 0.0048*(20-Ct)
}
    # CF: MJ/day/kg
      # Non-Lactating, steers, heifers and calves = 0.322
      # Lactating Cows = 0.386
      # Bulls = 0.370
    # Ct: Mean daily temperature during winter season

#-------------------------------------------------------------------------------#
# Net Energy for Maintenance
  # Equation 10.3
#-------------------------------------------------------------------------------#

NE.M <- function(CF, Weight.A){
  CF*(Weight.A)^0.75
}
  # CF: MJ/day/kg
    # Non-Lactating, steers, heifers and calves = 0.322
    # Lactating Cows = 0.386
    # Bulls = 0.370
  # Weight: Live weight of animal, kg
#-------------------------------------------------------------------------------#
# Net Energy for Activity
  # Equation 10.4
#-------------------------------------------------------------------------------#

NE.A <- function(Ca, NE.M){
  Ca*NE.M
}
  # Ca: coefficient corresponding to animal’s feeding situation
    # Stall = 0: Animals are confined to a small area (i.e., tethered, pen, barn) with the result that they expend very little or no energy to acquire feed.
    # Pasture = 0.17: Animals are confined in areas with sufficient forage requiring modest energy expense to acquire feed.
    # Grazing large areas = 0.36: Animals graze in open range land or hilly terrain and expend significant energy to acquire feed.
  # NE_M: net energy required by the animal for maintenance MJ/day

#-------------------------------------------------------------------------------#
# Net Energy for Growth
  # Equation 10.6
#-------------------------------------------------------------------------------#

NE.G <- function(Weight.A, Cs, Weight.M, Weight.G){
  22.02*((Weight.A/(Cs*Weight.M))^0.75)*(Weight.G^1.097)
}

  # Weight.A: Live body weight (kg)
  # Cs: Coefficent for cattle sex
    # Female: 0.8
    # Steers: 1
    # Bulls: 1.2
  # Weight.M: Mature body weight/Target weight (kg)
  # Weight.G: Average daily weight gain (kg/day)

#-------------------------------------------------------------------------------#
# Net Energy for Lactation
  # Equation 10.8
#-------------------------------------------------------------------------------#

NE.L <- function(Milk, Fat){
  Milk*(1.47+(0.4*Fat))
}

  # Milk: Milk produced (kg milk/day): # IPCC default = Value averaged over entire year
  # Fat: Milk fat content (% of Weight)

#-------------------------------------------------------------------------------#
# Net Energy for Work
  # Equation 10.11
#-------------------------------------------------------------------------------#

NE.W <-function(NE.M, Hours){
  10*NE.M*Hours
}

  # Net energy required for maintenance (MJ day)
  # Number of hours of work per day (Value = 0 in Canada)

#-------------------------------------------------------------------------------#
# Net Energy for Pregnancy
  # Equation 10.13
#-------------------------------------------------------------------------------#

NE.P <-function(Cp, NE.M){
  Cp*NE.M
}

  # pregnancy coefficient: 0.1: additional energy requiremts for the 281-day gestation period averaged over entire year  
  # Net energy required for maintenance (MJ day)

#-------------------------------------------------------------------------------#
# Ratio of net energy available in diet for maintenance to digestible energy consumed (REM)
  # Equation 10.14
#-------------------------------------------------------------------------------#

REM <- function(DE){
  1.123-(4.092*(10^-3)*DE)+(1.126*(10^-5)*(DE^2))-(25.4/DE)
}

  # DE: digestibility of feed expressed as a fraction of gross energy (digestible energy/gross energy)

#-------------------------------------------------------------------------------#
# Ratio of net energy available for growth in a diet to digestible energy consumed (REG)
  # Equation 10.15
#-------------------------------------------------------------------------------#

REG <- function(DE){
  1.164-(1.164*(10^-3)*DE)+(1.308*(10^-5)*(DE^2))-(37.4/DE)
}

# DE: digestibility of feed expressed as a fraction of gross energy (digestible energy/gross energy)

#-------------------------------------------------------------------------------#
# Ratio of net energy available for growth in a diet to digestible energy consumed (REG)
# Equation 10.16
#-------------------------------------------------------------------------------#

GE = (((NE.M + NE.A + NE.L + NE.W + NE.P)/REM)+(NE.G/REG))/DE

# Note: 
  # Once the values for GE are calculated for each animal subcategory, the feed intake in units of kilograms of dry
  # matter per day (kg day-1) should also be calculated. To convert from GE in energy units to dry matter intake
  # (DMI), divide GE by the energy density of the feed. A default value of 18.45 MJ kg-1 of dry matter can be used if
  # feed-specific information is not available. The resulting daily dry matter intake should be in the order of 2 percent
  # to 3 percent of the body weight of the mature or growing animals. In high producing milk cows, intakes may
  # exceed 4 percent of body weight.


#-------------------------------------------------------------------------------#
# Tier 2 Emission Factor: GE Approach ----
# Equation 10.21
#-------------------------------------------------------------------------------#

EF.GE <- function(GE, Ym, Days = 1){
  (GE*(Ym/100)*Days)/55.65
}

# EF.GE = emission factor, kg CH4/head (over time period)
# GE = gross energy intake, CH4/head/day
# Ym = methane conversion factor, per cent of gross energy in feed converted to methane
# Days = Time Period
# 55.65 (MJ/kg CH4) is the energy content of methane

#-------------------------------------------------------------------------------#

# Feed Intake: Simplified Tier 2 Method Approach ----
  # Prediction of DMI for cattle based on body weight and estimated dietary net energy concentration (NEmf) and digestiblity values (DE)

#-------------------------------------------------------------------------------#
# Dietary net energy concentration of the feed
  # Table 18.A
#-------------------------------------------------------------------------------#
# GE approach is prefered but DMI should be used as a check to ensure estimates are biologically realistic 
  # ~2-4%, ~4% high yielding dairy cattle body weight



NE.mf <- function(REM, DE){
  REM*18.45*DE
}

# NE.mf: dietary net energy concentration of the feed
# DE%: DE*100

#-------------------------------------------------------------------------------#
# DMI estimate Calves
  # Equation 10.17
#-------------------------------------------------------------------------------#

DMI.Calf <- function(Weight.A, NE.mf)
  (Weight.A*0.75)*(((0.0582*NE.mf-0.00266*(NE.mf^2)-0.1128))/(0.239*NE.mf))

  # Weight.A: Livebody weight (kg)
  # NE.mf: estimated dietary net energy concentration of diet, Default Values (Table 18.A)
    # High grain diet >90%: 7.5-8.5
    # High quality forage: (Vegetative legumes & Grasses: 6.5-7.5
    # Moderate quality forage (mid-season legume & gresses): 5.5-6.5
    # Low quality forage (Straws, Mature grasses): 3.5-5.5

#-------------------------------------------------------------------------------#
# DMI estimate Growing Cattle
  # Equation 10.18
#-------------------------------------------------------------------------------#

DMI.Growing <- function(Weight.A, NE.mf){
  (Weight.A*0.75)*(((0.0582*NE.mf-0.00266*(NE.mf^2)-0.0869))/(0.239*NE.mf))
}

# Weight.A: Livebody weight (kg)
# NE.mf: estimated dietary net energy concentration of diet, Default Values (Table 18.A)
  # High grain diet >90%: 7.5-8.5
  # High quality forage: (Vegetative legumes & Grasses: 6.5-7.5
  # Moderate quality forage (mid-season legume & gresses): 5.5-6.5
  # Low quality forage (Straws, Mature grasses): 3.5-5.5

#-------------------------------------------------------------------------------#
# DMI estimate Growing Cattle (Feedlot - Steers) & (Feelot - Heifers)
  # Equation 10.18A
#-------------------------------------------------------------------------------#

DMI.FL.Steers <- function(Weight.A){
  3.83+0.0143*Weight.A*0.96
} 

DMI.FL.Heifers <- function(Weight.A){
  3.184+0.01536*Weight.A*0.96
}

  # Weight.A: Livebody weight (kg)

#-------------------------------------------------------------------------------#
# DMI estimate Non-Dairy Cows
  # Table 10.18
#-------------------------------------------------------------------------------#

# Low Quality Forage (DE <52)
  # Non-lactating: 1.8
  # Lactating: 2.2

# Avg Quality (DE 52-59)
  # Non-lactating: 2.2
  # Lactating: 2.5

# High Quality (DE >59)
  # Non-lactating: 2.5
  # Lactating: 2.7

#-------------------------------------------------------------------------------#
# DMI estimate Lactating Dairy Cows----
  # Table 10.18
#-------------------------------------------------------------------------------#

DMI.LDC <- function(Weight, FCM){
  0.0185*Weight.A+0.305*FCM
}

FCM <- function(Milk, Fat){
  (0.4324*Milk) + (16.216*Fat)
}

  # Weight.A: Livebody weight (kg)
  # FCM: Fat corrected milk (kg/day)


#-------------------------------------------------------------------------------#
# Tier 2 Emission Factor: DMI Approach ----
# Equation 10.21A
#-------------------------------------------------------------------------------#

EF.DMI <- function(DMI, MY, Days){
  DMI*(MY/1000)*Days
}

# EF = emission factor, kg CH4/head (over time period)
# DMI = kg DMI/day
# MY = Methane yield, kg CH4 kg DMI-1 (Table 10.12)
# Days = Time Period
# 1000 = conversion from g CH4 to kg CH4



# Manure Management ----

#-------------------------------------------------------------------------------#
# CH4 EMISSION FACTOR FROM MANURE MANAGEMENT
  # Equations 10.23
#-------------------------------------------------------------------------------#

EF.CH.MM <- function(VS, B, MCF){
  (VS)*(B*0.67*(MCF/100))
}

# EF = CH4 emission factor kg CH4/head/day
# VS = daily volatile solid excreted kg dry matter/head/day
# B = maximum methane producing capacity for manure produced, m3 CH4 kg-1 of VS excreted
  # North American Default (High productivity) 
    # Non-Dairy = 0.18
    # Dairy = 0.24
# 0.67 = conversion factor of m3 CH4 to kilograms CH4
# MCF = methane conversion factors for manure management system (percent) -> see table 10.17 


#-------------------------------------------------------------------------------#
# VS excretion rates
  # Equations 10.24
#-------------------------------------------------------------------------------#

VS<- function(GE, DE, UE, ASH)
  GE*(1-(DE/100)+(UE*GE))*((1-ASH)/18.45)

  # VS = volatile solid excretion per day on a dry-organic matter basis, kg VS day-1
  # GE = gross energy intake, MJ day-1
  # DE = digestibility of the feed in percent (e.g. 60 percent)
  # (UE  GE) = urinary energy expressed as fraction of GE. Typically 0.04*GE can be considered urinary excretion by most ruminants.
   # Use country-specific values where available.
  # ASH = the ash content of feed calculated as a fraction of the dry matter feed intake. Use country-specific values where available.
  # 318.45 = conversion factor for dietary GE per kg of dry matter (MJ kg-1).



# N2O Emissions from Manure Management ----

# N intake

N.Intake <- function(GE, CP){
  (GE/18.45)*(CP/6.25)
}
# GE = gross energy intake of the animal
# CP = percent crude protein in dry matter for stage of production (CP%/100)


N.Retention <- function (Milk, Milk.Pr, Weight.G, NE.G){
  ((Milk*(Milk.Pr/100))/6.38)+((Weight.G*(((268-(7.03*NE.G))/Weight.G)/1000))/6.25)
}

  # Milk = milk production, kg animal-1 day-1 
  # Milk.Pr = percent of protein in milk, calculated as [1.9 + 0.4 ● %Fat], where %Fat is an input, assumed to be 4% or or the values reported in Table 10A.1, Table 10A.2 and Table 10A.3
  # Weight.G = weight gain kg/day
  # NE.G = net energy for growth

N.ex <- function(N.Intake, N.Retention){
  N.Intake-N.Retention
}

#-------------------------------------------------------------------------------#
# Direct N2O emissions from Manure management --> Daily Calculation
  # Equation 10.25
#-------------------------------------------------------------------------------#

D.N2O.MM <- function(N.ex, EF.AWMS){
  (N.ex*EF.AWMS)*(44/28)
}

  # N.ex = N excretion per head kg N/day
  # EF.AWMS = emission factor for direct N2O emissions from manure management system, kg N2O-N/kg N
  # 44 / 28  = conversion of N2O-N emissions to N2O emissions

  # Note: Estimate does not include co0digestates from biogas production

#-------------------------------------------------------------------------------#
# Indirect N2O emissions from Manure management --> Daily Calculation
#-------------------------------------------------------------------------------#

# N Loss from Volatilization 
  # Equation 10.26

N.Vol.MMS <- function(N.ex, Frac.Gas.MMS){
  N.ex * Frac.Gas.MMS
}

  # N.ex = N excretion per head kg N/day
  # Frac.Gas.MMS = fraction of managed manure nitrogen that volatilises as NH3 and NOx from MMS
    # See Chapter 10, Table 10.22

N2O.Vol.MMS <- function(N.ex, Frac.Gas.MMS, EF.Vol.MMS){
  (N.Vol.MMS(N.ex, Frac.Gas.MMS)*EF.Vol.MMS)*(44/28)
}
  #EF.Vol.MMS = emission factor for N2O emissions from atmospheric deposition of nitrogen on soils and water surfaces, kg N2O-N (kg NH3-N + NOx-N volatilised)-1
    # See Chapter 11, Table 11.3
#-------------------------------------------------------------------------------#
# N Loss from Leaching
  # Equation 10.27

N.Leach.MMS <- function(N.ex, Frac.Leach.MMS){
  N.ex * Frac.Leach.MMS
}

  # N.ex = N excretion per head kg N/day
  # Frac.Leach .MMS = fraction of managed manure nitrogen that is leached from the MMS 

N2O.Leach.MMS <- function(N.ex, Frac.Leach.MMS, EF.Leach.MMS){
  (N.Leach.MMS(N.ex, Frac.Leach.MMS)*EF.Leach.MMS)*(44/28)
}

  # EF.Leach.MMS = emission factor for N2O emissions from nitrogen leaching and runoff, kg N2O-N/kg N leached and runoff,
    # See  Chapter 11, Table 11.3
#-------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------#
# Emissions from Agricultural Soils ----
# Annual Totals + Field Level Estimates
#-------------------------------------------------------------------------------#


EF.N2O <- function(P){
  exp((0.00558*P)-7.701)
}

# P = Growing Season Precipitation (mm)


EF.N2O.Base <- function(PE, P, Topo, Texture){ 
  ((EF.N2O(PE)*Topo)+(EF.N2O(P)*(1-Topo)))*Texture
}


N.Rate.Organic<-function(Total.NEX, PRP, Frac.Leach.MMS, Frac.Gas.MMS){
  Total.NEX*(1-FR.PRP)*(1-(Frac.Leach.MMS+Frac.Gas.MMS))
  
}

# Sum.N.ex = Total N Excreted during period
# FR.PRP = Fraction of manure not managed
# (Frac.Leach.MMS+Frac.Gas.MMS) = fraction of manure N loss during storage and handling within MSS i


N2O.Direct <- function(N.Rate, PE, P, Topo, Texture, RF_NS, RF_CS = 1){
  (N.Rate*(EF.N2O.Base(PE, P, Topo, Texture)*(1/0.634)*RF_NS*RF_CS))*(44/28)}

## Emissions from manure and Urine on PRP ----

N2O.PRP <- function(Total.NEX, FR.PRP, EF.PRP = 0.00043){
  Total.NEX*FR.PRP*EF.PRP
}

## Crop Residual Decomposition ----
  # Region Values Can be modified to farm in simulation
  # I would suggest running a group by function to find values for each crop type in production on farm or area

N2O.CRD <- function(F.CRD, PE, P, Topo, Texture, RF_CS = 1){
  (F_CRD*(EF.N2O.Base(PE, P, Topo, Texture)*(1/0.634)*0.84*RF_CS))*(44/28)}


  # F.CRD = total amount of crop residue N that is returned to soils, excluding N losses due to residue burning
  # RF_CS = ratio factor for cropping system, m (annual or perennial)


F.CRD<-function(P.CP, FR.Renew, R.AG, N.AG, R.BG, N.BG){
  P.CP*RF.Renew*(R.AG*N.AG*R.BG*N.BG)
}

  # P.CP = total production of the Tth crop type that is renewed annually kg DM yr-1
  # FR.Renew = fraction of total area under crop T that is renewed annually
  # R.AG = ratio of above-ground residues to harvested yield for crop T, kg dry matter (DM) kg-1
  # N.AG = N content of above-ground residues for crop T, kg-N kg-DM-1
  # R.BG = ratio of below-ground residues to harvested yield for crop T, kg DM kg-1
  # N.BG = content of below-ground residues for crop T, kg-N kg-DM-1



  # formula differs from NIR methodology as does not adjust for ecodistrict, provincial level differences

P.CP<-function(Area.CT, Yield.CT, H2O){
  Area.CT*Yield.CT*(1-H2O)
}
  # Area.CT = area under crop type T
  # Yield.CT = average crop yield for crop type T
  # Janzen et al. (2003) for crop specific parameters: "The fate of nitrogen in agroecosystems: an illustration using Canadian estimates"


## Mineralization Associated with Loss of SOM ----
  # The N in mineral soils that is mineralized in association with the loss of soil organic matter as a result of changes
  # to land management practices can result in additional N2O emissions in the Cropland Remaining Cropland category.

F.SOM <- function(C.Min, R.CN){
  (C.Min* (1/R.CN))*1000
}

  # Sum across all management practices
  #F.SOM: net annual amount of N mineralized in mineral soils in ecodistrict i as a result of loss of soil organic carbon due to changes in land management practices, kg N
  # C.Min = average annual loss of soil organic carbon for each land management practice (LM), Mg C
  # R.CN = C:N ratio of soil organic matter,  default value = 11.0 ± 1.9, Should be able to find using SLC polygon

N2O.SOM <- function(F.SOM, PE, P, Topo, Texture, RF_CS = 1){
  (F.SOM*EF.N2O.Base(PE, P, Topo, Texture)*(1/0.634)*0.84*RF_CS)*(44/28)}


## Cultivation of Organic Soils ----

N2O.Hist<-function(Area.Organic, EF.Organic = 8){
  (Area.Organic*EF.Organic)*(44/28)
}

  # Area.Organic = area of cultivated organic soils, ha
  # EF.Organic = IPCC default emission factor for mid-latitude organic soils, 8.0 kg N2O-N ha-1 year-1

## Changes in N2O emissions due to the Adoption of NT-RT ----

C.N2O.Till <-function(N2O.Org, N2O.InOrg, N2O.CRD, FR.Till, RF_Till = 0.73){
  (N2O.Org+N2O.InOrg+N2O.CRD)*(FR.Till*(RF_Till-1))
}

# N2O.Org = soil N2O emissions from manure
# N2O.InOrg = soil N2O emissions from inorganic fertilizer
# N2O.CRD = soil N2O emissions from crop residue
# FR.Till = fraction of cropland under no-till and reduced tillage
# RF_Till = ratio factor adjusting EF_Base due to the adoption of no-till and reduced till, Default value for prairie = 0.73

## Change in N2O emissions due to irrigation ----

RF.IRR = function(PET, PRCP){
  EF.N2O(PET)/EF.N2O(PRCP)
}


C.N2O.IRR <-function(N2O.Org, N2O.InOrg, N2O.CRD, FR.IRR, RF_IRR){
  (N2O.Org+N2O.InOrg+N2O.CRD)*(FR.IRR*(RF_IRR-1))
}
  
  

## Indirect Emissions from Volatilization and Re-deposition of Nitrogen ----


## Indirect Emissions from Leaching and Runoff ----


## CO2 Emissions from Liming and Urea Fertilization ----



