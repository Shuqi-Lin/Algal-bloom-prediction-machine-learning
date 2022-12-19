NP_model <- function(time, states, parms, inputs){


  PHYTO <- states[1]
  DIN <- states[2]

  maxUptake <- parms[1]
  kspar <- parms[2] #uEinst m-2 s-1
  ksdin <- parms[3] #mmol m-3
  maxGrazing <- parms[4]
  ksphyto <- parms[5]
  pFaeces <- parms[6]
  mortalityRate <- parms[7]
  excretionRate <- parms[8]
  mineralizationRate <- parms[9]
  Chl_Nratio  <- parms[10]
  Q10 <- parms[11]
  refTEMP <- parms[12]

  #USE THE PAR INPUT AND TIME-STEP INDEX TO GET THE CURRENT PAR VALUE
  PAR <- inputs[time, 2]
  TEMP <- inputs[time, 3] + parms[12]

  #FLUX EQUATIONS HERE
  Temp_effect = Q10^((TEMP-refTEMP)/10)
  N_Uptake <- maxUptake*PHYTO*(PAR/(PAR+kspar))*(DIN/(DIN+ksdin))*Temp_effect

  Mortality <- mortalityRate*PHYTO^2
  Mineralization <- mineralizationRate *
    Temp_effect

  #Convert from plankton biomass to Chlorophyll to compare to data
  Chlorophyll <- PHYTO^Chl_Nratio

  dPHYTO <- N_Uptake - Mortality
  dDIN <- Mortality - N_Uptake #+ NLOAD + Excretion

  return(list(c(dPHYTO,
                dDIN),                          # the rate of change
              c(Chlorophyll = Chlorophyll, PAR=PAR, TEMP = TEMP, N_Uptake = N_Uptake, Mortality = Mortality, Temp_effect = Temp_effect)))   # the ordinary output variables
}
