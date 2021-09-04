# Algal-bloom-prediction-machine-learning

In Training data folder:
Observation-df -- All the training features (including lake nutrient observations)

Daily_Observation_df -- Daily training features (Inflow, meteorological data, ice information, themal structure, hydrodynamic features from process-based model)

SST-- surface water temperature (°C)
delT -- temperature difference between surface and bottom water (°C)
U -- wind speed (m/s)
AirT -- air temperature (°C)
Humidity -- (0-100 %)
CC -- cloud cover fraction (0-1)
Prec -- precipitation (mm/day)
swr -- short wave radiation (w/m2)
inflow -- river inflow (m3/s)
outflow -- river outflow (m3/s) 
Ice_d -- ice duration in the previous winter (days)
days from iceoff -- the number of days from previous ice-off date (days)
MLD -- mixing layer depth (m)
W -- Wedderburn number
thermD -- themocline depth

Scenario 1:
Direct prediction based on observation scenario -- predict the Chl concentrations on the date when all the training features are prepared

Scenario 2: 
Two-step data-driven models based on pre-generated daily nutrients, observed physical factors, and hydrodynamic features from process-based model.
