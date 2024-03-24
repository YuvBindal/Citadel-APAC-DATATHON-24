#Panel VAR in R 
#Eloriaga, Justin Raymond S.

#Installing and Loading the Package
install.packages("panelvar")
library(panelvar)
citation("panelvar")

#Importing the dataset 
df1 <- read.csv('C:/Users/sulta/Desktop/Citadel Datathon/C-DATATHON/child_ethnicity_obesity_data_cleaned.csv')
df1

#Running a Panel VAR

varone <-pvargmm(  
  dependent_vars = c("Physical_Percentage", "Obesity_Percentage","Soda_Percentage"),
  lags = 2, 
  exog_vars = c('Production_diff'),
  transformation = "fod",  data = df1,
  panel_identifier = c('PanelID', 'YearEnd'),  steps = c("twostep"),
  system_instruments = TRUE,  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,  collapse = FALSE
)
summary(varone)

#Some Diagnostics
Andrews_Lu_MMSC(varone)
stab_varone <- stability(varone)
print(stab_varone)
plot(stab_varone)

#Generating IRFs

varone_oirf <- oirf(varone, n.ahead = 4)
#varone_girf <- girf(varone, n.ahead = 4, ma_approx_steps = 4)

varone_bs <- bootstrap_irf(varone, typeof_irf = c("OIRF"), n.ahead = 4, nof_Nstar_draws = 10, confidence.band = 0.95, mc.cores = 1)

plot(varone_oirf, varone_bs)

