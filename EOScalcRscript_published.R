#Author: Achten, NB
#Last edit: May 1, 2022
#References: 
# - Puopolo e.a. 2011 - Pediatrics (doi: 10.1542/peds.2010-3464)
# - Escobar e.a. 2014 - Pediatrics (doi: 10.1542/peds.2013-1689)
# - Kuzniewicz e.a. 2017 - JAMA Pediatrics (doi: 10.1001/jamapediatrics.2016.4678); 
# - Benitz & Achten 2020 - Lancet Infectious Diseases (doi: 10.1016/S1473-3099(20)30490-4)
# - kp.org EOS calculator: https://neonatalsepsiscalculator.kaiserpermanente.org (as of March 20, 2022)

# 1 Importing data 
# Example: importing from Excel into datafram 'df' (remove # before line to use code). Other formats can be used using other packages such as 'foreign'. Numeric data should be formatted as numeric.
  # install.packages("readxl") #install readxl for importing data in Microsoft Excel format)
  # library(readxl) #loading readxl package
  # df <- read_excel(file.choose()) #import data if from Excel 

# 2 Prepare input variables. 
#Replace below (under sections 2.0 through 2.6) 'x0', 'x1', 'x2', etc. , with:
  # x0 - variable name for incidence rate (per 1000 live births)
  # x1 - variable name for Gestational Age (weeks)
  # x2 - variable name for Gestational Age (days)
  # x3 - variable name for Highest intrapartum maternal temperature (°, C or F)
  # x4 - variable name for Duration of rupture of membranes (hours)
  # x5 - variable name for GBS status ("Negative", "Positive" or "Unknown")
  # x6 - variable name for AB type and timing ("Broad4", "Broad2_4", "Specific2", "None")
  # x7 - variable name for Clinical Status ("Ill", "Equivocal", "Well")
#note: use quotation marks if variable name contains spaces, e.g. df$"Gestational Age Weeks". 

# 2.0 Incidence rate 
df$Inc <- 0.6 # default incidence rate
df$Inc <- df$x0#chosen incidence rate (default 0.6 per 1000 live births; other values require calibration/validation, see literature references above)

# 2.1 Gestational age
df$GAwks <- df$x1#variable GA(weeks)
df$GAdys <- df$x2#variable GA(days)
df$GA <- df$GAwks + (df$GAdys/7) #combining weeks and days of gestational age
df$GA <- round(df$GA, digits = 2)  #rounding used by the kp.org online EOS calculator; mathematically imprecise. http://newbornsepsiscalculator.org/ does not use this rounding.

# 2.2 Highest intrapartum maternal temperature (termed 'Highest antepartum maternal temperature' in the kp.org calculator)
df$Temp <- df$x3#variable Highest intrapartum maternal temperature (°C or °F)
df$Temp[df$Temp<50] <- (df$Temp[df$Temp<50]*9/5)+32 #converts Celsius temperature to Fahrenheit (applies if input <50°) 

# 2.3 Duration of rupture of membranes   
df$ROM <- df$x4#variable duration of rupture of membranes (hours)

# 2.4 Maternal GBS status
df$GBS <- df$x5 #variable GBS status ("Negative", "Positive" or "Unknown")
df$GBSpos <- 0; df$GBSpos[df$GBS == "Positive"] <- 1 #binary variable for GBS positive status (1=yes; 0=no)
df$GBSunk <- 0; df$GBSunk[df$GBS == "Unknown"] <- 1 #binary variable for GBS status unknown (1=yes; 0=no)

# 2.5 Type of intrapartum antibiotics
#Input:
# - Broad spectrum antibiotics ≥4 hrs prior to birth: "Broad4"
# - Broad spectrum antibiotics 2-4 hrs prior to birth: "Broad2_4" 
# - GBS specific antibiotics ≥2 hrs prior to birth: "Specific2"
# - No antibiotics or any antibiotics < 2 hrs prior to birth: "None"
df$AB <- df$x6 # variable for AB type and timing ("Broad4", "Broad2_4", "Specific2", "None")
df$ABbr <- 0; df$ABbr[df$AB == "Broad4"] <- 1 #binary variable for Broad spectrum antibiotics ≥4 hrs prior to birth (1=yes; 0=no)
df$ABsp <- 0; df$ABsp[df$AB == "Broad2_4"] <- 1; df$ABsp[df$AB == "Specific2"] <- 1 #binary variable for either Broad spectrum antibiotics 2-4 hrs or GBS specific antibiotics ≥2 hrs prior to birth (1=yes; 0=no)

# 2.6 Classification of Infant's Clinical Presentation
df$status <- df$x7 #variable of clinical status ("Ill" ('Clinical Illness'), "Equivocal", "Well" ('Well Appearing'))
df$Well <- 0; df$Well[df$status == "Well"] <- 1 #binary variable for clinical status 'Well' (1=yes; 0=no)
df$Equ <- 0; df$Equ[df$status == "Equivocal"] <- 1 #binary variable for clinical status 'Equivocal' (1=yes; 0=no)
df$Ill <- 0; df$Ill[df$status == "Ill"] <- 1 #binary variable for clinical status 'Ill' (1=yes; 0=no)

# 3 Load β parameters
# 3.1 This loads β0, the coefficient for the intercept
df$b0 <- 0
df$b0[df$Inc == 0.6] <- 40.7489 #β0 for default 0.6

df$b0[df$Inc == 0.1] <- 38.952265
df$b0[df$Inc == 0.2] <- 39.646367
df$b0[df$Inc == 0.3] <- 40.0528
df$b0[df$Inc == 0.4] <- 40.3415
df$b0[df$Inc == 0.5] <- 40.5656
df$b0[df$Inc == 0.7] <- 40.903919
df$b0[df$Inc == 0.8] <- 41.0384
df$b0[df$Inc == 0.9] <- 41.1571
df$b0[df$Inc == 1] <- 41.263432
df$b0[df$Inc == 2] <- 41.965852
df$b0[df$Inc == 4] <- 42.676976

# 3.2 This loads β coefficients for other parameters
bGA <- -6.9325 #β for Gestational age 
bGAsq <- 0.0877 #β for Gestational age squared
bTemp <- 0.8680 #β for Highest intrapartum maternal temperature
bROM <- 1.2256 #β for Highest intrapartum maternal temperature
bGBSpos <- 0.5771 #β for positive maternal GBS status
bGBSunk <- 0.0427 #β for unknown maternal GBS status
bABbr <- -1.1861 #β for either Broad spectrum antibiotics 2-4 hrs or GBS specific antibiotics ≥2 hrs prior to birth
bABsp <- -1.0488 #β for Broad spectrum antibiotics ≥4 hrs prior to birth

# 4 Calculation of EOS risk estimates
# 4.1 Pre-examination risk ('PreRisk') calculation, including necessary transformations
df$logit <- df$b0+df$GA*bGA+df$GA^2*bGAsq+(df$Temp)*bTemp+((df$ROM+0.05)^0.2)*bROM+df$GBSpos*bGBSpos+df$GBSunk*bGBSunk+df$ABbr*bABbr+df$ABsp*bABsp #calculate βx
df$PreProb <- 1/(1+exp(1)^-df$logit) #calculate probability
df$PreRisk <- df$PreProb*1000 #risk per 1000 live births
df$PreRisk <- round(df$PreRisk, digits = 2) #round risk to two decimals

# 4.2 Post-examination risk ('PostRisk') calculation using likelihood ratio's (method used by kp.org EOS calculator)
df$PostRisk <- (df$PreProb/(1-df$PreProb))*(df$Ill*21.2+df$Equ*5.0+df$Well*0.41) #convert to likelihood; apply applicable ratio
df$PostRisk <- df$PostRisk/(1+df$PostRisk)*1000 #convert to probability; calculate risk per 1000 live births
df$PostRisk <- round(df$PostRisk, digits = 2) #round risk to two decimals

# 5 Determination of recommendation by the EOS calculator (as by the kp.org EOS calculator)
df$Recom <- "0"
df$Recom[df$PostRisk >=3] <- "Empiric antibiotics, vitals per NICU"  #Obtain blood culture and start antibiotics if any status but PostRisk >3
df$Recom[df$Ill==1 & df$PostRisk <3] <- "Strongly consider starting empiric antibiotics, vitals per NICU"   #Strongly consider antibiotics if Ill but PostRisk <3
df$Recom[df$Ill==0 & df$PostRisk <3 & df$PostRisk >=1] <- "Blood culture, vitals every 4 hours for 24 hours" #Obtain blood culture and monitor vitals each 4 hours if not Ill and PostRisk<3 and PostRisk ≥1
df$Recom[df$Ill==0 & df$PostRisk <1 & df$PreRisk >=1] <- "No culture, no antibiotics, vitals every 4 hours for 24 hours" #Vitals each 4 hours if not Ill and PostRisk <1 but PreRisk ≥1
df$Recom[df$Ill==0 & df$PostRisk <1 & df$PreRisk <1] <- "No culture, no antibiotics, routine vitals" #Obtain routine vitals if not Ill and both PostRisk and PreRisk <1

# 6 Conclusion
#Pre-examination risk ('PreRisk'), Post-examination risk ('PostRisk') and EOS calculator recommendation ('Recom') are now separate columns in the dataframe.

# 7 Clean and Export
# 7.1 # Dropping of redundant variables can be done using:
#df <- subset(df, select=-c(GBSpos,GBSunk,ABbr,ABsp,Well,Ill,Equ,PreProb,logit))
# 7.2 Export of the dataframe can be done using packages, for example 'writexl' for Excel, or 'haven' for SPSS. 


