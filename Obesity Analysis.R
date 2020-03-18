#Clearing working space
rm(list = ls())

#Library
library(stargazer)
library(sandwich)
library(car)
library(AER)
library(zoo)
library(ggplot2)

#Code to calculate corrected standard errors
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

HROSE = read.csv("Combined_HROSE.csv", header=TRUE, sep=",")

#Creating a Far West States only dataset
Far_West_States_Data <- subset(HROSE, fips_state_code == 2 | fips_state_code == 6 | fips_state_code == 15 | fips_state_code == 32 |  fips_state_code == 41 | fips_state_code == 53 )

#Fips State Code 2 corresponds to AL (Alaska)
# Fips State Code 6 corresponds to CA (California)
# Fips State Code 15 corresponds to HI (Hawaii)
# Fips State Code 32 corresponds to NV (Nevada)
# Fips State Code 41 corresponds to OR (Oregon)
# Fips State Code 53 corresponds to WA (Washington)

#All States Data
ggplot(data=HROSE, aes(Gini, p_adult_obesity)) + geom_point(shape=0)+ labs(title = "Gini vs. p_adult_obesity for All States", x = "Gini", y = "p_adult_obesity") + stat_smooth(method = "lm", col = "green", se=FALSE)

#Far West States Data
ggplot(data=Far_West_States_Data, aes(Gini, p_adult_obesity)) + geom_point(shape=0)+ labs(title = "Gini vs. p_adult_obesity for the Far West", x = "Gini", y = "p_adult_obesity") + stat_smooth(method = "lm", col = "green", se=FALSE)

#Far West States Datatable
stargazer(Far_West_States_Data, type="text", median=TRUE, digits=2, star.cutoffs=NA, title="Far West Table Statistics")

#All States Datatable
stargazer(HROSE, type="text", median=TRUE, digits=2, star.cutoffs=NA, title="All States Table Statistics")

# Variable Definitions: 
# P_adult_obesity = proportion of adults that are obese
# prim_care_phys_rate = Physicians per 100,000
# P_phys_inactive = proportion physically inactive
# High_school_grad_rate = proportion of population that graduated high school
# Median_hh_income = median household income
# Gini = GINI coefficient for parent family income within county using parents in core sample

#Regressions all states

Reg7 = lm(p_adult_obesity ~ Gini + log(median_hh_income) +  prim_care_phys_rate + p_rural + high_school_grad_rate + I(high_school_grad_rate^2) + p_phys_inactive, data= HROSE)

Reg5 = lm(p_adult_obesity ~ Gini + log(median_hh_income) +  prim_care_phys_rate + p_rural + high_school_grad_rate + I(high_school_grad_rate^2), data=HROSE)

Reg4 = lm(p_adult_obesity ~ Gini + log(median_hh_income) +  prim_care_phys_rate + p_rural + high_school_grad_rate, data=HROSE)

Reg3 = lm(p_adult_obesity ~ Gini + log(median_hh_income) +  prim_care_phys_rate + p_rural, data=HROSE) 

Reg2 = lm(p_adult_obesity ~ Gini + log(median_hh_income) +  prim_care_phys_rate, data=HROSE) 

Reg1 = lm(p_adult_obesity ~ Gini + log(median_hh_income) , data= HROSE)

Reg0 = lm(p_adult_obesity ~ Gini, data= HROSE)

#Multiple Regression Table with All States Data
stargazer(Reg0, Reg1, Reg2, Reg3, Reg4, Reg5, Reg7, 
          se=list(cse(Reg0),cse(Reg1),cse(Reg2),cse(Reg3),cse(Reg4), cse(Reg5), cse(Reg7)), 
          title="Regression Results of All States", type="text",
          df=FALSE, digits=3)

#Calculating Regressions

Reg14 = lm(p_adult_obesity ~ Gini + log(median_hh_income) +  prim_care_phys_rate + p_rural+ high_school_grad_rate + I(high_school_grad_rate^2) + p_phys_inactive, data= Far_West_States_Data)

Reg13 = lm(p_adult_obesity ~ Gini + log(median_hh_income) +  prim_care_phys_rate + p_rural + high_school_grad_rate + I(high_school_grad_rate^2), data=Far_West_States_Data)

Reg12 = lm(p_adult_obesity ~ Gini + log(median_hh_income) +  prim_care_phys_rate + p_rural + high_school_grad_rate, data=Far_West_States_Data)

Reg11 = lm(p_adult_obesity ~ Gini + log(median_hh_income) +  prim_care_phys_rate + p_rural, data=Far_West_States_Data) 

Reg10 = lm(p_adult_obesity ~ Gini + log(median_hh_income) +  prim_care_phys_rate, data=Far_West_States_Data) 

Reg9 = lm(p_adult_obesity ~ Gini + log(median_hh_income) , data= Far_West_States_Data)

Reg8 = lm(p_adult_obesity ~ Gini, data= Far_West_States_Data)

#Multiple Regression Table with Far West Data
stargazer(Reg8, Reg9, Reg10, Reg11, Reg12, Reg13, Reg14, 
          se=list(cse(Reg8),cse(Reg9),cse(Reg10),cse(Reg11), cse(Reg12), cse(Reg13), cse(Reg14)), 
          title="Regression Results of Far West", type="text",
          df=FALSE, digits=3)

#Cutting data frames to be the same length
ScrubbedData <- Far_West_States_Data[!(is.na(Far_West_States_Data$Gini)),]
ScrubbedData2 <- ScrubbedData[!(is.na(ScrubbedData$prim_care_phys_rate)),]
ScrubbedData3 <- ScrubbedData2[!(is.na(ScrubbedData2$high_school_grad_rate)),]

#Running Complete Regression 6
Reg100 = lm(p_adult_obesity ~ Gini + p_rural+log(median_hh_income) +  prim_care_phys_rate + high_school_grad_rate + I(high_school_grad_rate^2), data = ScrubbedData3)

#Saving New Variable
ScrubbedData3$resid = resid(Reg100)

#Plotting Variable
ggplot(data=ScrubbedData3, aes(x=Gini, y=resid)) + geom_point(shape=0)+ labs(title="Residual Plot of Gini") + labs(x="Gini Coefficient", y="Regression residual")
