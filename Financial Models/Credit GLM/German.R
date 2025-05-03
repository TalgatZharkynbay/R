library(readr)
library(aod)
library(lmtest)
library(dplyr)
GermanData <- read_csv("GermanData.csv", col_names = FALSE)

GermanData = GermanData %>%
  rename(
   money=X1,
   duration=X2,
   credit_history=X3,
   purpose=X4,
   amount=X5,
   savings=X6,
   employment=X7,
   installment_rate=X8,
   status=X9,
   debtors=X10,
   residence=X11,
   property=X12,
   age=X13,
   installment_plans=X14,
   housing=X15,
   credits=X16,
   job=X17,
   people=X18,
   telephone=X19,
   foreign=X20,
   Y=X21
  )

GermanData <- GermanData %>%  #NEW Columns by classification
  mutate(Less_than_0 = ifelse(money == "A11", 1, 0))%>%
  mutate(Less_than_200 = ifelse(money == "A12", 1, 0))%>%
  mutate(More_than_200 = ifelse(money == "A13", 1, 0))%>%
  
 mutate(nocreditstaken = ifelse(credit_history == "A30", 1, 0))%>%
 mutate(paidback = ifelse(credit_history == "A31", 1, 0))%>%
 mutate(existing = ifelse(credit_history == "A32", 1, 0))%>%
 mutate(delay = ifelse(credit_history == "A33", 1, 0))%>%
 #mutate(critical = ifelse(credit_history == "A34", 1, 0))%>%
  
 mutate(car_new = ifelse(purpose == "A40", 1, 0))%>%
 mutate(car_used = ifelse(purpose == "A41", 1, 0))%>%
 mutate(furniture = ifelse(purpose == "A42", 1, 0))%>%
 mutate(radio_television = ifelse(purpose == "A43", 1, 0))%>%
  mutate(domestic = ifelse(purpose == "A44", 1, 0))%>%
  mutate(repairs = ifelse(purpose == "A45", 1, 0))%>%
  mutate(education = ifelse(purpose == "A46", 1, 0))%>%
  #mutate(vacation = ifelse(purpose == "A47", 1, 0))%>%
  #mutate(R_R = ifelse(purpose == "A48", 1, 0))%>%
  mutate(business = ifelse(purpose == "A49", 1, 0))%>%
  #mutate(others = ifelse(purpose == "A410", 1, 0))%>%

  mutate(S_Less_than_100 = ifelse(savings == "A61", 1, 0))%>%
  mutate(S_Less_than_500 = ifelse(savings == "A62", 1, 0))%>%
  mutate(S_Less_than_1000 = ifelse(savings == "A63", 1, 0))%>%
  mutate(S_More_than_1000 = ifelse(savings == "A64", 1, 0))%>%
  #mutate(S_Unknown = ifelse(savings == "A65", 1, 0))%>%
  
  mutate(unemployed = ifelse(employment == "A71", 1, 0))%>%
  mutate(employed_1 = ifelse(employment == "A72", 1, 0))%>%
  mutate(employed_4 = ifelse(employment == "A73", 1, 0))%>%
  mutate(employed_7 = ifelse(employment == "A74", 1, 0))%>%
  #mutate(employed_plus_7 = ifelse(employment == "A75", 1, 0))%>%
  
  mutate(male_d = ifelse(status == "A91", 1, 0))%>%
  mutate(female_d_m = ifelse(status == "A92", 1, 0))%>%
  mutate(male_s = ifelse(status == "A93", 1, 0))%>%
  #mutate(male_m_w = ifelse(status == "A94", 1, 0))%>%
  #mutate(female_s = ifelse(status == "A95", 1, 0))%>%
  
  mutate(no_debt = ifelse(debtors == "A101", 1, 0))%>%
  mutate(applicant = ifelse(debtors == "A102", 1, 0))%>%
  #mutate(guarantor = ifelse(debtors == "A103", 1, 0))%>%
  
  mutate(real_estate = ifelse(property == "A121", 1, 0))%>%
  mutate(life_insurance = ifelse(property == "A122", 1, 0))%>%
  mutate(car_or_other = ifelse(property == "A123", 1, 0))%>%
 # mutate(unknown = ifelse(property == "A124", 1, 0))%>%
  
  mutate(bank = ifelse(installment_plans == "A141", 1, 0))%>%
  mutate(stores = ifelse(installment_plans == "A142", 1, 0))%>%
  #mutate(no_installment_plans = ifelse(installment_plans == "A143", 1, 0))%>%
  
  mutate(rent = ifelse(housing == "A151", 1, 0))%>%
  mutate(own = ifelse(housing == "A152", 1, 0))%>%
  #mutate(free = ifelse(housing == "A153", 1, 0))%>%
  
  mutate(unemployed_unskilled = ifelse(job == "A171", 1, 0))%>%
  mutate(unskilled_resident = ifelse(job == "A172", 1, 0))%>%
  mutate(skilled= ifelse(job == "A173", 1, 0))%>%
#  mutate(management_self_employed = ifelse(job == "A174", 1, 0))%>%
  
  mutate(has_phone = ifelse(telephone == "A192", 1, 0))%>%
  mutate(is_foreign = ifelse(foreign == "A201", 1, 0))

GermanData=subset(GermanData, select = -c(money, credit_history,
                                          purpose, savings, employment,
                                          status, debtors, property,
                                          installment_plans, housing,
                                          job, telephone, foreign))

GermanData <- GermanData %>%  #NEW Columns by classification
  mutate(Y = ifelse(Y == 2, 1, 0))


############# Regression ###########################################
Fit_Logit=glm(Y~ ., 
              data=GermanData,
              family = binomial (link="logit"))

summary(Fit_Logit)
