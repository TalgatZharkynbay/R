library(AER)
library(plm)
data(Fatalities)
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000
fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state - 1, data = Fatalities)
fatal_fe_lm_mod

# obtain demeaned data
Fatalities_demeaned <- with(Fatalities,
                            data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
                                       beertax = beertax - ave(beertax, state)))

# estimate the regression
summary(lm(fatal_rate ~ beertax - 1, data = Fatalities_demeaned))

# estimate a combined time and entity fixed effects regression model

# via lm()
fatal_tefe_lm_mod <- lm(fatal_rate ~ beertax + state + year - 1, data = Fatalities)
fatal_tefe_lm_mod

# via plm()
fatal_tefe_mod <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")

coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")
