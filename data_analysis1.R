# Installing necessary Libraries and packages
install.packages("rJava")
install.packages("glmulti")
install.packages("https://cran.r-project.org/src/contrib/zoo_1.8-8.tar.gz", repo=NULL, type="source")
install.packages("https://cran.r-project.org/bin/macosx/contrib/4.0/lmtest_0.9-37.tgz", repo=NULL, type="source")
install.packages("het.test")
install.packages("sjPlot")
install.packages("stargazer")
install.packages("sjmisc")
install.packages("xtable")
install.packages("export_summs")
library(rJava)
library(leaps)
library(glmulti)
library(vars)
library(sjPlot)
library(stargazer)
library(sjmisc)
library("xtable")



#Loading Data
load("./week8_data.Rda")
names(data)
data <- data
head(data)

# Defining Full Linear Model
mod <- lm(GNIPC ~ data$`Life Expectancy` + data$`CA Balance` + data$`Debt Service` + data$`Education Expenditure` + data$`Health Expenditure` + data$`ODA Received`, data=data)

# Results of regression
summary(mod)


#Defining log of GNIPC model
modlog <- lm(log(GNIPC) ~ data$`Life Expectancy` + data$`CA Balance` + data$`Debt Service` + data$`Education Expenditure` + data$`Health Expenditure` + data$`ODA Received`, data=data)
summary(modlog)
modlog2 <- lm(log(GNIPC) ~ data$`CA Balance` + data$`Debt Service` + data$`Education Expenditure` + data$`Health Expenditure` + data$`ODA Received`, data=data)
modlog3 <- lm(log(GNIPC) ~ data$`Life Expectancy` + data$`CA Balance` + data$`Debt Service` + data$`ODA Received`, data=data)


# Testing for Heteroskedasticity
#plot(modlog)
#plot(mod)
lmtest::bptest(mod)
lmtest::bptest(modlog)

# Testing for multicollinearity
car::vif(mod)
car::vif(modlog)

# Conducting the Ramsey RESET test
resids <- mod$residuals
yhats <- mod$fitted.values
reseteq <- lm(resids ~ I(yhats^2))
summary(reseteq)
reseteq2 <- lm(resids ~ I(yhats^2) + I(yhats^3))
summary(reseteq2)

residslog <- modlog$residuals
yhatslog <- modlog$fitted.values
reseteqlog <- lm(residslog ~ I(yhatslog^2))
summary(reseteqlog)
reseteq2log <- lm(residslog ~ I(yhatslog^2) + I(yhatslog^3))
summary(reseteq2log)
mean(residslog)

## Exhaustive Search

glmulti.lm.out <- 
  glmulti(log(GNIPC) ~  data$`Life Expectancy` + data$`CA Balance` + data$`Debt Service` + data$`Education Expenditure` + data$`Health Expenditure` + data$`ODA Received`, data = data,
          level = 1,               # No interaction considered
          method = "h",            # Exhaustive approach
          crit = "aic",            # AIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "lm")      # lm function

# Show 5 best models (Use @ instead of $ for an S4 object)
glmulti.lm.out@formulas
#Summary of best model
summary(glmulti.lm.out@objects[[1]])
lmtest::bptest(glmulti.lm.out@objects[[1]])
car::vif(glmulti.lm.out@objects[[1]])


## Exhaustive Search with Interactions 
glmulti.lm.outb <- 
  glmulti(log(GNIPC) ~  data$`Life Expectancy` + data$`CA Balance` + data$`Debt Service` + data$`Education Expenditure` + data$`Health Expenditure` + data$`ODA Received`, data = data,
          level = 2,               # Pair Wise interaction considered
          method = "h",            # Exhaustive approach
          crit = "aic",            # AIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = TRUE, report = TRUE,  # No plot or interim reports
          chunk=1, chunks=20,      # Batch size of 1, 20 simultaneous batches
          fitfunction = "lm")      # lm function

# Show 5 best models (Use @ instead of $ for an S4 object)
glmulti.lm.outb@formulas
#Summary of best model
summary(glmulti.lm.outb@objects[[1]])
lmtest::bptest(glmulti.lm.outb@objects[[1]])
car::vif(glmulti.lm.outb@objects[[1]])

# Exporting regression results:
stargazer(modlog, mod, out="mods.html")
stargazer(reseteq2log, reseteq2, out="reset.html")
stargazer(car::vif(mod), car::vif(modlog), out="vir.html")

