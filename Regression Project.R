# load the London Stock Exchange data set
load("project_data.RData")
head(lse)

# Part 1
# a)
# i)
#libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)

#looking at BA closing stock price over time - BA is the dependent variable
   lse      
# remove the data, year, month columns
dat <- lse %>% select(-Date, -Year,-Month,-Weekday)
dat
sum(is.na(dat)) # no missing values
#### FOR Loop to iterate over all of covariate stocks (independent variables)
for(i in names(dat)[2:ncol(dat)]){
  plot(BA~get(i), data=dat)
}

#visualising correlation of variables

cor(dat)
ggcorr(dat,layout.exp = 1, hjust = 0.75, size = 3,  label = TRUE, label_size = 2, label_round = 2, label_alpha = TRUE)
# after correcting for multicollineartity
# removing SMT, NMC,SSE, MIN, RB, CCH, EXPN, SDR, MIN
nomultdata <- dat %>%
  select(-SMT, -NMC,-SSE, -MIN, -RB, -CCH, -EXPN, -SDR, -MIN, -SPX, -VOD, -GVC, -RMV)
nomultdata
sum(is.na(nomultdata)) # no missing values
# new correlogram
ggcorr(nomultdata,layout.exp = 1, hjust = 0.75, size = 3,  label = TRUE, label_size = 2, label_round = 2, label_alpha = TRUE)

# ii)



# correlation coefficients just for companies vs BA  
y <- cor(nomultdata)
x <- y[1,2:16]
print(x)
#x<-data.frame(x)

# in absolute value, and sorted
x <- abs(x)
sort(x)
# STJ, CNA, EZJ, DLG, AHT are most correlated to BA (absolute value)

# b)
#obtaining insight
plot(BA~STJ, data=nomultdata)
plot(BA~CNA, data=nomultdata)
plot(BA~EZJ, data=nomultdata)
plot(BA~DLG, data=nomultdata)
plot(BA~AHT, data=nomultdata)

#fitting the linear regression model 

# i) 
#using new dataset with no multicollinearity
summary(nomultdata)# BA and 15 other companies - 1007 rows x 16 columns
model1 <- lm(BA~STJ+CNA+EZJ+DLG+AHT, data= nomultdata)
summary(model1)
#creating the anova table manually
dfR <- nrow(nomultdata) - length(coef(model1))
dfM <- length(coef(model1))-1
dfT <- nrow(nomultdata) - 1
RSS <- sum(resid(model1)^2)
TSS <- sum((nomultdata$BA - mean(nomultdata$BA))^2)
MSS <- TSS - RSS
MSR <- RSS/dfR
MSM <- MSS/dfM
F <- MSM/MSR

#carrying out F test using the ANOVA table
# first argument (p) is 1-significance level
# next two arguments are the degrees of freedom
qf(0.95, 5, 1001) #2.223043

# pf()
pf(F, 5, 1001, lower.tail=FALSE)# 2.692116e-249
# interpretation of model coefficients
summary(model1)

# the residual standard error squared of the error variance 
# 35.75^2 = 1278.062

# looking at confidence intervals
confint.lm(model1)

anova(model1) # anova table function

coefficients(model1)# Model coefficients

confint(model1)# Confidence intervals for the regression coefficients


deviance(model1)# Residual sum of squares


effects(model1) # Vector of orthogonal effects


fitted(model1)# Vector of fitted y values


residuals(model1)# Model residuals

#Key statistics, such as R2, the F statistic, and the residual standard error (??)
summary(model1)
summary(model1)$r.sq #0.6866725 is the R squared value
summary(model1)$sigma #35.753 is the RSE value
plot(model1) # non constant variance and errors are not normally distributed
#investigating BA vs CNA relationship
summary(lm(BA~CNA,data=nomultdata))

#common transformation 
model2 <- lm(log(BA)~STJ+(CNA^2)+EZJ+DLG+AHT, data= nomultdata)
plot(model2) # perhaps the variance has been delt with?

sqrt(mean(resid(model1) ^ 2))
sqrt(mean(resid(model2) ^ 2))
# taking into account scaling
sqrt(mean((nomultdata$BA - fitted(model1)) ^ 2))
sqrt(mean((nomultdata$BA - exp(fitted(model2))) ^ 2)) # model 2 has a lower RMSE

summary(model2)
summary(model2)$r.sq #0.6839986 is the R squared value
# the adjusted R square is lower, and introducing log(y) makes the model more complicated
# therefore I have decided to do nothing
summary(model2)$sigma #0.06551797

#box cox
library(MASS)
boxcox(model1, plotit= TRUE)
# do nothing = 1 or lambda = .5 ()
# .5
#specifying a range of lambda
boxcox(model1, plotit= TRUE, lambda = seq(0.5, 1.5, by = 0.1))
# ??=1 is in the confidence interval
# and is extremely close to the maximum
# suggest y-1 transformation

library(lmtest)
bptest(model1)

# backward selection
# full model - so all stocks
colnames(nomultdata)

full.model <- lm(BA~STJ+PRU+AHT+CPG+CCL+RR+DLG+TUI+LLOY+EZJ+BATS+TSCO+CNA+RTO+ANTO, data=nomultdata)
summary(full.model)

#using the step function we will eliminate under-performers
#reduced model
reduced.model <- step(full.model, direction = 'backward')
#R squared value of reduced model
summary(reduced.model)$r.sq #0.8576528
plot(reduced.model)
# forward selection
# intercept model
min.model <- lm(BA~1,data=nomultdata)
fwd.model <- step(min.model, direction = 'forward', scope = (~STJ+PRU+AHT+CPG+CCL+RR+DLG+TUI+LLOY+EZJ+BATS+TSCO+CNA+RTO+ANTO))
summary(fwd.model)
#R squared value of fwd model
summary(fwd.model)$r.sq #0.8576528
#forward and backward selection produce the same model!

#try stepwise selection
min.model <- lm(BA~1,data=nomultdata)
step.model <- step(min.model, direction = 'both', scope = (~STJ+PRU+AHT+CPG+CCL+RR+DLG+TUI+LLOY+EZJ+BATS+TSCO+CNA+RTO+ANTO))

#checking plots of the model produced my forward/backward selection
select_model <- lm(BA ~ STJ + DLG + BATS + AHT + RTO + CPG + CNA + TSCO + EZJ + LLOY + PRU + ANTO + RR, data = nomultdata)
plot(select_model)
summary(select_model)$r.sq #0.8576528
#box cox
library(MASS)
boxcox(select_model, plotit= TRUE) # 0 at point of inversion therefore try natural log
#shapiro test
shapiro.test(resid(select_model))#W = 0.99785, p-value = 0.2218

# try natural log
select_model_1 <- lm(log(BA) ~ STJ + DLG + BATS + AHT + RTO + CPG + CNA + TSCO + EZJ + LLOY + PRU + ANTO + RR, data = nomultdata)
plot(select_model_1)
summary(select_model_1)$r.sq #0.8614344
shapiro.test(resid(select_model_1)) #W = 0.9973, p-value = 0.091


####       ****** PART 2 *****     ####
#
predict_BAE <- function(lse, newdata){
  # Carry out any transformations prior to fitting you model
  # Add transformed variables to both lse and newdata. E.g.:
  #lse$BA.log <- log(lse$BA)
  #newdata$BA.log <- log(newdata$BA)
  # this is the part that fits your linear model
  BAE.lm <- lm(BA ~ STJ + DLG + BATS + AHT + RTO + CPG + CNA + TSCO + EZJ + LLOY + PRU + ANTO + RR, data = lse)
  # this is the part that produces predictions using your linear model
  predictions <- predict(BAE.lm, newdata = newdata)
  return(predictions)
}
predict_BAE

# looking to reduce the RMSE value, going to look for outliers...
describe(lse)
boxplot(lse$STJ)#no outliers
boxplot(lse$DLG) # outlier identified at top end
boxplot(lse$BATS) #outliers at bottom end
boxplot(lse$AHT) # no outliers
boxplot(lse$RTO)# lots of outlier *** should remove
boxplot(lse$CPG) #no outliers
boxplot(lse$CNA)#no outliers
boxplot(lse$TSCO)#no outliers
boxplot(lse$EZJ)#no outliers
boxplot(lse$LLOY) # lots of outliers towards bottom end *** should remove
boxplot(lse$PRU)#no outliers
boxplot(lse$ANTO)#no outliers
boxplot(lse$RR)#lots of outliers *** should remove

#investigating BA vs CNA relationship
summary(lm(BA~CNA,data=nomultdata)) # negative coefficient here
#but when included in multiple regression model it becomes positive
summary(model1)

# final prediction #30.8
predict_BAE <- function(lse, newdata){
  # Carry out any transformations prior to fitting you model
  # Add transformed variables to both lse and newdata. E.g.:
  
  # this is the part that fits your linear model
  BAE.lm <- lm(BA ~ STJ + PRU + AHT + CPG + RR + DLG + LLOY + BATS + TSCO + CNA*EZJ + ANTO, data = lse)
  # this is the part that produces predictions using your linear model
  predictions <- predict(BAE.lm, newdata = newdata)
  return(predictions)
}
