library(readr)
accident_us <- read_csv("~/US_Accidents_New.csv")
View(accident_us)

#=========dummy variable
accident_us$weather_dummy <- ifelse(accident_us$Weather_Condition == 'Clear', 1, 0)

#==========check correlation
cor.test(accident_us$`Wind_Chill(F)`,accident_us$`Wind_Speed(mph)`) #p-value = 1.006e-13 < 0.5 hence they are correlated
cor.test(accident_us$`Wind_Chill(F)`,accident_us$`Humidity(%)`) #p-value < 2.2e-16 <0.5 hence they are correlated
cor.test(accident_us$`Wind_Speed(mph)`,accident_us$`Humidity(%)`)
# Logit model (Logistic regression)

library(lmtest)
library(sandwich)

#======= linear
test=lm(Severity_new~weather_dummy, data=accident_us)
coeftest(test,vcov. = vcovHC)

# The interpretation is that, holding constant the P/I ratio, 
# being black increases the probability of a mortgage application denial by about 17.7%
# being a clear weather increases the probabiltity of severity of accident by 0.93%
#being a non clear weather will increase the chance of Severity by 0.0093 so just 0.93 % chances od higher severity
# But it might be distorted by omitted variable bias so Severity could be a premature conclusion.


# The linear probability model has a major flaw: 
# it assumes the conditional probability function to be linear.
#This does not restrict dependent variable to lie between 0 and 1.


# So, we need a nonlinear function to model the conditional probability function of a binary dependent variable.
# logit model (logistic regression) is a commonly used model
#====== run regression
#---------------------
# Logistic regression
#---------------------
Severitylogit1 <- glm(Severity_new ~ weather_dummy, 
                     family = binomial(link = "logit"), 
                     data = accident_us)

coeftest(Severitylogit1, vcov. = vcovHC, type = "HC1")
#================================================================================
Severitylogit2 <- glm(Severity_new ~ weather_dummy+`Temperature(F)`+`Pressure(in)`, 
                     family = binomial(link = "logit"), 
                     data = accident_us)

coeftest(Severitylogit2, vcov. = vcovHC, type = "HC1")
#================================================================================
Severitylogit <- glm(Severity_new ~ weather_dummy+`Temperature(F)`+`Pressure(in)`+`Visibility(mi)`+`Humidity(%)`+`Wind_Chill(F)`, 
                 family = binomial(link = "logit"), 
                 data = accident_us)

coeftest(Severitylogit, vcov. = vcovHC, type = "HC1") #logodds we get from this

exp(coef(Severitylogit)) #odds ratio

#the odds ratio of 1 indicates no change but suppose it was 5.97 then I could say
#The odds ratio of 5.97 implies that a 1 unit increase in weather_condtion increases the odds of Severity by a factor of 5.97.
#In terms of percent change, we can say that the odds for unclear weather are (5.97-1)*100%= % higher than the odds of clear weather.

install.packages("stargazer") #Use this to install it, do this only once
library(stargazer)
stargazer(Severitylogit,Severitylogit1,Severitylogit2, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")
