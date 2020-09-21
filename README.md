# SEIR-model
SEIR model for coivd-19
# Library
library(covid19.analytics)

# Data
ag <- covid19.data(case = 'aggregated')

tsc <- covid19.data(case = 'ts-confirmed')

tsa <- covid19.data(case ='ts-ALL')

# Summary
report.summary(Nentries = 10,graphical.output =T)

# Totals per location
tots.per.location(tsc,geo.loc = 'italy')

#Growth rate
growth.rate(tsc,geo.loc = 'italy')

glm1<-glm(formula = y.var~x.var)
summary(glm1)
residuals(glm1,type = "deviance")
        
# Totals plot
totals.plt(tsa,c('italy'))

                                             SEIR Model developmemt 




######

# Intial Values
S0 <- 60461826        # susceptibles    
E0 <- 10    # exposed
I0 <- 3     # infectious
R0 <- 0     # recovered

# Parameters
alpha1 <- 1/100000000
beta1 <- 1/14
gamma1 <- 1/7

Out1 <- matrix(0, ncol= 4, nrow= 80)

for (i in 1:80) { 
  S0n <- S0
  E0n <- E0
  I0n <- I0
  R0n <- R0
  S0 <- max (0, S0n - alpha1*E0n*S0n)
  E0 <- max (0, E0n + alpha1*E0n*S0n-gamma1*E0n)
  I0 <- max (0, I0n + gamma1*E0n-beta1*I0n)
  R0 <- max (0, R0n + beta1*I0n)
  Out1[i,1] <- S0
  Out1[i,2] <- E0
  Out1[i,3] <- I0
  Out1[i,4] <- R0
}  

plot (1:80, Out1[,1], type="l", ylim= c(0,60461826))
lines (1:80, Out1[,2], col= "orange")
lines (1:80, Out1[,3], col= "red")
lines (1:80, Out1[,4], col= "seagreen")
