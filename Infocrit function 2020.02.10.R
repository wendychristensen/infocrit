## https://github.com/wendychristensen/infocrit ##
## infocrit function ##

infocrit <- function(deviance, k, m, N) {
  ModelAIC <- deviance + 2*k
  ModelAICC_m <- deviance + 2*k*(m/(m-k-1))
  ModelAICC_N <- deviance + 2*k*(N/(N-k-1))
  ModelBIC_m <- deviance+log(m)*k
  ModelBIC_N <- deviance+log(N)*k
  ModelCAIC_m <- deviance + (log(m+1)*k)
  ModelCAIC_N <- deviance + (log(N+1)*k)
  ModelHQIC_m <- deviance + 2*k*log(log(m))
  ModelHQIC_N <- deviance + 2*k*log(log(N))
  print(paste("Model AIC:",ModelAIC))
  print(paste("Model AICC(m):",ModelAICC_m))
  print(paste("Model AICC(N):",ModelAICC_N))
  print(paste("Model BIC(m):",ModelBIC_m))
  print(paste("Model BIC(N):",ModelBIC_N))
  print(paste("Model CAIC(m):",ModelCAIC_m))
  print(paste("Model CAIC(N):",ModelCAIC_N))
  print(paste("Model HQIC(m):",ModelHQIC_m))
  print(paste("Model HQIC(N):",ModelHQIC_N))
}

# Example of direct computation of information criteria; user directly inputs deviance, number of estimated parameters, number of clusters (m), and number of observations (N)

infocrit(500,5,50,150)

# Computation of information criteria using nlme output
# Three parameter model from Singer & Willett (2003) "Applied Longitudinal Data Analysis" (Chapter 4, p. 94-95)
# Model specification in nlme from https://stats.idre.ucla.edu/r/examples/alda/r-applied-longitudinal-data-analysis-ch-4/

library(nlme)

alcohol1 <- read.table("https://stats.idre.ucla.edu/stat/r/examples/alda/data/alcohol1_pp.txt", header=T, sep=",")
attach(alcohol1)

model <- lme(alcuse~ 1, alcohol1, random= ~1 |id, method="ML")
summary(model) # Displays AIC and BIC(m)

logLikelihood <- model$logLik
modeldeviance <- -2*logLikelihood

groups <- head(model$dims$ngrps,n=1)
model_m <- unname(groups, force = FALSE)

model_N <- model$dims$N

infocrit(modeldeviance, 3, model_m, model_N) # AIC and BIC(m) match what's shown in summary(model)

