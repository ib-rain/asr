library(readxl)
library(car)
library(lmtest)
library(MASS)
mlr02 <- read_excel("Datasets/mlr02.xls")

kuiper <- read_excel("Datasets/kuiper.xls")

cigarettes <- read.delim("Datasets/cigarettes.dat.txt", header=FALSE)
names(cigarettes) = c("Name", "Tar", "Nicotine", "Weight", "CO")

research_lin = function(formula) {
  res = lm(formula = formula)
  plot(res)
  print(summary(res))
  print(bptest(formula = res))
  #print(dwtest(formula = res))
}

research_aic = function(formula, upper) {
  res = lm(formula = formula) 
  step = stepAIC(res, scope = upper, direction = "both", trace = FALSE)
  print(summary(res))
  print(summary(step))
  print(bptest(formula = step))
  print(dwtest(formula = step))
}

#research_lin(kuiper$Price ~ kuiper$Mileage + kuiper$Liter + kuiper$Cruise + kuiper$Sound + kuiper$Leather)
#research_aic(formula = kuiper$Price ~ kuiper$Mileage + kuiper$Liter + kuiper$Cruise + kuiper$Sound + kuiper$Leather,
#             upper = ~ kuiper$Mileage + kuiper$Liter + kuiper$Cruise + kuiper$Sound + kuiper$Leather)

#research_lin(mlr02$X1 ~ mlr02$X2 + mlr02$X3)
research_lin(cigarettes$CO ~ cigarettes$Tar + cigarettes$Nicotine + cigarettes$Weight)

