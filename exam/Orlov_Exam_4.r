library(readxl)
library(car)
library(lmtest)
library(MASS)
library(aod)
library(InformationValue)
library(quantreg)
library(ridge)

t41 <- read_excel("Student 1/Task4-1.xlsx")

research_reg = function(data, formula, reg_name){
  print(reg_name)
  res = 0
  if (reg_name == 'linear'){
    res = lm(formula = formula, data = data)

    print(bptest(formula = res))
    if (as.character(formula[[3]])[[1]] == "+"){
      print(vif(mod = res))
    }
    print(dwtest(formula = res))
  }
  else if (reg_name == 'ridge') {
    # res = lm.ridge(formula = formula, lambda = seq(0.1, 20, by=0.1), data = data)
    res = linearRidge(formula = formula, lambda = c(0.1, 0.2, 4.9, 5.0, 5.1, 9.9, 10.0, 10.1, 14.9, 15.0, 15.1, 19.9, 20.0), data = data)
    print(res)
  }
  else if (reg_name == 'quantile') {
    res = rq(formula = formula, tau = 1:9/10, data = data)
  }
  
  print(summary(res, se="ker"))
  plot(res)
}

regs = list('quantile', 'ridge')

fs = list(Y ~ X1 + X2, Y ~ X1, Y ~ X2)

for (f in fs){
  print(f)
  research_reg(data = t41, formula = f, reg_name = "linear")
}

for (r in regs){
  research_reg(data = t41, formula = fs[[1]], reg_name = r)
}

t42 <- read_excel("Student 1/Task4-2.xlsx")


research_bin = function(formula, data, actual_var, family_name){
  print(paste(family_name, "for", actual_var))
  mybin = glm(formula, data, family = binomial(family_name))
  print(summary(mybin))

  print(wald.test(b = coef(mybin), Sigma = vcov(mybin), Terms = 1:ncol(data)))

  predicted = plogis(predict(mybin, data))
  actual = data[[actual_var]]
  optCutOff = optimalCutoff(actual, predicted)[1]
  print(optCutOff)

  print(confusionMatrix(actual, predicted))
  print(confusionMatrix(actual, predicted, threshold = optCutOff))

  plotROC(actual, predicted)
}

f = Y ~ X

family_names = list('logit', 'probit')

for (f_n in family_names){
  research_bin(f, t42, "Y", f_n)
}
