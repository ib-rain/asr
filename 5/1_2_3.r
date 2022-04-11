library(quantreg)
library(readxl)
library(lmtest)
library(MASS)

blood <- read_excel("Datasets/blood.xlsx")

kuiper <- read_excel("Datasets/kuiper.xls")

cigarettes <- read.delim("Datasets/cigarettes.dat.txt", header=FALSE)
names(cigarettes) = c("Name", "Tar", "Nicotine", "Weight", "CO")

research_reg = function(data, formula, reg_name){
  print(reg_name)
  res = 0
  if (reg_name == 'linear'){
    res = lm(formula = formula, data = data)
  }
  else if (reg_name == 'ridge') {
    res = lm.ridge(formula = formula, lambda = seq(0.1, 20, by=0.1), data = data)
  }
  else if (reg_name == 'quantile') {
    res = rq(formula = formula, tau = 1:99/100, data = data)
  }
  
  print(res)
  plot(res)
}


formulas = c(X1 ~ X2 + X3,
             Price ~ Mileage + Liter + Cruise + Sound + Leather,
             CO ~ Tar + Nicotine + Weight)
datas = list(blood, kuiper, cigarettes)
#for (f in formulas)
#  pairs(f, main = 'Simple Scatterplot Matrix')

regs = c('linear', 'ridge', 'quantile')


#for (i in seq(1,3))
#  for (r in regs)
#    research_reg(data = datas[[i]], formula = formulas[[i]], reg = r)

i = 2
for (r in regs)
  research_reg(data = datas[[i]], formula = formulas[[i]], reg = r)