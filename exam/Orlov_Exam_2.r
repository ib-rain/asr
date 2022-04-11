library(readxl)
library(MVN)
library(car)
library(nortest)

t2.dat <- read_excel("Student 1/Task2.xls.xlsx", col_names = FALSE)
t2 = t2.dat[[1]]

conf.lvl = 0.9

qqPlot(t2, distribution = "norm", main = "Q-Q plot", envelope = conf.lvl)

for (adj in list(TRUE, FALSE)){
  print(pearson.test(t2, adjust = adj))
}

for (t in list("Lillie", "CVM", "AD")){
  print(mvn(t2.dat, univariateTest = t, desc = TRUE))
}
