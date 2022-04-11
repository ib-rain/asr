library(readxl)
library(car)

t3.dat <- read_excel("Student 1/Task3.xlsx", col_names = FALSE)

names(t3.dat) = c(1,2,3,4)

conf.lvl = 0.95

samples = list()
for (i in (1:2)){
  samples[[i]] = na.omit(t3.dat[[i]])
}

for (i in (1:2)){
  qqPlot(samples[[i]], distribution = "norm", envelope = conf.lvl, main = paste("Sample", i))
  print(shapiro.test(samples[[i]]))
}

t.test(x = samples[[1]], y = samples[[2]],  mu = 0, alternative = "two.sided", var.equal = TRUE, conf.level = conf.lvl)

var.test(x = samples[[1]], y = samples[[2]], ratio = 1, alternative = "two.sided", conf.level = conf.lvl)

ks.test(x = samples[[1]], y = samples[[2]], alternative = "two.sided")

wilcox.test(x = samples[[1]], y = samples[[2]], mu = 0, alternative = "two.sided", conf.level = conf.lvl)