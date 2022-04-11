library(MASS)
library(exptest)
library(DescriptiveStats.OBeu)
library(car)
babyboom.dat <- read.table("~/00_mag/ASR/1/babyboom.dat.txt", quote = "\"", comment.char = "")

names(babyboom.dat) = c("Time_of_birth", "Sex", "Weight", "Min_from_midnight")
cut = names(babyboom.dat)[-2]

m = subset(babyboom.dat[babyboom.dat$Sex == 2, ], select = cut)
f = subset(babyboom.dat[babyboom.dat$Sex == 1, ], select = cut)
mf = subset(babyboom.dat, select = cut)

to_check = list(m, f, mf)

#DS
for (l in to_check) {
  print(summary(l))
  #print(ds.statistics(l))
}

#Comparing weights and testing normality
for (l in to_check) {
  weight = l$Weight
  hist(weight)
  qqPlot(weight, ylab = "Weight")
  print(shapiro.test(weight))
}

#Binom
{
  x = nrow(m)
  n = nrow(babyboom.dat)
  binom.test(x, n)
}

#Exp
for (l in to_check) {
  first_diff = diff(l$Time_of_birth, lag = 1)
  hist(first_diff)
  print(ks.exp.test(first_diff))
  print(shapiro.exp.test(first_diff))
}

#Geom
for (l in to_check[c(-3)]){
  row_ids = as.numeric(rownames(l))
  row_diff = diff(row_ids, lag = 1)
  hist(row_diff)
  #print(ks.test(row_diff, "pgamma"))
}


#Poisson
  {
  h_from_midnight = mf$Min_from_midnight / 60
  
  bph = hist(h_from_midnight, breaks = 0:24)
  
  for (b in bph$counts) {
    print(poisson.test(b, r = 1.8))
  }
}
