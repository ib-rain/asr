babyboom.dat <- read.table("~/00_mag/ASR/2/Datasets/babyboom.dat.txt", quote="\"", comment.char="")
library("car")
library(exptest)
names(babyboom.dat) = c("Time_of_birth", "Sex", "Weight", "Min_from_midnight")
cut = names(babyboom.dat)[-2]

m = subset(babyboom.dat[babyboom.dat$Sex == 2, ], select = cut)
f = subset(babyboom.dat[babyboom.dat$Sex == 1, ], select = cut)
mf = subset(babyboom.dat, select = cut)

to_check = list(mf, m, f)

#Testing weights for normality
for (l in to_check) {
  weight = l$Weight
  hist(weight)
  qqPlot(weight, ylab = "Weight")
  print(shapiro.test(weight))
}

#Mean and sd comparison
m = c()
v = c()

for (i in seq_along(to_check)) {
  for (j in seq_along(to_check)) {
    m = append(m, t.test(to_check[[i]]$Weight, to_check[[j]]$Weight, alternative = "two.sided")$p.value > 0.05)
    v = append(v, var.test(to_check[[i]]$Weight, to_check[[j]]$Weight, alternative = "two.sided")$p.value > 0.05)
    }
}
print("Mean equality:")
print(matrix(m, nrow = length(to_check)))
print("Var equality:")
print(matrix(v, nrow = length(to_check)))

#Exp birthtime
for (l in to_check) {
  first_diff = diff(l$Time_of_birth, lag = 1)
  hist(first_diff)
  print(ks.exp.test(first_diff))
}

#Births per hour Poisson
{
  h_from_midnight = mf$Min_from_midnight / 60
  
  bph = hist(h_from_midnight, breaks = 0:24)
  
  for (b in bph$counts) {
    print(poisson.test(b, r = 1.8))
  }
}
