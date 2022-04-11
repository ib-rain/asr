library(readxl)
height <- read_excel("Datasets/height.xls")
library("car")

fb = na.omit(height$HtFt)
bb = na.omit(height$HtBk)

#Testing heights for normality
for (height in list(fb,bb)) {
  hist(height)
  qqPlot(height, ylab = "Height")
  print(shapiro.test(height))
}

#Testing means and vars equity
print(t.test(fb, bb, alternative = "greater"))
print(var.test(fb, bb, alternative = "two.sided"))

#Testing sameness of distribution
plot(density(fb))
lines(density(bb))
ks.test(fb, bb)
wilcox.test(fb, bb)
