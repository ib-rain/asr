euroweight.dat <- read.delim("~/00_mag/ASR/1/euroweight.dat.txt", header = FALSE)
library("car")

euro = subset(euroweight.dat, select = c(-1))
names(euro) = c("Weight", "Batch")

research_weight = function(weight, name) {
  print(name)
  print(summary(weight))
  d = density(weight)
  plot(d, main = name, xlab = "Weight")
  qqPlot(weight, ylab = "Weight", main = name)
  print(shapiro.test(weight))
}

for (i in unique(euro$Batch)) {
  batch_string = paste("Batch #", i, sep = "")
  sub = subset(euro[euro$Batch == i, ])
  research_weight(sub$Weight, batch_string)
}

research_weight(euro$Weight, "Whole")
