euroweight.dat <- read.delim("~/00_mag/ASR/2/Datasets/euroweight.dat.txt", header = FALSE)
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

batches = list()

for (i in unique(euro$Batch)) {
  batches[i] = subset(euro[euro$Batch == i, ])
}

#Testing for normality
for (i in seq_along(batches)) {
  batch_string = paste("Batch #", i, sep = "")
  research_weight(batches[[i]], batch_string)
}

research_weight(euro$Weight, "Whole")

#Testing mean equivalence
eq = c()
for (j in seq_along(batches)) {
  for (k in seq_along(batches)) {
    eq = append(eq, t.test(batches[[j]], batches[[k]], alternative = "two.sided")$p.value > 0.05)
  }
}

M = matrix(eq, nrow = length(batches))
print(M)