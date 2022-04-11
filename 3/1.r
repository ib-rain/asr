euroweight.dat <- read.delim("~/00_mag/ASR/2/Datasets/euroweight.dat.txt", header = FALSE)
library("car")

euro = subset(euroweight.dat, select = c(-1))
names(euro) = c("Weight", "Batch")

batches = list()

for (i in unique(euro$Batch)) {
  batches[i] = subset(euro[euro$Batch == i, ])
}

test_dat = function(dat, test_func) {
  vec = c()
  for (d1 in dat)
    for (d2 in dat) {
      vec = append(vec, test_func(d1, d2)$p.value > 0.05)
    }
  return(vec)
}

batches_distr = test_dat(batches, wilcox.test)
print(matrix(batches_distr, nrow = length(batches)))

