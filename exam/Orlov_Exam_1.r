library(readxl)
library(EnvStats)
library(DescriptiveStats.OBeu)

getmode = function(v) {
  uniqv = unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}


t1 = read_excel("Student 1/Task1.xls.xlsx", col_names = FALSE, col_types = c("numeric"))[[1]]
name = "Girth, mm"

#print(summary(t1))
print(ds.statistics(t1))
print(paste("Mode: ", getmode(t1)))

ecdfPlot(t1, main = "CDF", xlab = name)

for (i in list(5, 10, 15, 60)) {
  hist(t1, main = paste("Histogram for approx.", i, "intervals"), xlab = name, breaks = i)
}

boxplot(t1, ylab = name)