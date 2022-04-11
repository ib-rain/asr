airport.dat <- read.delim("~/00_mag/ASR/1/airport.dat.txt", header = FALSE)
library(EnvStats)

names(airport.dat) = c("Airport", "City", "Sch_dep", "Perf_dep", "Passengers", "Freight", "Mail")

for (i in 3:7) {
  name = names(airport.dat)[i]
  data = airport.dat[[i]]
  print(name)
  print(summary(data))
  epdfPlot(data, main = "PDF", xlab = name)
  ecdfPlot(data, main = "CDF", xlab = name)
}
