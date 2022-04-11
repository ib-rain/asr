cigarettes.dat <- read.delim("~/00_mag/ASR/3/Datasets/cigarettes.dat.txt", header=FALSE)

cigarettes = subset(cigarettes.dat, select = c(-1,-2))

names(cigarettes) = c("Nicotine", "Weight", "CO")

research_cor = function(name_x, name_y, x, y) {
  print(paste(name_x, name_y, sep = "-"))
  print(cor.test(x, y, method = "kendall"))
  print(cor.test(x, y, method = "spearman"))
}

for (var in names(cigarettes)[-1])
  research_cor("Nicotine", var, cigarettes$Nicotine, cigarettes[[var]])

