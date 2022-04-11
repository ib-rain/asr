iris <- read.csv("~/00_mag/ASR/2/Datasets/iris.txt", header=FALSE)
library("car")
names(iris) = c("Sepal_len", "Sepal_wid", "Petal_len", "Petal_wid", "Class")

iris_by_class = split(iris, iris$Class)

research_cor = function(name_x, name_y, data) {
  print(paste(name_x, name_y, sep = "-"))
  print(cor.test(data[[name_x]], data[[name_y]], method = "kendall"))
  print(cor.test(data[[name_x]], data[[name_y]], method = "spearman"))
}

to_check = names(iris_by_class[[1]][-5])

for (ir in iris_by_class) {
  print(unique(ir$Class))
  research_cor(to_check[1], to_check[2], ir)
  research_cor(to_check[3], to_check[4], ir)
}

