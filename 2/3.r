iris <- read.csv("~/00_mag/ASR/2/Datasets/iris.txt", header=FALSE)
library("car")
names(iris) = c("Sepal_len", "Sepal_wid", "Petal_len", "Petal_wid", "Class")

iris_by_class = split(iris, iris$Class)

check_norm = function (dat, desc) {
  hist(dat, main = desc)
  qqPlot(dat, main = desc)
  print(desc)
  print(shapiro.test(dat))
}

#Checking normality
for (ir in iris_by_class) {
  cl = unique(ir$Class)
  check_norm(ir$Sepal_len, paste("Sepal length of", cl))
  check_norm(ir$Petal_len, paste("Petal length of", cl))
}

test_dat = function(dat, variable, test_func) {
  vec = c()
  for (d1 in dat)
    for (d2 in dat) {
      val1 = d1[[variable]]
      val2 = d2[[variable]]
      vec = append(vec, test_func(val1, val2)$p.value > 0.05)
    }
  return(vec)
}

#Distributions equivalence
to_check = names(iris)[-5]
dis_checks = list(1,2,3,4)
names(dis_checks) = to_check

for (var in to_check)
  dis_checks[[var]] = test_dat(dat = iris_by_class, variable = var, test_func = wilcox.test)

print(names(dis_checks))
for (c in dis_checks) {
  print(matrix(c, nrow = length(iris_by_class)))
}

#Mean and var equivalence
m_checks = list(1,2,3,4)
v_checks = list(1,2,3,4)
names(m_checks) = to_check
names(v_checks) = to_check

for (var in to_check) {
  m_checks[[var]] = test_dat(iris_by_class, var, t.test)
  v_checks[[var]] = test_dat(iris_by_class, var, var.test)
}

print(names(m_checks))
for (c in m_checks) {
  print(matrix(c, nrow = length(iris_by_class)))
}

print(names(v_checks))
for (c in v_checks) {
  print(matrix(c, nrow = length(iris_by_class)))
}

for (i in seq_along(iris_by_class))
  print(paste(i,unique(iris_by_class[[i]]$Class)))
