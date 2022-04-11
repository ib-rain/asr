library(readxl)
library(InformationValue)
library(aod)

doctor <- read_excel("Datasets/Doctor.xlsx")

exam <- read_excel("Datasets/binary regression.xls", col_names = FALSE)

names(exam) = c("Sleep", "Study", "Res")

research_bin = function(formula, data, actual_var, family_name){
  print(paste(family_name, "for", actual_var))
  mybin = glm(formula, data, family = binomial(family_name))
  print(summary(mybin))
  
  print(wald.test(b = coef(mybin), Sigma = vcov(mybin), Terms = 1:ncol(data)))
  
  predicted = plogis(predict(mybin, data))
  actual = data[[actual_var]]
  optCutOff = optimalCutoff(actual, predicted)[1]
  print(optCutOff)
  
  print(confusionMatrix(actual, predicted))
  print(confusionMatrix(actual, predicted, threshold = optCutOff))
  
  plotROC(actual, predicted)
}

datas = list(doctor, exam)

formulas = c(y ~ x1 + x2 + x3 + x4 + x5,
             Res ~ Sleep + Study)

actual_vars = c('y', 'Res')

family_names = list('logit', 'probit')

for (i in seq(1,1))
  for (f in family_names)
    research_bin(formulas[[i]], datas[[i]], actual_vars[[i]], f)
