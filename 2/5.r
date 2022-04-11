library(readxl)
surgery <- read_excel("Datasets/surgery.xlsx", skip = 1)

success_n = nrow(surgery[surgery$B_Vright < surgery$A_Vright & surgery$B_Vleft < surgery$A_Vleft, ])

binom.test(x = success_n, n = nrow(surgery), p = 0.7, alternative = "less")