library(tidyverse)
library(semTools)
library(semPlot)


data <- read_delim('http://yuhikaku-nibu.txt-nifty.com/blog/files/CSIdata.txt', delim=';')

model <- '
  quality =~ y1 + y2 + y3 +y4
  expectation =~ y5 + y6 + y7
  satisfaction =~ y8 + y9 + y10
  satisfaction ~ quality + expectation
  quality ~ expectation'

## SEM実行
fit <- sem(model, data=data)


## 結果分析
summary(fit, standardized=T, fit.measures=T)
parametertable(fit)
fitMeasures(fit, fit.measures="all")
semPaths(object=fit, whatLabels="par")


## 参考 
## https://qiita.com/Masahiro_T/items/d03b9a5a86eee75eb22e
## https://ides.hatenablog.com/archive/category/lavaan