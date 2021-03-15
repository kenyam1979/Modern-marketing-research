library(tidyverse)
library(sem)
library(DiagrammeR)

data <- read_delim('http://yuhikaku-nibu.txt-nifty.com/blog/files/CSIdata.txt', delim=';')

cor <- cor(data)
model <- specifyModel(text='
  quality -> y1, NA, 1
  quality -> y2, b12, NA
  quality -> y3, b13, NA
  quality -> y4, b14, NA
  expectation -> y5, NA,  1
  expectation -> y6, b22, NA
  expectation -> y7, b23, NA
  satisfaction -> y8, NA, 1
  satisfaction -> y9, b32, NA
  satisfaction -> y10, b33, NA
  y1 <-> y1, e01, NA
  y2 <-> y2, e02, NA
  y3 <-> y3, e03, NA
  y4 <-> y4, e04, NA
  y5 <-> y5, e05, NA
  y6 <-> y6, e06, NA
  y7 <-> y7, e07, NA
  y8 <-> y8, e08, NA
  y9 <-> y9, e09, NA
  y10 <-> y10, e10, NA
  quality -> satisfaction,  b1,NA
  expectation -> satisfaction,  b2,NA
  expectation -> quality,  b4,NA
  quality <-> quality , NA, 1   
  expectation <-> expectation , NA, 1
  satisfaction <-> satisfaction , NA, 1')


result <- sem(model, cor, N=100)

## 結果分析
summary(result)
pathDiagram(result, edge.labels='values')


## CSI計算
fscores <- fscores(result, data)
csi <- (fscores[,1]-min(fscores[,1]))/(max(fscores[,1])-min(fscores[,1]))*100
hist(csi)



