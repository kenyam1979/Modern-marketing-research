library(tidyverse)


# 無作為抽出
data <- data_frame(ind=c(1:10))
data

## サンプル数指定
data %>% sample_n(5)

## 5%をサンプリング
data %>% sample_frac(0.5)

## 復元抽出 (sampling with replacement) (デフォルトは replace=FALSE)
data %>% sample_n(15, replace=TRUE)




# 層別サンプリング
data2 <- data_frame(ind=c(1:20), cat=c(rep('A',10), rep('B', 10)))
data2
## group_byで層を指定
data2 %>% group_by(cat) %>% sample_frac(0.5)




rm(list=ls(all.names=TRUE))
