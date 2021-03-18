library(tidyverse)

# データ準備
df <- read_csv('http://yuhikaku-nibu.txt-nifty.com/blog/files/Bass.txt', locale=locale(encoding='shift-jis'))
df <- df %>% rename(t=時点, n_buy=購買者数, cum_n_buy=累積購買者数, cum_n_buy_prev=一期前累積購買者数, sq_cum_n_buy_prev=一期前累積購買者数の二乗)


# lmの最小二乗法を使って係数を推定
model <- lm(n_buy~cum_n_buy_prev+sq_cum_n_buy_prev, df)
summary(model)


# 係数の計算
a=model$coefficients[[1]]
b=model$coefficients[[2]]
c=model$coefficients[[3]]

m = (-b-sqrt(b^2-4*a*c))/(2*c)
p = a / m 
q = p + b

m
p
q


# モデルによる予測
tibble(t=1:15, n_buy_p=predict(model)) %>%
  ggplot() + 
  geom_line(aes(x=t, y=n_buy_p))



rm(list=ls(all.names=TRUE))
