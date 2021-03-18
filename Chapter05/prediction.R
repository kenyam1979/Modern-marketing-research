library(tidyverse)
library(GGally)


# データ読み込み
data <- read_delim('http://yuhikaku-nibu.txt-nifty.com/blog/files/sales-price-promo.txt', locale=locale(encoding='shift-jis'), 
                   delim=' ', col_names=FALSE, skip=1, trim_ws=TRUE)
data <- data %>% rename(SALES=X1, PRICE=X2, PROMO_FLG=X3)

# データ確認
ggpairs(data)

# 線形モデル
model <- lm(SALES~PRICE+PROMO_FLG, data)
summary(model)

### <チェックする観点>
### F検定  全係数がゼロというH0の検定



rm(list=ls(all.names=TRUE))
