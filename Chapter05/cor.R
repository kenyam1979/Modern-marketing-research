library(tidyverse)

# データ読み込み
data <- read_delim('http://yuhikaku-nibu.txt-nifty.com/blog/files/sales-price-promo.txt', locale=locale(encoding='shift-jis'), 
                   delim=' ', col_names=FALSE, skip=1, trim_ws=TRUE)
data <- data %>% rename(SALES=X1, PRICE=X2, PROMO_FLG=X3)
data

# データプロット
data %>% 
  ggplot(aes(x=PRICE, y=SALES)) + 
  geom_point() +
  geom_smooth(method='lm')


# 相関の仮説検定
cor.test(data$PRICE, data$SALES)
         


rm(list = ls(all.names = TRUE))
