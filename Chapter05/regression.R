library(tidyverse)
library(broom)

# データ読み込み
data <- read_delim('http://yuhikaku-nibu.txt-nifty.com/blog/files/sales-price-promo.txt', locale=locale(encoding='shift-jis'), 
                   delim=' ', col_names=FALSE, skip=1, trim_ws=TRUE)
data <- data %>% rename(SALES=X1, PRICE=X2, PROMO_FLG=X3)
data %>% 
  ggplot(aes(x=PRICE, y=SALES)) + 
  geom_point()


# 回帰分析
model <- lm(SALES~PRICE, data)
summary(model)

### <チェックする観点>
### 係数  関係性のどあい
### P値   係数がゼロのH0が棄却されるか
### 決定係数(R-sq)　どの程度説明されているか


# 係数の区間推定
confint(model, level = 0.95)



# 直線の表示
data %>% 
  ggplot() + 
  geom_point(aes(x=PRICE, y=SALES)) + 
  geom_line(data=augment(model), aes(x=PRICE, y=.fitted))

data %>% 
  ggplot(aes(x=PRICE, y=SALES)) + 
  geom_point() + 
  stat_smooth(method='lm', col='red')



rm(list = ls(all.names = TRUE))
