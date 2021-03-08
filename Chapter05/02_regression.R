library(tidyverse)
library(broom)

data <- read_table2("Chapter4/sales-priec-promo.csv")
data %>% ggplot() + geom_point(aes(x=PRICE, y=SALES))


# 回帰分析
model <- lm(SALES~PRICE, data)

##サマリ
summary(model)

### <チェックする観点>
### 係数  関係性のどあい
### P値   係数がゼロのH0が棄却されるか
### 決定係数(R-sq)　どの程度説明されているか


# 係数の区間推定
confint(model, level = 0.95)


# 直線の表示
data %>% ggplot() + geom_point(aes(x=PRICE, y=SALES)) + 
  geom_line(data=augment(model), aes(x=PRICE, y=.fitted))

data %>% ggplot(aes(x=PRICE, y=SALES)) + geom_point() + 
  stat_smooth(method='lm', col='red')
