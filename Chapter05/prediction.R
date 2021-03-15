library(tidyverse)
library(GGally)


# データ読み込み
data <- read_delim('http://yuhikaku-nibu.txt-nifty.com/blog/files/sales-price-promo.txt', locale=locale(encoding='shift-jis'), 
                   delim=' ', col_names=FALSE, skip=1, trim_ws=TRUE)

# データ確認
ggpairs(data)

model <- lm(SALES~PRICE+PROMO_FLG, data)

summary(model)

### <チェックする観点>
### F検定  全係数がゼロというH0の検定

