library(tidyverse)

# データ読み込み
data <- read_table2("http://yuhikaku-nibu.txt-nifty.com/blog/files/yogurt.txt")


# データプロット
data %>% 
  ggplot() + 
  geom_point(aes(x=X1, y=Y1)) + 
  scale_x_log10() + scale_y_log10()


# 弾力性モデル
model1 <- lm(log(Y1)~log(X1)+log(X2), data)
summary(model1)

### <チェックする観点>
### Xが1%増えると、係数がb%増えると読むことができる


rm(list=ls(all.names=TRUE))
