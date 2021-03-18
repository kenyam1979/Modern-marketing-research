library(tidyverse)

# データ読み込み
dat_full <- read_tsv('http://yuhikaku-nibu.txt-nifty.com/blog/files/conjoint1.txt', 
                     locale=locale(encoding='shift-jis'), 
                     col_names=c('id', 'battery_6', 'battery_4', 'wty_2', 'color_red', 'color_silver', 'rank', 'util'), 
                     skip=1)
dat_full

# 回帰分析で係数算出
model <- lm(formula=util~battery_6+battery_4+wty_2+color_red+color_silver, data=dat_full)
summary(model)


# 係数データ整形
# 効用は平均がゼロになるようにscaleでずらしている
model$coefficients

battery <- tibble(attr=rep('battery', 3), level=c('b_6yrs', 'b_4yrs', 'b_2yrs'), u=c(model$coefficients[2:3], 0))
battery$u2 <- scale(battery$u, scale=FALSE)[,1]

wty <- tibble(attr=rep('wty' ,2), level=c('w_2yrs', 'w_1yr'), u=c(model$coefficients[4], 0))
wty$u2 <- scale(wty$u, scale=FALSE)[,1]

color <- tibble(attr=rep('color', 3), level=c('c_red', 'c_silver', 'c_black'), u=c(model$coefficients[5:6], 0))
color$u2 <- scale(color$u, scale=FALSE)[,1]

util <- bind_rows(battery, wty, color)


# 表示
## 効用表示
util %>% 
  ggplot(aes(x=level, y=u2)) + 
  geom_bar(stat='identity')

## 重要度表示
### レンジの総和計算
tr <- util %>% 
  group_by(attr) %>% 
  summarize(r=max(u2)-min(u2)) %>% 
  summarize(sum(r)) %>% 
  pull()  
### プロット
util %>% 
  group_by(attr) %>% 
  summarize(importance=((max(u2)-min(u2))/tr)) %>%
  ggplot(aes(x=attr, y=importance)) +
  geom_bar(stat='identity')


rm(list=ls(all.names=TRUE))

