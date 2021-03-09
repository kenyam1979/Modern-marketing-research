# データのパタンの作成
## 全パタン
all_pattern <- expand.grid(
  battery=c('b_6yrs', 'b_4yrs', 'b_2yrs'),
  wty=c('w_2yrs', 'w_1yr'),
  color=c('c_red', 'c_silver', 'c_black'))

## 直交表
library(conjoint)
orth_pattern <- caFactorialDesign(data=all_pattern, type='orthogonal')

## ダミー変数化
library(tidymodels)
all_pattern %>% recipe(~.) %>% step_dummy(battery, wty, color, one_hot=TRUE) %>% prep() %>% bake(new_data=all_pattern)


rm(list=ls(all.names=TRUE))
