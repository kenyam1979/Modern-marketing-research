library(tidyverse)


# データ準備
df <- read_csv('http://yuhikaku-nibu.txt-nifty.com/blog/files/RFM.txt')
df <- df %>% mutate(
  rank_R=if_else(R<=30, 'r5', if_else(R<=60, 'r4', if_else(R<=90, 'r3', if_else(R<=180, 'r2', 'r1')))),
  rank_F=if_else(F>=30, 'f5', if_else(F>=10, 'f4', if_else(F>=3, 'f3', if_else(F>=2, 'f2', 'f1')))),
  rank_M=if_else(M>=500000, 'm5', if_else(M>=300000, 'm4', if_else(M>=100000, 'm3', if_else(M>=50000, 'm2', 'm1')))))
df <- df %>% mutate(DM_flg=if_else(DM=='yes', 1, 0))


# データ確認
df %>% ggplot() + geom_boxplot(aes(x=rank_R, y=M))
df %>% ggplot() + geom_boxplot(aes(x=rank_R, y=F))


# 分散分析
## 一元配置分散分析
summary(aov(M~rank_R, df)) # 分散分析表が表示される

## 多重比較
TukeyHSD(aov(M~rank_R, df))
pairwise.t.test(df$M, df$rank_R)


# DMへの反応する変数をロジスティクス回帰で分析
model <- glm(DM_flg~log(M)+log(F)+log(R), data=df, family=binomial(link='logit'))
summary(model)

## RFMにlogを取るのはこの分布が偏っているから
## この場合Mはlog取らなくてもよいのでは?
df %>% ggplot() + geom_histogram(aes(x=R))
df %>% ggplot() + geom_histogram(aes(x=R)) + scale_x_log10()

df %>% ggplot() + geom_histogram(aes(x=F))
df %>% ggplot() + geom_histogram(aes(x=F)) + scale_x_log10()

df %>% ggplot() + geom_histogram(aes(x=M))
df %>% ggplot() + geom_histogram(aes(x=M)) + scale_x_log10()


rm(list=ls(all.names=TRUE))
