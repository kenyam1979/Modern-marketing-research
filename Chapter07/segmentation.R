library(tidyverse)
library(GGally)

# データ準備 ----
seg <- read_csv('http://yuhikaku-nibu.txt-nifty.com/blog/files/seg.txt', locale=locale(encoding="shift_jis"))
seg <- seg %>% rename(age=年齢, sex=性別, expr_yr=投資経験年数, trading_type=取引形態)

# seg <- seg %>% mutate(across(starts_with('q'), ~ .%/%4))

seg %>% 
  group_by(sex) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(x=sex, y=n)) + 
  geom_bar(stat='identity')

seg %>% select(age) %>% 
  ggplot(aes(age)) + 
  geom_histogram(binwidth=5)




# 因子分析 ----
### (注意) scoreを指定しないと後で値を取れない
fa <- factanal(~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27, 
               factors=4, data=seg, rotation='varimax', scores='regression')
#fa <- factanal(~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27, 
#               factors=6, data=seg, rotation='varimax', scores='regression')
fa
### Cumulative varが50.6%で十分説明力があるとしている
### あるいはfactorを5や6にしても説明力はさほどあがらないので、factorを4としている

## ファクターの読み解き
head(sort(fa$loadings[, 1], decreasing=TRUE), 5) # インターネット情報収集
head(sort(fa$loadings[, 2], decreasing=TRUE), 5) # マクロ安定志向
head(sort(fa$loadings[, 3], decreasing=TRUE), 5) # 企業業績重視
head(sort(fa$loadings[, 4], decreasing=TRUE), 5) # 知人、営業のススメ

## ファクターの
data_frame(q=formatC(1:27, width=2, flag=0), as_tibble(fa$loadings[])) %>%
  pivot_longer(cols=-q, names_to='factors') %>%
  ggplot() + 
  geom_bar(stat='identity', aes(x=q, y=value, fill=factors)) +
  coord_flip() +
  facet_grid(.~factors)




# k-meansでのセグメンテーション ----
scores <- fa$scores
km <- kmeans(scores, centers=3)

## 結果の可視化
data_frame(data.frame(fa$scores), cluster=as.factor(km$cluster)) %>%
  ggpairs(aes(color=cluster),
          lower = list(continuous = wrap("points", size = 1.5, alpha = 0.8)),
          upper = list(continuous = wrap("cor", size = 5)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.5)))

## 結果のチェック
km$size # 各サンプルが特定のクラスタに偏っていないか
km$centers #どの軸に重みがあるか

## 軸の重みの可視化
data.frame(cluster=rownames(km$centers), km$centers) %>% 
  pivot_longer(cols=-cluster, names_to='factors') %>%
  ggplot() + 
  geom_bar(stat='identity', aes(x=factors, y=value, fill=cluster)) +
  facet_grid(cluster~.)





# クラスタの属性分析 ----
df <- tibble(panel=seg$panel, cluster=factor(km$cluster), seg[ ,29:32])  # 元データにクラスタと顧客属性を追加
df$sex <- factor(df$sex)
df$trading_type <- factor(df$trading_type) 

## クラスタごとの年齢
df %>% 
  ggplot() + 
  geom_boxplot(aes(x=cluster, y=age))

## クラスタごとの経験年
df %>% 
  ggplot() + 
  geom_boxplot(aes(x=cluster, y=expr_yr))

## クラスタごとの性別割合
df %>% 
  group_by(cluster, sex) %>% 
  summarize(n=n()) %>%
  ggplot() + 
  geom_bar(aes(x=cluster, y=n, fill=sex), position='fill', stat='identity')

## クラスタごとの取引形態割合
df %>% 
  group_by(cluster, trading_type) %>% 
  summarize(n=n()) %>%
  ggplot() + 
  geom_bar(aes(x=cluster, y=n, fill=trading_type), position='fill', stat='identity')





# Appendix. クラスタ数の評価 ----
library(NbClust)
NbClust(scores, distance = "euclidean",
        min.nc = 2, max.nc = 5, 
        method = "ward.D2", index ="all")



rm(list=ls(all.names=TRUE))
