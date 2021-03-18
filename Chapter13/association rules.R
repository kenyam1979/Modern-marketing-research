library(tidyverse)
library(arules)

# データ準備
apriori <- read_csv('http://yuhikaku-nibu.txt-nifty.com/blog/files/apriori.txt', locale=locale(encoding='shift-jis'))
apriori <- as(as.matrix(apriori), 'transactions') # aprioriが読み込むデータ形式に変換

# ルール生成実行
rules <- apriori(apriori, parameter=list(supp=0.4, conf=0.5))

# liftの高い上位ルールを抽出
inspect(sort(rules, by='lift'), n=20)
