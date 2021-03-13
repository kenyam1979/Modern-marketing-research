library(tidyverse)
library(arules)
apriori <- read_csv('http://yuhikaku-nibu.txt-nifty.com/blog/files/apriori.txt', locale=locale(encoding='shift-jis'))


apriori <- as(as.matrix(apriori), 'transactions')

rules <- apriori(apriori, parameter=list(supp=0.4, conf=0.5))

inspect(sort(rules, by='lift'), n=20)
