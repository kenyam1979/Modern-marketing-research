library(tidyverse)

binomial_choice <- read_tsv('http://yuhikaku-nibu.txt-nifty.com/blog/files/binomial-choice.txt',
                            locale=locale(encoding='shift-jis'), col_names=FALSE, skip=1)
binomial_choice <- binomial_choice %>% rename(id=X1, brand1=X2, brand2=X3, price1=X4, price2=X5)


# ブランド選択モデル
## テキストはあやまりでは?
logit <- glm(brand1 ~ price1 - price2, data=binomial_choice, family=binomial(link='logit'))
summary(logit)
probit <- glm(brand1 ~ price1 - price2, data=binomial_choice, family=binomial(link='probit'))
summary(probit)

## こうなるはず
binomial_choice <- binomial_choice %>% mutate(diff_p1p2=price1-price2) 
logit <- glm(brand1 ~ diff_p1p2, data=binomial_choice, family=binomial(link='logit'))
summary(logit)



# 多項離散選択モデル
library(mlogit)
data(Catsup)
catdata <- mlogit.data(Catsup, choice = "choice", shape = "wide", varying = c(2:13), sep=".")
mlogit <- mlogit(choice ~ disp + feat + price, data=catdata)
summary(mlogit)
