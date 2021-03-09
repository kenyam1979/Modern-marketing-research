library(tidyverse)
library(GGally)

data <- read_tsv('http://yuhikaku-nibu.txt-nifty.com/blog/files/senbei.txt', locale = locale(encoding = 'shift-jis'))
colnames(data) <- c('Name','Taste','Package','Ads','Ingredients','Promotion')
data$Name <- c('Happy Turn', 'Yukino Yado', 'Potapota Yaki', 'Kuromame Senbei', 'Magari Senbei', 'Cheese Almond', 'Teshioya', 'Bakauke', 'Tsubuyori Komoti', 'Inaka Okaki', 'Umai! Katayaki')


# 相関係数行列
cor(data[,2:6])
ggcorr(data[,2:6])


# 因子分析
model <- factanal(x=data[,2:6], factors = 2, scores = 'regression')

##　<何をチェックするか>
## Uniqueness 独自因子:1-独自因子=共通性(どれだけ説明されているか)
## Loadings   因子負荷行列　どのファクターがどの程度効いているか
## Proportion/Cumulative var  寄与率(累積寄与率)


## Loadingのプロット
data_frame(dim=rownames(model$loadings), loadings=model$loadings[,1]) %>% 
  ggplot() + geom_bar(aes(x=dim, y=loadings), stat='identity')

data_frame(dim=rownames(model$loadings), loadings=model$loadings[,2]) %>% 
  ggplot() + geom_bar(aes(x=dim, y=loadings), stat='identity')


## Perception map (知覚マップ)
data_fac <- data_frame(name=data$Name, fac1=model$scores[,1], fac2=model$scores[,2]) 
data_fac %>% ggplot(aes(x=fac1, y=fac2, label=name)) + geom_point() + geom_text(hjust=0, vjust=0)




# 因子分析で次元圧縮した
# 階層クラスタリングによるサブ市場の構造分析
plot(hclust(dist(data_fac[,2:3]), method='ward.D'))


rm(list=ls(all.names = TRUE))