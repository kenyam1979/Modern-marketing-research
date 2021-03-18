library(tidyverse)


# 二要因間の独立性検定
data <- matrix(c(
  75,  80, 145, 
  125, 90, 85), 
  nrow=2, ncol=3, byrow=TRUE)
colnames(data) <- c('Mild', 'Regular', 'Hot')
rownames(data) <- c('East', 'West')
data
chisq <- chisq.test(data)
chisq

## f* (e.g. f11 = f1.*f.1 = 300*200/600*600)
(300*200)/(600)
chisq$expected

## Residual  f - f*
chisq$residuals





# コレスポンデンス分析
library(ca)

## cleaner
### データ準備
cleaner <- read_tsv('http://yuhikaku-nibu.txt-nifty.com/blog/files/Cleaner.txt', 
                    locale=locale(encoding='shift-jis'), col_names=FALSE, skip=1)
cleaner <- cleaner %>% rename(brand=X1, design=X2, usability=X3, power=X4, silence=X5, size=X6, maintainability=X7, handy=X8, satisfaction=X9)
cleaner_matrix <- as.matrix(cleaner[, 2:9])
row.names(cleaner_matrix) <- cleaner$brand # matrix型の行名をcaパッケージが利用するのでそうしている

### CA
ca_cleaner <- ca(cleaner_matrix)
plot(ca_cleaner)

library(rgl)
plot3d.ca(ca_cleaner)


### 階層クラスタリング
plot_cleaner <- bind_cols(dim1=ca_cleaner$rowcoord[,1]*ca_cleaner$rowdist[1], dim2=ca_cleaner$rowcoord[,2]*ca_cleaner$rowdist[2])
dist_cleaner <- dist(plot_cleaner)
plot(hclust(dist_cleaner, method='ward.D'))



## camera
### データ準備
camera <- read_tsv('http://yuhikaku-nibu.txt-nifty.com/blog/files/Camera.txt',
                   locale=locale(encoding='shift-jis'), col_names=FALSE, skip=1)
camera <- camera %>% rename(brand=X1, design=X2, quality=X3, usability=X4, battery=X5, portability=X6, functionality=X7, LCD=X8, hold=X9, satisfaction=X10)
camera_matrix <- as.matrix(camera[, 2:10])
rownames(camera_matrix) <- camera$brand

### CA
ca_camera <- ca(camera_matrix)
plot(ca_camera)

plot3d.ca(ca_camera)

### 階層クラスタリング
plot_camera <- bind_cols(dim1=ca_camera$rowcoord[,1]*ca_camera$rowdist[1], dim2=ca_camera$rowcoord[,2]*ca_camera$rowdist[2])
dist_camera <- dist(plot_camera)
plot(hclust(dist_camera, method='ward.D'))



rm(list=ls(all.names=TRUE))
