library(tidyverse)

data <- read_table2("Chapter4/sales-priec-promo.csv")
data %>% ggplot() + geom_point(aes(x=PRICE, y=SALES))

# 相関の仮説検定
cor.test(data$PRICE, data$SALES)
         

rm(list = ls(all.names = TRUE))
