library(tidyverse)
library(GGally)

data <- read_table2("Chapter4/sales-priec-promo.csv")
ggpairs(data)

model <- lm(SALES~PRICE+PROMO_FLG, data)

summary(model)

### <チェックする観点>
### F検定  全係数がゼロというH0の検定

