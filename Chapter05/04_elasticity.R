library(tidyverse)

data <- read_table2("http://yuhikaku-nibu.txt-nifty.com/blog/files/yogurt.txt")

data %>% ggplot() + geom_point(aes(x=X1, y=Y1)) + scale_x_log10() + scale_y_log10()

model1 <- lm(log(Y1)~log(X1)+log(X2), data)
summary(model1)

