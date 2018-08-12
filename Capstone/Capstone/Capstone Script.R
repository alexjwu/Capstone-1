library(readxl)
winemag_data_130k_v2 <- read_excel("winemag-data-130k-v2.xlsx")
View(winemag_data_130k_v2)

library(dplyr)
library(tidyr)
library(ggplot2)


DF <- winemag_data_130k_v2
DF %>% summary(price)
DF <- DF %>% mutate(`price` = ifelse(is.na(price), "25", price))
DF3 <- DF[, c(1,2,5,6,7,13,14)]
WineList <- DF3
rm(DF, DF2, DF3, winemag_data_130k_v2)
WineList <- WineList %>% mutate(`price_cat` = ifelse(price >= 6 & price <= 10, "10", ifelse(price >= 11 & price <= 15, "15", ifelse(price >= 16 & price <= 20, "20", ifelse(price >= 21 & price <= 25, "25", ifelse(price >= 26 & price <= 30, "30", ifelse(price >= 1 & price <= 5, "5", ifelse(price >= 31 & price <= 35, "35", ifelse(price >= 36 & price <= 40, "40", ifelse(price >= 41 & price <= 45, "45", ifelse(price >= 46 & price <= 50, "50", ifelse(price >= 51 & price <= 55, "55", ifelse(price >= 56 & price <= 60, "60", ifelse(price >= 61 & price <= 65, "65", ifelse(price >= 66 & price <= 70, "70", ifelse(price >= 71 & price <= 75, "75", ifelse(price >= 76 & price <= 80, "80", ifelse(price >= 81 & price <= 85, "85", ifelse(price >= 86 & price <= 90, "90", ifelse(price >= 91 & price <= 95, "95", ifelse (price >= 96 & price <= 100, "100", "no")))))))))))))))))))))
ggplot(WineList, aes(points)) + geom_histogram()
ggplot(WineList, aes(price_cut)) + geom_histogram()
ggplot(WineList, aes(x = price, y = points)) + geom_point() + geom_smooth()