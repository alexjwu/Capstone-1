library(readxl)
winemag_data_130k_v2 <- read_excel("winemag-data-130k-v2.xlsx")
View(winemag_data_130k_v2)

library(dplyr)
library(tidyr)


DF <- winemag_data_130k_v2
DF %>% summary(price)
DF <- DF %>% mutate(`price` = ifelse(is.na(price), "25", price))
DF3 <- DF[, c(1,2,5,6,7,13,14)]
WineList <- DF3
rm(DF, DF2, DF3, winemag_data_130k_v2)
