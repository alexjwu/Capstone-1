---
title: "Capstone Data Manip Desc"
author: "Alex Wu"
date: "July 22, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Wine Ratings by Region, Price, and Variety
This data set contain over 150k lines of data of various wines accross the world.

Key points

- points are on a scale of 1 - 100
- maximum price of $99
- Wine is identified by 
    - Country
    - Province
    - Varietal
    - Winery
  

### Cleaning up the data

#### Importing the data
library(readxl)
winemag_data_130k_v2 <- read_excel("winemag-data-130k-v2.xlsx")
View(winemag_data_130k_v2)

library(dplyr)
library(tidyr)



#### Manipulating the data

DF <- winemag_data_130k_v2
DF %>% summary(price)


##### Price was missing. Filled it with median price
DF <- DF %>% mutate(`price` = ifelse(is.na(price), "25", price))



##### Removed columns of data that would not be used in analysis
DF3 <- DF[, c(1,2,5,6,7,13,14)]
