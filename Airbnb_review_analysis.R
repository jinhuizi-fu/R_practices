install.packages("textcat")
library(dplyr)
library(textcat)

reviews <- read.csv("~/Documents/GitHub/data/reviews.csv", comment.char="#")

review2020 <- reviews %>% 
  na.omit() %>% 
  filter(date > '2020-01-01') %>% 
  mutate(len = nchar(comments)) %>% 
  filter(len > 8)

review2020$lan <- textcat(review2020$comments)
  