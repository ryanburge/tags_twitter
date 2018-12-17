library(socsci)
library(lubridate)
library(showtext)
library(tidytext)
source("D://theme.R")


dat1 <- read_csv("D://tags_twitter/data/tweets_mar17.csv") %>% select(-X1)
dat2 <- read_csv("D://tags_twitter/data/tweets_dec17.csv") 

dat1$time <- dmy_hms(dat1$time)
dat2$time <- dmy_hms(dat2$time)


first <- bind_rows(dat1, dat2)
rm(dat1)
rm(dat2)

first <- first %>% 
  select(status_id = id_str, screen_name = from_user, text, created_at = time)


load("D://tags_twitter/data/tweets_apr18.RData")
load("D://tags_twitter/data/tweets_aug18.RData")
load("D://tags_twitter/data/tweets_dec18.RData")

dat3 <- tweets_apr %>% 
  select(status_id, screen_name, text, created_at) %>% 
  mutate(status_id = as.numeric(status_id))

dat4 <- tweets_aug %>% 
  select(status_id, screen_name, text, created_at) %>% 
  mutate(status_id = as.numeric(status_id))

dat5 <- tt %>% 
  select(status_id, screen_name, text, created_at) %>% 
  mutate(status_id = as.numeric(status_id))

tweets <- bind_rows(first, dat3, dat4, dat5)

rm(tweets_apr, tweets_aug, dat3, dat4, first, tt)

