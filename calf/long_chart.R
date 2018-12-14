## Run combine_all_data.R first ##

## Getting things into date formats for later

tweets$month <- lubridate::month(tweets$created_at)
tweets$year <- lubridate::year(tweets$created_at)
# tweets$hour <- lubridate::hour(tweets$created_at)

tweets$date <- date(tweets$created_at)

tweets$date2 <- round_date(tweets$created_at, "hour")



## Calculating number of tweets per day
graph <- tweets %>% 
  group_by(date) %>% 
  count()
  
## Graphing the whole thing
graph %>% 
  na.omit() %>% 
  ggplot(., aes(x=date, y = n, fill = n)) +
  geom_col() +
  scale_fill_gradient(low = "#33001b", high = "#ff0084") +
  theme_gg("Roboto Slab") +
  labs(x = "Date", y = "Count per Day", title = "The Timeline of Evangelical Tweet Gathering", caption = "Data: Twitter REST API") +
  scale_x_date(labels = date_format("%b-%Y")) +
  ggsave("D://tags_twitter/calf/long.png", width = 10, height = 6)

tweets %>% 
  group_by(date) %>% 
  count() %>% 
  ungroup(date) %>% 
  mean_ci(n)

# # A tibble: 1 x 7
# mean    sd     n level    se lower upper
# <dbl> <dbl> <int> <dbl> <dbl> <dbl> <dbl>
#   1 2680. 3598.    67  0.05  440. 1802. 3557.

# > graph %>% arrange(-n)
# # A tibble: 57 x 2
# # Groups:   date [57]
# date           n
# <date>     <int>
#   1 2017-12-13 25316
# 2 2017-12-14 15297
# 3 2017-12-12  7906
# 4 2017-12-15  7567
# 5 2017-12-17  7391
# 6 2018-08-28  6250
# 7 2018-08-29  5368
# 8 2017-12-16  4031
# 9 2017-03-07  3453
# 10 2017-03-30  3419

