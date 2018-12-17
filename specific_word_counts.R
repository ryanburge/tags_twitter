match <- tweets %>%
  unnest_tokens(word, text, to_lower = TRUE)


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

find_fun <- function(df, word){
  
  df %>% 
    filter(word == !! word) %>% 
    count() %>% 
    mutate(term = !! word) %>% 
    mutate(term = firstup(term))
  
}


ccc1 <- match %>% find_fun("god") %>% mutate(type = "Religious")
ccc2 <- match %>% find_fun("jesus") %>% mutate(type = "Religious")
ccc3 <- match %>% find_fun("christ") %>% mutate(type = "Religious")
ccc4 <- match %>% find_fun("faith") %>% mutate(type = "Religious")
ccc5 <- match %>% find_fun("bible") %>% mutate(type = "Religious")
ccc6 <- match %>% find_fun("trump") %>% mutate(type = "Political")
ccc7 <- match %>% find_fun("vote") %>% mutate(type = "Political")
ccc8 <- match %>% find_fun("election") %>% mutate(type = "Political")
ccc9 <- match %>% find_fun("pence") %>% mutate(type = "Political")
ccc10 <- match %>% find_fun("political") %>% mutate(type = "Political")

graph <- bind_df("ccc")


graph %>% 
  mutate(pol = as.factor(type)) %>% 
  ggplot(., aes(x= reorder(term, n), y = n, fill = pol)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("firebrick3", "azure3")) +
  theme_gg("Roboto Slab") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 32000)) +
  geom_text(aes(y = n + 1625, label = n), position = position_dodge(width = .9), size = 10, family = "font") +
  theme(plot.title = element_text(size = 34)) +
  labs(x = "", y = "Count", title = "The Frequency of Key Political and Religious Terms", caption = "Data: Twitter REST API") + 
  ggsave("D://tags_twitter/calf/top_ten_count.png")


match %>% find_fun("hypocrite")
match %>% find_fun("hypocrisy")
match %>% find_fun("hypocritical")


match %>% find_fun("love")





