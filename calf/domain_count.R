
library(httr)


reg <- "([^A-Za-z_\\d#@:/']|'(?![A-Za-z_\\d#@:/]))"
urls_temp <- tweets %>%
  unnest_tokens(word, text, token = "regex", pattern = reg, to_lower = FALSE) %>%
  mutate(word = str_replace_all(word, "https|//t|http|&amp;|&lt;|&gt;", ""),
         word = str_replace_all(word, "co/", "https://t.co/")) %>%
  select(word) %>%
  filter(grepl('https://t.co/', word, fixed = TRUE)) %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word, n))

urls_common <- urls_temp %>%
  filter(n >= 50) %>%
  mutate(source_url = as.character(word)) %>%
  select(source_url, count = n)

url <- t(sapply(urls_common$source_url, GET)) %>%
  as_tibble() %>%
  select(url, status_code)


url_list <- cbind(urls_common, unnest(url)) %>%
  as_tibble() %>%
  select(url, count, status_code) %>%
  filter(status_code != 404,
         url != 'https://t.co/',
         !grepl('https://twitter.com/', url))

extract_domain <- function(url) {
  return(gsub('www.', '', unlist(strsplit(unlist(strsplit(as.character(url), '//'))[2], '/'))[1]))
}

# count the frequency of a domain's occurrence in the most frequent URL list
domain_list <- url_list %>%
  mutate(domain = mapply(extract_domain, url)) %>%
  group_by(domain) %>%
  summarize(domain_count = sum(count)) %>%
  arrange(desc(domain_count))



domain_list$color <- c(0,0,0,0,0,
                  0,0,0,0,0,
                  0,0,0,1,1,
                  0,0,0,0,0,
                  0,0,0,0,0,
                  0,1,0,0,0,
                  0,0,0,0,0,
                  0,0
                  )

domain_list %>% 
  mutate(color = as.factor(color)) %>% 
  filter(domain_count > 50) %>% 
  ggplot(., aes(x= reorder(domain, domain_count), y = domain_count, fill = color)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("azure3", "firebrick3")) +
  coord_flip() +
  geom_text(aes(y = domain_count + 55, label = domain_count), position = position_dodge(width = .9), size = 10, family = "font") +
  theme_gg("Roboto Slab") +
  labs(x = "", y = "Count", title = "Which Websites Drive Traffic About Evangelicals?", caption = "Data: Twitter REST API") + 
  theme(plot.title = element_text(size = 44)) +
  ggsave("D://tags_twitter/calf/domain_count.png", width = 7)

