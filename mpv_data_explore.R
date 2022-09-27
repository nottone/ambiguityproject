rm(list = ls())

library(tidyverse)

raw <- read_csv("Mapping Police Violence.csv")

# Looking at numbers
raw %>%
  count(name) %>%
  arrange(-n)

length(unique(raw$name)) # 10054 unique names

length(unique(raw$mpv_id)) # 9927 unique mpv_id's - incident level ID?

# Cleaning up URLs
urls <- str_split(raw$news_urls, "\n", simplify = T) %>%
  as.data.frame()

colnames(urls) <- paste0("single_url", 1:length(urls))

dat <- cbind(raw, urls) %>%
  pivot_longer(cols = starts_with("single_url"), names_to = "num_url", values_to = "url", 
               names_repair = "minimal", values_drop_na = TRUE) %>%
  filter(url != "") %>%
  select(-num_url)

# Flags for major websites
dat <- dat %>%
  mutate(nyt_flag = ifelse(str_detect(url, pattern = "nytimes"), 1, 0),
         washpo_flag = ifelse(str_detect(url, pattern = "washingtonpost"), 1, 0),
         wsj_flag = ifelse(str_detect(url, pattern = "wsj"), 1, 0),
         ap_flag = ifelse(str_detect(url, pattern = "apnews"), 1, 0),
         cnn_flag = ifelse(str_detect(url, pattern = "cnn"), 1, 0),
         fox_flag = ifelse(str_detect(url, pattern = "foxnews"), 1, 0),
         usatoday_flag = ifelse(str_detect(url, pattern = "usatoday"), 1, 0),
         nypost_flag = ifelse(str_detect(url, pattern = "nypost"), 1, 0),
         msn_flag = ifelse(str_detect(url, pattern = "msn"), 1, 0)) %>%
  mutate(majornews_flag = nyt_flag + washpo_flag + wsj_flag + ap_flag + msn_flag +
                                cnn_flag + fox_flag + usatoday_flag + nypost_flag)

