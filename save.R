

source("use.R")

# install.packages("pacman")
#pacman::p_load(
#  janitor,
#  tidyr,
#  dplyr,
#  stringr,
#  lubridate,
#  purrr,
#  glue,
#  ggplot2
#)


# daily_dat <- readRDS("data/daily.rds")
#old_dat <- dir("daily", full.names = T) %>% 
#  keep(~str_detect(.x, "rds")) %>%
#  map_dfr_progress(readRDS)

#old_dat %>%
#  mutate(date_produced = lubridate::ymd(date)) %>%
#  drop_na(date_produced) %>% 
#  janitor::clean_names() %>% 
#  distinct(cntry, date_produced, .keep_all = T) %>% 
#  count(date_produced) %>%
#  ggplot(aes(date_produced, n)) +
#  geom_col() +
#  theme_minimal() +
#  labs(y = "How many Countries", x = "For each date", title = paste0("Daily Reports: ", Sys.time()))

#ggsave("overview.png", width = 8, height = 5)



#old_dat <- dir("lifelong", full.names = T) %>% 
#  keep(~str_detect(.x, "rds")) %>%
#  map_dfr_progress(readRDS)

#old_dat %>%
#  mutate(date_produced = lubridate::ymd(date)) %>%
#  drop_na(date_produced) %>% 
#  janitor::clean_names() %>% 
#  distinct(cntry, date_produced, .keep_all = T) %>% 
#  count(date_produced) %>%
#  ggplot(aes(date_produced, n)) +
#  geom_col() +
#  theme_minimal() +
#  labs(y = "How many Countries", x = "For each date", title = paste0("Lifelong Reports: ", Sys.time()))

#ggsave("overview_ll.png", width = 8, height = 5)


