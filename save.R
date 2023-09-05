
# library(tidyverse)

source("use.R")

options(python_init = TRUE)




# install.packages("pacman")
pacman::p_load(
  janitor,
  tidyr,
  dplyr,
  stringr,
  lubridate,
  purrr,
  glue,
  rvest
)


# daily_dat <- readRDS("data/daily.rds")
old_dat <- dir("daily", full.names = F) %>% 
  keep(~str_detect(.x, "rds")) %>%
  map_dfr(readRDS)

old_dat %>%
  mutate(date_produced = lubridate::ymd(date)) %>%
  drop_na(date_produced) %>% 
  janitor::clean_names() %>% 
  distinct(cntry, date_produced, .keep_all = T) %>% 
  count(date_produced) %>%
  ggplot(aes(date_produced, n)) +
  geom_col() +
  theme_minimal() +
  labs(y = "How many Countries", x = "For each date")

ggsave("overview.png", width = 8, height = 6)

