
# library(tidyverse)

source("use.R")

options(python_init = TRUE)




# install.packages("pacman")
pacman::p_load(
  vroom,
  progress,
  janitor,
  tidyr,
  dplyr,
  stringr,
  lubridate,
  purrr,
  glue,
  rvest,
  cli,
  googledrive
)

options(googledrive_quiet = TRUE)
# googledrive::drive_auth(path = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
# 
# drive_auth_configure(
#   path = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
# )

drive_auth_configure(api_key = Sys.getenv("GOOGLE_APPLICATION_KEY"))



try({
  
  print("garcia")
  dates_already_present_old <- readRDS("logs/dates_already_present.rds")
  
  dates_already_present <-
    dir("extracted", full.names = T, recursive = F) %>%
    # .[10] %>% 
    str_split("_") %>% map_chr(~ paste0(
      str_split(.x, "_") %>% unlist %>% .[3],
      "/",
      lubridate::as_date(str_split(.x, "_") %>% unlist %>% .[2]) + (lubridate::days(1))
    )) %>%
    unique() %>%
    discard( ~ str_detect(.x, "NA/NA")) %>%
    na.omit() %>% as.character() 
  
  
  dir("report", full.names = T, recursive = T) %>%
    walk_progress( ~ {
      try({
        unzip(.x, exdir = "extracted")
      })
    })
  
  print("garcia2")
   old_dat <- dir("daily", full.names = T) %>%
     keep(~str_detect(.x, "rds")) %>%
     map_dfr_progress(readRDS)
  
  if (any(c("name_disclaimer_amount") %in% names(old_dat))) {
    old_dat <- old_dat %>%
      filter(is.na(name_disclaimer_amount))  %>%
      janitor::remove_empty()
  } else {
    old_dat <- old_dat
  }
  
  # the_dat %>% count(id, sort  = T)
  
  # table(1:50000%%50)
  
  print("garcia3")
  
  step1 <- dir("extracted", full.names = T, recursive = F) 
  print(head(step1))
  step2 <- step1 %>% keep(~ str_detect(.x, "advert")) 
  print(head(step2))
  tobeextracted <- step2 %>%  discard( ~ magrittr::is_in(.x, unique(old_dat$path)))
  print(head(tobeextracted))
  
  # tobeextracted <- dir("extracted", full.names = T, recursive = F) %>%
  #    keep(~ str_detect(.x, "advert"))  %>%
  #    discard( ~ magrittr::is_in(.x, unique(old_dat$path))) 
  
  #  print(head(tobeextracted))
  
  the_dat <- tobeextracted %>%
    walk_progress(~ {
      cntry_str <- str_split(.x, "_") %>% unlist %>% .[3]
      tframe <- str_split(.x, "_") %>% unlist %>% .[4]
      
      thedata <- vroom::vroom(.x, show_col_types = F) %>%
        janitor::clean_names() %>%
        mutate(date = str_extract(.x, "\\d{4}-\\d{2}-\\d{2}")) %>%
        mutate_all(as.character) %>%
        mutate(path = .x) %>%
        mutate(tf = tframe) %>%
        mutate(cntry = cntry_str)

    if (any(c("name_disclaimer_amount") %in% names(thedata))) {
    print("##within1")
    print(thedata)
    thedata <- thedata %>%
      filter(is.na(name_disclaimer_amount))  %>%
      janitor::remove_empty()
    print("##within2")
    print(thedata)
  } else {
    print("##after1")
    # print(thedata)
    thedata <- thedata
    print("##after2")
    # print(thedata)
  }

  thedata %>%
    bind_rows(readRDS(paste0("daily/",cntry_str, ".rds"))) %>%
    distinct() %>%
    saveRDS(paste0("daily/",cntry_str, ".rds"))
      
      return(thedata)
    })
  
  
  
  print("################1")
  

  print("################2")
  

  
  # saveRDS(the_dat, "data/daily.rds")
  print("################3")
  

  
  print("################4")
  # vroom::vroom_write(the_dat, "data/daily.csv")
})

print("################5")

unlink("node_modules", recursive = T, force = T)
unlink("out", recursive = T, force = T)

print("################6")

dir() %>%
  keep( ~ str_detect(.x, ".txt")) %>%
  walk(file.remove)

print("################7")



dates_already_present <- dates_already_present_old %>% 
  c(dates_already_present) %>% 
  unique()

all_reports_old <- readRDS("logs/all_reports.rds")

all_reports <- dir("report", full.names = T, recursive = T)

all_reports <- all_reports_old %>% 
  c(all_reports) %>% 
  unique()

saveRDS(all_reports, file = "logs/all_reports.rds")



extracted_id <- googledrive::drive_ls("meta_reports") %>% 
  filter(name == "extracted") %>% pull(id)

drive_upload_folder(folder = "extracted", drive_path = extracted_id)

report_id <- googledrive::drive_ls("meta_reports") %>% 
  filter(name == "report") %>% pull(id)

drive_upload_folder(folder = "report", drive_path = report_id)

unlink("report", recursive = T, force = T)
unlink("extracted", recursive = T, force = T)




