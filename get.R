library(playwrightr)
# library(tidyverse)

source("use.R")

options(python_init = TRUE)




# install.packages("pacman")
pacman::p_load(
  reticulate,
  vroom,
  progress,
  janitor,
  fs,
  tidyr,
  # appendornot,
  countrycode,
  dplyr,
  stringr,
  lubridate,
  purrr,
  glue,
  rvest,
  cli,
  digest,
  googledrive
)

options(googledrive_quiet = TRUE)
# googledrive::drive_auth(path = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
# 
# drive_auth_configure(
#   path = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
# )

drive_auth_configure(api_key = Sys.getenv("GOOGLE_APPLICATION_KEY"))



# cntry <- "ES"
# cntry <- "ES"
py_install("xvfbwrapper", pip = T)
print("installed xvfbwrapper")
py_install("playwright", pip = T)
print("installed playwright")

conda_install("playwright", pip = T)
#system("playwright install")



py_install("fcntl", pip = T)
pw_init(use_xvfb = T)
# Launch the browser
print("Launch the browser")

browser_df <- browser_launch(
  headless = F,
  browser = "firefox",
  user_agent = NULL,
  user_data_dir = "out"
)

# daily_dat <- readRDS("data/daily.rds")
daily_dat <- dir("daily", full.names = T) %>% 
  keep(~str_detect(.x, "rds")) %>% 
  map_dfr_progress(readRDS)

print("headlesss")
# Create a new page

# page_df <- new_page(browser_df)
page_df <- browser_df %>%
  glimpse


print("sooo")
pw_restart <- function() {
  reticulate::py_run_string("p.stop()")
  pw_init(use_xvfb = T)
  reticulate::py_run_string("p = sync_playwright().start()")
}


print("sooo22")

on <- function(page_df, event, lambda_string) {
  playwrightr:::py_run(glue('{page_df$page_id}.on("{event}", {lambda_string})'))
  return(page_df)
}
off <- function(page_df, event, lambda_string) {
  playwrightr:::py_run(glue(
    '{page_df$page_id}.remove_listener("{event}", {lambda_string})'
  ))
  return(page_df)
}

print("soooxx")
execute_script <- function (page_df, script) {
  playwrightr:::py_run(glue("d = {{page_df$page_id}}.evaluate('{{script}}')"))
}

page_df %>%
  goto("https://www.facebook.com/ads/library/report")
print("visit website")
Sys.sleep(2)

# page_df %>% screenshot("/data/res/facebook_add_reports/test.png")

try({
  page_df %>%
    get_by_test_id("cookie-policy-manage-dialog-accept-button") %>%
    slice(1) %>%
    click() %>%
    screenshot("/data/res/facebook_add_reports/test.png")
})


# Write post-data string to disk into tmp
tmp_post_data_string <-
  paste0(digest::digest("YOOYOOo"), ".txt")#  tempfile(fileext = ".txt")
# page_df %>% on("request", glue::glue('lambda request: print(request.url)'))
# page_df %>% on("request", glue::glue('lambda request: print(request.post_data) if (request.method == "POST" and "report/v2/download" in request.url) else None'))
# page_df %>% on("request", glue::glue('lambda request: print(request.post_data) if (request.method == "POST" and "graphql" in request.url) else None'))
page_df %>% on(
  "request",
  glue::glue(
    'lambda request: open("{tmp_post_data_string}", "w").write(request.post_data) if (request.method == "POST" and "report/v2/download" in request.url) else None'
  )
)
page_df %>% on(
  "request",
  glue::glue(
    'lambda request: open("{tmp_post_data_string}", "w").write(request.post_data) if (request.method == "POST" and "graphql" in request.url) else None'
  )
)
print("some other stuff")
# Print Console
# tmp_download_link <- tempfile()
tmp_download_link <-
  paste0(digest::digest("sdff"), ".txt")#  tempfile(fileext = ".txt")

page_df %>% on("console",
               "lambda msg: open('{tmp_download_link}', 'w').write(msg.text)")

# First click to obtain the request post-body-data
page_df %>%
  get_by_text("Download report") %>%
  slice(2) %>%
  click()

# Print download path
tmp_download_path <-
  paste0(digest::digest("sdsdfsdfdff"), ".txt")#
page_df %>% on(
  "download",
  glue::glue(
    'lambda download: open("{tmp_download_path}", "w").write(download.path())'
  )
)
print("some other stuff 2")

data_string <- readLines(tmp_post_data_string, warn = F) %>%
  str_squish() %>%
  glimpse


# countries <- tibble::tibble(country = c("NL", "DE", "CA", "FR", "US"))
countries <-
  tibble::tibble(country = countrycode::codelist$iso2c) %>%
  filter(!is.na(country)) %>%
  glimpse
# countries <- fs::file_info(dir("/data", recursive = T, full.names = T)) %>%
#   filter(size > 1) %>%
#   pull(path) %>%
#   fs::path_dir() %>%
#   fs::path_file() %>%
#   unique
# readr::write_rds(countries, "data/countries.rds")
#
# countries <- tibble::tibble(country = readr::read_rds("data/countries.rds")) %>%
#   filter(!is.na(country)) %>%
#   glimpse

daysies <-
  tibble::tibble(day = lubridate::as_date(seq.int(
    lubridate::dmy("01-07-2019"), lubridate::today(), by = 1
  ))) %>%
  # days <- tibble::tibble(day = lubridate::as_date(seq.int(lubridate::dmy("15-07-2023"), lubridate::today(), by = 1))) #%>%
  head(-2)

dt <- expand_grid(countries, daysies) %>%
  glimpse


# all_reports <- dir("report", full.names = T, recursive = T)

# saveRDS(all_reports, "logs/all_reports.rds")

all_reports_old <- readRDS("logs/all_reports.rds")

dt %>%
  # arrange(day, country != "RU") %>%
  filter(country == "NL") %>%
  arrange(desc(day), country) %>%
  slice(1:7) %>%
  split(1:nrow(.)) %>% #bashR::simule_map(1)
  walk_progress( ~ {
    file_name <-
      glue::glue("report/{.x$country}/{as.character(.x$day)}.zip")
    if (file_name %in% all_reports_old)
      return()
    
    cli::cli_alert_info(glue::glue("{.x$country} - {.x$day}"))
    
    path_dir <- fs::path_dir(file_name)
    if (!fs::dir_exists(path_dir))
      fs::dir_create(path_dir)
    
    time_preset <- "yesterday"
    
    js_code <-
      paste0(
        'fetch("https://www.facebook.com/ads/library/report/v2/download/?report_ds=',
        as.character(.x$day),
        '&country=',
        .x$country,
        '&time_preset=',
        time_preset,
        '", {"headers": {"accept": "*/*", "content-type": "application/x-www-form-urlencoded"}, "body": "',
        data_string,
        '", "method": "POST", "mode": "cors", "credentials": "include" }).then(resp => resp.text()).then(data => console.log(data));'
      )
    
    page_df %>% execute_script(js_code)
    Sys.sleep(.1)
    
    download_url <- readLines(tmp_download_link, warn = F) %>%
      str_extract("\"https.*?\"") %>%
      str_remove_all("(^\")|(\"$)") %>%
      str_remove_all("\\\\") %>%
      glimpse
    
    if (is.na(download_url)) {
      if (!(.x$day %in% lubridate::as_date((lubridate::today() - lubridate::days(10)):lubridate::today()
      ))) {
        write(list(), file_name)
      }
    } else if (str_detect(download_url, "facebook.com/help/contact/")) {
      cli::cli_alert_danger("Blocked")
      Sys.sleep(10)
      return("Blocked")
    } else {
      download.file(download_url,
                    file_name,
                    quiet = T,
                    mode = "wb")
    }
    
    
    
    Sys.sleep(runif(1, 0, .3))
  })

# dir("report/ES", full.names = T, recursive = T) %>% sort
dir.create("extracted")
dir.create("report")

dir("report/NL", full.names = T, recursive = T) %>%
  sort(decreasing = T) %>% 
  .[1:7] %>% 
  walk_progress( ~ {
    unzip(.x, exdir = "extracted")
  })

latest_available_date <- dir("extracted") %>% 
  keep(~str_detect(.x, "NL")) %>% 
  sort(decreasing = T) %>% 
  str_split("_") %>% unlist %>% .[2]

daysies <-
  tibble::tibble(day = lubridate::as_date(seq.int(
    lubridate::dmy("01-07-2019"), lubridate::today(), by = 1
  ))) %>%
  # days <- tibble::tibble(day = lubridate::as_date(seq.int(lubridate::dmy("15-07-2023"), lubridate::today(), by = 1))) #%>%
  filter(day <= lubridate::as_date(latest_available_date))

dt <- expand_grid(countries, daysies) %>%
  filter(country %in% dplyr::count(daily_dat, cntry, sort = T)$cntry) %>% 
  glimpse


try({
  
  
  dt %>%
    # slice(1:10) %>% 
    # arrange(day, country != "RU") %>%
    # filter(country == cntry) %>%
    arrange(desc(day), country) %>%
    split(1:nrow(.)) %>% #bashR::simule_map(1)
    walk_progress( ~ {
      file_name <<-
        glue::glue("report/{.x$country}/{as.character(.x$day)}.zip")
      if (file_name %in% all_reports_old)
        return()
      
      cli::cli_alert_info(glue::glue("{.x$country} - {.x$day}"))
      # return("s")
      path_dir <- fs::path_dir(file_name)
      if (!fs::dir_exists(path_dir))
        fs::dir_create(path_dir)
      
      # .x <- list(day = "2021-12-17", country = "NL")
      # .x <- list(day = "2021-12-16", country = "NL")
      # .x <- list(day = "2021-01-17", country = "NL")
      # time_preset <- "lifelong"
      time_preset <- "yesterday"
      # time_preset <- "last_90_days"
      # time_preset <- "last_365_days"
      
      js_code <-
        paste0(
          'fetch("https://www.facebook.com/ads/library/report/v2/download/?report_ds=',
          as.character(.x$day),
          '&country=',
          .x$country,
          '&time_preset=',
          time_preset,
          '", {"headers": {"accept": "*/*", "content-type": "application/x-www-form-urlencoded"}, "body": "',
          data_string,
          '", "method": "POST", "mode": "cors", "credentials": "include" }).then(resp => resp.text()).then(data => console.log(data));'
        )
      
      page_df %>% execute_script(js_code)
      Sys.sleep(.1)
      
      download_url <- readLines(tmp_download_link, warn = F) %>%
        str_extract("\"https.*?\"") %>%
        str_remove_all("(^\")|(\"$)") %>%
        str_remove_all("\\\\") %>%
        glimpse
      
      if (is.na(download_url)) {
        if (!(.x$day %in% lubridate::as_date((lubridate::today() - lubridate::days(10)):lubridate::today()
        ))) {
          write(list(), file_name)
        }
      } else if (str_detect(download_url, "facebook.com/help/contact/")) {
        cli::cli_alert_danger("Blocked")
        Sys.sleep(10)
        stop("Blocked")
      } else {
        download.file(download_url,
                      file_name,
                      quiet = T,
                      mode = "wb")
      }
      
      
      
      Sys.sleep(runif(1, 0, .3))
    })
  
  
}) 
#
try({
  
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
  

  # saveRDS(dates_already_present, "logs/dates_already_present.rds")
  
  # appendornot::save_lines(dates_already_present, "logs/dates_already_present.txt")
  
  # dates_already_present[str_detect(dates_already_present, "ZW")]
  
  # still_to_extracted <-  dir("report", full.names = T, recursive = T) %>%
  #   discard( ~ magrittr::is_in(.x, paste0("report/", dates_already_present, ".zip"))) 
  
  # sample(still_to_extracted)
  
  # unzip("report/ZW/2023-07-05.zip", exdir = "extracted")
  
  dir("report", full.names = T, recursive = T) %>%
    walk_progress( ~ {
      try({
        unzip(.x, exdir = "extracted")
      })
    })
  
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
  
  
  
  the_dat <-  dir("extracted", full.names = T, recursive = F) %>%
    keep(~ str_detect(.x, "advert")) %>%
    # .[1:5000] %>%
    discard( ~ magrittr::is_in(.x, unique(old_dat$path))) %>%
    tibble(thepath = .) %>% 
    mutate(id =  row_number()%%50) %>% 
    group_split(id) %>% 
    map_dfr_progress(~{
      .x$thepath %>%  map_dfr(~ {
        cntry_str <- str_split(.x, "_") %>% unlist %>% .[3]
        tframe <- str_split(.x, "_") %>% unlist %>% .[4]
        
        thedata <- vroom::vroom(.x, show_col_types = F) %>%
          janitor::clean_names() %>%
          mutate(date = str_extract(.x, "\\d{4}-\\d{2}-\\d{2}")) %>%
          mutate_all(as.character) %>%
          mutate(path = .x) %>%
          mutate(tf = tframe) %>%
          mutate(cntry = cntry_str)
        
        return(thedata)
      })
    })
  
  print("################1")
  
  if (any(c("name_disclaimer_amount") %in% names(the_dat))) {
    print("##within1")
    print(the_dat)
    the_dat <- the_dat %>%
      filter(is.na(name_disclaimer_amount))  %>%
      janitor::remove_empty()
    print("##within2")
    print(the_dat)
  } else {
    print("##after1")
    # print(the_dat)
    the_dat <- the_dat
    print("##after2")
    # print(the_dat)
  }
  print("################2")
  
  the_dat <- the_dat %>%
    bind_rows(old_dat) %>%
    distinct()
  
  # saveRDS(the_dat, "data/daily.rds")
  print("################3")
  
  the_dat %>%
    group_by(cntry) %>%
    group_split() %>%
    walk_progress(~{saveRDS(.x, paste0("daily/",.x$cntry[1], ".rds"))})
  
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

all_reports <- dir("report", full.names = T, recursive = T)

all_reports <- all_reports_old %>% 
  c(all_reports) %>% 
  unique()

saveRDS(all_reports, file = "logs/all_reports.rds")

library("googledrive")


extracted_id <- googledrive::drive_ls("meta_reports") %>% 
  filter(name == "extracted") %>% pull(id)

drive_upload_folder(folder = "extracted", drive_path = extracted_id)

report_id <- googledrive::drive_ls("meta_reports") %>% 
  filter(name == "report") %>% pull(id)

drive_upload_folder(folder = "report", drive_path = report_id)

unlink("report", recursive = T, force = T)
unlink("extracted", recursive = T, force = T)




