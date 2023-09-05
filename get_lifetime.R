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

drive_auth(path = Sys.getenv("GOOGLE_APPLICATION_KEY"))



# cntry <- "ES"
# cntry <- "ES"
#py_install("xvfbwrapper", pip = T)
#py_install("playwright", pip = T)

conda_install(packages = "xvfbwrapper", pip = T)

print("installed xvfbwrapper")
conda_install(packages = "playwright", pip = T)

print("installed playwright")


#py_install("fcntl", pip = T)
pw_init(use_xvfb = T)
print("pw initted")
# Launch the browser
system("playwright install")
print("Launch the browser")

browser_df <- browser_launch(
  headless = F,
  browser = "firefox",
  user_agent = NULL,
  user_data_dir = "out"
)

dir.create("lifelong")

old_dat <- dir("daily", full.names = F) %>% 
  keep(~str_detect(.x, "rds")) %>%
  str_remove_all("\\.rds") %>%
  unique()


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
print("afterdaises")


dt <- expand_grid(countries, daysies) %>%
  glimpse


try({
  all_reports_old <- readRDS("logs/all_reports_lifelong.rds")
})

if(!exists("all_reports_old")){
 all_reports_old <- c()
}

# dir("report/ES", full.names = T, recursive = T) %>% sort
dir.create("extracted")
dir.create("report")
print("creation")


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
    
    #time_preset <- "yesterday"
    time_preset <- "lifelong"

    
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


print("NL DOWNLOADED")

dir("report/NL", full.names = T, recursive = T) %>%
  sort(decreasing = T) %>% 
  .[1:7] %>% 
  walk_progress( ~ {
    unzip(.x, exdir = "extracted")
  })

print("NL UNZIPPED")


latest_available_date <- dir("extracted") %>% 
  keep(~str_detect(.x, "NL")) %>% 
  sort(decreasing = T) %>% 
  str_split("_") %>% unlist %>% .[2]

print("whats the latest available date")


if(length(latest_available_date)==0){
  print("its actually zero why")

  latest_available_date <- as.character(lubridate::today()-lubridate::days(4))
}

daysies <-
  tibble::tibble(day = lubridate::as_date(seq.int(
    lubridate::dmy("01-07-2019"), lubridate::today(), by = 1
  ))) %>%
  # days <- tibble::tibble(day = lubridate::as_date(seq.int(lubridate::dmy("15-07-2023"), lubridate::today(), by = 1))) #%>%
  filter(day <= lubridate::as_date(latest_available_date))

dt <- expand_grid(countries, daysies) %>%
  filter(country %in% old_dat) %>% 
  glimpse

print("NL DONE")

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
      time_preset <- "lifelong"
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






  print("garcia")

  
  
  dir("report", full.names = T, recursive = T) %>%
    walk_progress( ~ {
      try({
        unzip(.x, exdir = "extracted")
      })
    })
  
  
  print("garcia3")
  
  step1 <- dir("extracted", full.names = T, recursive = F) 
  print(head(step1))
  tobeextracted <- step1 %>% keep(~ str_detect(.x, "advert")) 
  print(head(tobeextracted))
  
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
   # print("##after1")
    # print(thedata)
    thedata <- thedata
 #   print("##after2")
    # print(thedata)
  }

try({
  thedata <- thedata %>%
    bind_rows(readRDS(paste0("lifelong/",cntry_str, ".rds"))) %>%
    distinct()   
})
      
thedata %>%
    saveRDS(paste0("lifelong/",cntry_str, ".rds"))
      
      return(thedata)
    })
  

print("################5")

unlink("node_modules", recursive = T, force = T)
unlink("out", recursive = T, force = T)

print("################6")

dir() %>%
  keep( ~ str_detect(.x, ".txt")) %>%
  walk(file.remove)


print("################8")


# all_reports_old <- readRDS("logs/all_reports.rds")

print("################9")

all_reports <- dir("report", full.names = T, recursive = T)

print("################10")

all_reports <- all_reports_old %>% 
  c(all_reports) %>% 
  unique()
print("################11")

saveRDS(all_reports, file = "logs/all_reports_lifelong.rds")

print("################12")


extracted_id <- googledrive::drive_ls("meta_reports") %>% 
  filter(name == "extracted_lifelong") %>% pull(id)

print("################13")


unlink("extracted/regions", recursive = T, force = T)
drive_upload_folder(folder = "extracted", drive_path = extracted_id)

print("################14")


report_id <- googledrive::drive_ls("meta_reports") %>% 
  filter(name == "report_lifelong") %>% pull(id)

print("################15")


drive_upload_folder(folder = "report", drive_path = report_id)

print("################16")


unlink("report", recursive = T, force = T)
unlink("extracted", recursive = T, force = T)

print("################17")

# source("save.R")














