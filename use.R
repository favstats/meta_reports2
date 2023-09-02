#' Upload a folder to Google Drive
#'
#' Upload the contents of a folder (directory) to Google Drive recursively. The
#' implementation is depth-first. Only uploads objects that have type "file" or
#' "directory" according to `fs::dir_info()`; ignores types "symlink", "FIFO",
#' "socket", "character_device" or "block_device".
#'
#' @param folder The local folder that is to be uploaded, given as a path e.g.
#'   with `fs::path()`. Note only the contents of the folder are uploaded; the
#'   original local folder's name will not appear in Google Drive.
#'
#' @param drive_path The destination folder on Google Drive, given as a URL, file id, or dribble
#'
#' @return A dribble of the uploaded files (not directories though.)
drive_upload_folder <- function(folder, drive_path) {
  # Only call fs::dir_info once in order to avoid weirdness if the contents of the folder is changing
  contents <- fs::dir_info(folder, type = c("file", "dir"))
  dirs_to_upload <- contents %>%
    dplyr::filter(type == "directory") %>%
    pull(path)
  
  # Directly upload the files
  uploaded_files <- contents %>%
    dplyr::filter(type == "file") %>%
    pull(path) %>%
    purrr::map_dfr(googledrive::drive_upload, path = drive_path)
  
  # Create the next level down of directories
  dirs_to_upload %>%
    fs::path_rel(folder) %>%
    purrr::map(., googledrive::drive_mkdir, path = drive_path) %>%
    # Recursively call this function
    purrr::map2_dfr(dirs_to_upload, ., drive_upload_folder) %>%
    # return a dribble of what's been uploaded
    dplyr::bind_rows(uploaded_files) %>%
    invisible()
}






walk_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x),
                                   format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
                                   # format = " downloading [:bar] :percent eta: :eta",
                                   force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::walk(.x, f, ...)
}

map_dfr_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_dfr(.x, f, ...)
}
