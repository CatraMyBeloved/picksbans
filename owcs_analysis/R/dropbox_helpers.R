library(rdrop2)

# Download data from Dropbox INTO ENVIRONMENT
download_data <- function(file_path = "/owcs_analysis_shinyapp/esports_data.Rdata") {
  temp_file <- tempfile(fileext = ".RData")
  tryCatch({
    # Download file from Dropbox
    drop_download(path = file_path, local_path = temp_file, overwrite = TRUE)
    message("Downloaded data file from Dropbox")
    
    # Load data into global environment
    load(temp_file, envir = .GlobalEnv)
    file.remove(temp_file)
    return(TRUE)
  }, error = function(e) {
    message("Error downloading file: ", e$message)
    return(FALSE)
  })
}

# Upload data to Dropbox FROM ENVIRONMENT
upload_data <- function(dropbox_path = "/owcs_analysis_shinyapp/", file_path = "./data/esports_data.Rdata") {
  tryCatch({
    # Save current data objects to data file
    save(teams, maps, heroes, matches, match_maps, rounds, hero_composition, bans, 
         file = "./data/esports_data.Rdata")
    
    # Upload to Dropbox
    drop_upload(file = file_path, path = dropbox_path, mode = "overwrite", verbose = TRUE)
    message("Uploaded data file to Dropbox")
    
    return(TRUE)
  }, error = function(e) {
    message("Error uploading file: ", e$message)
    return(FALSE)
  })
}