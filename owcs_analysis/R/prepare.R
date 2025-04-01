drop_auth()

download_success <- download_data()
if (!download_success) {
  
  message("Failed to download data. Falling back to local.")
  load("./data/esports_data.RData")
  
} else {
  message("Data received from Dropbox.")
}

team_list <- as.list(unique(teams$team_name))
hero_list <- as.list(unique(heroes$hero_name))
map_list <- as.list(unique(maps$map_name))

