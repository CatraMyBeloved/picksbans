# Authenticate using saved token
drop_auth(rdstoken = "./dropbox_token.rds")

download_success <- download_data()
if (!download_success) {
  
  message("Failed to download data. Falling back to local.")
  load("./data/esports_data.RData")
  
} else {
  message("Data received from Dropbox.")
}

# Create team, hero and map lists for later use in filters
team_list <- as.list(unique(teams$team_name))
hero_list <- as.list(unique(heroes$hero_name))
map_list <- as.list(unique(maps$map_name))

# Make sure they are available in the global environment
assign("team_list", team_list, envir = .GlobalEnv)
assign("hero_list", hero_list, envir = .GlobalEnv)
assign("map_list", map_list, envir = .GlobalEnv)