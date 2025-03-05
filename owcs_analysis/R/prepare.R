load("./data/esports_data.RData")

team_list <- as.list(unique(teams$team_name))
hero_list <- as.list(unique(heroes$hero_name))
map_list <- as.list(unique(maps$map_name))

