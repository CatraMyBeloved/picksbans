get_pickrates <- function(min_appearances = 2){
  #get total number of maps played
  total_maps <- n_distinct(match_maps$match_map_id)
  #join tables
  pickrates <- hero_composition |> 
    left_join(rounds, by = "round_id") |> 
    left_join(heroes, by = "hero_id") |> 
    left_join(match_maps, by = "match_map_id") |> 
    #get distinct hero appearances per map
    distinct(hero_name, match_map_id, role) |> 
    group_by(hero_name, role) |> 
    #get counts and rates
    summarise(
      appearances = n(),
      pickrate = appearances/total_maps
    ) |> 
    filter(appearances > {{min_appearances}}) |> 
    arrange(desc(pickrate)) 
  return(pickrates)
}

#Visualize pickrates

visualize_pickrates <- function(pickrates, top_n = 5){
  pickrates |> 
    group_by(role) |> 
    slice(1:{{top_n}}) |> 
    ungroup() |> 
    ggplot(aes(x = reorder(hero_name, pickrate), y = pickrate, fill = role)) +
    geom_col() + 
    labs(x = "Hero", y = "Pickrate") + 
    coord_flip() 
}

#visualize pickrates divided by role
visualize_pickrates_by_role <- function(pickrates, top_n = 5){
  visualize_pickrates({{pickrates}}, {{top_n}}) +
    facet_wrap(vars(role), nrow = 3, scales = "free")
}

#get pickrates per mode

get_pickrates_per_mode <- function(min_appearances = 2){
  total_maps_per_mode <- match_maps |> 
    left_join(maps, by = "map_id") |> 
    group_by(mode) |> 
    summarise(
      n_played = n()
    )
    
    pickrates_by_mode <- hero_composition |> 
    left_join(heroes, by = "hero_id") |> 
    left_join(rounds, by = "round_id") |> 
    left_join(match_maps, by = "match_map_id") |> 
    left_join(maps, by = "map_id") |> 
    distinct(hero_name, match_map_id, role, mode) |> 
    group_by(hero_name, role, mode) |> 
    summarise(
      appearances_by_mode = n()) |> 
    left_join(total_maps_per_mode, by = "mode") |> 
      mutate(pickrate = appearances_by_mode / n_played, n_played = NULL) |> 
      arrange(desc(pickrate))
    
    return(pickrates_by_mode)
}

get_pickrates_by_team <- function(min_appearances = 2){
  maps_played_per_team <- bind_rows(
    match_maps |> left_join(
      matches, by = "match_id") |> 
      select(team = team1_id),
    match_maps |> left_join(
      matches, by = "match_id") |> 
      select(team = team2_id)
  ) |> 
    count(team) |> 
    arrange(desc(n)) |> 
    rename(total_games = n) |> 
    left_join(teams, by = c("team" = "team_id")) |> 
    select(team, total_games)
  
  pickrates_per_team <- hero_composition |> 
    left_join(heroes, by = "hero_id") |> 
    left_join(rounds, by = "round_id") |> 
    left_join(match_maps, by = "match_map_id") |> 
    distinct(match_map_id, hero_name, role, team) |> 
    group_by(team, hero_name, role) |> 
    summarize(
      appearances_by_team = n()) |> 
    left_join(maps_played_per_team, by = "team") |> 
    mutate(pickrate_by_team = appearances_by_team/total_games, total_games = NULL) |> 
    arrange(desc(pickrate_by_team)) |> 
    ungroup() |> 
    left_join(teams, by = c("team" = "team_id")) |> 
    mutate(region = NULL) |> 
    filter(appearances_by_team > min_appearances)
  
  pickrates_per_team <- pickrates_per_team |> select(-`team`) 
  pickrates_per_team <- pickrates_per_team[, c(5,1,2,3,4)]
  
  return(pickrates_per_team)
}

compare_teams_to_avg_pickrate <- function(team_name){
  pickrates_teams <- get_pickrates_by_team()
  pickrates_avg <- get_pickrates()
  result <- data.frame()
  
  for(team_name_temp in unique(pickrates_teams$team_name)){
    comparison <- pickrates_teams |>
      filter(team_name == {{team_name_temp}}) |> 
      left_join(pickrates_avg, by = "hero_name") |> 
      mutate(pickrate_diff = pickrate_by_team - pickrate,
             role.x = NULL)
    result <- bind_rows(result, comparison) |> 
      arrange(desc(pickrate_diff)) |> 
      select(-appearances_by_team, -appearances)
    
    result <- result[,c(1,2,4,3,5,6)] 
  }

  return(result)
}

