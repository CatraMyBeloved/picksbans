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


