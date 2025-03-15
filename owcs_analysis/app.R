#
# This is a Shiny web application. You can run the application by clicking the
# 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# https://shiny.posit.co/
# 
# TODO: Update filtered matches to reflect modes
# TODO: add filtered matches to composition and ban pages
library(shiny)
library(bslib)
library(tidyverse)
library(DT)

all_data <- hero_composition |> 
  left_join(heroes, by = "hero_id") |> 
  left_join(rounds, by = "round_id") |> 
  left_join(match_maps, by = "match_map_id") |> 
  left_join(matches, by = "match_id") |> 
  left_join(teams, by = c("team" = "team_id")) |> 
  left_join(maps, by = "map_id") |> 
  mutate(iswin = case_when(team == map_win_team_id ~ 1,
                           .default = 0))

all_bans <- bans |> 
  left_join(heroes, by = "hero_id") |> 
  left_join(match_maps, by = "match_map_id") |> 
  left_join(matches, by = "match_id") |> 
  left_join(teams, by = "team_id") |> 
  left_join(maps, by = "map_id") 


# Define UI for application 
#Title and refresh button -----------
ui <- page_fluid(
  title = "OWCS TCAT",
  div(
    style = "position: absolute; top: 10px; right: 10px; z-index: 1000;",
    actionButton("updateDataBtn", "Update Data", 
                 icon = icon("refresh"),
                 class = "btn-primary btn-sm",
                 title = "Update Data")
  ),
  titlePanel("OWCS Team Composition Analysis Tool"),
  
  #Panels
#Overview pannel ----------
  navset_card_tab(
    

    
    nav_panel(
      "Overview",
      layout_sidebar(
        sidebar = sidebar(
          checkboxGroupInput("regionFilterGen", "Region",
                             choices = list("NA"= "north_america", "EMEA" = "emea", "Korea" = "korea"),
                             selected = list("north_america", "emea", "korea")),
          checkboxGroupInput("weekFilterGen", "Week",
                             choices = list("Week 1" = 1, "Week 2" = 2,
                                            "Week 3" = 3, "Week 4" = 4,
                                            "Playoffs"),
                             selected = list(1,2,3,4,"Playoffs")),
          checkboxGroupInput("modeFilterGen", "Modes",
                             choices = list("Control", "Flashpoint", "Push",
                                            "Escort", "Hybrid"),
                             selected = list("Control", "Flashpoint", "Push",
                                             "Escort", "Hybrid")),
          checkboxGroupInput("roleFilterGen", "Roles",
                             choices = list("Tank" = "tank", "Support" = "sup", "DPS" = "dps"),
                             selected = list("tank", "sup", "dps"))
        ),
        card(
          card_header("General Pickrates"),
          card_body(
            dataTableOutput("generalPickrates")
          )
          ),
        card(
          card_header("General Pickrates - Plot"),
          card_body(
            sliderInput("topngeneralPickrates", "Top N heroes to show",
                        min = 1, max = 20, value = 10),
            plotOutput("generalPickratesVis")
          )
        ),
        card(
          card_header("Filtered Matches"),
          card_body(
            dataTableOutput("filteredMatchesGen")
          )
        )
      )
    ),
    
#Team panel ----------
    
    nav_panel(
      "Team Analysis",
      layout_sidebar(
        sidebar = sidebar(
          checkboxGroupInput("regionFilterTeam", "Region",
                             choices = list("NA" = "north_america", "EMEA" = "emea", "Korea" = "korea"),
                             selected = list("north_america", "emea", "korea")),
          selectInput("teamFilterTeam", "Team",
                      choices = team_list),
          checkboxGroupInput("weekFilterTeam", "Week",
                             choices = list("Week 1" = 1, "Week 2" = 2,
                                            "Week 3" = 3, "Week 4" = 4,
                                            "Playoffs"),
                             selected = list(1,2,3,4,"Playoffs")),
          checkboxGroupInput("modeFilterTeam", "Modes",
                             choices = list("Control", "Flashpoint", "Push",
                                            "Escort", "Hybrid"),
                             selected = list("Control", "Flashpoint", "Push",
                                             "Escort", "Hybrid")),
          checkboxGroupInput("roleFilterTeam", "Roles",
                             choices = list("Tank" = "tank", "Support" = "sup", "DPS" = "dps"),
                             selected = list("tank", "sup", "dps"))
          
        ),
        card(
          card_header("Team pickrates"),
          card_body(
            dataTableOutput("teamPickrates")
          )
        ),
        card(
          card_header("Team pickrates - Plot"),
          card_body(
            sliderInput("topnteamPickrates", "Top N heroes to show",
                        min = 1, max = 20, value = 10),
            plotOutput("teamPickratesVis")
          )
        ),
        card(
          card_header("Filtered Matches"),
          card_body(
            dataTableOutput("filteredMatchesTeams")
          )
        )
      )
    ),
    
    
#Map panel ----------
    
    nav_panel(
      "Map Analysis",
      layout_sidebar(
        sidebar = sidebar(
          checkboxGroupInput("regionFilterMaps", "Region",
                             choices = list("NA" = "north_america", "EMEA" = "emea", "Korea" = "korea"),
                             selected = list("north_america", "emea", "korea")),
          selectInput("mapFilterMaps", "Maps",
                      choices = map_list),
          selectInput("teamFilterMaps", "Team",
                      choices = c("All" = "All", team_list)),
          checkboxGroupInput("weekFilterMaps", "Week",
                             choices = list("Week 1" = 1, "Week 2" = 2,
                                            "Week 3" = 3, "Week 4" = 4,
                                            "Playoffs"),
                             selected = list(1,2,3,4,"Playoffs")),
          checkboxGroupInput("roleFilterMaps", "Roles",
                             choices = list("Tank" = "tank", "Support" = "sup", "DPS" = "dps"),
                             selected = list("tank", "sup", "dps"))
          
        ),
        card(
          card_header("Hero pickrates on selected map"),
          card_body(
            dataTableOutput(
              "mapPickrates")
          )
        ),
        card(
          card_header("Hero pickrates on selected map - Plot"),
          sliderInput("topnmapPickrates", "Top N heroes to show",
                      min = 1, max = 20, value = 10),
          plotOutput(
            "mapPickratesVis"
          )
        ),
        card(
          card_header("Filtered Matches"),
          card_body(
            dataTableOutput("filteredMatchesMaps")
          )
        )
      )
    ),

#Composition panel ----------
    nav_panel(
      "Composition Analysis",
      layout_sidebar(
        sidebar = sidebar(
          checkboxGroupInput("regionFilterComp", "Region",
                             choices = list("NA" = "north_america", "EMEA" = "emea", "Korea" = "korea"),
                             selected = list("north_america", "emea", "korea")),
          selectInput("teamFilterComp", "Team",
                      choices = c("All" = "All", team_list)),
          selectInput("mapFilterComp", "Map(s)",
                      choices = c("All" = "All", map_list)),
          checkboxGroupInput("weekFilterComp", "Week",
                             choices = list("Week 1" = 1, "Week 2" = 2,
                                            "Week 3" = 3, "Week 4" = 4,
                                            "Playoffs"),
                             selected = list(1,2,3,4,"Playoffs")),
          checkboxGroupInput("modeFilterComp", "Mode",
                      choices = list("Control", "Flashpoint", "Push",
                                     "Escort", "Hybrid"),
                      selected = list("Control", "Flashpoint", "Push",
                                      "Escort", "Hybrid"))
          ),
        card(
          card_header(
            "Composition counts"
          ),
          card_body(
            checkboxGroupInput("roleSelectionComp", "Roles",
                               choices = list("Tank" = "tank", "DPS" = "dps", "Support" = "sup"),
                               inline = TRUE),
            dataTableOutput("compositions")
            
          )
        ),
        card(
          card_header("Filtered Matches"),
          card_body(
            dataTableOutput("filteredMatchesComp")
          )
        )
    )
  ),
  nav_panel(
    "Ban analysis",
    layout_sidebar(
      sidebar = sidebar(
        
        checkboxGroupInput("regionFilterBan", "Region",
                           choices = list("NA" = "north_america", "EMEA" = "emea", "Korea" = "korea"),
                           selected = list("north_america", "emea", "korea")),
        selectInput("teamFilterBan", "Team",
                    choices = c("All" = "All", team_list)),
        selectInput("mapFilterBan", "Map(s)",
                    choices = c("All" = "All", map_list)),
        checkboxGroupInput("weekFilterBan", "Week",
                           choices = list("Week 1" = 1, "Week 2" = 2,
                                          "Week 3" = 3, "Week 4" = 4,
                                          "Playoffs"),
                           selected = list(1,2,3,4,"Playoffs"))
      ),
      card(
        card_title("Banrates per hero"),
        card_body(
          dataTableOutput("banrates")
        )
      ),
      card(
        card_header("Ban Rates - Plot"),
        card_body(
          sliderInput("topnBanrates", "Top N heroes to show",
                      min = 1, max = 20, value = 10),
          plotOutput("banratesVis")
        )
      ),
      card(
        card_header("Filtered Matches"),
        card_body(
          dataTableOutput("applicableResultsBans")
        )
      )
    )
  ),
nav_item(
  input_dark_mode(id = "dark_mode", mode = "dark")
)

)
)
  
  
  
# Define server logic ----------
server <- function(input, output) {
  
#Refresh data section ----------
  observeEvent(input$updateDataBtn, {
    # Show a modal dialog to indicate data is being updated
    showModal(modalDialog(
      title = "Updating Data",
      "Please wait while the data is being updated...",
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Try to source the update script
    tryCatch({
      source("./scripts/update.R", local = TRUE)
      
      # If successful, reload the data in the current session
      all_data <- hero_composition |> 
        left_join(heroes, by = "hero_id") |> 
        left_join(rounds, by = "round_id") |> 
        left_join(match_maps, by = "match_map_id") |> 
        left_join(matches, by = "match_id") |> 
        left_join(teams, by = c("team" = "team_id")) |> 
        left_join(maps, by = "map_id")
      
      # Close the modal and show success message
      removeModal()
      showNotification("Data successfully updated!", type = "message")
    }, 
    error = function(e) {
      # If there's an error, show the error message
      removeModal()
      showNotification(paste("Error updating data:", e$message), type = "error")
    })
  })
  

# Overview page----------
  
  #Filter logic ----------
  filtered_data_general <- reactive({
    all_data |> 
      filter(week %in% input$weekFilterGen,
             mode %in% input$modeFilterGen,
             role %in% input$roleFilterGen,
             region %in% input$regionFilterGen) |> 
      select(round_id, match_map_id, match_id, hero_name,
             role, map_name, mode, team_name, team, iswin) 
  })
  
  filtered_matches_general <- reactive({
    matches |> 
      left_join(teams, by = c("team1_id" = "team_id")) |> 
      rename(team_1 = team_name) |> 
      left_join(teams, by = c("team2_id" = "team_id")) |> 
      rename(team_2 = team_name) |> 
      rename(region = region.x) |> 
      filter(week %in% input$weekFilterGen, 
             region %in% input$regionFilterGen)
  })
  
  #Calculations ----------
  total_maps_general <- reactive({
    n_distinct(filtered_data_general()$match_map_id)
  })
  
  general_pickrate <- reactive({filtered_data_general() |> 
      distinct(hero_name, match_map_id, role, .keep_all = TRUE) |> 
      group_by(hero_name, role) |> 
      summarise(
        appearances = n(),
        pickrate = appearances/total_maps_general(),
        winrate = mean(iswin)
      ) |>
      filter(appearances > 0) |> 
      arrange(desc(pickrate)) 
  })
  
  # Outputs -----------
  output$generalPickrates <- renderDT({general_pickrate() |> 
      select(hero_name, appearances, pickrate, winrate) |> 
      datatable(
        colnames = c("Hero", "Maps played", "Pickrate", "Winrate"),
        filter = "top",
        options = list(
          searching = TRUE, 
          pageLength = 10,
          autoWidth = TRUE
          )) |> 
      formatPercentage(c("pickrate", "winrate"), digits = 1)
  })
  
  output$generalPickratesVis <- renderPlot({
    general_pickrate() |> 
      mutate(pickrate = pickrate *100) |> 
      head(input$topngeneralPickrates) |> 
      ggplot(aes(x = reorder(hero_name, pickrate), y = pickrate, fill = role)) +
      geom_col() + 
      labs(x = "Hero", y = "Pickrate (%)") + 
      coord_flip() 
  })
  
  output$filteredMatchesGen <- renderDT({
    filtered_matches_general() |> 
    select(team_1, team_2, date, bracket) |> 
      datatable(
        colnames = c("Team 1", "Team 2", "Date", "Bracket"),
      )
  })
  
  # Team page ---------
  # Filter logic ----------
  filtered_data_by_team_filters <- reactive({
  all_data |>
    filter(week %in% input$weekFilterTeam,
           mode %in% input$modeFilterTeam,
           role %in% input$roleFilterTeam,
           region %in% input$regionFilterTeam)
  })
  
  filtered_matches_team <- reactive({
    matches |> 
      left_join(teams, by = c("team1_id" = "team_id")) |> 
      rename(team_1 = team_name) |> 
      left_join(teams, by = c("team2_id" = "team_id")) |> 
      rename(team_2 = team_name) |> 
      rename(region = region.x) |> 
      filter(week %in% input$weekFilterTeam, 
             region %in% input$regionFilterTeam,
             ((team_1 == input$teamFilterTeam) | (team_2 == input$teamFilterTeam))
             )
  })
  # Calculations ----------
  maps_by_team <- reactive({
    filtered_data_by_team_filters() |>
      group_by(team_name) |>
      summarise(total_maps = n_distinct(match_map_id))
  })
  
  hero_usage_by_team <- reactive({
    filtered_data_by_team_filters() |>
      group_by(team_name, hero_name, role) |>
      distinct(match_map_id, .keep_all = TRUE) |> 
      summarise(maps_with_hero = n(),
                wins_with_hero = sum(iswin),
                .groups = "drop")
  })
  
  # Calculate team pickrates for all teams
  team_pickrates <- reactive({
    hero_usage_by_team() |>
      left_join(maps_by_team(), by = "team_name") |>
      mutate(team_pickrate = maps_with_hero / total_maps,
             team_winrate = wins_with_hero / maps_with_hero)
  })
  
  # Calculate weighted average pickrate across teams
  average_team_pickrates <- reactive({
    team_pickrates() |>
      group_by(hero_name, role) |>
      summarise(
        total_weighted_pickrate = sum(team_pickrate * total_maps, na.rm = TRUE),
        total_weights = sum(total_maps, na.rm = TRUE),
        avg_pickrate = total_weighted_pickrate / total_weights,
        .groups = "drop"
      )
  })
  
  # Hero pickrates for the selected team + diff to weighted avg
  pickrate_comparison_team <- reactive({
    # Get the selected team's pickrates
    selected_team_pickrates <- team_pickrates() |>
      filter(team_name == input$teamFilterTeam)
    
    # Join with average pickrates
    selected_team_pickrates |>
      left_join(average_team_pickrates(), by = c("hero_name", "role")) |>
      left_join(general_pickrate(), by = c("hero_name", "role")) |> 
      mutate(pickrate_diff = team_pickrate - avg_pickrate,
             winrate_diff = team_winrate - winrate) |>
      select(hero_name, role, maps_with_hero, team_pickrate, avg_pickrate, pickrate_diff, team_winrate, winrate_diff) |>
      arrange(desc(abs(pickrate_diff)))
  })
  
  # Outputs ----------
  output$teamPickrates <- renderDT({
    pickrate_comparison_team() |>
      select(-role) |>
      datatable(
        colnames = c("Hero", "Maps played",
                     "Team pickrate", "Avg pickrate",
                     "Pickrate difference", "Team winrate", "Winrate diff"),
        filter = "top",
        options = list(
          searching = TRUE, 
          pageLength = 10,
          autoWidth = TRUE
        )) |>
      formatPercentage("team_pickrate", digits = 1) |>
      formatPercentage("avg_pickrate", digits = 1) |>
      formatPercentage("pickrate_diff", digits = 1) |> 
      formatPercentage("team_winrate", digits = 1) |> 
      formatPercentage("winrate_diff", digits = 1)
    
    
  })
  
  output$teamPickratesVis <- renderPlot({
    pickrate_comparison_team() |>
      head(input$topnteamPickrates) |>
      mutate(pickrate_diff = pickrate_diff * 100) |>
      ggplot(aes(x = reorder(hero_name, abs(pickrate_diff)), y = pickrate_diff, fill = pickrate_diff > 0)) +
      geom_col() +
      scale_fill_manual(values = c("#FF9E7A", "#7AB8FF"), 
                        labels = c("Below Average", "Above Average"),
                        name = "Pickrate") +
      labs(x = "Hero", y = "Pickrate difference (pp)") +
      coord_flip()
  })
  
  output$filteredMatchesTeams <- renderDT({
    filtered_matches_team() |> 
      select(team_1, team_2, date, bracket) |> 
      datatable(
        colnames = c("Team 1", "Team 2", "Date", "Bracket"),
      )
  })
  
  # Map page ----------
  
  # Filter logic ----------
  filtered_data_by_maps <- reactive({
    filtered_data <- all_data |> 
      filter(week %in% input$weekFilterMaps,
             role %in% input$roleFilterMaps,
             region %in% input$regionFilterMaps)
    
    if(input$teamFilterMaps != "All"){
      filtered_data <- filtered_data |> filter(team_name == input$teamFilterMaps)
    } 
    
  
    
    return(filtered_data)
  })
  
  filtered_matches_maps <- reactive({
    filtered_matches <- matches |> 
      left_join(teams, by = c("team1_id" = "team_id")) |> 
      rename(team_1 = team_name) |> 
      left_join(teams, by = c("team2_id" = "team_id")) |> 
      rename(team_2 = team_name) |> 
      rename(region = region.x) |> 
      filter(week %in% input$weekFilterMaps, 
             region %in% input$regionFilterMaps)
    
    if(input$teamFilterMaps != "All"){
      filtered_matches <- filtered_matches |> 
        filter(
          ((team_1 == input$teamFilterMaps) | (team_2 == input$teamFilterMaps))
        )
    }
    
    selected_map_id <- maps |> 
      filter(map_name == input$mapFilterMaps) |> 
      pull(map_id)
    
    
    filtered_match_ids <- filtered_matches |> 
      right_join(match_maps, by = "match_id") |> 
      filter(map_id == selected_map_id) |> 
      select(match_id) |> 
      distinct()
    
    filtered_matches <- filtered_matches |> 
      filter(match_id %in% filtered_match_ids$match_id)
    
    return(filtered_matches)
  })
  
  # Calculations -----------
  
  teams_in_region <- reactive({
    # Get region filter from input
    selected_regions <- input$regionFilterTeam
    
    # Filter teams based on selected regions
    filtered_teams <- teams %>%
      filter(region %in% selected_regions) %>%
      pull(team_name)
    
    # Return as a named list for selectInput
    setNames(as.list(filtered_teams), filtered_teams)
  })
  
  observeEvent(input$regionFilterTeam, {
    # Get filtered teams
    teams_list <- teams_in_region()
    
    # Handle case when no teams match (provide a placeholder)
    if(length(teams_list) == 0) {
      teams_list <- list("No teams available" = "")
    }
    
    # Update the select input
    updateSelectInput(
      inputId = "teamFilterMaps",
      choices = c("All" = "All", teams_list),
      # Try to maintain current selection if it's still valid
      selected = if(input$teamFilterMaps %in% names(teams_list)) input$teamFilterMaps else NULL
    )
  })
  
  total_n_played <- reactive({
    filtered_data_by_maps() |>
      group_by(map_name) |>
      summarise(n_played = n_distinct(match_map_id)) 
  })
  
  total_pickrates_for_maps <- reactive({
    filtered_data_by_maps() |> 
      group_by(map_name, hero_name, role) |> 
      summarize(all_appearances= n(),
                hero_played = n_distinct(match_map_id),
                hero_wins = sum(iswin),
                .groups = "drop") |> 
      left_join(total_n_played(), by = "map_name") |>
      mutate(pickrate = hero_played / n_played,
             winrate = hero_wins / all_appearances) |> 
      arrange(desc(pickrate))
  })
  
  # Outputs -----------
  
  output$mapPickrates <- renderDT({
    total_pickrates_for_maps() |> 
      filter(map_name == input$mapFilterMaps) |> 
      select(hero_name, hero_played, pickrate, winrate) |> 
      datatable(
        colnames = c("Hero", "Appearances on map",
                     "Pickrate", "Winrate"),
        filter = "top",
        options = list(
          searching = TRUE, 
          pageLength = 10,
          autoWidth = TRUE
        )) |>
        formatPercentage("pickrate", digits = 1) |> 
        formatPercentage("winrate", digits = 1)
      })
  
  output$mapPickratesVis <- renderPlot({
    total_pickrates_for_maps() |> 
      filter(map_name == input$mapFilterMaps) |> 
      mutate(pickrate = pickrate) |> 
      head(input$topnmapPickrates) |> 
      ggplot(aes(x = reorder(hero_name, pickrate), y = pickrate, fill = role)) +
      geom_col() + 
      labs(x = "Hero", y = "Pickrate (%)") + 
      coord_flip() 
  })
  
  output$filteredMatchesMaps <- renderDT({
    filtered_matches_maps() |> 
      select(team_1, team_2, date, bracket) |> 
      datatable(
        colnames = c("Team 1", "Team 2", "Date", "Bracket"),
      )
  })

  # Composition page ----------
  
  #Filter logic ----------
  
  filtered_data_composition <- reactive({
    filtered_data <- all_data |> 
      filter(week %in% input$weekFilterComp,
             mode %in% input$modeFilterComp,
             region %in% input$regionFilterComp) |> 
      select(round_id, match_map_id, match_id, hero_name,
             role, map_name, mode, team_name) 
    
    if(input$teamFilterComp != "All"){
      filtered_data <- filtered_data |> filter(team_name == input$teamFilterComp)
    } 
      
    if(input$mapFilterComp != "All"){
      filtered_data <- filtered_data |> filter(map_name == input$mapFilterComp)
    }
    
    return(filtered_data)
  })
  
  filtered_matches_comp <- reactive({
    filtered_matches <- matches |> 
      left_join(teams, by = c("team1_id" = "team_id")) |> 
      rename(team_1 = team_name) |> 
      left_join(teams, by = c("team2_id" = "team_id")) |> 
      rename(team_2 = team_name) |> 
      rename(region = region.x) |> 
      filter(week %in% input$weekFilterComp, 
             region %in% input$regionFilterComp)
    
    if(input$teamFilterComp != "All"){
      filtered_matches <- filtered_matches |> 
        filter(
          ((team_1 == input$teamFilterComp) | (team_2 == input$teamFilterComp))
        )
    }
    
    if(input$mapFilterComp != "All"){
      selected_map_id <- maps |> 
        filter(input$mapFilterComp == map_name) |> 
        pull(map_id)
    }
    
    filtered_match_ids <- filtered_matches |> 
      right_join(match_maps, by = "match_id") 

    if(input$mapFilterComp != "All"){
      filtered_match_ids <- filtered_matches |> 
        filter(map_id == selected_map_id$map_id)
    }
    
    filtered_match_ids <- filtered_match_ids |> 
      select(match_id) |> 
      distinct()
    
    
    filtered_matches <- filtered_matches |> 
      filter(match_id %in% filtered_match_ids$match_id)
    
    return(filtered_matches)
  })
  
  
  #Calculations ----------
  
  teams_in_region <- reactive({
    # Get region filter from input
    selected_regions <- input$regionFilterComp
    
    # Filter teams based on selected regions
    filtered_teams <- teams %>%
      filter(region %in% selected_regions) %>%
      pull(team_name)
    
    # Return as a named list for selectInput
    setNames(as.list(filtered_teams), filtered_teams)
  })
  
  observeEvent(input$regionFilterComp, {
    # Get filtered teams
    teams_list <- teams_in_region()
    
    # Handle case when no teams match (provide a placeholder)
    if(length(teams_list) == 0) {
      teams_list <- list("No teams available" = "")
    }
    
    # Update the select input
    updateSelectInput(
      inputId = "teamFilterComp",
      choices = c("All" = "All",teams_list),
      # Try to maintain current selection if it's still valid
      selected = if(input$teamFilterComp %in% names(teams_list)) input$teamFilterComp else NULL
    )
  })
  
  compositions <- reactive({
    filtered_data_composition() |> 
      group_by(match_map_id, round_id, team_name) |> 
      reframe(
        tank = hero_name[role == "tank"],
        dps = paste(head(sort(unique(hero_name[role == "dps"])), 2), collapse = ", "),
        sup = paste(head(sort(unique(hero_name[role == "sup"])), 2), collapse = ", ")
      )
  })
  
  composition_counts <- reactive({
    compositions() |> 
      count(!!!syms(input$roleSelectionComp), name = "n_played") 
  })
  
  output$compositions <- renderDT(
    datatable(composition_counts() |> arrange(desc(n_played)),
              colnames = c(input$roleSelectionComp, "Rounds played"),
              options = list(
                searching = TRUE, 
                pageLength = 10,
                autoWidth = TRUE
              )) 
  )
  
  output$filteredMatchesComp <- renderDT({
    filtered_matches_comp() |> 
      select(team_1, team_2, date, bracket) |> 
      datatable(
        colnames = c("Team 1", "Team 2", "Date", "Bracket"),
      )
  })
  
  
  #Ban page ---------
  
  #Filter logic----------
  
  filtered_ban_data <- reactive({
    filtered_data <- all_bans |> filter(
      week %in% input$weekFilterBan,
      region %in% input$regionFilterBan
    )
    
    if(input$teamFilterBan != "All"){
      filtered_data <- filtered_data |> filter(team_name == input$teamFilterBan)
    } 
    
    if(input$mapFilterBan != "All"){
      filtered_data <- filtered_data |> filter(map_name == input$mapFilterBan)
    }
    return(filtered_data)
  })
  
  # update available teams by region
  
  teams_in_region <- reactive({
    # Get region filter from input
    selected_regions <- input$regionFilterBan
    
    # Filter teams based on selected regions
    filtered_teams <- teams %>%
      filter(region %in% selected_regions) %>%
      pull(team_name)
    
    # Return as a named list for selectInput
    setNames(as.list(filtered_teams), filtered_teams)
  })
  observeEvent(input$regionFilterBan, {
    # Get filtered teams
    teams_list <- teams_in_region()
    
    # Handle case when no teams match (provide a placeholder)
    if(length(teams_list) == 0) {
      teams_list <- list("No teams available" = "")
    }
    
    # Update the select input
    updateSelectInput(
      inputId = "teamFilterBan",
      choices = c("All" = "All",teams_list),
      # Try to maintain current selection if it's still valid
      selected = if(input$teamFilterBan %in% names(teams_list)) input$teamFilterBan else NULL
    )
  })
  
  total_maps_ban <- reactive({
    n_distinct(filtered_ban_data()$match_map_id)
    })
  
  banrates <- reactive({
    filtered_ban_data() |>
      group_by(hero_name) |> 
      summarize(
        first_ban = sum(first_bool == TRUE),
        second_ban = sum(first_bool == FALSE),
        total_ban = n(),
        first_ban_rate = first_ban / total_maps_ban(),
        second_ban_rate = second_ban / total_maps_ban(),
        total_ban_rate = total_ban / total_maps_ban()
      ) |> 
      arrange(desc(total_ban))
  })
  
  #outputs ---------
  
  output$banrates <- renderDT(
    banrates() |> 
      datatable(
        colnames = c("Hero", "N first bans", "N follow-up bans", "N total bans",
                     "Rate first bans", "Rate second bans", "Total rate"),
        options = list(
          searching = TRUE, 
          pageLength = 10,
          autoWidth = TRUE
        )
      ) |> 
      formatPercentage(c("first_ban_rate", "second_ban_rate", "total_ban_rate"), digits = 1)
    
  )
  
  output$banratesVis <- renderPlot({
    # Prepare data in long format
    ban_data_long <- banrates() |>
      head(input$topnBanrates) |>
      select(hero_name, first_ban_rate, second_ban_rate) |>
      pivot_longer(
        cols = c(first_ban_rate, second_ban_rate),
        names_to = "ban_type",
        values_to = "ban_rate"
      ) |>
      mutate(
        ban_type = case_when(
          ban_type == "first_ban_rate" ~ "First Ban",
          ban_type == "second_ban_rate" ~ "Second Ban"
        ),
        hero_name = reorder(hero_name, ban_rate, sum),
        ban_type = factor(ban_type, levels = c("Second Ban", "First Ban"))
      )
    
    # Create the stacked bar chart
    ggplot(ban_data_long, aes(x = hero_name, y = ban_rate, fill = ban_type)) +
      geom_col() +
      scale_fill_manual(
        values = c("First Ban" = "#3A85C7", "Second Ban" = "#5AAAE0"),
        name = "Ban Type"
      )  +
      labs(
        x = NULL,
        y = "Ban Rate",
        title = "Hero Ban Rates"
      ) +
      coord_flip() +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent) +
      guides(fill = guide_legend(reverse = TRUE))
  })
  
}
  
  
# Run the application -----------
shinyApp(ui = ui, server = server)
