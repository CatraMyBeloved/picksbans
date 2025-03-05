#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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
  left_join(maps, by = "map_id") 

# Define UI for application 
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
  
  navset_card_tab(
    
    #Overview pannel
    
    nav_panel(
      "Overview",
      layout_sidebar(
        sidebar = sidebar(
          helpText("Select criteria to filter composition data."),
          checkboxGroupInput("weekFilterGen", "Week",
                             choices = list("Week 1" = 1, "Week 2" = 2,
                                            "Week 3" = 3, "Week 4" = 4),
                             selected = list(1,2,3,4)),
          checkboxGroupInput("regionFilterGen", "Region",
                             choices = list("NA"= "na", "EMEA" = "emea", "Korea" = "kr"),
                             selected = list("na", "emea", "kr")),
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
        )
      )
    ),
    
    # Team panel
    
    nav_panel(
      "Team Analysis",
      layout_sidebar(
        sidebar = sidebar(
          helpText("Select criteria to filter composition data."),
          checkboxGroupInput("weekFilterTeam", "Week",
                             choices = list("Week 1" = 1, "Week 2" = 2,
                                            "Week 3" = 3, "Week 4" = 4),
                             selected = list(1,2,3,4)),
          checkboxGroupInput("regionFilterTeam", "Region",
                             choices = list("NA" = "na", "EMEA" = "emea", "Korea" = "kr"),
                             selected = list("na", "emea", "kr")),
          selectInput("teamFilterTeam", "Team",
                      choices = team_list),
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
        )

      )
    ),
    
    #Map panel
    
    nav_panel(
      "Map Analysis",
      layout_sidebar(
        sidebar = sidebar(
          helpText("Select criteria to filter composition data."),
          checkboxGroupInput("weekFilterMaps", "Week",
                             choices = list("Week 1" = 1, "Week 2" = 2,
                                            "Week 3" = 3, "Week 4" = 4),
                             selected = list(1,2,3,4)),
          checkboxGroupInput("regionFilterMaps", "Region",
                             choices = list("NA" = "na", "EMEA" = "emea", "Korea" = "kr"),
                             selected = list("na", "emea", "kr")),
          selectInput("mapFilterMaps", "Maps",
                             choices = map_list),
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
        )
      )
    ),
    
    nav_panel(
      "Composition Analysis",
      layout_sidebar(
        sidebar = sidebar(
          helpText("Select criteria to filter composition data."),
          checkboxGroupInput("weekFilterComp", "Week",
                             choices = list("Week 1" = 1, "Week 2" = 2,
                                            "Week 3" = 3, "Week 4" = 4),
                             selected = list(1,2,3,4)),
          
          checkboxGroupInput("regionFilterComp", "Region",
                             choices = list("NA" = "na", "EMEA" = "emea", "Korea" = "kr"),
                             selected = list("na", "emea", "kr")),
          
          selectInput("modeFilterComp", "Mode",
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
        )
    )
  )
)
)
  
  
  
# Define server logic 
server <- function(input, output) {
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
      source("./R/update.R", local = TRUE)
      
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
  
  # Overview page
  filtered_data_general <- reactive({
    all_data |> 
      filter(week %in% as.integer(input$weekFilterGen),
             mode %in% input$modeFilterGen,
             role %in% input$roleFilterGen) |> 
      select(round_id, match_map_id, match_id, hero_name,
             role, map_name, mode, team_name) 
  })
  
  total_maps_general <- reactive({
    n_distinct(filtered_data_general()$match_map_id)
  })
  
  general_pickrate <- reactive({filtered_data_general() |> 
      distinct(hero_name, match_map_id, role) |> 
      group_by(hero_name, role) |> 
      summarise(
        appearances = n(),
        pickrate = appearances/total_maps_general()
      ) |>
      filter(appearances > 0) |> 
      arrange(desc(pickrate)) 
  })
  
  output$generalPickrates <- renderDT({general_pickrate() |> 
      select(hero_name, appearances, pickrate) |> 
      datatable(
        colnames = c("Hero", "Maps played", "Pickrate"),
        filter = "top",
        options = list(
          searching = TRUE, 
          pageLength = 10,
          autoWidth = TRUE
          )) |> 
      formatPercentage("pickrate", digits = 1)
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
  
  # Team page
  # Calculate filtered data for each team
  filtered_data_by_team_filters <- reactive({
    all_data |>
      filter(week %in% as.integer(input$weekFilterTeam),
             mode %in% input$modeFilterTeam,
             role %in% input$roleFilterTeam)
  })
  
  maps_by_team <- reactive({
    filtered_data_by_team_filters() |>
      group_by(team_name) |>
      summarise(total_maps = n_distinct(match_map_id))
  })
  
  hero_usage_by_team <- reactive({
    filtered_data_by_team_filters() |>
      group_by(team_name, hero_name, role) |>
      summarise(maps_with_hero = n_distinct(match_map_id),
                .groups = "drop")
  })
  
  # Calculate team pickrates for all teams
  team_pickrates <- reactive({
    hero_usage_by_team() |>
      left_join(maps_by_team(), by = "team_name") |>
      mutate(team_pickrate = maps_with_hero / total_maps)
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
      mutate(pickrate_diff = team_pickrate - avg_pickrate) |>
      select(hero_name, role, maps_with_hero, team_pickrate, avg_pickrate, pickrate_diff) |>
      arrange(desc(abs(pickrate_diff)))
  })
  
  output$teamPickrates <- renderDT({
    pickrate_comparison_team() |>
      select(-role) |>
      datatable(
        colnames = c("Hero", "Maps played",
                     "Team pickrate", "Avg pickrate",
                     "Pickrate difference"),
        filter = "top",
        options = list(
          searching = TRUE, 
          pageLength = 10,
          autoWidth = TRUE
        )) |>
      formatPercentage("team_pickrate", digits = 1) |>
      formatPercentage("avg_pickrate", digits = 1) |>
      formatPercentage("pickrate_diff", digits = 1)
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
  
  #Map page
  filtered_data_by_maps <- reactive({
    all_data |> 
      filter(week %in% as.integer(input$weekFilterMaps),
             role %in% input$roleFilterMaps)
  })
  
  total_n_maps <- reactive({
    filtered_data_by_maps() |> 
      group_by(map_name) |> 
      summarize(n_played = n_distinct(match_map_id)) |> 
      ungroup()
    
  })
  
  map_pickrates <- reactive({
    filtered_data_by_maps() |> 
    distinct(map_name, hero_name, role, match_map_id) |> 
    group_by(map_name, hero_name, role) |> 
      summarise(
        appearances = n()
      ) |> 
      left_join(total_n_maps(), by = "map_name") |> 
      mutate(pickrate = appearances/n_played) |> 
      ungroup() |> 
      select(hero_name, map_name, role, appearances, pickrate) |> 
      arrange(desc(pickrate))
  })
  
  output$mapPickrates <- renderDT({
    map_pickrates() |> 
      filter(map_name == input$mapFilterMaps) |> 
      select(-role, -map_name) |> 
      datatable(
        colnames = c("Hero", "Appearances on map",
                     "Pickrate"),
        filter = "top",
        options = list(
          searching = TRUE, 
          pageLength = 10,
          autoWidth = TRUE
        )) |>
        formatPercentage("pickrate", digits = 1)
      })
  
  output$mapPickratesVis <- renderPlot({
    map_pickrates() |> 
      filter(map_name == input$mapFilterMaps) |> 
      mutate(pickrate = pickrate) |> 
      head(input$topnmapPickrates) |> 
      ggplot(aes(x = reorder(hero_name, pickrate), y = pickrate, fill = role)) +
      geom_col() + 
      labs(x = "Hero", y = "Pickrate (%)") + 
      coord_flip() 
  })
  
  #Composition page
  
  filtered_data_composition <- reactive({
    all_data |> 
      filter(week %in% as.integer(input$weekFilterComp),
             mode %in% input$modeFilterComp) |> 
      mutate(unique_round_id = paste(match_map_id, round_id, name, sep = "_")) |>
      select(round_id, match_map_id, match_id, hero_name,
             role, map_name, mode, team_name, unique_round_id) 
  })
  
  compositions <- reactive({
    filtered_data_composition() |> 
      group_by(match_map_id, unique_round_id, team_name) |> 
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
