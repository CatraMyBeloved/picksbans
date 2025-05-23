# app.R

# Load required libraries
library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(rdrop2)


#Load all R files in a folder, in order to load all modules
source_files <- function(dir) {
  files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
  lapply(files, source)
}


app_data <- reactiveValues(
  heroes = heroes,
  maps = maps,
  matches = matches,
  match_maps = match_maps,
  rounds = rounds,
  teams = teams,
  hero_composition = hero_composition,
  bans = bans
)


all_data_reactive <- reactive({
  app_data$hero_composition |> 
    left_join(app_data$heroes, by = "hero_id") |> 
    left_join(app_data$rounds, by = "round_id") |> 
    left_join(app_data$match_maps, by = "match_map_id") |> 
    left_join(app_data$matches, by = "match_id") |> 
    left_join(app_data$teams, by = c("team" = "team_id")) |> 
    left_join(app_data$maps, by = "map_id") |> 
    mutate(iswin = case_when(team == map_win_team_id ~ 1,
                             .default = 0))
})

all_bans_reactive <- reactive({
  app_data$bans |> 
    left_join(app_data$heroes, by = "hero_id") |> 
    left_join(app_data$match_maps, by = "match_map_id") |> 
    left_join(app_data$matches, by = "match_id") |> 
    left_join(app_data$teams, by = "team_id") |> 
    left_join(app_data$maps, by = "map_id") 
})



# Load UI modules
source_files("R/ui")

# Load server modules
source_files("R/server")

# Load analysis functions
source_files("R/analysis")

# Prepare data for the app
build_all_data()

# Define UI
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
  
  navset_card_tab(
    # Call each UI module
    overview_ui("overview"),
    team_ui("team"),
    map_ui("map"),
    composition_ui("composition"),
    ban_ui("ban"),
    
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "dark")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Update data logic
  # When button is pressed, start dialog and begin update logic
  observeEvent(input$updateDataBtn, {
    showModal(modalDialog(
      title = "Updating Data",
      "Please wait while the data is being updated...",
      footer = NULL,
      easyClose = FALSE
    ))
    
    download_success <- download_data()
    if (!download_success) {
      removeModal()
      showNotification("Failed to download current data", type = "error")
      return()
    }
    
    # Update the data
    tryCatch({
      source("./scripts/update.R", local = TRUE)
      
      # Check whether there are new matches in the data
      if(identical(matches, new_matches)){
        
        removeModal()
        showNotification("No new data available!", type = "message")
        return()
        
      } else {
        # Move downloaded data into reactive storage
        appData$hero_composition <- new_hero_composition
        appData$heroes <- new_heroes
        appData$maps <- new_maps
        appData$match_maps <- new_match_maps
        appData$matches <- new_matches
        appData$rounds <- new_rounds
        appData$teams <- new_teams
        appData$bans <- new_bans
      }
      
      upload_success <- upload_data()
      
      if (upload_success) {
        
        removeModal()
        showNotification("Data successfully updated!", type = "message")
        
      } else {
        
        removeModal()
        showNotification("Failed to upload updated data", type = "error")
        
      }
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error updating data:", e$message), type = "error")
    })
  })
  
  # Call each server module, passing the data
  overview_server("overview", all_data_reactive())
  team_server("team", all_data_reactive())
  map_server("map", all_data_reactive())
  server_composition("composition", all_data_reactive())
  ban_server("ban", all_bans_reactive())
}

# Run the application
shinyApp(ui = ui, server = server)