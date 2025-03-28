# app.R

# Load required libraries
library(shiny)
library(bslib)
library(tidyverse)
library(DT)

# Source helper functions
source("R/prepare.R")  # This will load your data and create necessary lists

# Load all UI and server modules
source_files <- function(dir) {
  files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
  lapply(files, source)
}

# Load UI modules
source_files("R/ui")

# Load server modules
source_files("R/server")

# Load analysis functions
source_files("R/analysis")

# Prepare data for the app
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
  observeEvent(input$updateDataBtn, {
    showModal(modalDialog(
      title = "Updating Data",
      "Please wait while the data is being updated...",
      footer = NULL,
      easyClose = FALSE
    ))
    
    tryCatch({
      source("./scripts/update.R", local = TRUE)
      
      # Reload the data
      source("R/prepare.R", local = TRUE)
      
      # Update the joined data
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
  
  # Call each server module, passing the data
  overview_server("overview", all_data)
  team_server("team", all_data)
  map_server("map", all_data)
  server_composition("composition", all_data)
  ban_server("ban", all_bans)
}

# Run the application
shinyApp(ui = ui, server = server)