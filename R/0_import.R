library(rvest)
library(tidyverse)
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRTkWc3-NK0HtsOjipS8cqYMJRTddCSgecM-KD4wbNSpdjDJcpaYt-E-N8jU6j13CefwHGZ3I22SV-x/pubhtml"

data <- url %>% read_html() %>% html_table()

column_names <- c("vod_link", "patch_date", "bracket",
                  "week", "date", "map", "t1_round",
                  "t1_team", "t1_result", "vs", "t2_result",
                  "t2_team", "t2_round", "first_ban_team",
                  "first_ban_hero", "second_ban_team",
                  "second_ban_hero", "t1_tank", "t1_dps1",
                  "t1_dps2", "t1_sup1", "t1_sup2", "t2_tank",
                  "t2_dps1", "t2_dps2", "t2_sup1", "t2_sup2", "notes")

clean_table <- function(table, col_names){
  table <- table[3:nrow(table), c(2, 4:29)]
  names(table) <- col_names
  table <- table[!is.na(table$vod_link) & table$vod_link != "", ]
  table
}

cleaned_data <- lapply(data, clean_table, col_names = column_names)

cleaned_data[[1]] <- cleaned_data[[1]] |> mutate(region = "korea")
cleaned_data[[2]] <- cleaned_data[[2]] |> mutate(region = "emea")
cleaned_data[[3]] <- cleaned_data[[3]] |> mutate(region = "north_america")

combined_data <- bind_rows(cleaned_data)

write.csv(combined_data, "./data/origin.csv")

