library(tidyverse)
library(rvest)
library(glue)

settings <- list(
  teams = 12,
  qbs = 1,
  wrs = 2,
  rbs = 2,
  tes = 1,
  passing_yards = 25,
  passing_td = 6,
  int = -2,
  rushing_yards = 10,
  rushing_td = 6,
  reception = 1,
  receiving_yards = 10,
  receiving_td = 6,
  return_td = 6,
  two_pt = 2,
  fmbl = -2,
  off_fmbl_return_td = 6
)

scrape_season_html <- function(season = 2019) {
  url = glue("https://www.pro-football-reference.com/years/{season}/fantasy.htm#fantasy::none", sep = "")
  season_stats <- read_html(url)
  
  return(season_stats)
}

get_column_names <- function(tbl) {
  cols <- tbl %>%
    slice(1:2) %>%
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from = value) %>%
    unite(`1`, `2`, col = "col_names", sep = "_") %>%
    select(col_names)
  
  cols <- cols$col_names %>%
    str_extract("[A-Za-z]+\\_?[A-Za-z0-9]*")
  
  return(cols)
}

clean_names <- function(names) {
  return(
    names %>%
      str_extract("[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+")
  )
}

format_season <- function(season_html) {
  season_l <- season_html %>%
    html_nodes("table") %>%
    html_table(header = FALSE)
  
  season_tbl <- season_l[[1]]
  colnames(season_tbl) <- season_tbl %>% get_column_names
  season_tbl <- season_tbl %>% 
    slice(3:n()) %>%
    filter(Player != "Player")
  
  season_tbl$Player <- season_tbl$Player %>% clean_names
  
  
  return(season_tbl)
}

calculate_league_scoring <- function(season, settings) {
  season$league_score <- season$Passing_Yds / settings$passing_yards +
        season$Passing_TD * settings$passing_td +
        season$Passing_Int * settings$int +
        season$Rushing_Yds / settings$rushing_yards +
        season$Rushing_TD * settings$rushing_td +
        season$Receiving_Rec * settings$reception +
        season$Receiving_Yds / settings$receiving_yards +
        season$Receiving_TD * settings$receiving_td +
        season$Fumbles_FL * settings$fmbl +
        season$Scoring_2PM * settings$two_pt +
        season$Scoring_2PP * settings$two_pt      
  
  return(season)
}

calculate_league_vbd <- function(season, settings) {
  rep_qb <- season %>% 
    filter(FantPos == 'QB' & league_pos_rank == settings$teams * settings$qbs)
  
  rep_rb <- season %>% 
    filter(FantPos == 'RB' & league_pos_rank == settings$teams * settings$rbs)
  
  rep_wr <- season %>% 
    filter(FantPos == 'WR' & league_pos_rank == settings$teams * settings$wrs)
  
  rep_te <- season %>% 
    filter(FantPos == 'TE' & league_pos_rank == settings$teams * settings$tes)
  
  season_qb <- season %>%
    filter(FantPos == 'QB') %>%
    mutate(
      league_vbd = league_score - rep_qb$league_score
    )
  
  season_rb <- season %>%
    filter(FantPos == 'RB') %>%
    mutate(
      league_vbd = league_score - rep_rb$league_score
    )
    
  season_wr <- season %>%
    filter(FantPos == 'WR') %>%
    mutate(
      league_vbd = league_score - rep_wr$league_score
    )
  
  season_te <- season %>%
    filter(FantPos == 'TE') %>%
    mutate(
      league_vbd = league_score - rep_te$league_score
    )
  
  season <- season_qb %>%
    bind_rows(season_rb, season_wr, season_te)
  
  return(season)
}

calculate_league_rank <- function(season) {
  season <- season %>%
    mutate(
      league_rank = row_number(desc(league_score))
    )
  
  season <- season %>%
    group_by(FantPos) %>%
    mutate(
      league_pos_rank = row_number(desc(league_score))
    )
  
  return(season)
}

scrape_season <- function(season = 2019) {
  season_data <- scrape_season_html(season) %>%
    format_season %>%
    type_convert %>%
    mutate(
      across(everything(), ~replace_na(.x, 0))
    ) %>%
    calculate_league_scoring(settings) %>%
    calculate_league_rank %>%
    calculate_league_vbd(settings)
  
  return(season_data)
}

season <- scrape_season(2019)

