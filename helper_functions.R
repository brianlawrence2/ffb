library(tidyverse)

scrape_season_html <- function(season = 2019) {
  url = str_c("https://www.pro-football-reference.com/years/", season, "/fantasy.htm#fantasy::none", sep = "")
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
  
  return(cols)
}

format_season <- function(season_html) {
  season_l <- season_html %>%
    html_nodes("table") %>%
    html_table(header = FALSE)
  
  season_tbl <- season_l[[1]]
  col_names(season_tbl) <- get_column_names(season_tbl)
  season_tbl <- season_tbl %>% slice(3:n())
  
  return(season_tbl)
}

scrape_season <- function(season = 2019) {
  return(
    scrape_season_html(season) %>%
      format_season
  )
}
