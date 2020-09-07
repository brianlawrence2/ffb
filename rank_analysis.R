library(tidyverse)
source("helper_functions.R")

seasons_l <- list(2015, 2016, 2017, 2018, 2019)

seasons <- seasons_l %>%
  map(scrape_season) %>%
  bind_rows()

position_vbd <- seasons %>%
  group_by(FantPos, league_pos_rank) %>%
  mutate(
    pos_rank_vbd = mean(league_vbd),
    pos_rank_avg_score = mean(league_score)
  ) %>%
  select(FantPos, league_pos_rank, pos_rank_vbd, pos_rank_avg_score) %>%
  distinct()

ggplot(position_vbd, aes(league_pos_rank, pos_rank_vbd)) +
  geom_point() +
  facet_wrap(vars(FantPos))





calc_vbd <- function(pos, rank, df) {
  rep = df %>% filter(FantPos == pos & league_pos_rank == rank)
  
  df <- df %>%
    filter(FantPos == pos) %>%
    mutate(
      vbd = pos_rank_avg_score - rep$pos_rank_avg_score
    )
  
  return(df)
}

pos = c('QB', 'RB', 'WR', 'TE')
rank = c(12, 24, 24, 12)

vbd <- map2(pos, rank, ~calc_vbd(.x, .y, position_vbd)) %>% bind_rows()



