library(ffanalytics)

my_scrape <- scrape_data(src = c("CBS", "ESPN", "FantasyData", "FantasyPros", "FFToday",
                                 "FleaFlicker", "NumberFire", "Yahoo", "FantasyFootballNerd", "NFL",
                                 "RTSports"), 
                         pos = c("QB", "RB", "WR", "TE", "DST"),
                         season = 2020, week = 0)

### QB ###

QB <- my_scrape$QB %>%
  group_by(id) %>%
  mutate(
    pass_att = mean(pass_att, na.rm = TRUE),
    pass_comp = mean(pass_comp, na.rm = TRUE),
    pass_yds = mean(pass_yds, na.rm = TRUE),
    pass_tds = mean(pass_tds, na.rm = TRUE),
    pass_int = mean(pass_int, na.rm = TRUE),
    rush_yds = mean(rush_yds, na.rm = TRUE),
    rush_tds = mean(rush_tds, na.rm = TRUE),
    fumbles_lost = mean(fumbles_lost, na.rm = TRUE)
  ) %>%
  select(id, player, pass_att, pass_comp, pass_yds, pass_tds, pass_int, rush_yds, rush_tds, fumbles_lost) %>%
  distinct()

QB$position <- 'QB'

QB$points <- QB$pass_yds / settings$passing_yards +
  QB$pass_tds * settings$passing_td +
  QB$pass_int * settings$int +
  QB$rush_yds / settings$rushing_yards +
  QB$rush_tds * settings$rushing_td +
  QB$fumbles_lost * settings$fmbl


QB <- QB %>%
  group_by(position) %>%
  mutate(
    pos_rank = row_number(desc(points) == 1L)
  ) %>%
  arrange(desc(points))

rep <- QB %>% filter(pos_rank == 12)

QB <- QB %>%
  mutate(
    vbd = points - rep$points
  )

QB <- QB %>%
  inner_join(position_vbd, by = c("position" = "FantPos", "pos_rank" = "league_pos_rank"))

### RB ###

RB <- my_scrape$RB %>%
  group_by(id) %>%
  mutate(
    rush_yds = mean(rush_yds, na.rm = TRUE),
    rush_tds = mean(rush_tds, na.rm = TRUE),
    fumbles_lost = mean(fumbles_lost, na.rm = TRUE),
    rec = mean(rec, na.rm = TRUE),
    rec_yds = mean(rec_yds, na.rm = TRUE),
    rec_tds = mean(rec_tds, na.rm = TRUE)
  ) %>%
  select(id, player, rush_yds, rush_tds, fumbles_lost, rec, rec_yds, rec_tds) %>%
  distinct()

RB$position <- 'RB'

RB$points <- 
  RB$rush_yds / settings$rushing_yards +
  RB$rush_tds * settings$rushing_td +
  RB$fumbles_lost * settings$fmbl +
  RB$rec * settings$reception +
  RB$rec_yds / settings$receiving_yards +
  RB$rec_tds * settings$receiving_td


RB <- RB %>%
  group_by(position) %>%
  mutate(
    pos_rank = row_number(desc(points))
  ) %>%
  arrange(desc(points))

rep <- RB %>% filter(pos_rank == 24)

RB <- RB %>%
  mutate(
    vbd = points - rep$points
  )

RB <- RB %>%
  inner_join(position_vbd, by = c("position" = "FantPos", "pos_rank" = "league_pos_rank"))

### WR ###

WR <- my_scrape$WR %>%
  group_by(id) %>%
  mutate(
    rush_yds = mean(rush_yds, na.rm = TRUE),
    rush_tds = mean(rush_tds, na.rm = TRUE),
    fumbles_lost = mean(fumbles_lost, na.rm = TRUE),
    rec = mean(rec, na.rm = TRUE),
    rec_yds = mean(rec_yds, na.rm = TRUE),
    rec_tds = mean(rec_tds, na.rm = TRUE)
  ) %>%
  select(id, player, rush_yds, rush_tds, fumbles_lost, rec, rec_yds, rec_tds) %>%
  distinct()

WR$position <- 'WR'

WR$points <- 
  WR$rush_yds / settings$rushing_yards +
  WR$rush_tds * settings$rushing_td +
  WR$fumbles_lost * settings$fmbl +
  WR$rec * settings$reception +
  WR$rec_yds / settings$receiving_yards +
  WR$rec_tds * settings$receiving_td


WR <- WR %>%
  group_by(position) %>%
  mutate(
    pos_rank = row_number(desc(points))
  ) %>%
  arrange(desc(points))

rep <- WR %>% filter(pos_rank == 24)

WR <- WR %>%
  mutate(
    vbd = points - rep$points
  )

WR <- WR %>%
  inner_join(position_vbd, by = c("position" = "FantPos", "pos_rank" = "league_pos_rank"))

### TE ###

TE <- my_scrape$TE %>%
  group_by(id) %>%
  mutate(
    rush_yds = mean(rush_yds, na.rm = TRUE),
    rush_tds = mean(rush_tds, na.rm = TRUE),
    fumbles_lost = mean(fumbles_lost, na.rm = TRUE),
    rec = mean(rec, na.rm = TRUE),
    rec_yds = mean(rec_yds, na.rm = TRUE),
    rec_tds = mean(rec_tds, na.rm = TRUE)
  ) %>%
  select(id, player, rush_yds, rush_tds, fumbles_lost, rec, rec_yds, rec_tds) %>%
  distinct()

TE$position <- 'TE'

TE$points <- 
  TE$rush_yds / settings$rushing_yards +
  TE$rush_tds * settings$rushing_td +
  TE$fumbles_lost * settings$fmbl +
  TE$rec * settings$reception +
  TE$rec_yds / settings$receiving_yards +
  TE$rec_tds * settings$receiving_td


TE <- TE %>%
  group_by(position) %>%
  mutate(
    pos_rank = row_number(desc(points))
  ) %>%
  arrange(desc(points))

rep <- TE %>% filter(pos_rank == 12)

TE <- TE %>%
  mutate(
    vbd = points - rep$points
  )

TE <- TE %>%
  inner_join(position_vbd, by = c("position" = "FantPos", "pos_rank" = "league_pos_rank"))

QB <- QB %>% select(id, player, position, points, pos_rank_avg_score, pos_rank, vbd, pos_rank_vbd)
WR <- WR %>% select(id, player, position, points, pos_rank_avg_score, pos_rank, vbd, pos_rank_vbd)
RB <- RB %>% select(id, player, position, points, pos_rank_avg_score, pos_rank, vbd, pos_rank_vbd)
TE <- TE %>% select(id, player, position, points, pos_rank_avg_score, pos_rank, vbd, pos_rank_vbd)

positions <- list(QB, RB, WR, TE)

projections <- positions %>% bind_rows()
projections <- projections %>%
  mutate(
    point_var = pos_rank_avg_score - points,
    vbd_var = pos_rank_vbd - vbd
  )

projections %>%
  select(position, pos_rank, pos_rank_vbd, vbd) %>%
  gather(pos_rank_vbd, vbd, key = "metric", value = "vbd") %>%
  ggplot(aes(pos_rank, vbd, fill=metric, color=metric)) +
    geom_point() +
    facet_wrap(vars(position))


