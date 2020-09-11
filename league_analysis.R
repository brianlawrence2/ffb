library(tidyverse)
library(rvest)

league <- read_csv("league.csv")

league <- league %>%
  mutate(
    QB = str_extract(QB, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    RB = str_extract(RB, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    RB_1 = str_extract(RB_1, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    WR = str_extract(WR, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    WR_1 = str_extract(WR_1, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    TE = str_extract(TE, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    FLEX = str_extract(FLEX, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    K = str_extract(K, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    DST = str_extract(DST, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    BN = str_extract(BN, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    BN_1 = str_extract(BN_1, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    BN_2 = str_extract(BN_2, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    BN_3 = str_extract(BN_3, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+"),
    BN_4 = str_extract(BN_4, "[A-Za-z\'\\.\\-]+\\s[A-Za-z\\.\\-]+")
  )

league_g <- league %>%
  select(Teams, QB, RB, RB_1, WR, WR_1, TE, FLEX) %>%
  gather("position", "player", 2:8)

league <- league_g %>%
  inner_join(projections, by = "player")

league %>%
  group_by(Teams) %>%
  mutate(
    team_vbd = sum(pos_rank_vbd),
    team_points = sum(pos_rank_avg_score)
  ) %>%
  select(Teams, team_vbd, team_points) %>%
  distinct() %>%
  arrange(desc(team_points))

