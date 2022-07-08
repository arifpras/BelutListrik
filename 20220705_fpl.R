---
#title: "20220706_fpl"
#author: "Arif P. Sulistiono / @arifpras"
#format: html
#editor: visual
---


# Remove all items in Environment!
rm(list = ls())
ls()

# Load packages
pacman::p_load(tidyverse, data.table, readxl, writexl, glue, wesanderson,
               RColorBrewer, viridis, ggrepel, ggthemes, ggsci, mFilter,
               quantmod, corrplot, cowplot, readstata13, readr, knitr,
               kableExtra, stargazer, parameters, colorspace, dendextend,
               ggdendro, purrr, cluster, plotly, DescTools, factoextra,
               grid, gridExtra, ggsoccer, RCurl, tidytext, rvest)

options(tz = "Europe/London")

# Working directory
setwd("~/OneDrive - The University of Nottingham/BB_SideProject/2022FPL")



id_dict <- "https://raw.githubusercontent.com/arifpras/Fantasy-Premier-League/master/data/2021-22/id_dict.csv"

db00a <- readr::read_csv(id_dict)



player_udrstat_2021 <- "https://raw.githubusercontent.com/arifpras/Fantasy-Premier-League/master/data/2020-21/understat/understat_player.csv"

player_udrstat_2122 <- "https://raw.githubusercontent.com/arifpras/Fantasy-Premier-League/master/data/2021-22/understat/understat_player.csv"

db00b <- read_csv(player_udrstat_2021)
db00c <- read_csv(player_udrstat_2122)



player_hist_2021 <- "https://raw.githubusercontent.com/arifpras/Fantasy-Premier-League/master/data/2020-21/gws/merged_gw.csv"

player_hist_2122 <- "https://raw.githubusercontent.com/arifpras/Fantasy-Premier-League/master/data/2021-22/gws/merged_gw.csv"

db00d <- read_csv(player_hist_2021)
db00e <- read_csv(player_hist_2122)



#season 2020/2021
db00f <- db00d %>%
  left_join(db00a, by = c("name" = "FPL_Name")) %>%
  na.omit() %>%
  mutate(kickoff_time_adj = as.Date(kickoff_time, "%Y-%m-%d"))

#season 2021/2022
db00g <- db00e %>%
  left_join(db00a, by = c("name" = "FPL_Name")) %>%
  na.omit() %>%
  mutate(kickoff_time_adj = as.Date(kickoff_time, "%Y-%m-%d"))



#NA understat_id
db99a <- db00d %>%
  left_join(db00a, by = c("name" = "FPL_Name")) %>%
  filter(is.na(FPL_ID)) %>%
  distinct(name)

db99b <- db00e %>%
  left_join(db00a, by = c("name" = "FPL_Name")) %>%
  filter(is.na(FPL_ID)) %>%
  distinct(name)



#Active players from mid 2021/2022
#db00h <- db00f %>%
#  filter(kickoff_time_adj > "2022-01-31") %>%
#  group_by(name, Understat_ID) %>%
#  summarise(
#    totminutes = sum(minutes),
#    .groups = "keep"
#  ) %>%
#  ungroup %>%
#  filter(totminutes != 0) %>%
#  mutate(
#    active_mid2122 = "Yes"
#  )

relegated <- c("Burnley", "Norwich", "Watford")

(
  db00h <- db00g %>%
    filter(kickoff_time_adj > "2022-01-31") %>%
    group_by(name, Understat_ID) %>%
    mutate(totminutes = sum(minutes),
           .groups = "keep") %>%
    ungroup %>%
    filter(totminutes != 0) %>%
    mutate(active_mid2122 = "Yes") %>%
    group_by(name) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(name, Understat_ID, position, team, totminutes, active_mid2122) %>%
    filter(!team %in% relegated #not including the relegated teams
    )
)



#merging player stats from 20/21 and 21/22

#db00i <- db00f %>%
#  bind_rows(db00g) %>%
#  right_join(db00h, c = "name")

(
  db00i <- db00f %>%
    bind_rows(db00g) %>%
    left_join((db00h %>% select(-position, -team)), c("name", "Understat_ID")) %>%
    filter(active_mid2122 == "Yes") %>%
    arrange(name, kickoff_time_adj, GW)
)



#aggregate, player stats in two seasons
(
  db01a <- db00i %>%
  group_by(name, Understat_ID) %>%
  summarise(
    avg_points = mean(total_points),
    wvg_points = weighted.mean(total_points, (value/10)),
    std_points = sd(total_points),
    avg_minutes = mean(minutes),
    avg_price = mean((value/10)),
    wvg_price = weighted.mean((value/10), total_points),
    std_influence = sd(influence),
    avg_saves = mean(saves),
    avg_threat = mean(threat),
    avg_clean = mean(clean_sheets),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  filter(
    avg_points > 2 & avg_minutes > 40
  ) %>%
  arrange(desc(wvg_points))
)



#merging understat's statistics
(
  db00j <- (db00b %>% mutate(season = "21/22")) %>%
    bind_rows(db00c %>% mutate(season = "20/21")) %>%
    arrange(player_name, season)
)



#individual statistics in two seasons
(
  db00k <- db00j %>%
  group_by(player_name, id) %>%
  summarise(
    avg_xG = mean(xG), #expected goals
    avg_xA = mean(xA), #expected assist
    avg_npxG = mean(npxG), #non-penalty expected goals
    avg_xGChain = mean(xGChain), #number of Expected Goals in which a player has participated
    avg_xGBuildup = mean(xGBuildup), #which players are part of passing chains that end in a shot but without taking into account the last two links – shot and last pass -, thus measuring midfielders, centre-backs or full-backs who have an important influence on their team's possessions
    .groups = "keep"
  ) %>%
  ungroup() %>%
  right_join(
    (db00h %>% select(Understat_ID, team, position)), by = c("id" = "Understat_ID")
  )
)



#merging db01a with the understat's statistics
(
  db01b <- db01a %>%
  left_join((db00k %>% select(-player_name)), c("Understat_ID" = "id")) %>%
  relocate(
    name, position, team
  )
)


(
  db01c <- db01b %>%
  left_join(db00a %>% select(Understat_ID, Understat_Name), by = c("Understat_ID")) %>%
  select(-name) %>%
  relocate(
    Understat_Name, position, team
  )
)



(
  db00l <- db00g %>%
    filter(!team %in% relegated) %>%  #not including the relegated teams
    group_by(team) %>%
    summarise(
      team_points = mean(total_points),
      team_threat = mean(threat),
      team_ict = mean(ict_index),
      team_conceded = mean(goals_conceded),
      team_scored = mean(goals_scored),
      team_clean = mean(clean_sheets),
      team_saves = mean(saves)
    ) %>%
    ungroup() %>%
    mutate(
      difficulty_index = (0.75 * team_scored) + (0.25 * team_clean),
      difficulty_scaled = scales::rescale(difficulty_index, to = c(1, 5))
    ) %>%
    arrange(difficulty_scaled)
  %>%
    mutate(n = row_number()) %>%
    select(team, difficulty_scaled, n) %>%
    mutate(difficulty_rating = case_when(n <= 1 ~ 1,
                                         n <= 5 ~ 2,
                                         n <= 9 ~ 3,
                                         n <= 13 ~ 4,
                                         n > 13 ~ 5)) %>%
    add_row(
      team = "Nottingham Forest", difficulty_scaled = 1, n = 0, difficulty_rating = 1
    ) %>%
    add_row(
      team = "Bournemouth", difficulty_scaled = 1, n = 0, difficulty_rating = 1
    ) %>%
    add_row(
      team = "Fulham", difficulty_scaled = 1, n = 0, difficulty_rating = 1
    ) %>%
    arrange(n)
)



(
  db00m <- db00l %>%
    select(-team, -team_points, -team_ict, -team_scored, -team_conceded, -team_clean) %>%
    as.matrix() %>%
    Hmisc::rcorr()
)



fixt00 <- "https://fixturedownload.com/results/epl-2022"
fixt01 <- read_html(fixt00)

fixt02 <- fixt01 %>%
  html_node("body") %>%
  html_children()

fixt03 <- html_nodes(fixt02, 'td')

fixt04 <- html_text(fixt03)
fixt05 <- data.frame(fixt04)

fixt06 <-
  as.data.frame(matrix(fixt05$fixt04, ncol = 6, byrow = TRUE))



(
  db00n <- fixt06 %>%
    select(
      V1, V4, V5
    ) %>%
    rename(
      gameweek = V1,
      home = V4,
      away = V5
    ) %>%
    left_join(
      db00l, by = c("away" = "team")
    ) %>%
    select(-difficulty_scaled, -n) %>%
    rename(hometeam_dr = difficulty_rating) %>%
    left_join(
      db00l, by = c("home" = "team")
    ) %>%
    rename(awayteam_dr = difficulty_rating) %>%
    select(-difficulty_scaled, -n)
)



db00o <- db00n %>%
  select(gameweek, home, hometeam_dr) %>%
  rename(
    team = home,
    difficulty_rating = hometeam_dr
  )

db00p <- db00n %>%
  select(gameweek, away, awayteam_dr) %>%
  rename(
    team = away,
    difficulty_rating = awayteam_dr
  )

(
  db00q <- db00o %>%
    bind_rows(db00p) %>%
    arrange(gameweek)
)



db00r <- db00q %>%
  arrange(team, gameweek) %>%
  filter(gameweek < 6) %>%
  group_by(team) %>%
  summarise(difficulty_next5 = mean(difficulty_rating)) %>%
  ungroup()


(
  db02a <- db01c %>%
    select(Understat_Name, position) %>%
  as_tibble() %>%
  group_by(position) %>%
  mutate(
    pos_id = row_number()
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = position,
    values_from = Understat_Name
  ) %>%
  select(-pos_id) %>%
  relocate(GK, DEF, MID, FWD)
)


pos <- "GK"
prop <- 0.5

#main player
gk_maxpoints <- db01c %>%
  group_by(position) %>%
  arrange(desc(avg_points)) %>%
  slice_head(prop = prop) %>%
  ungroup() %>%
  filter(position == pos)

#bench player
gk_minprice <- db01c %>%
  group_by(position) %>%
  arrange(avg_price) %>%
  slice_head(prop = prop) %>%
  ungroup() %>%
  filter(position == pos)

#all players
gk_pool <- gk_maxpoints %>%
  bind_rows(gk_minprice) %>%
  unique()

db03a <- t(combn(gk_pool$Understat_Name, 2)) #max: 2 players
#saveRDS(db03a, "db03a.rds")

db03b <- db03a %>%
  na.omit() %>%
  as_tibble() %>%
  mutate("id_gk" = row_number()) %>%
  relocate(id_gk) %>%
  pivot_longer(cols = 2:3,
               names_to = "v",
               values_to = "Understat_Name") %>%
  select(-v) %>%
  left_join(db01c, by = "Understat_Name") %>%
  left_join(db00r, by = "team")

#saveRDS(db03b, "db03b.rds")

db03c <- db03b %>%
  group_by(id_gk) %>%
  summarise(
    agg_avgpoints = sum(avg_points),
    avg_wvgpoints = mean(wvg_points),
    avg_avgminutes = mean(avg_minutes),
    avg_stdpoints = mean(std_points),
    agg_avgprice = sum(avg_price),
    avg_wvgprice = mean(wvg_price),
    avg_stdinfluence = mean(std_influence),
    avg_avgXG = mean(avg_xG),
    avg_avgxA = mean(avg_xA),
    avg_avgnpxG = mean(avg_npxG),
    avg_avgxGChain = mean(avg_xGChain),
    avg_avgxGBuildup = mean(avg_xGBuildup),
    avg_avgsaves = mean(avg_saves),
    avg_avgthreat = mean(avg_threat),
    avg_difficulty = mean(difficulty_next5),
    avg_avgclean = mean(avg_clean)
    ) %>%
  arrange(
    desc(agg_avgpoints),
    desc(avg_wvgpoints), #semakin besar, semakin worth it
    agg_avgprice,
    desc(avg_avgXG),
    desc(avg_avgxA),
    desc(avg_avgminutes),
    desc(avg_stdpoints)
  )

#saveRDS(db03c, "db03c.rds")

#db99c <- db03c %>%
#  filter(between(total_avgpoints,
#                 quantile(total_avgpoints, .25),
#                 quantile(total_avgpoints, .75))
#         )

db03d <- db03c %>%
  arrange(desc(agg_avgpoints)) %>%
  filter(agg_avgprice < 10 & agg_avgprice > 8) %>%
  slice_head(n = 5) %>%
  left_join(db03b, by = "id_gk") %>%
  select(
    id_gk,
    Understat_Name,
    team,
    avg_points,
    std_points,
    wvg_points,
    avg_price,
    wvg_price,
    avg_xG,
    avg_xA,
    avg_xGChain,
    avg_xGBuildup,
    avg_saves,
    avg_threat,
    avg_clean,
    difficulty_next5,
    agg_avgprice,
    agg_avgpoints,
    avg_stdpoints,
    avg_wvgpoints,
    avg_wvgprice,
    avg_avgXG,
    avg_avgxA,
    avg_avgxGChain,
    avg_avgxGBuildup,
    avg_avgsaves,
    avg_avgthreat,
    avg_difficulty,
    avg_avgclean
  ) %>%
  arrange(desc(agg_avgpoints),
          desc(avg_wvgpoints)) %>%
  relocate(id_gk,
           Understat_Name,
           team,
           avg_points,
           avg_price,
           avg_saves,
           avg_difficulty)

(
  gk_plot <- db03d %>%
    select(
      id_gk,
      agg_avgprice,
      agg_avgpoints,
      avg_avgsaves,
      avg_avgclean,
      avg_difficulty,
      avg_avgxGBuildup
    ) %>%
    group_by(id_gk) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    pivot_longer(
      cols = 2:7,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    ggplot(aes(
      x = reorder_within(id_gk, value, indicator),
      y = value,
      fill = factor(id_gk)
    )) +
    geom_col() +
    #geom_bar(stat = "identity", aes(fill = factor(id_gk))) +
    coord_flip() +
    scale_x_reordered() +
    #scale_fill_viridis(discrete = TRUE) +
    #scale_fill_manual(values = wes_palette("Darjeeling1")) +
    scale_fill_manual(values = c(
      "#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c"
    )) +
    egg::theme_article() +
    facet_wrap( ~ indicator, ncol = 3, scales = "free") +
    theme(legend.position = "none") +
    labs(x = "",
         y = "")
)



(gk_choice <- db03d %>% filter(id_gk == 49) %>% mutate("position" = "GK"))


pos <- "DEF"
prop <- 0.5

#main player
def_maxpoints <- db01c %>%
  group_by(position) %>%
  arrange(desc(avg_points)) %>%
  slice_head(prop = prop) %>%
  ungroup() %>%
  filter(position == pos)

#bench player
def_minprice <- db01c %>%
  group_by(position) %>%
  arrange(avg_price) %>%
  slice_head(prop = prop) %>%
  ungroup() %>%
  filter(position == pos)

#all players
def_pool <- def_maxpoints %>%
  bind_rows(def_minprice) %>%
  unique()

db04a <- t(combn(def_pool$Understat_Name, 5)) #max: 5 players
#saveRDS(db04a, "db04a.rds")

db04b <- db04a %>%
  na.omit() %>%
  as_tibble() %>%
  mutate("id_def" = row_number()) %>%
  relocate(id_def) %>%
  pivot_longer(cols = 2:6,
               names_to = "v",
               values_to = "Understat_Name") %>%
  select(-v) %>%
  left_join(db01c, by = "Understat_Name") %>%
  left_join(db00r, by = "team")

#saveRDS(db04b, "db04b.rds")


db04c <- db04b %>%
  filter(Understat_Name != "Marc Cucurella" ) %>%
  group_by(id_def) %>%
  summarise(
    agg_avgpoints = sum(avg_points),
    avg_wvgpoints = mean(wvg_points),
    avg_avgminutes = mean(avg_minutes),
    avg_stdpoints = mean(std_points),
    agg_avgprice = sum(avg_price),
    avg_wvgprice = mean(wvg_price),
    avg_stdinfluence = mean(std_influence),
    avg_avgXG = mean(avg_xG),
    avg_avgxA = mean(avg_xA),
    avg_avgnpxG = mean(avg_npxG),
    avg_avgxGChain = mean(avg_xGChain),
    avg_avgxGBuildup = mean(avg_xGBuildup),
    avg_avgsaves = mean(avg_saves),
    avg_avgthreat = mean(avg_threat),
    avg_difficulty = mean(difficulty_next5),
    avg_avgclean = mean(avg_clean)
    ) %>%
  arrange(
    desc(agg_avgpoints),
    desc(avg_wvgpoints), #semakin besar, semakin worth it
    agg_avgprice,
    desc(avg_avgXG),
    desc(avg_avgxA),
    desc(avg_avgminutes),
    desc(avg_stdpoints)
  )

#saveRDS(db04c, "db04c.rds")

#db99d <- db04c %>%
#  filter(between(total_avgpoints,
#                 quantile(total_avgpoints, .25),
#                 quantile(total_avgpoints, .75))
#         )

db04d <- db04c %>%
  arrange(desc(agg_avgpoints)) %>%
  filter(agg_avgprice < 30 & agg_avgprice > 29) %>%
  slice_head(n = 5) %>%
  left_join(db04b, by = "id_def") %>%
  select(
    id_def,
    Understat_Name,
    team,
    avg_points,
    std_points,
    wvg_points,
    avg_price,
    wvg_price,
    avg_xG,
    avg_xA,
    avg_xGChain,
    avg_xGBuildup,
    avg_saves,
    avg_threat,
    avg_clean,
    difficulty_next5,
    agg_avgprice,
    agg_avgpoints,
    avg_stdpoints,
    avg_wvgpoints,
    avg_wvgprice,
    avg_avgXG,
    avg_avgxA,
    avg_avgxGChain,
    avg_avgxGBuildup,
    avg_avgsaves,
    avg_avgthreat,
    avg_difficulty,
    avg_avgclean
  ) %>%
  arrange(desc(agg_avgpoints),
          desc(avg_wvgpoints)) %>%
  relocate(id_def,
           Understat_Name,
           team,
           avg_points,
           avg_price,
           avg_saves,
           avg_difficulty)

(
  def_plot <- db04d %>%
    select(
      id_def,
      agg_avgprice,
      agg_avgpoints,
      avg_avgXG,
      avg_avgxA,
      avg_avgclean,
      avg_difficulty
    ) %>%
    group_by(id_def) %>%
    slice_head(n=1) %>%
    ungroup() %>%
    pivot_longer(
      cols = 2:7,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    ggplot(aes(
      x = reorder_within(id_def, value, indicator),
      y = value,
      fill = factor(id_def)
    )) +
    geom_col() +
    #geom_bar(stat = "identity", aes(fill = factor(id_gk))) +
    coord_flip() +
    scale_x_reordered() +
    #scale_fill_viridis(discrete = TRUE) +
    #scale_fill_manual(values = wes_palette("Darjeeling1")) +
    scale_fill_manual(values = c("#ef476f","#ffd166","#06d6a0","#118ab2","#073b4c")) +
    egg::theme_article() +
    facet_wrap(~ indicator, ncol = 3, scales = "free") +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = "",
      y = ""
    )
)




(def_choice <- db04d %>% filter(id_def == 13292) %>% mutate("position" = "DEF"))


pos <- "MID"
prop <- 0.5

#main player
mid_maxpoints <- db01c %>%
  group_by(position) %>%
  arrange(desc(avg_points)) %>%
  slice_head(prop = prop) %>%
  ungroup() %>%
  filter(position == pos)

#bench player
mid_minprice <- db01c %>%
  group_by(position) %>%
  arrange(avg_price) %>%
  slice_head(prop = prop) %>%
  ungroup() %>%
  filter(position == pos)

#all players
mid_pool <- mid_maxpoints %>%
  bind_rows(mid_minprice) %>%
  unique()

db05a <- t(combn(mid_pool$Understat_Name, 5)) #max: 5 players
#saveRDS(db05a, "db05a.rds")

db05b <- db05a %>%
  na.omit() %>%
  as_tibble() %>%
  mutate("id_mid" = row_number()) %>%
  relocate(id_mid) %>%
  pivot_longer(cols = 2:6,
               names_to = "v",
               values_to = "Understat_Name") %>%
  select(-v) %>%
  left_join(db01c, by = "Understat_Name") %>%
  left_join(db00r, by = "team")


#saveRDS(db05b, "db05b.rds")


db05c <- db05b %>%
  group_by(id_mid) %>%
summarise(
    agg_avgpoints = sum(avg_points),
    avg_wvgpoints = mean(wvg_points),
    avg_avgminutes = mean(avg_minutes),
    avg_stdpoints = mean(std_points),
    agg_avgprice = sum(avg_price),
    avg_wvgprice = mean(wvg_price),
    avg_stdinfluence = mean(std_influence),
    avg_avgXG = mean(avg_xG),
    avg_avgxA = mean(avg_xA),
    avg_avgnpxG = mean(avg_npxG),
    avg_avgxGChain = mean(avg_xGChain),
    avg_avgxGBuildup = mean(avg_xGBuildup),
    avg_avgsaves = mean(avg_saves),
    avg_avgthreat = mean(avg_threat),
    avg_difficulty = mean(difficulty_next5),
    avg_avgclean = mean(avg_clean)
    ) %>%
  arrange(
    desc(agg_avgpoints),
    desc(avg_wvgpoints), #semakin besar, semakin worth it
    agg_avgprice,
    desc(avg_avgXG),
    desc(avg_avgxA),
    desc(avg_avgminutes),
    desc(avg_stdpoints)
  )


#saveRDS(db05c, "db05c.rds")

#db99e <- db05c %>%
#  filter(between(total_avgpoints,
#                 quantile(total_avgpoints, .25),
#                 quantile(total_avgpoints, .75))
#         )

db05d <- db05c %>%
  arrange(desc(agg_avgpoints)) %>%
  filter(agg_avgprice < 40 & agg_avgprice > 39) %>%
  slice_head(n = 5) %>%
  left_join(db05b, by = "id_mid") %>%
  select(
    id_mid,
    Understat_Name,
    team,
    avg_points,
    std_points,
    wvg_points,
    avg_price,
    wvg_price,
    avg_xG,
    avg_xA,
    avg_xGChain,
    avg_xGBuildup,
    avg_saves,
    avg_threat,
    avg_clean,
    difficulty_next5,
    agg_avgprice,
    agg_avgpoints,
    avg_stdpoints,
    avg_wvgpoints,
    avg_wvgprice,
    avg_avgXG,
    avg_avgxA,
    avg_avgxGChain,
    avg_avgxGBuildup,
    avg_avgsaves,
    avg_avgthreat,
    avg_difficulty,
    avg_avgclean
  ) %>%
  arrange(desc(agg_avgpoints),
          desc(avg_wvgpoints)) %>%
  relocate(id_mid,
           Understat_Name,
           team,
           avg_points,
           avg_price,
           avg_threat,
           avg_difficulty)


(
  mid_plot <- db05d %>%
    select(
      id_mid,
      agg_avgprice,
      agg_avgpoints,
      avg_avgXG,
      avg_avgxA,
      avg_threat,
      avg_difficulty
    ) %>%
    group_by(id_mid) %>%
    slice_head(n=1) %>%
    ungroup() %>%
    pivot_longer(
      cols = 2:7,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    ggplot(aes(
      x = reorder_within(id_mid, value, indicator),
      y = value,
      fill = factor(id_mid)
    )) +
    geom_col() +
    #geom_bar(stat = "identity", aes(fill = factor(id_gk))) +
    coord_flip() +
    scale_x_reordered() +
    #scale_fill_viridis(discrete = TRUE) +
    #scale_fill_manual(values = wes_palette("Darjeeling1")) +
    scale_fill_manual(values = c("#ef476f","#ffd166","#06d6a0","#118ab2","#073b4c")) +
    egg::theme_article() +
    facet_wrap(~ indicator, ncol = 3, scales = "free") +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = "",
      y = ""
    )
)




(mid_choice <- db05d %>% filter(id_mid == 579) %>% mutate("position" = "MID"))



pos <- "FWD"
prop <- 0.5

#main player
fwd_maxpoints <- db01c %>%
  group_by(position) %>%
  arrange(desc(avg_points)) %>%
  slice_head(prop = prop) %>%
  ungroup() %>%
  filter(position == pos)

#bench player
fwd_minprice <- db01c %>%
  group_by(position) %>%
  arrange(avg_price) %>%
  slice_head(prop = prop) %>%
  ungroup() %>%
  filter(position == pos)

#all players
fwd_pool <- fwd_maxpoints %>%
  bind_rows(fwd_minprice) %>%
  unique()

db06a <- t(combn(fwd_pool$Understat_Name, 3)) #max: 3 players

#saveRDS(db06a, "db06a.rds")

db06b <- db06a %>%
  na.omit() %>%
  as_tibble() %>%
  mutate("id_fwd" = row_number()) %>%
  relocate(id_fwd) %>%
  pivot_longer(cols = 2:4,
               names_to = "v",
               values_to = "Understat_Name") %>%
  select(-v) %>%
  left_join(db01c, by = "Understat_Name") %>%
  left_join(db00r, by = "team")

#saveRDS(db06b, "db06b.rds")

#db06c <- db06b %>%
#  group_by(id_fwd) %>%
#  mutate(
#    agg_avgpoints = sum(avg_points),
#    avg_wvgpoints = mean(wvg_points),
#    avg_avgminutes = mean(avg_minutes),
#    avg_stdpoints = mean(std_points),
#    agg_avgprice = sum(avg_price),
#    avg_stdinfluence = mean(std_influence),
#    avg_avgXG = mean(avg_xG),
#    avg_avgxA = mean(avg_xA),
#    avg_avgnpxG = mean(avg_npxG),
#    avg_avgxGChain = mean(avg_xGChain),
#    avg_avgxGBuildup = mean(avg_xGBuildup)
#  ) %>%
#  arrange(
#    desc(agg_avgpoints),
#    desc(avg_wvgpoints), #semakin besar, semakin worth it
#    agg_avgprice,
#    desc(avg_avgXG),
#    desc(avg_avgxA),
#    desc(avg_avgminutes),
#    desc(avg_stdpoints)
#  )

db06c <- db06b %>%
  group_by(id_fwd) %>%
  summarise(
    agg_avgpoints = sum(avg_points),
    avg_wvgpoints = mean(wvg_points),
    avg_avgminutes = mean(avg_minutes),
    avg_stdpoints = mean(std_points),
    agg_avgprice = sum(avg_price),
    avg_wvgprice = mean(wvg_price),
    avg_stdinfluence = mean(std_influence),
    avg_avgXG = mean(avg_xG),
    avg_avgxA = mean(avg_xA),
    avg_avgnpxG = mean(avg_npxG),
    avg_avgxGChain = mean(avg_xGChain),
    avg_avgxGBuildup = mean(avg_xGBuildup),
    avg_avgsaves = mean(avg_saves),
    avg_avgthreat = mean(avg_threat),
    avg_difficulty = mean(difficulty_next5),
    avg_avgclean = mean(avg_clean)
    ) %>%
  arrange(
    desc(agg_avgpoints),
    desc(avg_wvgpoints), #semakin besar, semakin worth it
    agg_avgprice,
    desc(avg_avgXG),
    desc(avg_avgxA),
    desc(avg_avgminutes),
    desc(avg_stdpoints)
  )


#saveRDS(db06c, "db06c.rds")

#db99f <- db06c %>%
#  filter(between(total_avgpoints,
#                 quantile(total_avgpoints, .25),
#                 quantile(total_avgpoints, .75))
#         )

db06d <- db06c %>%
  arrange(desc(agg_avgpoints)) %>%
  filter(agg_avgprice < 20 & agg_avgprice > 19) %>%
  slice_head(n = 5) %>%
  left_join(db06b, by = "id_fwd") %>%
  select(
    id_fwd,
    Understat_Name,
    team,
    avg_points,
    std_points,
    wvg_points,
    avg_price,
    wvg_price,
    avg_xG,
    avg_xA,
    avg_xGChain,
    avg_xGBuildup,
    avg_saves,
    avg_threat,
    avg_clean,
    difficulty_next5,
    agg_avgprice,
    agg_avgpoints,
    avg_stdpoints,
    avg_wvgpoints,
    avg_wvgprice,
    avg_avgXG,
    avg_avgxA,
    avg_avgxGChain,
    avg_avgxGBuildup,
    avg_avgsaves,
    avg_avgthreat,
    avg_difficulty,
    avg_avgclean
  ) %>%
  arrange(desc(agg_avgpoints),
          desc(avg_wvgpoints)) %>%
  relocate(id_fwd,
           Understat_Name,
           team,
           avg_points,
           avg_price,
           avg_threat,
           avg_difficulty)

(
  fwd_plot <- db06d %>%
    select(
      id_fwd,
      agg_avgprice,
      agg_avgpoints,
      avg_avgXG,
      avg_avgxA,
      avg_threat,
      avg_difficulty
    ) %>%
    group_by(id_fwd) %>%
    slice_head(n=1) %>%
    ungroup() %>%
    pivot_longer(
      cols = 2:7,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    ggplot(aes(
      x = reorder_within(id_fwd, value, indicator),
      y = value,
      fill = factor(id_fwd)
    )) +
    geom_col() +
    #geom_bar(stat = "identity", aes(fill = factor(id_gk))) +
    coord_flip() +
    scale_x_reordered() +
    #scale_fill_viridis(discrete = TRUE) +
    #scale_fill_manual(values = wes_palette("Darjeeling1")) +
    scale_fill_manual(values = c("#ef476f","#ffd166","#06d6a0","#118ab2","#073b4c")) +
    egg::theme_article() +
    facet_wrap(~ indicator, ncol = 3, scales = "free") +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = "",
      y = ""
    )
)




(fwd_choice <- db06d %>% filter(id_fwd == 573) %>% mutate("position" = "FWD"))



(belutlistrik_squad <- (gk_choice %>% rename("id" = id_gk)) %>%
   bind_rows((def_choice %>% rename("id" = id_def)),
             (mid_choice %>% rename("id" = id_mid)),
             (fwd_choice %>% rename("id" = id_fwd))
   ) %>%
   mutate(
     "formid" = row_number()
   ) %>%
   relocate(id, formid, Understat_Name, position) %>%
   mutate(
     Understat_Name = recode(Understat_Name, "Trent Alexander-Arnold" = "Trent")
   )
 )



(
  belutlistrik_select <- belutlistrik_squad %>%
    filter(position != "GK") %>%
    select(
      Understat_Name,
      position,
      avg_points,
      avg_price,
      #wvg_points,
      avg_xG,
      avg_xA,
      avg_xGChain,
      avg_xGBuildup,
      #std_points,
      avg_threat,
      #avg_clean,
      difficulty_next5
    ) %>%
    pivot_longer(
      cols = 3:10,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    #mutate(
    #  "indicator" = as.factor(indicator),
    #  "Understat_Name" = tidytext::reorder_within(Understat_Name, value, indicator)
    #    ) %>%
    ggplot(aes(
      x = reorder_within(Understat_Name, value, indicator),
      y = value,
      fill = factor(position)
    )) +
    #geom_col(show.legend = FALSE) +
    geom_col() +
    #geom_bar(stat = "identity", aes(fill = factor(position))) +
    coord_flip() +
    scale_x_reordered() +
    #scale_fill_viridis(discrete = TRUE) +
    #scale_fill_manual(values = wes_palette("Darjeeling1")) +
    scale_fill_manual(values = c("#ef476f", "#ffd166", "#06d6a0")) +
    egg::theme_article() +
    facet_wrap( ~ indicator, ncol = 4, scales = "free_y") +
    theme(legend.position = "top",
          legend.title = element_blank()) +
    labs(x = "",
         y = "")
  #guides(fill = guide_legend("Position:"))
)



(belutlistrik_options <- belutlistrik_squad %>%
   mutate(
     "form352" = ifelse(formid %in% c(2, 6, 7, 15), "bench", "play"),
     "form442" = ifelse(formid %in% c(2, 7, 12, 15), "bench", "play"),
     "form343" = ifelse(formid %in% c(2, 6, 7, 12), "bench", "play")
   ) %>%
   relocate(id, formid, form442, form343, form352, Understat_Name)
)



(for352 <- tibble(
  x = c(98, 98, 83, 83, 83, 89 ,80, 68, 68, 68, 68, 68, 53, 53, 71),
  y = c(50, 110, 25, 50, 75, 110, 110, 10, 30, 50, 70, 90, 37.5, 62.5, 110),
  formid = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
))

(for442 <- tibble(
  x = c(98, 98, 83, 83, 83, 83, 89 ,68, 68, 68, 68, 80, 53, 53, 71),
  y = c(50, 110, 20, 40, 60, 80, 110, 20, 40, 60, 80, 110, 37.5, 62.5, 110),
  formid = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
))

(for343 <- tibble(
  x = c(98, 98, 83, 83, 83, 89, 80, 68, 68, 68, 68, 71, 53, 53, 53),
  y = c(50, 110, 25, 50, 75, 110, 110, 20, 40, 60, 80, 110, 25, 50, 75),
  formid = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
))



(
  belutlistrik_442 <- belutlistrik_options %>%
    select(formid, form442, Understat_Name) %>%
    right_join(for442, by = "formid")
)

(
  belutlistrik_343 <- belutlistrik_options %>%
    select(formid, form343, Understat_Name) %>%
    right_join(for343, by = "formid")
)



(
  belutlistrik <- ggplot(belutlistrik_442) + #change
    annotate_pitch(
      colour = "white",
      fill   = "#e0e2db",
      limits = FALSE
    ) +
    geom_point(
      aes(x = x, y = 100 - y, fill = factor(form442)), #change
      shape = 21,
      size = 5
    ) +
    scale_fill_manual(values = c("play" = "#8b2635", "bench" = "#2e3532")) +
    geom_text_repel(
      aes(
        x = x,
        y = 100 - y,
        label = stringr::str_wrap(Understat_Name, 10),
        color = factor(form442) #change
      ),
      direction = "y",
      nudge_x = -4,
      segment.colour = NA,
      size = 3,
      #box.padding = unit(-2.5, "lines")
    ) +
    scale_color_manual(values = c("play" = "#8b2635", "bench" = "#2e3532")) +
    theme_pitch() +
    theme(
      panel.background = element_rect(fill = "#e0e2db"),
      legend.position = "none",
      plot.title = element_text(hjust = 0, size = 18,
                                face = "bold"),
      plot.subtitle = element_text(hjust = 0, size = 14),
    ) +
    coord_flip(xlim = c(49, 101),
               ylim = c(-12, 112)) +
    ggtitle("Belut Listrik FC",
            "instagram/arifpras")
)

#belutlistrik



# Removes all items in Environment!
rm(list = ls())
ls()
