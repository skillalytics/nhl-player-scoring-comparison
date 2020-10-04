
#---  SETUP ---#
library(tidyverse)
library(rvest)
library(plyr)


#---  DATA PREP ---#

# Load Datasets
href_sbs <- read.csv('./skater_basic_stats.csv')
href_stnd <- read.csv('./league_standings.csv')

# Skater Stats
skater_stats <- href_sbs %>%
  filter(
    SEASON >= 1922
  ) %>%
  select(
    SEASON, PLAYER_NAME, AGE, TM, POS, GP, G, A, PTS
  ) %>%
  mutate(
    GPG = round(G/GP, digits=4),
    APG = round(A/GP, digits=4),
    PTSPG = round(PTS/GP, digits=4)
  )

# Remove TOT Seasons
skater_stats_nontot <- skater_stats[!(skater_stats$TM=="TOT"),]
skater_stats_nontot <- skater_stats_nontot %>%
  group_by(SEASON, PLAYER_NAME) %>%
  summarise(
    GP = sum(GP),
    G = sum(G),
    A = sum(A),
    PTS = sum(PTS)
  ) %>%
  mutate(
    GPG = round(G/GP, digits=4),
    APG = round(A/GP, digits=4),
    PTSPG = round(PTS/GP, digits=4)
  )

# Standings
leag_stand <- href_stnd %>%
  filter(
    SEASON >= 1922
  ) %>%
  select(
    SEASON, TM, GP, GF, GA
  )


#---  ANALYSIS ---#

# Calculate Career Stats by Player
career_stats <- skater_stats_nontot %>%
  group_by(PLAYER_NAME) %>%
  summarise(
    GP_TOT = sum(GP),
    G_TOT = sum(G),
    A_TOT = sum(A),
    PTS_TOT = sum(PTS)
  ) %>%
  mutate(
    GPG_TOT = round(G_TOT/GP_TOT, digits=4),
    APG_TOT = round(A_TOT/GP_TOT, digits=4),
    PTSPG_TOT = round(PTS_TOT/GP_TOT, digits=4)
  )  

# Calculate Season Data
teams_per_season <- leag_stand %>%
  count(SEASON)
colnames(teams_per_season) <- c('SEASON', 'TEAM_COUNT')

games_played_by_season <- leag_stand %>%
  group_by(SEASON) %>%
  summarise(
    TM_GP_TOT = sum(GP)
  )

players_per_season <- skater_stats_nontot %>%
  count(SEASON)
colnames(players_per_season) <- c('SEASON', 'PLAYER_COUNT')

season_stats <- skater_stats_nontot %>%
  group_by(SEASON) %>%
  summarise(
    GP_TOT = sum(GP),
    G_TOT = sum(G),
    A_TOT = sum(A),
    PTS_TOT = sum(PTS)
  ) %>%
  mutate(
    GPG_TOT = round(G_TOT/GP_TOT, digits=4),
    APG_TOT = round(A_TOT/GP_TOT, digits=4),
    PTSPG_TOT = round(PTS_TOT/GP_TOT, digits=4)
  )  

season_dat <- Reduce(function(x,y) merge(x, y, by="SEASON"), list(teams_per_season, games_played_by_season, players_per_season, season_stats))

season_dat <- add_column(season_dat,
                         LEAG_GP = season_dat$TM_GP_TOT/2, .after = 3 # calculate number of games played league wide per season (each team plays one another)
)

season_dat <- season_dat %>%
  mutate(
    LEAG_GPG = G_TOT/LEAG_GP,
    LEAG_APG = A_TOT/LEAG_GP,
    LEAG_PTSPG = PTS_TOT/LEAG_GP,
  )

# Calculate Adjusted Scoring Stats and Rate Statistics
skater_stats_nontot_leagtots <- merge(
  skater_stats_nontot, season_dat[ , c('SEASON', 'G_TOT', 'A_TOT', 'PTS_TOT', 'LEAG_GP')], by='SEASON', all.x = TRUE
)
skater_stats_leagtots <- merge(
  skater_stats, season_dat[ , c('SEASON', 'G_TOT', 'A_TOT', 'PTS_TOT', 'LEAG_GP')], by='SEASON', all.x = TRUE
)

  # Goals
  BASELINE_GPG <- sum(season_dat$G_TOT)/sum(season_dat$LEAG_GP)
  
  skater_adj_goal_stats <- skater_stats_nontot_leagtots %>%
    mutate(
      LEAG_GPG_LESSCP = (G_TOT - G)/LEAG_GP,
      ADJFAC_GPG = BASELINE_GPG/LEAG_GPG_LESSCP,
      ADJ_GPG = GPG*ADJFAC_GPG,
      ADJ_G = GP*ADJ_GPG
    )
  
  career_adj_goal_stats <- skater_adj_goal_stats %>%
    group_by(PLAYER_NAME) %>%
    summarise(
      GP = sum(GP),
      G = sum(G),
      AVG_SEAS_ADJ_GPG = mean(ADJ_GPG),
      ADJ_G = sum(ADJ_G)
    ) %>%
    mutate(
      ADJ_GPG = ADJ_G/GP
    )
  
  skater_adj_goal_stats <- skater_stats_leagtots %>%
    mutate(
      LEAG_GPG_LESSCP = (G_TOT - G)/LEAG_GP,
      ADJFAC_GPG = BASELINE_GPG/LEAG_GPG_LESSCP,
      ADJ_GPG = GPG*ADJFAC_GPG,
      ADJ_G = GP*ADJ_GPG
    )

  # Assists
  BASELINE_APG <- sum(season_dat$A_TOT)/sum(season_dat$LEAG_GP)
  
  skater_adj_assist_stats <- skater_stats_nontot_leagtots %>%
    mutate(
      LEAG_APG_LESSCP = (A_TOT - A)/LEAG_GP,
      ADJFAC_APG = BASELINE_APG/LEAG_APG_LESSCP,
      ADJ_APG = APG*ADJFAC_APG,
      ADJ_A = GP*ADJ_APG
    )
  
  career_adj_assist_stats <- skater_adj_assist_stats %>%
    group_by(PLAYER_NAME) %>%
    summarise(
      GP = sum(GP),
      A = sum(A),
      AVG_SEAS_ADJ_APG = mean(ADJ_APG),
      ADJ_A = sum(ADJ_A)
    ) %>%
    mutate(
      ADJ_APG = ADJ_A/GP
    )
  
  skater_adj_assist_stats <- skater_stats_leagtots %>%
    mutate(
      LEAG_APG_LESSCP = (A_TOT - A)/LEAG_GP,
      ADJFAC_APG = BASELINE_APG/LEAG_APG_LESSCP,
      ADJ_APG = APG*ADJFAC_APG,
      ADJ_A = GP*ADJ_APG
    )

  # Points
  BASELINE_PTSPG <- sum(season_dat$PTS_TOT)/sum(season_dat$LEAG_GP)
  
  skater_adj_point_stats <- skater_stats_nontot_leagtots %>%
    mutate(
      LEAG_PTSPG_LESSCP = (PTS_TOT - PTS)/LEAG_GP,
      ADJFAC_PTSPG = BASELINE_PTSPG/LEAG_PTSPG_LESSCP,
      ADJ_PTSPG = PTSPG*ADJFAC_PTSPG,
      ADJ_PTS = GP*ADJ_PTSPG
    )
  
  career_adj_point_stats <- skater_adj_point_stats %>%
    group_by(PLAYER_NAME) %>%
    summarise(
      GP = sum(GP),
      PTS = sum(PTS),
      AVG_SEAS_ADJ_PTSPG = mean(ADJ_PTSPG),
      ADJ_PTS = sum(ADJ_PTS)
    ) %>%
    mutate(
      ADJ_PTSPG = ADJ_PTS/GP
    )
  
  skater_adj_point_stats <- skater_stats_leagtots %>%
    mutate(
      LEAG_PTSPG_LESSCP = (PTS_TOT - PTS)/LEAG_GP,
      ADJFAC_PTSPG = BASELINE_PTSPG/LEAG_PTSPG_LESSCP,
      ADJ_PTSPG = PTSPG*ADJFAC_PTSPG,
      ADJ_PTS = GP*ADJ_PTSPG
    )


#---  FINAL OUTPUT ---#

  # Career Totals
  career_tot_output_tbl <- merge(
    career_adj_goal_stats, career_adj_assist_stats[ , c('PLAYER_NAME', 'A', 'AVG_SEAS_ADJ_APG', 'ADJ_A', 'ADJ_APG')],
    by='PLAYER_NAME'
  )
  career_tot_output_tbl <- merge(
    career_tot_output_tbl, career_adj_point_stats[ , c('PLAYER_NAME', 'PTS', 'AVG_SEAS_ADJ_PTSPG', 'ADJ_PTS', 'ADJ_PTSPG')],
    by='PLAYER_NAME'
  )
  
  # Season Skater Stats
  skater_stats_output_tbl <- merge(
    skater_adj_goal_stats,
    skater_adj_assist_stats[ , c('SEASON', 'PLAYER_NAME', 'AGE', 'TM', 'POS', 'LEAG_APG_LESSCP', 'ADJFAC_APG', 'ADJ_APG', 'ADJ_A')],
    by=c('SEASON', 'PLAYER_NAME', 'AGE', 'TM', 'POS')
  )
  skater_stats_output_tbl <- merge(
    skater_stats_output_tbl,
    skater_adj_point_stats[ , c('SEASON', 'PLAYER_NAME', 'AGE', 'TM', 'POS', 'LEAG_PTSPG_LESSCP', 'ADJFAC_PTSPG', 'ADJ_PTSPG', 'ADJ_PTS')],
    by=c('SEASON', 'PLAYER_NAME', 'AGE', 'TM', 'POS')
  )

write.csv(career_tot_output_tbl, './career_adjusted_stats.csv', row.names = FALSE)  
write.csv(skater_stats_output_tbl, './season_adjusted_skater_stats.csv', row.names = FALSE)  
  
