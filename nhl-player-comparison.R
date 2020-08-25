
#---  SETUP ---#

# Packages
library(tidyverse)
library(rvest)
library(plyr)

# Functions
'%notin%' <- Negate('%in%')


#---  ACQUIRE DATA FOR ANALYSIS ---#

# data used in this analysis was scraped from hockey-reference.com using the below code
# some data cleaning was performed in excel on the scraped .csv files afterwards (removing/adding columns, changing variable names etc.)
# copies of the cleaned datasets can be downloaded directly from Skillalytics.com or GitHub.
  # https://github.com/skillalytics/nhl-player-scoring-comparison/tree/master/data


# Skater Basic Stats
ssp_s <- 'https://www.hockey-reference.com/leagues/NHL_'
ssp_e <- '_skaters.html'
ssp_yrs <- c(1918:2004, 2006:2020) # select years to scrape (2005 not included due to lockout, loop will return error if included)

href_sbs <- data.frame()

for (i in ssp_yrs){
  
  sbs_url <- read_html(paste(ssp_s, i, ssp_e, sep=""))
  
  df_sbs <- sbs_url %>%
    html_node('table#stats') %>%
    html_table()
  names(df_sbs) <- as.matrix(df_sbs[1, ])
  df_sbs <- df_sbs[-1, ]
  df_sbs <-df_sbs[!(df_sbs$Rk=="Rk"),]
  df_sbs <- cbind(Season = i, df_sbs)
  
  href_sbs <- rbind(href_sbs, df_sbs)
  df_sbs <- NULL
  
}


# League Standings
stsp_s <- 'https://www.hockey-reference.com/leagues/NHL_'
stsp_e <- '_standings.html'

# due to changes in conference names within the league, table names on hockey-reference.com change
# therefore, the loop below is setup to be run by conference eras and can be rbind together afterwards
# based on the timeframe you are scraping change the table names accordingly:
  # no conf from 1918-1974 (#standings), !remove conference B scrape for this era
  # clarence campbell and prince of wales conf from 1975-1993 (#standings_CAM; #standings_WAL)
  # east and west conf from 1994+ (#standings_EAS; #standings_WES)

# stsp_yrs <- c(1918:1974) # select years to scrape
# stsp_yrs <- c(1975:1993) # select years to scrape
stsp_yrs <- c(1994:2020) # select years to scrape

href_stnd_temp <- data.frame()

for (i in stsp_yrs){
  
  stnd_url <- read_html(paste(stsp_s, i, stsp_e, sep=""))
  
  # conference A
  df_stnd <- stnd_url %>%
    # select the desired table
    # html_node('table#standings') %>% 
    # html_node('table#standings_CAM') %>% 
    html_node('table#standings_EAS') %>%
    html_table()
  df_stnd <- cbind(Season = i, df_stnd)
  
  # conference B
  df_stnd <- stnd_url %>%
    # select the desired table
    # html_node('table#standings_WAL') %>% 
    html_node('table#standings_WES') %>%    # select the desired table
    html_table()
  df_stnd <- cbind(Season = i, df_stnd)
  
  if (i > 1960) {
    df_stnd <- head(stnd_url,-3)
  } else {
    df_stnd <- head(stnd_url,-2)
  }
  
  href_stnd_temp <- rbind(href_stnd_temp, df_stnd, df_stnd)
  df_stnd <- NULL
  
}

# if running for multiple conference eras than run the first line below after first iteration of loop
# and the second line below after each subsequent loop
# !only run the first line once, do not run first line after second or third iteration as it will overwrite df
  # href_stnd <- href_stnd_temp #first line
  # href_stnd <- rbind(href_stnd, href_stnd_temp) #second line


#---  DATA PREP ---#

# load dataset if already scraped and saved in project folder
href_sbs <- read.csv('./skater_basic_stats.csv')
href_stnd <- read.csv('./league_standings.csv')

# !as mentioned under the acquistion section, some data cleansing activities were performed in excel on the scraped .csv files prior to running the below code

# Skater Stats
skater_stats <- href_sbs %>%
  filter(
    SEASON >= 1940
  ) %>%
  select(
    SEASON, PLAYER_NAME, AGE, TM, POS, GP, G, A, PTS
  ) %>%
  mutate(
    GPG = round(G/GP, digits=4),
    APG = round(A/GP, digits=4),
    PTSPG = round(PTS/GP, digits=4)
  )

# Standings
leag_stand <- href_stnd %>%
  select(
    SEASON, TM, GP, GF, GA
  )


#---  ANALYSIS ---#

# Calculate Career Stats by Player
career_stats <- skater_stats %>%
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

  # Create Table of Career 100 Goal Scorers
  career_100G_list <- career_stats %>%
    filter(
      G_TOT >= 100
      ) %>%
    select(
      PLAYER_NAME
    )
  # career_100G <- career_100G[[1]]
  
  # Create Table of Career 150 Assist Players
  career_150A <- career_stats %>%
    filter(
      A_TOT >= 150
    )
  
  # Create Table of Career 200 Point Players
  career_200PTS <- career_stats %>%
    filter(
      PTS_TOT >= 200
    )

  # the cut-off numbers above by career were arbitrarily chosen
  # in essence we are only interested in looking at players who have had some form of career success
  # however we don't want this number to be so low that it eliminates younger players in the league which may be of interest
  
# Calculate Season Data
teams_per_season <- leag_stand %>%
  count(SEASON)
colnames(teams_per_season) <- c('SEASON', 'TEAM_COUNT')

games_played_by_season <- leag_stand %>%
  group_by(SEASON) %>%
  summarise(
    TM_GP_TOT = sum(GP)
  )

players_per_season <- skater_stats %>%
  count(SEASON)
colnames(players_per_season) <- c('SEASON', 'PLAYER_COUNT')

season_stats <- skater_stats %>%
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

season_dat <- cbind(teams_per_season, games_played_by_season, players_per_season, season_stats) %>%
  select(-c(3,5,7))

season_dat <- add_column(season_dat,
    LEAG_GP = season_dat$TM_GP_TOT/2, .after = 3 # calculate number of games played league wide per season (each team plays one another)
  )

season_dat <- season_dat %>%
  mutate(
     LEAG_GPG = G_TOT/LEAG_GP,
     LEAG_APG = A_TOT/LEAG_GP,
     LEAG_PTSPG = PTS_TOT/LEAG_GP,
  )

# Calculate Adjustment Factors
adj_baseline <- season_dat %>%
  filter(SEASON == 2020) %>%
  select(LEAG_GPG, LEAG_APG, LEAG_PTSPG)

season_dat <- season_dat %>%
  mutate(
    ADJFAC_GPG = as.numeric(adj_baseline[1])/LEAG_GPG,
    ADJFAC_APG = as.numeric(adj_baseline[2])/LEAG_APG,
    ADJFAC_PTSPG = as.numeric(adj_baseline[3])/LEAG_PTSPG,
  )

# Add Adjustment Factors to skater_stats and Calculate Adj Stats
skater_stats <- merge(
  skater_stats, season_dat[ , c('SEASON', 'ADJFAC_GPG', 'ADJFAC_APG', 'ADJFAC_PTSPG')], by='SEASON', all.x = TRUE
  )

skater_stats <- skater_stats %>%
  mutate(
    ADJ_GPG = GPG*ADJFAC_GPG,
    ADJ_APG = APG*ADJFAC_APG,
    ADJ_PTSPG = PTSPG*ADJFAC_PTSPG
  )

career_adj_stats <- skater_stats %>%
  group_by(PLAYER_NAME) %>%
  summarise(
    AVG_ADJ_GPG = mean(ADJ_GPG),
    AVG_ADJ_APG = mean(ADJ_APG),
    AVG_ADJ_PTSPG = mean(ADJ_PTSPG)
  )

career_stats <- merge(
  career_stats, career_adj_stats, by='PLAYER_NAME', all = TRUE
  )

# Sort by Leaders
  #GPG
  career_stats <- subset(career_stats, G_TOT >= 100)
  career_stats <- career_stats[order(-career_stats$AVG_ADJ_GPG),]
  rownames(career_stats) <- 1:nrow(career_stats)
  #APG
  career_stats <- career_stats[order(-career_stats$AVG_ADJ_APG),]
  #PTSPG
  career_stats <- career_stats[order(-career_stats$AVG_ADJ_PTSPG),]

  
  
  
###-- CALCULATED WITH 100CG PLAYERS REMOVED FROM ADJUSTMENT --###

# this method is testing what our results would look like if we were to remove the players we are trying to analyze
# the idea behind it is that the excellent performance of someone who leads the league in scoring by a significant margin is being hurt by the analysis from their own production
# their high scoring leads to a high GPG for that season which causes the adjustment factor to adjust downward based on the baseline, therefore bringing down their GPG in that season
  
  skater_stats_non100cg <- skater_stats[skater_stats$PLAYER_NAME %notin% career_100G_list$PLAYER_NAME, ]
  
  # Calculate Season Data
  
  # teams_per_season <- leag_stand %>%
  #   count(SEASON)
  # colnames(teams_per_season) <- c('SEASON', 'TEAM_COUNT')
  # 
  # games_played_by_season <- leag_stand %>%
  #   group_by(SEASON) %>%
  #   summarise(
  #     TM_GP_TOT = sum(GP)
  #   )
  
  players_per_season_non100cg <- skater_stats_non100cg %>%
    count(SEASON)
  colnames(players_per_season_non100cg) <- c('SEASON', 'PLAYER_COUNT')
  
  season_stats_non100cg <- skater_stats_non100cg %>%
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
  
  season_dat_non100cg <- cbind(teams_per_season, games_played_by_season, players_per_season_non100cg, season_stats_non100cg) %>%
    select(-c(3,5,7))
  
  season_dat_non100cg <- add_column(season_dat_non100cg,
                           LEAG_GP = season_dat_non100cg$TM_GP_TOT/2, .after = 3 # calculate number of games played league wide per season (each team plays one another)
  )
  
  season_dat_non100cg <- season_dat_non100cg %>%
    mutate(
      LEAG_GPG = G_TOT/LEAG_GP,
      LEAG_APG = A_TOT/LEAG_GP,
      LEAG_PTSPG = PTS_TOT/LEAG_GP,
    )
  
    # Plotting GPG by Season w/ All Players vs 100CG Removed
    season_dat <- add_column(season_dat, INCLD100CG = "Y", .before=0)
    season_dat_non100cg <- add_column(season_dat_non100cg, INCLD100CG = "N", .before=0)
    season_dat_plot <- bind_rows(season_dat, season_dat_non100cg)
      
      ggplot(data = season_dat_plot, aes(x=SEASON, y=LEAG_GPG, group=INCLD100CG)) +
      geom_line(aes(color=INCLD100CG)) +
      geom_point(aes(color=INCLD100CG))
  
  # Calculate Adjustment Factors
  adj_baseline_non100cg <- season_dat_non100cg %>%
    filter(SEASON == 2020) %>%
    select(LEAG_GPG, LEAG_APG, LEAG_PTSPG)
  
  season_dat_non100cg <- season_dat_non100cg %>%
    mutate(
      ADJFAC_GPG = as.numeric(adj_baseline_non100cg[1])/LEAG_GPG,
      ADJFAC_APG = as.numeric(adj_baseline_non100cg[2])/LEAG_APG,
      ADJFAC_PTSPG = as.numeric(adj_baseline_non100cg[3])/LEAG_PTSPG
    )
  
  # Add Adjustment Factors to skater_stats and Calculate Adj Stats
  skater_stats_incld100cg <- merge(
    skater_stats, season_dat_non100cg[ , c('SEASON', 'ADJFAC_GPG', 'ADJFAC_APG', 'ADJFAC_PTSPG')], by='SEASON', all.x = TRUE
  )
  skater_stats_incld100cg <- skater_stats_incld100cg[-c(13:18)]
  colnames(skater_stats_incld100cg)[13:15] <- c("ADJFAC_GPG", "ADJFAC_APG", "ADJFAC_PTSPG")
  
  skater_stats_incld100cg <- skater_stats_incld100cg %>%
    mutate(
      ADJ_GPG = GPG*ADJFAC_GPG,
      ADJ_APG = APG*ADJFAC_APG,
      ADJ_PTSPG = PTSPG*ADJFAC_PTSPG
    )
  
  career_adj_stats_incld100cg <- skater_stats_incld100cg %>%
    group_by(PLAYER_NAME) %>%
    summarise(
      AVG_ADJ_GPG = mean(ADJ_GPG),
      AVG_ADJ_APG = mean(ADJ_APG),
      AVG_ADJ_PTSPG = mean(ADJ_PTSPG)
    )
  
  career_stats_incld100cg <- merge(
    career_stats, career_adj_stats_incld100cg, by='PLAYER_NAME', all = TRUE
  )
  career_stats_incld100cg <-career_stats_incld100cg[-c(10:11)]
  
  # Sort by Leaders
  #GPG
  career_stats_incld100cg <- career_stats_incld100cg[order(-career_stats_incld100cg$AVG_ADJ_GPG.y),]

# the above shows this method also has its own problems
# the main being that because we only want to look at top career goal scorers, a significant amount of goals each season are being removed from earlier seasons
# while there are young players currently who will soon or eventually be on the 100CG list, most players who have only been in the league ~1-4yrs do not show up
# this causes seasons within the last 5yrs or so the be adjusted downward drastically all while lifting up goal scorers from 1970-2000
# its unlikely this would be solved by changing the group removed. Higher leads to more goals removed in a certain timeframe
# and lower undercuts the basis of the analysis as it won't accurately capture scoring rates by season
  

  
  
###-- CALCULATED WITH TOP GOAL SCORERS BY SEASON REMOVED FROM ADJUSTMENT --###  

# next we will try to remove only the goals each season from the top scorers

  # Plot G & GPG by Season to Find Possible Outliers or Trends
  ggplot(skater_stats, aes(x=SEASON, y=G)) + geom_point()
  ggplot(skater_stats, aes(x=SEASON, y=GPG)) + geom_point()
  
  ggplot(skater_stats, aes(x=SEASON, y=A)) + geom_point()
  
  ggplot(skater_stats, aes(x=SEASON, y=PTS)) + geom_point()
  
  # Locate and Remove Outliers
  
    # Goals
    g_outlier_tbl <- skater_stats %>%
      select(SEASON, G) %>%
      group_by(SEASON) %>%
      summarise(
        MEAN = mean(G),
        SD = sd(G)
      ) %>%
      mutate(
        '2SD_OVER_MEAN' = MEAN+(SD*2),
        '3SD_OVER_MEAN' = MEAN+(SD*3),
        '4SD_OVER_MEAN' = MEAN+(SD*4)
      )
    
    skater_stats_outliers_removed <- merge(skater_stats, g_outlier_tbl, by='SEASON', all=TRUE)
    skater_stats_outliers_removed <- skater_stats_outliers_removed %>%
      subset(
        G < `3SD_OVER_MEAN`
      )
    
    ggplot(skater_stats_outliers_removed, aes(x=SEASON, y=G)) + geom_point()
    
    # Assists
    a_outlier_tbl <- skater_stats %>%
      select(SEASON, A) %>%
      group_by(SEASON) %>%
      summarise(
        MEAN = mean(A),
        SD = sd(A)
      ) %>%
      mutate(
        '2SD_OVER_MEAN' = MEAN+(SD*2),
        '3SD_OVER_MEAN' = MEAN+(SD*3),
        '4SD_OVER_MEAN' = MEAN+(SD*4)
      )
    
    skater_stats_assist_outliers_removed <- merge(skater_stats, a_outlier_tbl, by='SEASON', all=TRUE)
    skater_stats_assist_outliers_removed <- skater_stats_assist_outliers_removed %>%
      subset(
        A < `3SD_OVER_MEAN`
      )
    
    ggplot(skater_stats_assist_outliers_removed, aes(x=SEASON, y=A)) + geom_point()
    
    # Points
    pts_outlier_tbl <- skater_stats %>%
      select(SEASON, PTS) %>%
      group_by(SEASON) %>%
      summarise(
        MEAN = mean(PTS),
        SD = sd(PTS)
      ) %>%
      mutate(
        '2SD_OVER_MEAN' = MEAN+(SD*2),
        '3SD_OVER_MEAN' = MEAN+(SD*3),
        '4SD_OVER_MEAN' = MEAN+(SD*4)
      )
    
    skater_stats_point_outliers_removed <- merge(skater_stats, pts_outlier_tbl, by='SEASON', all=TRUE)
    skater_stats_point_outliers_removed <- skater_stats_point_outliers_removed %>%
      subset(
        PTS < `3SD_OVER_MEAN`
      )
    
    ggplot(skater_stats_point_outliers_removed, aes(x=SEASON, y=PTS)) + geom_point()

  # Calculate Season Data
  
  # teams_per_season <- leag_stand %>%
  #   count(SEASON)
  # colnames(teams_per_season) <- c('SEASON', 'TEAM_COUNT')
  # 
  # games_played_by_season <- leag_stand %>%
  #   group_by(SEASON) %>%
  #   summarise(
  #     TM_GP_TOT = sum(GP)
  #   )
  
    # Goals
    players_per_season_outliers_removed <- skater_stats_outliers_removed %>%
      count(SEASON)
    colnames(players_per_season_outliers_removed) <- c('SEASON', 'PLAYER_COUNT')
    
    season_stats_outliers_removed <- skater_stats_outliers_removed %>%
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
    
    season_dat_outliers_removed <- cbind(teams_per_season, games_played_by_season, players_per_season_outliers_removed, season_stats_outliers_removed) %>%
      select(-c(3,5,7))
    
    season_dat_outliers_removed <- add_column(season_dat_outliers_removed,
                             LEAG_GP = season_dat_outliers_removed$TM_GP_TOT/2, .after = 3 # calculate number of games played league wide per season (each team plays one another)
    )
    
    season_dat_outliers_removed <- season_dat_outliers_removed %>%
      mutate(
        LEAG_GPG = G_TOT/LEAG_GP,
        LEAG_APG = A_TOT/LEAG_GP,
        LEAG_PTSPG = PTS_TOT/LEAG_GP,
      )
    
    # Assists
    players_per_season_assist_outliers_removed <- skater_stats_assist_outliers_removed %>%
      count(SEASON)
    colnames(players_per_season_assist_outliers_removed) <- c('SEASON', 'PLAYER_COUNT')
    
    season_stats_assist_outliers_removed <- skater_stats_assist_outliers_removed %>%
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
    
    season_dat_assist_outliers_removed <- cbind(teams_per_season, games_played_by_season, players_per_season_assist_outliers_removed, season_stats_assist_outliers_removed) %>%
      select(-c(3,5,7))
    
    season_dat_assist_outliers_removed <- add_column(season_dat_assist_outliers_removed,
                                              LEAG_GP = season_dat_assist_outliers_removed$TM_GP_TOT/2, .after = 3 # calculate number of games played league wide per season (each team plays one another)
    )
    
    season_dat_assist_outliers_removed <- season_dat_assist_outliers_removed %>%
      mutate(
        LEAG_GPG = G_TOT/LEAG_GP,
        LEAG_APG = A_TOT/LEAG_GP,
        LEAG_PTSPG = PTS_TOT/LEAG_GP,
      )
    
    # Points
    players_per_season_point_outliers_removed <- skater_stats_point_outliers_removed %>%
      count(SEASON)
    colnames(players_per_season_point_outliers_removed) <- c('SEASON', 'PLAYER_COUNT')
    
    season_stats_point_outliers_removed <- skater_stats_point_outliers_removed %>%
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
    
    season_dat_point_outliers_removed <- cbind(teams_per_season, games_played_by_season, players_per_season_point_outliers_removed, season_stats_point_outliers_removed) %>%
      select(-c(3,5,7))
    
    season_dat_point_outliers_removed <- add_column(season_dat_point_outliers_removed,
                                                     LEAG_GP = season_dat_point_outliers_removed$TM_GP_TOT/2, .after = 3 # calculate number of games played league wide per season (each team plays one another)
    )
    
    season_dat_point_outliers_removed <- season_dat_point_outliers_removed %>%
      mutate(
        LEAG_GPG = G_TOT/LEAG_GP,
        LEAG_APG = A_TOT/LEAG_GP,
        LEAG_PTSPG = PTS_TOT/LEAG_GP,
      )
  
  # Calculate Adjustment Factors
    
    # Goals
    adj_baseline_outliers_removed <- season_dat_outliers_removed %>%
      filter(SEASON == 2020) %>%
      select(LEAG_GPG, LEAG_APG, LEAG_PTSPG)
    
    season_dat_outliers_removed <- season_dat_outliers_removed %>%
      mutate(
        ADJFAC_GPG = as.numeric(adj_baseline_outliers_removed[1])/LEAG_GPG,
      )
    
    # Assists
    adj_baseline_assist_outliers_removed <- season_dat_assist_outliers_removed %>%
      filter(SEASON == 2020) %>%
      select(LEAG_GPG, LEAG_APG, LEAG_PTSPG)
    
    season_dat_assist_outliers_removed <- season_dat_assist_outliers_removed %>%
      mutate(
        ADJFAC_APG = as.numeric(adj_baseline_assist_outliers_removed[2])/LEAG_APG,
      )
    
    # Points
    adj_baseline_point_outliers_removed <- season_dat_point_outliers_removed %>%
      filter(SEASON == 2020) %>%
      select(LEAG_GPG, LEAG_APG, LEAG_PTSPG)
    
    season_dat_point_outliers_removed <- season_dat_point_outliers_removed %>%
      mutate(
        ADJFAC_PTSPG = as.numeric(adj_baseline_point_outliers_removed[3])/LEAG_PTSPG,
      )
  
  # Add Adjustment Factors to skater_stats and Calculate Adj Stats
    
    # Goals
    skater_stats_outliers_removed <- merge(
      skater_stats, season_dat_outliers_removed[ , c('SEASON', 'ADJFAC_GPG')], by='SEASON', all.x = TRUE
    )
    
    skater_stats_outliers_removed <- skater_stats_outliers_removed %>%
      mutate(
        ADJ_GPG = GPG*ADJFAC_GPG.y
      )
    
    career_adj_stats_outliers_removed <- skater_stats_outliers_removed %>%
      group_by(PLAYER_NAME) %>%
      summarise(
        AVG_ADJ_GPG = mean(ADJ_GPG)
      )
    
    career_stats_outliers_removed <- merge(
      career_stats, career_adj_stats_outliers_removed, by='PLAYER_NAME', all = TRUE
    )
    
    # Sort by Leaders
    #GPG
    career_stats_outliers_removed <- subset(career_stats_outliers_removed, G_TOT >= 100)
    career_stats_outliers_removed <- career_stats_outliers_removed[order(-career_stats_outliers_removed$AVG_ADJ_GPG.y),]
    rownames(career_stats_outliers_removed) <- 1:nrow(career_stats_outliers_removed)
  
    # Assists
    skater_stats_assist_outliers_removed <- merge(
      skater_stats, season_dat_assist_outliers_removed[ , c('SEASON', 'ADJFAC_APG')], by='SEASON', all.x = TRUE
    )
    
    skater_stats_assist_outliers_removed <- skater_stats_assist_outliers_removed %>%
      mutate(
        ADJ_APG = APG*ADJFAC_APG.y
      )
    
    career_adj_stats_assist_outliers_removed <- skater_stats_assist_outliers_removed %>%
      group_by(PLAYER_NAME) %>%
      summarise(
        AVG_ADJ_APG = mean(ADJ_APG)
      )
    
    career_stats_assist_outliers_removed <- merge(
      career_stats, career_adj_stats_assist_outliers_removed, by='PLAYER_NAME', all = TRUE
    )
    
    # Sort by Leaders
    #APG
    career_stats_assist_outliers_removed <- subset(career_stats_assist_outliers_removed, A_TOT >= 150)
    career_stats_assist_outliers_removed <- career_stats_assist_outliers_removed[order(-career_stats_assist_outliers_removed$AVG_ADJ_APG.y),]
    rownames(career_stats_assist_outliers_removed) <- 1:nrow(career_stats_assist_outliers_removed)
    
    # Points
    skater_stats_point_outliers_removed <- merge(
      skater_stats, season_dat_point_outliers_removed[ , c('SEASON', 'ADJFAC_PTSPG')], by='SEASON', all.x = TRUE
    )
    
    skater_stats_point_outliers_removed <- skater_stats_point_outliers_removed %>%
      mutate(
        ADJ_PTSPG = PTSPG*ADJFAC_PTSPG.y
      )
    
    career_adj_stats_point_outliers_removed <- skater_stats_point_outliers_removed %>%
      group_by(PLAYER_NAME) %>%
      summarise(
        AVG_ADJ_PTSPG = mean(ADJ_PTSPG)
      )
    
    career_stats_point_outliers_removed <- merge(
      career_stats, career_adj_stats_point_outliers_removed, by='PLAYER_NAME', all = TRUE
    )
    
    # Sort by Leaders
    #PTSPG
    career_stats_point_outliers_removed <- subset(career_stats_point_outliers_removed, G_TOT >= 100)
    career_stats_point_outliers_removed <- career_stats_point_outliers_removed[order(-career_stats_point_outliers_removed$AVG_ADJ_PTSPG.y),]
    rownames(career_stats_point_outliers_removed) <- 1:nrow(career_stats_point_outliers_removed)
    
    
    
    
  