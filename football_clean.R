library(rpart)
library(rpart.plot)
library(tidyverse)
library(pROC)
library(reshape2)
library(nflfastR)

pbp <- load_pbp(2019:2024)
fourth <-  pbp %>% filter(down == 4)
fourth <- fourth %>% filter(play_type %in% c('pass','run'))

fourth_clean <- fourth %>% clean_pbp()

fourth_sub <- fourth_clean %>% select(c(away_coach,away_score,away_team,away_timeouts_remaining,away_wp,cp,def_wp,defteam,defteam_score,defteam_timeouts_remaining,
                                        div_game,down,drive,drive_game_clock_start,drive_quarter_start,drive_real_start_time,drive_start_transition,
                                        ep,fg_prob,fourth_down_converted,game_date,game_half,game_stadium,goal_to_go,
                                        home_coach,home_score,home_team,home_timeouts_remaining,home_wp,lateral_reception,lateral_rush,location,no_huddle,no_score_prob,
                                        opp_fg_prob,opp_safety_prob,opp_td_prob,pass_attempt,pass_length,pass_location,pass_oe,passer,
                                        play_type,posteam,posteam_score,posteam_timeouts_remaining,posteam_type,qb_dropback,qtr,quarter_seconds_remaining,
                                        receiver,roof,run_gap,run_location,rush_attempt,rusher,score_differential,season,season_type,series,shotgun,
                                        side_of_field,spread_line,stadium,start_time,surface,temp,total,total_away_comp_air_epa,total_away_comp_air_wpa,
                                        total_away_comp_yac_epa,total_away_comp_yac_wpa,total_away_epa,total_away_rush_epa,total_away_pass_epa,total_away_pass_wpa,
                                        total_away_raw_air_epa,total_away_raw_air_wpa,total_away_raw_yac_epa,total_away_raw_yac_wpa,total_away_rush_epa,
                                        total_away_rush_wpa,total_away_score,total_home_comp_air_epa,total_home_comp_air_wpa,total_home_comp_yac_epa,
                                        total_home_comp_yac_wpa,total_home_epa,total_home_pass_epa,total_home_pass_wpa,total_home_raw_air_epa,total_home_raw_air_wpa,
                                        total_home_raw_yac_epa,total_home_raw_yac_wpa,total_home_rush_epa,total_home_rush_wpa,total_home_score,total_line,vegas_home_wp,
                                        vegas_wp,week,wind,wp,yardline_100,ydstogo))

fourth_sub <- fourth_clean %>% select(c(away_coach,away_score,away_team,away_timeouts_remaining,away_wp,cp,def_wp,defteam,defteam_score,defteam_timeouts_remaining,
                                        div_game,down,drive,drive_game_clock_start,drive_quarter_start,drive_real_start_time,drive_start_transition,
                                        ep,fg_prob,fourth_down_converted,game_date,game_half,game_stadium,goal_to_go,
                                        home_coach,home_score,home_team,home_timeouts_remaining,home_wp,lateral_reception,lateral_rush,location,no_huddle,no_score_prob,
                                        opp_fg_prob,opp_safety_prob,opp_td_prob,pass_attempt,pass_length,pass_location,pass_oe,passer,
                                        play_type,posteam,posteam_score,posteam_timeouts_remaining,posteam_type,qb_dropback,qtr,quarter_seconds_remaining,
                                        receiver,roof,run_gap,run_location,rush_attempt,rusher,score_differential,season,season_type,series,shotgun,
                                        side_of_field,spread_line,stadium,start_time,surface,temp,total,total_away_epa,total_away_score,
                                        total_home_epa,total_home_score,total_line,vegas_home_wp,vegas_wp,week,wind,wp,yardline_100,ydstogo))

rm(fourth)
rm(fourth_clean)

# CLEAN UP THIS SECTION + ADD DOCUMENTATION - COMBINE HOME/AWAY FOR POSTEAM

# fourth_down_converted - factor for simplicity
# cp - introduce complete separation
# div_game - factor for simplicity
# game_clock - same essence as quarter_seconds_remaining but with the drive start
# drive_start - the hour in which the drive started in
# goal_to_go - factor for simplicity
# lateral_reception - factor for simplicity
# lateral_rush - factor for simplicity
# no_huddle - factor for simplicity
# temp - average indoor temp for indoor games
# wind - 0 wind for indoor games
# playcall - playtype and location
# player - player who touches the ball
fourth_sub <- fourth_sub %>% mutate(fourth_down_converted = factor(fourth_down_converted, levels = c('0','1'), labels = c('No','Yes'))
                                    ,cp = if_else(is.na(cp) == TRUE, 0, cp)
                                    ,div_game = factor(div_game, levels = c('0','1'), labels = c('No','Yes'))
                                    ,drive_game_clock_start = as.numeric(seconds(ms(drive_game_clock_start)))
                                    ,drive_real_start_time = as.numeric(hour(ymd_hms(drive_real_start_time)))
                                    ,game_date = year(game_date)
                                    ,goal_to_go = factor(goal_to_go, levels = c('0','1'), labels = c('No','Yes'))
                                    ,lateral_reception = factor(lateral_reception, levels = c('0','1'), labels = c('No','Yes'))
                                    ,lateral_rush = factor(lateral_rush, levels = c('0','1'), labels = c('No','Yes'))
                                    ,no_huddle = factor(no_huddle, levels = c('0','1'), labels = c('No','Yes'))
                                    ,pass_attempt = factor(pass_attempt, levels = c('0','1'), labels = c('No','Yes'))
                                    ,rush_attempt = factor(rush_attempt, levels = c('0','1'), labels = c('No','Yes'))
                                    ,start_time = as.numeric(hour(mdy_hms(start_time)))
                                    ,temp = if_else(is.na(temp) == TRUE, 72, temp)
                                    ,wind = if_else(is.na(wind) == TRUE, 0, wind)
                                    ,total_posteam_epa = if_else(posteam_type == 'home', total_home_epa, total_away_epa)
                                    ,total_defteam_epa = if_else(posteam_type == 'home', total_away_epa, total_home_epa)
                                    ,playcall = if_else(play_type == 'pass',paste0(play_type,if_else(is.na(pass_length) == TRUE,'',paste0(' ',pass_length)),if_else(is.na(pass_location) == TRUE,'',paste0(' ',pass_location))),paste0(play_type,if_else(is.na(run_location) == TRUE,'',paste0(' ',run_location)),if_else(is.na(run_gap) == TRUE,'',paste0(' ',run_gap))))
                                    ,target_player = if_else(play_type == 'pass', if_else(is.na(receiver) == TRUE, passer, receiver), if_else(is.na(rusher) == TRUE, passer, rusher))) %>% 
  rename(game_year = game_date)

sapply(fourth_sub %>% select_if(function(x) is.factor(x) || is.character(x)), function(x) length(unique(x)) > 53)

fourth_final <- fourth_sub %>% select(-c(pass_length,pass_location,receiver,run_gap,run_location,rusher,away_coach,home_coach,passer,target_player)) %>% filter(is.na(drive_real_start_time) == FALSE)
