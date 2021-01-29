library(tidyverse)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(na.tools)
library(ggimage)
library(nflfastR)
library(gt)
library(mgcv)
library(scales)
library(ggforce)
library(remotes)
library(ggtext)

source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_ep_wp.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_cp_cpoe.R")

seasons <- 2013:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

pbp <- pbp %>%
  add_xpass()

pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

pbp_rp <- pbp_rp %>%
  mutate(season = substr(old_game_id, 1, 4))

pbp_rp <- pbp_rp %>%
  mutate(
    posteam = case_when(
      posteam == 'OAK' ~ 'LV',
      posteam == 'SD' ~ 'LAC',
      posteam == 'STL' ~ 'LA',
      TRUE ~ posteam
    )
  )

pbp_rp <- pbp_rp %>%
  mutate(
    defteam = case_when(
      defteam == 'OAK' ~ 'LV',
      defteam == 'SD' ~ 'LAC',
      defteam == 'STL' ~ 'LA',
      TRUE ~ defteam
    )
  )

pass_attempts <- pbp_rp %>%
  filter(pass_attempt == 1, qb_scramble == 0, !is.na(air_yards))

pass_attempts <- pass_attempts %>%
  mutate(season = ifelse(season == 2021, 2020, season))

pass_attempts %>%
  group_by(season, defteam) %>%
  summarize(def_ypa = mean(air_yards),
            count = n()) %>%
  select(-count) -> def_ypa

pass_attempts <- pass_attempts %>%
  left_join(def_ypa, by = c("season", "defteam"))

pass_attempts2 <- pass_attempts %>%
  mutate(yards_aired = ifelse(air_yards > 20, 20L, air_yards))
pass_attempts2 <- pass_attempts2 %>%
  mutate(yards_aired = ifelse(air_yards < -5, 5L, air_yards))
pass_attempts2 <- pass_attempts2 %>%
  mutate(label = yards_aired + 5)
pass_attempts2 <- pass_attempts2 %>%
  mutate(yards_aired = ifelse(air_yards > 20, 20L, air_yards))

pass_attempts2 <- pass_attempts2 %>%
  filter(!is.na(pass_location)) %>%
  mutate(pass_right = ifelse(pass_location == "right", 1, 0),
         pass_left = ifelse(pass_location == "left", 1, 0),
         pass_middle = ifelse(pass_location == "middle", 1, 0))

passes_select <- pass_attempts2 %>%
  select(yardline_100, quarter_seconds_remaining, half_seconds_remaining,
         game_seconds_remaining, qtr, down, shotgun, ep, wp, xpass, 
         def_ypa, pass_right, pass_left, pass_middle, label, ydstogo) %>%
  mutate(label = ifelse(label > 25, 25L, label))

nrounds <- 100
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 26,
    eta = .025,
    gamma = 2,
    subsample=0.8,
    colsample_bytree=0.8,
    max_depth = 4,
    min_child_weight = 1
  )

smp_size <- floor(0.80 * nrow(passes_select))
set.seed(123)
ind <- sample(seq_len(nrow(passes_select)), size = smp_size)
ind_train <- passes_select[ind, ]
ind_test <- passes_select[-ind, ]

full_train <- xgboost::xgb.DMatrix(as.matrix(ind_train %>% select(-label)), label = as.integer(ind_train$label))

xay_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

imp <- xgb.importance(colnames(ind_train), model = xay_model)
xgb.plot.importance(imp)

passes_2020 <- pass_attempts2 %>%
  filter(season == 2020) %>%
  select(yardline_100, quarter_seconds_remaining, half_seconds_remaining,
         game_seconds_remaining, qtr, down, shotgun, ep, wp, xpass, 
         def_ypa, pass_right, pass_left, pass_middle, ydstogo, label) %>%
  mutate(index = 1:n())

xay_2020 <- stats::predict(xay_model,
                            as.matrix(passes_2020 %>%
                                        select(yardline_100, quarter_seconds_remaining, half_seconds_remaining,
                                               game_seconds_remaining, qtr, down, shotgun, ep, wp, xpass, 
                                               def_ypa, pass_right, pass_left, pass_middle, ydstogo))) %>%
  tibble::as_tibble() %>%
  dplyr::rename(prob = "value") %>%
  dplyr::bind_cols(purrr::map_dfr(seq_along(passes_2020$index), function(x) {
  tibble::tibble("xay" = -5:20,
                 "down" = passes_2020$down[[x]],
                 "yardline_100" = passes_2020$yardline_100[[x]],
                 "quarter_seconds_remaining" = passes_2020$quarter_seconds_remaining[[x]],
                 "half_seconds_remaining" = passes_2020$half_seconds_remaining[[x]],
                 "game_seconds_remaining" = passes_2020$game_seconds_remaining[[x]],
                 "qtr" = passes_2020$qtr[[x]],
                 "ydstogo" = passes_2020$ydstogo[[x]],
                 "shotgun" = passes_2020$shotgun[[x]],
                 "pass_right" = passes_2020$pass_right[[x]],
                 "pass_left" = passes_2020$pass_left[[x]],
                 "pass_middle" = passes_2020$pass_middle[[x]],
                 "xpass" = passes_2020$xpass[[x]],
                 "ep" = passes_2020$ep[[x]],
                 "wp" = passes_2020$wp[[x]],
                 "index" = passes_2020$index[[x]]) 
})) %>%
  dplyr::group_by(.data$index) %>%
  dplyr::mutate(max_loss = dplyr::if_else(.data$yardline_100 < 95, -5L, as.integer(.data$yardline_100 - 99L)),
                max_gain = dplyr::if_else(.data$yardline_100 > 20, 20L, as.integer(.data$yardline_100)),
                cum_prob = cumsum(.data$prob),
                prob = dplyr::case_when(.data$xay == .data$max_loss ~ .data$prob,
                                        .data$xay == .data$max_gain ~ 1 - dplyr::lag(.data$cum_prob, 1),
                                        TRUE ~ .data$prob),
                yardline_100 = .data$yardline_100 - .data$xay) %>%
  dplyr::filter(.data$xay >= .data$max_loss, .data$xay <= .data$max_gain) %>%
  dplyr::select(-.data$cum_prob) %>%
  dplyr::summarise(x_air_yards = sum(.data$prob * .data$xay)) %>%
  ungroup() 

x_air_yards <- passes_2020 %>%
  inner_join(xay_2020)

qb_names <- pass_attempts %>%
  filter(season == "2020") %>%
  select(yardline_100, quarter_seconds_remaining, half_seconds_remaining,
         game_seconds_remaining, qtr, down, shotgun, ep, wp, xpass, passer)

pbp_2020 <- pbp_rp %>%
  inner_join(x_air_yards) %>%
  filter(season_type == 'REG') %>%
  select(posteam, defteam, passer, air_yards, x_air_yards, epa) %>%
  mutate(ayoe = air_yards - x_air_yards - 0.72)

passers_2020 <- pbp_2020 %>%
  filter(!is.na(passer)) %>%
  group_by(passer, posteam) %>%
  summarize(passes = n(),
            sum_ayoe = sum(ayoe, na.rm = T),
            avg_ayoe = mean(ayoe, na.rm =T),
            mean_epa = mean(epa, na.rm = T)) %>%
  filter(passes > 180) %>%
  arrange(desc(avg_ayoe))
  
pass_attempts %>%
  nflfastR::decode_player_ids() %>%
  select(desc, name, id)

decoded_pbp <- pass_attempts %>%
  nflfastR::decode_player_ids()

joined <- decoded_pbp %>% 
  filter(!is.na(passer)) %>%
  select(posteam, season, desc, passer, passer_id, epa) %>%
  left_join(roster, by = c('passer_id' = 'gsis_id'))

unique_join <- joined[!duplicated(joined$passer), ]

table_20 <- passers_2020 %>%
  left_join(unique_join, by = "passer")



  



