season_label_from_start <- function(season_start) {
  season_start <- as.integer(season_start)
  season_end_two_digit <- sprintf("%02d", (season_start + 1L) %% 100L)
  paste0(season_start, "-", season_end_two_digit)
}

load_base_shots <- function(path = "data/NBA_All_Shots.rds") {
  suppressPackageStartupMessages({
    library(dplyr)
    library(janitor)
  })

  raw <- readRDS(path) |> janitor::clean_names()

  if (!"season_1" %in% names(raw)) {
    stop("Expected column season_1 in shot dataset.")
  }

  raw |>
    mutate(
      season = as.integer(season_1),
      season_label = season_label_from_start(season),
      player_name = as.character(player_name),
      team_name = as.character(team_name),
      game_id = as.character(game_id),
      shot_type_std = if_else(grepl("3PT", as.character(shot_type), ignore.case = TRUE), "3PT", "2PT"),
      shot_made = as.logical(shot_made),
      basic_zone_raw = as.character(basic_zone),
      basic_zone_std = case_when(
        grepl("^\\s*Restricted Area\\s*$", basic_zone_raw, ignore.case = TRUE) ~ "Restricted Area",
        grepl("^\\s*In The Paint \\(Non-RA\\)\\s*$", basic_zone_raw, ignore.case = TRUE) ~ "Paint (non-RA)",
        grepl("^\\s*In the Paint \\(non-RA\\)\\s*$", basic_zone_raw, ignore.case = TRUE) ~ "Paint (non-RA)",
        grepl("^\\s*Mid-Range\\s*$", basic_zone_raw, ignore.case = TRUE) ~ "Midrange",
        grepl("^\\s*Midrange\\s*$", basic_zone_raw, ignore.case = TRUE) ~ "Midrange",
        grepl("^\\s*Left Corner 3\\s*$", basic_zone_raw, ignore.case = TRUE) ~ "Corner 3",
        grepl("^\\s*Right Corner 3\\s*$", basic_zone_raw, ignore.case = TRUE) ~ "Corner 3",
        grepl("^\\s*Above the Break 3\\s*$", basic_zone_raw, ignore.case = TRUE) ~ "Above the Break",
        grepl("^\\s*Backcourt\\s*$", basic_zone_raw, ignore.case = TRUE) ~ "Backcourt",
        TRUE ~ "Other"
      ),
      shot_distance = suppressWarnings(as.numeric(shot_distance)),
      loc_x = suppressWarnings(as.numeric(loc_x)),
      loc_y = suppressWarnings(as.numeric(loc_y))
    ) |>
    filter(!is.na(season), !is.na(player_name))
}

load_shot_data <- function(path = "data/NBA_All_Shots.rds", sample_per_group = 3000L) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
  })

  shots <- load_base_shots(path)

  season_summ <- shots |>
    group_by(season, season_label) |>
    summarise(
      fga = n(),
      fgm = sum(shot_made %in% TRUE, na.rm = TRUE),
      fga_3 = sum(shot_type_std == "3PT", na.rm = TRUE),
      fgm_3 = sum(shot_type_std == "3PT" & shot_made %in% TRUE, na.rm = TRUE),
      share_mid = mean(basic_zone_std == "Midrange", na.rm = TRUE),
      share_ra = mean(basic_zone_std == "Restricted Area", na.rm = TRUE),
      share_paint = mean(basic_zone_std == "Paint (non-RA)", na.rm = TRUE),
      share_c3 = mean(basic_zone_std == "Corner 3", na.rm = TRUE),
      share_atb = mean(basic_zone_std == "Above the Break", na.rm = TRUE),
      med_dist = median(shot_distance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      three_rate = if_else(fga > 0, fga_3 / fga, NA_real_),
      efg = if_else(fga > 0, (fgm + 0.5 * fgm_3) / fga, NA_real_)
    ) |>
    arrange(season)

  selection_long <- season_summ |>
    select(season, season_label, share_ra, share_paint, share_mid, share_c3, share_atb) |>
    pivot_longer(c(share_ra, share_paint, share_mid, share_c3, share_atb), names_to = "bucket", values_to = "share") |>
    mutate(
      bucket = factor(
        bucket,
        levels = c("share_ra", "share_paint", "share_mid", "share_c3", "share_atb"),
        labels = c("Restricted Area", "Paint (non-RA)", "Midrange", "Corner 3", "Above the Break")
      )
    )

  non_ra <- selection_long |>
    filter(bucket != "Restricted Area") |>
    group_by(season, season_label) |>
    mutate(share_non_ra = share / sum(share, na.rm = TRUE)) |>
    ungroup()

  sampled_shots <- shots |>
    filter(!is.na(loc_x), !is.na(loc_y), basic_zone_std != "Backcourt") |>
    group_by(season, shot_type_std) |>
    group_modify(~ dplyr::slice_sample(.x, n = min(nrow(.x), sample_per_group))) |>
    ungroup() |>
    transmute(
      season = as.integer(season),
      season_label,
      team_name,
      shot_type = shot_type_std,
      shot_made = if_else(shot_made %in% TRUE, "Made", "Missed"),
      basic_zone = basic_zone_std,
      loc_x,
      loc_y,
      shot_distance
    )

  list(
    shots = shots,
    season_summ = season_summ,
    selection_long = selection_long,
    non_ra = non_ra,
    sampled_shots = sampled_shots
  )
}

build_player_comparison_data <- function(
  path = "data/NBA_All_Shots.rds",
  min_career_fga = 1000L,
  sample_made_per_player_season = 180L
) {
  suppressPackageStartupMessages({
    library(dplyr)
  })

  shots <- load_base_shots(path)

  eligible_players <- shots |>
    count(player_name, name = "career_fga") |>
    filter(career_fga >= min_career_fga)

  shots_eligible <- shots |>
    semi_join(eligible_players, by = "player_name")

  player_year_team <- shots_eligible |>
    count(player_name, season, season_label, team_name, name = "team_fga") |>
    group_by(player_name, season, season_label) |>
    slice_max(team_fga, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(player_name, season, season_label, team_name)

  player_year <- shots_eligible |>
    group_by(player_name, season, season_label) |>
    summarise(
      games = n_distinct(game_id),
      fga = n(),
      fgm = sum(shot_made %in% TRUE, na.rm = TRUE),
      fga3 = sum(shot_type_std == "3PT", na.rm = TRUE),
      fgm3 = sum(shot_type_std == "3PT" & shot_made %in% TRUE, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      fg_pct = if_else(fga > 0, fgm / fga, NA_real_),
      tp_pct = if_else(fga3 > 0, fgm3 / fga3, NA_real_),
      efg = if_else(fga > 0, (fgm + 0.5 * fgm3) / fga, NA_real_),
      three_rate = if_else(fga > 0, fga3 / fga, NA_real_),
      tpa_per_game = if_else(games > 0, fga3 / games, NA_real_)
    ) |>
    left_join(player_year_team, by = c("player_name", "season", "season_label")) |>
    arrange(player_name, season)

  league_year <- shots |>
    group_by(season, season_label) |>
    summarise(
      league_games = n_distinct(game_id),
      league_fga = n(),
      league_fga3 = sum(shot_type_std == "3PT", na.rm = TRUE),
      league_three_rate = if_else(league_fga > 0, league_fga3 / league_fga, NA_real_),
      league_tpa_per_game = if_else(league_games > 0, league_fga3 / league_games, NA_real_),
      league_efg = if_else(
        league_fga > 0,
        (sum(shot_made %in% TRUE, na.rm = TRUE) + 0.5 * sum(shot_type_std == "3PT" & shot_made %in% TRUE, na.rm = TRUE)) / league_fga,
        NA_real_
      ),
      .groups = "drop"
    ) |>
    arrange(season)

  steph_league <- league_year |>
    left_join(
      player_year |>
        filter(player_name == "Stephen Curry") |>
        transmute(
          season,
          steph_fga3 = fga3,
          steph_tpa_per_game = tpa_per_game,
          steph_tp_pct = tp_pct
        ),
      by = "season"
    ) |>
    mutate(
      league_three_rate_index = 100 * league_three_rate / first(league_three_rate),
      steph_tpa_index = 100 * steph_tpa_per_game / first(steph_tpa_per_game)
    )

  made_shots <- shots_eligible |>
    filter(
      shot_made %in% TRUE,
      !is.na(loc_x),
      !is.na(loc_y),
      loc_y >= 0,
      loc_y <= 47,
      abs(loc_x) <= 25
    ) |>
    group_by(player_name, season, shot_type_std) |>
    group_modify(~ dplyr::slice_sample(.x, n = min(nrow(.x), sample_made_per_player_season))) |>
    ungroup() |>
    transmute(
      player_name,
      season = as.integer(season),
      season_label,
      team_name,
      shot_type = shot_type_std,
      basic_zone = basic_zone_std,
      loc_x,
      loc_y,
      shot_distance
    )

  list(
    eligible_players = eligible_players,
    player_year = player_year,
    league_year = league_year,
    steph_league = steph_league,
    made_shots = made_shots
  )
}
