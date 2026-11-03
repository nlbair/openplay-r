sample_dir <- function() {
  env <- Sys.getenv("OPENPLAY_SAMPLE_DIR", "")
  if (nzchar(env)) return(env)
  system.file("extdata", "sample", package = "openplay")
}

expected <- jsonlite::fromJSON(file.path(sample_dir(), "expected.json"))
old <- set_data_dir(sample_dir())
withr_defer <- function() set_data_dir(old)

quiet <- function(expr) suppressWarnings(expr)

test_that("season labels normalize", {
  expect_equal(normalize_season("2024/25"), "2425")
  expect_equal(normalize_season("2024-2025"), "2425")
  expect_equal(normalize_season("2425"), "2425")
  expect_equal(normalize_season(2022), "2022")
})

test_that("competitions and matches match the reference numbers", {
  expect_equal(nrow(competitions()), expected$n_competitions)
  m <- quiet(matches())
  expect_equal(nrow(m), expected$n_matches)
  expect_equal(sum(m$home_score) + sum(m$away_score), expected$total_goals)
  expect_equal(nrow(matches("usc")), expected$n_matches_usc)
})

test_that("events read and filter", {
  e <- quiet(events("nla", "2022/23"))
  expect_equal(nrow(e), expected$n_events_nla_2223)
  expect_equal(sum(as.character(e$type) == "Pass"), expected$n_passes_nla_2223)
  small <- events("usc", columns = c("match_id", "type", "xg", "not_a_column"))
  expect_equal(names(small), c("match_id", "type", "xg"))
  expect_error(events(NULL))
})

test_that("shots and stats match the reference numbers", {
  s <- quiet(shots())
  expect_equal(nrow(s), expected$n_shots)
  expect_equal(sum(as.numeric(s$xg), na.rm = TRUE), expected$sum_xg, tolerance = 1e-6)
  lu <- quiet(lineups())
  expect_equal(nrow(lu), expected$n_lineup_rows)
  p <- quiet(player_match_stats())
  expect_equal(nrow(p), expected$n_player_match_rows)
  expect_equal(sum(p$minutes_played), expected$sum_minutes)
  expect_equal(sum(p$goals), expected$sum_player_goals)
  expect_equal(sum(p$assists), expected$sum_player_assists)
  expect_equal(sum(as.numeric(p$xa)), expected$sum_player_xa, tolerance = 1e-6)
  g <- stats::aggregate(goals ~ player_id, data = data.frame(player_id = p$player_id, goals = p$goals), FUN = sum)
  g <- g[order(-g$goals, g$player_id), ]
  expect_equal(g$player_id[1], expected$top_scorer_player_id)
  expect_equal(g$goals[1], expected$top_scorer_goals)
  t <- quiet(team_match_stats())
  expect_equal(sum(t$passes), expected$sum_team_passes)
  expect_equal(nrow(t), 2 * expected$n_matches)
})

test_that("coverage warns and never errors", {
  expect_warning(cov <- coverage_report("nla", "2223"), "no event data")
  expect_equal(sum(cov$gaps), expected$gaps_nla_2223)
  expect_warning(matches("nla", "2223"))
  expect_no_warning(coverage_report("not_a_comp"))
  expect_equal(nrow(known_gaps()), expected$n_known_gaps)
})

test_that("player season stats match the reference numbers", {
  s <- quiet(player_season_stats("nla", "2223"))
  s90 <- quiet(player_season_stats("nla", "2223", per90 = TRUE, min_minutes = 180))
  expect_equal(nrow(s), expected$n_season_rows_nla_2223)
  expect_equal(sum(s$minutes_played), expected$sum_season_minutes_nla_2223)
  expect_equal(sum(s$goals), expected$sum_season_goals_nla_2223)
  expect_equal(nrow(s90), expected$n_season_rows_nla_2223_min180)
  best <- s90[order(-s90$xg, s90$player_id), ][1, ]
  expect_equal(best$player_id, expected$top_xg90_player_id_nla_2223)
  expect_equal(best$xg, expected$top_xg90_nla_2223, tolerance = 1e-5)
})

test_that("player filter and career match the reference numbers", {
  expect_equal(name_key("  Kylian  Mbappé "), "kylian mbappe")
  m <- quiet(player_match_stats("nla", "2022/23", player = "kylian mbappe"))
  s <- quiet(shots("usc", player = "Lionel Messi"))
  c <- quiet(player_career("Luka Modric"))
  expect_equal(nrow(m), expected$mbappe_matches_nla_2223)
  expect_equal(nrow(s), expected$messi_shots_usc)
  expect_equal(nrow(c), expected$modric_career_rows)
  expect_equal(sum(c$goals), expected$modric_career_goals)
  expect_equal(sum(c$minutes_played), expected$modric_career_minutes)
  expect_false(is.unsorted(c$first_match))
})

test_that("misspelled players get suggestions", {
  expect_warning(r <- player_season_stats(player = "Lionel Messsi"), "Did you mean: Lionel Messi")
  expect_equal(nrow(r), 0)
  expect_equal(suggest_players("Messi")[1], "Lionel Messi")
  expect_warning(r2 <- player_career("Zzzz Qqqq"), "Check the spelling")
  expect_equal(nrow(r2), 0)
})

test_that("players and teams load", {
  expect_gt(nrow(players()), 0)
  expect_gt(nrow(teams()), 0)
})

withr_defer()
