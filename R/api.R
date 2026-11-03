#' Convert a season label to the stored form
#'
#' `"2024/25"`, `"2024-25"`, `"2024/2025"` and `"2425"` all become `"2425"`.
#' Tournament years such as `"2022"` are returned unchanged.
#'
#' @param season A season label, or a vector of them.
#' @return Character vector of stored season codes.
#' @export
normalize_season <- function(season) {
  s <- trimws(as.character(season))
  m <- regmatches(s, regexec("^(\\d{4})\\s*[/-]\\s*(\\d{2}|\\d{4})$", s))
  vapply(seq_along(s), function(i) {
    if (length(m[[i]]) == 3) paste0(substr(m[[i]][2], 3, 4), substr(m[[i]][3], nchar(m[[i]][3]) - 1, nchar(m[[i]][3]))) else s[i]
  }, "")
}

as_chr <- function(x) if (is.null(x)) NULL else as.character(x)

filter_cs <- function(df, comp, season) {
  comps <- as_chr(comp)
  seasons <- if (is.null(season)) NULL else normalize_season(season)
  keep <- rep(TRUE, nrow(df))
  if (!is.null(comps) && "comp" %in% names(df)) keep <- keep & as.character(df$comp) %in% comps
  if (!is.null(seasons) && "season" %in% names(df)) keep <- keep & as.character(df$season) %in% seasons
  df[keep, , drop = FALSE]
}

read_table <- function(name, comp = NULL, season = NULL) {
  df <- arrow::read_parquet(fetch(paste0(name, ".parquet")))
  filter_cs(df, comp, season)
}

read_csv_table <- function(name, comp = NULL, season = NULL) {
  df <- utils::read.csv(fetch(name), colClasses = c(season = "character"), stringsAsFactors = FALSE)
  filter_cs(df, comp, season)
}

check_coverage <- function(comp, season, ids = NULL) {
  if (!is.null(ids) && length(ids) == 0) return(invisible(NULL))
  tryCatch(coverage_report(comp, season, warn = TRUE), error = function(e) NULL)
  invisible(NULL)
}

#' All competitions
#'
#' @return A data frame with code, name, type, seasons and whether each competition is in the v1 scope.
#' @export
competitions <- function() read_table("competitions")

#' Coverage per competition-season
#'
#' Matches played, matches with event data, and known gaps. Never errors for
#' gaps. With `warn = TRUE` it warns when the selection includes matches with
#' no event data or with known data issues.
#'
#' @param comp Competition code(s), or `NULL` for all.
#' @param season Season(s), or `NULL` for all.
#' @param warn Whether to warn about gaps and data issues.
#' @return A data frame.
#' @export
coverage_report <- function(comp = NULL, season = NULL, warn = TRUE) {
  cov <- read_csv_table("coverage.csv", comp, season)
  if (warn && nrow(cov) > 0) {
    gaps <- sum(cov$gaps)
    issues <- sum(cov$matches_with_data_issues)
    if (gaps > 0) warning(sprintf("%d played match(es) in this selection have no event data; see known_gaps()", gaps), call. = FALSE)
    if (issues > 0) warning(sprintf("%d match(es) in this selection have known data issues; see data_issues()", issues), call. = FALSE)
  }
  cov
}

#' Played matches with no event data
#'
#' @inheritParams coverage_report
#' @return A data frame with match id, date, teams and score where known.
#' @export
known_gaps <- function(comp = NULL, season = NULL) read_csv_table("known_gaps.csv", comp, season)

#' Matches with known data issues
#'
#' Partial events, incomplete lineups, or events that disagree with the official score.
#'
#' @inheritParams coverage_report
#' @return A data frame.
#' @export
data_issues <- function(comp = NULL, season = NULL) read_csv_table("data_issues.csv", comp, season)

#' Matches
#'
#' @inheritParams coverage_report
#' @param in_scope_only Keep only competitions in the v1 scope.
#' @return One row per match with teams, scores, venue, referee, managers and formations.
#' @export
matches <- function(comp = NULL, season = NULL, in_scope_only = FALSE) {
  df <- read_table("matches", comp, season)
  if (in_scope_only) df <- df[df$in_v1_scope, , drop = FALSE]
  check_coverage(comp, season)
  df
}

#' Events
#'
#' Every event for one or more competition-seasons. `comp` is required because
#' the full dataset has about 42 million rows.
#'
#' @param comp Competition code(s).
#' @param season Season(s), or `NULL` for every season of the competition.
#' @param columns Optional vector of columns to read; unknown names are skipped.
#' @param player Full player name to keep (capitals and accents are ignored). A misspelling gets a "did you mean" warning.
#' @param player_id Player ID(s), instead of a name.
#' @return A data frame.
#' @export
events <- function(comp, season = NULL, columns = NULL, player = NULL, player_id = NULL) {
  ids <- player_ids(player, player_id)
  if (!is.null(ids) && !is.null(columns) && !"player_id" %in% columns) columns <- c(columns, "player_id")
  comps <- as_chr(comp)
  if (length(comps) == 0) stop("events() needs at least one competition code, for example events('eng', '2425')", call. = FALSE)
  seasons <- if (is.null(season)) NULL else normalize_season(season)
  files <- available_files()
  m <- regmatches(files, regexec("^events_([a-z]+)_(\\d+)\\.parquet$", files))
  pick <- vapply(m, function(x) length(x) == 3 && x[2] %in% comps && (is.null(seasons) || x[3] %in% seasons), TRUE)
  names <- sort(files[pick])
  if (length(names) == 0) stop(sprintf("no event files for comp=%s season=%s; see competitions()", paste(comps, collapse = ","), paste(seasons, collapse = ",")), call. = FALSE)
  frames <- lapply(names, function(n) {
    p <- fetch(n)
    if (is.null(columns)) return(keep_players(arrow::read_parquet(p), ids))
    have <- names(arrow::open_dataset(p)$schema)
    keep_players(arrow::read_parquet(p, col_select = dplyr::all_of(intersect(columns, have))), ids)
  })
  check_coverage(comp, season, ids)
  if (length(frames) == 1) return(frames[[1]])
  dplyr::bind_rows(frames)
}

#' Shots
#'
#' @inheritParams coverage_report
#' @param player Full player name to keep (capitals and accents are ignored). A misspelling gets a "did you mean" warning.
#' @param player_id Player ID(s), instead of a name.
#' @return Every shot with location, body part, situation, outcome, xG and the player who set it up.
#' @export
shots <- function(comp = NULL, season = NULL, player = NULL, player_id = NULL) {
  ids <- player_ids(player, player_id)
  df <- keep_players(read_table("shots", comp, season), ids)
  check_coverage(comp, season, ids)
  df
}

#' Lineups
#'
#' @inheritParams coverage_report
#' @param player Full player name to keep (capitals and accents are ignored). A misspelling gets a "did you mean" warning.
#' @param player_id Player ID(s), instead of a name.
#' @return Every player in every matchday squad: starter or sub, minutes, position.
#' @export
lineups <- function(comp = NULL, season = NULL, player = NULL, player_id = NULL) {
  ids <- player_ids(player, player_id)
  df <- keep_players(read_table("lineups", comp, season), ids)
  check_coverage(comp, season, ids)
  df
}

#' Player match stats
#'
#' @inheritParams coverage_report
#' @param player Full player name to keep (capitals and accents are ignored). A misspelling gets a "did you mean" warning.
#' @param player_id Player ID(s), instead of a name.
#' @return One row per player per match played, with about 80 counts built from the events.
#' @export
player_match_stats <- function(comp = NULL, season = NULL, player = NULL, player_id = NULL) {
  ids <- player_ids(player, player_id)
  df <- keep_players(read_table("player_match_stats", comp, season), ids)
  check_coverage(comp, season, ids)
  df
}

#' Team match stats
#'
#' @inheritParams coverage_report
#' @return One row per team per match, with the same counts plus goals, xG and shots against.
#' @export
team_match_stats <- function(comp = NULL, season = NULL) {
  df <- read_table("team_match_stats", comp, season)
  check_coverage(comp, season)
  df
}

#' Players
#'
#' @return One row per player: name, height, weight, estimated birth year, main position, appearances and minutes.
#' @export
players <- function() read_table("players")

#' Teams
#'
#' @return One row per team: name, country, club or national team, and competitions played.
#' @export
teams <- function() read_table("teams")

#' Formations
#'
#' @inheritParams coverage_report
#' @return Every formation a team used in a match, with the minutes it was used.
#' @export
formations <- function(comp = NULL, season = NULL) {
  df <- read_table("formations")
  if (!is.null(comp) || !is.null(season)) df <- df[df$match_id %in% matches(comp, season)$match_id, , drop = FALSE]
  df
}

#' Cards
#'
#' @inheritParams coverage_report
#' @return Every card shown, with player, team and minute.
#' @export
cards <- function(comp = NULL, season = NULL) {
  df <- read_table("cards")
  if (!is.null(comp) || !is.null(season)) df <- df[df$match_id %in% matches(comp, season)$match_id, , drop = FALSE]
  df
}

#' Player season stats
#'
#' Season totals per player and team, built from [player_match_stats()]. A
#' player who moved clubs mid-season gets one row per team.
#'
#' @inheritParams coverage_report
#' @param per90 Divide every count by minutes played and multiply by 90.
#' @param min_minutes Drop rows below this many minutes.
#' @param player Full player name to keep (capitals and accents are ignored). A misspelling gets a "did you mean" warning.
#' @param player_id Player ID(s), instead of a name.
#' @return A data frame.
#' @export
player_season_stats <- function(comp = NULL, season = NULL, per90 = FALSE, min_minutes = 0, player = NULL, player_id = NULL) {
  ids <- player_ids(player, player_id)
  p <- player_match_stats(comp, season, player_id = if (is.null(ids)) NULL else sort(ids))
  p$comp <- as.character(p$comp)
  p$season <- as.character(p$season)
  p$team <- as.character(p$team)
  p$player <- as.character(p$player)
  id_cols <- c("comp", "season", "player_id", "player", "team_id", "team")
  not_summed <- c("match_id", "date", "opponent_id", "opponent", "side", "position", "is_starter", "played", "in_lineup", "minute_on", "minute_off", "pass_pct", "minutes_played")
  counts <- names(p)[!names(p) %in% c(id_cols, not_summed) & vapply(p, is.numeric, TRUE)]
  p <- dplyr::arrange(p, .data$comp, .data$season, .data$player_id, .data$team_id, .data$date, .data$match_id)
  out <- dplyr::summarise(
    dplyr::group_by(p, .data$comp, .data$season, .data$player_id, .data$team_id),
    player = dplyr::last(.data$player), team = dplyr::last(.data$team),
    matches = dplyr::n_distinct(.data$match_id), starts = sum(.data$is_starter %in% TRUE),
    minutes_played = as.numeric(sum(.data$minutes_played, na.rm = TRUE)),
    dplyr::across(dplyr::all_of(counts), ~ sum(as.numeric(.x), na.rm = TRUE)),
    .groups = "drop"
  )
  out$pass_pct <- floor(1000 * out$passes_completed / ifelse(out$passes > 0, out$passes, NA) + 0.5) / 10
  out <- out[out$minutes_played >= min_minutes, , drop = FALSE]
  if (per90) {
    mins <- ifelse(out$minutes_played > 0, out$minutes_played, NA)
    for (c in counts) out[[c]] <- out[[c]] / mins * 90
  }
  as.data.frame(out)
}
