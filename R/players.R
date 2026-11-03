#' Simplify a name for matching
#'
#' Lower-cases a name and strips accents, so `"Kylian Mbappé"` and
#' `"kylian mbappe"` match.
#'
#' @param name A name, or a vector of names.
#' @return Character vector.
#' @export
name_key <- function(name) {
  s <- stringi::stri_trans_general(as.character(name), "Latin-ASCII")
  tolower(trimws(gsub("[[:space:]]+", " ", s)))
}

player_keys <- function() {
  pl <- players()[, c("player_id", "player")]
  pl <- pl[!is.na(pl$player_id) & !is.na(pl$player), , drop = FALSE]
  pl$player <- as.character(pl$player)
  pl$key <- name_key(pl$player)
  pl
}

#' Suggest full player names
#'
#' The full names closest to `name`: small spelling mistakes, or names that
#' contain every word typed.
#'
#' @param name What was typed.
#' @param n Maximum number of suggestions.
#' @return Character vector of full names.
#' @export
suggest_players <- function(name, n = 5) {
  key <- name_key(name)
  pl <- player_keys()
  pl <- pl[!duplicated(pl$key), , drop = FALSE]
  limit <- max(2, nchar(key) %/% 4)
  words <- unique(strsplit(key, " ", fixed = TRUE)[[1]])
  words <- words[nzchar(words)]
  partial <- if (length(words) == 0) rep(FALSE, nrow(pl)) else vapply(strsplit(pl$key, " ", fixed = TRUE), function(w) all(words %in% w), TRUE)
  d <- as.vector(utils::adist(key, pl$key))
  keep <- partial | d <= limit
  hits <- data.frame(rank = ifelse(partial, 0, 1), d = d, key = pl$key, player = pl$player, stringsAsFactors = FALSE)[keep, , drop = FALSE]
  hits <- hits[order(hits$rank, hits$d, hits$key, method = "radix"), , drop = FALSE]
  utils::head(hits$player, n)
}

player_ids <- function(player, player_id) {
  if (!is.null(player_id)) return(as.integer(player_id))
  if (is.null(player)) return(NULL)
  pl <- player_keys()
  ids <- integer(0)
  for (name in as.character(player)) {
    found <- pl[pl$key == name_key(name), , drop = FALSE]
    if (nrow(found) == 0) {
      sug <- suggest_players(name)
      msg <- if (length(sug)) sprintf("No player named '%s'. Did you mean: %s?", name, paste(sug, collapse = ", ")) else sprintf("No player named '%s'. Check the spelling (use the full name) or search players().", name)
      warning(msg, call. = FALSE)
      next
    }
    fid <- sort(unique(as.integer(found$player_id)))
    if (length(fid) > 1) warning(sprintf("%d players are named '%s' (player_id %s); all are included. Use player_id= to pick one.", length(fid), name, paste(fid, collapse = ", ")), call. = FALSE)
    ids <- c(ids, fid)
  }
  unique(ids)
}

keep_players <- function(df, ids) {
  if (is.null(ids)) return(df)
  out <- df[!is.na(df$player_id) & df$player_id %in% ids, , drop = FALSE]
  if (length(ids) > 0 && nrow(out) == 0) {
    pl <- players()
    pl <- pl[pl$player_id %in% ids, , drop = FALSE]
    names <- paste(sprintf("%s (player_id %d)", as.character(pl$player), as.integer(pl$player_id)), collapse = ", ")
    warning(sprintf("Found %s, but there are no rows for them in this selection (they may not have played in this competition or season); try player_career().", names), call. = FALSE)
  }
  out
}

#' A player's whole career
#'
#' One row per competition, season and team, oldest first. `player` is the
#' full name (capitals and accents are ignored). A close misspelling gets a
#' "did you mean" warning and an empty result.
#'
#' @param player Full player name.
#' @param player_id Player ID, instead of a name.
#' @param per90 Divide every count by minutes played and multiply by 90.
#' @return A data frame.
#' @export
player_career <- function(player = NULL, player_id = NULL, per90 = FALSE) {
  if (is.null(player) && is.null(player_id)) stop("player_career() needs a full player name or a player_id, for example player_career('Lionel Messi')", call. = FALSE)
  ids <- player_ids(player, player_id)
  if (length(ids) == 0) return(data.frame(player_id = integer(0), player = character(0), comp = character(0), season = character(0), team_id = integer(0), team = character(0), first_match = as.Date(character(0))))
  out <- suppressWarnings(player_season_stats(per90 = per90, player_id = ids))
  pm <- suppressWarnings(player_match_stats(player_id = ids))
  pm$comp <- as.character(pm$comp)
  pm$season <- as.character(pm$season)
  first <- dplyr::summarise(dplyr::group_by(pm, .data$comp, .data$season, .data$player_id, .data$team_id), first_match = min(.data$date), .groups = "drop")
  out <- dplyr::left_join(out, first, by = c("comp", "season", "player_id", "team_id"))
  out <- out[order(out$player_id, out$first_match, out$comp, out$team_id, method = "radix"), , drop = FALSE]
  cov <- coverage_report(warn = FALSE)
  pairs <- paste(out$comp, out$season)
  gaps <- sum(cov$gaps[paste(cov$comp, cov$season) %in% pairs])
  if (gaps > 0) warning(sprintf("%d played match(es) in this player's competition-seasons have no event data, so the totals may miss some of their matches; see known_gaps()", gaps), call. = FALSE)
  front <- c("player_id", "player", "comp", "season", "team_id", "team", "first_match")
  out <- out[, c(front, setdiff(names(out), front)), drop = FALSE]
  rownames(out) <- NULL
  out
}
