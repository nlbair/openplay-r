.op <- new.env(parent = emptyenv())
.op$data_dir <- NULL
.op$checksums <- NULL
.op$assets <- NULL

data_repo <- function() Sys.getenv("OPENPLAY_REPO", "nlbair/openplay")
data_tag <- function() Sys.getenv("OPENPLAY_DATA_TAG", "data-v0.1.0")

#' Read tables from a local folder instead of downloading them
#'
#' The folder must hold the release files side by side, for example
#' `matches.parquet` and `events_eng_2425.parquet`. Use `NULL` to go back to
#' downloading from the GitHub release.
#'
#' @param path Folder path, or `NULL`.
#' @return The previous setting, invisibly.
#' @examples
#' set_data_dir(system.file("extdata", "sample", package = "openplay"))
#' nrow(competitions())
#' set_data_dir(NULL)
#' @export
set_data_dir <- function(path) {
  old <- .op$data_dir
  .op$data_dir <- if (is.null(path)) NULL else normalizePath(path, mustWork = TRUE)
  .op$checksums <- NULL
  invisible(old)
}

local_data_dir <- function() {
  if (!is.null(.op$data_dir)) return(.op$data_dir)
  env <- Sys.getenv("OPENPLAY_DATA_DIR", "")
  if (nzchar(env)) env else NULL
}

#' Folder where downloaded files are kept between sessions
#'
#' @return Path to the cache folder for the current data release.
#' @export
cache_dir <- function() {
  base <- Sys.getenv("OPENPLAY_CACHE_DIR", "")
  if (!nzchar(base)) base <- tools::R_user_dir("openplay", which = "cache")
  path <- file.path(base, data_tag())
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

#' Delete every downloaded file for the current data release
#'
#' @return `NULL`, invisibly.
#' @export
clear_cache <- function() {
  unlink(cache_dir(), recursive = TRUE)
  .op$checksums <- NULL
  invisible(NULL)
}

gh_token <- function() {
  for (v in c("OPENPLAY_GITHUB_TOKEN", "GITHUB_TOKEN", "GH_TOKEN")) {
    t <- Sys.getenv(v, "")
    if (nzchar(t)) return(t)
  }
  NULL
}

http_get <- function(url, dest, accept = NULL) {
  h <- curl::new_handle()
  headers <- list("User-Agent" = "openplay-r")
  tok <- gh_token()
  if (!is.null(tok)) headers[["Authorization"]] <- paste("Bearer", tok)
  if (!is.null(accept)) headers[["Accept"]] <- accept
  do.call(curl::handle_setheaders, c(list(h), headers))
  tmp <- paste0(dest, ".part")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  ok <- tryCatch({
    curl::curl_download(url, tmp, handle = h, quiet = TRUE)
    TRUE
  }, error = function(e) {
    stop(sprintf("Could not download %s from GitHub (%s). Check your internet connection, or point set_data_dir() at local copies of the files.", basename(dest), conditionMessage(e)), call. = FALSE)
  })
  file.rename(tmp, dest)
  dest
}

asset_urls <- function() {
  if (!is.null(.op$assets) && identical(attr(.op$assets, "tag"), data_tag())) return(.op$assets)
  tmp <- tempfile(fileext = ".json")
  http_get(sprintf("https://api.github.com/repos/%s/releases/tags/%s", data_repo(), data_tag()), tmp, "application/vnd.github+json")
  rel <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  urls <- stats::setNames(vapply(rel$assets, function(a) a$url, ""), vapply(rel$assets, function(a) a$name, ""))
  attr(urls, "tag") <- data_tag()
  .op$assets <- urls
  urls
}

download_asset <- function(name, dest) {
  if (!is.null(gh_token())) {
    urls <- asset_urls()
    if (!name %in% names(urls)) stop(sprintf("%s is not in release %s of %s", name, data_tag(), data_repo()), call. = FALSE)
    http_get(urls[[name]], dest, "application/octet-stream")
  } else {
    http_get(sprintf("https://github.com/%s/releases/download/%s/%s", data_repo(), data_tag(), name), dest)
  }
}

sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

checksums <- function() {
  if (!is.null(.op$checksums)) return(.op$checksums)
  local <- local_data_dir()
  if (!is.null(local)) {
    p <- file.path(local, "checksums.json")
    .op$checksums <- if (file.exists(p)) jsonlite::fromJSON(p, simplifyVector = FALSE) else list()
    return(.op$checksums)
  }
  p <- file.path(cache_dir(), "checksums.json")
  if (!file.exists(p)) download_asset("checksums.json", p)
  .op$checksums <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  .op$checksums
}

available_files <- function() {
  local <- local_data_dir()
  if (!is.null(local)) return(sort(list.files(local)))
  sort(names(checksums()))
}

fetch <- function(name) {
  local <- local_data_dir()
  if (!is.null(local)) {
    p <- file.path(local, name)
    if (!file.exists(p)) stop(sprintf("%s not found in %s", name, local), call. = FALSE)
    return(p)
  }
  sums <- checksums()
  if (!name %in% names(sums)) stop(sprintf("%s is not part of data release %s", name, data_tag()), call. = FALSE)
  p <- file.path(cache_dir(), name)
  if (file.exists(p) && identical(sha256_file(p), sums[[name]]$sha256)) return(p)
  download_asset(name, p)
  if (!identical(sha256_file(p), sums[[name]]$sha256)) {
    unlink(p)
    stop(sprintf("checksum mismatch for %s; the download was removed, please try again", name), call. = FALSE)
  }
  p
}

#' @importFrom rlang .data
NULL
