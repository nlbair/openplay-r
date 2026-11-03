# openplay (R)

Read the [openplay](https://github.com/nlbair/openplay) soccer event data in R. It covers 26,536 men's matches with every pass, shot and tackle, plus xG on every shot. Files download once from the data release, are checked against published SHA-256 checksums, and are cached on your computer.

The Python package [openplay-py](https://github.com/nlbair/openplay-py) has the same functions and returns the same numbers.

## Install

```r
# install.packages("remotes")
remotes::install_github("nlbair/openplay-r")
```

## Use

```r
library(openplay)

competitions()
m <- matches("eng", "2024/25")
e <- events("eng", "2024/25", columns = c("match_id", "type", "x", "y", "player"))
s <- shots("eng", "2024/25")
p <- player_match_stats("eng", "2024/25")
ps <- player_season_stats("eng", "2024/25", per90 = TRUE, min_minutes = 900)
player_season_stats("esp", "2016/17", player = "Cristiano Ronaldo")
player_career("Lionel Messi")
t <- team_match_stats("eng", "2024/25")
coverage_report("nla")
known_gaps()
data_issues()
```

- **Seasons.** You can write a season as `"2024/25"`, `"2024-25"` or `"2425"`. Tournaments use the year, for example `"2022"`.
- **Players.** `player =` takes the full name. Capitals and accents don't matter. A misspelling gets a warning like *No player named 'Lionel Messsi'. Did you mean: Lionel Messi?* If two players share a name, use `player_id =`.
- **Gaps.** When a request includes matches that have no event data, or have a known data issue, you get a warning. It never stops your code with an error.
- **Local copy.** If you already have the files on disk, `set_data_dir(path)` reads them from there. Setting the `OPENPLAY_DATA_DIR` environment variable does the same.
- **Cache.** Downloads are kept in `cache_dir()`. `clear_cache()` frees the space.

## License

Code: MIT. Data: CC-BY 4.0; credit "openplay (Noah Bair)".
