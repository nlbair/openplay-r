## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes for reviewers

* The package downloads data files from a public GitHub release (https://github.com/nlbair/openplay/releases) on request and caches them under tools::R_user_dir(). Examples and tests use a small sample shipped in inst/extdata and need no internet connection.
* If a download fails, functions stop with an informative message.
