library(tidyverse)
library(cli)
library(gh)
library(googlesheets4)
library(furrr)
library(future)
library(lubridate)

# ------------------------------------------------------------------------------

plan("multisession")

job_date <- ymd("2026/03/20")

# ------------------------------------------------------------------------------

source("functions.R")

# ------------------------------------------------------------------------------
# Find some real users as examples to test with

# broom contributors as examples
some_ids <-
  gh::gh(
    "GET /repos/{org}/{repo}/contributors",
    org = "tidymodels",
    repo = "broom",
    .limit = Inf
  ) |>
  map_chr(~ .x$login) |>
  unique()

# Take a few with fewer repos plus me
test_data <- tibble(github_id = c("Edild", "jrob95", "jbiesanz"))

res <-
  test_data |>
  mutate(
    url = map_chr(
      github_id,
      ~ cli::format_inline("https://github.com/{.x}")
    ),
    pushes_in_2w = map_int(github_id, num_pushes),
    repos = map(github_id, query_repos, tm_keywords),
    kw_kits = map_int(repos, kw_hits),
    num_repos = map_int(repos, nrow),
    num_repos_before = map_int(repos, num_old_repos, job_date),
    num_not_forked = map_int(repos, not_forked),
    num_r_not_forked_repos = map_int(repos, repo_kind_count, forked = FALSE),
    num_py_not_forked_repos = map_int(
      repos,
      repo_kind_count,
      type = "python",
      forked = FALSE
    ),
    num_r_repos = map_int(repos, repo_kind_count),
    num_py_repos = map_int(repos, repo_kind_count, type = "python")
  ) |>
  arrange(desc(num_r_not_forked_repos), desc(kw_kits), num_r_repos)
