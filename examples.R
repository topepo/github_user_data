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

tm_keywords <- c(
  "tune",
  "caret",
  "parsnip",
  "recipes",
  "torch",
  "rsample"
)

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

# Take a few with fewer repos
test_data <- tibble(github_id = c("Edild", "jrob95", "jbiesanz"))

res <-
  test_data |>
  mutate(
    url = map_chr(
      github_id,
      ~ cli::format_inline("https://github.com/{.x}")
    ),
    # How often has someone puched in the last two weeks?
    pushes_in_2w = map_int(github_id, num_pushes),
    # Pull info on their repos (currently limit: 25 per user)
    repos = map(github_id, query_repos, tm_keywords),
    # Are the interesting keywords in any readme files?
    kw_kits = map_int(repos, kw_hits),
    # How many (up to 25) do they have?
    num_repos = map_int(repos, nrow),
    # How many were made prior to the job listing?
    num_repos_before = map_int(repos, num_old_repos, job_date),
    # How many are not forked?
    num_not_forked = map_int(repos, not_forked),
    # Similar for R and python repos. Note: GH is fairly bad
    # at classifying the repos so these are not as accurate.
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
