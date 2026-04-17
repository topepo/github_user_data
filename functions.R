#library(tidyverse)
#library(cli)
#library(gh)
#library(googlesheets4)
#library(furrr)
#library(future)

find_gh_user <- function(id) {
  require(gh)
  res <- try(gh::gh("GET /users/{username}", username = id), silent = TRUE)
  inherits(res, "gh_response")
}

query_repos <- function(user, kw) {
  require(gh)
  require(tidyverse)

  no_res <- tibble(
    name = NA_character_,
    user = user,
    forked = rlang::na_lgl,
    language = NA_character_
  )
  if (is.na(user)) {
    return(no_res)
  }

  ref <- cli::format_inline("/users/{user}/repos")
  res <- try(gh(ref, .limit = 25), silent = TRUE)
  if (inherits(res, "try-error")) {
    cli::cli_inform("Repo list failed for {.val {user}}")
    return(no_res)
  } else {
    res <- map(res, ~ repo_info(.x, user = user, kw = kw)) %>% list_rbind()
  }

  # Avoid Waiting Xs for retry backoff
  Sys.sleep(2)
  res
}

get_contrib_info <- function(repo, user) {
  require(gh)
  require(tidyverse)

  allcontributors::get_contributors(repo = repo, org = user, quiet = TRUE) |>
    filter(type == "code") |>
    mutate(is_user = logins == user) |>
    mutate(
      contrib_total = is_user * contributions,
      contrib_percent = contrib_total / sum(contributions) * 100
    ) |>
    filter(is_user) |>
    select(contrib_total, contrib_percent)
}

get_number_of_same_name <- function(repo, user) {
  require(gh)
  require(tidyverse)

  results <- gh("GET /search/repositories", q = repo)
  is_same_name <- map_lgl(
    results[[3]],
    ~ .x$name == repo & .x$owner$login != user & !.x$fork
  )
  tibble(num_same_name = sum(is_same_name))
}

parse_readme <- function(repo, user) {
  require(gh)
  require(tidyverse)

  base_url <- glue::glue(
    "https://raw.githubusercontent.com/{user}/{repo}/refs/heads/main/README"
  )

  quiet_read <- purrr::quietly(readLines)

  res <- try(quiet_read(base_url), silent = TRUE)
  if (inherits(res, "try-error")) {
    res <- try(quiet_read(paste0(base_url, ".txt")), silent = TRUE)
    if (inherits(res, "try-error")) {
      res <- try(quiet_read(paste0(base_url, ".md")), silent = TRUE)
      if (inherits(res, "try-error")) {
        res <- list(result = character(0))
      }
    }
  }
  res <- res$result
  res <- res[res != ""]
  res <- try(tolower(res), silent = TRUE)
  if (inherits(res, "try-error")) {
    res <- character(0)
  }

  list(res)
}

repo_info <- function(x, user, kw) {
  require(gh)
  require(tidyverse)

  if (is.null(names(x))) {
    res <- tibble(
      repo = NA_character_,
      user = user,
      created = POSIXct(0),
      forked = rlang::na_lgl,
      language = NA_character_,
      readme = list()
    )
  } else {
    repo <- x$name
    readme <- parse_readme(repo, user)
    kw_res <- map_dfr(readme, kw_counts, kw = kw)
    res <-
      tibble(
        repo = repo,
        user = user,
        created = lubridate::ymd_hms(x$created_at),
        forked = x$fork,
        language = x$language,
        readme = readme
      )
    res <- bind_cols(res, kw_res)

    # Get user contribution info
    ctrb <- try(get_contrib_info(repo, user), silent = TRUE)
    if (!inherits(ctrb, "try-error")) {
      res <- bind_cols(res, ctrb)
    }

    # Are their other repos with the same name?
    num_named <- try(get_number_of_same_name(repo, user), silent = TRUE)
    if (!inherits(num_named, "try-error")) {
      res <- bind_cols(res, num_named)
    }
  }

  res
}

contribs_at_repo <- function(user, repos) {
  require(gh)
  require(tidyverse)

  target <-
    purrr::map(
      repos,
      ~ glue::glue(
        # Maximum count is 100 for efficiency
        "https://api.github.com/repos/{.x}/commits?author={user}&per_page=100&page=1"
      )
    ) |>
    purrr::map(gh::gh) |>
    purrr::map_int(length)
  total <- sum(target, na.rm = TRUE)
  names(target) <- paste0(basename(repos), "_commits")
  names(target) <- gsub("-", "_", names(target))
  tibble::as_tibble_row(target) |>
    dplyr::mutate(gh_id = user) |>
    dplyr::mutate(any_py_repo_commits = total > 0) |>
    dplyr::relocate(any_py_repo_commits)
}

kw_counts <- function(text, kw) {
  require(gh)
  require(tidyverse)

  if (length(kw) == 0) {
    res <- tibble::tibble(kw_none = 0L)
  } else {
    counts <- map_int(kw, ~ length(grep(.x, text)))
    names(counts) <- paste0("kw_", gsub(" ", "_", kw))
    res <- tibble::as_tibble_row(counts)
  }
  res
}

not_forked <- function(x) {
  if (nrow(x) == 0) {
    res <- 0L
  } else {
    res <- sum(!x$forked, na.rm = TRUE)
  }
  res
}

repo_kind_count <- function(x, type = "R", forked = TRUE) {
  res <- 0L
  if (any(names(x) == "language")) {
    is_type <- x$language == type
    is_type[is.na(is_type)] <- FALSE
    if (!forked) {
      is_type <- is_type & !x$forked
    }
    res <- sum(is_type)
  }

  res
}

num_pushes <- function(user, lag = 14) {
  since_date <- format(Sys.Date() - lag, "%Y-%m-%dT%H:%M:%SZ")

  # Get repositories sorted by most recently pushed
  repos <-
    gh(
      "GET /users/{username}/repos",
      username = user,
      sort = "pushed",
      direction = "desc",
      .limit = Inf
    )

  push_dates <-
    purrr::map(repos, ~ .x$pushed_at) |>
    purrr::map_vec(lubridate::ymd_hms)

  sum(push_dates >= since_date)
}

kw_hits <- function(repos) {
  repos |>
    dplyr::mutate(any_kw = sum(c_across(starts_with("kw_")))) |>
    dplyr::summarize(kw_hits = sum(any_kw)) |>
    pluck("kw_hits")
}

num_old_repos <- function(repos, job_date) {
  if (nrow(repos) == 0 | all(names(repos) != "created")) {
    return(0L)
  }
  repos |> dplyr::filter(created > job_date) |> nrow()
}

reassess_kw <- function(repos, kw) {
  map_dfr(repos, ~ kw_counts(.x$readme, kw = kw)) |>
    rowwise() |>
    dplyr::mutate(kw_hits = sum(c_across(starts_with("kw_")))) |>
    ungroup() |>
    pluck("kw_hits")
}
