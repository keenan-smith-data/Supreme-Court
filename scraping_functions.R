# Function for Splitting large datasets
data_split <- function(data, n_groups = 10) {
  temp <- data
  temp$group <- 1:nrow(temp) %% n_groups + 1
  temp_list <- split(temp, temp$group)
  return(temp_list)
}

# Try Error Function
pull_error <- function(cond, hyperlink) {
  message(paste("This URL has caused an error:", hyperlink))
  message(cond)
  tibble::tibble(text = character(), art_title = character(), art_author = character(),
                 art_date = lubridate::date("1970-01-01"), art_link = character())
}

# Try Warning Function
pull_warning <- function(cond, hyperlink) {
  message(paste("URL has a warning:", hyperlink))
  message(cond)
  tibble::tibble(text = character(), art_title = character(), art_author = character(),
                 art_date = lubridate::date("1970-01-01"), art_link = character())
}

# American Mind Article Pull
american_mind_pull <- function(hyperlink) {
  temp <- xml2::read_html(hyperlink)
  art_link <- hyperlink
  art_title <-
    temp |>
    rvest::html_elements(css = ".tam__single-header-title") |>
    rvest::html_text2()
  art_author <-
    temp |>
    rvest::html_elements(css = ".tam__single-header-author") |>
    rvest::html_nodes("a") |>
    rvest::html_text2()
  if (length(art_author) > 1) {
    art_author = paste(art_author, collapse = ", ")
  } else if (length(art_author) == 1) {
    art_author = art_author
  } else {
    art_author = NA
  }
  art_date <-
    temp |>
    rvest::html_elements(css = ".tam__single-header-meta-date") |>
    rvest::html_text2() |>
    stringr::str_replace("\\.", "/") |>
    lubridate::mdy()
  text_data <-
    temp |>
    rvest::html_nodes(".tam__single-content-output") |>
    rvest::html_nodes("p") |>
    rvest::html_text2() |>
    dplyr::as_tibble() |>
    dplyr::rename(text = value) |>
    dplyr::mutate(
      art_title = art_title,
      art_author = art_author,
      art_date = art_date,
      art_link = art_link
    )
  return(text_data)
}

am_mind_pull_try <- function(hyperlink) {
  tryCatch(
    expr = {
      message(paste("Trying", hyperlink))
      american_mind_pull(hyperlink)
    },
    error = function(cond) {
      pull_error(cond, hyperlink)
    },
    warning = function(cond) {
      pull_warning(cond, hyperlink)
    },
    finally = {
      message(paste("Processed URL:", hyperlink))
      Sys.sleep(5)
    }
  )
}

# Jacobin Article Pull
jacobin_pull <- function(hyperlink) {
  temp <- xml2::read_html(hyperlink)
  art_link <- hyperlink
  art_title <-
    temp |>
    rvest::html_elements(css = ".po-hr-cn__title") |>
    rvest::html_text2()
  art_author <-
    temp |>
    rvest::html_elements(css = ".po-hr-cn__author-link") |>
    rvest::html_text2()
  if (length(art_author) > 1) {
    art_author = paste(art_author, collapse = ", ")
  } else if (length(art_author) == 1) {
    art_author = art_author
  } else {
    art_author = NA
  }
  art_date <-
    temp |>
    rvest::html_elements(css = ".po-hr-fl__date") |>
    rvest::html_text2() |>
    stringr::str_replace("\\.", "/") |>
    lubridate::mdy()
  text_data <-
    temp |>
    rvest::html_element(css = "#post-content") |>
    rvest::html_nodes("p") |>
    rvest::html_text2() |>
    dplyr::as_tibble() |>
    dplyr::rename(text = value) |>
    dplyr::mutate(
      art_title = art_title,
      art_author = art_author,
      art_date = art_date,
      art_link = art_link
    )
  return(text_data)
}

jacobin_pull_tests <- function(hyperlink) {
  session <- read_html(hyperlink)
  return(session)
}

j_pull_try <- function(hyperlink) {
  tryCatch(
    expr = {
      message(paste("Trying", hyperlink))
      jacobin_pull(hyperlink)
    },
    error = function(cond) {
      pull_error(cond, hyperlink)
    },
    warning = function(cond) {
      pull_warning(cond, hyperlink)
    },
    finally = {
      message(paste("Processed URL:", hyperlink))
      Sys.sleep(5)
    }
  )
}

# Heritage Article Pull
heritage_com_pull <- function(hyperlink) {
  date_formats <- c("\\w\\w\\w \\d\\w\\w, \\d\\d\\d\\d", "\\w\\w\\w \\d\\d\\w\\w, \\d\\d\\d\\d")
  authors <- c(".author-card__name", "author-card__multi-name")
  temp <- xml2::read_html(hyperlink)
  art_link <- hyperlink
  art_title <-
    temp |>
    rvest::html_elements(css = ".commentary__headline") |>
    rvest::html_text2()
  art_author_1 <-
    temp |>
    rvest::html_nodes(".author-card__name") |>
    rvest::html_text2()
  art_author_2 <-
    temp |>
    rvest::html_nodes(".author-card__multi-name") |>
    rvest::html_text2()
  art_author <- c(art_author_1, art_author_2)
  if (length(art_author) > 1) {
    art_author = paste(art_author, collapse = ", ")
  } else if (length(art_author) == 1) {
    art_author = art_author
  } else {
    art_author = NA
  }
  art_date <-
    temp |>
    rvest::html_elements(css = ".article-general-info") |>
    rvest::html_text2() |>
    stringr::str_extract(paste(date_formats, collapse = "|")) |>
    lubridate::mdy()
  text_data <-
    temp |>
    rvest::html_nodes(".article__body-copy") |>
    rvest::html_nodes("p") |>
    rvest::html_text2() |>
    dplyr::as_tibble() |>
    dplyr::rename(text = value) |>
    dplyr::mutate(
      art_title = art_title,
      art_author = art_author,
      art_date = art_date,
      art_link = art_link
    )
  return(text_data)
}

heritage_pull_test <- function(hyperlink) {
  authors <- c(".author-card__name", ".author-card__multi-name")
  temp <- xml2::read_html(hyperlink)
  art_author_1 <-
    temp |>
    rvest::html_nodes(".author-card__name") |>
    rvest::html_text2()
  art_author_2 <-
    temp |>
    rvest::html_nodes(".author-card__multi-name") |>
    rvest::html_text2()
  art_author <- c(art_author_1, art_author_2)
  if (length(art_author) > 1) {
    art_author = paste(art_author, collapse = ", ")
  } else if (length(art_author) == 1) {
    art_author = art_author
  } else {
    art_author = NA
  }
  return(art_author)
}

h_com_pull_try <- function(hyperlink) {
  tryCatch(
    expr = {
      message(paste("Trying", hyperlink))
      heritage_com_pull(hyperlink)
    },
    error = function(cond) {
      pull_error(cond, hyperlink)
    },
    warning = function(cond) {
      pull_warning(cond, hyperlink)
    },
    finally = {
      message(paste("Processed URL:", hyperlink))
      Sys.sleep(5)
    }
  )
}

# Heritage Article Pull
heritage_rep_pull <- function(hyperlink) {
  date_formats <- c("\\w\\w\\w \\d\\w\\w, \\d\\d\\d\\d", "\\w\\w\\w \\d\\d\\w\\w, \\d\\d\\d\\d")
  temp <- xml2::read_html(hyperlink)
  art_link <- hyperlink
  art_title <-
    temp |>
    rvest::html_elements(css = ".headline") |>
    rvest::html_text2()
  art_author <- "The Heritage Foundation"
  art_date <-
    temp |>
    rvest::html_elements(css = ".article-general-info") |>
    rvest::html_text2() |>
    stringr::str_extract(paste(date_formats, collapse = "|")) |>
    lubridate::mdy()
  text_data <-
    temp |>
    rvest::html_nodes(".article__body-copy") |>
    rvest::html_nodes("p") |>
    rvest::html_text2() |>
    dplyr::as_tibble() |>
    dplyr::rename(text = value) |>
    dplyr::mutate(
      art_title = art_title,
      art_author = art_author,
      art_date = art_date,
      art_link = art_link
    )
  return(text_data)
}

h_rep_pull_try <- function(hyperlink) {
  tryCatch(
    expr = {
      message(paste("Trying", hyperlink))
      heritage_rep_pull(hyperlink)
    },
    error = function(cond) {
      pull_error(cond, hyperlink)
    },
    warning = function(cond) {
      pull_warning(cond, hyperlink)
    },
    finally = {
      message(paste("Processed URL:", hyperlink))
      Sys.sleep(5)
    }
  )
}

# Brookings Article Pull
brookings_pull <- function(hyperlink) {
  temp <- xml2::read_html(hyperlink)
  art_link <- hyperlink
  art_title <-
    temp |>
    rvest::html_elements(css = ".page-content .report-title") |>
    rvest::html_text2()
  art_author <-
    temp |>
    rvest::html_elements(css = ".names") |>
    rvest::html_text2()
  if (length(art_author) > 1) {
    art_author = paste(art_author, collapse = ", ")
  } else if (length(art_author) == 1) {
    art_author = art_author
  } else {
    art_author = NA
  }
  art_date <-
    temp |>
    rvest::html_elements("time[content]") |>
    rvest::html_text2() |>
    lubridate::mdy()
  text_data <-
    temp |>
    rvest::html_element(css = ".post-body") |>
    rvest::html_nodes("p") |>
    rvest::html_text2() |>
    dplyr::as_tibble() |>
    dplyr::rename(text = value) |>
    dplyr::mutate(
      art_title = art_title,
      art_author = art_author,
      art_date = art_date,
      art_link = art_link
    )
  return(text_data)
}

b_pull_tests <- function(hyperlink) {
  temp <- rvest::read_html(hyperlink)
  art_link <- hyperlink
  art_title <-
    temp |>
    rvest::html_elements(css = ".page-content .report-title") |>
    rvest::html_text2()
  return(art_title)
}

b_pull_try <- function(hyperlink) {
  tryCatch(
    expr = {
      message(paste("Trying", hyperlink))
      brookings_pull(hyperlink)
    },
    error = function(cond) {
      pull_error(cond, hyperlink)
    },
    warning = function(cond) {
      pull_warning(cond, hyperlink)
    },
    finally = {
      message(paste("Processed URL:", hyperlink))
      Sys.sleep(5)
    }
  )
}
