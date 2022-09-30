# Function for Testing if Data is Valid from scrape
scrape_check <- function(df) {
  if (length(df) >= 1 &
      is.character(df$text) == TRUE &
      is.character(df$art_title) == TRUE &
      is.character(df$art_author) == TRUE &
      lubridate::is.Date(df$art_date) == TRUE &
      is.character(df$art_topic) == TRUE &
      is.character(df$art_link) == TRUE &
      is.character(df$art_source) == TRUE) {
    message("Data Valid")
  } else if (length(df) == 0 &
             is.character(df$text) == TRUE &
             is.character(df$art_title) == TRUE &
             is.character(df$art_author) == TRUE &
             lubridate::is.Date(df$art_date) == TRUE &
             is.character(df$art_topic) == TRUE &
             is.character(df$art_link) == TRUE &
             is.character(df$art_source) == TRUE) {
    warning("No Valid Data")
  } else if (length(df) >= 1 &
             is.character(df$text) == FALSE &
             is.character(df$art_title) == TRUE &
             is.character(df$art_author) == TRUE &
             lubridate::is.Date(df$art_date) == TRUE &
             is.character(df$art_topic) == TRUE &
             is.character(df$art_link) == TRUE &
             is.character(df$art_source) == TRUE) {
    warning("Issue w/ Text Data")
  } else if (length(df) >= 1 &
             is.character(df$text) == TRUE &
             is.character(df$art_title) == FALSE &
             is.character(df$art_author) == TRUE &
             lubridate::is.Date(df$art_date) == TRUE &
             is.character(df$art_topic) == TRUE &
             is.character(df$art_link) == TRUE &
             is.character(df$art_source) == TRUE) {
    warning("Issue w/ Title Data")
  } else if (length(df) >= 1 &
             is.character(df$text) == TRUE &
             is.character(df$art_title) == TRUE &
             is.character(df$art_author) == FALSE &
             lubridate::is.Date(df$art_date) == TRUE &
             is.character(df$art_topic) == TRUE &
             is.character(df$art_link) == TRUE &
             is.character(df$art_source) == TRUE) {
    warning("Issue w/ Author Data")
  } else if (length(df) >= 1 &
             is.character(df$text) == TRUE &
             is.character(df$art_title) == TRUE &
             is.character(df$art_author) == TRUE &
             lubridate::is.Date(df$art_date) == FALSE &
             is.character(df$art_topic) == TRUE &
             is.character(df$art_link) == TRUE &
             is.character(df$art_source) == TRUE) {
    warning("Issue w/ Date Data")
  } else if (length(df) >= 1 &
             is.character(df$text) == TRUE &
             is.character(df$art_title) == TRUE &
             is.character(df$art_author) == TRUE &
             lubridate::is.Date(df$art_date) == TRUE &
             is.character(df$art_topic) == FALSE &
             is.character(df$art_link) == TRUE &
             is.character(df$art_source) == TRUE) {
    warning("Issue w/ Topic Data")
  } else if (length(df) >= 1 &
             is.character(df$text) == TRUE &
             is.character(df$art_title) == TRUE &
             is.character(df$art_author) == TRUE &
             lubridate::is.Date(df$art_date) == TRUE &
             is.character(df$art_topic) == TRUE &
             is.character(df$art_link) == FALSE &
             is.character(df$art_source) == TRUE) {
    warning("Issue w/ Link Data")
  } else if (length(df) >= 1 &
             is.character(df$text) == TRUE &
             is.character(df$art_title) == TRUE &
             is.character(df$art_author) == TRUE &
             lubridate::is.Date(df$art_date) == TRUE &
             is.character(df$art_topic) == TRUE &
             is.character(df$art_link) == TRUE &
             is.character(df$art_source) == FALSE) {
    warning("Issue w/ Source Data")
  } else {
    (
      warning("Nothing is Initialized, Link is probably bad")
    )
  }
}