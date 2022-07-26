library(tidyverse)
library(rvest)

# Creating a Variable for HTML Links
# search_page_one <- "https://caselaw.findlaw.com/search.html?search_type=party&court=us-supreme-court&text=&date_start=20000101&date_end=20220101"
# search_page <- "https://caselaw.findlaw.com/search?search_type=party&court=us-supreme-court&date_start=20000101&date_end=20220101&page="

search_page_one <- "https://caselaw.findlaw.com/search.html?search_type=party&court=us-supreme-court&text=&date_start=17600101&date_end=20230101"
search_page <- "https://caselaw.findlaw.com/search?search_type=party&court=us-supreme-court&date_start=17600101&date_end=20230101&page="

# Initiating Court Cases Data Collection
court_cases_search <- read_html(search_page_one)

# Initiating Court Cases Tibble
court_cases <-
  court_cases_search |>
  html_elements("tbody") |>
  html_elements("a") |>
  html_attr("href") |>
  as_tibble() |>
  rename("link" = "value")

court_cases_titles <-
  court_cases_search |>
  html_elements("tbody") |>
  html_elements("a") |>
  html_attr("title") |>
  as_tibble() |>
  rename("title" = "value")

court_cases <-
  court_cases |>
  bind_cols(court_cases_titles)

# Variable for Defining how many Pages to Iterate through
pages <- c(2:1227)

# For Loops to Collect all Links for Supreme Court Decisions and adding them to
# the Court Case Tibble
for (page in pages) {
  temp <- str_c(search_page, page)
  court <- read_html(temp)
  extract <-
    court |>
    html_elements("tbody") |>
    html_elements("a") |>
    html_attr("href") |>
    as_tibble() |>
    rename("link" = "value")
  extract_titles <-
    court |>
    html_elements("tbody") |>
    html_elements("a") |>
    html_attr("title") |>
    as_tibble() |>
    rename("title" = "value")
  extract <-
    extract |>
    bind_cols(extract_titles)
  for (i in count(extract)) {
    court_cases <- add_row(court_cases, extract)
  }
}

# Writing a CSV with all of the links for Ease of Use
write_csv(court_cases, "court_cases_links_all.csv")