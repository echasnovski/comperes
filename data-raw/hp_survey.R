library(dplyr)
library(tidyr)
library(lubridate)


# Functions ---------------------------------------------------------------
append_ind <- function(x, ind) {
  lapply(x, function(i) {
    c(i, ind)
  })
}

compute_batch_num <- function(n) {
  n <- as.integer(n)
  if (n <= 1) {
    return(1)
  }

  # This is how books are generated in the original survey form
  batches <- list(1)
  for (i in seq_len(n - 1) + 1) {
    batches <- c(batches, list(i), append_ind(batches, i))
  }

  # Compute batch numbers
  batch_len <- sapply(batches, length)

  unlist(mapply(rep, seq_along(batches), batch_len, SIMPLIFY = FALSE))
}

hp_repair_colnames <- function(tbl) {
  books <- colnames(tbl)[-(1:2)]
  batch_num <- compute_batch_num(n = 7)
  books_out <- paste0(
    batch_num, "-",
    gsub("^[^0-9]*([1-7]).*$", "HP_\\1", books)
  )

  res_names <- c("time", "batch", books_out)
  colnames(tbl) <- res_names

  tbl
}


# Data creation -----------------------------------------------------------
hp_survey <- read.csv(
  file = file.path("data-raw", "hp_survey.csv"),
  header = TRUE, stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate_all(as.character) %>%
  mutate(
    Time = as.POSIXct(
      Time,
      format = "%Y/%m/%d %I:%M:%S %p", tz = "Europe/Kiev"
    ),
    Time = lubridate::with_tz(Time, "UTC")
  ) %>%
  hp_repair_colnames() %>%
  mutate(person = seq_len(n())) %>%
  gather("batch_book", "score", -time, -batch, -person) %>%
  filter(!((score == "") | is.na(score))) %>%
  separate(batch_book, into = c("new_batch", "book"), sep = "-")

# Test for correct batch number imputation. Should be TRUE
hp_survey %>% summarise(all_batch_equal = all(batch == new_batch))

hp_survey <- hp_survey %>%
  select(person, book, score) %>%
  arrange(person, book)

save(hp_survey, file = file.path("data", "hp_survey.rda"), compress = "bzip2")
