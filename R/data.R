#' Example competition results from 2005 NCAA football season
#'
#' `ncaa2005` is an example competition results of an isolated group of
#' Atlantic Coast Conference teams provided in book "Who's #1" by Langville and
#' Meyer.
#'
#' @format An object of class [longcr] containing information
#'   about 10 games.
"ncaa2005"

#' Results of Harry Potter Books Survey
#'
#' `hp_survey` contains results of the survey with a goal to collect data enough
#' to rate Harry Potter books.
#'
#' @details Survey was done via
#' [Google Forms](https://www.google.com/intl/en/forms/about/) service. To
#' participate in it, respondent is asked to log in into her/his Google account
#' (to ensure that one person takes part only once). It was popularized mostly
#' among R users via [R-bloggers](https://www.r-bloggers.com/) and
#' [Twitter](https://twitter.com/).
#'
#' At the beginning of the survey, there was the following text:
#'
#' _This is a survey with goal to collect data enough to rate Harry Potter books.
#' Data will be made public with complete anonymity of respondents. Please, take
#' part only if you have read all seven original J. K. Rowling Harry Potter
#' books and are willing to give an honest feedback about your impressions._
#'
#' Analyzed books were coded with the following names:
#' - “HP and the Philosopher’s (Sorcerer’s) Stone (#1)”.
#' - “HP and the Chamber of Secrets (#2)”.
#' - “HP and the Prisoner of Azkaban (#3)”.
#' - “HP and the Goblet of Fire (#4)”.
#' - “HP and the Order of the Phoenix (#5)”.
#' - “HP and the Half-Blood Prince (#6)”.
#' - “HP and the Deathly Hallows (#7)”.
#'
#' Survey had the following procedure:
#' - At first, respondent is asked to choose the first element in the randomly
#' shuffled list of number from 1 to 127. This simulates the random generation
#' of books subset in the next question.
#' - Next he/she is presented with a question "What is your impression of these
#' Harry Potter BOOKS?" (singular if there is one book) and the following
#' question grid:
#'     - Rows represent randomly shuffled subset of books corresponding to the
#'     number chosen in the first step.
#'     - Columns contain the following scale of answers: “1 - Poor”, “2 - Fair”,
#'     “3 - Good”, “4 - Very Good”, “5 - Excellent”. Respondent is asked and
#'     allowed to choose only one answer per book (every book should be rated).
#'
#' @format A [tibble][tibble::tibble] with answers from 182 respondents and the
#'   following columns:
#' - __person__ <int>: Identifier of a person.
#' - __book__ <chr>: Identifier of a Harry Potter book. Its values are of the
#' form "HP_x" where "x" represents book's number in the series (from 1 to 7).
#' - __score__ <chr>: Book's score. Can be one of "1 - Poor", "2 - Fair", "3 -
#' Good", "4 - Very Good", "5 - Excellent".
#'
#' Rows are ordered by person and then by book identifier.
"hp_survey"
