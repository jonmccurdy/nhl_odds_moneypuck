library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)

api_key <- "5282f67c29680a6e2eb36b79b1305ac5"

odds_url <- paste0(
  "https://api.the-odds-api.com/v4/sports/icehockey_nhl/odds/?",
  "apiKey=", api_key, "&",
  "regions=us&",
  "markets=h2h&",
  "oddsFormat=american"
)

odds_response <- GET(odds_url)

if (status_code(odds_response) != 200) {
  stop("Odds API request failed.")
}

odds_data <- content(odds_response, as = "text", encoding = "UTF-8") %>%
  fromJSON(simplifyVector = FALSE)

get_price_for_team <- function(outcomes, team_name) {
  if (length(outcomes) == 0) return(NA_real_)
  names_vec <- vapply(outcomes, function(x) x$name, character(1))
  idx <- which(names_vec == team_name)
  if (length(idx) == 0) return(NA_real_)
  as.numeric(outcomes[[idx[1]]]$price)
}

games_df <- map_dfr(
  odds_data,
  function(g) {
    if (length(g$bookmakers) == 0) return(NULL)
    bm <- g$bookmakers[[1]]
    if (length(bm$markets) == 0) return(NULL)
    
    mkt  <- bm$markets[[1]]
    outs <- mkt$outcomes
    
    home_price <- get_price_for_team(outs, g$home_team)
    away_price <- get_price_for_team(outs, g$away_team)
    
    tibble(
      game_id       = g$id,
      commence_time = ymd_hms(g$commence_time),
      home_team     = g$home_team,
      away_team     = g$away_team,
      home_price    = home_price,
      away_price    = away_price
    )
  }
)

implied_prob <- function(price) {
  ifelse(
    is.na(price),
    NA_real_,
    ifelse(price < 0,
           (-price) / ((-price) + 100),
           100 / (price + 100))
  )
}

games_df <- games_df %>%
  mutate(
    home_prob = implied_prob(home_price),
    away_prob = implied_prob(away_price)
  )

dir.create("data", showWarnings = FALSE)
write_csv(games_df, "data/games_today.csv")
