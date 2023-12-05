# Day 2

library(stringr)
library(purrr)
library(magrittr)


red <- list()
green <- list()
blue <- list()

a_line <- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

the_lines <- list(
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
)

the_lines <- readLines("~/Day2Input.txt") %>%
  as.list()

get_color_number <- function(pull) {
  
  blue_regex <- "\\d+ blue"
  red_regex <- "\\d+ red"
  green_regex <- "\\d+ green"
  
  green <- str_extract_all(pull[[1]], green_regex) %>%
    str_sub(end = -7) %>%
    as.numeric()
  
  blue <- str_extract_all(pull[[1]], blue_regex) %>%
    str_sub(end = -6) %>%
    as.numeric()
  
  red <- str_extract_all(pull[[1]], red_regex) %>%
    str_sub(end = -5) %>%
    as.numeric()
  
  list(red = red, blue = blue, green = green)
  
}

parse_game_line <- function(line) {
  
  # Split 1 has Game # and ...
  split1 <- str_split(line, ":", simplify = TRUE)
  
  # Game number is the 6th char onwards so split it out
  game_number <- as.numeric(str_sub(split1[1], 6))
  
  # Split pulls from the bag by ;
  pulls <- str_split(split1[2], ";")
  

  pull_vals <- get_color_number(pulls)
  
  max_vals <- map_dbl(pull_vals, max, na.rm = TRUE)
  
  
  data.frame(
    game_number = game_number,
    red_max = max_vals["red"],
    green_max = max_vals["green"],
    blue_max = max_vals["blue"]
  )
}


parse_game_line(a_line)

the_stuff <- map_dfr(the_lines, parse_game_line)

red_total <- 12
green_total <- 13
blue_total <- 14
the_stuff2 <- the_stuff %>%
  rowwise() %>%
  mutate(
    is_possible = red_max <= red_total &&
      blue_max <= blue_total &&
      green_max <= green_total
  )

the_stuff2 %>%
  filter(is_possible) %>%
  extract("game_number") %>%
  sum()

## Part 2
the_stuff %>%
  mutate(
    power = red_max * green_max * blue_max
  ) %>%
  extract("power") %>%
  sum()

