# Day 1

library(purrr)
library(stringr)
library(magrittr)
digit_regex <- "\\d"

#eli_input <- readLines("https://adventofcode.com/2023/day/1/input")
eli_input <- readLines("Day1Input.txt") %>% as.list()


# Use map to iterate through input and make sure we return a integer
# stringr::str_extract_all returns all of the digits in a matrix
map_int(eli_input, ~ str_extract_all(., digit_regex, simplify = TRUE) %>%
          # Get the first and last values from that matrix
          extract(c(1, ncol(.))) %>%
          #paste them together and collapse them to one thing
          paste0(collapse = "") %>%
          # force to numeric
          as.numeric()) %>% sum()


## Part 2

digit_regex_deux <- "\\d|one|two|three|four|five|six|seven|eight|nine"

vals <- map_dbl(eli_input, ~ str_extract_all(., regex(digit_regex_deux,ignore_case = TRUE), simplify = TRUE)  %>%
          # Get the first and last values from that matrix
          extract(c(1, ncol(.))) %>%
          as.list() %>%
          map_chr(\(x) case_match(x, 
                               "one" ~ "1",
                               "two" ~ "2",
                               "three" ~ "3",
                               "four" ~ "4",
                               "five" ~ "5",
                               "six" ~ "6",
                               "seven" ~ "7",
                               "eight" ~ "8",
                               "nine" ~ "9",
                               .default = x)) %>%
          # #paste them together and collapse them to one thing
          paste0(collapse = "") %>%
          # # force to numeric
          as.numeric())# %>%
 #reduce(`+`)
sum(vals)

