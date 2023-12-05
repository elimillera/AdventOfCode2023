## Day 3
library(stringr)
library(dplyr)
# raw_spec <- c(
# "467..114..
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598.."
# ) %>% str_remove_all("\n")

# Raw scehematic with the newline symbols removed
raw_spec <- readLines("Day3Input.txt") %>%
  paste0(collapse = "\n") %>%
  str_remove_all("\n")

# The offset is the number of rows/columns
offset <- 140
#offset <- 10

# Spec as an array, just split on nothing
spec_matrix <- raw_spec %>%
  str_split("", simplify = TRUE)

# List of valid symbols
symbs <- c("*", "+", "#", "$", "&", "-", "=", "/", "%", "@")

# Find the index of the symbols in the spec
is_sym_ind <- which(spec_matrix %in% symbs)

# Symb area finds the area of the schematic that would be valid,
# whether or not it has a number
symb_area <- map(
  as.list(is_sym_ind),
  ~ c(. - offset - 1, . - offset, . - offset + 1,
      . - 1, . + 1,
      . + offset -1, . + offset, . + offset + 1)
) %>%
  unlist()

all_numbers <- str_extract_all(raw_spec, "\\d+")[[1]]

# find all number in raw_spec and create a range of indices for each number.
# Compare the range to see if it appears in the symb_area. If it does its a
# part
schematic <- str_locate_all(raw_spec, "\\d+") %>%
  as.data.frame() %>%
  mutate(
    number = all_numbers
  ) %>%
  rowwise() %>%
  mutate(
    range = list(start:end),
    is_part = if(any(range %in% symb_area)) TRUE else FALSE
  )

# Filter on only the parts, and sum the numbers!
schematic %>%
  filter(is_part) %>%
  extract2("number") %>%
  as.numeric() %>%
  sum()

## Part 2

is_star_ind <- which(spec_matrix %in% "*")
star_area <- map(
  as.list(is_star_ind),
  ~ c(. - offset - 1, . - offset, . - offset + 1,
      . - 1, . + 1,
      . + offset -1, . + offset, . + offset + 1)
)

gears <- map(star_area,
        function(x) {
          has_overlap <- map_lgl(schematic$range, ~ any(. %in% x))
          
          if(sum(has_overlap) == 2) prod(as.numeric(all_numbers[which(has_overlap)]))
          else which(NA)
        }
      )

gears %>%
  unlist() %>%
  sum()

