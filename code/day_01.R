library(tidyverse)



tbl <- read_csv(
  "data/day_01_01.txt",
  col_names = c("line")
) |> 
  separate(
    line,
    into = c("v1", "v2"),
    sep = " +"
  ) |> 
  mutate(
    across(
      c(v1, v2),
      as.numeric
    )
  )

# part 01

v1 <- tbl |> select(v1) |> arrange(v1)
v2 <- tbl |> select(v2) |> arrange(v2)

bind_cols(v1, v2) |> 
  mutate(diff = abs(v1 - v2)) |> 
  summarize(total = sum(diff))

# part 02

v1 |> 
  left_join(
    tibble(v2, v2c = v2),
    by = join_by(v1 == v2)
  ) |> 
  filter(!is.na(v2c)) |> 
  count(v1) |> 
  summarize(
    total = sum(v1 * n)
  )

