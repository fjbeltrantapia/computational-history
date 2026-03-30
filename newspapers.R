rm(list=ls()) 

library(tidyverse)
library(dhlabR)

newspapers <- get_document_corpus(
  doctype = "digavis", 
  lang = NULL,
  from_year = 1700, to_year = 1951,
  limit = 5000000)

newspapers |> glimpse()
  # 1,999,645 docs (almost 2 million) up to 1950

news_adj <- newspapers |> 
  as_tibble() |>
  select(dhlabid, title, year, timestamp, city) |>
  mutate(dhlabid = map_int(dhlabid, 1),
         title = map_chr(title, 1),
         year = map_int(year, 1),
         timestamp = map_int(timestamp, 1),
         city = map_chr(city, 1))

news_adj |> count(city) |> print(n = Inf)
news_adj |> 
  filter(year>=1900 & year<=1910) |>
  count(city) |> print(n = Inf)

news_adj |>
  group_by(year) |>
  summarise(n = n()) |>
  ggplot(aes(x = year, y = n)) +
  geom_point() + geom_line() 