rm(list=ls())
library(tidyverse)

setwd("~/Library/CloudStorage/OneDrive-NTNU/course-websites/computational-history")

library(dhlabR)
news <- get_document_corpus(
  doctype = "digavis", 
  from_year = 1910, to_year = 1911,
  lang = NULL,
  limit = 50000)
news |> glimpse()

news <- news |>
  as_tibble() |>
  select(dhlabid, title, year, city) |>
  mutate(dhlabid = map_int(dhlabid, 1),
         title = map_chr(title, 1),
         year = map_int(year, 1),
         city = map_chr(city, 1))
news

write_csv(news, "data-assign/newspapers-1910/news-1910.csv")
