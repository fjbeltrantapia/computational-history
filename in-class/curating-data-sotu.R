# Curating the data set extracted from the summaries of the 1860 census 
  # for class assignments
# Using the version stored in my website
# Keeping only a few variables

rm(list=ls())
library(tidyverse)

setwd("~/Library/CloudStorage/OneDrive-NTNU/course-websites/computational-history")

sotu <- read_csv("data-assign/sotu/sotu.csv") |>
  select(-`Word Count`) |>
  rename(year = `Year`,
         president = `President`,
         text = `Text`)




sotu <- sotu |>
  distinct(year, president, text, .keep_all = TRUE)

sotu |> count(year) |> print(n = Inf)

# remove duplicates 
pres <- sotu |>
  count(president) |> select(president)

write_csv(pres, "data-assign/sotu/presidents.csv")

write_csv(sotu, "data-assign/sotu/sotu-texts.csv")

presidents |> print(n = Inf)
