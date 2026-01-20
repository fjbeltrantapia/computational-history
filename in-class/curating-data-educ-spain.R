# Curating the data set extracted from the summaries of the 1860 census 
  # for class assignments
# Using the version stored in my website
# Keeping only a few variables

library(tidyverse)

setwd("~/Library/CloudStorage/OneDrive-NTNU/course-websites/computational-history")

educ <- read_csv("~/Library/CloudStorage/OneDrive-NTNU/personal-website/datasets/dist/dist_educ_1860.csv") |> 
  rename(district = partido,
         illit_m = illiterate_m, 
         illit_f = illiterate_f,
         write_m = read_writ_m,
         write_f = read_writ_f) |>
  select(district, province, illit_m, read_m, write_m, illit_f, read_f, write_f)

write_csv(educ, "data-assign/educ-1860/dist-1860.csv")