# 01-setup-image-file-database.R

# subset data frame with Google ID's that was created in

# get image file

# drop videos & tiff files.
# some files are referred to more than once, so filter them out first
library(here)
library(tidyverse)
library(janitor)

image_details <- read_rds(here("data", "image_details.rds"))
nrow(image_details)

image_details_unique <- image_details |>
  distinct(resolved_id, .keep_all = TRUE) |>
  filter(is_image == TRUE) |>
  arrange(resolved_id) |>
  select(resolved_id, title, directory, created_date, mime_type)

nrow(image_details_unique)

write_rds(image_details_unique, here("data", "image_details_unique.rds"))