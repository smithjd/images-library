# upload_image_to_wordpress.R

library(here)
library(httr2)
library(jsonlite)
library(tidyverse)
library(janitor)

# image_path <- "~/Downloads/karmê-chöling-main-shrine-room-large-gong-with-blue-meditation-cushion-view-from-slightly-above.jpg"
alt_text <- "Karmê Chöling Main Shrine Room large gong with blue meditation cushion view from slightly above"
description <- paste0(
  alt_text,
  ".\n\n",
  result,
  "\n\n",
  "Meditation Objects, Shrine Objects, Shrine Room Objects."
)

# Authentication headers
# auth_header <- httr2::request("https://images.shambhala.org") |>
#   httr2::req_auth_basic(Sys.getenv("SHAMBHALA_WORDPRESS_ID"),
#                         Sys.getenv("SHAMBHALA_WORDPRESS_APP_PW"))

# Upload image to WordPress media library
upload_to_wordpress <- function(
  directory,
  image_file_name,
  alt_text,
  description) {
  upload_response <- httr2::request(
    "https://images.shambhala.org/wp-json/wp/v2/media"
  ) |>
    httr2::req_auth_basic(
      Sys.getenv("SHAMBHALA_WORDPRESS_ID"),
      Sys.getenv("SHAMBHALA_WORDPRESS_APP_PW")
    ) |>
    httr2::req_body_file(path = image_path, type = "image/jpeg") |>
    httr2::req_headers(
      "Content-Disposition" = paste0(
        'attachment; filename="',
        image_file_name,
        '"'
      )
    ) |>
    httr2::req_perform()

  # Extract the media ID from response

  media_data <- upload_response |> httr2::resp_body_json()
  media_id <- media_data$id

  # Prepare metadata payload

  metadata_payload <- list(
    title = "karmê-chöling-front-door-with-sun-and-blue-sky-and-purple-flowers-and-rocks.jpg",
    description = paste(description),
    alt_text = alt_text)

  # Update the media item with metadata
  update_response <- httr2::request(paste0(
    "https://images.shambhala.org/wp-json/wp/v2/media/",
    media_id
  )) |>
    httr2::req_auth_basic(
      Sys.getenv("SHAMBHALA_WORDPRESS_ID"),
      Sys.getenv("SHAMBHALA_WORDPRESS_APP_PW")
    ) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(metadata_payload) |>
    httr2::req_perform()
}
# check https://images.shambhala.org/wp-admin/upload.php