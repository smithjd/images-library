# upload_image_to_wordpress.R

library(here)
library(httr2)
library(jsonlite)
library(tidyverse)
library(janitor)

test_image <- "~/Downloads/karmê-chöling-main-shrine-room-large-gong-with-blue-meditation-cushion-view-from-slightly-above.jpg"
alt_text <- "Karmê Chöling Main Shrine Room large gong with blue meditation cushion view from slightly above"
description <- paste(alt_text, ".\n\n", result, "\n\n", "Meditation Objects, Shrine Objects, Shrine Room Objects.")


# select_image <- image_details_unique |>
#   filter(resolved_id == "11qpXg9hwyD4x1TVbk_C4-Yc_OO1kqLCk")


# Authentication headers
auth_header <- httr2::request("https://images.shambhala.org") |>
  httr2::req_auth_basic(Sys.getenv("SHAMBHALA_WORDPRESS_ID"),
                        Sys.getenv("SHAMBHALA_WORDPRESS_APP_PW"))


# Upload image to WordPress media library
upload_response <- httr2::request("https://images.shambhala.org/wp-json/wp/v2/media") |>
  httr2::req_auth_basic(Sys.getenv("SHAMBHALA_WORDPRESS_ID"),
                        Sys.getenv("SHAMBHALA_WORDPRESS_APP_PW")) |>
  httr2::req_body_file(path =  test_image,
                       type = "image/jpeg") |>
  httr2::req_headers(
    "Content-Disposition" = paste0('attachment; filename="',
                                   test_image, '"')
  ) |>
  httr2::req_perform()

# Extract the media ID from response

media_data <- upload_response |> httr2::resp_body_json()
media_id <- media_data$id


media_data <- upload_response |> httr2::resp_body_json()

# Step 2: Update metadata
metadata_payload <- list(
    title = "karmê-chöling-front-door-with-sun-and-blue-sky-and-purple-flowers-and-rocks.jpg",
    description = description,
    alt_text = alt_text %||% title
  )

# metadata_payload <- list(
#     title = list(rendered = metadata$name),
#     description = list(rendered = metadata$description),
#     alt_text = metadata$alt_text %||% metadata$name
#   )
#
  update_resp <- httr2::request(paste0("https://images.shambhala.org/wp-json/wp/v2/media/", media_data$id)) |>
    httr2::req_auth_basic(Sys.getenv("SHAMBHALA_WORDPRESS_ID"),
                          Sys.getenv("SHAMBHALA_WORDPRESS_APP_PW"))  |>
    httr2::req_method("POST") |>
    httr2::req_body_json(metadata_payload) |>
    httr2::req_perform()

  # return(list(
  #   media_id = media_data$id,
  #   url = media_data$source_url,
  #   success = httr2::resp_status(update_resp) == 200
  # ))

#### clean try ####

  # Prepare metadata payload

  metadata_payload <- list(
    title = "karmê-chöling-front-door-with-sun-and-blue-sky-and-purple-flowers-and-rocks.jpg",
    description = paste(description),
    alt_text = alt_text
  )

  # Update the media item with metadata
  update_response <- httr2::request(paste0("https://images.shambhala.org/wp-json/wp/v2/media/", media_id)) |>
    httr2::req_auth_basic(Sys.getenv("SHAMBHALA_WORDPRESS_ID"),
                          Sys.getenv("SHAMBHALA_WORDPRESS_APP_PW"))  |>
    httr2::req_method("POST") |>
    httr2::req_body_json(metadata_payload) |>
    httr2::req_perform()

# check https://images.shambhala.org/wp-admin/upload.php