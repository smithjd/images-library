# upload_image_to_wordpress.R

library(here)
library(httr2)
library(jsonlite)
library(tidyverse)
library(janitor)

image_details_unique <- read_rds(here("data", "image_details.rds")) |>
  distinct(resolved_id, .keep_all = TRUE) |>
  filter(is_image == TRUE)

select_image <- image_details_unique |>
  filter(resolved_id == "11qpXg9hwyD4x1TVbk_C4-Yc_OO1kqLCk")


# Authentication headers
auth_header <- httr2::request("https://portland.shambhala.org") |>
  httr2::req_auth_basic(Sys.getenv("SHAMBHALA_WORDPRESS_ID"),
                        Sys.getenv("SHAMBHALA_WORDPRESS_APP_PW"))


# Upload image to WordPress media library
upload_response <- httr2::request("https://images.shambhala.org/wp-json/wp/v2/media") |>
  httr2::req_auth_basic(Sys.getenv("SHAMBHALA_WORDPRESS_ID"),
                        Sys.getenv("SHAMBHALA_WORDPRESS_APP_PW")) |>
  httr2::req_body_file(path =  here("images", select_image$title),
                       type = select_image$mime_type) |>
  httr2::req_headers(
    "Content-Disposition" = paste0('attachment; filename="', basename(here("images", select_image$title)), '"')
  ) |>
  httr2::req_perform()

# Extract the media ID from response
media_data <- upload_response |> httr2::resp_body_json()
media_id <- media_data$id


media_data <- upload_resp |> httr2::resp_body_json()

# Step 2: Update metadata
metadata_payload <- list(
    title = list(rendered = metadata$name),
    description = list(rendered = metadata$description),
    alt_text = metadata$alt_text %||% metadata$name,
    caption = list(rendered = metadata$caption %||% "")
  )

  update_resp <- httr2::request(paste0(wp_url, "/wp-json/wp/v2/media/", media_data$id)) |>
    httr2::req_auth_basic(username, app_password) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(metadata_payload) |>
    httr2::req_perform()

  return(list(
    media_id = media_data$id,
    url = media_data$source_url,
    success = httr2::resp_status(update_resp) == 200
  ))
}