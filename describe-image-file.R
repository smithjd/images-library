# get image file

# drop videos & tiff files.
# some files are referred to more than once, so filter them out first

library(googledrive)
library(here)
library(httr2)
library(jsonlite)
library(tidyverse)
library(janitor)
library(base64enc)
library(ollamar)


image_details <- read_rds(here("data", "image_details.rds"))
nrow(image_details)

image_details_unique <- image_details |>
  distinct(resolved_id, .keep_all = TRUE) |>
  filter(is_image == TRUE)

nrow(image_details_unique)

# drive_download(file = as_id("1HxnBhHlvhQymDMxVfuTi6669eyZ_RefF"),
#                path = here("images", "new_file.jpeg"), overwrite = TRUE)
#
# drive_download(file = as_id("1perxmohAqnRfJH6G8HoVxikfgVR6L5xR"),
#                path = here("images", "100089371_2933508173363085_1929774139472609280_n.jpg"), overwrite = TRUE)

# drive_download(file = as_id("1NeUOPTkgCQLkAw1RdkvUZZ6cUUIpFz9Y"),
#                path = here("images", "test_.jpg"), overwrite = TRUE)

select_image <- image_details_unique |>
  filter(resolved_id == "11qpXg9hwyD4x1TVbk_C4-Yc_OO1kqLCk") |>
  mutate(directory_tag = str_extract(directory, "[^/]+$"))

drive_download(file = as_id(select_image$resolved_id),
               path = here("images", select_image$title), overwrite = TRUE)

describe_image <- function(image_path) {
  cat("Starting image description for:", image_path, "\n")

  # Check if file exists
  if (!file.exists(image_path)) {
    return("ERROR: File does not exist")
  }

  tryCatch({
    # Use ollamar's generate function with image
    response <- ollamar::generate(
      model = "llama3.2-vision:latest",
      prompt = "Write a brief description of this image in exactly 2-3 sentences. Include the most important objects, people, setting, and text if any. Be concise.",
      # prompt = "Describe this image in detail, including objects, people, setting, and any text visible. Limit output to 300 characters",
      images = image_path,
      stream = FALSE
    )

    # Extract the JSON content from the response body
    if (inherits(response, "httr2_response")) {
      # Parse the JSON response body
      response_text <- rawToChar(response$body)
      response_json <- jsonlite::fromJSON(response_text)

      cat("Parsed response structure:\n")
      str(response_json)

      # Extract the actual response text
      if ("response" %in% names(response_json)) {
        return(response_json$response)
      } else {
        cat("Full parsed response:\n")
        print(response_json)
        return("ERROR: Could not find 'response' field in JSON")
      }
    } else {
      # If it's already parsed (shouldn't happen based on your output)
      return(response$response)
    }

  }, error = function(e) {
    cat("Error occurred:", e$message, "\n")
    return(paste("Ollama Error:", e$message))
  })
}

start_time <- Sys.time()
# Your long-running code here
result <- describe_image(here::here("images", "test_.jpg"))

end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(elapsed_time)

start_time <- Sys.time()
# Your long-running code here
result <- describe_image(here::here("images", select_image$title))

end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(elapsed_time)

# parts <- str_split(text, "/")[[1]]
#
# last <- parts[length(parts)]
# penultimate <- parts[length(parts) - 1]

# result <- describe_image("/Users/jds/Documents/cch/SGS_IT/3-0-websites-rollout/images-library/images/new_file.jpeg")
