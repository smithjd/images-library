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

# for testing

image_folder <- "~/Downloads/"
image_name <- "karmê-chöling-main-shrine-room-large-gong-with-blue-meditation-cushion-view-from-slightly-above.jpg"
image_path <- paste0(image_folder, image_name)

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

# result <- describe_image("/Users/jds/Documents/cch/SGS_IT/3-0-websites-rollout/images-library/images/new_file.jpeg")
result <- describe_image(image_path)
