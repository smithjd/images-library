# Image Batch Processing Control Script
# Purpose: Process images from Google Drive -> Ollama -> WordPress in configurable batches

# Load required libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(googledrive)
library(here)
library(httr2)
library(janitor)
library(base64enc)
library(ollamar)
library(beepr)

# =============================================================================
# CONFIGURATION SECTION
# =============================================================================

# Batch configuration
BATCH_START_ROW <- 165      # Starting row number (1-based indexing)
BATCH_SIZE <- 20           # Number of images to process in this batch
LOG_FILE <- "image_processing_log.txt"
ERROR_LOG <- "image_processing_errors.txt"

# File paths
IMAGE_DATA_FILE <- here("data", "kcl_images_spreadsheet.rds")

# API configurations
drive_auth(email = "john.smith@shambhala.info")
OLLAMA_BASE_URL <- "http://localhost:11434"  # Adjust if Ollama runs elsewhere
WORDPRESS_SITE_URL <- "https://images.shambhala.org/wp-json/wp/v2/media"
WORDPRESS_USERNAME <- Sys.getenv("SHAMBHALA_WORDPRESS_ID")
WORDPRESS_APP_PASSWORD <- Sys.getenv("SHAMBHALA_WORDPRESS_APP_PW")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Logging function
log_message <- function(message, log_file = LOG_FILE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("[", timestamp, "] ", message)
  cat(log_entry, "\n")
  write(log_entry, file = log_file, append = TRUE)
}

# Error logging function
log_error <- function(error_msg, real_image_file_name = NULL, error_file = ERROR_LOG) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (!is.null(real_image_file_name)) {
    error_entry <- paste0("[", timestamp, "] ERROR - Image ID ", real_image_file_name, ": ", error_msg)
  } else {
    error_entry <- paste0("[", timestamp, "] ERROR: ", error_msg)
  }
  cat(error_entry, "\n")
  write(error_entry, file = error_file, append = TRUE)
}

get_google_drive_image <- function(
  google_image_id,
  local_image_location
) {
  local_file_directory <- "~/Downloads/"
  drive_download(
    file = as_id(google_image_id),
    path = paste0(local_file_directory, local_image_location),
    overwrite = TRUE
  )
}

get_ollama_description <- function(local_image_file) {
  cat("Starting image description for:", local_image_file, "\n")

  # Check if file exists
  if (!file.exists(local_image_file)) {
    return("ERROR: File does not exist")
  }

  tryCatch({
    # Use ollamar's generate function with image
    response <- ollamar::generate(
      model = "llama3.2-vision:latest",
      prompt = "Write a brief description of this image in exactly 2-3 sentences.
      Include the most important objects, people, setting, and text if any. Be concise.",
      images = local_image_file,
      stream = FALSE
    )

    # Extract the JSON content from the ollama_response body
    if (inherits(response, "httr2_response")) {
      # Parse the JSON response body
      response_text <- rawToChar(response$body)
      response_json <- jsonlite::fromJSON(response_text)

      # cat("Parsed response structure:\n")
      # str(response_json)

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

upload_to_wordpress <- function(
  local_image_location,
  real_image_file_name,
  text_description_for_text_and_filename,
  ollama_description,
  folder_description
) {
  upload_response <- httr2::request(
    "https://images.shambhala.org/wp-json/wp/v2/media"
  ) |>
    httr2::req_auth_basic(
      Sys.getenv("SHAMBHALA_WORDPRESS_ID"),
      Sys.getenv("SHAMBHALA_WORDPRESS_APP_PW")
    ) |>
    httr2::req_body_file(path = local_image_location, type = "image/jpeg") |>
    httr2::req_headers(
      "Content-Disposition" = paste0(
        'attachment; filename="',
        real_image_file_name,
        '"'
      )
    ) |>
    httr2::req_perform()

  # Extract the media ID from response

  media_data <- upload_response |> httr2::resp_body_json()
  media_id <- media_data$id

  # Prepare metadata payload

  metadata_payload <- list(
    title = real_image_file_name,
    description = paste0(
      text_description_for_text_and_filename,
      ". ",
      ollama_description, "\n\n",
      folder_description, "."
    ),
    alt_text = text_description_for_text_and_filename
  )

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
# =============================================================================
# MAIN PROCESSING FUNCTION (PLACEHOLDER)
# =============================================================================

# This function processes a single image through the entire pipeline
# You'll need to implement the actual Google Drive, Ollama, and WordPress logic
process_single_image <- function(image_row) {
  google_image_id <- image_row$url
  real_image_file_name <- image_row$target_filename
  folder_description <- image_row$folder_description
  text_description_for_text_and_filename <-
    image_row$text_description_for_text_and_filename
  local_file_directory <- "~/Downloads/"
  local_image_location <- paste0(local_file_directory, real_image_file_name)

  tryCatch({
    log_message(paste("Starting processing for image ID:", google_image_id))

    # Step 1: Download from Google Drive
    log_message(paste("Downloading image ID", google_image_id, "from Google Drive"))
    downloaded_file <- get_google_drive_image(google_image_id, real_image_file_name)

    # Step 2: Get description from Ollama
    log_message(paste("Getting description for image ID", google_image_id, "from Ollama"))

    ollama_description <- get_ollama_description(local_image_location)

    # Step 3: Upload to WordPress
    log_message(paste("Uploading image ID", google_image_id, "to WordPress"))

    upload_to_wordpress(
      local_image_location,
      real_image_file_name,
      text_description_for_text_and_filename,
      ollama_description,
      folder_description
    )

    # Step 4: Clean up temporary files

    if (file.exists(local_image_location)) file.remove(local_image_location)

    log_message(paste("Successfully processed:", real_image_file_name))
    return(list(success = TRUE, google_image_id = google_image_id))

  }, error = function(e) {
    error_msg <- paste("Failed to process image:", e$message)
    log_error(error_msg, google_image_id)
    return(list(success = FALSE, google_image_id = google_image_id, error = e$message))
  })
}

# =============================================================================
# MAIN BATCH PROCESSING LOGIC
# =============================================================================

main <- function() {

  # Initialize logging
  log_message("=== Starting new batch processing session ===")
  log_message(paste("Batch configuration: Start row =", BATCH_START_ROW, ", Batch size =", BATCH_SIZE))

  # Load image data
  tryCatch({
    if (!file.exists(IMAGE_DATA_FILE)) {
      stop(paste("Image data file not found:", IMAGE_DATA_FILE))
    }

    image_data <- read_rds(IMAGE_DATA_FILE)
    log_message(paste("Loaded image data with", nrow(image_data), "total images"))

  }, error = function(e) {
    log_error(paste("Failed to load image data:", e$message))
    stop("Cannot proceed without image data")
  })

  # Validate batch parameters
  if (BATCH_START_ROW > nrow(image_data)) {
    log_error(paste("Start row", BATCH_START_ROW, "exceeds total rows", nrow(image_data)))
    stop("Invalid batch start row")
  }

  # Calculate batch end row
  batch_end_row <- min(BATCH_START_ROW + BATCH_SIZE - 1, nrow(image_data))
  actual_batch_size <- batch_end_row - BATCH_START_ROW + 1

  log_message(paste("Processing rows", BATCH_START_ROW, "to", batch_end_row,
                    "(", actual_batch_size, "images)"))

  # Extract batch data
  batch_data <- image_data[BATCH_START_ROW:batch_end_row, ]

  # Initialize counters
  successful_count <- 0
  failed_count <- 0
  failed_ids <- character(0)

  # Process each image in the batch
  for (i in 1:nrow(batch_data)) {
    current_row <- BATCH_START_ROW + i - 1
    image_row <- batch_data[i, ]

    log_message(paste("Processing image", i, "of", nrow(batch_data),
                      "(row", current_row, ", ID:", image_row$google_image_id, ")"))

    # Process the image
    result <- process_single_image(image_row)

    if (result$success) {
      successful_count <- successful_count + 1
    } else {
      failed_count <- failed_count + 1
      failed_ids <- c(failed_ids, result$google_image_id)
    }

    # Optional: Add a small delay between images to be nice to APIs
    Sys.sleep(1)
  }

  # Final summary
  log_message("=== Batch processing completed ===")
  log_message(paste("Successfully processed:", successful_count, "images"))
  log_message(paste("Failed to process:", failed_count, "images"))

  if (failed_count > 0) {
    log_message(paste("Failed image IDs:", paste(failed_ids, collapse = ", ")))
  }

  # Final message to user
  success_message <- paste("Rows", BATCH_START_ROW, "to", batch_end_row, "have been processed.",
                           "Success:", successful_count, "| Failed:", failed_count)

  cat("\n" %+% rep("=", 60) %+% "\n")
  cat(success_message, "\n")
  cat(rep("=", 60) %+% "\n")

  log_message(success_message)

  beep(3)

  # Return summary for potential programmatic use
  return(list(
    batch_start = BATCH_START_ROW,
    batch_end = batch_end_row,
    successful = successful_count,
    failed = failed_count,
    failed_ids = failed_ids
  ))
}

# =============================================================================
# EXECUTION
# =============================================================================

# Run the main function
if (!interactive()) {
  # Only run automatically if script is being executed (not sourced)
  main()
  beep(5)
} else {
  cat("Script loaded. Run main() to execute batch processing.\n")
  cat("Current configuration:\n")
  cat("  Start row:", BATCH_START_ROW, "\n")
  cat("  Batch size:", BATCH_SIZE, "\n")
  cat("  Will process rows", BATCH_START_ROW, "to",
      min(BATCH_START_ROW + BATCH_SIZE - 1, 174), "\n")
}