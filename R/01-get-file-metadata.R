# 01-get-file-metadata.R

library(googledrive)
library(googlesheets4)
library(dplyr)
library(purrr)
library(stringr)
library(httr)
library(here)
library(janitor)
library(tidyverse)
library(ourmirror)

# Authenticate with Google Drive
# might be done previously

drive_auth()


# Function to resolve Google Drive ID redirects
resolve_drive_id <- function(id_or_url) {
  # Extract ID from URL if needed
  if (str_detect(id_or_url, "drive.google.com")) {
    if (str_detect(id_or_url, "/file/d/")) {
      id_or_url <- str_extract(id_or_url, "(?<=/file/d/)[^/]+")
    } else if (str_detect(id_or_url, "[?&]id=")) {
      id_or_url <- str_extract(id_or_url, "(?<=[?&]id=)[^&]+")
    }
  }

  tryCatch({
    # Method 1: HTTP redirect approach to get the actual resolved ID
    url <- paste0("https://drive.google.com/file/d/", id_or_url, "/view")
    response <- GET(url, config = config(followlocation = FALSE))

    if (response$status_code == 302) {
      # Extract ID from redirect location
      redirect_url <- response$headers$location
      if (!is.null(redirect_url) && str_detect(redirect_url, "/file/d/")) {
        resolved_id <- str_extract(redirect_url, "(?<=/file/d/)[^/]+")

        # Verify this resolved ID actually works
        tryCatch({
          file_check <- drive_get(as_id(resolved_id))
          if (nrow(file_check) > 0) {
            return(list(
              original_id = id_or_url,
              resolved_id = resolved_id,
              status = "Resolved via HTTP redirect"
            ))
          }
        }, error = function(e) {
          # If resolved ID doesn't work, fall back to original
        })
      }
    }

    # Method 2: Try direct API access and capture the actual ID from response
    file_info <- drive_get(as_id(id_or_url))

    if (nrow(file_info) == 0) {
      return(list(
        original_id = id_or_url,
        resolved_id = NA,
        status = "File not found or not accessible"
      ))
    }

    # Get the actual file ID from the API response
    actual_id <- file_info$id

    # Check if this is different from the input ID
    if (actual_id != id_or_url) {
      return(list(
        original_id = id_or_url,
        resolved_id = actual_id,
        status = "Resolved via API"
      ))
    } else {
      return(list(
        original_id = id_or_url,
        resolved_id = actual_id,
        status = "No redirect needed"
      ))
    }

  }, error = function(e) {
    return(list(
      original_id = id_or_url,
      resolved_id = NA,
      status = paste("Error:", e$message)
    ))
  })
}

# Function to get parent folder path
get_folder_path <- function(file_id) {
  tryCatch({
    file_info <- drive_get(as_id(file_id))
    if (nrow(file_info) == 0) return("File not found")

    # Get parent folders
    parents <- file_info$drive_resource[[1]]$parents
    if (is.null(parents) || length(parents) == 0) {
      return("Root")
    }

    # Build path by traversing up the folder hierarchy
    path_parts <- c()
    current_parent <- parents[[1]]

    while (!is.null(current_parent) && current_parent != "root") {
      parent_info <- drive_get(as_id(current_parent))
      if (nrow(parent_info) > 0) {
        folder_name <- parent_info$name
        path_parts <- c(folder_name, path_parts)

        # Get next parent
        parent_resource <- parent_info$drive_resource[[1]]$parents
        if (is.null(parent_resource) || length(parent_resource) == 0) {
          current_parent <- NULL
        } else {
          current_parent <- parent_resource[[1]]
        }
      } else {
        break
      }
    }

    if (length(path_parts) == 0) {
      return("Root")
    } else {
      return(paste(path_parts, collapse = "/"))
    }
  }, error = function(e) {
    return(paste("Error:", e$message))
  })
}

# Function to get detailed file information using resolved ID
get_file_details <- function(resolved_id, original_id) {
  if (is.na(resolved_id)) {
    return(tibble(
      original_id = original_id,
      resolved_id = NA,
      title = "File not accessible",
      directory = "N/A",
      size_bytes = NA,
      size_mb = NA,
      created_date = NA,
      modified_date = NA,
      mime_type = "N/A",
      is_image = FALSE,
      error = "Could not resolve file ID"
    ))
  }

  tryCatch({
    # Get file information using resolved ID
    file_info <- drive_get(as_id(resolved_id))

    if (nrow(file_info) == 0) {
      return(tibble(
        original_id = original_id,
        resolved_id = resolved_id,
        title = "File not found",
        directory = "N/A",
        size_bytes = NA,
        size_mb = NA,
        created_date = NA,
        modified_date = NA,
        mime_type = "N/A",
        is_image = FALSE,
        error = "File not accessible"
      ))
    }

    # Extract file details
    resource <- file_info$drive_resource[[1]]

    # Get folder path
    folder_path <- get_folder_path(resolved_id)

    # Check if it's an image
    mime_type <- resource$mimeType %||% "unknown"
    is_image <- str_detect(mime_type, "^image/")

    # Get file size
    size_bytes <- as.numeric(resource$size %||% NA)
    size_mb <- if (!is.na(size_bytes)) round(size_bytes / 1048576, 2) else NA

    # Get dates
    created_date <- as.Date(resource$createdTime %||% NA)
    modified_date <- as.Date(resource$modifiedTime %||% NA)

    return(tibble(
      original_id = original_id,
      resolved_id = resolved_id,
      title = file_info$name,
      directory = folder_path,
      size_bytes = size_bytes,
      size_mb = size_mb,
      created_date = created_date,
      modified_date = modified_date,
      mime_type = mime_type,
      is_image = is_image,
      error = NA
    ))

  }, error = function(e) {
    return(tibble(
      original_id = original_id,
      resolved_id = resolved_id,
      title = "Error accessing file",
      directory = "N/A",
      size_bytes = NA,
      size_mb = NA,
      created_date = NA,
      modified_date = NA,
      mime_type = "N/A",
      is_image = FALSE,
      error = e$message
    ))
  })
}

# Main function to process all files with ID resolution
process_image_files_with_resolution <- function(file_ids, batch_size = 50) {
  cat("Step 1: Resolving", length(file_ids), "file IDs...\n")

  # Step 1: Resolve all IDs
  resolved_ids <- list()

  for (i in seq(1, length(file_ids), by = batch_size)) {
    batch_start <- i
    batch_end <- min(i + batch_size - 1, length(file_ids))
    batch_ids <- file_ids[batch_start:batch_end]

    cat("Resolving batch", ceiling(i/batch_size), "of",
        ceiling(length(file_ids)/batch_size),
        "(IDs", batch_start, "to", batch_end, ")\n")

    # Resolve batch
    batch_resolved <- map(batch_ids, resolve_drive_id)
    resolved_ids <- c(resolved_ids, batch_resolved)

    # Small delay to respect API limits
    Sys.sleep(1)
  }

  # Convert to data frame
  resolution_df <- map_dfr(resolved_ids, ~ tibble(
    original_id = .x$original_id,
    resolved_id = .x$resolved_id,
    resolution_status = .x$status
  ))

  cat("Step 1 complete. Successfully resolved",
      sum(!is.na(resolution_df$resolved_id)), "out of",
      nrow(resolution_df), "IDs.\n\n")

  # Step 2: Get detailed information for successfully resolved IDs
  cat("Step 2: Getting detailed information for resolved files...\n")

  results <- list()
  valid_resolutions <- resolution_df[!is.na(resolution_df$resolved_id), ]

  for (i in seq(1, nrow(valid_resolutions), by = batch_size)) {
    batch_start <- i
    batch_end <- min(i + batch_size - 1, nrow(valid_resolutions))
    batch_data <- valid_resolutions[batch_start:batch_end, ]

    cat("Processing details batch", ceiling(i/batch_size), "of",
        ceiling(nrow(valid_resolutions)/batch_size),
        "(files", batch_start, "to", batch_end, ")\n")

    # Process batch
    batch_results <- map2_dfr(batch_data$resolved_id, batch_data$original_id,
                              get_file_details)
    results[[length(results) + 1]] <- batch_results

    # Small delay to respect API limits
    Sys.sleep(1)
  }

  # Combine all results
  if (length(results) > 0) {
    all_results <- bind_rows(results)
  } else {
    all_results <- tibble()
  }

  # Add failed resolutions to results
  failed_resolutions <- resolution_df[is.na(resolution_df$resolved_id), ]
  if (nrow(failed_resolutions) > 0) {
    failed_results <- failed_resolutions %>%
      mutate(
        title = "Failed to resolve ID",
        directory = "N/A",
        size_bytes = NA,
        size_mb = NA,
        created_date = NA,
        modified_date = NA,
        mime_type = "N/A",
        is_image = FALSE,
        error = resolution_status
      ) %>%
      select(original_id, resolved_id, title, directory, size_bytes, size_mb,
             created_date, modified_date, mime_type, is_image, error)

    all_results <- bind_rows(all_results, failed_results)
  }

  # Summary statistics
  cat("\n=== SUMMARY ===\n")
  cat("Total files processed:", nrow(all_results), "\n")
  cat("IDs successfully resolved:", sum(!is.na(all_results$resolved_id)), "\n")
  cat("Actual images found:", sum(all_results$is_image, na.rm = TRUE), "\n")
  cat("Files with errors:", sum(!is.na(all_results$error)), "\n")
  cat("Total size (MB):", round(sum(all_results$size_mb, na.rm = TRUE), 2), "\n")

  return(all_results)
}

# Test with your specific example
# test_id <- "1OF2gubExHJ-VMByOTilER_bpoHVBSSxz"
# cat("Testing ID resolution with your example...\n")
# test_result <- resolve_drive_id(test_id)
# cat("Original ID:", test_result$original_id, "\n")
# cat("Resolved ID:", test_result$resolved_id, "\n")
# cat("Status:", test_result$status, "\n\n")

images_spreadsheet <- read_sheet("https://docs.google.com/spreadsheets/d/1oKNLxr6IuZ_DQ4fUCyxE7qxF_RtIISay8QeoIz8o4ag/edit?gid=1239795533#gid=1239795533",
                                 sheet = "Only Yes Images from Main Drive") |>
  clean_names()

# yes_ids <- images_spreadsheet |>
#   filter(use == "y")
  # head(n = 30)

your_file_ids <- images_spreadsheet$id


# Process all files with ID resolution
image_details <- process_image_files_with_resolution(images_spreadsheet$id)

write_rds(image_details, here("data", "image_details.rds"))
write_rds(images_spreadsheet, here("data", "images_spreadsheet.rds"))
#####
# Check for
# * unique resolved_id
# * duplicate / overlapping vars between the 2 files
# * strip out "Drive/Marketing and Communications/Images/"
#   from image_details.directory
# * decide on the order of variables for the description field

image_details <- read_rds(here("data", "image_details.rds"))

file_documentation <- mrr_job_doc()

image_details_full <- image_details |>
  left_join(images_spreadsheet, by = c ("original_id" = "id")) |>
  select(-c(error, size, x1, x2)) |>
  rename(old_url = url)

# image_details_full <- images_spreadsheet |>
#   left_join(image_details, by = c("id" = "original_id")) |>
#   select(-c(size, new_filename, category_descriptions, x12))

library(googlesheets4)
ss <- gs4_create(
  paste0('image library data ', Sys.Date()),
  sheets = list('Original alias and real Google file ID' = image_details_full,
                'File Documentation' = file_documentation)
)
gs4_browse(ss)

# kcl_images_spreadsheet <-
#   read_sheet("https://docs.google.com/spreadsheets/d/1oKNLxr6IuZ_DQ4fUCyxE7qxF_RtIISay8QeoIz8o4ag/edit?gid=1853901660#gid=1853901660",
#     sheet = "KCL_Images_For_Import") |>
#   clean_names() |>
#   select(url, folder_description, target_filename,
#          text_description_for_text_and_filename) |>
#   mutate(folder_description = str_remove(folder_description, "\n\n---")) |>
#   distinct(url, .keep_all = TRUE)
#
# write_rds(kcl_images_spreadsheet, here("data", "kcl_images_spreadsheet.rds"))
