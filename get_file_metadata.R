library(googledrive)
library(dplyr)
library(purrr)
library(stringr)
library(here)

images_spreadsheet <- read_sheet("https://docs.google.com/spreadsheets/d/1oKNLxr6IuZ_DQ4fUCyxE7qxF_RtIISay8QeoIz8o4ag/edit?gid=1239795533#gid=1239795533") |>
  clean_names()


# Authenticate with Google Drive
drive_auth()

# Function to extract file ID from Google Drive URL if needed
extract_file_id <- function(id_or_url) {
  if (str_detect(id_or_url, "drive.google.com")) {
    # Extract ID from URL patterns
    if (str_detect(id_or_url, "/file/d/")) {
      return(str_extract(id_or_url, "(?<=/file/d/)[^/]+"))
    } else if (str_detect(id_or_url, "[?&]id=")) {
      return(str_extract(id_or_url, "(?<=[?&]id=)[^&]+"))
    }
  }
  return(id_or_url)  # Return as-is if it's already an ID
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

# Function to get detailed file information
get_file_details <- function(id_or_url) {
  # Extract actual file ID
  file_id <- extract_file_id(id_or_url)

  tryCatch({
    # Get file information
    file_info <- drive_get(as_id(file_id))

    if (nrow(file_info) == 0) {
      return(tibble(
        original_id = id_or_url,
        actual_file_id = file_id,
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
    folder_path <- get_folder_path(file_id)

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
      original_id = id_or_url,
      actual_file_id = file_id,
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
      original_id = id_or_url,
      actual_file_id = file_id,
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

# Main function to process all files
process_image_files <- function(file_ids, batch_size = 50) {
  cat("Processing", length(file_ids), "files...\n")

  # Process in batches to avoid API rate limits
  results <- list()

  for (i in seq(1, length(file_ids), by = batch_size)) {
    batch_start <- i
    batch_end <- min(i + batch_size - 1, length(file_ids))
    batch_ids <- file_ids[batch_start:batch_end]

    cat("Processing batch", ceiling(i/batch_size), "of",
        ceiling(length(file_ids)/batch_size),
        "(files", batch_start, "to", batch_end, ")\n")

    # Process batch
    batch_results <- map_dfr(batch_ids, get_file_details)
    results[[length(results) + 1]] <- batch_results

    # Small delay to respect API limits
    Sys.sleep(1)
  }

  # Combine all results
  all_results <- bind_rows(results)

  # Summary statistics
  cat("\n=== SUMMARY ===\n")
  cat("Total files processed:", nrow(all_results), "\n")
  cat("Actual images found:", sum(all_results$is_image, na.rm = TRUE), "\n")
  cat("Files with errors:", sum(!is.na(all_results$error)), "\n")
  cat("Total size (MB):", round(sum(all_results$size_mb, na.rm = TRUE), 2), "\n")

  return(all_results)
}

# Example usage:
# Replace this with your actual list of file IDs/URLs
# your_file_ids <- c(
#   "1ABC123...",  # Direct file ID
#   "https://drive.google.com/file/d/1XYZ789.../view",  # URL format
#   # ... add your 1400 file IDs/URLs here
# )

yes_ids <- images_spreadsheet |>
  filter(use == "y")

your_file_ids <- yes_ids$id

# If you have your IDs in a CSV file:
# your_file_ids <- read.csv("file_ids.csv")$file_id

# Process all files
image_details <- process_image_files(your_file_ids)

# Save results
# write.csv(image_details, "google_drive_image_details.csv", row.names = FALSE)

# Filter for only images if needed
# images_only <- image_details %>% filter(is_image == TRUE)

# View results
# View(image_details)

# Example of how to analyze the results:
# # Count files by directory
# directory_counts <- image_details %>%
#   filter(is_image == TRUE) %>%
#   count(directory, sort = TRUE)
#
# # Find largest files
# largest_files <- image_details %>%
#   filter(is_image == TRUE) %>%
#   arrange(desc(size_mb)) %>%
#   select(title, directory, size_mb, created_date)
#
# # Files by creation date
# files_by_date <- image_details %>%
#   filter(is_image == TRUE) %>%
#   count(created_date, sort = TRUE)

write_rds(image_details, here("data", "image_details.rds"))
