#' Global Configuration Management for Study Wrangler
#'
#' Functions to manage global configuration settings for validation profiles
#' and other package-wide settings.
#'
#' @name config
NULL

#' Package environment for storing global configuration
#' @keywords internal
.study_wrangler_env <- new.env(parent = emptyenv())

#' Default configuration values
#' @keywords internal
.default_config <- list(
  validation = list(
    profiles = "baseline"
  )
)

#' Initialize configuration environment
#' @keywords internal
.init_config <- function() {
  if (!exists("config", envir = .study_wrangler_env)) {
    assign("config", .default_config, envir = .study_wrangler_env)
    # Try to load config from files on first initialization
    .load_config_files()
  }
}

#' Load configuration from .wranglerrc files
#' @keywords internal
.load_config_files <- function() {
  # Check for config files in order of precedence:
  # 1. Current working directory
  # 2. Home directory
  
  config_files <- c(
    file.path(getwd(), ".wranglerrc"),
    file.path(Sys.getenv("HOME"), ".wranglerrc")
  )
  
  for (config_file in config_files) {
    if (file.exists(config_file)) {
      tryCatch({
        file_config <- yaml::read_yaml(config_file)
        if (!is.null(file_config)) {
          # Merge file config with current config using list_modify
          current_config <- get("config", envir = .study_wrangler_env)
          merged_config <- purrr::list_modify(current_config, !!!file_config)
          assign("config", merged_config, envir = .study_wrangler_env)
          message("Loaded configuration from: ", config_file)
          break  # Use first found config file
        }
      }, error = function(e) {
        warning("Failed to load config from ", config_file, ": ", e$message)
      })
    }
  }
}

#' Set global configuration
#'
#' Set global configuration values that will be used as defaults
#' throughout the package. Uses dot notation for nested configuration paths.
#'
#' @param ... Named configuration values using dot notation for nested paths.
#'   For example, `validation.profiles = c("baseline", "eda")` sets the
#'   validation profiles.
#'
#' @return Invisibly returns the updated configuration list
#' @export
#'
#' @examples
#' # Set default validation profiles
#' set_config(validation.profiles = "baseline")
#' set_config(validation.profiles = c("baseline", "eda"))
#'
#' # Future examples of other config options:
#' # set_config(export.default_format = "vdi")
#' # set_config(validation.strict_mode = TRUE)
set_config <- function(...) {
  .init_config()
  
  current_config <- get("config", envir = .study_wrangler_env)
  config_args <- list(...)
  
  if (length(config_args) == 0) {
    warning("No configuration values provided")
    return(invisible(current_config))
  }
  
  # Convert dot notation to nested list structure
  updates <- .convert_dot_notation_to_list(config_args)
  
  # Merge updates into current config using list_modify for proper replacement
  updated_config <- purrr::list_modify(current_config, !!!updates)
  assign("config", updated_config, envir = .study_wrangler_env)
  
  invisible(updated_config)
}

#' Convert dot notation arguments to nested list
#' @keywords internal
.convert_dot_notation_to_list <- function(args) {
  result <- list()
  
  for (name in names(args)) {
    value <- args[[name]]
    
    # Split on dots to create nested structure
    parts <- strsplit(name, "\\.")[[1]]
    
    # Build nested structure by constructing path
    if (length(parts) == 1) {
      # Simple case - no dots
      result[[parts[1]]] <- value
    } else {
      # Complex case - build nested structure from inside out
      # Start with the value and wrap it in nested lists
      temp_list <- value
      for (i in length(parts):2) {
        temp_list <- setNames(list(temp_list), parts[i])
      }
      
      # Now merge the fully built nested structure with result
      if (parts[1] %in% names(result)) {
        result[[parts[1]]] <- purrr::list_modify(result[[parts[1]]], !!!temp_list)
      } else {
        result[[parts[1]]] <- temp_list
      }
    }
  }
  
  result
}

#' Get global configuration
#'
#' Retrieve the current global configuration settings.
#'
#' @return A list containing the current configuration settings
#' @export
#'
#' @examples
#' # Get current configuration
#' get_config()
#'
#' # Get just the validation profiles
#' get_config()$validation$profiles
get_config <- function() {
  .init_config()
  get("config", envir = .study_wrangler_env)
}

#' Get validation profiles from configuration
#'
#' Helper function to extract validation profiles from the global configuration.
#' This function provides the fallback logic for validation functions.
#'
#' @param profile Character vector of profiles to use. If NULL, falls back to
#'   global configuration.
#'
#' @return Character vector of validation profiles to use
#' @export
#'
#' @examples
#' # Get profiles from argument or global config
#' get_validation_profiles(c("baseline", "eda"))
#' get_validation_profiles(NULL)  # Uses global config
get_validation_profiles <- function(profiles = NULL) {
  if (!is.null(profiles)) {
    return(profiles)
  }
  
  config <- get_config()
  profiles <- config$validation$profiles
  
  if (is.null(profiles) || length(profiles) == 0) {
    return("baseline")  # Ultimate fallback
  }
  
  profiles
}

#' Reset configuration to defaults
#'
#' Reset the global configuration to default values, ignoring any
#' configuration files.
#'
#' @return Invisibly returns the reset configuration
#' @export
#'
#' @examples
#' # Reset to defaults
#' reset_config()
reset_config <- function() {
  assign("config", .default_config, envir = .study_wrangler_env)
  invisible(.default_config)
}