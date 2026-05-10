#' Defensive assertions used across the package.
#' @keywords internal
#' @noRd
.assert_string <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a single non-empty string.", name), call. = FALSE)
  }
  invisible(TRUE)
}

.assert_character <- function(x, name = deparse(substitute(x)), min_len = 1L) {
  if (!is.character(x) || anyNA(x) || length(x) < min_len) {
    stop(sprintf("`%s` must be a character vector of length >= %d with no NAs.",
                 name, min_len), call. = FALSE)
  }
  invisible(TRUE)
}

.assert_date <- function(x, name = deparse(substitute(x))) {
  d <- tryCatch(as.Date(x), error = function(e) NA)
  if (length(d) != 1L || is.na(d)) {
    stop(sprintf("`%s` must be coercible to a single Date (got %s).",
                 name, paste(deparse(x), collapse = " ")), call. = FALSE)
  }
  invisible(d)
}

.assert_choice <- function(x, choices, name = deparse(substitute(x))) {
  .assert_string(x, name)
  if (!x %in% choices) {
    stop(sprintf("`%s` must be one of: %s. Got '%s'.",
                 name, paste(choices, collapse = ", "), x), call. = FALSE)
  }
  invisible(TRUE)
}

.assert_square_matrix <- function(x, name = deparse(substitute(x))) {
  if (!is.matrix(x) || !is.numeric(x) || nrow(x) != ncol(x) || nrow(x) < 2L) {
    stop(sprintf("`%s` must be a numeric square matrix of size >= 2.", name),
         call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("`%s` must contain only finite, non-NA values.", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Constants exposed to other files / tests.
#' @keywords internal
#' @noRd
.TRANSPORT_TYPES <- c("plane", "train", "bus", "car")
.TRAVEL_STYLES   <- c("scenic", "fastest", "cheapest")
