#' Calculate catch per unit effort (CPUE)
#'
#' Calculates CPUE from catch and effort data, with optional gear standardization.
#'
#' @param catch Numeric vector of catch (e.g. **kg**) or a data.frame with "catch" and "effort" columns
#' @param ... Additional argument passed on to methods
#'
#' @export
cpue <- function(catch, ...) {
  UseMethod("cpue")
}


#' @rdname cpue
#' @param effort Numeric vector of effort (e.g. **hours**)
#' @param gear_factor Numeric adjustment for gear standardize
#' @param method Character; one of `"ratio"` or `"log"`
#' @param verbose Logical, indicating whether to print processing messages (default is FALSE, also accepts value of `fishr.verbose`)
#'
#' @return A numeric vector of CPUE
#' @export
#'
#' @examples
#' cpue(100,10)
#' cpue(100,10, gear_factor = 0.5)
cpue.numeric <- function(
    catch,
    effort,
    gear_factor = 1,
    method = c("ratio", "log"),
    verbose = getOption("fishr.verbose", default = FALSE),
    ...
) {

  validate_numeric_inputs(catch = catch, effort = effort)
  if (verbose) {
    message("Processing ", length(catch), " records")
  }

  method <- match.arg(method)

  raw_cpue <- switch(
    method,
    ratio = catch / effort,
    log = log(catch / effort)
  )

  new_cpue_result(raw_cpue * gear_factor,
                  method = method,
                  gear_factor = gear_factor,
                  n_records = length(catch))

}

#' @rdname cpue
#' @export
cpue.data.frame <- function(
    catch,
    gear_factor = 1,
    method = c("ratio", "log"),
    verbose = getOption("fishr.verbose", default = FALSE),
    ...
) {
  if (!"catch" %in% names(catch)) {
    stop("Column 'catch' not found.", call. = FALSE)
  }
  if (!"effort" %in% names(catch)) {
    stop("Column 'effort' not found.", call. = FALSE)
  }

  cpue(
    catch = catch[["catch"]],
    effort = catch[["effort"]],
    gear_factor = gear_factor,
    method = method,
    verbose = verbose,
    ...
  )
}

#' @rdname cpue
#' @export
cpue.default <- function(catch, ...) {
  stop("Unsupported input type for cpue(): ", class(catch), call. = FALSE)
}


#' @export
print.cpue_result <- function(x, ...) {
  cat("CPUE Results for", length(x), "records\n")
  cat("CPUE Result\n")
  cat("Records:     ", attr(x, "n_records"), "\n")
  cat("Method:      ", attr(x, "method"), "\n")
  cat("Gear factor: ", attr(x, "gear_factor"), "\n")
  cat("Values:      ", round(x, 2), "\n")
  invisible(x)
}

#' @export
summary.cpue_result <- function(object, ...) {
  cat("Survey Result Summary\n")
  cat("---------------------\n")
  cat("Method:      ", attr(object, "method"), "\n")
  cat("Records:     ", attr(object, "n_records"), "\n")
  cat("Gear factor: ", attr(object, "gear_factor"), "\n")
  cat("Mean CPUE:   ", round(mean(object), 2), "\n")
  cat("Median CPUE: ", round(stats::median(object), 2), "\n")
  cat("SD CPUE:     ", round(stats::sd(object), 2), "\n")
  invisible(object)
}

new_cpue_result <- function(values, method, gear_factor, n_records) {
  structure(
    values, # the object
    # metadata as named attributes
    method = method,
    gear_factor = gear_factor,
    n_records = n_records,
    class = "cpue_result"
  )
}





