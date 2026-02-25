#' Calculate catch per unit effort (CPUE)
#'
#' Calculates CPUE from catch and effort data, with optional gear standardization.
#'
#' @param catch Numeric vector of catch
#' @param effort Numeric vector of effort
#' @param gear_factor Numeric adjustment for gear standardize
#' @param verbose
#'
#' @return A numeric vector of CPUE
#' @export
#'
#' @examples
#' cpue(100,10)
#' cpue(100,10, gear_factor = 0.5)
cpue <- function(catch, effort, gear_factor = 1, verbose = FALSE) {

  if (verbose) {
    message("Processing ", length(catch), " records")
  }
  raw_cpue <- catch / effort

  raw_cpue * gear_factor
}
