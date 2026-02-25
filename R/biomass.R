#' Calculate biomass index
#'
#' Calculates biomass from cpue and area fished
#'
#' @param cpue Numeric vector of CPUE values
#' @param area_swept Numeric vector of area swept (e.g., km^2)
#' @param catch catch value numeric
#' @param effort effort value numeric
#' @param verbose Logical, indicating whether to print processing messages (default is FALSE, also accepts value of `fishr.verbose`)
#' @param ... arguments passed to cpue
#'
#' @return A numeric vector of biomass index values
#' @export
#'
#' @examples
#' salmon_cpue <- cpue(catch = 2, effort = 2)
#' biomass_index(salmon_cpue, 5)
biomass_index <- function(cpue = NULL, area_swept, catch = NULL, effort = NULL,
                          verbose = getOption("fishr.verbose", default = FALSE),
                          ...) {
  rlang::check_dots_used()

  if (is.null(cpue) && (!is.null(catch) && !is.null(effort))) {
    cpue <- cpue(catch, effort, ...)
  }

  if (verbose) {
    message("Processing ", length(cpue), " records")
  }

  cpue * area_swept
}
