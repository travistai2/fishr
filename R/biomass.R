#' Calculate biomass index
#'
#' Calculates biomass from cpue and area fished
#'
#' @param cpue Numeric vector of CPUE values
#' @param area_swept Numeric vector of area swept (e.g., km^2)
#'
#' @return A numeric vector of biomass index values
#' @export
#'
#' @examples
#' salmon_cpue <- cpue(catch = 2, effort = 2)
#' biomass_index(salmon_cpue, 5)
biomass_index <- function(cpue, area_swept) {
  cpue * area_swept
}
