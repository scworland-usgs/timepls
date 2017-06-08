#' @export
#' @param pH integer value


is_valid_pH <- function(pH){
  pH >= 0 & pH <= 14
}
