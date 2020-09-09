#' Outersection of two vectors
#'
#' @param x vector
#' @param y vector
#'
#' @return vector
#' @export
#'
#' @examples
#' outersect(c(1, 2, 3), c(2, 3, 4))
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
