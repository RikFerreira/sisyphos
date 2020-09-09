#' Breaks a vector as a boxplot
#'
#' @param x vector
#' @param hinge scalar
#'
#' @return vector
#' @export
#'
#' @examples
boxcut <- function(x, hinge = 1.5) {
  x <- unname(x)

  qt <- quantile(x)
  iqr <- IQR(x)
  upf <- qt[4] + hinge * iqr
  lof <- qt[2] - hinge * iqr

  box <- vector(mode = "numeric", length = 7)

  if(lof < qt[1]) {
    box[1] <- floor(qt[1])
    box[2] <- qt[1]
  } else {
    box[1] <- qt[1]
    box[2] <- lof
  }

  if(upf > qt[5]) {
    box[7] <- ceiling(qt[5])
    box[6] <- qt[5]
  } else {
    box[7] <- qt[5]
    box[6] <- upf
  }

  box[3:5] <- qt[2:4]

  return(box)
}
