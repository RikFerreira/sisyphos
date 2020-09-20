#' str_extract_words
#'
#' @param string
#' @param n_words
#'
#' @return string
#' @export
#'
#' @examples
str_extract_words <- function(string, n_words) {
  spaces <- stringr::str_locate_all(string, " ") %>%
    unlist() %>%
    matrix(ncol = 2) %>%
    tibble::as_tibble() %>%
    dplyr::pull(V1)

  return(stringr::str_sub(string, 1, spaces[n_words] - 1))
}
