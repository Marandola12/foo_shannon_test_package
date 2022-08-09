#' Title
#'
#' @param x A vector with values of abundance in the community
#'
#' @return A numeric vector of len = 1 with the shannon diversity value
#' @export
#'
#' @examples
#' com <- c(1,2,3)
#' my_shannon(com)

my_shannon <- function(x){
  x <- x[x > 0]
  sum_all <- sum(x)
  pi <- x/sum_all
  -sum(pi * log(pi))
}
