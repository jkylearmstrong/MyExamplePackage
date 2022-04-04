#' add two numbers 
#'
#' A sample function that adds two inputs
#'
#' @param x a number 
#' @param y a number 
#' @return the sum of x and y  
#' @export
#' @examples
#' \dontrun{
#' add_two_nums(3,4)
#'}

add_two_nums <- function(x, y) {
  x + y
}

add_two_nums(3,10)

add_two_nums(3L,pi)
add_two_nums(c(2,3),c(1,2))
