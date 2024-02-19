#' Add two numbers incorrectly
#' @param x This is a number that we care about. Must be Nx1 vector.
#' @param y This is the other number.
#' @return The sum of the two numbers, but it's incorrect. It will be a numeric object.
#' @examples
#' my_addition(1, 2)
#' my_addition(3, 4)
#' @import data.table
#' @import fixest
#' @export
my_addition <- function(x, y) {
  x + y + 1
}
