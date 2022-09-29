#' Computes cosine similarity between two vectors.
#' 
#' @param x A vector of numbers.
#' 
#' @param y Another vector of numbers, of the same length as x.
#'                
#' 
#' @examples
#' x = c(3,0,0,2)
#' y = c(0,2,0,1)
#' cosine_similarity(x, y)
#' @export
cosine_similarity = function(x, y) {
  x %*% y/(sqrt(x %*% x) * sqrt(y %*% y))
}
