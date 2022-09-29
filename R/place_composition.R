#' 
#' Adds together the vectors corresponding to a list of place names.
#'
#' @export
place_composition = function(mat, places) {
  places = places[places %in% rownames(mat)]
  if (length(places) > 1) {
    mat = mat[places,]
    mat = as.matrix(mat)
    vec = colSums(mat)
  } else {
    vec = mat[places,]
  }
  return(vec)
}
