#' 
#' Calculates ppmi for a matrix
#'
#' @export
ppmi = function(mat) {
  total = sum(mat, na.rm = T)
  pcol = apply(mat, 2, function(x) sum(x, na.rm = T) / total)
  prow = apply(mat, 1, function(x) sum(x, na.rm = T) / total)
  pmat = as.matrix(prow) %*% t(pcol)
  mat = mat / total
  mat = mat / pmat
  mat = apply(mat, 1, log)
  mat = t(mat)
  mat[mat < 0] = 0
  mat[is.na(mat)] = 0
  return(mat)
}
