#' Data - EEBO keword-in-context matrix
#' 
#' A word-context matrix drawn from the EEBO-TCP collection, built from all publicly
#' available files with publication years between 1640 and 1699.
#' 
#' @format A matrix with 32,244 rows and 2,002 columns.
#' 
#' @examples
#' # Load the matrix, then analyze a term.
#' data(kwic)
#' similarity_map(mat = kwic, keyword = "rights")
"kwic"