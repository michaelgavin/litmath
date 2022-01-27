#' Data - EEBO term-document matrix
#' 
#' A term-document matrix drawn from the EEBO-TCP collection, built from all publicly
#' available files with publication years between 1640 and 1699.
#' 
#' @format A matrix with 2,002 rows (for each keyword) and 18,311 columns
#' (for each document).
#' 
#' @examples
#' # Load the matrix, then analyze a term.
#' data(deebo)
#' similarity(mat = term_doc, keyword = "A48901")
"term_doc"