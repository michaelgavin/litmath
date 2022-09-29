#' 
#' Given a gazetteer and a place name, finds all places that correspond to
#' and/or are contained within it.
#'
#' @export
place_join = function(places, gaz, mode = "all") {
  places = c(places,gaz$SUBJECT[intersect(grep("same", gaz$PREDICATE), which(gaz$OBJECT %in% places))])
  places = c(places,gaz$OBJECT[intersect(grep("same", gaz$PREDICATE), which(gaz$SUBJECT %in% places))])
  places = c(places,gaz$SUBJECT[intersect(grep("same", gaz$PREDICATE), which(gaz$OBJECT %in% places))])
  places = unique(places)
  
  if (mode == "all") {
    places = c(places, gaz$SUBJECT[intersect(grep("is in", gaz$PREDICATE), which(gaz$OBJECT %in% places))])
    places = c(places, gaz$SUBJECT[intersect(grep("is in", gaz$PREDICATE), which(gaz$OBJECT %in% places))])
    places = c(places,gaz$SUBJECT[intersect(grep("same", gaz$PREDICATE), which(gaz$OBJECT %in% places))])
    places = c(places,gaz$OBJECT[intersect(grep("same", gaz$PREDICATE), which(gaz$SUBJECT %in% places))])
    places = unique(places)
  }
  return(places)
}
