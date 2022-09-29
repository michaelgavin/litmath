#' 
#' Measures the semantic footprint of a term (computed over place-keyword matrix)
#'
#' @export
semantic_footprint = function(mat, term, coords) {
  vec = mat[,term]
  vec = vec[vec > 0]
  vec = vec[names(vec) %in% coords[,"NAME"]]
  hits = which(coords[,"NAME"] %in% names(vec))
  vec = vec[coords[hits,"NAME"]]
  mean_lon = sum(coords[hits,"LON"] * vec[vec > 0]) / sum(vec)
  mean_lat = sum(coords[hits,"LAT"] * vec[vec > 0]) / sum(vec)
  distances = c()
  for (i in hits) {
    lat = coords[i,"LAT"]
    lon = coords[i,"LON"]
    d = great_circle_distance(lon1 = mean_lon,
                         lat1 = mean_lat,
                         lon2 = lon,
                         lat2 = lat)
    distances = c(distances,d)
  }
  mean_dist = sum(distances * vec) / sum(vec)
  sd_dist = sd(distances)
  results = list()
  results$n = length(vec)
  results$freq = sum(vec)
  results$sd = sd(vec)
  results$lon = mean_lon
  results$lat = mean_lat
  results$radius = mean_dist
  results$stdev = sd_dist
  return(results)
}
