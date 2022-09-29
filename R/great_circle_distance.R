#' 
#' Computes the great-circle distance between two coordinate points.
#'
#' @export
great_circle_distance = function(lon1, lat1, lon2, lat2, radius = 3437) {
  # Note default radius in miles, assuming 60 miles per degree
  degrees_to_radians = function(x) return(x*pi/180)
  lon1 = degrees_to_radians(lon1)
  lat1 = degrees_to_radians(lat1)
  lon2 = degrees_to_radians(lon2)
  lat2 = degrees_to_radians(lat2)
  d = acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(lon2-lon1)) * radius
  return(d)
}
