#' 
#' Plots timeseres over a place-year matrix (1500-1699).
#'
#' @export
plot_timeseries = function(mat = py, gazetteer = gaz, places, normalize = T, compose_places = T, mode = "all") {
  if (ncol(mat) != 200) {
    stop("This graphing function assumes you are using data only from 1500 through 1699.")
  }
  if (normalize == T) {
    totals = colSums(mat)
    for (j in 1:ncol(mat)) {
      mat[,j] = mat[,j] / totals[j]
    }
  }
  PLACE = c()
  YEAR = c()
  COUNT = c()
  for (i in 1:length(places)) {
    term = places[i]
    if (compose_places == T) {
      pnames = place_join(term, gaz = gazetteer, mode = mode)
      vec = place_composition(place = pnames, mat = mat)
    } else {
      vec = mat[term,]
    }

    place = rep(term, length(vec))
    year = names(vec)
    count = vec
    PLACE = c(PLACE, place)
    YEAR = c(YEAR,year)
    COUNT = c(COUNT,count)
  }
  COUNT = COUNT
  df = data.frame(PLACE, YEAR, COUNT)
  ggplot(df, aes(x = YEAR, y = COUNT, color = PLACE)) + 
    geom_point() + 
    stat_smooth(method = loess, aes(group = PLACE), se = F, fullrange = F) + 
    theme(panel.background = element_blank(),
          axis.title = element_blank()) +
    scale_x_discrete(breaks=c("1500","1600","1699"),
                     labels=c("1500", "1600", "1700"))
}
