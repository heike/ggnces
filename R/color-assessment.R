#' CIE 76 Distance between a set of colors
#'
#' The CIE 76 distance between two colors is defined as the Euclidean distance of
#' these colors in LAB space. CIE 76 is a perception based distance and is designed to be
#' device indpendent.
#' A distance of 1 is supposed to be a 'Just noticeable difference' for 50% of the population.
#' A value of 5 is considered to be visible by most. \url{https://web.archive.org/web/20080705054344/http://www.aim-dtp.net/aim/evaluation/cie_de/index.htm}
#' For large differences (beyond 10) the CIE 76 formula is known to be unreliable (cite?).
#' Great source of information: \url{https://en.wikipedia.org/wiki/Color_difference#cite_note-10}
#' @param X vector (or matrix) of colors.
#' @return distance matrix of the colors
#' @importFrom colorspace hex2RGB
#' @export
dist_cie76 <- function(X) {
  # X is by default assumed to be a matrix of values in CIELAB space
  if (class(X)!="LAB") # try whether the colors were hex to begin with
    X <- hex2RGB(X) %>%  as("LAB")

  stopifnot(class(X)=="LAB")
  dist(X@coords)
}

#' CIE 2000 Distance between a set of colors
#'
#' The CIE 2000 distance between to two colors is, similar to the CIE 76 distance,
#' based on the LAB color space, but additional adjustments are made to better
#' adjust for non-linearities in the human perception of color (we see different hues of color at
#' different resolutions).
#' @param x vector (or matrix) of colors.
#' @return distance matrix of the colors
#' @importFrom colorspace hex2RGB
#' @importFrom stats as.dist dist
#' @importFrom dplyr `%>%` mutate
#' @importFrom methods as
#' @importFrom paleval pev_hex_distance
#' @importFrom utils getFromNamespace
#' @export
dist_cie2000 <- function(x) {
  i <- j <- NULL

  # x is vector of hex code colors
  #  stopifnot(class(X)=="LAB")
  alpha <- colorspace::extract_transparency(x)
  as_hexcolor <- getFromNamespace("as_hexcolor", "paleval")

  if (all(alpha== 1)) x <- as_hexcolor(x)
  mat <- data.frame(expand.grid(i=1:length(x), j = 1:length(x)))
  mat <- mat %>% mutate(
    dist = purrr::map2_dbl(.x = i, .y = j, .f = function(.x, .y) {
      paleval::pev_hex_distance(x[.x], x[.y])
    })
  )
  mat$dist %>% matrix(nrow=length(x), ncol=length(x)) %>% as.dist()
}
