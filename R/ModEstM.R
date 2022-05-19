#' Computes the modes, i.e. the local maxima fo the density function
#' for a given empirical distribution
#' 
#' @param x : the random values
#' @param ... : other parameters, passed to density. The main use of this feature
#' is to increase "adjust" in order to suppress spurious local density maxima.
#' 
#' @importFrom stats density
#' @importFrom rlang .data
#' @importFrom dplyr mutate filter group_by slice pull arrange desc
#' 
#' @return a list of the modes, in decreasing order of the corresponding density.
#' It allows to suppress the less significant modes, if necessary.
#' @export
#' 
#' @examples
#' require(dplyr)
#' 
#' x1 <- c(rbeta(1000, 23, 4))
#' x2 <- c(rbeta(1000, 23, 4), rbeta(1000, 4, 16))
#' 
#' Distribs <-
#'   rbind(data.frame(case = 1, XX = x1), data.frame(case = 2, XX = x2))
#' 
#' Adjust <- 1
#' 
#' Modes <- Distribs |> 
#'   group_by(case) |> 
#'   summarise(mode = ModEstM(XX, adjust = Adjust))
#' Modes$case
#' Modes$mode
#' 
#' ChosenCase <- 2
#' 
#' values <- Distribs |>
#'   filter(case == ChosenCase) |> 
#'   pull(XX)
#' plot(density(values, adjust = Adjust))
#' abline(v = Modes |> filter(case == ChosenCase) |> pull(mode) |> unlist())
#' 
ModEstM <- function(x, ...){
  Density = stats::density(x, ...)
  ###
  data.frame(abciss = Density$x,
             density = Density$y) |> 
    dplyr::mutate(decreasing = c(FALSE, diff(.data$density) < 0),
                  localextremum = c(FALSE, diff(.data$decreasing) != 0),
                  nblocalextrema = cumsum(.data$localextremum)) |>
    dplyr::filter(.data$decreasing) |> 
    dplyr::group_by(.data$nblocalextrema) |> 
    dplyr::slice(1) |> 
    dplyr::arrange(desc(density)) |>
    dplyr::pull(.data$abciss) |> 
    list()
}