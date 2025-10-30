box::use(
    tibble[as_tibble], 
    datasets[iris]
)

#' @export
pi = pi

#' @export
iris = as_tibble(iris)

#' Fuel economy data from 1999 to 2008 for 38 popular models of cars
#' 
#' Take not that this is a dataset from \code{ggplot2} package. Run \code{\link[ggplot2]{?ggplot2::mpg}} for more details.
#' 
#' @export
mpg = readRDS(box::file('data/mpg.rds'))

#' @export
name_md = paste0("Name of the module: ", box::name())
