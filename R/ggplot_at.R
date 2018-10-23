#' The ggplot2 equivalent to the scoped tidyverse functions.
#'
#' @param data Default dataset to use for plot.
#' @param xvars Variables to include/exclude in the plot. You can use same specifications as in \code{\link[dplyr]{select}}.
#' @return A list of ggplot-objects
#' @examples
#' ggplot_at(mtcars, vars(mpg:disp), vars(hp:wt), limit = 9)
#'
#'ggplot_at(mtcars, vars(mpg:disp), vars(hp:wt), limit = 9) %>%
#'  add_layer(geom_point())
#'
#'ggplot_at(mtcars, vars(mpg:disp), vars(hp:wt), limit = 9) %>%
#'  add_layer(geom_point(), aes(color = qsec))
#' @importFrom tidyselect vars_select
#' @importFrom rlang !!!
#' @export

ggplot_at <- function(data, xvars, yvars = NULL, limit = 100){
  if (is.null(yvars)){
    xvars <- tidyselect::vars_select(names(data), !!!xvars)
    to_plot <- data.frame(x = xvars, stringsAsFactors = FALSE)
    if(nrow(to_plot)>limit)stop("You're about to plot more then ",
                                limit,
                                " plots. Set your limit at least to ",
                                nrow(to_plot), ".")
    out <- purrr::map(to_plot$x, ~ggplot(data, aes(x = !!sym(.x))))
    names(out) <- to_plot$x
  }else{
    xvars <- tidyselect::vars_select(names(data), !!!xvars)
    yvars <- tidyselect::vars_select(names(data), !!!yvars)
    to_plot <- expand.grid(x = xvars, y = yvars, stringsAsFactors = FALSE)
    if(nrow(to_plot)>limit)stop("You're about to plot more then ",
                                limit,
                                " plots. Set your limit at least to ",
                                nrow(to_plot), ".")
    out <- purrr::map2(to_plot$x, to_plot$y, ~ggplot(data, aes(x = !!sym(.x), y = !!sym(.y))))
    names(out) <- paste(to_plot$x, to_plot$y, sep = "-")
  }
  return(out)
}
