#' Add layer to a plotlist
#' @export
add_layer <- function(plot_list, ...){
  dots <- rlang::enquos(...)
  purrr::map(plot_list, `+`, list(purrr::map(dots, rlang::eval_tidy)))
}
