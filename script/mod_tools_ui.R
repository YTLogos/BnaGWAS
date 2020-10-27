#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_tools_ui <- function(id) {
  ns <- NS(id)
  navbarMenu(
    "More",
    tabPanel("Sub-Component A"),
    tabPanel("Sub-Component B")
  )
}