
#' Format Axis
#'
#' A helper function used to build a custom theme for the axes.
#'
#' @param form A numeric object specifying the axis them (1 or 2)
#'
#' @return A ggplot2 theme
#'
#' @examples ggplot2::ggplot() + format_axis(1)
format_axis <- function(form){
 if(form == 1){
   ggplot2::theme(axis.text.x = ggplot2::element_text(size=14),
                 axis.text.y = ggplot2::element_text(size=14),
                 axis.title.x = ggplot2::element_text(size=14),
                 axis.title.y = ggplot2::element_text(size=14),
                 axis.text= ggplot2::element_text(colour="black")) +
  ggplot2::theme(panel.border= ggplot2::element_rect(colour="black"))
 }
  if(form == 2){
  ggplot2::theme(axis.text.x = ggplot2::element_text(size=12),
                 axis.text.y = ggplot2::element_text(size=10),
                 axis.title.x = ggplot2::element_text(size=14),
                 axis.title.y = ggplot2::element_text(size=14),
                 axis.text = ggplot2::element_text(colour="black")) +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour="black"))
  }
}

#' Format Legend
#'
#' A helper function used to format a ggplot2 graph's legend.
#'
#' @param side A character object specifying the in which legend will be
#' positioned ("top" or "side").
#'
#' @return A ggplot2 theme
#'
#' @examples ggplot2::ggplot() + format_legend("top")
format_legend <- function(side) {
  if (side == "top") {
    ggplot2::theme(
      legend.position = "top",
      legend.spacing.x = ggplot2::unit(0.3, 'cm'),
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank()
    )
  }
  if (side == "right") {
    ggplot2::theme(legend.position = "right",
                   legend.direction = "vertical")
  }
}
