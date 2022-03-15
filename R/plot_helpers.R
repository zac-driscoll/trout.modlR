
#' Format Axis
#'
#' A helper function used to build a custom theme for the axes.
#'
#' @param form A numeric object specifying the axis them (1 or 2)
#'
#' @return A ggplot2 theme
#'
#' @noRd
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
#' @noRd
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


#' Apply custom color palettes to ggplot2 plots
#'
#' A function used to apply custom color palettes to ggplot2 plots.
#' See custom_palletes for a list of palette options.  This  was adapted from a
#' function found in the ggtheme package.
#'
#' @param pal A character object
#'
#' @return
#' @noRd
#'
#' @examples \dontrun{scale_color_custom("muddy_river")}
scale_color_custom <- function(pal){
  ggplot2::discrete_scale("color","mmsd",custom_pal(pal))}

#' Apply custom fill palettes to ggplot2 plots
#'
#' A function used to apply custom color palettes to ggplot2 plots.
#' See custom_palletes for a list of palette options.  This  was adapted from a
#' function found in the ggtheme package.
#'
#' @param pal A character object
#'
#' @return
#' @noRd
#'
#' @examples \dontrun{scale_color_custom("muddy_river")}
scale_fill_custom <- function(pal){
  ggplot2::discrete_scale("fill","mmsd",custom_pal(pal))}

#' Custom Pallette
#'
#' Used to create a custom palette of variable size
#'
#' @param pal
#'
#' @return
#' @export
#' @noRd
#'
#' @examples custom_pal("bw")
custom_pal <- function(pal){

  palettes <- trout.modlR::custom_palettes[[pal]]

  max_n <- length(palettes)
  f <- function(n) {
    check_pal_n(n, max_n)
    palettes[[n]]
  }
  attr(f, "max_n") <- f
  f
}

#' Check Max palette number
#'
#' A helper function used to check the number of objects passed to an aesthetic
#' don't exceed the maximum number of colors within a palette
#'
#' @param n number of objects
#' @param max_n max number of colors in a palette
#'
#' @return
#' @noRd
check_pal_n <- function(n, max_n) {
  if (n > max_n) {
    warning("This palette can handle a maximum of ", max_n, " values.",
            "You have supplied ", n, ".")
  } else if (n < 0) {
    stop("`n` must be a non-negative integer.")
  }
}
