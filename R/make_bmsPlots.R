#' Plot estimated biomass
#'
#' This function plots the estimated biomass.
#'
#' @param dat A data.frame
#' @param title A plot title.  If NULL, a default title will be used
#' @param ymax  A numeric object specifying the maximum value of the y-axis.
#' If NULL, the value will be auto selected.
#' @param plotly_plot TRUE or FALSE.  If TRUE, a \code{plotly::ggplotly()}
#' object will be output. If FALSE (default), a \code{ggplot2::ggplot()}
#' object will be output instead.
#'
#' @return A \code{ggplot2::ggplot()} or \code{plotly::ggplotly()} object
#' @export
#'
#' @examples make_bmsPlot(dat = gb_wf_ar)
make_bmsPlot <-
  function(dat,
           title = NULL,
           ymax = NULL,
           plotly_plot = FALSE) {
  #get data
    bio <- get_biomass_data(gb_wf_ar)
  #plot options
    #format title
    gtitle <- if (is.null(title)) {
      paste0("Estimated ", bio$species[1], " Biomass in ", bio$mu[1])
    } else {
      title
    }
    #y-axis maximum value for plot
    if (is.null(ymax)) {
      ymax <- max(bio$result)
    }
    #Line plot with biomass and spawning biomass
    p <-
    ggplot2::ggplot(
      data = bio,
      ggplot2::aes(
        y = result,
        x = year,
        color = data_type,
        linetype = data_type,
        group = data_type
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_linetype_manual(
        "",
        values = c("biomass" = "solid", "sp_bio" = "dashed"),
        labels = c("Total", "Female Spawning Stock")
      ) +
      ggplot2::guides(colour =
                        ggplot2::guide_legend(
                          override.aes = list(shape = c(16, 1)))) +
      ggplot2::scale_colour_manual(
        "",
        values = c("biomass" = "black", "spbiomass" = "black"),
        labels = c("Total", "Female Spawning Stock")
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = seq(min(bio$year), max(bio$year), 5)) +
      format_axis(1) +
      format_legend("top") +
      ggplot2::ylim(0, ymax) +
      ggplot2::labs(x = "Year",
                    y = "Biomass (x 1,000 lb)",
                    title = gtitle)

    if (plotly_plot == "TRUE") {
      p <- plotly::ggplotly(p)
    }
    p
  }
