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
    params <-
      get_parameters(dat, c("Biomass", "SpBio", "years", "mu", "spp"))
    #assign parameters to environment
    for (i in 1:length(params)) {
      assign(unlist(names(params[i])),
             unlist(params[i]))
    }
    #read in biomass and spawning biomass
    bio1 <-
      data.frame(cbind(years,
                       (Biomass * 2.2046) / 1000,
                       (SpBio * 2.2046) / 1000)) #convert to pounds
    colnames(bio1) <- c("year", "biomass", "spbiomass")

    #plot options
    #format title
    gtitle <- if (is.null(title)) {
      paste0("Estimated ", spp, " Biomass in ", mu)
    } else {
      title
    }
    #y-axis maximum value for plot
    if (is.null(ymax)) {
      ymax <- max(bio1[, 2:3])
    }
    #Line plot with biomass and spawning biomass
    p <-
      ggplot2::ggplot(bio1, ggplot2::aes(x = year)) +
      #plot data
      ggplot2::geom_line(ggplot2::aes(
        y = biomass,
        colour = "biomass",
        linetype = "biomass"
      )) +
      ggplot2::geom_point(ggplot2::aes(y = biomass,
                                       colour = "biomass")) +
      ggplot2::geom_point(ggplot2::aes(y = spbiomass,
                                       colour = "spbiomass"),
                          shape = 1) +
      ggplot2::geom_line(ggplot2::aes(
        y = spbiomass,
        colour = "spbiomass",
        linetype = "spbiomass"
      )) +
      ggplot2::ylim(0, ymax) +
      ggplot2::labs(x = "Year",
                    y = "Biomass (x 1,000 lb)",
                    title = gtitle) +
      #scale color, axis, and linetype
      ggplot2::scale_colour_manual(
        "",
        values = c("biomass" = "black", "spbiomass" = "black"),
        labels = c("Total", "Female Spawning Stock")
      ) +
      ggplot2::scale_x_continuous(breaks = seq(min(years), max(years), 5)) +
      ggplot2::scale_linetype_manual(
        "",
        values = c("biomass" = "solid", "spbiomass" = "dashed"),
        labels = c("Total", "Female Spawning Stock")
      ) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(shape = c(16, 1)))) +
      #set theme
      ggplot2::theme_bw() +
      format_axis(1) +
      format_legend("top")
    if (plotly_plot == "TRUE") {
      p <- plotly::ggplotly(p)
    }
    p
  }
