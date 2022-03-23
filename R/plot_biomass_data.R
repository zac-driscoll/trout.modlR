#' Plot Biomass Data
#'
#'  \code{plot_biomass_data} plots biomass for a given model or models.
#'  Different graphs can be output by using the 'type' argument.
#'
#' @param dat1 A .rdat containing ADMB output
#' @param dat2 A .rdat containing ADMB output
#' @param dat3 A .rdat containing ADMB output
#' @param title A character object containing the plot title.
#' If left NULL (default), a title will be automatically generated
#' @param ymax A number representing the max for the y-axis. If left
#' null(default), this will be chosen automatically.
#' @param type "single","multiple", or "cumulative". This determines
#' the type of plot that will be produced. If "single" is selected
#' model results will be plotted for an individual model.
#' Only supply a '.rdat' object to 'dat1' when using 'single'.
#' The 'multiple' argument will add biomass data from multiple
#' models to one plot. The 'cumulative' type will produce a stacked
#' area plot containing biomass from multiple models.
#' When 'cumulative' or 'multiple' are specified, pass additional
#' '.rdat' files to 'dat2' and/or 'dat3'
#' @param plotly TRUE or FALSE (default). If TRUE, a plotly::ggplotly()
#' object will be returned.
#'
#' @return A ggplot2::ggplot() or plotly::ggplotly() object
#' @export
#'
#' @examples plot_biomass_data(dat1 = trout1,type = 'single')
#' plot_biomass_data(dat1 = trout1,dat2 = trout2, type = 'multiple')
#' plot_biomass_data(dat1 = trout1,dat2 = trout2, type = 'cumulative')
plot_biomass_data <- function(dat,
                           title = NULL,
                           ymax = NULL,
                           cumulative = FALSE,
                           plotly = FALSE) {
  #run error checks
  # run_bms_error_check(dat1,dat2,dat3,type)
  #get data
  bio_dat <- get_biomass_data(dat)
  #format title
  gtitle <- make_bms_title(bio_dat,title,dat,cumulative)
  #plot data
   p <- make_base_bms_plot(bio_dat,dat,cumulative)
   p <- add_shared_bms_aes(p,bio_dat,cumulative,ymax,gtitle)

  #plotly plot
  if (plotly == TRUE) {p <- plotly::ggplotly(p)}
  #output
  p
}

#' Make Biomass Title
#'
#' A helper function used by \code{make_bmsPlot}.  Creates
#' a plot title for the Biomass plot.
#'
#' @param bio_dat A tibble containing biomass data for one or more
#' models
#' @param title A title if provided
#' @param type The plot type
#'
#' @return
#' @noRd
#'
make_bms_title <- function(bio_dat, title, dat,cumulative) {
  #Premade title
  if (length(dat) == 1) {
    gtitle <-
      paste0("Estimated ", bio_dat$species[1], " Biomass in ", bio_dat$mu[1])
  }

  if (length(dat) > 1 & isFALSE(cumulative)) {
    gtitle <-
      paste0("Estimated ", bio_dat$species[1], " Biomass")
  }

  #create title for cumulative plot
  if (isTRUE(cumulative)) {
    gtitle <- paste("Cumulative Estimated Biomass")
  }
  #user defined title
  if (!is.null(title)) {
    gtitle <- title
  }
  #return data
  gtitle
}

#' Make Biomass Base Plot
#'
#' A helper function used within \code{plot_biomass_data}.
#' This provides the base plot for the final plot. The
#' \code{type} determines what base is used.
#'
#' @param type The plot type
#' @param bio_dat The biomass data
#'
#' @noRd
#'
#' @return A ggplot::ggplot() object
make_base_bms_plot <- function(bio_dat,dat,cumulative) {
  if (length(dat) == 1) {
    p <-
      ggplot2::ggplot(
        data = bio_dat,
        ggplot2::aes(
          y = result,
          x = year,
          color = data_type,
          linetype = data_type,
          group = data_type
        )
      ) +
      ggplot2::geom_line() +
      ggplot2::geom_point()
  }
  #model compare
  if(length(dat)>1 & isFALSE(cumulative)){
    p <- ggplot2::ggplot(data = bio_dat,
                         ggplot2::aes(
                           x = year,
                           y = result,
                           color = model,
                           linetype = model)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~data_type,nrow = 2,scales = "free_y")
    }
  #shared plot aesthetics aesthetics
  if(isTRUE(cumulative)){
    p <- ggplot2::ggplot(data = bio_dat,
                         ggplot2::aes(
                           x = year,
                           y = result,
                           fill = model)) +
      ggplot2::facet_wrap(~data_type,nrow = 2,scales = "free_y") +
      ggplot2::geom_area()
  }
  p
}

#' Add Shared Aesthetics to Biomass Plot
#'
#' A helper function used to add additional customization
#' to the output of \code{plot_biomass_data}.
#'
#' @param p A ggplot2::ggplot() object
#' @param bio_dat Biomass Data
#' @param type The type of plot being produced
#' @param ymax The maximum value for the y axis
#' @param gtitle The title of the plot
#'
#' @noRd
#'
#' @return A ggplot2::ggplot() object
add_shared_bms_aes <- function(p,bio_dat,cumulative,ymax,gtitle){
  #specify xmin and xmax
  xmin <- min(bio_dat$year)
  xmax <- max(bio_dat$year)
  #non-cumulative specific graph options
  if (isFALSE(cumulative)) {
    p <-
      p +
      scale_color_custom("bw")
    #add ymax if specified
    if (!is.null(ymax)) {
      p <- p + ggplot2::ylim(0, ymax)
    }
  }
  #cumulative specific graph options
  if(isTRUE(cumulative)){
    p <-
      p +
      scale_fill_custom("bw")
  }
  #shared graph options
  p +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(breaks = seq(xmin,xmax, 5)) +
    format_axis(1) +
    format_legend("top") +
    ggplot2::labs(x = "Year",
                  y = "Biomass (x 1,000 lb)",
                  title = gtitle)
}
#' Error checks for Biomass Plotting Function
#'
#' \code{run_bms_error_check} runs error checks for the
#' \code{plot_biomass_data} function.
#'
#' @param dat1 A .rdat file containing ADMB data output
#' @param dat2 A .rdat file containing ADMB data output
#' @param dat3 A .rdat file containing ADMB data output
#' @param type The plot
#'
#' @noRd
#'
#' @return Error checks
run_bms_error_check <- function(){

}
