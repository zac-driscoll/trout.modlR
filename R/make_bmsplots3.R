make_bmsPlots2 <- function(dat1,
                           dat2 = NULL,
                           dat3 = NULL,
                           title = NULL,
                           ymax = NULL,
                           type = "single",
                           pounds = TRUE,
                           plotly = TRUE) {
  #get data
  bio_dat <- get_biomass_data(dat1,dat2,dat3)
  #plot options
    #format title
    gtitle <- make_bms_title(bio_dat,title,type)
    #y-axis maximum value for plot
  #plot data
   p <- make_base_bms_plot(type,bio_dat)
   p <- add_shared_bms_aes(p,type,ymax,xmin,xmax,gtitle)
    #plotly plot
    if (plotly == TRUE) {
      p <- plotly::ggplotly(p)
    }
    #output
    p
}

make_bms_title <- function(bio_dat, title, type) {
  #Premade title
  if (type != "cumulative") {
    gtitle <-
      paste0("Estimated ", bio_dat$species[1], " Biomass in ", bio_dat$mu[1])
  }
  #create title for cumulative plot
  if (type == "cumulative") {
    gtitle <- paste("Cumulative Estimated Biomass")
  }
  #user defined title
  if (!is.null(title)) {
    gtitle <- title
  }
  #return data
  gtitle
}

make_base_bms_plot <- function(type,bio_dat) {
  if (type == "single") {
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
  if(type == "multiple"){
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
  if(type == "cumulative"){
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

add_shared_bms_aes <- function(p,type,ymax,xmin,xmax,gtitle){
  if (is.null(ymax)) {
    ymax <- max(bio_dat$result)
  }
  #x-axis limits
  xmin <- min(bio_dat$year)
  xmax <- max(bio_dat$year)
  if(type != "cumulative"){
  p <-
    p +
    ggplot2::ylim(0, ymax)  +
    scale_color_custom("bw") +
    ggplot2::scale_linetype_manual(values = c("solid","dashed","dotted"))
  }
  if(type == "cumulative"){
    p <-
      p +
      scale_fill_custom("bw")
  }
  p +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(breaks = seq(xmin,xmax, 5)) +
    format_axis(1) +
    format_legend("top") +
    ggplot2::labs(x = "Year",
                  y = "Biomass (x 1,000 lb)",
                  title = gtitle)
}

zz <- get_biomass_data(dat1 = dat1,dat2 =dat2)

p <- ggplot2::ggplot(data = zz,
                     ggplot2::aes(
                       x = year,
                       y = result,
                       fill = model,
                       linetype = model)) +
  ggplot2::facet_wrap(~data_type,nrow = 2,scales = "free_y") +
  ggplot2::geom_area()

make_bmsPlots2(trout1,trout2,type = "single")
