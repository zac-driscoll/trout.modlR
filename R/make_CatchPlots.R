#plot of harvest by number
graph_catchPlot <- function(dat,type,title="Y",plotly_plot = FALSE){
  #process data for plots
  catch_data <- process_catch_data(dat,type)
  #plot title
  gtitle <- format_catchPlot_title(dat,type,title)
  #make plot
  p <- make_catchPlot(dat,catch_data,type,gtitle)
  if(plotly_plot == TRUE){p <- plotly::ggplotly(p)}
  return(p)
}

process_catch_data <- function(dat,type){
  gear <- c("Gill","Trap","Trawl","Recreational")
  param <- ifelse(type == "harvest",
                  c("GLcatN","TPcatN","TWcatN","RCcatN"),
                  c("obsGLcat","obsTPcat","obsTWcat","obsRCcat")
  )
  purrr::map2_df(
    .x = gear,
    .y = param,
    .f =
      purrr::possibly(
        ~ get_parameter(dat, .y) %>%
          data.frame() %>%
          dplyr::rename(yield = 1) %>%
          tibble::rownames_to_column("year") %>%
          dplyr::mutate(fishery = .x,
                        year = as.numeric(year)),
        data.frame(
          year = NA,
          yield = NA,
          fishery = NA
        )
      )
  ) %>%
  na.omit()
}

make_catchPlot <- function(dat,catch_dat,type, title) {
  if (type == "harvest") {
    years <- get_parameter(dat,"years")
    p <- ggplot2::ggplot(catch_dat,
                    ggplot2::aes(
                      x = year,
                      y = yield / 1000,
                      fill = fishery
                    )) +
      ggplot2::geom_bar(colour = "black", stat = "identity") +
      ggplot2::labs(x = "Year",
                    y = "Number of Fish (x 1,000)",
                    title = title) +
      ggplot2::scale_x_continuous(breaks = seq(min(years),
                                               max(years), 5)) +
      ggplot2::scale_fill_grey("", start = 0.9, end = 0.2) +
      ggplot2::theme_bw() +
      format_axis(1) +
      format_legend("top")
  }
  if(type == "yield"){
    #Stacked bar plot of harvest (weight) by fishery type
    p <- ggplot2::ggplot(catch_dat,
                         ggplot2::aes(x=year,
                                      y=(yield*2.2046)/1000,
                                      fill=fishery))+
      ggplot2::labs(x="Year",y="Yield (x 1,000 lb)",title=title) +
      ggplot2::geom_bar(colour="black",stat="identity")+
      ggplot2::scale_x_continuous(breaks=seq(min(catch_dat$year),
                                             max(catch_dat$year),
                                             5)) +
      ggplot2::scale_fill_grey("", start=0.9, end=0.2) +
      ggplot2::theme_bw() +
      format_axis(1) +
      format_legend("top")
  }
  return(p)
}

format_catchPlot_title <- function(dat, type, title) {
  spp <- get_parameter(dat, "spp")
  mu <- get_parameter(dat, "mu")
  gtitle <-
    ifelse(
      type == "harvest",
      paste0("Commercial and Recreational ",
             spp, " Harvest in ", mu),
      paste0("Yield of ", spp, " in ", mu)
    )
  if (title == "N") {
    gtitle <- ""
  }
  gtitle
}
