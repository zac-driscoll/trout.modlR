#plot estimated biomass
make_bmsPlot <- function(dat,title="Y",ymax=0,plotly_plot = TRUE){
  params <- get_parameters(dat, c("Biomass", "SpBio", "years", "mu", "spp"))
  #assign parameters to environment
  for (i in 1:length(params)) {
    assign(
      unlist(names(params[i])),
      unlist(params[i]))
  }
  #read in biomass and spawning biomass
  bio1 <-
    data.frame(cbind(years,
                     (Biomass * 2.2046) / 1000,
                     (SpBio * 2.2046) / 1000)) #convert to pounds
  colnames(bio1) <- c("year","biomass","spbiomass")
  #plot options
  gtitle <- paste0("Estimated ",spp, " Biomass in ", mu)
  #no title
  if(title=="N"){gtitle <- "" }
  #y-axis maximum value for plot
  ym <- max(bio1[,2:3])
  #specified y-axis maximum value for plot
  if(ymax!=0){ym <- ymax}
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
    ggplot2::ylim(0, ym) +
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
    ggplot2::guides(colour = ggplot2::guide_legend(
      override.aes = list(shape = c(16, 1)))) +
    #set theme
    ggplot2::theme_bw() +
    format_axis(1) +
    format_legend("top")
  if(plotly_plot == "TRUE"){p <- plotly::ggplotly(p)}
  p
}
