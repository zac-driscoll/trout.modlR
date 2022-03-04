make_bmsPlots2 <- function(dat1, dat2, dat3 = NULL,gtitle = "") {
  #get data
  bio_dat <- list(dat1, dat2, dat3)
  bio_dat <- bio_dat[lengths(bio_dat) != 0]
  bio_dat <- purrr::map2_df(
    .x = bio_dat,
    .y = 1:length(bio_dat),
    .f =
      ~ get_biomass_data(.x) %>%
      dplyr::mutate(model = paste("model", .y))
  )
  #plot data
  ggplot2::ggplot(data = bio_dat,
                  ggplot2::aes(
                    x = year,
                    y = result,
                    color = model,
                    linetype = model)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~data_type,nrow = 2) +
    ggplot2::guides(colour =
                      ggplot2::guide_legend(
                        override.aes = list(shape = c(16, 1)))) +
    # ggplot2::scale_colour_manual(
    #   "",
    #   values = c("biomass" = "black", "spbiomass" = "black"),
    #   labels = c("Total", "Female Spawning Stock")
    # ) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(breaks = seq(min(bio_dat$year), max(bio_dat$year), 5)) +
    format_axis(1) +
    format_legend("top") +
    # ggplot2::ylim(0, ymax) +
    ggplot2::labs(x = "Year",
                  y = "Biomass (x 1,000 lb)",
                  title = gtitle)

}
