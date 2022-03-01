make_bmsPlots2 <- function(dat1, dat2, dat3 = NULL) {
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
    ggplot2::facet_wrap(~data_type,nrow = 2)
}

zz <- make_bmsPlots2(gb_wf_ar,gb_wn_ar,gb_wn_ar)
=
