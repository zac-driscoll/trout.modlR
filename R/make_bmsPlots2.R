make_bmsPlots <- function(df1,df2,df3 = NULL){
  params <-


}



params1 <- get_parameters(df1, c("Biomass", "SpBio", "years", "mu", "spp"))
params2 <- get_parameters(df2, c("Biomass", "SpBio", "years", "mu", "spp"))
if(!is.null(df3)){
  params3 <- get_parameters(df2, c("Biomass", "SpBio", "years", "mu", "spp"))

}

params1 <- get_parameters(gb_wn_ar, c("Biomass", "SpBio", "years", "mu", "spp"))
params1 <- get_parameters(gb_wn_ar, c("Biomass", "SpBio", "years", "mu", "spp"))
gb_wn_ar


purrr::

params1 <-
  purrr::map2_df(
    .x = list(gb_wf_ar,
           gb_wn_ar),
    .y = 1:2,
      .f = ~ get_parameters(
        .x,
        c("Biomass", "SpBio", "years", "mu", "spp") %>%
          mutate(model = paste("model", .y))
      )
  )


