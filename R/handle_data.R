#' Get model Parameter
#'
#' This function takes an .rdat file output by ADMB and a param_name
#'
#'
#' @param dat
#' @param param_name
#'
#' @return
#' @export
#'
#' @examples
get_parameter <- function(dat,param_name){
  param_df <- dat$dims
  info <- dat$info
  spp <-  info$species
  param <-
    switch(param_name,
         "years" = param_df$fyear:param_df$lyear,
         "ages" = param_df$fage:param_df$lage,
         "yearsGL" = param_df$fyear:param_df$lyear,
         "agesGL" = param_df$fage:param_df$lage,
         "mod.year" = param_df$lyear + ifelse(spp == "Lake Whitefish",2,1),
         "agesR" = param_df$fageR:param_df$lageR,
         "yearsR" = param_df$fyearR:param_df$lyearR,
         "agesS1" = param_df$fageS1:param_df$lageS1,
         "yearsS1" = param_df$fyearS1:param_df$lyearS1,
         "agesS2" = param_df$fageS2:param_df$lageS2,
         "yearsS2" = param_df$fyearS2:param_df$lyearS2,
         "spp" = spp,
         "mu" = info$mu,
         "Nage" = data.frame(dat$Nage),
         "Biomass" = dat$Biomass,
         "SpBio" = dat$SpBio,
         "GLcatN" = dat$GLcatN,
         "TPcatN" = dat$TPcatN,
         "TWcatN" = dat$TWcatN,
         "RCcatN" = dat$RCcatN,
         "obsGLcat" = dat$obsGLcat,
         "obsTPcat" = dat$obsTPcat,
         "obsTWcat" = dat$obsTWcat,
         "obsRCcat" = dat$obsRCcat

    )
  param
}

#' Get model Parameters
#'
#' @param dat
#' @param param_names
#'
#' @return
#' @export
#'
#' @examples
get_parameters <- function(dat,param_names){
  params <- purrr::map(.x = param_names,
                       .f = ~get_parameter(param_name = .x,dat = dat))
  names(params) <- param_names
  params
}


