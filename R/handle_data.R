#' Get model Parameters
#'
#' These functions takes an .rdat file output by ADMB and a param_name. The function
#' will return data for the given parameter.  See parameter_lookup to see
#' available options.
#'
#'
#' @param dat A list
#' @param param_name A parameter name (s)
#'
#' @return A vector
#' @export
#'
#'
#' @examples get_parameters(gb_wf_ar,"years")
get_parameters <- function(dat,param_names){
  params <- purrr::map(.x = param_names,
                       .f = ~get_parameter(param_name = .x,dat = dat))
  names(params) <- param_names
  params
}


#' Get Parameter
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

#' Get Biomass Data
#'
#' A function used to get biomass data from a dat file.
#'
#' @param dat A dataframe
#' @param wide TRUE or FALSE. if TRUE return a wide formatted
#' dataframe.
#'
#' @return
#' @export
#'
#' @examples get_biomass_data(gb_wb_ar)
get_biomass_data <- function(dat,wide = FALSE,pounds = TRUE) {
  #get biomass related parameters
  bio_dat <- get_parameters(
    gb_wf_ar,
    c("Biomass","SpBio","years","mu","spp"))
  #create a biomass dataframe
  bio_dat <- rbind(
    tibble::tibble(
      year = as.integer(names(bio_dat$Biomass)),
      result = bio_dat$Biomass,
      data_type = "biomass"
    ),
    tibble::tibble(
      year = as.integer(names(bio_dat$SpBio)),
      result = bio_dat$SpBio,
      data_type = "sp_bio"
    )
  ) %>%
    dplyr::mutate(species = bio_dat$spp,
                  mu = bio_dat$mu)
  #conver to pounds
  if(pounds == TRUE){
    bio_dat <- dplyr::mutate(
      bio_dat,
      result = (result * 2.2046) / 1000)
  }
  #convert to wider format
  if(wide == TRUE){
    bio_dat <-
      tidyr::pivot_wider(
      bio_dat,
      names_from = "data_type",
      values_from = "result")
  }
  #return data
  bio_dat
}
