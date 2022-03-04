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
#' \code{get_biomass_data} can be used to extract biomass data from .rdat
#' model outputs. This can be used to extract data from a single model, multiple
#' models, or calculate the sum of biomass between models.
#'
#' @param dat1 An rdat file containing a model output
#' @param dat2 An rdat file containing a model output
#' @param dat3 An rdat file containing a model output
#' @param grouping Grouping parameters. This determines which groups will be
#' used to sum biomass if cumulative = TRUE. Options include "year","data_type",
#' "mu",and "species".
#' @param cumulative TRUE of FALSE(default).  If TRUE, the sum of biomass between
#' models will be output.
#' @param pounds TRUE or FALSE.  If TRUE, converts from kg's to pounds.
#' @param wide TRUE or FALSE. If TRUE, output the data in wide format
#'
#' @return A tibble
#' @export
#'
#' @examples get_biomass_data(
get_biomass_data <- function(dat1,
                             dat2 = NULL,
                             dat3 = NULL,
                             grouping = c("year","data_type"),
                             cumulative = FALSE,
                             pounds = TRUE) {
  #if a single model is supplied
  bio_dat <-
    if (is.null(dat2) & is.null(dat3)) {
      get_biomass_data1(dat1)
    } else{
      get_biomass_data2(dat1, dat2, dat3,grouping,cumulative)
    }

  #convert to pounds
  if(pounds == TRUE){
    bio_dat <- dplyr::mutate(
      bio_dat,
      result = (result * 2.2046) / 1000)
  }
  bio_dat
}
#' Get Biomass Data1
#'
#' A function used to get biomass data from a single rdat model output file.
#'
#' @param dat A dataframe
#'
#' @noRd
#'
#' @examples get_biomass_data1(gb_wb_ar)
get_biomass_data1 <- function(dat) {
  #get biomass related parameters
  bio_dat <- get_parameters(
    dat,
    c("Biomass","SpBio","years","mu","spp"))
  #create a biomass dataframe
  bio_dat <- rbind(
    tibble::tibble(
      year = as.integer(names(bio_dat$Biomass)),
      result = bio_dat$Biomass,
      data_type = "Total"
    ),
    tibble::tibble(
      year = as.integer(names(bio_dat$SpBio)),
      result = bio_dat$SpBio,
      data_type = "Female Spawning Stock"
    )
  ) %>%
    dplyr::mutate(species = bio_dat$spp,
                  mu = bio_dat$mu,
                  data_type = forcats::fct_rev(data_type))
  #return data
  bio_dat
}

#' Title
#'
#' @param dat1
#' @param dat2
#' @param dat3
#' @param grouping
#' @param cumulative
#'
#' @return
#' @export
#'
#' @examples
get_biomass_data2 <- function(dat1,
                              dat2,
                              dat3 = NULL,
                              grouping = NULL,
                              cumulative = FALSE) {
  bio_dat <- list(dat1, dat2, dat3)
  bio_dat <- bio_dat[lengths(bio_dat) != 0]
  bio_dat <- purrr::map2_df(
    .x = bio_dat,
    .y = 1:length(bio_dat),
    .f =
      ~ get_biomass_data1(.x) %>%
      dplyr::mutate(model = paste("model", .y))
  )

  if (isTRUE(cumulative)) {
    data_groups <- rlang::syms(grouping)
    bio_dat <- bio_dat %>%
      dplyr::group_by(!!!data_groups) %>%
      dplyr::summarise(result = sum(result))
  }
  bio_dat
}
