#' Get Biomass Data
#'
#' \code{get_biomass_data} can be used to extract biomass data from .rdat
#' model outputs. This can be used to extract data from a single model, multiple
#' models, or calculate the sum of biomass by grouping parameters.
#'
#' @param dat1 An rdat file containing a model output
#' @param dat2 An rdat file containing a model output
#' @param dat3 An rdat file containing a model output
#' @param grouping Grouping parameters. This determines which groups will be
#' used to sum biomass if cumulative = TRUE. Options include \code{year},
#' \code{mu}, \code{species}, and \code{model}. \code{data_type} is a required
#' option and can not be turned off.
#' @param cumulative TRUE of FALSE(default).  If TRUE, the sum of biomass will
#' be calculate by \code{grouping}. This can work when multiple models are supplied
#' @param pounds TRUE or FALSE.  If TRUE, converts from kg's to pounds.
#' @param wide TRUE or FALSE. If TRUE, output the data in wide format
#'
#' @return A tibble
#' @export
#'
#' @examples get_biomass_data(trout1)
#' get_biomass_data(trout1,trout2,grouping = c("data_type","mu"), cumulative = TRUE)
get_biomass_data <- function(dat1,
                             dat2 = NULL,
                             dat3 = NULL,
                             grouping = c("year"),
                             cumulative = FALSE,
                             pounds = TRUE) {
  run_get_bms_error_check(cumulative, grouping,dat2,dat3)
  #get biomass data
  bio_dat <-
    #if a single model is supplied
    if (is.null(dat2) & is.null(dat3)) {
      get_biomass_data1(dat1)
    #if multiple models are supplied
    } else{
      get_biomass_data2(dat1, dat2, dat3)
    }
  #convert to pounds
  if(pounds == TRUE){
    bio_dat <- dplyr::mutate(
      bio_dat,
      result = (result * 2.2046) / 1000)
  }
  #sum data across groups
  if (isTRUE(cumulative)) {
    data_groups <- rlang::syms(c(grouping,"data_type"))

    bio_dat <- bio_dat %>%
      dplyr::group_by(!!!data_groups) %>%
      dplyr::summarise(result = sum(result))
  }
  bio_dat
}
#' Get Biomass Data1
#'
#' A function used to get biomass data from a single rdat model
#' output file.
#'
#' @noRd
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

#' Get Biomasss Data 2
#'
#' A helper function used in \code{get_biomass_data}.
#' This functions combines biomass data for multipe models.
#'
#' @noRd
get_biomass_data2 <- function(dat1,
                              dat2,
                              dat3 = NULL) {
  bio_dat <- list(dat1, dat2, dat3)
  bio_dat <- bio_dat[lengths(bio_dat) != 0]
  bio_dat <- purrr::map2_df(
    .x = bio_dat,
    .y = 1:length(bio_dat),
    .f =
      ~ get_biomass_data1(.x) %>%
      dplyr::mutate(model = paste("model", .y))
  )
  bio_dat
}

#' Error checks for \code{get_biomass_data}
#'
#' \code{run_get_bms_error_check} runs error checks for the
#' \code{get_bms_data} function.
#'
#' @param dat1 A .rdat file containing ADMB data output
#' @param dat2 A .rdat file containing ADMB data output
#' @param dat3 A .rdat file containing ADMB data output
#' @param type The plot
#'
#' @noRd
#'
#' @return Error checks
run_get_bms_error_check <- function(cumulative, grouping,dat2,dat3) {
  #check the type argument
  if (isTRUE(cumulative) & is.null(grouping)) {
    stop(
      paste(
        "If 'cumulative' is equal to TRUE, grouping cannot be equal",
        "to NULL. Available groups include year, data_type,mu, species",
        "and/or model"
      )
    )
  }
  if ("model" %in% grouping & is.null(dat2) & is.null(dat2)) {
    stop(
      paste(
        "'model' cannot be used a grouping parameter when only one",
        "model is supplied."
      )
    )
  }
}

