#' Get model Parameters
#'
#' \code{get_parameters} parses .rdat data output by an ADMB model.  The function
#' accepts a vector of parameter names, parses the data, and outputs a list.
#' Available optiuons for \code{param_name} can be found in
#' \code{parameter_names}.
#'
#'
#' @param dat A list
#' @param param_name A parameter name (s)
#'
#' @return A list
#' @export
#'
#' @examples get_parameters(trout1,c("years","ages"))
get_parameters <- function(dat,param_names){
  params <- purrr::map(.x = param_names,
                       .f = ~get_parameter(param_name = .x,dat = dat))
  names(params) <- param_names
  params
}

#' Get Parameter
#'
#' \code{get_parameter} parses .rdat data output by an ADMB model.
#' \code{get_parameter} can only retrieve a single parameter at a time. This
#' function is used within \code{get_parameters} to retrieve multiple parameters.
#' Available options for \code{param_name} can be found in \code{parameter_names}.
#'
#' @export
#' @return Data specified by \code{param_nam}.  This can be a list, a data.frame,
#' an integer, or other data types.
#' @param dat An .rdat file
#' @param param_name A character object specifying a parameter name. See
#' \code{parameter_names} for details.
#'
#' @examples get_parameter(trout1,"years")
#'
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

#' Get Parameters Raw
#'
#' @examples
get_parameter_raw <-
  function(dat,param_name){
    row_index <- which(parameter_names$name == param_name)
    #if parameter is nested in a list
    if(parameter_names[row_index,4] == "list"){
      super_name <- as.character(parameter_names[row_index,3])
      list_object <- dat[[which(names(dat) == super_name)]]
      list_index <- which(names(list_object) == param_name)
      list_object[[list_index]]
    } else {
      #if parameter is not nested in a list
      dat[[which(names(dat) == param_name)]]
    }
  }
