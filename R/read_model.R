#' Read Model
#'
#' read_model imports an rdat model file that was exported from ADMB.
#'
#' @param rdat The filepathway to the rdat file
#'
#' @return A list
#' @export
#'
#' @examples \dontrun{"trout.rdat"}
read_model <- function(rdat){

  #load model
  dat <- dget(rdat)

  #add model name to list
  pattern <- "/\\s*(.*?)\\s*.rdat"
  model_name <- regmatches(rdat, regexec(pattern, rdat))
  model_name <- model_name[[1]][2]
  names(model_name) <- "model_name"
  dat <- append(dat,model_name)
  dat

}
