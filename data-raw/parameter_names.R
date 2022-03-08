parameter_names <- read.csv("data-raw/model_variable_names.csv")
parameter_names <- tibble::tibble(parameter_names)

usethis::use_data(parameter_names,overwrite = TRUE)

#
# names(trout1)
# param <- which(names(trout1) == "info")
# trout1[[1]]
#
