parameter_names <-
tibble::tribble(
  ~param_name,~description,
  "years",1,
  "ages", 1,
  "yearsGL", 1,
  "agesGL",1,
  "mod.year",1,
  "agesR",1,
  "yearsR",1,
  "agesS1" ,1,
  "yearsS1" ,1,
  "agesS2" ,1,
  "yearsS2" ,1,
  "spp",1,
  "mu" ,1,
  "Nage" ,1,
  "Biomass" ,1,
  "SpBio" , 1,
  "GLcatN" ,1,
  "TPcatN" , 1,
  "TWcatN" , 1,
  "RCcatN" , 1,
  "obsGLcat" , 1,
  "obsTPcat" , 1,
  "obsTWcat" , 1,
  "obsRCcat" , 1)

usethis::use_data(parameter_names,overwrite = TRUE)
