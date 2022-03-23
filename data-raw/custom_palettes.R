library(purrr)

bw <- c("black","gray70",
        "gray60","gray50","gray40","gray30",
        "gray20","gray10")

pal_names <- c("bw")
proc_cols <- function(pals) {
  map(.x = 1:length(get(pals)),
      .f = ~ as.character(na.omit(get(pals)[1:.x])))
}

custom_palettes <- map(.x = pal_names,
                       .f = ~proc_cols(.x))

names(custom_palettes) <- pal_names

usethis::use_data(custom_palettes,overwrite = TRUE)
