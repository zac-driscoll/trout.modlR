whitefish1 <- dget("data-raw/GB_WF_AR.rdat")
whitefish2 <- dget("data-raw/GB_WF_WN.rdat")
trout1 <- dget("data-raw/trout1.rdat")
trout2 <- dget("data-raw/trout2.rdat")
trout3 <- dget("data-raw/trout3.rdat")

usethis::use_data(whitefish1,whitefish2,trout1,trout2,trout3,overwrite = TRUE)
