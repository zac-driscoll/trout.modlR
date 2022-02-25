gb_wf_ar <- dget("data/GB_WF_AR.rdat")
gb_wn_ar <- dget("data/GB_WF_WN.rdat")

usethis::use_data(gb_wf_ar,gb_wn_ar,overwrite = TRUE)
