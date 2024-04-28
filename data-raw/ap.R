library(appm3310.final)
ap <- read_ap_rankings() |> clean_ap_rankings()

usethis::use_data(ap, overwrite = TRUE)
