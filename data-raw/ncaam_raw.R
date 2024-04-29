library(appm3310.final)
ncaam <- read_ncaa_data("NCAAM") |> clean_ncaa_data()

usethis::use_data(ncaam, overwrite = TRUE)
