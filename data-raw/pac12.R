pac12 <- read_ncaa_data("PAC12") |> clean_ncaa_data()

usethis::use_data(pac12, overwrite = TRUE)
