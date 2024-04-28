extract_first_list <- function(x) x[[1]]
extract_first <- function(x) x[1]
extract_second <- function(x) x[2]
extract_scores <- function(x) stringr::str_extract_all(x, "\\d+")[[1]]
