url <- "http://www.aussportsbetting.com/historical_data/nrl.xlsx"
destfile <- "nrl_updated.xlsx"


download.file(url, destfile, quiet = FALSE, mode = "wb")

