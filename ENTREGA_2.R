library(readr)
bbdd<- read_delim("data/base-de-datos-ele7 (1).csv", 
                                    delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                    trim_ws = TRUE)


