#remove email addresses but keep the field
library(tidyverse)
path <- "survey_data/"
date <- "01032023/"
eng <- "(English) RJ Latino Communities Reporting Lab Survey.csv"
spn <- "(Spanish) RJ Latino Communities Reporting Lab Survey.csv"

read_eng <- read_csv(file = paste(path, date, eng, sep = ""))

write_eng <- read_eng %>%
  mutate(`If you'd like to receive a digital coupon to CTown Supermarket for completing the survey, please provide your email address below. The coupon is valid for $5 off of a purchase of $50 or more.` = if_else(grepl("@", `If you'd like to receive a digital coupon to CTown Supermarket for completing the survey, please provide your email address below. The coupon is valid for $5 off of a purchase of $50 or more.`), "REMOVED", "NOT PROVIDED"))

write_csv(write_eng, file = paste(path, date, eng, sep = ""))

read_spn <- read_csv(file = paste(path, date, spn, sep = ""))

write_spn <- read_spn %>%
  mutate(`Si desea recibir un cupón digital de CTown Supermarket, por completar la encuesta, por favor ingrese su dirección de correo electrónico a continuación. Este cupón es válido para $5 de descuento en compras de $50 o más.`  = if_else(grepl("@", `Si desea recibir un cupón digital de CTown Supermarket, por completar la encuesta, por favor ingrese su dirección de correo electrónico a continuación. Este cupón es válido para $5 de descuento en compras de $50 o más.`), "REMOVED", "NOT PROVIDED"))

write_csv(write_spn, file = paste(path, date, spn, sep = ""))
