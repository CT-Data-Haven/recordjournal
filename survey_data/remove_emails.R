#remove email addresses but keep the field
library(tidyverse)
path <- "survey_data/"
date <- "06132022/"
eng <- "(English) RJ Latino Communities Reporting Lab Survey (Responses) - Form Responses 1.csv"
spn <- "(Spanish) RJ Latino Communities Reporting Lab Survey (Responses) - Form Responses 1.csv"

read_eng <- read_csv(file = paste(path, date, eng, sep = "")) %>%
  mutate(`If you'd like to receive a digital coupon to CTown Supermarket for completing the survey (and be entered to win a CTown gift card) please provide your email address below.` = if_else(grepl("@", `If you'd like to receive a digital coupon to CTown Supermarket for completing the survey (and be entered to win a CTown gift card) please provide your email address below.`), "REMOVED", "NOT PROVIDED")) %>%
  write_csv(file = paste(path, date, eng, sep = ""))

read_spn <- read_csv(file = paste(path, date, spn, sep = "")) %>%
  mutate(`Si desea recibir un cupón digital de CTown Supermarket, por completar la encuesta, y también participar para ganar una tarjeta de regalo de CTown, por favor ingrese su dirección de correo electrónico a continuación.` = if_else(grepl("@", `Si desea recibir un cupón digital de CTown Supermarket, por completar la encuesta, y también participar para ganar una tarjeta de regalo de CTown, por favor ingrese su dirección de correo electrónico a continuación.`), "REMOVED", "NOT PROVIDED")) %>%
  write_csv(file = paste(path, date, spn, sep = ""))
