library(tidyverse)

age <- tibble(Q22 = c("Under 18", "Menor de 18", "18-29", "30-39", "40-49", "50-59", "60-69", "70 or older", "70 o mayor", "Prefer not to say", "Prefiero no decir"),
              age = c(rep("age00_29", 3), rep("age30_49", 2), rep("age50_99", 4), rep("Opt out", 2))) %>%
    mutate(age = na_if(age, "Opt out"))


gender <- tibble(Q23 = c("Woman", "Mujer", "Man", "Hombre", "Non-binary/Gender non-conforming", "Transgender", "Transgénero", "Prefer not to say", "Prefiero no decir"),
                 gender = c(rep("Woman", 2), rep("Man", 2), rep("Transgender & NB", 3), rep("Opt out", 2))) %>%
  mutate(gender = na_if(gender, "Opt out"))


latino <- tibble(Q20 = c("Yes", "Sí", "No", "Prefer not to say", "Prefiero no decir"),
                 latino = c(rep("Yes", 2), "No", rep("Opt out", 2))) %>%
  mutate(latino = na_if(latino, "Opt out"))


language <- tibble(Q24 = c("English", "Inglés", "Spanish", "Español", "Both English and Spanish", "Ambos: inglés y español"),
                   language = c(rep("English", 2), rep("Spanish", 2), rep("Both English and Spanish", 2)))
