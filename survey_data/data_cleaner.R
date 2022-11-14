library(tidyverse)
library(cwi)
library(janitor)
library(camiller)
source("analysis/look_up_tibbles.R")

path <- "survey_data/11082022/"
plot_path <- "/distro/"

read_english <- read_csv(paste(path, "(English) RJ Latino Communities Reporting Lab Survey.csv", sep = "")) %>%
  clean_names()

read_spanish <- read_csv(paste(path, "(Spanish) RJ Latino Communities Reporting Lab Survey.csv", sep = "")) %>%
  clean_names()

qhead_eng <- tibble(question = paste("Q", 1:32, sep = ""),
                    label = colnames(read_english))
qhead_spn <- tibble(question = paste("Q", 1:32, sep = ""),
                    label = colnames(read_spanish))

colnames(read_english) <- qhead_eng$question
colnames(read_spanish) <- qhead_eng$question

read_english <- read_english %>% select(-Q29, -Q30, -Q31, -Q32) %>% remove_empty("rows")
read_spanish <- read_spanish %>% select(-Q29, -Q30, -Q31, -Q32) %>% remove_empty("rows")

read_english_pr <- read_csv("survey_data/08302022/(English - PR) RJ Latino Communities Reporting Lab Survey 2 2.csv") %>%
  clean_names() %>%
  mutate(`are_you_interested_in_news_at_the_following_geographical_levels_select_all_that_apply` = NA,
         `which_of_these_topics_interest_you_most_select_up_to_five` = NA,
         `do_any_of_these_special_stories_interest_you_select_all_that_apply` = NA,
         `are_there_any_specific_stories_you_would_like_to_see_more_of_in_local_news_for_example_investigative_articles_business_profiles_opinion_columns_cartoons_etc` = NA) %>%
  select(timestamp, `are_you_interested_in_news_at_the_following_geographical_levels_select_all_that_apply`, `which_of_these_topics_interest_you_most_select_up_to_five`, `do_any_of_these_special_stories_interest_you_select_all_that_apply`, `are_there_any_specific_stories_you_would_like_to_see_more_of_in_local_news_for_example_investigative_articles_business_profiles_opinion_columns_cartoons_etc`, everything())

read_spanish_pr <- read_csv("survey_data/08302022/(Spanish - PR) RJ Latino Communities Reporting Lab Survey (Responses) - Form Responses.csv") %>%
  clean_names()

qhead_eng_pr <- tibble(question = paste("Q", 1:29, sep = ""),
                       label = colnames(read_english_pr))
qhead_spn_pr <- tibble(question = paste("Q", 1:29, sep = ""),
                       label = colnames(read_spanish_pr))

colnames(read_english_pr) <- qhead_spn_pr$question
colnames(read_spanish_pr) <- qhead_spn_pr$question

read_english_pr <- read_english_pr %>% remove_empty("rows") %>% select(-Q29)
read_spanish_pr <- read_spanish_pr %>% remove_empty("rows") %>% select(-Q29)

read_english <- read_english %>%
  bind_rows(read_english_pr) %>%
  rename(id = Q1) %>%
  mutate(survey = "English")

read_spanish <- read_spanish %>%
  bind_rows(read_spanish_pr) %>%
  rename(id = Q1) %>%
  mutate(survey = "Spanish")

bind <- bind_rows(read_english, read_spanish) %>%
  left_join(age, by = "Q22") %>%
  left_join(language, by = "Q24") %>%
  left_join(latino, by = "Q20") %>%
  left_join(gender, by = "Q23") %>%
  select(-Q5, -Q23, -Q24, -Q20) %>%
  rename(topics_of_interest = Q3,
         regions_of_interest = Q2,
         special_stories = Q4) %>%
  select(survey, everything())

#small age bands
x1 <- bind %>%
  mutate(Q22 = if_else(Q22 == "70 o mayor", "70 or older", as.character(Q22)),
         Q22 = if_else(Q22 == "Menor de 18", "Under 18", Q22)) %>%
  filter(!Q22 %in% c("Prefiero no decir", "Prefer not to say")) %>%
  mutate(Q22 = as.factor(Q22),
         Q22 = fct_relevel(Q22, "Under 18", after = 0L)) %>%
  rename(small_age_band = Q22) %>%
  rename(big_age_band = age)

#towns
x2 <- x1 %>%
  mutate(Q19 = str_to_title(Q19)) %>%
  mutate(Q19 = if_else(!Q19 %in% c("Meriden", "Wallingford", "New Haven", "Waterbury", "Hartford"), "All others", Q19)) %>%
  rename(residence_town = Q19)

x3 <- x2 %>%
  mutate(Q21 = str_to_upper(Q21)) %>%
  mutate(Q21 = if_else(grepl("PUER|P.R|PR", Q21), "PUERTO RICO", Q21),
         Q21 = if_else(grepl("AMERICA|MERIDEN|US|USA|UNITED|U.S|CT|NEW YORK|EEUU|ESTADO|COUNTY|CHICAGO|CALIFORNIA|NEW HAVEN", Q21), "USA", Q21),
         Q21 = if_else(grepl("REPUB|DR", Q21), "DOMINICAN REPUBLIC", Q21),
         Q21 = if_else(grepl("MEX|GUANA", Q21), "MEXICO", Q21),
         Q21 = if_else(grepl("ECUADOR", Q21), "ECUADOR", Q21)) %>%
  mutate(Q21 = if_else(!Q21 %in% c("USA", "PUERTO RICO", "DOMINICAN REPUBLIC", "ECUADOR", "MEXICO"), "OTHER", Q21)) %>%
  mutate(Q21 = as.factor(Q21),
         Q21 = fct_relevel(Q21, "USA", "PUERTO RICO", "ECUADOR", "MEXICO", "DOMINICAN REPUBLIC", "OTHER")) %>%
  rename(place_of_birth = Q21)

x4 <- x3 %>%
  mutate(Q27 = if_else(Q27 == "Sí", "Yes", Q27),
         Q27 = if_else(Q27 == "Prefiero no decir", "Prefer not to say", Q27)) %>%
  rename(web_at_home = Q27)

x5 <- x4 %>%
  mutate(Q26 = if_else(Q26 == "Sí", "Yes", Q26),
         Q26 = if_else(Q26 == "Prefiero no decir", "Prefer not to say", Q26)) %>%
  rename(web_device = Q26)

x6 <- x5 %>%
  mutate(Q28 = if_else(Q28 == "Sí", "Yes", Q28),
         Q28 = if_else(Q28 == "Prefiero no decir", "Prefer not to say", Q28),
         Q28,
         Q28 = if_else(Q28 == "No estoy seguro/a", "Not sure", Q28)) %>%
  rename(registered_voter = Q28)

x7 <- x6 %>%
  mutate(Q25 = if_else(grepl("stu", Q25), "Student", Q25),
         Q25 = if_else(grepl("18", Q25), "Parent", Q25),
         Q25 = if_else(grepl("business|empresa", Q25), "Business owner", Q25),
         Q25 = if_else(grepl("None|Ninguno", Q25), "None of these", Q25)) %>%
  rename(person_type = Q25)

x8 <- x7 %>%
  mutate(Q11 = if_else(grepl("app|online|noticias|online news|online local|Internet", Q11), "News apps/news websites", as.character(Q11)),
         Q11 = if_else(Q11 %in% c("Medios sociales"), "Social media", Q11),
         Q11 = if_else(grepl("Amistades|Friends|Familiares|Family", Q11), "Social circles", Q11),
         Q11 = if_else(grepl("Correo|email|Email", Q11), "Email/newsletter", Q11),
         Q11 = if_else(grepl("Periódicos", Q11), "Newspapers", Q11),
         Q11 = if_else(grepl("Textos|Celular", Q11), "Text message", Q11),
         Q11 = if_else(grepl("odcast", Q11), "Podcast", Q11)) %>%
  rename(formats_preferred = Q11) %>%
  rename(open_text_media = Q10)

x9 <- x8 %>%
  mutate(Q9 = if_else(grepl("app|online|noticias|online news|online local|Internet", Q9), "News apps/news websites", as.character(Q9)),
         Q9 = if_else(Q9 %in% c("Medios sociales"), "Social media", Q9),
         Q9 = if_else(grepl("Amistades|Friends|Familiares|Family", Q9), "Social circles", Q9),
         Q9 = if_else(grepl("Correo|email|Email", Q9), "Email/newsletter", Q9),
         Q9 = if_else(grepl("Periódicos", Q9), "Newspapers", Q9),
         Q9 = if_else(grepl("Textos|Celular", Q9), "Text message", Q9),
         Q9 = if_else(grepl("odcast", Q9), "Podcast", Q9)) %>%
  rename(formats_used = Q9)

languagetib <- tibble(Q6 = c("Spanish", "Español", "English", "Inglés", "Both English and Spanish", "Ambos: inglés y español", "Other", "Otro", "I don't use this format", "No uso este formato"),
                      Q7 = c("Spanish", "Español", "English", "Inglés", "Both English and Spanish", "Ambos: inglés y español", "Other", "Otro", "I don't use this format", "No uso este formato"),
                      Q8 = c("Spanish", "Español", "English", "Inglés", "Both English and Spanish", "Ambos: inglés y español", "Other", "Otro", "I don't use this format", "No uso este formato"),
                      A = c(rep("Spanish", 2), rep("English", 2), rep("Both English and Spanish", 2), rep("Other", 2), rep("I don't use this format", 2)))

x10 <- x9 %>%
  left_join(languagetib %>% select(Q6, A), by = "Q6") %>%
  rename(tv_video_language = A) %>%
  left_join(languagetib %>% select(Q7, A), by = "Q7") %>%
  rename(radio_audio_language = A) %>%
  left_join(languagetib %>% select(Q8, A), by = "Q8") %>%
  rename(print_language = A) %>%
  select(-Q6, -Q7, -Q8)

howmuchtib <- tibble(Q12 = c("Some", "Un poco", "A lot", "Mucho", "Not at all", "Para nada"),
                     Q13 = c("Some", "Un poco", "A lot", "Mucho", "Not at all", "Para nada"),
                     Q14 = c("Some", "Un poco", "A lot", "Mucho", "Not at all", "Para nada"),
                     Q15 = c("Some", "Un poco", "A lot", "Mucho", "Not at all", "Para nada"),
                     A = c(rep("Some", 2), rep("A lot", 2), rep("Not at all", 2)))

x11 <- x10 %>%
  left_join(howmuchtib %>% select(Q12, A), by = "Q12") %>%
  rename(trust_national_news = A) %>%
  left_join(howmuchtib %>% select(Q13, A), by = "Q13") %>%
  rename(trust_local_news = A) %>%
  left_join(howmuchtib %>% select(Q14, A), by = "Q14") %>%
  rename(trust_social_media_news = A) %>%
  left_join(howmuchtib %>% select(Q15, A), by = "Q15") %>%
  rename(trust_news_from_social_circle = A) %>%
  select(-Q12, -Q13, -Q14, -Q15)

agreetib <- tibble(Q16 = c("Agree", "De acuerdo", "Neutral", "Disagreee", "En desacuerdo"),
              Q17 = c("Agree", "De acuerdo", "Neutral", "Disagreee", "En desacuerdo"),
              Q18 = c("Agree", "De acuerdo", "Neutral", "Disagreee", "En desacuerdo"),
              A = c(rep("Agree", 2), "Neutral", rep("Disagree", 2)))

x12 <- x11 %>%
  left_join(agreetib %>% select(Q16, A), by = "Q16") %>%
  rename(trust_sources_of_news_about_my_community = A) %>%
  left_join(agreetib %>% select(Q17, A), by = "Q17") %>%
  rename(local_news_covers_issues_important_to_me = A) %>%
  left_join(agreetib %>% select(Q18, A), by = "Q18") %>%
  rename(local_news_reflets_my_beliefs_values = A) %>%
  select(-Q16, -Q17, -Q18)

final <- x12

write_csv(final, paste(path, "clean.csv"))
