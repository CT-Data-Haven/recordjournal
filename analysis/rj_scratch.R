### CANNOT!! just update this bc denoms are hard coded

library(tidyverse)
library(tidycensus)
library(cwi)
library(stylehaven)
library(showtext)

showtext_auto()
showtext_opts(dpi = 150)

font_add_weights("Barlow Semi Condensed", semibold = 500)

base_col <- "#5e3da7"

palx_colors <- palx(base_col, n_shades = 5, plot = F)

qual_pal <- palx_colors$shade03
opinion_pal <- c(palx_colors$shade02[["violet"]], palx_colors$shade03[["violet"]], palx_colors$shade04[["violet"]])

tf_pal <- c("TRUE" = palx_colors[["shade02"]][["gray"]], "FALSE" = palx_colors[["shade05"]][["gray"]])

theme_bar <- function(x, xgrid = FALSE, ygrid = FALSE, ...) {
  camiller::theme_din(base_family = "Barlow Semi Condensed", xgrid = xgrid, ygrid = ygrid) +
    theme(plot.caption.position = "plot",
          strip.text = element_text(family = "Barlow Semi Condensed Semibold", face = "plain"),
          ...)
}
theme_set(theme_bar())
update_geom_defaults("col", list(fill = base_col))
update_geom_defaults("text", list(size = 3.5, fontface = "bold", family = "Barlow Semi Condensed", color = tf_pal[["FALSE"]]))


#------------ fetch
fetch_latino <- multi_geo_acs(table = "B01001I", year = 2020, counties = NULL, towns = c("Meriden", "Wallingford")) %>%
  label_acs() %>%
  mutate(race = "latino") %>% 
  select(level, name, year, estimate, moe, label)

fetch_total <- multi_geo_acs(table = "B01001", year = 2020, counties = NULL, towns = c("Meriden", "Wallingford")) %>%
  label_acs() %>%
  select(level, name, year, estimate, moe, label)

#------------ calc
age_total <- fetch_total %>% 
  separate(label, into = c("total", "sex", "age"), sep = "!!", fill = "right") %>%
  filter(!is.na(age) | ((is.na(age) & is.na(sex)))) %>% 
  replace_na(list(age = "all_ages")) %>%
  mutate(race = "total") %>% 
  select(year, level, name, race, age, estimate) %>% 
  group_by(year, level, name, race) %>% 
  add_grps(list(all_ages = 1, ages00_17 = 2:5, ages18_29 = 6:8, ages18_34 = 6:9, ages35_64 = 10:12, ages65_74 = 13, ages75_99 = 14:15), group = age)
  
latino_age <- fetch_latino %>%
  separate(label, into = c("total", "sex", "age"), sep = "!!", fill = "right") %>%
  filter(!is.na(age) | ((is.na(age) & is.na(sex)))) %>% 
  replace_na(list(age = "all_ages")) %>%
  select(year, level, name, race, age, estimate) %>% 
  group_by(year, level, name, race) %>% 
  add_grps(list(all_ages = 1, ages00_17 = 2:5, ages18_29 = 6:8, ages18_34 = 6:9, ages35_64 = 10:12, ages65_74 = 13, ages75_99 = 14:15), group = age)


  rbind(race_age_total) %>%
  spread(key = race, value = estimate) %>%
  mutate(other_race = total - black - latino - white) %>%
  gather(key = race, value = value, black:other_race) %>%
  mutate(race = as.factor(race) %>% fct_relevel("total", "white", "black", "latino", "other_race"))

  #------------ summarize for report areas
meriden_wallingford <- race_age %>% 
  filter(name %in% c("Meriden", "Wallingford", "Connecticut")) %>% 
  ungroup() %>% 
  mutate(name = if_else(name != "Connecticut", "Meriden & Wallingford", name)) %>% 
  mutate(age = if_else(age %in% c("ages00_04", "ages05_17"), "ages00_17", as.character(age))) %>% 
  mutate(age = as.factor(age),
         age = fct_relevel(age, "all_ages", after = 0L)) %>% 
  group_by(name, age, race) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  group_by(name, age) %>% 
  camiller::calc_shares(group = race, denom = "total", value = value)

meriden_wallingford %>% 
  filter(race == "latino") %>% 
  ggplot(aes(x = share, y = fct_rev(age))) +
  geom_col(fill = "cadetblue") +
  geom_text(aes(label = scales::percent(share, accuracy = 1)), hjust = 1.2) +
  facet_grid(cols = vars(name))

#---------------------- survey

#----------- read and clean
read_english <- read_csv("Desktop/RJ milestone 1/(English) RJ Latino Communities Reporting Lab Survey (Responses) - Form Responses 1.csv") %>% 
  janitor::clean_names() %>% 
  filter(!is.na(what_town_do_you_live_in))

read_spanish <- read_csv("Desktop/RJ milestone 1/(Spanish) RJ Latino Communities Reporting Lab Survey (Responses) - Form Responses 1.csv") %>% 
  janitor::clean_names()

qhead_eng <- tibble(question = paste("Q", 1:31, sep = ""),
                    label = colnames(read_english))

qhead_spn <- tibble(question = paste("Q", 1:31, sep = ""),
                    label = colnames(read_spanish))

colnames(read_english) <- qhead_eng$question
colnames(read_spanish) <- qhead_spn$question  

#----------- summaries
response_town <- tibble(town = as.factor(read_english$Q19),
                        survey = as.factor("English")) %>% 
  bind_rows(tibble(town = as.factor(read_spanish$Q19),
                   survey = as.factor("Spanish"))) %>% 
  filter(!is.na(town)) %>% 
  mutate(town = if_else(!grepl("Meriden|Wallingford", town), "Other", as.character(town)))

response_town %>% group_by(survey, town) %>% count()

###

response_latino <- tibble(latino = as.factor(read_english$Q20),
                 survey = as.factor("English")) %>% 
  bind_rows(tibble(latino = as.factor(read_spanish$Q20),
                   survey = as.factor("Spanish")))

response_latino %>%  group_by(survey, latino) %>% count()

###

response_gender <- tibble(gender = as.factor(read_english$Q23),
                          survey = as.factor("English")) %>% 
  bind_rows(tibble(gender = as.factor(read_spanish$Q23),
                   survey = as.factor("Spanish")))

response_gender %>%  group_by(survey, gender) %>% count()

###

response_birthplace <- tibble(place = as.factor(read_english$Q21),
                              survey = as.factor("English")) %>% 
  bind_rows(tibble(place = as.factor(read_spanish$Q21),
                   survey = as.factor("Spanish"))) %>% 
  mutate(clean = if_else(grepl("Puerto|puerto", place), "Puerto Rico", as.character(place))) %>% 
  mutate(clean = if_else(grepl("US|Us|us|USA|United|united|UNITED|ct", place), "United States", clean))
         
response_birthplace %>% group_by(survey, clean) %>% count()

###

response_lang_at_home <- tibble(lang = as.factor(read_english$Q24),
                              survey = as.factor("English")) %>% 
  bind_rows(tibble(lang = as.factor(read_spanish$Q24),
                   survey = as.factor("Spanish")))

response_lang_at_home %>% group_by(survey, lang) %>% count()

###

response_web_at_home <- tibble(internet = as.factor(read_english$Q27),
                           survey = as.factor("English")) %>% 
  bind_rows(tibble(internet = as.factor(read_spanish$Q27),
                   survey = as.factor("Spanish")))

response_web_at_home %>% group_by(survey, internet) %>% count()

###

response_device <- tibble(device = as.factor(read_english$Q26),
                          survey = as.factor("English")) %>% 
  bind_rows(tibble(device = as.factor(read_spanish$Q26),
                   survey = as.factor("Spanish")))

response_device %>% group_by(device, survey) %>% count()

###

response_voter <- tibble(registered = as.factor(read_english$Q28),
       survey = as.factor("English")) %>% 
  bind_rows(tibble(registered = as.factor(read_spanish$Q28),
                   survey = as.factor("Spanish")))

response_voter %>% group_by(registered, survey) %>% count()

###

response_details <- tibble(indicator = as.factor(read_english$Q25),
                           survey = as.factor("English")) %>% 
  bind_rows(tibble(indicator = as.factor(read_spanish$Q25),
                   survey = as.factor("Spanish"))) %>% 
  mutate(student = if_else(grepl("stu", indicator), T, F),
         parent = if_else(grepl("18", indicator), T, F),
         biz = if_else(grepl("business|empresa", indicator), T, F)) %>% 
  mutate(across(survey:biz, as.factor))

response_details %>% group_by(survey, biz) %>% count()

###

response_mode <- tibble(mode = as.factor(read_english$Q9),
                           survey = as.factor("English")) %>% 
  bind_rows(tibble(mode = as.factor(read_spanish$Q9),
                   survey = as.factor("Spanish"))) %>% 
  mutate(clean = if_else(grepl("app|online", mode), "News apps", as.character(mode))) %>% 
  mutate(clean = if_else(mode %in% c("Medios sociales"), "Social media", clean),
         clean = if_else(grepl("Amistades|Friends", clean), "Social circles", clean),
         clean = if_else(grepl("Correo", clean), "Email/newsletters", clean),
         clean = if_else(grepl("Periódicos", clean), "Newspapers", clean),
         clean = if_else(clean == "Textos", "Text message", clean))

response_mode %>% 
  group_by(survey, clean) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(survey = "Spanish", clean = "News apps", n = 0) %>% 
  add_row(survey = "English", clean = "Text message", n = 0) %>% 
  mutate(share = if_else(survey == "English", (n / 44), (n / 30))) %>% 
  ggplot(aes(x = str_wrap(clean, 6), y = share)) +
  geom_col(position = position_dodge(.9), width = .8) +
  geom_text(data = . %>% filter(share > .03), aes(label = scales::percent(share, accuracy = 1)), position = position_dodge(.9), vjust = 1.2, color = "white") +
  geom_text(data = . %>% filter(share > 0 & share <= .03), aes(label = scales::percent(share, accuracy = 1)), position = position_dodge(.9), vjust = -0.2, color = "grey20") +
  geom_text(data = . %>% filter(share == 0), aes(label = "N/A"), position = position_dodge(.9), vjust = -1.2, color = "grey20") +
  facet_grid(rows = vars(survey), scales = "free_x", switch = "y") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(color = "grey20", size = 10, lineheight = 1.5)) +
  labs(x = NULL, y = "Survey language", title = NULL)

ggsave(filename = "news_mode.png", path = "Desktop/", dpi = "screen", height = 5)

###

response_pref_mode <- tibble(mode = as.factor(read_english$Q11),
                        survey = as.factor("English")) %>% 
  bind_rows(tibble(mode = as.factor(read_spanish$Q11),
                   survey = as.factor("Spanish"))) %>% 
  mutate(clean = if_else(grepl("app|online|noticias|Same", mode), "News apps", as.character(mode))) %>% 
  mutate(clean = if_else(mode %in% c("Medios sociales"), "Social media", clean),
         clean = if_else(grepl("Amistades|Friends", clean), "Social circles", clean),
         clean = if_else(grepl("Correo", clean), "Email/newsletter", clean),
         clean = if_else(grepl("Periódicos", clean), "Newspapers", clean),
         clean = if_else(clean %in% c("Textos", "Celular"), "Text message", clean))

response_pref_mode %>% 
  group_by(survey, clean) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(survey = "Spanish", clean = "Radio", n = 0) %>% 
  add_row(survey = "English", clean = "Social circles", n = 0) %>% 
  mutate(share = if_else(survey == "English", (n / 44), (n / 30))) %>% 
  ggplot(aes(x = str_wrap(clean, 6), y = share)) +
  geom_col(position = position_dodge(.9), width = .8) +
  geom_text(data = . %>% filter(share > .04), aes(label = scales::percent(share, accuracy = 1)), position = position_dodge(.9), vjust = 1.2, color = "white") +
  geom_text(data = . %>% filter(share > 0 & share <= .04), aes(label = scales::percent(share, accuracy = 1)), position = position_dodge(.9), vjust = -0.2, color = "grey20") +
  geom_text(data = . %>% filter(share == 0), aes(label = "N/A"), position = position_dodge(.9), vjust = -1.2, color = "grey20") +
  facet_grid(rows = vars(survey), scales = "free_x", switch = "y") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(color = "grey20", size = 10, lineheight = 1.5)) +
  labs(x = NULL, y = "Survey language", title = NULL)

ggsave(filename = "pref_mode.png", path = "Desktop/", dpi = "screen", height = 5)

###

pref_lang <- tibble(tv_video = as.factor(read_english$Q6),
                    radio_audio = as.factor(read_english$Q7),
                    print = as.factor(read_english$Q8),
                    survey = as.factor("English")) %>% 
  bind_rows(tibble(tv_video = as.factor(read_spanish$Q6),
                   radio_audio = as.factor(read_spanish$Q7),
                   print = as.factor(read_spanish$Q8),
                   survey = as.factor("Spanish"))) %>% 
  mutate(tv_video2 = if_else(grepl("Inglés", tv_video), "English", as.character(tv_video))) %>% 
  mutate(tv_video2 = if_else(grepl("Español", tv_video), "Spanish", tv_video2),
         tv_video2 = if_else(grepl("Ambos", tv_video), "Both English and Spanish", tv_video2)) %>% 
  mutate(radio_audio2 = if_else(grepl("Inglés", radio_audio), "English", as.character(radio_audio))) %>% 
  mutate(radio_audio2 = if_else(grepl("Español", radio_audio), "Spanish", radio_audio2),
         radio_audio2 = if_else(grepl("Ambos", radio_audio), "Both English and Spanish", radio_audio2)) %>% 
  mutate(print2 = if_else(grepl("Inglés", print), "English", as.character(print))) %>% 
  mutate(print2 = if_else(grepl("Español", print), "Spanish", print2),
         print2 = if_else(grepl("Ambos", print), "Both English and Spanish", print2)) %>% 
  select(survey, Print = print2, `TV/video` = tv_video2, `Radio/audio` = radio_audio2)

pref_lang %>% 
  pivot_longer(cols = -survey, names_to = "mode", values_to = "language") %>% 
  group_by(survey, mode, language) %>% 
  count() %>% 
  ungroup() %>%
  filter(!is.na(language), language != "I don't use this format") %>% 
  pivot_wider(id_cols = c("survey", "mode"), names_from = "language", values_from = "n") %>% 
  mutate(total = `Both English and Spanish` + English + Spanish) %>% 
  pivot_longer(cols = `Both English and Spanish`:total, names_to = "language", values_to = "count") %>% 
  group_by(survey, mode) %>% 
  camiller::calc_shares(group = language, denom = "total", value = count) %>% 
  mutate(language = as.character(language)) %>% 
  filter(language != "total") %>%
  ggplot(aes(x = str_wrap(language, 7), y = share)) +
  geom_col(aes(fill = language), position = position_dodge(.9), width = .8) +
  geom_text(data = . %>% filter(share > 0.099), aes(label = scales::percent(share, accuracy = 1)), position = position_dodge(.9), vjust = 1.2) +
  geom_text(data = . %>% filter(share <= 0.099), aes(label = scales::percent(share, accuracy = 1)), position = position_dodge(.9), vjust = -.2, color = "grey20") +
  guides(fill = guide_legend(title = NULL)) +
  facet_grid(rows = vars(survey), cols = vars(mode), scales = "free_y", switch = "y") +
  scale_fill_manual(values = unname(qual_pal[c("violet", "cyan", "pink")])) +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = NULL, y = "Survey language")

ggsave(filename = "pref_lang.png", path = "Desktop/", dpi = "screen", height = 5)

###

eng_list <- str_split(read_english$Q10, ",| and | or|\\/|  ", simplify = T)
spn_list <- str_split(read_spanish$Q10, ",| and | or|\\/|  ", simplify = T)
x <- tibble(source = c(spn_list, eng_list)) %>% 
  filter(!is.na(source), source != "") %>% 
  mutate(source = str_to_upper(source)) %>%
  mutate(source = str_trim(source, side = "both")) %>% 
  mutate(source = str_remove(source,  c(" IF AVAILABLE| PRINT EDITION"))) %>% 
  filter(!source %in% c("N", "A", "FACEBOOK GOOGLE", "HARTFORD COURANT TELEMUNDO WFSB 3 TELEFE INTERNATIONAL", "RECORD JOURNAL FACEBOOK")) %>% #will add these back properly below
  mutate(source = if_else(grepl("RECORD|RJ", source), "RECORD-JOURNAL", source),
         source = if_else(grepl("NBC|CHANNEL 30", source), "NBC", source),
         source = if_else(grepl("FOX", source), "FOX NEWS", source),
         source = if_else(grepl("CHANNEL 8", source), "WTNH", source),
         source = if_else(grepl("WFSB", source), "WFSB", source),
         source = if_else(grepl("BREAK", source), "NEWSBREAK", source),
         source = if_else(grepl("LOCAL", source), "LOCAL STATIONS", source),
         source = if_else(grepl("HEARD", source), "NEW BRITAIN HERALD", source),
         source = if_else(grepl("WAPO", source), "WASHINGTON POST", source)) %>% 
  mutate(source = str_trim(source, side = "both"))

cloud <- x %>% group_by(source) %>% count()

library(ggwordcloud)

set.seed(13)

ggplot(cloud, aes(label = source, size = n+3, color = n)) +
  geom_text_wordcloud_area(family = "Barlow Semi Condensed", eccentricity = .6) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  scale_color_gradient(high = qual_pal[1], low = qual_pal[7])

ggsave(filename = "cloud_source.png", path = "Desktop/", dpi = "screen", height = 5)

###

eng_geo <- tibble(geo = c(str_split(read_english$Q2, ", ", simplify = T)),
                  survey = "English")
spn_geo <- tibble(geo = c(str_split(read_spanish$Q2, ", ", simplify = T)),
                  survey = "Spanish")

response_geos <- bind_rows(eng_geo, spn_geo) %>% 
  filter(geo != "") %>% 
  mutate(geo = if_else(geo == "Estatal", "State", geo),
         geo = if_else(geo == "Nacional", "National", geo),
         geo = if_else(grepl("Inter", geo), "International", geo),
         geo = if_else(grepl("Regional", geo), "Regional", geo))

response_geos %>%
  group_by(survey, geo) %>%
  count() %>% 
  ungroup() %>% 
  mutate(geo = as.factor(geo),
         geo = fct_relevel(geo, "Local", "State", "Regional", "National", "International")) %>% 
  mutate(share = if_else(survey == "English", n/44, n/30)) %>% 
  ggplot(aes(x = geo, y = share)) +
  geom_col(width = .8) +
  geom_text(aes(label = scales::percent(share, accuracy = 1)), vjust = 1.2) +
  facet_grid(rows = vars(survey), switch = "y") +
  theme(axis.text.y = element_blank()) +
  labs(x = NULL, y = "Survey Language")

ggsave(filename = "geo_pref.png", path = "Desktop/", dpi = "screen", height = 5)

###

eng_topic <- tibble(topic = c(str_split(read_english$Q3, ", (?=[A-Z])", simplify = T)),
                  survey = "English")

spn_topic <- tibble(topic = c(str_split(read_spanish$Q3, ", (?=[A-Z])", simplify = T)),
                  survey = "Spanish")

response_topic <- bind_rows(eng_topic, spn_topic) %>%
  filter(topic != "") %>% 
  filter(!grepl("ratings", topic)) %>% #medical facilities = health
  add_row(topic = "Health", survey = "English") %>% 
  mutate(topic = if_else(grepl("arte", topic), "Music, arts, and culture", topic),
         topic = if_else(grepl("Educ", topic), "Education", topic),
         topic = if_else(grepl("Negocios", topic), "Local businesses", topic),
         topic = if_else(grepl("Eventos", topic), "Local events", topic),
         topic = if_else(grepl("Políticas", topic), "Politics", topic),
         topic = if_else(grepl("Salud", topic), "Health", topic),
         topic = if_else(grepl("Deportes", topic), "Sports", topic),
         topic = if_else(grepl("resources|Recursos", topic), "Community resources", topic),
         topic = if_else(grepl("Diversidad", topic), "Diversity and social justice", topic),
         topic = if_else(grepl("Tecno", topic), "Technology", topic),
         topic = if_else(grepl("Finanzas", topic), "Finance, wealth, and financial literacy", topic),
         topic = if_else(grepl("Oport", topic), "Opportunities for community engagement", topic),
         topic = if_else(grepl("Viajes", topic), "Travel and hospitality", topic),
         topic = if_else(grepl("cambio", topic), "Environmental issues & climate change", topic),
         topic = if_else(grepl("vivienda", topic), "Real estate and homeownership", topic)) %>% 
  mutate(topic = str_replace(topic, "&", "and"),
         topic = str_replace(topic, "Environmental", "Env."),
         topic = str_replace(topic, "Opportunities", "Opps."),
         topic = str_replace(topic, "Community", "Comm."),
         topic = str_replace(topic, "community", "comm."))

response_topic %>%
  group_by(survey, topic) %>%
  count() %>% 
  ungroup() %>% 
  mutate(topic = as.factor(topic),
         topic = fct_rev(topic)) %>% 
  mutate(share = if_else(survey == "English", n/44, n/30)) %>% 
  ggplot(aes(x = share, y = reorder(topic, share))) +
  geom_col(width = .8) +
  geom_text(aes(label = scales::percent(share, accuracy = 1)), hjust = 1.2, size = 2.75) +
  facet_grid(cols = vars(survey)) +
  theme(axis.text.x = element_blank(),
        text = element_text(size = 9)) +
  labs(y = NULL, x = NULL)

ggsave(filename = "topic.png", path = "Desktop/", dpi = "screen", height = 6)

###

eng_special_topic <- tibble(topic = c(str_split(read_english$Q4, ", (?=[A-Z])", simplify = T)),
                    survey = "English") %>% 
  mutate(topic = str_remove(topic, "Topics related to "))
spn_special_topic <- tibble(topic = c(str_split(read_spanish$Q4, ", (?=[A-Z])", simplify = T)),
                    survey = "Spanish") %>% 
  mutate(topic = str_remove(topic, "Temas acerca de "))

response_special_topic <- bind_rows(eng_special_topic, spn_special_topic) %>%
  filter(topic != "") %>% 
  mutate(topic = str_to_sentence(topic)) %>% 
  mutate(topic = if_else(grepl("Familias", topic), "Families with children", topic),
         topic = if_else(grepl("Propietarios", topic), "Small business owners", topic),
         topic = if_else(grepl("edad", topic), "Seniors", topic),
         topic = if_else(grepl("Los", topic), "Teens", topic),
         topic = if_else(grepl("Jóvenes", topic), "Young adults", topic),
         topic = if_else(grepl("Participacion", topic), "Community engagement", topic))

response_special_topic %>% 
  group_by(survey, topic) %>%
  count() %>% 
  ungroup() %>% 
  mutate(topic = as.factor(topic),
         topic = fct_rev(topic)) %>% 
  mutate(share = if_else(survey == "English", n/44, n/30)) %>% 
  ggplot(aes(x = share, y = reorder(topic, share))) +
  geom_col(width = .8) +
  geom_text(aes(label = scales::percent(share, accuracy = 1)), hjust = 1.2, size = 2.75) +
  facet_grid(cols = vars(survey)) +
  theme(axis.text.x = element_blank(),
        text = element_text(size = 9)) +
  labs(y = "Topics relevant to...", x = NULL)

ggsave(filename = "special_topic.png", path = "Desktop/", dpi = "screen")

###

trust <- tibble(national_news = as.factor(read_english$Q12),
                local_news = as.factor(read_english$Q13),
                social_media = as.factor(read_english$Q14),
                social_circle = as.factor(read_english$Q15),
                survey = as.factor("English")) %>% 
  bind_rows(tibble(national_news = as.factor(read_spanish$Q12),
                   local_news = as.factor(read_spanish$Q13),
                   social_media = as.factor(read_spanish$Q14),
                   social_circle = as.factor(read_spanish$Q15),
                   survey = as.factor("Spanish"))) %>% 
  pivot_longer(cols = national_news:social_circle, names_to = "level", values_to = "response") %>% 
  mutate(response = fct_collapse(response, `A lot` = c("A lot", "Mucho"),
                                 Some = c("Some", "Un poco"),
                                 `Not at all` = c("Not at all", "Para nada"))) %>% 
  group_by(survey, level, response) %>% 
  count() %>% 
  ungroup() %>%
  pivot_wider(id_cols = c("survey", "level"), names_from = "response", values_from = "n") %>% 
  select(-`NA`) %>% 
  mutate(`Not at all` = replace_na(`Not at all`, 0)) %>% 
  rowwise() %>% 
  mutate(total = sum(`A lot` + Some + `Not at all`, na.rm = T)) %>% 
  pivot_longer(cols = `A lot`:total, names_to = "response", values_to = "value") %>% 
  mutate(level = str_replace(level, "_", " "),
         level = str_to_sentence(level)) %>% 
  group_by(survey, level) %>% 
  camiller::calc_shares(group = response, denom = "total", value = value)

trust %>% 
  filter(!is.na(share)) %>% 
  mutate(response = fct_relevel(response, "A lot", "Some", "Not at all"),
         response = fct_rev(response),
         level = as.factor(level),
         level = fct_relevel(level, "National news", "Local news", "Social media", "Social circle"),
         level = fct_rev(level)) %>% 
  ggplot(aes(x = share, y = level, fill = response)) +
  geom_col(position = position_stack(.5), width = .8) +
  geom_text(data = . %>% filter(share >= .01), aes(label = scales::percent(share, accuracy = 1)), position = position_stack(.5), size = 2.7) +
  facet_grid(cols = vars(survey)) +
  scale_fill_manual(values = rev(opinion_pal)) +
  guides(fill = guide_legend(title = "Trust in source:", reverse = T)) +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 9)) +
  labs(x = NULL, y = NULL)

ggsave(filename = "trust_sources.png", path = "Desktop/", dpi = "screen")

###

labels <- tibble(code = c("Q1", "Q2", "Q3"), 
            question = c("I trust the sources of news I get about my community", "The local news covers issues that are important to me", "The local news reflects my beliefs and customs"))
                         
beliefs <- tibble(Q1 = read_english$Q16,
                Q2 = read_english$Q17,
                Q3 = read_english$Q18,
                survey = as.factor("English")) %>% 
  bind_rows(tibble(Q1 = read_spanish$Q16,
                   Q2 = read_spanish$Q17,
                   Q3 = read_spanish$Q18,
                   survey = as.factor("Spanish"))) %>% 
  pivot_longer(cols = Q1:Q3, names_to = "code", values_to = "response") %>% 
  mutate(response = if_else(response == "De acuerdo", "Agree", response),
         response = if_else(response == "En desacuerdo", "Disagree", response)) %>% 
  group_by(survey, code, response) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(labels, by = "code") %>% 
  select(-code) %>% 
  pivot_wider(id_cols = c("survey", "question"), names_from = "response", values_from = "n") %>% 
  select(-`NA`) %>% 
  rowwise() %>% 
  mutate(Disagree = replace_na(Disagree, 0)) %>% 
  mutate(total = sum(Agree + Disagree + Neutral, na.rm = T)) %>% 
  pivot_longer(cols = Agree:total, names_to = "response", values_to = "value") %>% 
  group_by(survey, question) %>% 
  camiller::calc_shares(group = response, denom = "total", value = value) %>% 
  ungroup() %>% 
  filter(!is.na(share))

beliefs %>% 
  mutate(response = as.factor(response),
         response = fct_relevel(response, "Agree", "Neutral", "Disagree"),
         response = fct_rev(response)) %>% 
  ggplot(aes(x = share, y = str_wrap(question, 25), fill = response)) +
  geom_col(position = position_stack(), width = .8) +
  geom_text(data = . %>% filter(share >= .01), aes(label = scales::percent(share, accuracy = 1)), position = position_stack(.5), size = 2.7) +
  facet_grid(cols = vars(survey)) +
  scale_fill_manual(values = rev(opinion_pal)) +
  guides(fill = guide_legend(title = NULL, reverse = T)) +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 9, lineheight = 1.5)) +
  labs(x = NULL, y = NULL)

ggsave(filename = "beliefs.png", path = "Desktop/", dpi = "screen", height = 6)
