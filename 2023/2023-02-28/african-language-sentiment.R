
# setup -------------------------------------------------------------------
# load libraries
pacman::p_load(tidytuesdayR, tidyverse, janitor, 
               maps, maptools)

# import data
tuesdata <- tidytuesdayR::tt_load('2023-02-28')
sentiment <- tuesdata$afrisenti
languages <- tuesdata$languages
language_scripts <- tuesdata$language_scripts 
language_countries <- tuesdata$language_countries
country_regions <- tuesdata$country_regions

data(wrld_simpl)

afr = wrld_simpl[wrld_simpl$REGION==2,]

# analysis ----------------------------------------------------------------
df <- 
  sentiment |> 
  left_join(language_countries, multiple = "all") |> 
  left_join(languages) |> 
  glimpse()

df <- 
  df |> 
  group_by(country, language, label) |> 
  count() |> 
  group_by(country, language) |> 
  mutate(fraction = n / sum(n))

df <- 
  df |>
  mutate(ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)))

df <- 
  df |> 
  mutate(label_position = (ymax + ymin) / 2)

df <- ungroup(df)

df <- df |> rename("sentiment" = "label")


# visualization -----------------------------------------------------------

df_lang <- df |> distinct(language, sentiment, fraction)

df_lang <- 
  df_lang |> 
  group_by(language) |> 
  mutate(positive = case_when(sentiment == "positive"~fraction)) |> 
  fill(positive, .direction = "up") |> 
  mutate(lab_position = case_when(
    sentiment == "negative" ~ 1 - (fraction/2),
    sentiment == "neutral" ~ positive + (fraction/2),
    sentiment == "positive" ~ fraction/2)
  )

df_lang |> 
  mutate(fraction = round(fraction, 2)) |> 
  ggplot(
    aes(x = reorder(language,  positive), 
        y = fraction, fill = sentiment)
  ) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(
    aes(y = lab_position, 
        label = scales::percent_format(accuracy = 1L)(fraction))
  ) + 
  scale_fill_manual(values = c("#CE0058", "lightgray", "cadetblue")) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
  ) +
  labs(title = "Sentiment Distribution by Language", y = NULL) +
  coord_flip() 

