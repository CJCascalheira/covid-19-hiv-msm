# Load dependencies
library(rio)
library(tidyverse)

# Import data
covid <- import("data/COVID qual 20 participants.sav") %>%
  as_tibble()

# Create a blank theme for visualizations
blank_theme <- theme_minimal()+
  theme(
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", color = NA), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size = 14, face = "bold")
  )

# Recode variable values
covid_1 <- covid %>%
  mutate(
    SexOrientation = recode(SexOrientation, `1` = "Gay", `2` = "Bisexual"),
    M24_employ = recode(M24_employ, `1` = "Full-Time", `2` = "Part-Time", 
                        `4` = "Disabled", `6` = "Student", `7` = "Unemployed"),
    Education = recode(Education, `5` = "High School", `6` = "Some College", `7` = "Some College",
                       `8` = "Bachelor's", `10` = "Master's"),
    race5cat = recode(race5cat, `1` = "White", `2` = "Black", `3` = "Latino", `4` = "Asian", `5` = "Multiracial")
  )
covid_1

# DATA FOR MANUSCRIPT -----------------------------------------------------

# Age 
covid_1 %>%
  summarize(
    M = mean(M24_Age),
    SD = sd(M24_Age),
    min = min(M24_Age),
    max = max(M24_Age)
  )

# Sexual orientation
covid_1 %>%
  count(SexOrientation) %>%
  mutate(percnt = n / nrow(covid_1)) %>%
  arrange(desc(n))

# Race
covid_1 %>%
  count(race5cat) %>%
  mutate(percnt = n / nrow(covid_1)) %>%
  arrange(desc(n))

# Education
covid_1 %>%
  count(Education) %>%
  mutate(percnt = n / nrow(covid_1)) %>%
  arrange(desc(n))

# Employment
covid_1 %>%
  count(M24_employ) %>%
  mutate(percnt = n / nrow(covid_1)) %>%
  arrange(desc(n))

# Income
covid_1 %>%
  count(M24_income) %>%
  mutate(percnt = n / nrow(covid_1)) %>%
  arrange(desc(n))

# Incarcerated
covid_1 %>%
  count(M24_incarceratedyear) %>%
  mutate(percnt = n / nrow(covid_1)) %>%
  arrange(desc(n))

# DATA FOR POSTER ---------------------------------------------------------

# Participants quotes in poster
quoted_participants <- c(827174, 822682, 805438, 822760, 829908, 818516)

# Pull info for quotes
covid_1 %>%
  filter(ParticipantID %in% quoted_participants) %>%
  select(ParticipantID, M24_Age, race5cat)

# Age descriptives
covid_1 %>%
  summarize(
    M = mean(M24_Age),
    SD = sd(M24_Age),
    min = min(M24_Age),
    max = max(M24_Age)
  )

# Sexual orientation
plot_sexor <- ggplot(covid_1, aes(x = "", y = SexOrientation, fill = SexOrientation)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.text.x = element_blank(), 
        legend.text = element_text(size = 30),
        legend.title = element_blank(),
        legend.position = "right")
plot_sexor

# Race
plot_race <- ggplot(covid_1, aes(x = "", y = race5cat, fill = race5cat)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.text.x = element_blank(), 
        legend.text = element_text(size = 30),
        legend.title = element_blank(),
        legend.position = "right")
plot_race

# Employment
plot_employ <- ggplot(covid_1, aes(x = "", y = M24_employ, fill = M24_employ)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.text.x = element_blank(), 
        legend.text = element_text(size = 30),
        legend.title = element_blank(),
        legend.position = "right")
plot_employ

# Education
plot_edu <- ggplot(covid_1, aes(x = "", y = Education, fill = Education)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.text.x = element_blank(), 
        legend.text = element_text(size = 30),
        legend.title = element_blank(),
        legend.position = "right")
plot_edu

# Save plots
ggsave("results/sex_orientation_plot.png", plot = plot_sexor)
ggsave("results/race_plot.png", plot = plot_race)
ggsave("results/employment_plot.png", plot = plot_employ)
ggsave("results/education_plot.png", plot = plot_edu)
