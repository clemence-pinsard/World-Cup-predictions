# All the teams qualified

all_teams_wc2026 <- c("Canada", "United States", "Mexico", "Saudi Arabia",
                      "Australia", "Iraq", "Japan", "Jordan", "Uzbekistan",
                      "Qatar", "South Korea", "Iran", "South Africa", "Algeria",
                      "Cape Verde", "Ivory Coast", "Egypt", "Ghana", "Morocco",
                      "DR Congo", "Senegal", "Tunisia", "Panama", "Curaçao", "Haiti",
                      "Argentina", "Brazil", "Colombia", "Ecuador", "Uruguay",
                      "Paraguay", "New Zealand", "Germany", "England", "Austria",
                      "Belgium", "Bosnia and Herzegovina", "Croatia", "Scotland",
                      "France", "Spain", "Norway", "Netherlands", "Portugal", 
                      "Sweden", "Switzerland", "Czech Republic", "Turkey")

# Loading of the FIFA ranking (one of the covariates we will use)

classement <- read.csv("data/classement_fifa_211.csv")

ranking <- classement %>% 
  select(Pays, Points) %>% 
  rename(team = Pays,
         rank_points = Points) %>% 
  mutate(periods = 2026)

ranking <- ranking %>%
  mutate(team = case_when(
    team == "Cabo Verde" ~ "Cape Verde",
    team == "Aotearoa New Zealand" ~ "New Zealand",
    team == "Côte d'Ivoire" ~ "Ivory Coast",
    team == "The Gambia" ~ "Gambia",
    team == "Korea Republic" ~ "South Korea",
    team == "USA" ~ "United States",
    team == "IR Iran" ~ "Iran",
    team == "Congo DR" ~ "DR Congo",
    team == "Czechia" ~ "Czech Republic",
    team == "St Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
    team == "US Virgin Islands" ~ "United States Virgin Islands",
    team == "Hong Kong, China" ~ "Hong Kong",
    team == "Kyrgyz Republic" ~ "Kyrgyzstan",
    team == "St Kitts and Nevis" ~ "Saint Kitts and Nevis",
    team == "St Lucia" ~ "Saint Lucia",
    team == "Brunei Darussalam" ~ "Brunei",
    team == "Chinese Taipei" ~ "Taiwan",
    team == "Korea DPR" ~ "North Korea",
    team == "Türkiye" ~ "Turkey",
    TRUE ~ team # 
  ))

# Loading the dataset of all the national teams games since 1872

results <- read.csv("data/results.csv")

all_teams <- union(levels(as.factor(results$home_team)), 
                   levels(as.factor(results$away_team)))

# Pre-processing

results <- results %>% 
  mutate(date = as.Date(date),
         tournament = as.factor(tournament),
         city = as.factor(city),
         country = as.factor(country))

# Matches since 2022

results_since_2022 <- results %>% 
  filter(date >= as.Date("2022-01-01")) %>%
  droplevels()

all_teams_since_2022 <- union(levels(as.factor(results_since_2022$home_team)), 
                              levels(as.factor(results_since_2022$away_team)))

results_since_2022 <- results_since_2022 %>% 
  mutate(home_team = factor(home_team, levels = all_teams_since_2022),
         away_team = factor(away_team, levels = all_teams_since_2022))

# Data for footBayes package

results_footBayes <- results_since_2022 %>% 
  rename(home_goals = home_score,
         away_goals = away_score) %>% 
  mutate(periods = as.numeric(substr(date, 1, 4))) %>% 
  select(periods, home_team, away_team, home_goals, away_goals) %>%
  filter(!is.na(home_goals) & !is.na(away_goals) )

results_footBayes_train <- results_footBayes %>%
  filter((home_team %in% ranking$team) & (away_team %in% ranking$team) )

results_footBayes_train <- results_footBayes_train %>%
  filter(!is.na(home_goals) & !is.na(away_goals) )

# Data with the groupstage 1 games

ngames_groupstage1 <- 24

groupstage1 <- data.frame(periods = rep(2026, ngames_groupstage1),
                          home_team = c("Mexico", "South Korea", "Canada",
                                        "Qatar", "Brazil", "Haiti",
                                        "United States", "Australia", "Germany",
                                        "Ivory Coast", "Netherlands", "Sweden",
                                        "Iran", "Belgium", "Spain",
                                        "Saudi Arabia", "France", "Iraq",
                                        "Argentina", "Austria", "Portugal",
                                        "Uzbekistan", "England", "Ghana"),
                          away_team = c("South Africa", "Czech Republic", "Bosnia and Herzegovina",
                                        "Switzerland", "Morocco", "Scotland", 
                                        "Paraguay", "Turkey", "Curaçao",
                                        "Ecuador", "Japan", "Tunisia",
                                        "New Zealand", "Egypt", "Cape Verde",
                                        "Uruguay", "Senegal", "Norway",
                                        "Algeria", "Jordan", "DR Congo",
                                        "Colombia", "Croatia", "Panama"),
                          home_goals = rep(NA, ngames_groupstage1),
                          away_goals = rep(NA, ngames_groupstage1))

# Final dataset 

results_footBayes_groupstage1 <- rbind(results_footBayes_train, groupstage1)

