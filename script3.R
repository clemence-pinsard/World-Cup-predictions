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

# Data for footBayes

results_footBayes <- results_since_2022 %>% 
  rename(home_goals = home_score,
         away_goals = away_score) %>% 
  mutate(periods = as.numeric(substr(date, 1, 4))) %>% 
  select(periods, home_team, away_team, home_goals, away_goals)

results_footBayes_train <- results_footBayes %>%
  filter((home_team %in% ranking$team) & (away_team %in% ranking$team) )

wc2026_train_teams <- unique(results_footBayes_train$home_team)
wc2026_ranking <- ranking %>% filter(team %in% wc2026_train_teams)

# The model 

fit_double_pois <- stan_foot(data = results_footBayes_train,
                             model="double_pois",
                             predict= 72,
                             ranking = wc2026_ranking,
                             dynamic_type = "seasonal",
                             home_effect = FALSE,
                             save_cmdstan_config = TRUE,
                             init = 0)

prob_dp <- foot_prob(fit_double_pois, results_footBayes_train)
colnames(prob_dp$prob_table) <- c("home", "away",
                                  "home win", "draw", "away win", "mlo")
knitr::kable(prob_dp$prob_table)
#save prob$prob_table as csv
write.csv(prob_dp$prob_table, "groupstage_1_dp2.csv", row.names = FALSE)
prob_dp$prob_plot
