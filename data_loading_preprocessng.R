# Loading of the FIFA ranking (one of the covariates we will use)

classement <- read.csv("data/classement_fifa_211.csv")

ranking <- classement %>% 
  select(Pays, Points) %>% 
  rename(team = Pays,
         rank_points = Points) %>% 
  mutate(periods = 1)

# Loading the dataset of all the national teams games since 1872

results <- read.csv("data/results.csv")

all_teams <- union(levels(as.factor(results$home_team)), 
                   levels(as.factor(results$away_team)))

# Pre-processing

results <- results %>% 
  mutate(date = as.Date(date),
         home_team = factor(home_team, levels = all_teams),
         away_team = factor(away_team, levels = all_teams),
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


