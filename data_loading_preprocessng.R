classement <- read.csv("data/classement_fifa_211.csv")

ranking <- classement %>% 
  select(Pays, Points) %>% 
  rename(team = Pays,
         rank_points = Points) %>% 
  mutate(periods = 1)

results <- read.csv("data/results.csv")
