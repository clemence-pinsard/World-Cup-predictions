# Bayesian approach with double poisson model 

# Data train 

footdata_raw <- read.csv("data/results.csv")
footdata_raw <- footdata_raw %>% 
  rename(ht = home_team,
         at = away_team,
         goals1 = home_score,
         goals2 = away_score) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2022-01-01")) %>% 
  mutate(ht = as.factor(ht),
         at = as.factor(at),
         goals1 = as.numeric(goals1),
         goals2 = as.numeric(goals2),
         neutral = ifelse(neutral, 0, 1)) 

# Data test

ngames_groupstage1 <- 24

hometeam = c("Mexico", "South Korea", "Canada", "Qatar", "Brazil", "Haiti",
              "United States", "Australia", "Germany", "Ivory Coast", "Netherlands", "Sweden",
              "Iran", "Belgium", "Spain", "Saudi Arabia", "France", "Iraq",
              "Argentina", "Austria", "Portugal", "Uzbekistan", "England", "Ghana")
                          
awayteam = c("South Africa", "Czech Republic", "Bosnia and Herzegovina", "Switzerland", "Morocco", "Scotland", 
              "Paraguay", "Turkey", "Curaçao", "Ecuador", "Japan", "Tunisia",
              "New Zealand", "Egypt", "Cape Verde", "Uruguay", "Senegal", "Norway",
              "Algeria", "Jordan", "DR Congo", "Colombia", "Croatia", "Panama")
                          
homegoals = rep(NA, ngames_groupstage1)
                          
awaygoals = rep(NA, ngames_groupstage1)

home = rep(0, 2*ngames_groupstage1)

data_final = data.frame()







finale <- data.frame(
  ht = factor("PSG", levels = levels(footdata_raw$ht)),
  at = factor("Arsenal", levels = levels(footdata_raw$ht)),
  goals1 = NA,
  goals2 = NA
)

footdata_raw <- rbind(footdata_raw[, c("ht","at","goals1","goals2")], finale)

h <- c(rep(1, nrow(footdata_raw)-1), 0)

footdata <- list(
  ht = as.numeric(footdata_raw$ht),
  at = as.numeric(footdata_raw$at),
  goals1 = footdata_raw$goals1,
  goals2 = footdata_raw$goals2,
  h = h,
  n = nrow(footdata_raw),
  K = 36
)

inits_multi <- list(
  list(mu = 0.5,  home = 0.5,  a = c(NA, rep(0, 35)), d = c(NA, rep(0, 35))),
  list(mu = 0,    home = 0,    a = c(NA, rep(0.5, 35)), d = c(NA, rep(0.5, 35))),
  list(mu = -0.5, home = 1,    a = c(NA, rep(-0.5, 35)), d = c(NA, rep(-0.5, 35)))
)

parameter.names <- c('mu', 'home', 'a', 'd', 'goals1', 'goals2')