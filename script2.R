# Bayesian approach with double poisson model 

library(R2OpenBUGS)
library(dplyr)
library(reshape2)
library(ggplot2)
library(coda)

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

ngames_groupstage1 <- 72
home <- footdata_raw$neutral

# Final data

footdata <- list(
  ht = as.numeric(footdata_raw$ht),
  at = as.numeric(footdata_raw$at),
  goals1 = footdata_raw$goals1,
  goals2 = footdata_raw$goals2,
  h = home,
  n = nrow(footdata_raw),
  K = 255
)

# Initialization of the parameters

inits_multi <- list(
  list(mu = 0.5,  home = 0.5,  a = c(NA, rep(0, 254)), d = c(NA, rep(0, 254))),
  list(mu = 0,    home = 0,    a = c(NA, rep(0.5, 254)), d = c(NA, rep(0.5, 254))),
  list(mu = -0.5, home = 1,    a = c(NA, rep(-0.5, 254)), d = c(NA, rep(-0.5, 254)))
)

parameter.names <- c('mu', 'home', 'a', 'd', 'goals1', 'goals2')

# The model 

model1 <- bugs(footdata, inits_multi, model.file = "DP1.txt",
               parameters = parameter.names,
               n.chains = 3, n.iter = 10000, n.burnin = 1000, n.thin = 1,
               debug = FALSE)

# Predictions

n_train <- nrow(footdata_raw) - ngames_groupstage1
index_pred <- (n_train + 1):nrow(footdata_raw)

# Noms des colonnes goals1[i] et goals2[i] pour chaque match prédit
cols_g1 <- paste0("goals1[", index_pred, "]")
cols_g2 <- paste0("goals2[", index_pred, "]")

G_pred <- data.frame(
  match    = 1:ngames_groupstage1,
  home_team = as.character(footdata_raw$ht[index_pred]),
  away_team = as.character(footdata_raw$at[index_pred]),
  mean_g1  = apply(model1$sims.matrix[, cols_g1], 2, mean),
  mean_g2  = apply(model1$sims.matrix[, cols_g2], 2, mean),
  sd_g1    = apply(model1$sims.matrix[, cols_g1], 2, sd),
  sd_g2    = apply(model1$sims.matrix[, cols_g2], 2, sd)
)

# Probabilities

probs <- t(sapply(1:ngames_groupstage1, function(i) {
  d <- model1$sims.matrix[, cols_g1[i]] - model1$sims.matrix[, cols_g2[i]]
  c(
    prob_home = mean(d > 0),
    prob_draw = mean(d == 0),
    prob_away = mean(d < 0)
  )
}))

results_df <- cbind(G_pred, probs)
print(round(results_df[, c("home_team","away_team","mean_g1","mean_g2",
                           "prob_home","prob_draw","prob_away")], 3))

# IC

ic_list <- lapply(1:ngames_groupstage1, function(i) {
  q1 <- quantile(model1$sims.matrix[, cols_g1[i]], probs = c(0.025, 0.5, 0.975))
  q2 <- quantile(model1$sims.matrix[, cols_g2[i]], probs = c(0.025, 0.5, 0.975))
  data.frame(
    home_team = as.character(footdata_raw$ht[index_pred[i]]),
    away_team = as.character(footdata_raw$at[index_pred[i]]),
    g1_q025 = q1[1], g1_med = q1[2], g1_q975 = q1[3],
    g2_q025 = q2[1], g2_med = q2[2], g2_q975 = q2[3]
  )
})
ic_df <- do.call(rbind, ic_list)
print(round(ic_df, 3))

# Heatmaps

for (i in 1:ngames_groupstage1) {
  
  ht_name <- as.character(footdata_raw$ht[index_pred[i]])
  at_name <- as.character(footdata_raw$at[index_pred[i]])
  
  g1_sims <- model1$sims.matrix[, cols_g1[i]]
  g2_sims <- model1$sims.matrix[, cols_g2[i]]
  
  tab_scores <- prop.table(table(g1_sims, g2_sims))
  tab_df <- melt(tab_scores)
  colnames(tab_df) <- c("HomeGoals", "AwayGoals", "Probabilite")
  tab_df <- tab_df %>%
    mutate(HomeGoals = as.numeric(as.character(HomeGoals)),
           AwayGoals = as.numeric(as.character(AwayGoals))) %>%
    filter(HomeGoals <= 5, AwayGoals <= 5)
  
  p <- ggplot(tab_df, aes(x = AwayGoals, y = HomeGoals, fill = Probabilite)) +
    geom_tile() +
    geom_text(aes(label = round(Probabilite, 3)), color = "white", size = 3) +
    scale_fill_gradient(low = "white", high = "darkblue") +
    scale_x_continuous(limits = c(-0.5, 5.5), breaks = 0:5) +
    scale_y_continuous(limits = c(-0.5, 5.5), breaks = 0:5) +
    labs(title = paste0("Score probabilities: ", ht_name, " vs ", at_name),
         x = paste0("Goals for ", at_name),
         y = paste0("Goals for ", ht_name),
         fill = "Probability") +
    theme_minimal()
  
  ggsave(paste0("outputs/heatmap_", ht_name, "_vs_", at_name, ".png"),
         plot = p, width = 8, height = 6, dpi = 300)
}

# Attack/Defense

teams   <- levels(footdata_raw$ht)
nteams  <- length(teams)

index_att <- paste0("a[", 1:nteams, "]")
index_def <- paste0("d[", 1:nteams, "]")

att_ci <- t(apply(model1$sims.matrix[, index_att], 2, quantile, probs = c(0.025, 0.5, 0.975)))
def_ci <- t(apply(model1$sims.matrix[, index_def], 2, quantile, probs = c(0.025, 0.5, 0.975)))

rownames(att_ci) <- teams
rownames(def_ci) <- teams
colnames(att_ci) <- c("Q2.5", "Median", "Q97.5")
colnames(def_ci) <- c("Q2.5", "Median", "Q97.5")

# Attaque
att_df <- as.data.frame(att_ci)
att_df$team <- rownames(att_df)
att_df <- att_df[order(att_df$Median), ]
att_df$team <- factor(att_df$team, levels = att_df$team)

ggplot(att_df, aes(x = Median, y = team, xmin = Q2.5, xmax = Q97.5)) +
  geom_point(color = "darkblue", size = 2) +
  geom_errorbarh(height = 0.3, color = "darkblue") +
  geom_vline(xintercept = 0, color = "red", lwd = 1.1) +
  labs(title = "Attack abilities - All teams",
       x = "Attack parameter", y = "Team") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))

# Défense
def_df <- as.data.frame(def_ci)
def_df$team <- rownames(def_df)
def_df <- def_df[order(def_df$Median), ]
def_df$team <- factor(def_df$team, levels = def_df$team)

ggplot(def_df, aes(x = Median, y = team, xmin = Q2.5, xmax = Q97.5)) +
  geom_point(color = "darkred", size = 2) +
  geom_errorbarh(height = 0.3, color = "darkred") +
  geom_vline(xintercept = 0, color = "red", lwd = 1.1) +
  labs(title = "Defense abilities - All teams",
       x = "Defense parameter", y = "Team") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))

att_sub <- att_df[, c("team", "Median")]; colnames(att_sub)[2] <- "Attaque"
def_sub <- def_df[, c("team", "Median")]; colnames(def_sub)[2] <- "Defense"
df_quadrant <- merge(att_sub, def_sub, by = "team")

# Mettre en évidence les équipes qui jouent les 24 matchs
teams_pred <- unique(c(as.character(footdata_raw$ht[index_pred]),
                       as.character(footdata_raw$at[index_pred])))
df_quadrant$highlight <- ifelse(df_quadrant$team %in% teams_pred, "yes", "no")

ggplot(df_quadrant, aes(x = Defense, y = Attaque, label = team, color = highlight)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.8, fontface = "bold", size = 3) +
  scale_color_manual(values = c("yes" = "orange", "no" = "cornflowerblue"), guide = "none") +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  labs(title = "Attack and defense abilities - All teams",
       x = "Defense abilities", y = "Attack abilities") +
  theme_minimal()

# Convergence

n_kept     <- (10000 - 1000) / 1
n_per_chain <- n_kept

chain1 <- as.mcmc(model1$sims.matrix[1:n_per_chain, ])
chain2 <- as.mcmc(model1$sims.matrix[(n_per_chain+1):(2*n_per_chain), ])
chain3 <- as.mcmc(model1$sims.matrix[(2*n_per_chain+1):(3*n_per_chain), ])
mcmc_chains <- mcmc.list(chain1, chain2, chain3)

gelman.diag(mcmc_chains)
gelman.plot(mcmc_chains)

# Moyennes ergodiques pour mu et home
par(mfrow = c(1,2))
ermean <- function(x) cumsum(x) / seq_along(x)
plot(ermean(model1$sims.matrix[,"mu"]),   type = "l", main = "Ergodic mean - mu",   ylab = "", xlab = "Iterations")
plot(ermean(model1$sims.matrix[,"home"]), type = "l", main = "Ergodic mean - home", ylab = "", xlab = "Iterations")
