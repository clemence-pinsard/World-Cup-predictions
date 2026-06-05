# We will conduct a descriptive analysis of the data available to us 

# We will start with ranking

summary(ranking)

# We have 3 variables in the dataframe ranking : team (team names), periods
# (time of this ranking), rank_points (points of each team) and 211 observations (countries)

summary(ranking$rank_points)
sd(ranking$rank_points, na.rm = TRUE)

hist(ranking$rank_points, main = "Distribution of ranking points", xlab = "Points", col = "hotpink3")

# Then we will look at results (just the results since 2022)

summary(results_since_2022)

sd(results_since_2022$home_score, na.rm = TRUE)
sd(results_since_2022$away_score, na.rm = TRUE)

# Scores distribution
table_scores <- results_since_2022 %>%
  count(home_goals, away_goals) %>%
  arrange(desc(n))
print(head(table_scores, 10))

# Distribution of the goals 
p1 <- ggplot(results_since_2022, aes(x = home_score)) +
  geom_bar(fill = "dodgerblue4", alpha = 0.8) +
  labs(title = "Home goals since 2022",
       x = "Goals", y = "Frequency") + theme_minimal()

p2 <- ggplot(results_since_2022, aes(x = away_score)) +
  geom_bar(fill = "indianred3", alpha = 0.8) +
  labs(title = "Away goals since 2022",
       x = "Goals", y = "Frequency") + theme_minimal()

print(p1)
print(p2)
# Poisson seems a good idea 

# Effect of a neutral field

aggregate(cbind(home_score, away_score) ~ neutral, data = results_since_2022, FUN = mean)
wilcox.test(home_score ~ neutral, data = results_since_2022)
wilcox.test(away_score ~ neutral, data = results_since_2022)

# Zero-inflation ?

prop.table(table(results_since_2022$home_score == 0))
prop.table(table(results_since_2022$away_score == 0))
# The results are ok, not really a zero-inflation

# Independance between home_score and away_score ?

table_scores <- table(results_since_2022$home_score, results_since_2022$away_score)
chisq.test(table_scores)
# The pvalue is < 0.05 so for a 5% risk we reject the hypothesis of independance between 
# home_goals and away goals 

# Draws ?

prop_nul_reel <- mean(results_since_2022$home_score == results_since_2022$away_score, na.rm = TRUE)
print(prop_nul_reel)
# We can try both bivariate poisson model and diagonal inflated bivariate poisson and decide
# with a validation procedure which one is the best because there is not a excess of draws 
# but the diagonal inflated bivariate poisson could be interresting after all
