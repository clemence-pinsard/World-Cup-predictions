fit <- stan_foot(data = results_footBayes_groupstage1,
                 model="diag_infl_biv_pois",
                 predict= ngames_groupstage1,
                 ranking = wc2026_ranking,
                 dynamic_type = "seasonal",
                 home_effect = FALSE,
                 save_cmdstan_config = TRUE,
                 init = 0)

prob <- foot_prob(fit, results_footBayes_groupstage1)
colnames(prob$prob_table) <- c("home", "away",
                               "home win", "draw", "away win", "mlo")
knitr::kable(prob$prob_table)
#save prob$prob_table as csv
write.csv(prob$prob_table, "groupstage_1.csv", row.names = FALSE)
prob$prob_plot




fit_double_pois <- stan_foot(data = results_footBayes_groupstage1,
                 model="double_pois",
                 predict= ngames_groupstage1,
                 ranking = wc2026_ranking,
                 dynamic_type = "seasonal",
                 home_effect = FALSE,
                 save_cmdstan_config = TRUE,
                 init = 0)

prob_dp <- foot_prob(fit_double_pois, results_footBayes_groupstage1)
colnames(prob_dp$prob_table) <- c("home", "away",
                               "home win", "draw", "away win", "mlo")
knitr::kable(prob_dp$prob_table)
#save prob$prob_table as csv
write.csv(prob_dp$prob_table, "groupstage_1.csv", row.names = FALSE)
prob_dp$prob_plot