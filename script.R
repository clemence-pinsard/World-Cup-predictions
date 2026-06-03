fit <- stan_foot(data = results_footBayes_groupstage1,
                 model="diag_infl_biv_pois",
                 predict= ngames_groupstage1,
                 ranking = ranking,
                 dynamic_type = "seasonal",
                 home_effect = FALSE)
