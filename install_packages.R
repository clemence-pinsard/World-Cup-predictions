# install.packages("devtools")

library(dplyr)
library(devtools)

# Package with the data (all the matches)
install_github("martj42/international_results")

# Package we need for footBayes
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan(overwrite = TRUE)

# FootBayes package installation
install_github("LeoEgidi/footBayes")

# Check
cmdstanr::check_cmdstan_toolchain()
library(footBayes)
packageVersion("footBayes")           
instantiate::stan_cmdstan_exists()

# Other packages
install.packages(c("posterior", "bayesplot", "loo"))

library(posterior)
library(bayesplot)
library(loo)
