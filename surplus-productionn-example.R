library(sraplus)

library(here)

library(readxl)

library(janitor)

library(dplyr)

library(tidyr)

data <- read_xlsx(here("data","GulfOfSaintLaurence.xlsx")) %>%
  janitor::clean_names() %>%
  filter(!is.na(catch))


data

driors <- format_driors(
  catch = data$catch,
  years = data$year,
  index = data$biomass,
  index_years = data$year,
  initial_state = 1,
  sigma_obs_prior = 0.05,
  sigma_obs_prior_cv = 0.01,
  sigma_ratio_prior = .5,
  sigma_ratio_prior_cv = 0.01
)

plot_driors(driors)

a <- Sys.time()

fit <- fit_sraplus(
  driors = driors,
  engine = "stan",
  estimate_proc_error = TRUE,
  estimate_initial_state = TRUE,
  n_keep = 2000,
  tune_prior_predictive = TRUE
)
Sys.time() - a
plot_prior_posterior(fit = fit, driors = driors)

plot_sraplus(fit)

data(iccat)

jabba_catch <- data %>%
  select(year, catch) %>%
  rename(Year = year, Total = catch) %>%
  arrange(Year) %>%
  as.data.frame()

jabba_index <- data %>%
  select(year, biomass) %>%
  rename(Year = year, Index = biomass) %>%
  arrange(Year) %>%
  as.data.frame()




# Compile JABBA JAGS model and input object for bigeye tuna (bet)
jbinput = build_jabba(
  catch = jabba_catch,
  cpue = jabba_index,
  assessment = "BET",
  scenario = "TestRun",
  model.type = "Fox",
  sigma.est = FALSE,
  fixed.obsE = 0.01
)

# Fit JABBA (here mostly default value - careful)
a <- Sys.time()
bet1 = fit_jabba(jbinput,quickmcmc=TRUE)
Sys.time() - a

jbplot_cpuefits(bet1)

jbplot_trj(bet1,type="BBmsy",add=T)
