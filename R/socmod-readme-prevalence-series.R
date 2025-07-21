library(socmod)
library(magrittr)
library(ggplot2)

# Model generator
abm_gen <- function(params) {
  do.call(make_abm, params) %>%
    initialize_agents(
      initial_prevalence = params$initial_prevalence,
      adaptive_fitness = params$adaptive_fitness
    )
}
# Run five trials per parameter setting, i.e., for each specified adaptive_fitness
adaptive_fitness_vals <- c(0.9, 1.1, 1.4)
trials <-
  run_trials(
    abm_gen,
    n_trials_per_param = 5,
    stop = socmod::fixated,
    n_agents = 20,
    initial_prevalence = 0.1,
    adaptive_fitness = adaptive_fitness_vals
  )
# Summarize within trials only; rename and factor adaptive fitness for display
summary <- summarise_prevalence(
  trials, input_parameters = "adaptive_fitness", across_trials = F
) %>% 
  dplyr::mutate(
    `Adaptive fitness` = factor(adaptive_fitness, adaptive_fitness_vals)
  )

p <- ggplot(
  summary,
  aes(x=Step, y=Prevalence,
      color=`Adaptive fitness`,
      linetype=`Adaptive fitness`,
      group = trial_id)) +
  geom_line(linewidth=1.4, alpha = 0.875) + theme_classic(base_size = 16) +
  ggplot2::scale_color_manual(values = unname(SOCMOD_PALETTE))

print(p)
