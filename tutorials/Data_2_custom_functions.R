# Script for Data Module 2
# ENVS 337
# Written Fall 2026 by Justin Pomeranz

# packages ####

# check to see if packages are downloaded on machine
# If they are not, downloads the proper package
required_packages <- c("tidyverse", "vegan")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(ggplot2)
# Set theme and ggplot options
theme_set(theme_bw())

# Set the session global for discrete fills (e.g., bars, boxplots)
options(ggplot2.discrete.fill = function() scale_fill_brewer(palette = "RdYlBu", direction = -1))

# custom functions ####

# Calculate metrics ####
# This function takes a data frame (counts) and calculates a suite of potential metrics

# The counts df has to have the following columns:
# taxon: unique taxa designation
# order: the taxonomic order for that taxon
# family: the taxonomic family for that taxon
# FFG: the functional feeding group for that taxon
# count: the count of individuals for that taxon (already corrected to the same scale)

# This function returns a wode data frame with metric calculations
calc_metrics <- function(counts) {
  counts |>
    summarise(
      impact = unique(site_category),
      tot_rich     = n_distinct(taxon),
      ept_rich = n_distinct(taxon[order %in% c('Ephemeroptera','Plecoptera', 'Trichoptera')]),
      e_rich = n_distinct(taxon[order %in% c('Ephemeroptera')]),
      t_rich =  n_distinct(taxon[order %in% c('Trichoptera')]),
      p_rich =  n_distinct(taxon[order %in% c('Plecoptera')]),
      hept_rich =  n_distinct(taxon[family %in% c('Heptageniidae')]),
      rhya_rich =  n_distinct(taxon[family %in% c('Rhyacophilidae')]),
      pred_rich =  n_distinct(taxon[FFG %in% c('predator')]),
      scraper_rich = n_distinct(taxon[FFG %in% c('scraper')]),
      shannon      = vegan::diversity(count, index = 'shannon'),
      pct_chiro    = 100 * 
        sum(count[family %in% 'Chironomidae']) / sum(count),
      pct_ept    = 100 * 
        sum(count[order %in% c('Ephemeroptera',
                               'Plecoptera',
                               'Trichoptera')]) / sum(count),
      .by = site_id
    )
}


# plot metrics ####
plot_metrics <- function(metric_df){
  p <- metric_df |>
  pivot_longer(tot_rich:pct_ept,
               names_to = 'metric',
               values_to = 'value') |>
  ggplot(aes(x = impact,
             y = value,
             fill = impact)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  facet_wrap(~metric, scales = 'free_y') +
  labs(x = 'Impact category',
       y = 'Metric value') +
  NULL
  print(p)
}

# Score sites ####

# this function gives relative scores for each chosen metric all on the same scale

# it requires a `metrics` data frame which has the scores in a wide format
# it also requires a 'scoring' table which defines the relative score values

# it returns a data frame with a total index score per site in a long format
score_sites <- function(metric_df, metrics, scoring) {
  metric_df |>
    pivot_longer(all_of(metrics), 
                 names_to = 'metric', values_to = 'value') |>
    inner_join(scoring, by = 'metric') |>
    mutate(score = case_when(
      direction == 'decrease' & value >= upper ~ 5,
      direction == 'decrease' & value >= lower ~ 3,
      direction == 'decrease'                  ~ 1,
      direction == 'increase' & value <= lower ~ 5,
      direction == 'increase' & value <= upper ~ 3,
      TRUE                                     ~ 1
    )) |>
    summarise(index = sum(score), .by = site_id)
}

# classify index scores

# this function take a data frame of index scores (from the score_sites() function) and classifies sites as references, or as light or heavy impacts

# You must define the cutoff values for `ref_min` and `light_min` in the environment
classify_index <- function(index) {
  case_when(
    index >= ref_min   ~ 'reference',
    index >= light_min ~ 'light',
    TRUE               ~ 'heavy'
  )
}