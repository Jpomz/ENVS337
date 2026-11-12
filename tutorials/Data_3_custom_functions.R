# Script for Data Module 2
# ENVS 337
# Written Fall 2026 by Justin Pomeranz

# packages ####

# check to see if packages are downloaded on machine
# If they are not, downloads the proper package
required_packages <- c("tidyverse", "googlesheets4")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(ggplot2)
# Set theme and ggplot options
theme_set(theme_bw())

# Set the session global for discrete fills (e.g., bars, boxplots)
options(ggplot2.discrete.fill = function() scale_fill_manual(values = c( "steelblue", "darkorange", "red4", "magenta4")))

# custom functions ####

# Calculate metrics ####
# This function takes a data frame (counts) and calculates a suite of potential metrics

# The counts df has to have the following columns:
# taxon: unique taxa designation
# order: the taxonomic order for that taxon
# family: the taxonomic family for that taxon
# FFG: the functional feeding group for that taxon
# count: the count of individuals for that taxon (already corrected to the same scale)

# This function returns a wide data frame with metric calculations
calc_bio2_metrics <- function(counts, group_column = site_code) {
  counts |>
    group_by(Sample_date, {{group_column}}) |>
    summarise(
      # impact = unique(site_category),
      TotalTax     = n_distinct(Taxon),
      aEPTTax = n_distinct(Taxon[Order %in% c('Ephemeroptera','Plecoptera', 'Trichoptera')]),
      pEPTnoB = 100 * sum(Count[Order %in% c('Ephemeroptera','Plecoptera', 'Trichoptera') & Family != "Baetidae"]) / sum(Count),
      ClngrTax = n_distinct(Taxon[Habit %in% c('clinger')]),
      IntolTax = n_distinct(Taxon[Bio_2_sensitive == TRUE]),
      pi_DecrMtnTrn = 100 * 
        sum(Count[Bio_2_sensitive == TRUE | Bio_1_sensitive == TRUE]) / sum(Count), 
      PredTaxFAC = n_distinct(Taxon[FFG %in% c('predator')]),
      ScrapPctFAC = 100 * 
        sum(Count[FFG %in% c("scraper")]) / sum(Count)#,
      #.by = c(Sample_date, {{group_column}})
    )
}

bio2_mmi <- function(metrics, group_column = site_code){
  metrics |>
    mutate(TotalTax = 100 * TotalTax / 42,
           aEPTTax = 100 * aEPTTax / 24.5,
           pEPTnoB = 100 * pEPTnoB / 79.3,
           ClngrTax = 100 * ClngrTax / 20,
           IntolTax = 100 * IntolTax / 21,
           pi_DecrMtnTrn = 100 * pi_DecrMtnTrn / 72.3,
           PredTaxFAC = 100 * PredTaxFAC / 13,
           ScrapPctFAC = 100 * ScrapPctFAC / 44.2,
           mmi_score = rowMeans(pick(TotalTax:ScrapPctFAC), na.rm = TRUE)) |>
    select({{group_column}}, Sample_date, mmi_score)
}

calc_bio1_metrics <- function(counts) {
  counts |>
    group_by(Sample_date, {{group_column}}) |>
    summarise(
      # impact = unique(site_category),
      aEPTTax = n_distinct(Taxon[Order %in% c('Ephemeroptera','Plecoptera', 'Trichoptera')]),
      NonINPct = 100 * sum(Count[!(Class %in% c('Insecta'))]) / sum(Count),
      pEPTnoB = 100 * sum(Count[Order %in% c('Ephemeroptera','Plecoptera', 'Trichoptera') & Family != "Baetidae"]) / sum(Count),
      ColeoPct = 100 * sum(Count[Order %in% c('Coleoptera')]) / sum(Count),
      apt_Intol = 100 * n_distinct(Taxon[Bio_1_sensitive == TRUE]) / n_distinct(Taxon),
      pi_IncrMidElev = 100 * sum(Count[Bio_1_tolerant == TRUE]) / sum(Count),
      ClngrTax = n_distinct(Taxon[Habit %in% c('clinger')]),
      PredShrTaxFac = n_distinct(Taxon[FFG %in% c('predator', 'shredder')]),
      .by = site_code
    )
}

bio1_mmi <- function(metrics){
  metrics |>
    mutate( aEPTTax= 100 *aEPTTax  / 24,
            NonINPct= 100 *(48.3 - NonINPct)  / 48.3,
            pEPTnoB= 100 *pEPTnoB  / 72.9,
            ColeoPct= 100 *ColeoPct  / 38.4,
            apt_Intol= 100 *apt_Intol  / 61,
            pi_IncrMidElev= 100 * (23.9 - pi_IncrMidElev)  / 23.9,
            ClngrTax= 100 *ClngrTax  / 20.8,
            PredShrTaxFac= 100 *PredShrTaxFac  / 14,
           mmi_score = rowMeans(pick(aEPTTax:PredShrTaxFac), na.rm = TRUE)) |>
    select({{group_column}}, Sample_date, mmi_score)
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