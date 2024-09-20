#0.01 Data format
# this script takes the community data from Pomeranz et al. 2018 and wrangles it for use in ENVS337. 
library(tidyverse)

raw_dat <- read_csv("data/raw_data/estimated_dw.csv")

names(raw_dat)
#[1] "site"        "surber"      "taxa"        "linear_meas" "Unit"       
#[6] "dw"          "FFG"


ab_dat <- raw_dat |> 
  group_by(site, surber, taxa) %>%
  count()
ab_dat <- ab_dat %>%
  mutate(taxa = case_when(taxa == "Copepod" ~ "Copepoda",
                          .default = taxa))

taxa_vector <- sort(unique(ab_dat$taxa))

taxa_dat <- tibble(taxa = taxa_vector)
taxa_dat <- taxa_dat %>%
  filter(taxa != "Elmidae Adult")


taxa_dat$taxa
write_csv(taxa_dat, "data/taxa_table.csv")
# add taxonomy information manually in excel 