library(googlesheets4)
library(tidyverse)
source("tutorials/Data_3_custom_functions.R")

# google sheets
# gs4_deauth()
# sheet_url <- "https://docs.google.com/spreadsheets/d/1wXRXqfBh6_ZDQVxkUaPT4DmoNSPi477qVDyDgnktajs/edit?usp=sharing/"
# 
# sheet_names <- sheet_names(sheet_url)
# sheet_names <- sheet_names[sheet_names != "Template"]
# 
# all_data <- map_dfr(sheet_names, function(tab) {
#   read_sheet(sheet_url, sheet = tab,
#              col_types = "ccdDdc")  # tag rows with source tab
# })
# all_data
# View(all_data)

# make site info
# longitudinal
# sample date - before/after/ after spill - prior/24 hours / 1 week / 6 weeks
# Comparisons?

# calc all MMI scores
# filter out biotype 1/2 or write function that takes it as an argument and calcs appropriate one?

# Longitudinal 
# Filter out month 09 | 10

# temporal
# site %in% c("A68", "A72", "32nd_street", "Rotary_Park")


site1 <- readxl::read_excel(here::here("data/MMI_HW_data.xlsx"),
                            sheet = "A75CC")
site2 <- readxl::read_excel(here::here("data/MMI_HW_data.xlsx"),
                            sheet = "A72")
other_sites <- readxl::read_excel(here::here("data/MMI_HW_data.xlsx"),
                                  sheet = "other_sites")
taxa_info <- readxl::read_excel(here::here("data/MMI_HW_data.xlsx"),
                                sheet = "taxa_info")

site_info <- readxl::read_excel(here::here("data/MMI_HW_data.xlsx"),
                                sheet = "site_info")

sites <- bind_rows(site1, site2, other_sites |>
                     filter(site_code != "Purple Cliffs"))
dat <- left_join(sites, taxa_info)
counts <- dat

purp <- other_sites |>
  filter(site_code == "Purple Cliffs") |>
  left_join(taxa_info)


bp <- read_csv(here::here("data/bonita_peak.csv"))
taxa <- read_csv("data/MMI_taxa_info.csv")
names(dat)
names(bp)
colnames(bp) <- str_to_sentence(colnames(bp))
# left_join(bp, taxa) |>
#   filter(is.na(Bio_1_sensitive)) |>
#   distinct(Taxon) |>
#   arrange(Taxon) |> 
#   View()

bp <- left_join(bp, taxa) #|>
  # na.omit()
names(bp)
(mmi_metrics <- calc_bio2_metrics(dat))
bio2_mmi(mmi_metrics)

(bp_metrics <- calc_bio2_metrics(bp, Site_id))
names(mmi_metrics)
names(bp_metrics)
(bp_mmi <- bio2_mmi(bp_metrics, Site_id))
ggplot(bp_mmi, 
       aes(x = Site_id, 
           y = mmi_score)) +
  geom_col() +
  geom_hline(aes(yintercept = 48))+
  geom_hline(aes(yintercept = 40))

