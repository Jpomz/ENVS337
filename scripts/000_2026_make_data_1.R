# clean BP data for 337 data modules 1 +2
library(tidyverse)
ffg <- read_csv("data/ffg_info.csv")
ffg
bp_raw <- read_csv("data/raw_data/bp_macro_count.csv")
bp_raw


# correct counts based on sub samples
# bp_raw |>
#   mutate(multiplier = fraction_processed / 100) |>
#   select(count, total_count, fraction_processed, multiplier) |>
#   View()
# 
# 
# bp_raw |>
#   mutate(multiplier = fraction_processed / 100,
#          total_count_corrected = round(total_count / multiplier),
#          count_corrected = round(count / multiplier)) |>
#   select(count,
#          count_corrected, 
#          total_count, 
#          total_count_corrected,
#          fraction_processed, multiplier) |>
#   View()


bp_corrected <- bp_raw |>
  mutate(multiplier = fraction_processed / 100,
         total_count_corrected = round(total_count / multiplier),
         count_corrected = round(count / multiplier))

bp_corrected <- bp_corrected |>
  mutate(taxon = str_to_sentence(taxon)) |>
  select(stream:id,
         taxon,
         total_count_corrected, count_corrected) |>
  rename(total_count = total_count_corrected,
         count = count_corrected,
         site_id = id)


# taxon overlap between bp and ffg
sort(setdiff(bp_corrected$taxon,
        ffg$taxon))

ffg_thin <- ffg |>
  select(taxon:FFG)

bp <- left_join(bp_corrected,
                ffg_thin)

bp <- bp |>
  filter(!is.na(site_id))

bp

write_csv(bp, 
          "data/bonita_peak.csv")
