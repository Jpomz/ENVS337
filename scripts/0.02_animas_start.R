# scratch
library(readxl)
library(tidyverse)

# single metrics
# Richness, count of taxa present
# 1. species richness
# 2. EPT Richness (together and separate)
# diversity
# 3. Shannon diversity
# proportion of individuals
# 4. % E individuals (number / tot_n)
# 5. % Chironomidae individuals (number / tot_n)
# 6. % scraper individuals

A75CC <- read_excel("data/MMI_HW_data.xlsx", sheet = 1)
other <- read_excel("data/MMI_HW_data.xlsx", sheet = "other_sites")
taxa_info <- read_excel("data/MMI_HW_data.xlsx", sheet = "taxa_info")
site_info <- read_excel("data/MMI_HW_data.xlsx", sheet = "site_info")

sort(setdiff(other$Taxon, taxa_info$Taxon))

a75cc <- left_join(A75CC, site_info)
a75cc <- left_join(a75cc, taxa_info)

tot_rich <- a75cc |>
  distinct(Taxon) |>
  count() |>
  pull(n)

hept_rich <- a75cc |>
  filter(Family == "Heptageniidae") |>
  distinct(Taxon) |>
  count()|>
  pull(n)

ept_rich <- a75cc |>
  filter(Order == "Ephemeroptera" |
           Order == "Plecoptera" |
           Order == "Trichoptera" ) |>
  distinct(Taxon) |>
  count()

names(a75cc)
(ept_n <- a75cc |>
  mutate(tot_n = sum(Count)) |>
  filter(Order == "Ephemeroptera" |
           Order == "Plecoptera" |
           Order == "Trichoptera" ,
         Family != "Baetidae") |>
  summarize(ept_n = sum(Count),
            tot_n = unique(tot_n)) |>
  mutate(pEPTnoB = ept_n / tot_n))

tot_abundance <- sum(a75cc$Count)
ept_n/ tot_abundance

data.frame(tot_rich = tot_rich,
           hept_rich = hept_rich)

dat2 <- bind_rows(A75CC, a72)
sort(setdiff(dat2$Taxon, taxa_info$Taxon))
dat2 <- left_join(dat2, site_info)
dat2 <- left_join(dat2, taxa_info)

(tot_rich <- dat2|>
  group_by(site_code) |>
  distinct(Taxon) |>
  summarize(tot_rich = n()))
(hept_rich <- dat2 |>
  group_by(site_code) |>
  filter(Family == "Heptageniidae") |>
  distinct(Taxon) |>
  summarize(hept_rich = n()))

full_join(tot_rich, hept_rich)

dat2 %>%
  group_by(site_code, impact_category,Order) %>%
  distinct(Taxon) |>
  summarize(n_order = n()) |>
  ggplot(aes(y = n_order,
             x = Order,
             fill = impact_category,
             group = site_code)) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single"))


dat2|>
  select(site_code,
         Taxon,
         Count,
         impact_category) |>
  group_by(site_code, impact_category) %>%
  mutate(tot_n = sum(Count),
         pi = Count / tot_n,
         ln_pi = log(pi),
         pi_ln_pi = pi*ln_pi) |>
  summarize(H = -sum(pi_ln_pi)) |>
  ggplot(aes(y = H,
             x = site_code,
             fill = impact_category)) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single"))
