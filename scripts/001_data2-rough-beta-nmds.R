# working out full analysis for data 2 

library(tidyverse)
library(vegan)

bp <- read_csv(here::here("data/bonita_peak.csv"))
names(bp)
#distinct(bp, site_id) |> write_csv("data/site_categories.csv")

filter(bp, site_id == "MFMaM") |>
  select(stream, site)

filter(bp, stream == "animas") |>
  distinct(site_id, stream, site)

distinct(bp, taxon) |>
  arrange(taxon) |>
  pull()

bp_wide <- bp |>
  select(site_id, taxon, count) |>
  arrange(site_id) |>
  pivot_wider(names_from = taxon,
              values_from = count,
              values_fill = 0) |>
  column_to_rownames("site_id")

site_categories <- read_csv("data/site_categories.csv") |>
  mutate(site_category = factor(site_category, 
                                  levels = c("reference", 
                                             "light", 
                                             "heavy"))) |>
  arrange(site_id)

comm <- as.matrix(bp_wide)
total_n <- rowSums(comm)
range(total_n)

# set.seed(22)
# rrarefy(comm, sample = min(total_n))[,"Taenionema"]
rare_richness <- rarefy(comm, sample = min(total_n))
rownames(comm)
site_categories$site_id

alpha <- data.frame(
  site     = rownames(comm),
  impact   = site_categories$ccu_chronic,
  n_ind    = total_n,
  S_rare   = as.numeric(rare_richness),
  S_raw    = specnumber(comm),
  shannon  = diversity(comm, "shannon"),
  simpson  = diversity(comm, "simpson")
)

alpha$hill1 <- exp(alpha$shannon)

ggplot(alpha,
       aes(x = impact, 
           y = shannon)) +
  geom_point() +
  scale_x_log10()


m_shan <- lm(shannon ~ log10(impact), data = alpha)
anova(m_shan)
summary(m_shan)
#plot(m_shan)  # residual diagnostics


# Beta ####
comm_t <- sqrt(comm) # or log1p(comm)
# (or use comm_rare if you rarefied)

bray <- vegdist(comm_t, method = "bray")
set.seed(42)
# are the centroids different?
perm <- adonis2(bray ~ site_category, data = site_categories, permutations = 9999)
perm

# pairs <- combn(levels(site_categories$site_category), 2, simplify = FALSE)
# 
# pw <- map_dfr(pairs, function(p) {
#   idx <- site_categories$site_category %in% p
#   r <- adonis2(as.dist(as.matrix(bray)[idx, idx]) ~ droplevels(site_categories$site_category[idx]),
#                permutations = 9999)
#   tibble(pair = paste(p, collapse = " vs "), R2 = r$R2[1], p = r$`Pr(>F)`[1])
# }) %>% mutate(p_adj = p.adjust(p, "holm"))
# pw
# 
# simper_out <- simper(comm_t, site_categories$site_category, permutations = 999)
# summary(simper_out)

# NMDS ####
set.seed(42)
nmds <- metaMDS(comm_t, distance = "bray", k = 2, trymax = 100)
nmds$stress          # < 0.2 acceptable, < 0.1 good
stressplot(nmds)

sc <- as.data.frame(scores(nmds, display = "sites"))
sc$impact <- site_categories$site_category

hulls <- sc %>% group_by(impact) %>% slice(chull(NMDS1, NMDS2))

ggplot(sc, aes(NMDS1, NMDS2, color = impact, fill = impact)) +
  geom_polygon(data = hulls, alpha = 0.15, color = NA) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95, linetype = 2) +   # needs >= 4 pts/group
  scale_color_manual(
    values = c(reference = "#2c7bb6",
               light = "#fdae61",
               heavy = "#d7191c")) +
  scale_fill_manual(
    values  = c(reference = "#2c7bb6",
                light = "#fdae61",
                heavy = "#d7191c")) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = paste0("Stress = ", round(nmds$stress, 3),
                          "\nPERMANOVA R² = ", round(perm$R2[1], 2),
                          ", p = ", perm$`Pr(>F)`[1])) +
  theme_minimal() +
  labs(color = "Impact", fill = "Impact")







