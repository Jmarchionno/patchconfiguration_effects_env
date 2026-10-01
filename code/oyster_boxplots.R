# ============================================================
# Oyster Reef Restoration — Boxplots
# Density, elevation, C:N ratio, and sediment grain-size fractions
# by treatment (h = high, c = control, l = low)
# ============================================================

library(tidyverse)   # install.packages("tidyverse") if needed

# ---- 1. Load data -------------------------------------------------
oyster_alldat <- read_csv("oyster_latlong_elevation_tex_element.csv")

# Rename columns by POSITION (not by their original text) so the script
# doesn't break if punctuation/spacing in the header gets altered by
# whatever reads the file (e.g. base read.csv() converts "C:N ratio" to
# "C.N.ratio"). This assumes the columns are still in their original
# order below — run `names(oyster_alldat)` first if you're not sure.
colnames(oyster_alldat) <- c(
  "sample_id", "treatment", "treatment_substrate", "substrate_sampled",
  "density", "percent_cover", "easting", "northing", "elevation",
  "longitude", "latitude", "mean_oyster_density",
  "wt_pct_n", "wt_pct_total_c", "pct_caco3", "pct_tic", "wt_pct_toc",
  "cn_ratio", "frac_gt_6mm", "frac_2_6mm", "frac_063_2mm", "frac_lt_063mm"
)

# Order treatment levels for consistent plotting (control, low, high)
oyster_alldat <- oyster_alldat %>%
  mutate(treatment = factor(treatment, levels = c("c", "l", "h"),
                             labels = c("Control", "Low", "High")))

# ---- 2. Oyster density by treatment --------------------------------
p_density <- ggplot(oyster_alldat, aes(x = treatment, y = density, fill = treatment)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.8) +
  labs(title = "Oyster Density by Treatment",
       x = "Treatment", y = expression("Density (#/m"^2*")")) +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 3. Elevation by treatment --------------------------------------
p_elevation <- ggplot(oyster_alldat, aes(x = treatment, y = elevation, fill = treatment)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.8) +
  labs(title = "Elevation by Treatment",
       x = "Treatment", y = "Elevation (m, NAVD88)") +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 4. C:N ratio by treatment (sediment/shell samples only) --------
p_cn <- oyster_alldat %>%
  filter(!is.na(cn_ratio)) %>%
  ggplot(aes(x = treatment, y = cn_ratio, fill = treatment)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.8) +
  labs(title = "Sediment C:N Ratio by Treatment",
       x = "Treatment", y = "C:N Ratio") +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 5. Sediment grain-size fractions (faceted) ----------------------
# Reshape the four fraction columns to long format for faceting
fraction_long <- oyster_alldat %>%
  filter(!is.na(frac_gt_6mm)) %>%
  select(treatment, frac_gt_6mm, frac_2_6mm, frac_063_2mm, frac_lt_063mm) %>%
  pivot_longer(cols = starts_with("frac_"),
               names_to = "grain_size", values_to = "fraction") %>%
  mutate(grain_size = factor(grain_size,
           levels = c("frac_gt_6mm", "frac_2_6mm", "frac_063_2mm", "frac_lt_063mm"),
           labels = c("> 6 mm", "2-6 mm", "0.063-2 mm", "< 0.063 mm")))

p_fractions <- ggplot(fraction_long, aes(x = treatment, y = fraction, fill = treatment)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.8) +
  facet_wrap(~ grain_size, nrow = 1) +
  labs(title = "Sediment Grain-Size Fractions by Treatment",
       x = "Treatment", y = "Fraction of sample") +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 6. Display all plots --------------------------------------------
print(p_density)
print(p_elevation)
print(p_cn)
print(p_fractions)

# ---- 7. Optional: save plots to file ----------------------------------
# ggsave("density_boxplot.png", p_density, width = 6, height = 4, dpi = 300)
# ggsave("elevation_boxplot.png", p_elevation, width = 6, height = 4, dpi = 300)
# ggsave("cn_ratio_boxplot.png", p_cn, width = 6, height = 4, dpi = 300)
# ggsave("sediment_fractions_boxplot.png", p_fractions, width = 10, height = 4, dpi = 300)
