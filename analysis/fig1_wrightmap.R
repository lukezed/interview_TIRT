# ==============================================================================
# Script: plot_simulation.R
# Purpose: Fit Bayesian RSM model and generate diagnostic plots
# Authorship: Zhang, C. (2026)
# ==============================================================================

# --- 1. Load Environment ---
library(tidyverse)
library(brms)
library(patchwork)

# --- 2. Fit or Load Model ---
# Using the 'file' argument to cache MCMC results and prevent redundant sampling
fit_rsm <- brm(
  formula = resp ~ 1 + comm + (0 + dim_a + dim_r | ID) + (1 | item),
  data = simdata,
  family = brmsfamily("acat", "logit"), 
  prior = c(
    prior(normal(0, 3), class = "b"), 
    prior(normal(0, 1), class = "sd"),
    prior(lkj(2), class = "cor")
  ),
  backend = "cmdstanr", 
  chains = 4, iter = 2000, warmup = 1000, cores = 4,
  file = "models/fit_rsm" # Automatically saves/loads models/fit_rsm.rds
)

# --- 3. Data Extraction & Processing ---
# Function to extract random effects and calculate ranks for "Caterpillar" plots
get_re_ranked <- function(model, trait_name) {
  # Merge with community info from the original simdata
  id_info <- simdata %>% 
    distinct(ID, comm) %>% 
    mutate(ID = as.character(ID))
  
  ranef(model)$ID[, , trait_name] %>%
    as_tibble(rownames = "ID") %>%
    left_join(id_info, by = "ID") %>%
    arrange(Estimate) %>%
    mutate(rank = row_number(), trait = trait_name)
}

# Prepare long-format data for person abilities
id_re_long <- bind_rows(
  get_re_ranked(fit_rsm, "dim_a"), 
  get_re_ranked(fit_rsm, "dim_r")
)

# Prepare item difficulty data
it_re <- ranef(fit_rsm)$item[, , 1] %>% 
  as_tibble(rownames = "item") %>% 
  mutate(dim = str_sub(item, 1, 1))

# --- 4. Define Aesthetic Theme (Bürkner-style) ---
theme_burkner <- theme_bw() +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 9, family = "serif"),
    strip.background = element_rect(fill = "#E0E0E0", color = "black"),
    strip.text = element_text(color = "black", size = 10, face = "bold"),
    legend.position = "right",
    aspect.ratio = 1.4
  )

colors_comm <- c("G" = "#D55E00", "N" = "#0072B2") # Colorblind friendly palette

# --- 5. Create Subplots ---

# P1: Item Difficulty Distribution
p1 <- it_re %>%
  mutate(panel_title = "Item Difficulty") %>%
  ggplot(aes(x = Estimate, y = item, color = dim)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0.4) +
  facet_wrap(~panel_title) +
  labs(x = "Logit", y = NULL, color = "Domain") +
  theme_burkner

# P2: Person Ability Facets
trait_labels <- as_labeller(c(
  "dim_a" = "Ability: Autonomy (a)",
  "dim_r" = "Ability: Relatedness (r)"
))

p_ability <- ggplot(id_re_long, aes(x = Estimate, y = rank, color = comm)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), alpha = 0.6) +
  facet_wrap(~trait, labeller = trait_labels) +
  scale_color_manual(values = colors_comm) +
  labs(x = "Logit", color = "Community", y = "Ranked Individuals") +
  theme_burkner

# --- 6. Assemble Final Composite Image ---
# Use patchwork to combine plots with relative widths
fig_final <- p1 + p_ability +
  plot_layout(ncol = 2, widths = c(1, 2), guides = "collect")