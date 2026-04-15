#!/usr/bin/env Rscript

# ────────────────────────────────────────────────
# CROSS-SPECIES GP-LAURDAN VISUALIZATION
# - Reads *_GP_values_*.csv files
# - Creates:
#     1) GP vs Pressure at 5 °C
#     2) GP vs Temperature at 2 bar
# - One color per species
# - Optional averaging across replicates
# Edited: 12/12/2025
# ────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(ggsci)
})

# ============================================================
# PARAMETERS TO EDIT
# ============================================================

data_dir <- "."          # directory with GP CSV files
csv_pattern <- "_GP_values_.*\\.csv$"

# Target conditions
target_temp  <- 5        # °C for pressure plot
target_press <- 2        # bar for temperature plot

# Tolerances (important for floating point / rounding)
temp_tol  <- 0.25        # ± °C
press_tol <- 0.25        # ± bar

# Average GP if multiple measurements exist?
average_replicates <- TRUE

# Optional fixed GP axis scale, set to NA if don't want this
#GP_ymin <- -0.6
#GP_ymax <- 0
GP_ymin <- NA
GP_ymax <- NA

# What to call the file name (which species I'm focusing on, or all)
species_file_name <- "lobates"

# Option 1: explicitly include ONLY these species
#species_include <- NULL

#---------------------
#April 2026
#---------------------
#All species
species_include <- c("BathocyroeV4573d3", "BolinopsisD1339bw9wproti", "BolinopsisV4573d5", "HcalMBAC1", "LampocteisV4715ss6", "BeroeabyssV4573ss11", "BeroeMB250428bw13" , "LampocteisMBA25C8", "RedcydipD1369d10")

#Cydippids
#species_include <- c("HcalMBAC1", "RedcydipD1369d10")

#Beroes
#species_include <- c("BeroeabyssV4573ss11", "BeroeMB250428bw13")

#Lobates
species_include <- c("BathocyroeV4573d3", "BolinopsisD1339bw9wproti", "BolinopsisV4573d5", "LampocteisV4715ss6", "LampocteisMBA25C8")

#Protease inhibitor tests
#species_include <- c("BathocyroeV4573d2wproti", "BathocyroeV4573d2", "BathocyroeV4573d3")
#species_include <- c("BolinopsisV4573d5", "BolinopsisD1339bw9wproti")
#species_include <- c("HcalMBAC1", "HcalMBAC2ProK", "HcalMBAC2wproti")
#species_include <- c("LampocteisV4715ss6", "LampocteisV4715ss6wproti", "LampocteisMBA25C8")

#---------------------
#December 2025
#---------------------
#species_include <- c("BathocyroeV4573d2", "HormiphoraMBAC5", "LampocteisV4573d10", "LeucotheaD1244bw25", "WhiteNepheloctenaD1398ss7")


# Option 2: exclude specific species
species_exclude <- NULL
# species_exclude <- c("HcalMBAC2wproti")


# ============================================================
# SPECIES COLOR MAPPING
# ============================================================

# Adjust species colours

species_colors <- c(
  #December 2025
  "BathocyroeV4573d2"   = "#48cae4",
  "HormiphoraMBAC5"  = "#5e548e",
  "LampocteisV4573d10"  = "#d00000",
  "LeucotheaD1244bw25"  = "#91D1C2FF",
  "WhiteNepheloctenaD1398ss7" = "#00A087FF",
  
  #April 2026
  "BathocyroeV4573d3"  = "#48cae4",
  "BathocyroeV4573d2wproti"  = "#ade8f4",
  
  "BolinopsisV4573d5"  = "#2d6a4f",
  "BolinopsisD1339bw9wproti"  = "#74c69d",
  
  "HcalMBAC1"       = "#5e548e",
  "HcalMBAC2ProK"  = "#9f86c0",
  "HcalMBAC2wproti" = "#e0b1cb",
  
  "RedcydipD1369d10" = "#800f2f",
  
  "LampocteisV4715ss6" = "#d00000",
  "LampocteisV4715ss6wproti" = "#6a040f",
  "LampocteisMBA25C8" = "#ffb3c1",
  
  "BeroeabyssV4573ss11" = "#f48c06",
  "BeroeMB250428bw13" = "#ffba08"
)

species_levels <- names(species_colors)

# ============================================================
# LOAD AND COMBINE CSV FILES
# ============================================================

files <- list.files(
  path = data_dir,
  pattern = csv_pattern,
  full.names = TRUE
)

if (length(files) == 0)
  stop("No GP CSV files found")

message("Found ", length(files), " GP CSV files")

gp_all <- map_dfr(files, function(f) {
  
  species <- str_extract(basename(f), "^[^_]+")
  
  read_csv(f, col_types = cols()) %>%
    mutate(
      Species = species,
      Press_bar = if ("Press_bar" %in% names(.)) Press_bar else Press_psi / 14.5038
    )
})

# Filter for which species to include/exclude
if (!is.null(species_include)) {
  gp_all <- gp_all %>%
    dplyr::filter(Species %in% species_include)
}

if (!is.null(species_exclude)) {
  gp_all <- gp_all %>%
    dplyr::filter(!Species %in% species_exclude)
}

# Lock species factor levels (prevents color shuffling)
# Keep only colors for species actually in use
species_colors_used <- species_colors[names(species_colors) %in% unique(gp_all$Species)]
gp_all$Species <- factor(gp_all$Species, levels = names(species_colors_used))

# Safety check: missing species colours
missing_species <- setdiff(unique(gp_all$Species), names(species_colors))
if (length(missing_species) > 0) {
  stop("Missing colours for species: ",
       paste(missing_species, collapse = ", "))
}

# ============================================================
# 1) GP vs PRESSURE at 5 °C
# ============================================================

gp_pressure <- gp_all %>%
  dplyr::filter(abs(Temp - target_temp) <= temp_tol)

# First: average triplicates within each step
gp_pressure_step <- gp_pressure %>%
  group_by(Species, Press_bar, Temp, step_id) %>%
  summarise(GP = mean(GP, na.rm = TRUE), .groups = "drop")

# Second: average across steps (if multiple ramps exist)
gp_pressure_summary <- gp_pressure_step %>%
  group_by(Species, Press_bar) %>%
  summarise(
    GP_mean = mean(GP, na.rm = TRUE),
    GP_sd = sd(GP, na.rm = TRUE),
    n = sum(is.finite(GP)),
    GP_se = GP_sd / sqrt(n),
    .groups = "drop"
  )


p_pressure <- ggplot(
  gp_pressure_summary,
  aes(x = Press_bar, y = GP_mean, color = Species, group = Species)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = GP_mean - GP_se, ymax = GP_mean + GP_se),
    width = 20, alpha = 0.6
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.subtitle = element_text(size = 18, colour = "grey30"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 18),
    axis.text.y  = element_text(size = 18)
  ) +
  labs(
    subtitle = paste("Temperature:", target_temp, "ºC"),
    x = "Pressure (bar)",
    y = "Laurdan GP"
  ) +
  scale_color_manual(values = species_colors_used, drop = TRUE) +
  scale_y_continuous(limits = c(GP_ymin, GP_ymax))
  

# ============================================================
# 2) GP vs TEMPERATURE at 2 bar
# ============================================================

gp_temperature <- gp_all %>%
  dplyr::filter(abs(Press_bar - target_press) <= press_tol)

# First: average triplicates within each step
gp_temperature_step <- gp_temperature %>%
  group_by(Species, Temp, Press_bar, step_id) %>%
  summarise(
    GP = mean(GP, na.rm = TRUE),
    .groups = "drop"
  )

# Second: average across steps
gp_temperature_summary <- gp_temperature_step %>%
  group_by(Species, Temp) %>%
  summarise(
    GP_mean = mean(GP, na.rm = TRUE),
    GP_sd   = sd(GP, na.rm = TRUE),
    n       = sum(is.finite(GP)),
    GP_se   = GP_sd / sqrt(n),
    .groups = "drop"
  )

p_temperature <- ggplot(
  gp_temperature_summary,
  aes(x = Temp, y = GP_mean, color = Species, group = Species)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = GP_mean - GP_se, ymax = GP_mean + GP_se),
    width = 0.5, alpha = 0.6
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.subtitle = element_text(size = 18, colour = "grey30"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 18),
    axis.text.y  = element_text(size = 18)
  ) +
  labs(
    subtitle = paste("Pressure:", target_press, "bar"),
    x = "Temperature (°C)",
    y = "Laurdan GP"
  ) +
  scale_color_manual(values = species_colors_used, drop = TRUE) +
  scale_y_continuous(limits = c(GP_ymin, GP_ymax)) 

# ============================================================
# SAVE OUTPUTS
# ============================================================

print(p_pressure)
print(p_temperature)

# Define file names
p_pressure_pdf <- paste0(species_file_name,"_GPvsPress_", target_temp, ".pdf")
p_temp_pdf <- paste0(species_file_name,"_GPvsTemo_", target_press, ".pdf")

# Save plots
ggsave(p_pressure_pdf, p_pressure, width = 8, height = 6, dpi = 300)
ggsave(p_temp_pdf, p_temperature, width = 8, height = 6, dpi = 300)

message("Saved cross-species GP comparison plots")
