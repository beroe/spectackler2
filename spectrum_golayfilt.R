#!/usr/bin/env Rscript

# ──────────────────────────────────────────────────────────────────────────
# LAURDAN GP METHOD COMPARISON (SINGLE SPECTRUM)
#
# - Tests all GP calculation methods used in pipeline
# - Optional blank subtraction (matches main GP-over-time script)
#
# Methods compared:
#   1) exact (raw 440/490)
#   2) band mean (raw)
#   3) band mean (SG)
#   4) sg_exact (SG 440/490)
#   5) sg_peaks
#   6) band integrated (SG)
#
# Edited: 12/18/2025
# ──────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(signal)
})

# ============================================================
# PARAMETERS
# ============================================================
sample_name <- "Lampocteis_MBA25C8_041326-start"
file <- "spectrum_20260413T092236904875_idx1_P30_T1.0.tsv"
data_dir <- "."   # where spectrum + blank live

subtract_blank <- TRUE

sg_poly_order <- 3
sg_window     <- 101

# Fixed wavelengths
wl_blue  <- 440
wl_green <- 490

# Band definitions
blue_band  <- c(420, 460)
green_band <- c(470, 510)

# Peak search windows
blue_peak_range  <- c(430, 450)
green_peak_range <- c(485, 505)

# ============================================================
# HELPERS
# ============================================================

trapz <- function(x, y) {
  idx <- order(x)
  x <- x[idx]; y <- y[idx]
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

# Function to calculate GP using band-averaged method (weighted so points spanning larger wavelength contribute more)
trapz <- function(x, y) {
  idx <- order(x)
  x <- x[idx]; y <- y[idx]
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

band_weighted_mean <- function(w, y, rng) {
  idx <- which(w >= rng[1] & w <= rng[2])
  if (length(idx) < 2) return(NA_real_)
  area <- trapz(w[idx], y[idx])
  width <- diff(rng)
  area / width
}

gp_band_weighted <- function(w, y, blue_rng, green_rng) {
  ib <- band_weighted_mean(w, y, blue_rng)
  ig <- band_weighted_mean(w, y, green_rng)
  if (!is.finite(ib + ig) || (ib + ig) == 0) return(NA_real_)
  (ib - ig) / (ib + ig)
}

# Function to calculate GP using band-integrated method
gp_band_integrated <- function(w, y, blue_range, green_range) {
  b <- which(w >= blue_range[1]  & w <= blue_range[2])
  g <- which(w >= green_range[1] & w <= green_range[2])
  if (length(b) < 2 || length(g) < 2) return(NA_real_)
  ib <- trapz(w[b], y[b])
  ig <- trapz(w[g], y[g])
  if (!is.finite(ib + ig) || (ib + ig) == 0) return(NA_real_)
  (ib - ig) / (ib + ig)
}

# Function to calculate GP using band-integrated method and normalize to band width
gp_band_int_norm <- function(w, y, blue_range, green_range) {
  b <- which(w >= blue_range[1]  & w <= blue_range[2])
  g <- which(w >= green_range[1] & w <= green_range[2])
  if (length(b) < 2 || length(g) < 2) return(NA_real_)
  ib <- trapz(w[b], y[b]) / diff(blue_range)
  ig <- trapz(w[g], y[g]) / diff(green_range)
  if (!is.finite(ib + ig) || (ib + ig) == 0) return(NA_real_)
  (ib - ig) / (ib + ig)
}


# ============================================================
# LOAD BLANK (if enabled)
# ============================================================

if (subtract_blank) {

  # Automatically detects blank.tsv file in directory
  blank_candidates <- list.files(
    path = data_dir,
    pattern = "^blank_.*\\.tsv$",
    full.names = TRUE
  )
  
  if (length(blank_candidates) == 0)
    stop("subtract_blank = TRUE but no blank_*.tsv file found")
  
  blank_file <- blank_candidates[which.max(file.info(blank_candidates)$mtime)]
  message("Using blank file: ", basename(blank_file))
  
  blank <- read_tsv(blank_file, show_col_types = FALSE) %>%
    rename(wl = Wavelength, intensity = Intensity) %>%
    mutate(
      wl = as.numeric(wl),
      intensity = as.numeric(intensity)
    )
}

# ============================================================
# LOAD SPECTRUM + BLANK SUBTRACTION
# ============================================================

spec <- read_tsv(file, show_col_types = FALSE) %>%
  rename(wl = Wavelength, intensity = Intensity) %>%
  mutate(
    wl = as.numeric(wl),
    intensity = as.numeric(intensity)
  ) %>%
  arrange(wl)

if (subtract_blank) {
  blank_interp <- approx(
    x = blank$wl,
    y = blank$intensity,
    xout = spec$wl,
    rule = 2
  )$y
  
  spec <- spec %>%
    mutate(intensity = intensity - blank_interp)
}

# ============================================================
# SG SMOOTHING - APPLY SG FILTER
# ============================================================

if (sg_window %% 2 == 0) sg_window <- sg_window + 1

spec <- spec %>%
  mutate(
    intensity_sg = sgolayfilt(
      x = intensity,
      p = sg_poly_order,
      n = sg_window
    )
  )

# ============================================================
# GP CALCULATIONS
# ============================================================

# exact (raw)
i440_raw <- approx(spec$wl, spec$intensity, xout = wl_blue)$y
i490_raw <- approx(spec$wl, spec$intensity, xout = wl_green)$y
GP_exact_raw <- (i440_raw - i490_raw) / (i440_raw + i490_raw)

# sg_exact
i440_sg <- approx(spec$wl, spec$intensity_sg, xout = wl_blue)$y
i490_sg <- approx(spec$wl, spec$intensity_sg, xout = wl_green)$y
GP_sg_exact <- (i440_sg - i490_sg) / (i440_sg + i490_sg)

# band mean
GP_band_mean_raw <- gp_band_weighted(spec$wl, spec$intensity,    blue_band, green_band)
GP_band_mean_sg  <- gp_band_weighted(spec$wl, spec$intensity_sg, blue_band, green_band)

# band integrated - use normalized method only, the un-normalized ones 
# GP_band_int_raw <- gp_band_integrated(spec$wl, spec$intensity,    blue_band, green_band)
# GP_band_int_sg  <- gp_band_integrated(spec$wl, spec$intensity_sg, blue_band, green_band)
GP_band_int_norm_sg <- gp_band_int_norm(spec$wl, spec$intensity_sg, blue_band, green_band)

# sg_peaks
blue_idx  <- which(spec$wl >= blue_peak_range[1]  & spec$wl <= blue_peak_range[2])
green_idx <- which(spec$wl >= green_peak_range[1] & spec$wl <= green_peak_range[2])

blue_i  <- which.max(spec$intensity_sg[blue_idx])
green_i <- which.max(spec$intensity_sg[green_idx])

blue_peak_wl  <- spec$wl[blue_idx][blue_i]
green_peak_wl <- spec$wl[green_idx][green_i]

blue_peak_int  <- spec$intensity_sg[blue_idx][blue_i]
green_peak_int <- spec$intensity_sg[green_idx][green_i]

GP_sg_peaks <- (blue_peak_int - green_peak_int) /
  (blue_peak_int + green_peak_int)

# ============================================================
# SUMMARY TABLE
# ============================================================

gp_summary <- tibble(
  Method = c(
    "exact (raw 440/490)",
    "sg_exact (SG 440/490)",
    "sg_peaks",
    "band mean (raw)",
    "band mean (SG)",
    "band integrated (SG)"
  ),
  GP = c(
    GP_exact_raw,
    GP_sg_exact,
    GP_sg_peaks,
    GP_band_mean_raw,
    GP_band_mean_sg,
    GP_band_int_norm_sg
  )
)

print(gp_summary)

# ============================================================
# PLOTS
# ============================================================

# Data for band shading (SG-smoothed)
blue_band_df <- spec %>%
  dplyr::filter(wl >= blue_band[1], wl <= blue_band[2]) %>%
  mutate(intensity_sg = pmax(intensity_sg, 0))  # avoid negative fill

green_band_df <- spec %>%
  dplyr::filter(wl >= green_band[1], wl <= green_band[2]) %>%
  mutate(intensity_sg = pmax(intensity_sg, 0))

# Plot spectrum with different GP calculation methods visualized
p_spec <- ggplot(spec, aes(wl)) +
  
  # ---- band-integrated areas ----
geom_area(
  data = blue_band_df,
  aes(y = intensity_sg, fill = "Blue band"),
  alpha = 0.35
) +
  geom_area(
    data = green_band_df,
    aes(y = intensity_sg, fill = "Green band"),
    alpha = 0.35
  ) +
  
  # ---- spectra ----
geom_line(
  aes(y = intensity, color = "Raw"),
  alpha = 0.4, linewidth = 0.6
) +
  geom_line(
    aes(y = intensity_sg, color = "SG-smoothed"),
    linewidth = 0.8
  ) +
  
  # ---- exact GP wavelengths ----
geom_vline(
  aes(xintercept = wl_blue, linetype = "Exact 440/490"),
  color = "gray40"
) +
  geom_vline(
    aes(xintercept = wl_green, linetype = "Exact 440/490"),
    color = "gray40"
  ) +
  
  # ---- sg_peaks wavelengths ----
geom_vline(
  aes(xintercept = blue_peak_wl, linetype = "SG peaks"),
  color = "navy"
) +
  geom_vline(
    aes(xintercept = green_peak_wl, linetype = "SG peaks"),
    color = "navy"
  ) +
  
  # ---- manual legend control ----
scale_color_manual(
  name = NULL,
  values = c(
    "Raw" = "black",
    "SG-smoothed" = "red"
  )
) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Blue band" = "steelblue",
      "Green band" = "seagreen"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Exact 440/490" = "dashed",
      "SG peaks" = "dashed"
    )
  ) +
  
  theme_bw() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.2, "lines")
  ) +
  labs(
    title = paste0(sample_name, " spectrum"),
    subtitle = paste0(
      "Exact = ", round(GP_exact_raw, 3),
      " | sg_exact = ", round(GP_sg_exact, 3),
      " | sg_peaks = ", round(GP_sg_peaks, 3),
      " | raw_band-mean = ", round(GP_band_mean_raw, 3),
      " | sg_band-mean = ", round(GP_band_mean_sg, 3),
      " | sg_band-int = ", round(GP_band_int_norm_sg, 3)
    ),
    x = "Wavelength (nm)",
    y = "Intensity"
  ) +
  theme(
    plot.title = element_text(size = 18, colour = "black", face = "bold"),
    plot.subtitle = element_text(size = 9, colour = "grey30"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15)
  ) +
  scale_y_continuous(limits=c(-25, 2000)) +
  scale_x_continuous(limits=c(400, 600))

print(p_spec)

p_spec_pdf  <- paste0(sample_name, "_spectrum.pdf")
ggsave(p_spec_pdf, width = 9, height = 6, dpi = 300)


