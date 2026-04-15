#!/usr/bin/env Rscript

# ──────────────────────────────────────────────────────────────────────────
# LAURDAN GP VISUALIZATION OVER TIME

# This script can be used to visualize results from C-Laurdan spectra captured
# from Cubette experiments by plotting C-Laurdan GP over time and annotating
# points with that step's pressure/temperature.

# - Subtracts no-dye ASW-only blanks from spectra before GP calculation
#     - Auto-detects blank spectrum file
# - Computes GP using 4 different methods
#     1) Exact 440/490nm computation
#     2) Applies SG smoothing filter, then 440/490nm computation
#     3) Applies SG smoothing filter, then calculates weighted band average over blue/green range
#     4) Applies SG smoothing filter, then auto-detects blue/green peaks
# - Supports single-measurement and triplicate averaging
#     - Groups *every 3 files in time order* into a "step"
#     - Computes mean GP when taking triplicate measurements
# - Optional fixed GP axis limits

# Edited: 12/17/2025
# ──────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(ggrepel)
  library(readr)
  library(signal)
})

# ============================================================
# PARAMETERS TO EDIT
# ============================================================

data_dir   <- "."
sample_name <- "Redcydip_D1369d10_041326"
time_unit  <- "mins"

subtract_blank <- TRUE

# Measurements per step
n_measurements_per_step <- 3    # 1 or 3

# GP method options:
# - "exact"    = exact 440/490 (raw, standardized spectroscopy GP)
# - "sg_exact" = SG-smoothed 440/490 (apply Savitzky–Golay smoothing filter)
# - "sg_band"   = SG-smoothed Δλ-weighted band GP (normalized integration) - USE THIS ONE
# - "sg_peaks" = SG-smoothed peak-based GP 
GP_method <- "sg_band"

# Savitzky–Golay parameters (used only if GP_method == "sg_exact")
sg_poly_order <- 3       # polynomial order
sg_window     <- 101     # window length (will be forced to odd)

# Band-averaged GP windows (used only if GP_method == "sg_band")
blue_range  <- c(420, 460)
green_range <- c(470, 510)

# Peak search windows (used only if GP_method == "sg_peaks)
blue_peak_range  <- c(430, 450)
green_peak_range <- c(485, 505)

# Spectral filters
min_wl       <- 390
thres_intens <- 1

# Optional fixed GP axis scale, set to NA if don't want this
GP_ymin <- -0.6
GP_ymax <- 0.1
#GP_ymin <- NA
#GP_ymax <- NA

# Tags for file labels
scale_tag <- if (is.na(GP_ymin) || is.na(GP_ymax)) {
  "autoscale"
} else {
  paste0("scale_", GP_ymin, "_", GP_ymax)
}
method_tag <- paste(GP_method, scale_tag, sep = "_")

# ============================================================
# AUTO-DETECT BLANK FILE
# ============================================================

blank_candidates <- list.files(
  path = data_dir,
  pattern = "^blank_.*\\.tsv$",
  full.names = TRUE
)

if (length(blank_candidates) == 0)
  stop("No blank_*.tsv file found in ", data_dir)

# Pick the *most recently modified* blank
blank_file <- blank_candidates[which.max(file.info(blank_candidates)$mtime)]

message("Using blank file: ", basename(blank_file))

blank <- read_tsv(blank_file, col_types = cols()) %>%
  mutate(
    Wavelength = as.numeric(Wavelength),
    Intensity  = as.numeric(Intensity)
  )

# ============================================================
# BLANK SUBTRACTION
# ============================================================

subtract_blank_if_enabled <- function(spec) {
  
  spec <- spec %>%
    mutate(
      Wavelength = as.numeric(Wavelength),
      Intensity  = as.numeric(Intensity)
    )
  
  if (!subtract_blank) return(spec)
  
  blank_interp <- approx(
    x    = blank$Wavelength,
    y    = blank$Intensity,
    xout = spec$Wavelength,
    rule = 2
  )$y
  
  spec$Intensity <- spec$Intensity - blank_interp
  spec
}

# ============================================================
# GP CALCULATION
# ============================================================

# Apply trapezoid integration for band-average calculation because spectra spaced by 1nm exactly
trapz <- function(x, y) {
  idx <- order(x)
  x <- x[idx]; y <- y[idx]
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

# Function to calculate weighted band-average GP (i.e., normalized to band width)
band_weighted_mean <- function(w, y, rng) {
  idx <- which(w >= rng[1] & w <= rng[2])
  if (length(idx) < 2) return(NA_real_)
  trapz(w[idx], y[idx]) / diff(rng)
}

compute_gp <- function(df) {
  
  df <- df %>% dplyr::filter(Wavelength >= min_wl, Intensity >= thres_intens)
  if (nrow(df) == 0) return(NA_real_)
  
  w <- df$Wavelength
  y <- df$Intensity
  
  # ---------------------------------------------------------
  # Exact 440/490 calculation
  # ---------------------------------------------------------
  
  if (GP_method == "exact") {
    i440 <- approx(w, y, xout = 440, rule = 2)$y
    i490 <- approx(w, y, xout = 490, rule = 2)$y
    if (!is.finite(i440 + i490) || (i440 + i490) == 0) return(NA_real_)
    return((i440 - i490) / (i440 + i490))
  }
  
  # ---------------------------------------------------------
  # Methods that REQUIRE SG smoothing
  # ---------------------------------------------------------
  
  if (GP_method %in% c("sg_exact", "sg_peaks", "sg_band")) {
    
    win <- ifelse(sg_window %% 2 == 0, sg_window + 1, sg_window)
    
    y_smooth <- tryCatch(
      signal::sgolayfilt(y, p = sg_poly_order, n = win),
      error = function(e) rep(NA_real_, length(y))
    )
    
    if (all(is.na(y_smooth))) return(NA_real_)
  }
  
  # ---------------------------------------------------------
  # SG + fixed wavelengths
  # ---------------------------------------------------------
  
  if (GP_method == "sg_exact") {
    i440 <- approx(w, y_smooth, xout = 440, rule = 2)$y
    i490 <- approx(w, y_smooth, xout = 490, rule = 2)$y
    if (!is.finite(i440 + i490) || (i440 + i490) == 0) return(NA_real_)
    return((i440 - i490) / (i440 + i490))
  }
  
  # ---------------------------------------------------------
  # SG + automatic peak detection
  # ---------------------------------------------------------
  
  if (GP_method == "sg_peaks") {
    
    blue_idx  <- which(w >= blue_peak_range[1]  & w <= blue_peak_range[2])
    green_idx <- which(w >= green_peak_range[1] & w <= green_peak_range[2])
    
    if (length(blue_idx) == 0 || length(green_idx) == 0)
      return(NA_real_)
    
    ib <- max(y_smooth[blue_idx],  na.rm = TRUE)
    ig <- max(y_smooth[green_idx], na.rm = TRUE)
    
    if (!is.finite(ib + ig) || (ib + ig) == 0) return(NA_real_)
    
    return((ib - ig) / (ib + ig))
  }
  
  # ---------------------------------------------------------
  # SG + wavelength-weighted band average
  # ---------------------------------------------------------
  
  if (GP_method == "sg_band") {
    
    # SG smoothing required for band GP
    win <- ifelse(sg_window %% 2 == 0, sg_window + 1, sg_window)
    
    y_smooth <- tryCatch(
      signal::sgolayfilt(y, p = sg_poly_order, n = win),
      error = function(e) rep(NA_real_, length(y))
    )
    
    if (all(is.na(y_smooth))) return(NA_real_)
    
    ib <- band_weighted_mean(w, y_smooth, blue_range)
    ig <- band_weighted_mean(w, y_smooth, green_range)
    
    if (!is.finite(ib + ig) || (ib + ig) == 0) return(NA_real_)
    return((ib - ig) / (ib + ig))
  }
  
  stop("Unknown GP_method")
}


# ============================================================
# LOAD SPECTRA
# ============================================================

files <- list.files(
  path = data_dir,
  pattern = "^spectrum_.*\\.tsv$",
  full.names = TRUE
)

if (length(files) == 0)
  stop("No spectrum_*.tsv files found in ", data_dir)

rows <- map_dfr(seq_along(files), function(i) {
  
  f <- files[i]
  spec_raw  <- read_tsv(f, col_types = cols())
  spec_corr <- subtract_blank_if_enabled(spec_raw)
  gp_val    <- compute_gp(spec_corr)
  
  # Extract metadata from file name
  m <- str_match(
    basename(f),
    "^spectrum_(\\d{8}T\\d+)_idx(\\d+)_P(-?[0-9]+)_T([0-9.]+)\\.tsv$"
  )
  
  # Timestamp uses first 15 chars YYYYMMDDThhmmss
  ts <- suppressWarnings(
    ymd_hms(
      str_replace(str_sub(m[1,2], 1, 15), "T", " "),
      tz = "UTC"
    )
  )
  
  tibble(
    file       = f,
    file_index = i,
    Timestamp  = ts,
    idx        = as.integer(m[1,3]),
    Press_psi  = as.numeric(m[1,4]),
    Temp       = as.numeric(m[1,5]),
    GP         = gp_val
  )
}) %>%
  arrange(Timestamp) %>%
  mutate(
    time_min = as.numeric(difftime(Timestamp, min(Timestamp), units = time_unit))
  )

# ============================================================
# GROUP INTO STEPS (triplicates) → data_plot
# ============================================================

if (n_measurements_per_step == 3) {
  
  rows <- rows %>%
    arrange(file_index) %>%
    mutate(step_id = ceiling(row_number() / 3))
  
  data_plot <- rows %>%
    group_by(step_id) %>%
    summarise(
      n         = sum(is.finite(GP)),
      GP_sd     = sd(GP, na.rm = TRUE),
      GP        = mean(GP, na.rm = TRUE),
      Timestamp = as_datetime(mean(as.numeric(Timestamp), na.rm = TRUE), tz = "UTC"),
      Press_psi = mean(Press_psi, na.rm = TRUE),
      Temp      = mean(Temp, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(
      GP_se    = GP_sd / sqrt(n),
      time_min = as.numeric(difftime(Timestamp, min(Timestamp), units = time_unit)),
      Press_bar = Press_psi / 14.5038
    )
  
} else {
  
  data_plot <- rows %>%
    mutate(
      step_id   = row_number(),
      n         = 1L,
      GP_sd     = NA_real_,
      GP_se     = NA_real_,
      Press_bar = Press_psi / 14.5038
    )
}

# ============================================================
# GP vs TIME plot
# ============================================================

p_gp <- ggplot(data_plot, aes(x = time_min, y = GP, color = Temp)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 2.8) +
  {
    if (any(is.finite(data_plot$GP_se))) {
      geom_errorbar(aes(ymin = GP - GP_se, ymax = GP + GP_se),
                    width = 0.3, alpha = 0.6, color = "black")
    }
  } +
  geom_text_repel(
    aes(label = paste0(round(Press_bar), " bar")),
    size = 3,
    max.overlaps = 30
  ) +
  scale_color_gradient(low = "blue", high = "red", name = "Temp (°C)", limits=c(0, 25)) +
  scale_y_continuous(limits = c(GP_ymin, GP_ymax)) +
  scale_x_continuous(limits = c(0, 330)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20, colour = "black", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, colour = "grey30", hjust = 0.5),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 18),
    axis.text.y  = element_text(size = 18)
  ) +
  labs(
    title = paste0(sample_name),
    subtitle = paste(
      ifelse(subtract_blank, "blank-subtracted", "raw"),
      "| GP method:", GP_method,
      "| n =", n_measurements_per_step, "per step"
    ),
    x = paste0("Time (", time_unit, ")"),
    y = "Laurdan GP"
  )
# ============================================================
# Pressure/Temperature profile
# ============================================================

# press_range <- range(data_plot$Press_bar, na.rm = TRUE)
# temp_range  <- range(data_plot$Temp, na.rm = TRUE)
# scale_factor <- diff(press_range) / diff(temp_range)
# 
# p_pt <- ggplot(data_plot, aes(x = time_min)) +
#   geom_step(aes(y = Press_bar, color = "Pressure"), linewidth = 1) +
#   geom_point(aes(y = Press_bar, color = "Pressure"), size = 2) +
#   geom_line(
#     aes(y = Temp * scale_factor, color = "Temperature"),
#     linetype = "dashed", linewidth = 1
#   ) +
#   geom_point(
#     aes(y = Temp * scale_factor, color = "Temperature"),
#     shape = 17, size = 2
#   ) +
#   scale_y_continuous(
#     name = "Pressure (bar)",
#     sec.axis = sec_axis(~ . / scale_factor, name = "Temperature (°C)")
#   ) +
#   scale_color_manual(values = c("Pressure" = "red", "Temperature" = "orange")) +
#   theme_bw() +
#   labs(
#     title = paste0(sample_name, " — Pressure & Temperature vs Time"),
#     x = paste0("Time (", time_unit, ")")
#   ) +
#   theme(legend.position = "bottom")

# ============================================================
# SAVE OUTPUTS
# ============================================================

print(p_gp)
# print(p_pt)

gp_pdf  <- paste0(sample_name, "_GP_over_time_", method_tag, ".pdf")
#pt_pdf  <- paste0(sample_name, "_PT_profile.pdf")
gp_csv  <- paste0(sample_name, "_GP_values_", GP_method, ".csv")

ggsave(gp_pdf, p_gp, width = 9, height = 6, dpi = 300)
#ggsave(pt_pdf, p_pt, width = 9, height = 4, dpi = 300)
write_csv(rows, gp_csv)

#message("Saved outputs: ", gp_pdf, ", ", gp_csv)
