#!/usr/bin/env Rscript

# ──────────────────────────────────────────────────────────────────────────
# LAURDAN GP METHOD COMPARISON (SINGLE SPECTRUM)
#
# - Tests all GP calculation methods used in pipeline
# - Optional blank subtraction
# - Prints plot of spectrum with different GP calculations visualized
# - Outputs a summary table in the console to compare GP values for each method
#
# Methods compared:
#   1) raw_exact (440/490): calculates GP from raw intensity values at 440nm and 490nm
#   2) raw_band_mean: calculates GP from average intensity under the curve in the blue and green band from the raw spectrum
#   3) sg_exact (440/490): applies SG filter and calculates GP from raw intensity values at 440nm and 490nm
#   4) sg_peaks (auto-detect): applies SG filter, auto-detects intensity maxima within green/blue range for GP calculation
#   5) sg_band_mean (normalized): applies SG filter and calculates GP from average intensity under the curve in the blue and green band 
#   6) sg_band_integrated (un-normalized): applies SG filter and calculates GP from raw integrated area under the curve in the blue and green band (not normalized to band width)
#   7) lognormal decomposition: fits 2- or 3-peak asymmetric Siano-Metzler log-normal functions and calculates GP from the ratio of integrated areas un the blue and green peaks
#         - Bacalum et al. (2013) and Mangiarotti & Bagatolli (2020)
#
# Edited: 04/17/2026
# ──────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(signal)
  library(minpack.lm)   # nls with Levenberg-Marquardt for lognormal fit
})

# ============================================================
# PARAMETERS
# ============================================================
sample_name <- "Redcydip_D1369d10_041326_start"  # Change sample name/date, which spectra
file <- "Redcydip_D1369d10_041326_start.tsv"     # Change file name
data_dir <- "."                                                # Move to directory with experiment + blank spectra

# Blank subtraction?
subtract_blank <- TRUE

# Parameters for Savitzgy-Golay (SG) smoothing filter
sg_poly_order <- 3 # cubic polynomial (can capture curves with 1 inflection point)
sg_window     <- 101 # fits cubic across 101 nm stretch at each point, this value must be odd

# Fixed wavelengths for blue and green peaks
wl_blue  <- 440
wl_green <- 490

# Band definitions for blue and green range
blue_band  <- c(420, 460)
green_band <- c(470, 510)

# Peak search windows (for automatic peak detection with sg_peaks)
blue_peak_range  <- c(430, 450)
green_peak_range <- c(485, 505)

# ============================================================
# PARAMETERS FOR LN DECOMPOSITION
# ============================================================

# Lognormal decomposition fit range (nm) — covers both Laurdan peaks
lognorm_fit_range <- c(400, 560)

# Number of log-normal peaks to fit:
#   2 = blue + green only (Bacalum et al. 2013 original method)
#   3 = blue + intermediate + green (better for spectra with a shoulder ~455-465 nm)
# GP is always computed from blue and green peak areas only.
lognorm_n_peaks <- 2

# Siano-Metzler log-normal fit (in wavenumber (i.e., cm-1), not nm!)
# Each peak: I0 (height), nu0 (peak position), H (full width at 1/2 maximum of peak), rho (asymmetry)
# 440 nm ≈ 22727 cm-1 (blue); 462 nm ≈ 21645 cm-1 (intermediate); 490 nm ≈ 20408 cm-1 (green)
# rho ~1.3 = slightly right skew (typical for Laurdan emission)

lognorm_start_2peak <- list(
  I0_b  = 300,  nu0_b = 22727, H_b = 1800, rho_b = 1.3,
  I0_g  = 800,   nu0_g = 20408, H_g = 1800, rho_g = 1.3
)
lognorm_start_3peak <- list(
  I0_b  = 300,   nu0_b = 22727, H_b = 1500, rho_b = 1.3,
  I0_m  = 200,   nu0_m = 21645, H_m = 1500, rho_m = 1.3,
  I0_g  = 800,   nu0_g = 20408, H_g = 1500, rho_g = 1.3
)

# ============================================================
# HELPERS
# ============================================================

# Calculate area under curve by dividing it into trapezoids
trapz <- function(x, y) {
  idx <- order(x)
  x <- x[idx]; y <- y[idx]
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

# Function to calculate GP using band-averaged method (weighted so points spanning larger wavelength contribute more)

# First calculate mean intensity across a wavelength range (band) - normalized by band width
band_weighted_mean <- function(w, y, rng) {
  idx <- which(w >= rng[1] & w <= rng[2])
  if (length(idx) < 2) return(NA_real_)
  area <- trapz(w[idx], y[idx])
  width <- diff(rng)
  area / width
}
# Then use that to calculate mean intensity in BLUE + GREEN to calculate GP
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

# Siano-Metzler asymmetric log-normal function (wavenumber domain)
# Parameters:
#   I0   : peak height
#   nu0  : peak position (cm-1)
#   H    : full width at half maximum (cm-1)
#   rho  : asymmetry parameter (rho > 1 → right-skewed; rho = 1 → Gaussian limit)
#   nu   : wavenumber vector (cm-1)
#
# Formula (Siano & Metzler 1969, as used in Bacalum et al. 2013):
#   alogn = I0 * exp( -(ln2 / ln(rho)^2) * ln( ((nu-nu0)/H) * ((rho^2-1)/rho) + 1 )^2 )
# The argument of the inner ln must be > 0; outside that range intensity = 0.
alogn <- function(nu, I0, nu0, H, rho) {
  arg <- ((nu - nu0) / H) * ((rho^2 - 1) / rho) + 1
  out <- numeric(length(nu))
  valid <- arg > 0
  out[valid] <- I0 * exp(-(log(2) / log(rho)^2) * log(arg[valid])^2)
  out
}

# Fit two or three Siano-Metzler log-normal peaks to a Laurdan spectrum and
# return GP computed from the ratio of blue vs green fitted peak areas.
# The intermediate peak (3-peak mode) is used only to improve fit quality;
# it does not contribute to the GP calculation.
# Returns a list: GP value, fit object, and component curves for plotting.
gp_lognormal_fit <- function(w_nm, y_int, n_peaks = lognorm_n_peaks,
                             fit_range_nm = lognorm_fit_range) {
  
  # Restrict to fit window
  idx <- which(w_nm >= fit_range_nm[1] & w_nm <= fit_range_nm[2] & is.finite(y_int))
  if (length(idx) < 20) return(list(GP = NA_real_, fit = NULL))
  
  w_sub  <- w_nm[idx]
  y_sub  <- y_int[idx]
  
  # Convert wavelength (nm) → wavenumber (cm-1)
  nu_sub <- 1e7 / w_sub
  
  # ── 2-peak model ──────────────────────────────────────────────────────────
  if (n_peaks == 2) {
    
    model_fn <- function(nu, I0_b, nu0_b, H_b, rho_b,
                         I0_g, nu0_g, H_g, rho_g) {
      alogn(nu, I0_b, nu0_b, H_b, rho_b) +
        alogn(nu, I0_g, nu0_g, H_g, rho_g)
    }
    
    fit <- tryCatch(
      nlsLM(
        y_sub ~ model_fn(nu_sub, I0_b, nu0_b, H_b, rho_b,
                         I0_g, nu0_g, H_g, rho_g),
        start = lognorm_start_2peak,
        # Set limits for parameters (green between 430-460nm, green between 480-510nm)
        lower = c(0,  21739, 500, 1.01,   0,  19608, 500, 1.01), 
        upper = c(Inf, 23256, 4000, 5,   Inf, 20833, 4000, 5),
        control = nls.lm.control(maxiter = 200)
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit)) return(list(GP = NA_real_, fit = NULL))
    p <- coef(fit)
    
    nu_grid <- seq(16000, 26000, by = 5)
    area_b  <- trapz(nu_grid, alogn(nu_grid, p["I0_b"], p["nu0_b"], p["H_b"], p["rho_b"]))
    area_g  <- trapz(nu_grid, alogn(nu_grid, p["I0_g"], p["nu0_g"], p["H_g"], p["rho_g"]))
    
    if (!is.finite(area_b + area_g) || (area_b + area_g) == 0)
      return(list(GP = NA_real_, fit = fit))
    
    return(list(
      GP = (area_b - area_g) / (area_b + area_g),
      fit = fit, coef = p, n_peaks = 2,
      area_b = area_b, area_g = area_g,
      nu_sub = nu_sub, y_sub = y_sub
    ))
  }
  
  # ── 3-peak model ──────────────────────────────────────────────────────────
  if (n_peaks == 3) {
    
    model_fn <- function(nu, I0_b, nu0_b, H_b, rho_b,
                         I0_m, nu0_m, H_m, rho_m,
                         I0_g, nu0_g, H_g, rho_g) {
      alogn(nu, I0_b, nu0_b, H_b, rho_b) +
        alogn(nu, I0_m, nu0_m, H_m, rho_m) +
        alogn(nu, I0_g, nu0_g, H_g, rho_g)
    }
    
    fit <- tryCatch(
      nlsLM(
        y_sub ~ model_fn(nu_sub, I0_b, nu0_b, H_b, rho_b,
                         I0_m, nu0_m, H_m, rho_m,
                         I0_g, nu0_g, H_g, rho_g),
        start = lognorm_start_3peak,
        # Set limits for parameters (green between 430-460nm, green between 480-510nm, middle between 460-480)
        lower = c(0,  21739, 300, 1.01,   0,  20833, 300, 1.01,   0,  19608, 300, 1.01), 
        upper = c(Inf, 23256, 4000, 5,   Inf, 21739, 4000, 5,   Inf, 20833, 4000, 5),
        control = nls.lm.control(maxiter = 200)
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit)) return(list(GP = NA_real_, fit = NULL))
    p <- coef(fit)
    
    nu_grid <- seq(16000, 26000, by = 5)
    area_b  <- trapz(nu_grid, alogn(nu_grid, p["I0_b"], p["nu0_b"], p["H_b"], p["rho_b"]))
    area_g  <- trapz(nu_grid, alogn(nu_grid, p["I0_g"], p["nu0_g"], p["H_g"], p["rho_g"]))
    
    if (!is.finite(area_b + area_g) || (area_b + area_g) == 0)
      return(list(GP = NA_real_, fit = fit))
    
    return(list(
      GP = (area_b - area_g) / (area_b + area_g),
      fit = fit, coef = p, n_peaks = 3,
      area_b = area_b, area_g = area_g,
      nu_sub = nu_sub, y_sub = y_sub
    ))
  }
  
  stop("lognorm_n_peaks must be 2 or 3")
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
GP_raw_exact <- (i440_raw - i490_raw) / (i440_raw + i490_raw)

# sg_exact
i440_sg <- approx(spec$wl, spec$intensity_sg, xout = wl_blue)$y
i490_sg <- approx(spec$wl, spec$intensity_sg, xout = wl_green)$y
GP_sg_exact <- (i440_sg - i490_sg) / (i440_sg + i490_sg)

# band mean (raw and SG)
GP_raw_band_mean <- gp_band_weighted(spec$wl, spec$intensity,    blue_band, green_band)
GP_sg_band_mean  <- gp_band_weighted(spec$wl, spec$intensity_sg, blue_band, green_band)

# band integrated - use normalized method only, not the un-normalized ones 
# GP_band_int_raw <- gp_band_integrated(spec$wl, spec$intensity,    blue_band, green_band)
# GP_band_int_sg  <- gp_band_integrated(spec$wl, spec$intensity_sg, blue_band, green_band)
GP_sg_band_int <- gp_band_int_norm(spec$wl, spec$intensity_sg, blue_band, green_band)

# sg_peaks (auto-detection of peaks in SG)
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

# lognormal decomposition (Bacalum et al. 2013 — two-peak Siano-Metzler fit)
ln_result   <- gp_lognormal_fit(spec$wl, spec$intensity)
GP_lognorm  <- ln_result$GP

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
    legend.position  = "right",
    legend.text      = element_text(size = 10),
    legend.key.width = unit(1.2, "lines"),
    plot.caption     = element_text(
      family = "mono", size = 9, color = "gray30",
      hjust = 1, margin = margin(t = 6)
    )
  ) +
  labs(
    title   = paste0(sample_name, " spectrum"),
    x       = "Wavelength (nm)",
    y       = "Intensity",
    caption = paste0(
      "Exact: ",      round(GP_raw_exact,       3), "   ",
      "SG exact: ",   round(GP_sg_exact,         3), "   ",
      "SG peaks: ",   round(GP_sg_peaks,         3), "\n",
      "Band mean: ",  round(GP_sg_band_mean,     3), "   ",
      "Band int: ",   round(GP_sg_band_int, 3), "   ",
      "Log-normal: ", round(GP_lognorm,          3)
    )
  ) +
  theme(
    plot.title = element_text(size = 18, colour = "black", face = "bold"),
    plot.subtitle = element_text(size = 9, colour = "grey30"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15)
  ) +
  scale_y_continuous(limits = c(-25, 2000)) +
  scale_x_continuous(limits = c(400, 600))

print(p_spec)

p_spec_pdf  <- paste0(sample_name, "_spectrum.pdf")
#ggsave(p_spec_pdf, width = 9, height = 6, dpi = 300)

# ============================================================
# LOGNORMAL FIT DIAGNOSTIC PLOT
# ============================================================

if (!is.null(ln_result$fit)) {
  
  p <- ln_result$coef
  nu_plot <- seq(16000, 26000, by = 10)
  
  # Build component curves in wavelength space
  ln_fit_df <- tibble(
    nu    = nu_plot,
    wl    = 1e7 / nu_plot,
    blue  = alogn(nu_plot, p["I0_b"], p["nu0_b"], p["H_b"], p["rho_b"]),
    green = alogn(nu_plot, p["I0_g"], p["nu0_g"], p["H_g"], p["rho_g"]),
    mid   = if (ln_result$n_peaks == 3)
      alogn(nu_plot, p["I0_m"], p["nu0_m"], p["H_m"], p["rho_m"])
    else
      rep(0, length(nu_plot)),
    total = blue + mid + green
  ) %>%
    dplyr::filter(wl >= 400, wl <= 600)
  
  # Raw data in fit window
  raw_fit_df <- spec %>%
    dplyr::filter(wl >= 400, wl <= 600)
  
  # Colour/linetype values — add intermediate peak only if 3-peak fit
  color_vals <- c(
    "Raw data"   = "gray50",
    "Total fit"  = "black",
    "Blue peak"  = "steelblue",
    "Green peak" = "seagreen"
  )
  ltype_vals <- c(
    "Raw data"   = "solid",
    "Total fit"  = "solid",
    "Blue peak"  = "dashed",
    "Green peak" = "dashed"
  )
  if (ln_result$n_peaks == 3) {
    color_vals <- c(color_vals, "Mid peak" = "darkorchid")
    ltype_vals <- c(ltype_vals, "Mid peak" = "dashed")
  }
  
  # Plot the fit
  p_ln <- ggplot() +
    geom_line(data = raw_fit_df,
              aes(wl, intensity, color = "Raw data", linetype = "Raw data"),
              linewidth = 0.7, alpha = 0.7) +
    geom_line(data = ln_fit_df,
              aes(wl, total,  color = "Total fit",  linetype = "Total fit"),
              linewidth = 1.0) +
    geom_line(data = ln_fit_df,
              aes(wl, blue,   color = "Blue peak",  linetype = "Blue peak"),
              linewidth = 0.9) +
    geom_line(data = ln_fit_df,
              aes(wl, green,  color = "Green peak", linetype = "Green peak"),
              linewidth = 0.9) +
    { if (ln_result$n_peaks == 3)
      geom_line(data = ln_fit_df,
                aes(wl, mid, color = "Mid peak", linetype = "Mid peak"),
                linewidth = 0.9)
    } +
    scale_color_manual(name = NULL, values = color_vals) +
    scale_linetype_manual(name = NULL, values = ltype_vals) +
    theme_bw() +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 9,  colour = "grey30"),
      axis.title    = element_text(size = 13),
      axis.text     = element_text(size = 12),
      legend.position = "right"
    ) +
    labs(
      title    = paste0(sample_name, " - LN decomposition (",
                        ln_result$n_peaks, " peaks)"),
      subtitle = paste0(
        "Blue: ", round(1e7 / p["nu0_b"], 1), " nm",
        if (ln_result$n_peaks == 3)
          paste0("  |  Mid: ", round(1e7 / p["nu0_m"], 1), " nm"),
        "  |  Green: ", round(1e7 / p["nu0_g"], 1), " nm"
      ),
      x = "Wavelength (nm)",
      y = "Intensity"
    ) +
    scale_x_continuous(limits = c(400, 600))
  
  print(p_ln)
  ln_pdf <- paste0(sample_name, "_", lognorm_n_peaks, "peaks_lognorm_fit.pdf")
  ggsave(ln_pdf, p_ln, width = 8, height = 5, dpi = 300)
  message("Lognormal fit saved to: ", ln_pdf)
  
} else {
  message("Lognormal fit did not converge — plotting raw spectrum only.")
  
  raw_fit_df <- spec %>%
    dplyr::filter(wl >= 400, wl <= 600)
  
  p_ln <- ggplot() +
    geom_line(data = raw_fit_df,
              aes(wl, intensity),
              color = "gray50", linewidth = 0.7, alpha = 0.7) +
    theme_bw() +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "red"),
      axis.title    = element_text(size = 13),
      axis.text     = element_text(size = 12),
      legend.position = "right"
    ) +
    labs(
      title    = paste0(sample_name, " — Log-normal decomposition (",
                        lognorm_n_peaks, " peaks)"),
      subtitle = "Log-normal fit did not converge — no decomposition available",
      x = "Wavelength (nm)",
      y = "Intensity"
    ) +
    scale_x_continuous(limits = c(400, 600))
  
  print(p_ln)
  ln_pdf <- paste0(sample_name, "_lognorm_fit.pdf")
  ggsave(ln_pdf, p_ln, width = 8, height = 5, dpi = 300)
  message("Raw spectrum plot saved to: ", ln_pdf)
}
# ============================================================
# SUMMARY TABLE
# ============================================================

gp_summary <- tibble(
  Method = c(
    "raw exact (440/490)",
    "raw band mean",
    "sg exact (440/490)",
    "sg peaks (auto-detect)",
    "sg band mean (normalized)",
    "sg band integrated (un-normalized)",
    "lognormal decomposition"
  ),
  GP = c(
    GP_raw_exact,
    GP_raw_band_mean,
    GP_sg_exact,
    GP_sg_peaks,
    GP_sg_band_mean,
    GP_sg_band_int,
    GP_lognorm
  )
)

print(gp_summary)
