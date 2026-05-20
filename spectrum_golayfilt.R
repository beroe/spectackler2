#!/usr/bin/env Rscript

# ──────────────────────────────────────────────────────────────────────────
# LAURDAN GP: SPECTRUM DIAGNOSTIC
#
# Diagnostic script for inspecting individual C-Laurdan emission spectra and
# comparing GP calculation methods across a pressure/temperature experiment.
# Unlike the pipeline scripts (gp_over_time.R, gp_landscape.R), this script
# does NOT average spectra — each file is plotted individually.
#
# WHAT IT DOES:
#   - Loads individual spectrum .tsv files from data/raw/<experiment>/
#   - Optionally subtracts a blank (blank_*.tsv auto-detected in same folder)
#   - Applies Savitzky-Golay smoothing (configurable poly order + window)
#   - Computes all 7 GP methods for each spectrum and prints to console
#   - Produces a multi-page PDF per sample; y-axis is fixed across all pages
#     within a sample for easy scrolling comparison
#
# TWO PLOTTING MODES (set plot_all_steps):
#   FALSE (default) : plots only the first and last spectrum of the experiment
#   TRUE            : one page per individual spectrum file; output goes to
#                     figures/spectra/all_spectra/
#
# TWO PLOT TYPES (independently toggled):
#   show_method_comparison = TRUE  (default): plots the raw + SG-smoothed
#     spectrum with band regions shaded and vertical lines marking the blue/
#     green wavelengths for all 6 non-LN methods; GP values in caption
#   show_ln_decomposition  = FALSE (default): adds a second plot showing the
#     Siano-Metzler log-normal decomposition with fitted blue/green (and
#     optional mid) peak components overlaid; output goes to ln_decomp/
#     subdirectory (ln_decomp/all_spectra/ if plot_all_steps = TRUE).
#     Available but not recommended for per-spectrum pipeline use
#     (high step-to-step noise; see README)
#
# OUTPUT DIRECTORIES (all under figures/spectra/):
#   show_method_comparison only  →  figures/spectra/
#   + plot_all_steps             →  figures/spectra/all_spectra/
#   + show_ln_decomposition      →  figures/spectra/ln_decomp/
#   + both                       →  figures/spectra/ln_decomp/all_spectra/
#
# Run from the project root:
#   Rscript scripts/spectrum_golayfilt.R
#
# Edited: 05/15/2026
# ──────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(signal)
  library(minpack.lm)
})

# ============================================================
# SAMPLES: Which samples to plot spectra for
# ============================================================
run_all_samples <- FALSE   # TRUE: auto-detect all experiment folders in raw_data_root
samples <- c(              # ignored when run_all_samples = TRUE
  "Bolinopsis_D1339bw9_wproti_040826"
)

# ============================================================
# PATHS
# ============================================================
raw_data_root <- "data/raw"        # root folder containing experiment subfolders
output_dir    <- "figures/spectra" # where output PDFs are saved

# ============================================================
# OPTIONS
# ============================================================
show_method_comparison <- TRUE   # TRUE: plot spectrum with all 6 GP methods annotated (default)
show_ln_decomposition  <- FALSE  # TRUE: also plot LN decomposition with fitted peak components
plot_all_steps         <- FALSE   # TRUE: one page per triplicate step (multi-page PDF); FALSE: start + end only
subtract_blank         <- TRUE
n_per_step             <- 3      # spectra to average per step

# Y-axis (intensity) limits — three modes, same as gp_scale in gp_cubette.R:
#   "global"     auto-compute from ALL experiments in raw_data_root (default)
#   "auto"       compute per-sample from that sample's files only
#   c(min, max)  fixed manual limits, e.g. c(0, 5000)
#intensity_scale <- "global"
intensity_scale <- c(0, 1500)

# All 7 GP methods are always computed and printed to the console for each condition.
# show_method_comparison (default TRUE) plots the spectrum with all 6 non-LN methods annotated.
# show_ln_decomposition adds the Siano-Metzler decomposition plot with fitted peak components.
#
# GP methods:
#   1. raw exact (440/490)   — raw intensity ratio at exactly 440 nm and 490 nm
#   2. raw band mean         — raw trapezoid-integrated band average, normalized by band width
#   3. SG exact (440/490)    — SG-smoothed intensity ratio at exactly 440 nm and 490 nm
#   4. SG peaks (auto)       — SG-smoothed intensity at auto-detected peak maxima within
#                              blue_peak_range and green_peak_range
#   5. SG band mean          — SG-smoothed weighted band average over blue_band / green_band
#   6. SG band integrated    — SG-smoothed trapezoid-integrated band mean (same as sg_band
#                              in gp_over_time.R and gp_landscape.R)
#   7. log-normal            — Siano-Metzler asymmetric log-normal fit in wavenumber space;
#                              GP from blue:green peak area ratio (see LN DECOMPOSITION PARAMETERS)

# ============================================================
# SAVITZKY-GOLAY FILTER PARAMETERS
# ============================================================
sg_poly_order <- 3
sg_window     <- 101   # forced odd if even

# ============================================================
# BAND / WAVELENGTH DEFINITIONS
# ============================================================
wl_blue  <- 440
wl_green <- 490

blue_band        <- c(420, 460)
green_band       <- c(470, 510)
blue_peak_range  <- c(430, 450)
green_peak_range <- c(485, 505)

# ============================================================
# LN DECOMPOSITION PARAMETERS
# ============================================================

# Fit range (nm) — covers both Laurdan peaks
lognorm_fit_range <- c(410, 560)

# Number of log-normal peaks to fit:
#   2 = blue + green only (Bacalum et al. 2013 original method)
#   3 = blue + intermediate + green (better for spectra with a shoulder ~455-465 nm)
# GP is always computed from blue and green peak areas only.
lognorm_n_peaks <- 3

# Siano-Metzler log-normal fit (in wavenumber (i.e., cm-1), not nm!)
# Each peak: I0 (height), nu0 (peak position), H (full width at 1/2 maximum of peak), rho (asymmetry)
# 440 nm ≈ 22727 cm-1 (blue); 462 nm ≈ 21645 cm-1 (intermediate); 490 nm ≈ 20408 cm-1 (green)
# rho ~1.3 = slightly right skew (typical for Laurdan emission)
lognorm_start_2peak <- list(
  I0_b = 300, nu0_b = 22727, H_b = 1800, rho_b = 1.3,
  I0_g = 800, nu0_g = 20408, H_g = 1800, rho_g = 1.3
)
lognorm_start_3peak <- list(
  I0_b = 300, nu0_b = 22727, H_b = 1500, rho_b = 1.3,
  I0_m = 200, nu0_m = 21645, H_m = 1500, rho_m = 1.3,
  I0_g = 800, nu0_g = 20408, H_g = 1500, rho_g = 1.3
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

# Mean intensity across a wavelength band, normalized by band width
band_weighted_mean <- function(w, y, rng) {
  idx <- which(w >= rng[1] & w <= rng[2])
  if (length(idx) < 2) return(NA_real_)
  trapz(w[idx], y[idx]) / diff(rng)
}

# GP from band-weighted means
gp_band_weighted <- function(w, y, blue_rng, green_rng) {
  ib <- band_weighted_mean(w, y, blue_rng)
  ig <- band_weighted_mean(w, y, green_rng)
  if (!is.finite(ib + ig) || (ib + ig) == 0) return(NA_real_)
  (ib - ig) / (ib + ig)
}

# GP from band-integrated area normalized to band width
gp_band_int_norm <- function(w, y, blue_range, green_range) {
  b <- which(w >= blue_range[1] & w <= blue_range[2])
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
#
# Uses a multi-start strategy: tries several starting configurations and keeps
# the best-converged fit (lowest RSS). Data-driven estimates for peak amplitudes
# and positions are used to generate starting configs that handle weak-blue and
# shoulder spectra where fixed defaults fail.
#
# If all 3-peak starting configs fail, automatically falls back to 2-peak fit
# (flagged in the returned list as fallback_to_2peak = TRUE).
#
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
  
  # ── Data-driven initial estimates ─────────────────────────────────────────
  # Green peak: find max intensity in 480-520 nm (uncontaminated by blue peak)
  g_i <- which(w_sub >= 480 & w_sub <= 520)
  if (length(g_i) >= 2) {
    gm        <- g_i[which.max(y_sub[g_i])]
    I0_g_dat  <- y_sub[gm]
    nu0_g_dat <- max(19608, min(20833, nu_sub[gm]))
  } else {
    I0_g_dat <- 800; nu0_g_dat <- 20408
  }
  # Blue peak amplitude: use intensity at ~440 nm as a conservative estimate.
  # Avoids inflated values when the green tail dominates the blue region
  # in flat/shoulder spectra (e.g., highly fluid membranes).
  b440_i    <- which.min(abs(w_sub - 440))
  I0_b_dat  <- if (length(b440_i) >= 1) y_sub[b440_i] else 300
  nu0_b_dat <- 22727  # always start at 440 nm; data max is unreliable for no-blue spectra
  
  nu_grid <- seq(16000, 26000, by = 5)
  
  # ── 2-peak model ──────────────────────────────────────────────────────────
  if (n_peaks == 2) {
    
    model_fn <- function(nu, I0_b, nu0_b, H_b, rho_b,
                         I0_g, nu0_g, H_g, rho_g) {
      alogn(nu, I0_b, nu0_b, H_b, rho_b) +
        alogn(nu, I0_g, nu0_g, H_g, rho_g)
    }
    
    starts_2 <- list(
      # Data-driven
      list(I0_b = I0_b_dat, nu0_b = nu0_b_dat, H_b = 1800, rho_b = 1.3,
           I0_g = I0_g_dat, nu0_g = nu0_g_dat, H_g = 1800, rho_g = 1.3),
      # Original defaults
      lognorm_start_2peak
    )
    lo_2 <- c(0, 21739, 500, 1.01, 0, 19608, 500, 1.01)
    hi_2 <- c(Inf, 23256, 4000, 5, Inf, 20833, 4000, 5)
    
    fit <- NULL; best_rss <- Inf
    for (s in starts_2) {
      f <- tryCatch(
        nlsLM(y_sub ~ model_fn(nu_sub, I0_b, nu0_b, H_b, rho_b,
                               I0_g, nu0_g, H_g, rho_g),
              start = s, lower = lo_2, upper = hi_2,
              control = nls.lm.control(maxiter = 200)),
        error = function(e) NULL
      )
      if (!is.null(f)) {
        rss <- sum(resid(f)^2)
        if (rss < best_rss) { best_rss <- rss; fit <- f }
      }
    }
    
    if (is.null(fit)) return(list(GP = NA_real_, fit = NULL))
    p <- coef(fit)
    
    area_b <- trapz(nu_grid, alogn(nu_grid, p["I0_b"], p["nu0_b"], p["H_b"], p["rho_b"]))
    area_g <- trapz(nu_grid, alogn(nu_grid, p["I0_g"], p["nu0_g"], p["H_g"], p["rho_g"]))
    
    if (!is.finite(area_b + area_g) || (area_b + area_g) == 0)
      return(list(GP = NA_real_, fit = fit))
    
    return(list(
      GP = (area_b - area_g) / (area_b + area_g),
      fit = fit, coef = p, n_peaks = 2, fallback_to_2peak = FALSE,
      area_b = area_b, area_g = area_g,
      nu_sub = nu_sub, y_sub = y_sub
    ))
  }
  
  # ── 3-peak model with multi-start ─────────────────────────────────────────
  if (n_peaks == 3) {
    
    model_fn <- function(nu, I0_b, nu0_b, H_b, rho_b,
                         I0_m, nu0_m, H_m, rho_m,
                         I0_g, nu0_g, H_g, rho_g) {
      alogn(nu, I0_b, nu0_b, H_b, rho_b) +
        alogn(nu, I0_m, nu0_m, H_m, rho_m) +
        alogn(nu, I0_g, nu0_g, H_g, rho_g)
    }
    
    starts_3 <- list(
      # 1. Data-driven: estimated amplitudes from spectrum, mid at center of its range (~471 nm)
      list(I0_b = I0_b_dat,                nu0_b = nu0_b_dat, H_b = 1500, rho_b = 1.3,
           I0_m = max(I0_b_dat * 0.3, 30), nu0_m = 21200,     H_m = 1200, rho_m = 1.3,
           I0_g = I0_g_dat,                nu0_g = nu0_g_dat,  H_g = 1800, rho_g = 1.3),
      # 2. Original defaults
      lognorm_start_3peak,
      # 3. Weak blue / shoulder: blue and mid both start small relative to green.
      #    Critical for spectra where the blue component is just a slight shoulder or
      #    absent (e.g., highly fluid membranes where the green peak dominates).
      list(I0_b = max(I0_g_dat * 0.05, 15), nu0_b = 22727, H_b = 1200, rho_b = 1.3,
           I0_m = max(I0_g_dat * 0.05, 15), nu0_m = 21500, H_m = 1200, rho_m = 1.3,
           I0_g = I0_g_dat,                  nu0_g = nu0_g_dat, H_g = 1800, rho_g = 1.3),
      # 4. Data-driven with bluer mid (~463 nm), near the upper boundary of the mid range
      list(I0_b = I0_b_dat,                nu0_b = nu0_b_dat, H_b = 1500, rho_b = 1.3,
           I0_m = max(I0_b_dat * 0.4, 30), nu0_m = 21600,     H_m = 1200, rho_m = 1.3,
           I0_g = I0_g_dat,                nu0_g = nu0_g_dat,  H_g = 1800, rho_g = 1.3)
    )
    lo_3 <- c(0, 21739, 300, 1.01, 0, 20833, 300, 1.01, 0, 19608, 300, 1.01)
    hi_3 <- c(Inf, 23256, 4000, 5, Inf, 21739, 4000, 5, Inf, 20833, 4000, 5)
    
    fit <- NULL; best_rss <- Inf
    for (s in starts_3) {
      f <- tryCatch(
        nlsLM(y_sub ~ model_fn(nu_sub, I0_b, nu0_b, H_b, rho_b,
                               I0_m, nu0_m, H_m, rho_m,
                               I0_g, nu0_g, H_g, rho_g),
              start = s, lower = lo_3, upper = hi_3,
              control = nls.lm.control(maxiter = 200)),
        error = function(e) NULL
      )
      if (!is.null(f)) {
        rss <- sum(resid(f)^2)
        if (rss < best_rss) { best_rss <- rss; fit <- f }
      }
    }
    
    # Fallback to 2-peak if all 3-peak starting configurations failed
    if (is.null(fit)) {
      message("3-peak: all starting configs failed — falling back to 2-peak")
      result <- gp_lognormal_fit(w_nm, y_int, n_peaks = 2, fit_range_nm = fit_range_nm)
      if (!is.null(result)) result$fallback_to_2peak <- TRUE
      return(result)
    }
    
    p <- coef(fit)
    
    area_b <- trapz(nu_grid, alogn(nu_grid, p["I0_b"], p["nu0_b"], p["H_b"], p["rho_b"]))
    area_g <- trapz(nu_grid, alogn(nu_grid, p["I0_g"], p["nu0_g"], p["H_g"], p["rho_g"]))
    
    if (!is.finite(area_b + area_g) || (area_b + area_g) == 0)
      return(list(GP = NA_real_, fit = fit))
    
    return(list(
      GP = (area_b - area_g) / (area_b + area_g),
      fit = fit, coef = p, n_peaks = 3, fallback_to_2peak = FALSE,
      area_b = area_b, area_g = area_g,
      nu_sub = nu_sub, y_sub = y_sub
    ))
  }
  
  stop("lognorm_n_peaks must be 2 or 3")
}

# Load N spectrum files, optionally subtract blank, return averaged spectrum
load_and_average <- function(files, blank = NULL) {
  specs <- lapply(files, function(f) {
    s <- read_tsv(f, show_col_types = FALSE) %>%
      rename(wl = Wavelength, intensity = Intensity) %>%
      mutate(wl = as.numeric(wl), intensity = as.numeric(intensity)) %>%
      arrange(wl)
    if (!is.null(blank)) {
      blank_interp <- approx(blank$wl, blank$intensity, xout = s$wl, rule = 2)$y
      s <- mutate(s, intensity = intensity - blank_interp)
    }
    s
  })
  wl_ref  <- specs[[1]]$wl
  int_mat <- sapply(specs, function(s) approx(s$wl, s$intensity, xout = wl_ref, rule = 2)$y)
  tibble(wl = wl_ref, intensity = if (is.vector(int_mat)) int_mat else rowMeans(int_mat))
}

# ============================================================
# MAIN LOOP
# ============================================================

# Resolve paths and guard against accidentally writing into the data directory
raw_data_root <- normalizePath(raw_data_root, mustWork = TRUE)
output_dir    <- normalizePath(output_dir,    mustWork = FALSE)
if (startsWith(output_dir, raw_data_root))
  stop("output_dir must not be inside raw_data_root — check your path settings")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Auto-detect all experiment subfolders if requested
if (run_all_samples) {
  samples <- list.dirs(raw_data_root, full.names = FALSE, recursive = FALSE)
  samples <- samples[nzchar(samples)]
  message("run_all_samples = TRUE — found ", length(samples), " experiment folders")
}

sg_window_eff <- if (sg_window %% 2 == 0) sg_window + 1 else sg_window

# ── Resolve y-axis limits ─────────────────────────────────────────────────
scan_intensity_range <- function(sample_dirs) {
  all_int <- unlist(lapply(sample_dirs, function(smp) {
    d <- file.path(raw_data_root, smp)
    if (!dir.exists(d)) return(NULL)
    bl <- NULL
    if (subtract_blank) {
      bc <- list.files(d, pattern = "^blank_.*\\.tsv$", full.names = TRUE)
      if (length(bc) > 0) {
        bf <- bc[which.max(file.info(bc)$mtime)]
        bl <- tryCatch(
          read_tsv(bf, show_col_types = FALSE) %>%
            rename(wl = Wavelength, intensity = Intensity) %>%
            mutate(wl = as.numeric(wl), intensity = as.numeric(intensity)),
          error = function(e) NULL
        )
      }
    }
    fls <- list.files(d, pattern = "^spectrum_.*\\.tsv$", full.names = TRUE)
    unlist(lapply(fls, function(f) {
      tryCatch({
        s_df <- read_tsv(f, show_col_types = FALSE)
        wl   <- as.numeric(s_df$Wavelength)
        int  <- as.numeric(s_df$Intensity)
        if (!is.null(bl)) int <- int - approx(bl$wl, bl$intensity, xout = wl, rule = 2)$y
        int[wl >= 400 & wl <= 600]
      }, error = function(e) NULL)
    }))
  }))
  c(min(all_int, na.rm = TRUE), max(all_int, na.rm = TRUE) * 1.05)
}

ylim_resolved <- if (is.numeric(intensity_scale)) {
  intensity_scale
} else if (intensity_scale == "global") {
  message("Computing global y-axis limits across all samples...")
  lims <- scan_intensity_range(samples)
  message("  y-axis range: [", round(lims[1]), ", ", round(lims[2]), "]")
  lims
} else {
  NULL  # "auto" — computed per sample inside the loop
}

for (sample in samples) {
  
  message("\n── Processing: ", sample, " ──")
  data_dir <- file.path(raw_data_root, sample)
  if (!dir.exists(data_dir)) {
    warning("Directory not found, skipping: ", data_dir); next
  }
  
  # ── Load blank ─────────────────────────────────────────────
  blank <- NULL
  if (subtract_blank) {
    blank_candidates <- list.files(data_dir, pattern = "^blank_.*\\.tsv$", full.names = TRUE)
    if (length(blank_candidates) == 0) {
      warning("No blank_*.tsv found in ", data_dir, " — skipping blank subtraction")
    } else {
      blank_file <- blank_candidates[which.max(file.info(blank_candidates)$mtime)]
      message("  blank: ", basename(blank_file))
      blank <- read_tsv(blank_file, show_col_types = FALSE) %>%
        rename(wl = Wavelength, intensity = Intensity) %>%
        mutate(wl = as.numeric(wl), intensity = as.numeric(intensity))
    }
  }
  
  # ── Discover and group spectrum files ──────────────────────
  spec_files <- sort(list.files(data_dir, pattern = "^spectrum_.*\\.tsv$", full.names = TRUE))
  if (length(spec_files) == 0) {
    warning("No spectrum files found in ", data_dir, " — skipping"); next
  }
  
  if (plot_all_steps) {
    conditions <- lapply(seq_along(spec_files), function(i) {
      m <- str_match(
        basename(spec_files[i]),
        "^spectrum_(\\d{8}T\\d+)_idx(\\d+)_P(-?[0-9]+)_T([0-9.]+)\\.tsv$"
      )
      p_bar <- if (!is.na(m[1, 4])) round(as.numeric(m[1, 4]) / 14.5038) else NA
      t_c   <- if (!is.na(m[1, 5])) as.numeric(m[1, 5]) else NA
      lbl <- if (!is.na(p_bar) && !is.na(t_c)) {
        paste0("spectrum ", i, "  (", p_bar, " bar / ", t_c, "°C)")
      } else {
        paste0("spectrum ", i)
      }
      list(label = lbl, files = spec_files[i])
    })
  } else {
    if (spec_files[1] == spec_files[length(spec_files)])
      warning(sample, ": only one spectrum file found; start and end will be identical")
    
    conditions <- list(
      list(label = "start", files = spec_files[1]),
      list(label = "end",   files = spec_files[length(spec_files)])
    )
  }
  
  # ── Resolve per-sample y-axis limits (auto mode only) ─────
  ylim_global <- if (is.null(ylim_resolved)) {
    scan_intensity_range(sample)   # single-element vector; function iterates over it
  } else {
    ylim_resolved
  }
  
  # ── Process each condition ─────────────────────────────────
  plots_ln      <- list()
  plots_methods <- list()
  
  for (cond in conditions) {
    
    spec_label <- paste0(sample, " (", cond$label, ")")
    message("  ", cond$label, ": ", paste(basename(cond$files), collapse = ", "))
    
    spec <- load_and_average(cond$files, blank) %>%
      mutate(intensity_sg = sgolayfilt(intensity, p = sg_poly_order, n = sg_window_eff))
    
    # ── GP calculations ────────────────────────────────────────
    i440_raw <- approx(spec$wl, spec$intensity,    xout = wl_blue)$y
    i490_raw <- approx(spec$wl, spec$intensity,    xout = wl_green)$y
    i440_sg  <- approx(spec$wl, spec$intensity_sg, xout = wl_blue)$y
    i490_sg  <- approx(spec$wl, spec$intensity_sg, xout = wl_green)$y
    
    GP_raw_exact     <- (i440_raw - i490_raw) / (i440_raw + i490_raw)
    GP_sg_exact      <- (i440_sg  - i490_sg)  / (i440_sg  + i490_sg)
    GP_raw_band_mean <- gp_band_weighted(spec$wl, spec$intensity,    blue_band, green_band)
    GP_sg_band_mean  <- gp_band_weighted(spec$wl, spec$intensity_sg, blue_band, green_band)
    GP_sg_band_int   <- gp_band_int_norm(spec$wl, spec$intensity_sg, blue_band, green_band)
    
    b_idx <- which(spec$wl >= blue_peak_range[1]  & spec$wl <= blue_peak_range[2])
    g_idx <- which(spec$wl >= green_peak_range[1] & spec$wl <= green_peak_range[2])
    blue_peak_wl   <- spec$wl[b_idx[which.max(spec$intensity_sg[b_idx])]]
    green_peak_wl  <- spec$wl[g_idx[which.max(spec$intensity_sg[g_idx])]]
    blue_peak_int  <- max(spec$intensity_sg[b_idx])
    green_peak_int <- max(spec$intensity_sg[g_idx])
    GP_sg_peaks    <- (blue_peak_int - green_peak_int) / (blue_peak_int + green_peak_int)
    
    ln_result  <- gp_lognormal_fit(spec$wl, spec$intensity)
    GP_lognorm <- ln_result$GP
    
    # ── Console summary ────────────────────────────────────────
    cat("\n", spec_label, "\n", sep = "")
    print(tibble(
      Method = c("raw exact (440/490)", "raw band mean",
                 "sg exact (440/490)", "sg peaks (auto)",
                 "sg band mean", "sg band integrated", "lognormal"),
      GP = c(GP_raw_exact, GP_raw_band_mean, GP_sg_exact,
             GP_sg_peaks, GP_sg_band_mean, GP_sg_band_int, GP_lognorm)
    ))
    
    # ── LN decomposition plot (optional) ──────────────────────
    if (show_ln_decomposition) {
      raw_fit_df <- dplyr::filter(spec, wl >= 400, wl <= 600)
      
      if (!is.null(ln_result$fit)) {
        
        cp      <- ln_result$coef
        nu_plot <- seq(16000, 26000, by = 10)
        
        ln_fit_df <- tibble(
          nu    = nu_plot,
          wl    = 1e7 / nu_plot,
          blue  = alogn(nu_plot, cp["I0_b"], cp["nu0_b"], cp["H_b"], cp["rho_b"]),
          green = alogn(nu_plot, cp["I0_g"], cp["nu0_g"], cp["H_g"], cp["rho_g"]),
          mid   = if (ln_result$n_peaks == 3)
            alogn(nu_plot, cp["I0_m"], cp["nu0_m"], cp["H_m"], cp["rho_m"])
          else
            rep(0, length(nu_plot)),
          total = blue + mid + green
        ) %>% dplyr::filter(wl >= 400, wl <= 600)
        
        color_vals <- c("Raw data" = "gray50", "Total fit" = "black",
                        "Blue peak" = "steelblue", "Green peak" = "seagreen")
        ltype_vals <- c("Raw data" = "solid",  "Total fit" = "solid",
                        "Blue peak" = "dashed", "Green peak" = "dashed")
        if (ln_result$n_peaks == 3) {
          color_vals <- c(color_vals, "Mid peak" = "darkorchid")
          ltype_vals <- c(ltype_vals, "Mid peak" = "dashed")
        }
        
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
                      linewidth = 0.9) } +
          scale_color_manual(name = NULL, values = color_vals) +
          scale_linetype_manual(name = NULL, values = ltype_vals) +
          theme_bw() +
          theme(
            plot.title      = element_text(size = 16, face = "bold"),
            plot.subtitle   = element_text(size = 9,  colour = "grey30"),
            axis.title      = element_text(size = 13),
            axis.text       = element_text(size = 12),
            legend.position = "right"
          ) +
          labs(
            title    = paste0(spec_label, " — LN decomposition (", ln_result$n_peaks, " peaks",
                              if (isTRUE(ln_result$fallback_to_2peak)) " — fallback from 3-peak" else "",
                              ")"),
            subtitle = paste0(
              "Blue: ",  round(1e7 / cp["nu0_b"], 1), " nm",
              if (ln_result$n_peaks == 3)
                paste0("  |  Mid: ", round(1e7 / cp["nu0_m"], 1), " nm"),
              "  |  Green: ", round(1e7 / cp["nu0_g"], 1), " nm",
              "  |  GP = ", round(GP_lognorm, 3)
            ),
            x = "Wavelength (nm)",
            y = "Intensity"
          ) +
          scale_x_continuous(limits = c(400, 600)) +
          { if (!is.null(ylim_global)) coord_cartesian(ylim = ylim_global) }
        
      } else {
        
        p_ln <- ggplot() +
          geom_line(data = raw_fit_df, aes(wl, intensity),
                    color = "gray50", linewidth = 0.7, alpha = 0.7) +
          theme_bw() +
          theme(
            plot.title    = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 9,  colour = "red"),
            axis.title    = element_text(size = 13),
            axis.text     = element_text(size = 12)
          ) +
          labs(
            title    = paste0(spec_label, " — LN decomposition (", lognorm_n_peaks, " peaks)"),
            subtitle = "Log-normal fit did not converge — no decomposition available",
            x = "Wavelength (nm)",
            y = "Intensity"
          ) +
          scale_x_continuous(limits = c(400, 600)) +
          { if (!is.null(ylim_global)) coord_cartesian(ylim = ylim_global) }
      }
      
      plots_ln[[cond$label]] <- p_ln
    } # end if (show_ln_decomposition)
    
    # ── Method comparison plot ─────────────────────────────────
    if (show_method_comparison) {
      
      blue_band_df  <- dplyr::filter(spec, wl >= blue_band[1],  wl <= blue_band[2]) %>%
        mutate(intensity_sg = pmax(intensity_sg, 0))
      green_band_df <- dplyr::filter(spec, wl >= green_band[1], wl <= green_band[2]) %>%
        mutate(intensity_sg = pmax(intensity_sg, 0))
      
      p_spec <- ggplot(spec, aes(wl)) +
        geom_area(data = blue_band_df,
                  aes(y = intensity_sg, fill = "Blue band"),  alpha = 0.35) +
        geom_area(data = green_band_df,
                  aes(y = intensity_sg, fill = "Green band"), alpha = 0.35) +
        geom_line(aes(y = intensity,    color = "Raw"),          alpha = 0.4, linewidth = 0.6) +
        geom_line(aes(y = intensity_sg, color = "SG-smoothed"),  linewidth = 0.8) +
        geom_vline(aes(xintercept = wl_blue,       linetype = "Exact 440/490"), color = "gray40") +
        geom_vline(aes(xintercept = wl_green,      linetype = "Exact 440/490"), color = "gray40") +
        geom_vline(data = data.frame(x = c(blue_peak_wl, green_peak_wl)),
                   aes(xintercept = x, linetype = "SG peaks"), color = "navy") +
        scale_color_manual(name = NULL,
                           values = c("Raw" = "black", "SG-smoothed" = "red")) +
        scale_fill_manual(name = NULL,
                          values = c("Blue band" = "steelblue", "Green band" = "seagreen")) +
        scale_linetype_manual(name = NULL,
                              values = c("Exact 440/490" = "dashed", "SG peaks" = "dashed")) +
        theme_bw() +
        theme(
          legend.position  = "right",
          legend.text      = element_text(size = 10),
          legend.key.width = unit(1.2, "lines"),
          plot.caption     = element_text(family = "mono", size = 9, color = "gray30",
                                          hjust = 1, margin = margin(t = 6)),
          plot.title       = element_text(size = 18, face = "bold"),
          axis.title       = element_text(size = 15),
          axis.text        = element_text(size = 15)
        ) +
        labs(
          title   = spec_label,
          x       = "Wavelength (nm)",
          y       = "Intensity",
          caption = paste0(
            "Exact: ",      round(GP_raw_exact,    3), "   ",
            "SG exact: ",   round(GP_sg_exact,     3), "   ",
            "SG peaks: ",   round(GP_sg_peaks,     3), "\n",
            "Band mean: ",  round(GP_sg_band_mean, 3), "   ",
            "Band int: ",   round(GP_sg_band_int,  3), "   ",
            "Log-normal: ", round(GP_lognorm,      3)
          )
        ) +
        scale_x_continuous(limits = c(400, 600)) +
        coord_cartesian(ylim = ylim_global)
      
      plots_methods[[cond$label]] <- p_spec
    }
  }
  
  # ── Save PDF ───────────────────────────────────────────────
  stem     <- if (plot_all_steps) "_all_steps_diagnostic" else "_diagnostic"
  base_dir <- if (show_ln_decomposition) file.path(output_dir, "ln_decomp") else output_dir
  out_dir  <- if (plot_all_steps) file.path(base_dir, "all_spectra") else base_dir
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  pdf_path <- file.path(out_dir, paste0(
    sample, stem, if (show_ln_decomposition) "_ln.pdf" else ".pdf"
  ))
  pdf(pdf_path, width = 8, height = 5)
  if (show_method_comparison) for (p in plots_methods) print(p)
  if (show_ln_decomposition)  for (p in plots_ln)      print(p)
  invisible(dev.off())
  message("Saved: ", pdf_path)
}
