#!/usr/bin/env Rscript

# ──────────────────────────────────────────────────────────────────────────
# LAURDAN GP ANALYSIS
#
# Computes and visualises C-Laurdan GP for one or more Cubette experiments.
# Generates up to two complementary plots per sample:
#
#   plot_over_time : GP vs. elapsed time, colour-coded by temperature,
#                    annotated with pressure at each step.
#                    Outputs: figures/gp_over_time/<sample>_GP_over_time_*.pdf
#                             results/<GP_method>/<sample>_GP_values_*.csv
#
#   plot_landscape : GP as a P/T contour landscape (MBA interpolation),
#                    with optional GIF animation and live-mode refresh.
#                    Outputs: figures/gp_landscape/<sample>_lgp_landscape_*.pdf
#
# GP METHODS (set GP_method):
#   sg_band   : SG-smoothed (poly=3, window=101) trapezoid-integrated band
#               average over blue (420-460nm) / green (470-510nm); recommended
#   lognormal : Siano-Metzler 2- or 3-peak log-normal fit in wavenumber space;
#               not recommended for per-spectrum pipeline use (high step noise)
#
# BATCH PROCESSING:
#   Set run_all_samples <- TRUE to process every subfolder in data/raw/,
#   or list specific folder names in samples <- c("name1", "name2", ...)
#
# Run from the project root:
#   Rscript scripts/gp_cubette.R
#
# Edited: 05/19/2026
# ──────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(ggrepel)
  library(readr)
  library(signal)
  library(minpack.lm)
  library(ggpubr)
  library(gridExtra)
  library(MBA)
  library(magick)
  library(readxl)
  library(shadowtext)
})

# ============================================================
# PLOT SELECTION
# ============================================================

plot_over_time <- TRUE    # GP vs. elapsed time  (PDF + CSV per sample)
plot_landscape <- FALSE    # P/T contour landscape (PDF per sample)

# ============================================================
# SHARED PARAMETERS
# ============================================================

# Which samples to run:
run_all_samples <- FALSE
samples         <- c("Bolinopsis_D1339bw9_wproti_040826")
raw_data_root   <- "data/raw"

# GP axis / colour scale — change ONE value to switch modes:
#   "auto"       per-plot autoscale (each experiment scaled independently)
#   "global"     auto-compute from ALL experiments in data/raw/ before plotting
#   c(min, max)  fixed manual limits, e.g. c(-0.6, 0.1)
gp_scale <- "global"
#gp_scale <- c(-0.6, 0.1)

subtract_blank <- TRUE
blank_pattern  <- "^blank_.*\\.tsv$"
pat_data       <- "^spectrum_.*\\.tsv$"

# GP method options:
# - "sg_band"   = SG-smoothed Δλ-weighted band GP (normalized integration)
# - "lognormal" = log-normal decomposition (Siano-Metzler, 2- or 3-peak)
GP_method <- "sg_band"

# Savitzky–Golay parameters
sg_poly_order <- 3       # polynomial order
sg_window     <- 101     # window length (will be forced to odd)

# Band-averaged GP windows (used only if GP_method == "sg_band")
blue_range  <- c(420, 460)
green_range <- c(470, 510)

# LN decomposition parameters (used only if GP_method == "lognormal")
lognorm_n_peaks   <- 3          # 2 = blue + green; 3 = blue + mid + green
lognorm_fit_range <- c(410, 560)
lognorm_start_2peak <- list(
  I0_b = 300, nu0_b = 22727, H_b = 1800, rho_b = 1.3,
  I0_g = 800, nu0_g = 20408, H_g = 1800, rho_g = 1.3
)
lognorm_start_3peak <- list(
  I0_b = 300, nu0_b = 22727, H_b = 1500, rho_b = 1.3,
  I0_m = 200, nu0_m = 21645, H_m = 1500, rho_m = 1.3,
  I0_g = 800, nu0_g = 20408, H_g = 1500, rho_g = 1.3
)

# Spectral filters
min_wl       <- 390
thres_intens <- 1

# ============================================================
# OVER-TIME PARAMETERS  (used when plot_over_time = TRUE)
# ============================================================

figures_dir_ot          <- "figures/gp_over_time"
time_unit               <- "mins"
n_measurements_per_step <- 3    # 1 or 3; set to 1 for per-spectrum (no averaging)

# ============================================================
# LANDSCAPE PARAMETERS  (used when plot_landscape = TRUE)
# ============================================================

figures_dir_ls <- "figures/gp_landscape"
live_mode      <- FALSE   # TRUE = re-render on a timer during a running experiment
make_gif       <- FALSE   # TRUE = also export animated GIF
refresh_sec    <- 5       # seconds between live-mode refreshes

# ============================================================
# DERIVED TAGS  (do not edit)
# ============================================================

scale_tag <- if (is.numeric(gp_scale)) {
  paste0("scale_", gp_scale[1], "_", gp_scale[2])
} else {
  gp_scale
}
method_tag  <- paste(GP_method, scale_tag, sep = "_")
results_dir <- file.path("results", GP_method)

# ============================================================
# SHARED FUNCTIONS — DATA LOADING
# ============================================================

read_spectra <- function(dir, pat) {
  files <- list.files(dir, pattern = pat, full.names = TRUE)
  if (length(files) == 0) return(tibble())

  spectra <- map_df(files, function(f) {
    readr::read_tsv(f, show_col_types = FALSE) %>%
      mutate(file = basename(f))
  })

  meta <- tibble(file = basename(files)) %>%
    mutate(
      parts = stringr::str_match(
        file,
        "spectrum_(\\d{8}T\\d+)_idx(\\d+)_P(-?[0-9]+)_T([0-9.]+)\\.tsv$"
      ),
      clock = suppressWarnings(
        lubridate::ymd_hms(stringr::str_sub(parts[, 2], 1, 15), tz = "UTC")
      ),
      rep   = as.integer(parts[, 3]),
      P_act = as.numeric(parts[, 4]) / 14.5038,   # psi → bar
      T_act = as.numeric(parts[, 5])
    ) %>%
    select(file, clock, rep, P_act, T_act)

  left_join(spectra, meta, by = "file")
}

read_blank <- function(dir, pat) {
  candidates <- list.files(dir, pattern = pat, full.names = TRUE)
  if (length(candidates) == 0) return(NULL)
  readr::read_tsv(candidates[1], show_col_types = FALSE)
}

subtract_blank_if_enabled <- function(raw, blank) {
  if (!subtract_blank || is.null(blank)) return(raw)
  blank_i       <- approx(blank$Wavelength, blank$Intensity,
                          xout = raw$Wavelength, rule = 2)$y
  raw$Intensity <- raw$Intensity - blank_i
  raw
}

# ============================================================
# SHARED FUNCTIONS — GP CALCULATION
# ============================================================

trapz <- function(x, y) {
  idx <- order(x)
  x <- x[idx]; y <- y[idx]
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

band_weighted_mean <- function(w, y, rng) {
  idx <- which(w >= rng[1] & w <= rng[2])
  if (length(idx) < 2) return(NA_real_)
  trapz(w[idx], y[idx]) / diff(rng)
}

alogn <- function(nu, I0, nu0, H, rho) {
  arg <- ((nu - nu0) / H) * ((rho^2 - 1) / rho) + 1
  out <- numeric(length(nu))
  valid <- arg > 0
  out[valid] <- I0 * exp(-(log(2) / log(rho)^2) * log(arg[valid])^2)
  out
}

gp_lognormal_fit <- function(w_nm, y_int, n_peaks = lognorm_n_peaks,
                             fit_range_nm = lognorm_fit_range) {

  idx <- which(w_nm >= fit_range_nm[1] & w_nm <= fit_range_nm[2] & is.finite(y_int))
  if (length(idx) < 20) return(list(GP = NA_real_, fit = NULL))

  w_sub  <- w_nm[idx]
  y_sub  <- y_int[idx]
  nu_sub <- 1e7 / w_sub

  g_i <- which(w_sub >= 480 & w_sub <= 520)
  if (length(g_i) >= 2) {
    gm        <- g_i[which.max(y_sub[g_i])]
    I0_g_dat  <- y_sub[gm]
    nu0_g_dat <- max(19608, min(20833, nu_sub[gm]))
  } else {
    I0_g_dat <- 800; nu0_g_dat <- 20408
  }
  b440_i    <- which.min(abs(w_sub - 440))
  I0_b_dat  <- if (length(b440_i) >= 1) y_sub[b440_i] else 300
  nu0_b_dat <- 22727

  nu_grid <- seq(16000, 26000, by = 5)

  if (n_peaks == 2) {

    model_fn <- function(nu, I0_b, nu0_b, H_b, rho_b,
                         I0_g, nu0_g, H_g, rho_g) {
      alogn(nu, I0_b, nu0_b, H_b, rho_b) +
        alogn(nu, I0_g, nu0_g, H_g, rho_g)
    }
    starts_2 <- list(
      list(I0_b = I0_b_dat, nu0_b = nu0_b_dat, H_b = 1800, rho_b = 1.3,
           I0_g = I0_g_dat, nu0_g = nu0_g_dat, H_g = 1800, rho_g = 1.3),
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
      area_b = area_b, area_g = area_g, nu_sub = nu_sub, y_sub = y_sub
    ))
  }

  if (n_peaks == 3) {

    model_fn <- function(nu, I0_b, nu0_b, H_b, rho_b,
                         I0_m, nu0_m, H_m, rho_m,
                         I0_g, nu0_g, H_g, rho_g) {
      alogn(nu, I0_b, nu0_b, H_b, rho_b) +
        alogn(nu, I0_m, nu0_m, H_m, rho_m) +
        alogn(nu, I0_g, nu0_g, H_g, rho_g)
    }
    starts_3 <- list(
      list(I0_b = I0_b_dat,                nu0_b = nu0_b_dat, H_b = 1500, rho_b = 1.3,
           I0_m = max(I0_b_dat * 0.3, 30), nu0_m = 21200,     H_m = 1200, rho_m = 1.3,
           I0_g = I0_g_dat,                nu0_g = nu0_g_dat,  H_g = 1800, rho_g = 1.3),
      lognorm_start_3peak,
      list(I0_b = max(I0_g_dat * 0.05, 15), nu0_b = 22727,     H_b = 1200, rho_b = 1.3,
           I0_m = max(I0_g_dat * 0.05, 15), nu0_m = 21500,     H_m = 1200, rho_m = 1.3,
           I0_g = I0_g_dat,                  nu0_g = nu0_g_dat, H_g = 1800, rho_g = 1.3),
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
      area_b = area_b, area_g = area_g, nu_sub = nu_sub, y_sub = y_sub
    ))
  }

  stop("lognorm_n_peaks must be 2 or 3")
}

compute_gp <- function(df) {
  df <- df %>% dplyr::filter(Wavelength >= min_wl, Intensity >= thres_intens)
  if (nrow(df) == 0) return(NA_real_)

  w <- df$Wavelength
  y <- df$Intensity

  if (GP_method == "sg_band") {
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

  if (GP_method == "lognormal") {
    win <- ifelse(sg_window %% 2 == 0, sg_window + 1, sg_window)
    y_smooth <- tryCatch(
      signal::sgolayfilt(y, p = sg_poly_order, n = win),
      error = function(e) rep(NA_real_, length(y))
    )
    if (all(is.na(y_smooth))) return(NA_real_)
    return(gp_lognormal_fit(w, y_smooth)$GP)
  }

  stop("Unknown GP_method: use 'sg_band' or 'lognormal'")
}

# Returns one row per spectrum file with columns: file, clock, P_act, T_act, gp, file_index
build_gp_table <- function(raw) {
  raw %>%
    group_by(file, clock, P_act, T_act) %>%
    nest() %>%
    mutate(gp = map_dbl(data, compute_gp)) %>%
    select(-data) %>%
    dplyr::filter(is.finite(gp)) %>%
    arrange(clock) %>%
    mutate(file_index = row_number())
}

compute_global_gp_limits <- function(root) {
  all_dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  all_gp   <- numeric(0)

  for (d in all_dirs) {
    raw <- read_spectra(d, pat_data)
    if (nrow(raw) == 0) next
    b <- read_blank(d, blank_pattern)
    if (!is.null(b))
      raw <- raw %>% split(.$file) %>%
        lapply(subtract_blank_if_enabled, blank = b) %>% bind_rows()
    gp_vals <- build_gp_table(raw)$gp
    all_gp  <- c(all_gp, gp_vals[is.finite(gp_vals)])
  }

  if (length(all_gp) == 0) {
    warning("compute_global_gp_limits: no finite GP values found — falling back to auto")
    return(NULL)
  }
  pad <- diff(range(all_gp)) * 0.05
  c(min(all_gp) - pad, max(all_gp) + pad)
}

compute_pt_limits <- function(raw) {
  list(
    T_min = min(raw$T_act, na.rm = TRUE),
    T_max = max(raw$T_act, na.rm = TRUE),
    P_min = min(raw$P_act, na.rm = TRUE),
    P_max = max(raw$P_act, na.rm = TRUE)
  )
}

# ============================================================
# OVER-TIME FUNCTIONS
# ============================================================

# Reads spectrum files one at a time — mirrors the original gp_over_time.R
# approach exactly so timestamps and step order are correct.
build_ot_rows <- function(dir_data, blank) {
  files <- list.files(dir_data, pattern = pat_data, full.names = TRUE)
  if (length(files) == 0) return(NULL)

  map_dfr(seq_along(files), function(i) {
    f        <- files[i]
    spec_raw <- read_tsv(f, col_types = cols())
    spec_cor <- subtract_blank_if_enabled(spec_raw, blank)
    gp_val   <- compute_gp(spec_cor)

    m  <- str_match(basename(f),
                    "^spectrum_(\\d{8}T\\d+)_idx(\\d+)_P(-?[0-9]+)_T([0-9.]+)\\.tsv$")
    ts <- suppressWarnings(
      ymd_hms(str_replace(str_sub(m[1, 2], 1, 15), "T", " "), tz = "UTC")
    )

    tibble(
      file_index = i,
      Timestamp  = ts,
      Press_psi  = as.numeric(m[1, 4]),
      Temp       = as.numeric(m[1, 5]),
      GP         = gp_val
    )
  }) %>%
    arrange(Timestamp) %>%
    mutate(time_min = as.numeric(difftime(Timestamp, min(Timestamp), units = time_unit)))
}

# Groups per-spectrum rows into steps and returns a data_plot-ready tibble
# with columns: gp, gp_sd, gp_se, n, time_min, P_act, T_act.
group_ot_steps <- function(rows, n_per_step) {
  if (n_per_step >= 2) {
    rows %>%
      arrange(file_index) %>%
      mutate(step_id = ceiling(row_number() / n_per_step)) %>%
      group_by(step_id) %>%
      summarise(
        n         = sum(is.finite(GP)),
        gp_sd     = sd(GP,        na.rm = TRUE),
        gp        = mean(GP,      na.rm = TRUE),
        Timestamp = as_datetime(mean(as.numeric(Timestamp), na.rm = TRUE), tz = "UTC"),
        Press_psi = mean(Press_psi, na.rm = TRUE),
        T_act     = mean(Temp,    na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      mutate(
        gp_se    = gp_sd / sqrt(n),
        time_min = as.numeric(difftime(Timestamp, min(Timestamp), units = time_unit)),
        P_act    = Press_psi / 14.5038
      )
  } else {
    rows %>%
      mutate(
        step_id = row_number(),
        n       = 1L,
        gp      = GP,
        gp_sd   = NA_real_,
        gp_se   = NA_real_,
        T_act   = Temp,
        P_act   = Press_psi / 14.5038
      )
  }
}

plot_gp_over_time <- function(data_plot, sample_name, gp_limits) {
  ggplot(data_plot, aes(x = time_min, y = gp, color = T_act)) +
    { if (nrow(data_plot) > 1) geom_line(aes(group = 1), alpha = 0.4) } +
    geom_point(size = 2.8) +
    {
      if (any(is.finite(data_plot$gp_se)))
        geom_errorbar(aes(ymin = gp - gp_se, ymax = gp + gp_se),
                      width = 0.3, alpha = 0.6, color = "black")
    } +
    geom_text_repel(
      aes(label = paste0(round(P_act), " bar")),
      size = 3, max.overlaps = 30
    ) +
    scale_color_gradient(low = "blue", high = "red",
                         name = "temp (°C)", limits = c(0, 25)) +
    scale_y_continuous(limits = gp_limits) +
    scale_x_continuous(limits = c(0, 330)) +
    theme_bw() +
    theme(
      plot.title    = element_text(size = 20, colour = "black", face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13, colour = "grey30", hjust = 0.5),
      axis.title.x  = element_text(size = 20),
      axis.title.y  = element_text(size = 20),
      axis.text.x   = element_text(size = 18),
      axis.text.y   = element_text(size = 18)
    ) +
    labs(
      title    = sample_name,
      subtitle = paste(
        ifelse(subtract_blank, "blank-subtracted", "raw"),
        "| GP method:", GP_method,
        "| n =", n_measurements_per_step, "per step"
      ),
      x = paste0("time (", time_unit, ")"),
      y = "Laurdan GP"
    )
}

# ============================================================
# LANDSCAPE FUNCTIONS
# ============================================================

state_plot <- function(raw, pt_lims) {
  raw %>%
    ggplot(aes(T_act, P_act, color = clock)) +
    geom_point(size = 2.2, alpha = 0.9) +
    scale_color_gradient(low = "black", high = "red", guide = "none") +
    scale_y_reverse() +
    coord_cartesian(
      xlim = c(pt_lims$T_min, pt_lims$T_max),
      ylim = c(pt_lims$P_min, pt_lims$P_max)
    ) +
    theme_pubr() +
    theme(
      legend.position = "none",
      plot.margin  = margin(5, 5, 5, 5),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x  = element_text(size = 18),
      axis.text.y  = element_text(size = 18),
      plot.title   = element_text(size = 16, hjust = 0.5)
    ) +
    labs(title = "Cubette state", x = "temperature (°C)", y = "pressure (bar)")
}

contour_plot <- function(gpdata, pt_lims, gp_limits, sample_name, native_pt = NULL) {

  if (nrow(gpdata) < 3) {
    return(
      ggplot() +
        scale_fill_viridis_c(limits = gp_limits, direction = -1, name = "Laurdan GP") +
        scale_y_reverse() +
        coord_cartesian(
          xlim = c(pt_lims$T_min, pt_lims$T_max),
          ylim = c(pt_lims$P_min, pt_lims$P_max)
        ) +
        theme_pubr() +
        labs(
          title    = sample_name,
          subtitle = paste(ifelse(subtract_blank, "blank-subtracted", "raw"),
                           "| GP method:", GP_method, "| waiting for GP data"),
          x = "temperature (°C)", y = "pressure (bar)"
        )
    )
  }

  surf <- MBA::mba.surf(
    gpdata[, c("T_act", "P_act", "gp")],
    no.X = 150, no.Y = 150, extend = TRUE
  )
  grid <- surf$xyz.est
  tib  <- expand.grid(T_act = grid$x, P_act = grid$y) %>%
    mutate(gp = as.vector(grid$z)) %>%
    drop_na(gp)

  ggplot(tib, aes(T_act, P_act)) +
    geom_raster(aes(fill = gp), interpolate = TRUE) +
    geom_contour(aes(z = gp), color = "white", linewidth = 0.25) +
    geom_point(data = gpdata, aes(T_act, P_act), color = "black", size = 1.6) +
    {
      if (!is.null(native_pt))
        geom_shadowtext(
          data        = data.frame(T_act = native_pt$T,
                                   P_act = max(native_pt$P, pt_lims$P_min)),
          aes(T_act, P_act),
          label       = "*", family = "Helvetica", fontface = "bold",
          size        = 14, colour = "black", bg.colour = "white", bg.r = 0.05,
          inherit.aes = FALSE
        )
    } +
    scale_fill_viridis_c(limits = gp_limits, direction = -1, name = "Laurdan GP") +
    scale_y_reverse() +
    coord_cartesian(
      xlim = c(pt_lims$T_min, pt_lims$T_max),
      ylim = c(pt_lims$P_min, pt_lims$P_max)
    ) +
    theme_pubr() +
    theme(
      legend.position = "right",
      plot.title    = element_text(size = 20, colour = "black", face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13, colour = "grey30", hjust = 0.5),
      axis.title.x  = element_text(size = 20),
      axis.title.y  = element_text(size = 20),
      axis.text.x   = element_text(size = 18),
      axis.text.y   = element_text(size = 18)
    ) +
    labs(
      title    = sample_name,
      subtitle = paste(ifelse(subtract_blank, "blank-subtracted", "raw"),
                       "| GP method:", GP_method),
      x = "temperature (°C)", y = "pressure (bar)"
    )
}

render_static <- function(raw, pt_lims, gp_limits, outfile, sample_name, native_pt = NULL) {
  gpdata <- build_gp_table(raw)
  if (is.null(gp_limits)) {
    pad       <- diff(range(gpdata$gp, na.rm = TRUE)) * 0.05
    gp_limits <- c(min(gpdata$gp, na.rm = TRUE) - pad, max(gpdata$gp, na.rm = TRUE) + pad)
  }
  p <- contour_plot(gpdata, pt_lims, gp_limits, sample_name, native_pt)
  print(p)
  ggsave(outfile, p, width = 6, height = 6, dpi = 300)
}

render_frame <- function(raw, pt_lims, gp_limits, outfile, sample_name,
                         time_label, native_pt = NULL) {
  gpdata  <- build_gp_table(raw)
  p_state <- state_plot(raw, pt_lims) +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14),
          plot.title = element_text(size = 16, hjust = 0))
  p_cont  <- contour_plot(gpdata, pt_lims, gp_limits, sample_name, native_pt) +
    ggtitle(sample_name, subtitle = paste("Time", time_label)) +
    theme(plot.title    = element_text(size = 16, hjust = 0),
          plot.subtitle = element_text(size = 14, hjust = 0),
          axis.title    = element_text(size = 16),
          axis.text     = element_text(size = 14))
  combined <- grid.arrange(p_state, p_cont, ncol = 1, heights = c(1, 2))
  ggsave(outfile, combined, width = 6.5, height = 9, dpi = 150)
}

make_gp_gif <- function(raw, pt_lims, gp_limits, gif_out, sample_name, native_pt = NULL) {
  if (is.null(gp_limits)) {
    gp_all    <- build_gp_table(raw)$gp
    pad       <- diff(range(gp_all, na.rm = TRUE)) * 0.05
    gp_limits <- c(min(gp_all, na.rm = TRUE) - pad, max(gp_all, na.rm = TRUE) + pad)
  }

  raw_sorted <- raw %>% arrange(clock)
  times      <- sort(unique(raw_sorted$clock))
  frame_dir  <- file.path(figures_dir_ls, paste0(sample_name, "_gif_frames"))
  dir.create(frame_dir, showWarnings = FALSE)

  message("Rendering ", length(times), " GIF frames...")
  for (i in seq_along(times)) {
    frame_raw  <- raw_sorted[raw_sorted$clock <= times[i], ]
    frame_file <- file.path(frame_dir, sprintf("frame_%04d.png", i))
    render_frame(frame_raw, pt_lims, gp_limits, frame_file, sample_name,
                 format(times[i], "%H:%M:%S"), native_pt)
  }

  gif <- image_read(list.files(frame_dir, full.names = TRUE))
  gif <- image_animate(gif, fps = 10)
  image_write(gif, gif_out)
  message("GIF saved to ", gif_out)
}

# ============================================================
# MAIN SETUP
# ============================================================

raw_data_root  <- normalizePath(raw_data_root,  mustWork = TRUE)
figures_dir_ot <- normalizePath(figures_dir_ot, mustWork = FALSE)
figures_dir_ls <- normalizePath(figures_dir_ls, mustWork = FALSE)
results_dir    <- normalizePath(results_dir,    mustWork = FALSE)

for (d in c(figures_dir_ot, figures_dir_ls, results_dir)) {
  if (startsWith(d, raw_data_root))
    stop("Output directory must not be inside raw_data_root: ", d)
}

if (plot_over_time) {
  dir.create(figures_dir_ot, showWarnings = FALSE, recursive = TRUE)
  dir.create(results_dir,    showWarnings = FALSE, recursive = TRUE)
}
if (plot_landscape) dir.create(figures_dir_ls, showWarnings = FALSE, recursive = TRUE)

# Resolve GP axis / colour limits once before the loop
gp_limits_resolved <- if (is.numeric(gp_scale)) {
  gp_scale
} else if (gp_scale == "global") {
  message("Computing global GP limits from all experiments in ", raw_data_root, " ...")
  lims <- compute_global_gp_limits(raw_data_root)
  if (!is.null(lims))
    message("  GP scale: [", round(lims[1], 3), ", ", round(lims[2], 3), "]")
  lims
} else {
  NULL   # "auto" — computed per sample inside the loop
}

# Load native collection conditions from metadata (landscape only)
meta <- NULL
if (plot_landscape) {
  meta_file <- file.path(dirname(raw_data_root), "cubette_metadata.xlsx")
  if (file.exists(meta_file)) {
    meta <- read_excel(meta_file) %>%
      mutate(native_P = depth * 0.1006 + 1.013)   # depth (m) → absolute pressure (bar): hydrostatic + 1 atm
    message("Loaded metadata: ", basename(meta_file))
  } else {
    warning("cubette_metadata.xlsx not found at ", meta_file,
            " — native P/T marker disabled")
  }
}

if (run_all_samples) {
  samples <- list.dirs(raw_data_root, full.names = FALSE, recursive = FALSE)
  samples <- samples[nzchar(samples)]
  message("run_all_samples = TRUE — found ", length(samples), " experiment folders")
}

# ============================================================
# MAIN LOOP
# ============================================================

for (sample_name in samples) {

  message("\n── Processing: ", sample_name, " ──")
  dir_data <- file.path(raw_data_root, sample_name)
  if (!dir.exists(dir_data)) {
    warning("Directory not found, skipping: ", dir_data); next
  }

  # Load blank once — used by both plots
  blank <- read_blank(dir_data, blank_pattern)
  if (subtract_blank && is.null(blank))
    warning("No blank file found in ", dir_data, " — skipping blank subtraction")

  # ── Over-time plot ─────────────────────────────────────────
  if (plot_over_time) {

    rows <- build_ot_rows(dir_data, blank)
    if (is.null(rows) || nrow(rows) == 0) {
      warning("No spectrum data for ", sample_name, " — skipping over-time plot")
    } else {
      data_plot  <- group_ot_steps(rows, n_measurements_per_step)
      gp_lims_ot <- if (is.null(gp_limits_resolved)) c(NA_real_, NA_real_) else gp_limits_resolved

      p_ot   <- plot_gp_over_time(data_plot, sample_name, gp_lims_ot)
      pdf_ot <- file.path(figures_dir_ot,
                          paste0(sample_name, "_GP_over_time_", method_tag, ".pdf"))
      csv_ot <- file.path(results_dir,
                          paste0(sample_name, "_GP_values_", GP_method, ".csv"))

      print(p_ot)
      ggsave(pdf_ot, p_ot, width = 9, height = 6, dpi = 300)
      write_csv(rows, csv_ot)
      message("Saved: ", pdf_ot)
      message("Saved: ", csv_ot)
    }
  }

  # ── Landscape plot ─────────────────────────────────────────
  if (plot_landscape) {

    raw <- read_spectra(dir_data, pat_data)
    if (nrow(raw) == 0) {
      warning("No spectrum files found in ", dir_data, " — skipping landscape plot")
    } else {
      if (subtract_blank && !is.null(blank))
        raw <- raw %>%
          split(.$file) %>%
          lapply(subtract_blank_if_enabled, blank = blank) %>%
          bind_rows()

      pdf_ls <- file.path(figures_dir_ls,
                          paste0(sample_name, "_lgp_landscape_", method_tag, ".pdf"))
      gif_ls <- file.path(figures_dir_ls,
                          paste0(sample_name, "_lgp_landscape_", method_tag, ".gif"))

      native_pt <- NULL
      if (!is.null(meta)) {
        meta_row <- dplyr::filter(meta, file_name == sample_name)
        if (nrow(meta_row) == 1 && !is.na(meta_row$depth)) {
          native_pt <- list(T = meta_row$temp, P = meta_row$native_P)
          message("  native conditions: ", round(meta_row$temp, 1), " °C / ",
                  round(meta_row$native_P, 1), " bar (", meta_row$depth, " m)")
        } else {
          message("  native conditions: not found in metadata — marker skipped")
        }
      }

      if (!live_mode) {

        pt_lims <- compute_pt_limits(raw)
        render_static(raw, pt_lims, gp_limits_resolved, pdf_ls, sample_name, native_pt)
        message("Saved: ", pdf_ls)

        if (make_gif)
          make_gp_gif(raw, pt_lims, gp_limits_resolved, gif_ls, sample_name, native_pt)

      } else {

        while (TRUE) {
          raw_live <- read_spectra(dir_data, pat_data)
          if (subtract_blank && !is.null(blank))
            raw_live <- raw_live %>%
              split(.$file) %>%
              lapply(subtract_blank_if_enabled, blank = blank) %>%
              bind_rows()
          pt_lims <- compute_pt_limits(raw_live)
          render_static(raw_live, pt_lims, gp_limits_resolved, pdf_ls, sample_name, native_pt)
          Sys.sleep(refresh_sec)
        }
      }
    }
  }
}
