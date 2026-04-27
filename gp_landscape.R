#!/usr/bin/env Rscript

# ────────────────────────────────────────────────
# LAURDAN GP LANDSCAPE VISUALIZATION (CONTOUR PLOTS)

# This script can be used to visualize results from C-Laurdan spectra captured
# from Cubette experiments by plotting a landscape of C-Laurdan GP across 
# pressure and temperature.

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
# - Exports a PDF of Laurdan-GP Landscape
# - Options to create a GIF of results over time, and live visualization

# Edited: 12/17/2025
# ────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggpubr)
  library(gridExtra)
  library(MBA)
  library(magick)
})

# ============================================================
# PARAMETERS (edit these between experiments)
# ============================================================

sample_name <- "Llyria_D1442d4_041726"   # Change sample name/date
dir_data <- "."                          # Move to directory with experiment + blank spectra
pat_data <- "^spectrum_.*\\.tsv$"        # Pattern to search for spectrum files

live_mode   <- FALSE      # TRUE = live updating
make_gif    <- FALSE      # TRUE = also export GIF
refresh_sec <- 5          # Time between refresh for live mode

# Blank subtraction (LED ON, Cuvette with ASW only, use same integration time)
subtract_blank <- TRUE
blank_pattern <- "^blank_.*\\.tsv$"

# GP method options:
# - "exact"    = exact 440/490 (raw, standardized spectroscopy GP)
# - "sg_exact" = SG-smoothed 440/490 (apply Savitzky–Golay smoothing filter)
# - "sg_band"     = SG-smoothed Δλ-weighted band GP (normalized integration) - USE THIS ONE
# - "sg_peaks" = SG-smoothed peak-based GP 
GP_method <- "sg_band"

# Savitzky–Golay parameters
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

# Fixed colour scale for Laurdan GP
# Set to NA if don't want fixed scale across graphs
GP_min <- -0.6
GP_max <- 0.1
#GP_min <- NA
#GP_max <- NA

# Tags for file labels
scale_tag <- if (is.na(GP_min) || is.na(GP_max)) {
  "autoscale"
} else {
  paste0("scale_", GP_min, "_", GP_max)
}
method_tag <- paste(GP_method, scale_tag, sep = "_")

# Output files
pdf_out <- file.path(dir_data, paste0(sample_name, "_lgp_landscape_", method_tag, ".pdf"))
gif_out <- file.path(dir_data, paste0(sample_name, "_lgp_landscape_", method_tag, ".gif"))

# ============================================================
# LOAD SPECTRA
# ============================================================

# Load all spectra files
read_spectra <- function(dir, pat) {
  files <- list.files(dir, pattern = pat, full.names = TRUE)
  if (length(files) == 0) return(tibble())
  
  spectra <- map_df(files, function(f) {
    readr::read_tsv(f, show_col_types = FALSE) %>%
      mutate(file = basename(f))
  })
  
  # Extract metadata from file name
  meta <- tibble(file = basename(files)) %>%
    mutate(
      parts = stringr::str_match(
        file,
        "spectrum_(\\d{8}T\\d+)_idx(\\d+)_P(-?[0-9]+)_T([0-9.]+)\\.tsv$"
      ),
      clock = suppressWarnings(
        as.POSIXct(parts[, 2], "%Y%m%dT%H%M%OS", tz = "UTC")
      ),
      rep   = as.integer(parts[, 3]),
      P_act = as.numeric(parts[, 4]) / 14.5038,   # psi → bar
      T_act = as.numeric(parts[, 5])
    ) %>%
    select(file, clock, rep, P_act, T_act)
  
  left_join(spectra, meta, by = "file")
}

# Load blank spectrum file
read_blank <- function(dir, pat) {
  f <- list.files(dir, pattern = pat, full.names = TRUE)[1]
  if (is.na(f)) stop("Blank spectrum not found (pattern: ", pat, ")")
  readr::read_tsv(f, show_col_types = FALSE)
}

# Subtract blank if enabled
subtract_blank_if_enabled <- function(raw, blank) {
  if (!subtract_blank || is.null(blank)) return(raw)
  
  blank_i <- approx(
    x    = blank$Wavelength,
    y    = blank$Intensity,
    xout = raw$Wavelength,
    rule = 2
  )$y
  
  raw$Intensity <- raw$Intensity - blank_i
  raw
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
# BUILD GP TABLE — ONE ROW PER FILE
# ============================================================

build_gp_table <- function(raw) {
  raw %>%
    group_by(file, clock, P_act, T_act) %>%
    nest() %>%
    mutate(gp = map_dbl(data, compute_gp)) %>%
    select(-data) %>%
    dplyr::filter(is.finite(gp))
}

# ============================================================
# GLOBAL AXIS LIMITS
# ============================================================

# Set limits for Laurdan GP colour scale
resolve_gp_limits <- function(raw_all) {
  
  # Manual scale → use exactly what user set
  if (!is.na(GP_min) && !is.na(GP_max)) {
    return(c(GP_min, GP_max))
  }
  
  # Auto scale → compute ONCE from full dataset
  gp_all <- build_gp_table(raw_all)$gp
  range(gp_all, na.rm = TRUE)
}

# Set limits for x and y axes (temp and pressure)
compute_global_limits <- function(raw_all) {
  list(
    T_min = min(raw_all$T_act, na.rm = TRUE),
    T_max = max(raw_all$T_act, na.rm = TRUE),
    P_min = min(raw_all$P_act, na.rm = TRUE),
    P_max = max(raw_all$P_act, na.rm = TRUE)
  )
}


# ============================================================
# PLOTTING FUNCTIONS
# ============================================================

state_plot <- function(raw, lims) {
  raw %>%
    ggplot(aes(T_act, P_act, color = clock)) +
    geom_point(size = 2.2, alpha = 0.9) +
    scale_color_gradient(low = "black", high = "red", guide = "none") +
    scale_y_reverse() +
    coord_cartesian(
      xlim = c(lims$T_min, lims$T_max),
      ylim = c(lims$P_min, lims$P_max)
    ) +
    theme_pubr() +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 5, 5, 5),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x  = element_text(size = 18),
      axis.text.y  = element_text(size = 18),
      plot.title = element_text(size = 16, hjust = 0.5)
    ) +
    labs(
      title = "Cubette state",
      x = "Temperature (°C)",
      y = "Pressure (bar)"
    )
}

contour_plot <- function(gpdata, lims, gp_limits) {
  
  if (nrow(gpdata) < 3) {
    return(
      ggplot() +
        scale_fill_viridis_c(
          limits = gp_limits,
          direction = -1,
          name = "Laurdan GP"
        ) +
        scale_y_reverse() +
        coord_cartesian(
          xlim = c(lims$T_min, lims$T_max),
          ylim = c(lims$P_min, lims$P_max)
        ) +
        theme_pubr() +
        labs(
          title = sample_name,
          subtitle = paste(
            ifelse(subtract_blank, "blank-subtracted", "raw"),
            "| GP method:", GP_method,
            "| waiting for GP data"
          ),
          x = "Temperature (ºC)",
          y = "Pressure (bar)"
        )
    )
  }
  
  # ---- MBA Surface ----
  surf <- MBA::mba.surf(
    gpdata[, c("T_act", "P_act", "gp")],
    no.X   = 150,
    no.Y   = 150,
    extend = TRUE
  )
  
  grid <- surf$xyz.est
  
  tib <- expand.grid(
    T_act = grid$x,
    P_act = grid$y
  ) %>%
    mutate(gp = as.vector(grid$z)) %>%
    drop_na(gp)
  
  # ---- FINAL PLOT ----
  ggplot(tib, aes(T_act, P_act)) +
    geom_raster(aes(fill = gp), interpolate = TRUE) +
    geom_contour(
      aes(z = gp),
      color = "white",
      linewidth = 0.25
    ) +
    geom_point(
      data = gpdata,
      aes(T_act, P_act),
      color = "black",
      size = 1.6
    ) +
    scale_fill_viridis_c(
      limits = gp_limits,
      direction = -1,
      name = "Laurdan GP"
    ) +
    scale_y_reverse() +
    coord_cartesian(
      xlim = c(lims$T_min, lims$T_max),
      ylim = c(lims$P_min, lims$P_max)
    ) +
    theme_pubr() +
    theme(
      legend.position = "right",
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
        "| GP method:", GP_method
      ),
      x = "Temperature (ºC)",
      y = "Pressure (bar)"
    )
}


# ============================================================
# STATIC PDF RENDER
# ============================================================

render_static <- function(raw, lims, outfile) {
  gpdata <- build_gp_table(raw)
  gp_limits <- resolve_gp_limits(raw)
  p <- contour_plot(gpdata, lims, gp_limits)
  
  print(p)
  
  ggsave(outfile, p, width = 6, height = 6, dpi = 300)
}


# ============================================================
# FRAME RENDER FOR GIF
# ============================================================

render_frame <- function(raw, lims, gp_limits, outfile, time_label) {
  gpdata <- build_gp_table(raw)
  
  p_state <- state_plot(raw, lims) +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x  = element_text(size = 14),
      axis.text.y  = element_text(size = 14),
      plot.title = element_text(size = 16, hjust = 0)
    )  
  p_cont <- contour_plot(gpdata, lims, gp_limits) +
    ggtitle(
      paste(sample_name),
      subtitle = paste("Time", time_label)
    ) +
    theme(
      plot.title = element_text(size = 16, hjust = 0),
      plot.subtitle = element_text(size = 14, hjust = 0),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x  = element_text(size = 14),
      axis.text.y  = element_text(size = 14)
    )
  
  combined <- grid.arrange(p_state, p_cont, ncol = 1, heights = c(1, 2))
  ggsave(outfile, combined, width = 6.5, height = 9, dpi = 150)
}

# ============================================================
# GIF GENERATION
# ============================================================

make_gp_gif <- function() {
  
  raw_all <- read_spectra(dir_data, pat_data)
  blank   <- if (subtract_blank) read_blank(dir_data, blank_pattern) else NULL
  
  if (subtract_blank) {
    raw_all <- raw_all %>%
      split(.$file) %>%
      lapply(subtract_blank_if_enabled, blank = blank) %>%
      bind_rows()
  }
  
  raw_all <- raw_all %>% arrange(clock)
  lims    <- compute_global_limits(raw_all)
  gp_limits <- resolve_gp_limits(raw_all)
  
  times <- sort(unique(raw_all$clock))
  frame_dir <- file.path(dir_data, "gif_frames")
  dir.create(frame_dir, showWarnings = FALSE)
  
  message("Rendering ", length(times), " GIF frames...")
  
  for (i in seq_along(times)) {
    
    frame_raw <- raw_all[raw_all$clock <= times[i], ]
    
    frame_file <- file.path(frame_dir, sprintf("frame_%04d.png", i))
    
    render_frame(
      raw   = frame_raw,
      lims  = lims,
      gp_limits = gp_limits,
      outfile    = frame_file,
      time_label = format(times[i], "%H:%M:%S")
    )
  }
  
  gif <- image_read(list.files(frame_dir, full.names = TRUE))
  gif <- image_animate(gif, fps = 10)
  image_write(gif, gif_out)
  
  message("GIF saved to ", gif_out)
}

# ============================================================
# EXECUTION
# ============================================================

blank <- if (subtract_blank) read_blank(dir_data, blank_pattern) else NULL

if (!live_mode) {
  
  raw <- read_spectra(dir_data, pat_data)
  
  if (subtract_blank) {
    raw <- raw %>%
      split(.$file) %>%
      lapply(subtract_blank_if_enabled, blank = blank) %>%
      bind_rows()
  }
  
  lims <- compute_global_limits(raw)
  
  # Static contour-only PDF
  render_static(raw, lims, pdf_out)
  
  # Optional GIF with state + contour and fixed axes
  if (make_gif) {
    make_gp_gif()
  }
  
} else {
  # Live mode
  while (TRUE) {
    raw <- read_spectra(dir_data, pat_data)
    if (subtract_blank) {
      raw <- raw %>%
        split(.$file) %>%
        lapply(subtract_blank_if_enabled, blank = blank) %>%
        bind_rows()
    }
    lims <- compute_global_limits(raw)
    render_static(raw, lims, pdf_out, tsv_out)
    Sys.sleep(refresh_sec)
  }
}

