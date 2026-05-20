#!/usr/bin/env Rscript

# ──────────────────────────────────────────────────────────────────────────
# LAURDAN GP: MULTI-PANEL SUMMARY FIGURE
#
# Assembles one portrait PDF page per experiment from pre-computed data:
#   Header  : species name, sample ID, and collection/experiment metadata
#   A.      : P/T GP landscape heatmap (no legend)
#   B.      : GP over time, colored by temperature; shared legend to the right
#   C.      : Start and end emission spectra side-by-side; legend on right
#
# REQUIRES: gp_cubette.R must have been run first so that
#   results/<GP_method>/<sample>_GP_values_<GP_method>.csv exists.
#   GP is NOT recalculated here — it is read directly from those CSVs.
#   Spectrum panels read the first and last raw .tsv files and apply
#   SG-smoothing only (fast).
#
# Run from the project root:
#   Rscript scripts/gp_summary_figure.R
#
# Edited: 05/19/2026
# ──────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(signal)
  library(MBA)
  library(shadowtext)
  library(ggrepel)
  library(patchwork)
  library(cowplot)
})

# ============================================================
# SAMPLES
# ============================================================
run_all_samples <- FALSE
samples <- c(
  "Bolinopsis_D1339bw9_wproti_040826"
)

# ============================================================
# PATHS
# ============================================================
raw_data_root <- "data/raw"
meta_file     <- "data/cubette_metadata.xlsx"
output_dir    <- "figures/summary"

# ============================================================
# GP PARAMETERS  (must match gp_cubette.R)
# ============================================================
GP_method             <- "sg_band"    # "sg_band" or "lognormal"
subtract_blank        <- TRUE
blank_pattern         <- "^blank_.*\\.tsv$"
pat_data              <- "^spectrum_.*\\.tsv$"
n_measurements_per_step <- 3
time_unit             <- "mins"
sg_poly_order         <- 3
sg_window             <- 101

# Wavelength / band definitions (for spectrum panel display only)
blue_band        <- c(420, 460)
green_band       <- c(470, 510)
blue_peak_range  <- c(430, 450)
green_peak_range <- c(485, 505)
wl_blue          <- 440
wl_green         <- 490
min_wl           <- 390

# ============================================================
# AXIS SCALE PARAMETERS
# ============================================================
# GP (landscape colour / over-time y-axis):
#   "global"     auto-compute from all experiment CSVs
#   "auto"       per-sample from that sample's CSV
#   c(min, max)  manual, e.g. c(-0.6, 0.1)
gp_scale <- "global"
#gp_scale <- c(-0.6, 0.1)

# Spectrum intensity y-axis:
#   "global"     auto-compute from all experiments
#   "auto"       per-sample
#   c(min, max)  manual, e.g. c(0, 2000)
#intensity_scale <- "global"
intensity_scale <- c(0, 2000)

# ============================================================
# DERIVED PATHS
# ============================================================
results_dir <- file.path("results", GP_method)

# ============================================================
# HELPERS : SPECTRUM LOADING
# ============================================================

read_blank <- function(dir, pat) {
  candidates <- list.files(dir, pattern = pat, full.names = TRUE)
  if (length(candidates) == 0) return(NULL)
  read_tsv(candidates[1], show_col_types = FALSE)
}

load_spectrum <- function(f, blank = NULL) {
  s <- read_tsv(f, show_col_types = FALSE) %>%
    rename(wl = Wavelength, intensity = Intensity) %>%
    mutate(wl = as.numeric(wl), intensity = as.numeric(intensity)) %>%
    arrange(wl)
  if (!is.null(blank)) {
    bi <- approx(blank$wl, blank$intensity, xout = s$wl, rule = 2)$y
    s  <- mutate(s, intensity = intensity - bi)
  }
  s
}

scan_intensity_range <- function(sample_dirs) {
  all_int <- unlist(lapply(sample_dirs, function(smp) {
    d <- file.path(raw_data_root, smp)
    if (!dir.exists(d)) return(NULL)
    bl <- NULL
    if (subtract_blank) {
      bc <- list.files(d, pattern = "^blank_.*\\.tsv$", full.names = TRUE)
      if (length(bc) > 0) {
        bl <- tryCatch(
          read_tsv(bc[which.max(file.info(bc)$mtime)], show_col_types = FALSE) %>%
            rename(wl = Wavelength, intensity = Intensity) %>%
            mutate(wl = as.numeric(wl), intensity = as.numeric(intensity)),
          error = function(e) NULL)
      }
    }
    fls <- list.files(d, pattern = "^spectrum_.*\\.tsv$", full.names = TRUE)
    unlist(lapply(fls, function(f) {
      tryCatch({
        s_df <- read_tsv(f, show_col_types = FALSE)
        wl  <- as.numeric(s_df$Wavelength)
        int <- as.numeric(s_df$Intensity)
        if (!is.null(bl)) int <- int - approx(bl$wl, bl$intensity, xout = wl, rule = 2)$y
        int[wl >= 400 & wl <= 600]
      }, error = function(e) NULL)
    }))
  }))
  c(min(all_int, na.rm = TRUE), max(all_int, na.rm = TRUE) * 1.05)
}

# ============================================================
# HELPERS : GP DATA (read from pre-computed CSV)
# ============================================================

load_gp_csv <- function(sample_name) {
  csv_path <- file.path(results_dir,
    paste0(sample_name, "_GP_values_", GP_method, ".csv"))
  if (!file.exists(csv_path)) {
    warning("CSV not found for ", sample_name,
            " — run gp_cubette.R first.\n  Expected: ", csv_path)
    return(NULL)
  }
  read_csv(csv_path, show_col_types = FALSE) %>%
    mutate(P_act = Press_psi / 14.5038, T_act = Temp, gp = GP) %>%
    dplyr::filter(is.finite(gp))
}

group_ot_steps <- function(rows, n_per_step) {
  if (n_per_step >= 2) {
    rows %>% arrange(file_index) %>%
      mutate(step_id = ceiling(row_number() / n_per_step)) %>%
      group_by(step_id) %>%
      summarise(
        n         = sum(is.finite(GP)),
        gp_sd     = sd(GP, na.rm = TRUE),
        gp        = mean(GP, na.rm = TRUE),
        Timestamp = as_datetime(mean(as.numeric(Timestamp), na.rm = TRUE), tz = "UTC"),
        Press_psi = mean(Press_psi, na.rm = TRUE),
        T_act     = mean(Temp, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      mutate(
        gp_se    = gp_sd / sqrt(n),
        time_min = as.numeric(difftime(Timestamp, min(Timestamp), units = time_unit)),
        P_act    = Press_psi / 14.5038
      )
  } else {
    rows %>% arrange(file_index) %>%
      mutate(
        step_id = row_number(), n = 1, gp = GP,
        gp_sd = 0, gp_se = 0,
        T_act = Temp, P_act = Press_psi / 14.5038
      ) %>%
      select(step_id, n, gp, gp_sd, gp_se, Timestamp,
             Press_psi, T_act, P_act, time_min)
  }
}

compute_global_gp_limits <- function() {
  csv_files <- list.files(results_dir,
    pattern = paste0("_GP_values_", GP_method, "\\.csv$"),
    full.names = TRUE)
  all_gp <- unlist(lapply(csv_files, function(f)
    tryCatch(read_csv(f, show_col_types = FALSE)$GP, error = function(e) NULL)))
  all_gp <- all_gp[is.finite(all_gp)]
  if (length(all_gp) == 0) { warning("No GP values — GP scale will be auto"); return(NULL) }
  pad <- diff(range(all_gp)) * 0.05
  c(min(all_gp) - pad, max(all_gp) + pad)
}

compute_pt_limits <- function(gpdata)
  list(T_min = min(gpdata$T_act, na.rm = TRUE), T_max = max(gpdata$T_act, na.rm = TRUE),
       P_min = min(gpdata$P_act, na.rm = TRUE), P_max = max(gpdata$P_act, na.rm = TRUE))

# ============================================================
# METADATA FORMATTING
# ============================================================

format_depth_temp <- function(depth_fig, depth, temp) {
  if (is.na(depth_fig) || depth_fig == "")
    return(list(depth_str = paste0(depth, "m"), temp_str = paste0(temp, "ºC")))
  if (depth_fig == "MBA")
    return(list(depth_str = "Cultured at surface",
                temp_str  = paste0("Cultured at ", temp, "ºC")))
  if (depth_fig == "surface")
    return(list(depth_str = paste0("~surface (", depth, "m)"),
                temp_str  = paste0(temp, "ºC")))
  list(depth_str = paste0(depth_fig, "m"), temp_str = paste0(temp, "ºC"))
}

format_gp_calc <- function() {
  base <- if (subtract_blank) "Blank subtraction,\n" else ""
  meth <- switch(GP_method,
    sg_band   = "SG weighted band avg",
    lognormal = "log-normal decomposition",
    GP_method)
  paste0(base, meth)
}

# ============================================================
# FIGURE PANEL FUNCTIONS
# ============================================================

# ── Header panel ───────────────────────────────────────────

make_header_panel <- function(meta_row) {
  species   <- meta_row$species
  sample_no <- meta_row$sample_no
  exp_date  <- format(as.Date(meta_row$exp_date), "%m/%d/%Y")
  col_date  <- format(as.Date(meta_row$col_date), "%m/%d/%Y")
  col_loc   <- meta_row$col_loc
  type_str  <- meta_row$type
  dt        <- format_depth_temp(meta_row$depth_fig, meta_row$depth, meta_row$temp)

  gp_parts <- strsplit(format_gp_calc(), "\n")[[1]]
  gp_line1 <- gp_parts[1]
  gp_line2 <- if (length(gp_parts) > 1) gp_parts[2] else ""

  sp_esc    <- gsub('"', "'", species)
  sno_esc   <- gsub('"', "'", sample_no)
  treat     <- trimws(meta_row$treat)
  sno_part  <- if (!is.na(treat) && treat != "none") {
    paste0("(", sno_esc, ") + ", gsub('"', "'", treat))
  } else {
    paste0("(", sno_esc, ")")
  }
  title_str <- if (isTRUE(trimws(meta_row$text) == "italic")) {
    paste0('bolditalic("', sp_esc, '")~bold("', sno_part, '")')
  } else {
    paste0('bold("', sp_esc, ' ', sno_part, '")')
  }

  ll <- 0.00; lv <- 0.23   # left label | left value x
  rl <- 0.52; rv <- 0.77   # right label | right value x
  sz <- 4.2                 # body text size (≈ 15 pt)

  # Title at y=0.97; data rows start at 0.62 (gap gives clear separation).
  # Row spacing 0.15 keeps rows compact within the remaining height.
  rows <- c(0.62, 0.47, 0.32, 0.17)

  ggplot() +
    theme_void() +
    theme(plot.background = element_rect(fill = "grey92", colour = "grey70", linewidth = 0.8),
          plot.margin     = margin(3, 4, 3, 2, "pt")) +
    # expand=c(0,0) removes the default 5% axis padding so annotations at y=0/1
    # map exactly to the edges of the coordinate area — no phantom blank space.
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    annotate("text", x = 0.00, y = 0.9, label = title_str, parse = TRUE,
             hjust = 0, vjust = 1, size = 6.5) +
    # Left labels
    annotate("text", x = ll, y = rows[1], label = "Experiment date:", hjust = 0, vjust = 1, fontface = "bold", size = sz) +
    annotate("text", x = ll, y = rows[2], label = "GP calculation:",  hjust = 0, vjust = 1, fontface = "bold", size = sz) +
    annotate("text", x = ll, y = rows[4], label = "Tissue type:",     hjust = 0, vjust = 1, fontface = "bold", size = sz) +
    # Left values
    annotate("text", x = lv, y = rows[1], label = exp_date, hjust = 0, vjust = 1, size = sz) +
    annotate("text", x = lv, y = rows[2], label = gp_line1, hjust = 0, vjust = 1, size = sz) +
    annotate("text", x = lv, y = rows[4], label = type_str, hjust = 0, vjust = 1, size = sz) +
    { if (nzchar(gp_line2))
        annotate("text", x = lv, y = rows[3], label = gp_line2, hjust = 0, vjust = 1, size = sz)
    } +
    # Right labels
    annotate("text", x = rl, y = rows[1], label = "Collection depth:",    hjust = 0, vjust = 1, fontface = "bold", size = sz) +
    annotate("text", x = rl, y = rows[2], label = "Collection temp:",     hjust = 0, vjust = 1, fontface = "bold", size = sz) +
    annotate("text", x = rl, y = rows[3], label = "Collection date:",     hjust = 0, vjust = 1, fontface = "bold", size = sz) +
    annotate("text", x = rl, y = rows[4], label = "Collection location:", hjust = 0, vjust = 1, fontface = "bold", size = sz) +
    # Right values
    annotate("text", x = rv, y = rows[1], label = dt$depth_str, hjust = 0, vjust = 1, size = sz) +
    annotate("text", x = rv, y = rows[2], label = dt$temp_str,  hjust = 0, vjust = 1, size = sz) +
    annotate("text", x = rv, y = rows[3], label = col_date,     hjust = 0, vjust = 1, size = sz) +
    annotate("text", x = rv, y = rows[4], label = col_loc,      hjust = 0, vjust = 1, size = sz)
}

# ── A. Landscape panel (no legend) ────────────────────────

make_landscape_panel <- function(gpdata, pt_lims, gp_limits, native_pt = NULL) {
  if (nrow(gpdata) < 3) {
    return(ggplot() + theme_void() + labs(title = "Landscape: insufficient data"))
  }

  surf <- MBA::mba.surf(gpdata[, c("T_act", "P_act", "gp")],
                        no.X = 150, no.Y = 150, extend = TRUE)
  grid <- surf$xyz.est
  tib  <- expand.grid(T_act = grid$x, P_act = grid$y) %>%
    mutate(gp = as.vector(grid$z)) %>% drop_na(gp)

  ggplot(tib, aes(T_act, P_act)) +
    geom_raster(aes(fill = gp), interpolate = TRUE) +
    geom_contour(aes(z = gp), color = "white", linewidth = 0.25) +
    geom_point(data = gpdata, aes(T_act, P_act), color = "black", size = 1.4) +
    {
      if (!is.null(native_pt))
        geom_shadowtext(
          data = data.frame(T_act = native_pt$T,
                            P_act = max(native_pt$P, pt_lims$P_min)),
          aes(T_act, P_act), label = "*",
          family = "Helvetica", fontface = "bold", size = 12,
          colour = "black", bg.colour = "white", bg.r = 0.05, inherit.aes = FALSE)
    } +
    scale_fill_viridis_c(limits = gp_limits, direction = -1, name = "Laurdan GP") +
    scale_y_reverse() +
    coord_cartesian(xlim = c(pt_lims$T_min, pt_lims$T_max),
                    ylim = c(pt_lims$P_min, pt_lims$P_max),
                    clip = "off") +
    theme_classic() +
    theme(legend.position = "none",
          plot.title      = element_blank(),
          axis.title      = element_text(size = 10),
          axis.text       = element_text(size = 9)) +
    labs(x = "Temperature (ºC)", y = "Pressure (bar)") +
    annotate("text", x = -Inf, y = -Inf, label = "A.",
             hjust = 3, vjust = 1.5, fontface = "bold", size = 4)
}

# ── B. GP over-time panel (no built-in legend) ─────────────

make_overtime_panel <- function(data_plot, gp_limits) {
  ggplot(data_plot, aes(x = time_min, y = gp, color = T_act)) +
    { if (nrow(data_plot) > 1) geom_line(aes(group = 1), alpha = 0.4, linewidth = 0.5) } +
    geom_point(size = 1.2) +
    { if (any(is.finite(data_plot$gp_se)))
        geom_errorbar(aes(ymin = gp - gp_se, ymax = gp + gp_se),
                      width = 0.3, alpha = 0.6, color = "black") } +
    geom_text_repel(aes(label = paste0(round(P_act), " bar")),
                    size = 1.5, max.overlaps = 30, segment.size = 0.2) +
    scale_color_gradient(low = "blue", high = "red",
                         name = "Temp (°C)", limits = c(0, 25)) +
    scale_y_continuous(limits = gp_limits) +
    scale_x_continuous(limits = c(0, 330)) +
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme(legend.position = "none",
          plot.title      = element_blank(),
          axis.title      = element_text(size = 10),
          axis.text       = element_text(size = 9)) +
    labs(x = paste0("Time (", time_unit, ")"), y = "Laurdan GP") +
    annotate("text", x = -Inf, y = Inf, label = "B.",
             hjust = 3, vjust = 1.5, fontface = "bold", size = 4)
}

# ── B. Combined right-side legend ──────────────────────────

make_combined_legend <- function(gp_limits, has_native_pt = FALSE) {
  gp_dummy <- ggplot(
    data.frame(x = 1, y = seq(gp_limits[1], gp_limits[2], length.out = 50),
               z = seq(gp_limits[1], gp_limits[2], length.out = 50)),
    aes(x, y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(direction = -1, limits = gp_limits, name = "Laurdan GP",
                         guide = guide_colorbar(title.position = "top",
                                                barwidth = 0.8, barheight = 3.5)) +
    theme_void() + theme(legend.position = "right",
                         legend.title = element_text(size = 9, face = "bold"),
                         legend.text  = element_text(size = 8))
  leg_gp <- get_legend(gp_dummy)

  temp_dummy <- ggplot(
    data.frame(x = 1, y = seq(0, 25, length.out = 50),
               z = seq(0, 25, length.out = 50)),
    aes(x, y, color = z)) +
    geom_point() +
    scale_color_gradient(low = "blue", high = "red", limits = c(0, 25),
                         name = "Temp (°C)",
                         guide = guide_colorbar(title.position = "top",
                                                barwidth = 0.8, barheight = 3.5)) +
    theme_void() + theme(legend.position = "right",
                         legend.title = element_text(size = 9, face = "bold"),
                         legend.text  = element_text(size = 8))
  leg_temp <- get_legend(temp_dummy)

  if (has_native_pt) {
    leg_npt <- ggdraw() +
      draw_label("*", x = 0.15, y = 0.65, hjust = 0.95, vjust = 0.75,
                 fontface = "bold",  size = 30, colour = "black") +
      draw_label("Native P/T", x = 0.55, y = 0.65, hjust = 0.50,   vjust = 0.5,
                 fontface = "plain", size = 9,  colour = "black")
    plot_grid(leg_gp, leg_temp, leg_npt, ncol = 1, rel_heights = c(1, 1, 0.3), align = "v")
  } else {
    plot_grid(leg_gp, leg_temp, ncol = 1, rel_heights = c(1, 1), align = "v")
  }
}

# ── C. Spectrum panel ──────────────────────────────────────

make_spectrum_panel <- function(spec_file, blank, ylim_spec,
                                title, show_legend = FALSE,
                                panel_label = NULL) {
  sg_win <- if (sg_window %% 2 == 0) sg_window + 1 else sg_window

  bl_named <- if (!is.null(blank)) {
    blank %>% rename(wl = Wavelength, intensity = Intensity) %>%
      mutate(wl = as.numeric(wl), intensity = as.numeric(intensity))
  } else NULL

  spec <- load_spectrum(spec_file, bl_named) %>%
    mutate(intensity_sg = signal::sgolayfilt(intensity, p = sg_poly_order, n = sg_win))

  b_idx  <- which(spec$wl >= blue_peak_range[1]  & spec$wl <= blue_peak_range[2])
  g_idx  <- which(spec$wl >= green_peak_range[1] & spec$wl <= green_peak_range[2])
  bpk_wl <- spec$wl[b_idx[which.max(spec$intensity_sg[b_idx])]]
  gpk_wl <- spec$wl[g_idx[which.max(spec$intensity_sg[g_idx])]]

  bdf <- dplyr::filter(spec, wl >= blue_band[1],  wl <= blue_band[2])  %>% mutate(intensity_sg = pmax(intensity_sg, 0))
  gdf <- dplyr::filter(spec, wl >= green_band[1], wl <= green_band[2]) %>% mutate(intensity_sg = pmax(intensity_sg, 0))

  p <- ggplot(spec, aes(wl)) +
    geom_area(data = bdf, aes(y = intensity_sg, fill = "Blue band"),  alpha = 0.35) +
    geom_area(data = gdf, aes(y = intensity_sg, fill = "Green band"), alpha = 0.35) +
    geom_line(aes(y = intensity,    color = "Raw"),         alpha = 0.4, linewidth = 0.6) +
    geom_line(aes(y = intensity_sg, color = "SG-smoothed"), linewidth = 0.8) +
    geom_vline(aes(xintercept = wl_blue,  linetype = "Exact 440/490"), color = "gray40") +
    geom_vline(aes(xintercept = wl_green, linetype = "Exact 440/490"), color = "gray40") +
    geom_vline(data = data.frame(x = c(bpk_wl, gpk_wl)),
               aes(xintercept = x, linetype = "SG peaks"), color = "navy") +
    scale_color_manual(name = NULL, values = c("Raw" = "black", "SG-smoothed" = "red")) +
    scale_fill_manual(name = NULL, values = c("Blue band" = "steelblue", "Green band" = "seagreen")) +
    scale_linetype_manual(name = NULL, values = c("Exact 440/490" = "dashed", "SG peaks" = "dashed")) +
    scale_x_continuous(limits = c(400, 600)) +
    coord_cartesian(ylim = ylim_spec, clip = "off") +
    theme_bw() +
    theme(legend.position        = if (show_legend) c(0.99, 0.99) else "none",
          legend.justification   = c(1, 1),
          legend.background      = element_rect(fill = alpha("white", 0.75), colour = NA),
          legend.margin          = margin(2, 4, 2, 4, "pt"),
          legend.spacing.y       = unit(1, "pt"),
          legend.text            = element_text(size = 6),
          legend.key.width       = unit(0.7, "lines"),
          legend.key.size        = unit(0.65, "lines"),
          plot.title             = element_text(size = 10, hjust = 0.5),
          axis.title             = element_text(size = 10),
          axis.text              = element_text(size = 9)) +
    labs(title = title, x = "Wavelength (nm)", y = "Intensity")

  if (!is.null(panel_label))
    p <- p + annotate("text", x = -Inf, y = Inf, label = panel_label,
                      hjust = 3, vjust = 1.5, fontface = "bold", size = 4)
  p
}

# ── Full page assembly ─────────────────────────────────────

make_summary_page <- function(sample_name, meta_row, gpdata, gp_limits, ylim_spec) {

  dir_data <- file.path(raw_data_root, sample_name)
  message("  Building panels for ", sample_name)

  # Native P/T
  native_pt <- NULL
  if (!is.null(meta_row) && nrow(meta_row) == 1 && !is.na(meta_row$depth)) {
    native_P  <- meta_row$depth * 0.1006 + 1.013
    native_pt <- list(T = meta_row$temp, P = native_P)
  }

  # Per-sample GP limits if "auto"
  gp_lims_use <- if (!is.null(gp_limits)) gp_limits else {
    pad <- diff(range(gpdata$gp, na.rm = TRUE)) * 0.05
    c(min(gpdata$gp, na.rm = TRUE) - pad, max(gpdata$gp, na.rm = TRUE) + pad)
  }

  # ── A: Landscape ──────────────────────────────────────────
  pt_lims     <- compute_pt_limits(gpdata)
  p_landscape <- make_landscape_panel(gpdata, pt_lims, gp_lims_use, native_pt)

  # ── B: Over-time ──────────────────────────────────────────
  data_plot  <- group_ot_steps(gpdata %>%
                                 mutate(GP = gp, Temp = T_act),
                               n_measurements_per_step)
  p_overtime <- make_overtime_panel(data_plot, gp_lims_use)

  has_npt  <- !is.null(native_pt)
  p_legend <- make_combined_legend(gp_lims_use, has_npt)

  # ── C: Spectra ────────────────────────────────────────────
  blank      <- read_blank(dir_data, blank_pattern)
  spec_files <- sort(list.files(dir_data, pattern = pat_data, full.names = TRUE))
  f_start    <- spec_files[1]
  f_end      <- spec_files[length(spec_files)]

  parse_T <- function(f) {
    m <- str_match(basename(f),
           "^spectrum_\\d{8}T\\d+_idx\\d+_P-?\\d+_T([0-9.]+)\\.tsv$")
    if (!is.na(m[1, 2])) as.numeric(m[1, 2]) else NA
  }

  p_spec_start <- make_spectrum_panel(
    f_start, blank, ylim_spec,
    title       = paste0("Start of experiment (T=", parse_T(f_start), "ºC)"),
    show_legend = FALSE, panel_label = "C.")
  p_spec_end <- make_spectrum_panel(
    f_end, blank, ylim_spec,
    title       = paste0("End of experiment (T=", parse_T(f_end), "ºC)"),
    show_legend = TRUE, panel_label = NULL)

  # ── Header ────────────────────────────────────────────────
  p_header <- if (!is.null(meta_row) && nrow(meta_row) == 1) {
    make_header_panel(meta_row)
  } else {
    ggdraw() +
      draw_grob(grid::rectGrob(gp = grid::gpar(fill = "grey92", col = "grey70", lwd = 0.8))) +
      draw_label(sample_name, x = 0.03, y = 0.5, hjust = 0, fontface = "bold", size = 14)
  }

  # ── Assemble ──────────────────────────────────────────────
  row1 <- wrap_plots(p_landscape, p_overtime, plot_spacer(), p_legend,
                     ncol = 4, widths = c(1.4, 2.45, 0.01, 0.55))
  row2 <- wrap_plots(p_spec_start, p_spec_end, ncol = 2)
  wrap_plots(p_header, plot_spacer(), row1, row2, plot_spacer(), ncol = 1,
             heights = c(0.14, 0.01, 0.25, 0.30, 0.29))
}

# ============================================================
# PRE-COMPUTATION
# ============================================================

raw_data_root <- normalizePath(raw_data_root, mustWork = TRUE)
results_dir   <- normalizePath(results_dir,   mustWork = FALSE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

if (run_all_samples) {
  samples <- list.dirs(raw_data_root, full.names = FALSE, recursive = FALSE)
  samples <- samples[nzchar(samples)]
  message("run_all_samples = TRUE — found ", length(samples), " experiment folders")
}

# Load metadata
meta <- if (file.exists(meta_file)) {
  read_excel(meta_file)
} else {
  warning("cubette_metadata.xlsx not found — header metadata will be blank")
  NULL
}

# Resolve GP limits
message("Resolving GP axis limits...")
gp_limits_resolved <- if (is.numeric(gp_scale)) {
  gp_scale
} else if (gp_scale == "global") {
  message("  Reading global GP limits from CSVs...")
  lims <- compute_global_gp_limits()
  if (!is.null(lims)) message("  GP range: [", round(lims[1], 3), ", ", round(lims[2], 3), "]")
  lims
} else {
  NULL   # "auto" — per sample
}

# Resolve intensity limits
message("Resolving spectrum intensity limits...")
ylim_spec_resolved <- if (is.numeric(intensity_scale)) {
  intensity_scale
} else if (intensity_scale == "global") {
  message("  Scanning raw spectra for intensity range...")
  lims <- scan_intensity_range(samples)
  message("  Intensity range: [", round(lims[1]), ", ", round(lims[2]), "]")
  lims
} else {
  NULL   # "auto"
}

# ============================================================
# MAIN LOOP
# ============================================================

for (sample_name in samples) {

  message("\n── Processing: ", sample_name, " ──")

  # Load pre-computed GP data
  gpdata <- load_gp_csv(sample_name)
  if (is.null(gpdata)) next

  # Look up metadata
  meta_row <- if (!is.null(meta)) {
    dplyr::filter(meta, file_name == sample_name)
  } else NULL
  if (!is.null(meta_row) && nrow(meta_row) == 0) {
    warning("  No metadata row found for ", sample_name, " — header will be minimal")
    meta_row <- NULL
  }

  # Per-sample limits if "auto"
  ylim_spec_use <- ylim_spec_resolved %||% scan_intensity_range(sample_name)

  # Build and save page
  page <- tryCatch(
    make_summary_page(sample_name, meta_row, gpdata, gp_limits_resolved, ylim_spec_use),
    error = function(e) {
      warning("Failed to build page for ", sample_name, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(page)) next

  pdf_out <- file.path(output_dir,
    paste0(sample_name, "_summary_", GP_method, ".pdf"))
  ggsave(pdf_out, page, width = 8.5, height = 11, dpi = 300)
  message("Saved: ", pdf_out)
}
