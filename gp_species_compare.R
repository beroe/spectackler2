#!/usr/bin/env Rscript

# ──────────────────────────────────────────────────────────────────────────
# CROSS-SPECIES GP COMPARISON
#
# Reads pre-computed GP CSVs from results/<GP_method>/ and plots:
#   1) GP vs Pressure
#   2) GP vs Temperature
#
# Colors, groups, native P/T, and treatment status from cubette_metadata.xlsx.
# Native collection P/T marked with a species-colored asterisk (white outline).
#
# Run from the project root:
#   Rscript scripts/gp_species_compare.R
#
# Edited: 05/20/2026
# ──────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(shadowtext)
})

# Pin dplyr functions that are commonly masked by other packages (e.g. stats::filter)
filter <- dplyr::filter
select <- dplyr::select

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================
# OPTIONS
# ============================================================

GP_method   <- "sg_band"
results_dir <- file.path("results", GP_method)
meta_file   <- "data/cubette_metadata.xlsx"
output_dir  <- "figures/gp_compare"

# ── Averaging mode ──────────────────────────────────────────
# "all"   : P plot averages GP over ALL temperatures per pressure step;
#           T plot averages GP over ALL pressures per temperature step.
# "fixed" : P plot filters to data where Temp ≈ target_temp;
#           T plot filters to data where Press_bar ≈ target_press.
avg_mode     <- "all"
target_temp  <- 5      # °C  — used only when avg_mode = "fixed"
target_press <- 2      # bar — used only when avg_mode = "fixed"
temp_tol     <- 0.5    # ±°C  tolerance for "fixed" filter
press_tol    <- 0.5    # ±bar tolerance for "fixed" filter

# Bin size for rounding steps to a common grid
press_round_bar <- 5   # round pressure to nearest N bar
temp_round_deg  <- 1   # round temperature to nearest N °C

# ── Treatment handling ──────────────────────────────────────
# FALSE (default): exclude ProK and Protease inhibitor samples.
# TRUE: include — linetype encodes treatment:
#         solid          = none (control)
#         dashed         = Protease inhibitor
#         dotted         = ProK
# When TRUE, treated samples automatically inherit the control color
# for that species (same species, treat = "none"), so lines share a
# color and differ only in style.
include_treatments <- TRUE

# ── Group / species filtering ───────────────────────────────
# NULL: include all groups in one combined plot.
# Character vector: restrict to those groups e.g., c("lobate","cydippid") or "all"
# Options:
# - "lobate"
# - "beroid"
# - "cydippid"
# "all": produce a combined plot AND one plot per group.
groups_include <- c("beroid")

# TRUE: save one plot per group in addition to the combined plot.
# Automatically set to TRUE when groups_include = "all".
plot_by_group  <- FALSE

# ── Output ──────────────────────────────────────────────────
output_prefix <- "all_species"

# ── GP y-axis limits ────────────────────────────────────────
GP_ylim <- NULL
# GP_ylim <- c(-0.7, 0.1)

# Fallback colors for samples with NA in the metadata color column
fallback_palette <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
                      "#66a61e", "#e6ab02", "#a6761d", "#666666")

# ============================================================
# LOAD METADATA
# ============================================================

meta <- read_excel(meta_file) %>%
  mutate(
    Press_bar_native = depth * 0.1006 + 1.013,
    Temp_native      = temp,
    treat            = trimws(treat)
  )

# For treated samples, override color with their species' control color
# so treated and control lines share a hue — only linetype distinguishes them.
control_colors <- meta %>%
  filter(treat == "none") %>%
  group_by(species) %>%
  slice(1) %>%
  ungroup() %>%
  select(species, control_color = color)

meta <- meta %>%
  left_join(control_colors, by = "species") %>%
  mutate(color = if_else(treat != "none" & !is.na(control_color),
                         control_color, color)) %>%
  select(-control_color)

# Assign fallback colors to any remaining NA entries
na_rows <- which(is.na(meta$color))
if (length(na_rows) > 0) {
  meta$color[na_rows] <- fallback_palette[
    ((seq_along(na_rows) - 1) %% length(fallback_palette)) + 1]
  message("Assigned fallback colors to: ",
          paste(meta$file_name[na_rows], collapse = ", "))
}

# ============================================================
# LOAD GP CSVs AND JOIN METADATA
# ============================================================

csv_files <- list.files(results_dir,
                        pattern = paste0("_GP_values_", GP_method, "\\.csv$"),
                        full.names = TRUE)

if (length(csv_files) == 0)
  stop("No GP CSV files found in ", results_dir, " — run gp_cubette.R first")

message("Found ", length(csv_files), " GP CSV files")

gp_raw <- map_dfr(csv_files, function(f) {
  fn <- sub(paste0("_GP_values_", GP_method, "\\.csv$"), "", basename(f))
  read_csv(f, show_col_types = FALSE) %>%
    mutate(file_name = fn, Press_bar = Press_psi / 14.5038)
})

gp_all <- gp_raw %>%
  left_join(
    meta %>% select(file_name, species, sample_no, group, color, treat,
                    Press_bar_native, Temp_native, depth_fig),
    by = "file_name"
  ) %>%
  filter(is.finite(GP))

unmatched <- setdiff(unique(gp_raw$file_name), meta$file_name)
if (length(unmatched) > 0)
  warning("CSVs not found in metadata (skipped): ",
          paste(unmatched, collapse = ", "))

# ============================================================
# FILTER: treatments and groups
# ============================================================

if (!include_treatments) {
  n_before <- nrow(gp_all)
  gp_all   <- gp_all %>% filter(treat == "none")
  dropped  <- n_before - nrow(gp_all)
  if (dropped > 0) message("Excluded ", dropped, " rows with treatments (set include_treatments = TRUE to keep)")
}

# Keep a group-unfiltered copy for per-species treatment plots
gp_all_ungrouped <- gp_all

if (!is.null(groups_include) && !identical(groups_include, "all")) {
  gp_all <- gp_all %>% filter(group %in% groups_include)
}

if (nrow(gp_all) == 0)
  stop("No data remains after filtering — check groups_include / include_treatments")

# ============================================================
# SUMMARISE HELPERS
# ============================================================

summarise_for_axis <- function(data, axis = c("pressure", "temperature")) {
  axis <- match.arg(axis)
  
  if (axis == "pressure") {
    if (avg_mode == "fixed")
      data <- data %>% filter(abs(Temp - target_temp) <= temp_tol)
    data %>%
      mutate(x_step = round(Press_bar / press_round_bar) * press_round_bar) %>%
      group_by(file_name, species, sample_no, group, color, treat,
               Press_bar_native, Temp_native, depth_fig, x_step) %>%
      summarise(GP_mean = mean(GP, na.rm = TRUE),
                GP_sd   = sd(GP,   na.rm = TRUE),
                n       = sum(is.finite(GP)), .groups = "drop") %>%
      mutate(GP_se = GP_sd / sqrt(pmax(n, 1)))
    
  } else {
    if (avg_mode == "fixed")
      data <- data %>% filter(abs(Press_bar - target_press) <= press_tol)
    data %>%
      mutate(x_step = round(Temp / temp_round_deg) * temp_round_deg) %>%
      group_by(file_name, species, sample_no, group, color, treat,
               Press_bar_native, Temp_native, depth_fig, x_step) %>%
      summarise(GP_mean = mean(GP, na.rm = TRUE),
                GP_sd   = sd(GP,   na.rm = TRUE),
                n       = sum(is.finite(GP)), .groups = "drop") %>%
      mutate(GP_se = GP_sd / sqrt(pmax(n, 1)))
  }
}

native_marker_df <- function(summary_df, axis = c("pressure", "temperature")) {
  axis       <- match.arg(axis)
  native_col <- if (axis == "pressure") "Press_bar_native" else "Temp_native"
  
  summary_df %>%
    filter(!is.na(.data[[native_col]])) %>%
    group_by(file_name, species, sample_no, group, color, treat,
             Press_bar_native, Temp_native, depth_fig) %>%
    summarise(
      x_native = first(.data[[native_col]]),
      GP_mean  = approx(x_step, GP_mean,
                        xout = first(.data[[native_col]]),
                        rule = 2)$y,
      .groups  = "drop"
    )
}

# ============================================================
# PLOT HELPER
# ============================================================

ltype_map <- c("none"                = "solid",
               "Protease inhibitor"  = "dashed",
               "ProK"                = "dotted")

make_plot <- function(summary_df, axis = c("pressure", "temperature"),
                      title_str = NULL) {
  axis      <- match.arg(axis)
  xlab      <- if (axis == "pressure") "Pressure (bar)" else "Temperature (°C)"
  eb_w      <- if (axis == "pressure") press_round_bar * 0.6 else temp_round_deg * 0.8
  
  color_map <- setNames(summary_df$color, summary_df$file_name) %>%
    .[!duplicated(names(.))]
  
  meta_one <- summary_df[!duplicated(summary_df$file_name), ]
  
  # Legend order: group (alphabetical) then depth shallow → deep
  meta_ordered  <- meta_one %>% arrange(group, Press_bar_native)
  ordered_files <- meta_ordered$file_name
  
  depth_fmt  <- ifelse(grepl("^[0-9]+$", meta_one$depth_fig),
                       paste0(meta_one$depth_fig, " m"),
                       meta_one$depth_fig)
  depth_part <- ifelse(!is.na(meta_one$depth_fig),
                       paste0(", ", depth_fmt), "")
  temp_part  <- ifelse(!is.na(meta_one$Temp_native),
                       paste0(", ", round(meta_one$Temp_native), "°C"), "")
  label_map <- setNames(
    paste0(meta_one$species, " (", meta_one$sample_no,
           depth_part, temp_part, ")"),
    meta_one$file_name)
  
  sub_str <- switch(avg_mode,
                    fixed = if (axis == "pressure") paste("Temperature:", target_temp, "°C")
                    else paste("Pressure:", target_press, "bar"),
                    all   = if (axis == "pressure") "averaged across all temperatures"
                    else "averaged across all pressures")
  
  markers <- native_marker_df(summary_df, axis)
  
  p <- ggplot(summary_df,
              aes(x = x_step, y = GP_mean,
                  color = file_name, linetype = treat, group = file_name)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    { if (any(is.finite(summary_df$GP_se) & summary_df$GP_se > 0))
      geom_errorbar(aes(ymin = GP_mean - GP_se, ymax = GP_mean + GP_se),
                    width = eb_w, alpha = 0.8) } +
    { if (nrow(markers) > 0)
      geom_shadowtext(
        data        = markers,
        aes(x = x_native, y = GP_mean, colour = file_name),
        label       = "*",
        family      = "Helvetica",
        fontface    = "bold",
        size        = 14,
        vjust       = 0.75,
        bg.colour   = "white",
        bg.r        = 0.05,
        show.legend = FALSE,
        inherit.aes = FALSE
      )
    } +
    scale_color_manual(values = color_map, labels = label_map,
                       breaks = ordered_files,
                       name = NULL, drop = TRUE) +
    scale_linetype_manual(values = ltype_map, name = "Treatment", drop = TRUE) +
    scale_y_continuous(limits = GP_ylim) +
    theme_bw() +
    theme(legend.position = "right",
          plot.title      = element_text(size = 14, face = "bold"),
          plot.subtitle   = element_text(size = 12, colour = "grey30"),
          axis.title      = element_text(size = 14),
          axis.text       = element_text(size = 12),
          legend.text     = element_text(size = 8)) +
    labs(title = title_str, subtitle = sub_str, x = xlab, y = "Laurdan GP")
  
  if (!include_treatments) p <- p + guides(linetype = "none")
  p
}

# ============================================================
# SAVE HELPER
# ============================================================

suffix_p <- if (avg_mode == "fixed") paste0("_T", target_temp)  else "_allT"
suffix_t <- if (avg_mode == "fixed") paste0("_P", target_press) else "_allP"

save_pair <- function(data, prefix, title_label = NULL, out_dir = output_dir) {
  if (nrow(data) == 0) {
    message("  No data for '", prefix, "' — skipping"); return(invisible())
  }
  
  s_press <- summarise_for_axis(data, "pressure")
  s_temp  <- summarise_for_axis(data, "temperature")
  
  if (nrow(s_press) == 0 && nrow(s_temp) == 0) {
    message("  No summarised data for '", prefix, "' — skipping"); return(invisible())
  }
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  title_p <- paste0(title_label %||% prefix, " - GP vs Pressure")
  title_t <- paste0(title_label %||% prefix, " - GP vs Temperature")
  
  if (nrow(s_press) > 0) {
    p1 <- make_plot(s_press, "pressure",     title_p)
    f1 <- file.path(out_dir, paste0(prefix, "_GPvsPress", suffix_p, ".pdf"))
    ggsave(f1, p1, width = 9, height = 6, dpi = 300)
    message("  Saved: ", basename(f1))
  }
  if (nrow(s_temp) > 0) {
    p2 <- make_plot(s_temp,  "temperature",  title_t)
    f2 <- file.path(out_dir, paste0(prefix, "_GPvsTemp",  suffix_t, ".pdf"))
    ggsave(f2, p2, width = 9, height = 6, dpi = 300)
    message("  Saved: ", basename(f2))
  }
}

# ============================================================
# RUN
# ============================================================

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

message("\nSaving combined comparison plots...")
effective_prefix <- if (!is.null(groups_include) && !identical(groups_include, "all")) {
  paste(sort(groups_include), collapse = "_")
} else {
  output_prefix
}
effective_title <- if (!is.null(groups_include) && !identical(groups_include, "all")) {
  paste(sort(groups_include), collapse = " + ")
} else {
  "All species"
}
save_pair(gp_all, effective_prefix, title_label = effective_title)

do_by_group <- isTRUE(plot_by_group) || identical(groups_include, "all")
if (do_by_group) {
  grps <- sort(unique(gp_all$group))
  grps <- grps[!is.na(grps)]
  for (grp in grps) {
    message("Saving group: ", grp)
    save_pair(gp_all %>% filter(group == grp),
              prefix      = grp,
              title_label = grp)
  }
}

# Per-species treatment comparison plots (only when include_treatments = TRUE)
# Uses gp_all_ungrouped so treatment plots are never limited by groups_include.
if (include_treatments) {
  species_with_treat <- gp_all_ungrouped %>%
    group_by(species) %>%
    filter(n_distinct(treat) > 1) %>%
    pull(species) %>% unique() %>% sort()
  
  if (length(species_with_treat) > 0) {
    treat_dir <- file.path(output_dir, "treatments")
    message("\nSaving per-species treatment comparison plots to ", treat_dir, "...")
    for (sp in species_with_treat) {
      message("  Species: ", sp)
      save_pair(gp_all_ungrouped %>% filter(species == sp),
                prefix      = gsub(" ", "_", sp),
                title_label = paste0(sp, " - treatment comparison"),
                out_dir     = treat_dir)
    }
  }
}

message("\nDone.")
