# =============================================================================
# nm_export_engine_OT.R
# Figure-export engine shared by 1.Soluble_OldTrt_v2.Rmd, 2.Insoluble_OldTrt_v2.Rmd
# and 3.Solubility_OldTrt_v2.Rmd (moved verbatim out of the R Markdown; the three
# original in-document copies were byte-identical; no code changes). Presentation/export only: it never touches data,
# models, or statistics.
#
# Sourced by the Rmd AFTER the settings, figure-export settings and the first
# helper chunk, because it reads out_dir, nm_style, nm_dims, nm_panel_dims,
# nm_layout and similar objects from the session at definition time.
#
# What it provides: figure registration, PNG + editable SVG export, separate
# legends, Inkscape-safe SVG clean-up (removes the svglite page rectangle) and
# re-export of single panels or size families without recomputing any model.
#
# Console helpers once the Rmd has been run:
#   list_panels()                              find exact panel IDs
#   panel_info("Volcano_Exercise_vs_Control")  inspect one panel
#   set_panel_size("PCA", 60, 52); reexport("PCA")   resize one panel (mm)
#   set_family_size("family_interaction_one", 16, 16); reexport_family(...)
#   export_all_panels(); figure_export_manifest()
#
# NOTE: several functions (nm_legend_dims_for, nm_resolve_dims,
# make_svg_inkscape_safe, check_svg_inkscape_safe, nm_save_separate_legend,
# save_plot_png, save_heatmap_png, save_nm_svg, save_nm_png) are defined twice
# in the original code. The second (registry) definition silently replaces the
# first, so the first set is dead code. Both sets are kept here unchanged so that
# behaviour is identical; see CHANGES for details.
# =============================================================================

nm_legend_dims_for <- function(size_key, plot = NULL) {
  # Explicit dimensions attached by a custom plotting function take priority.
  if (!is.null(plot)) {
    w_attr <- attr(plot, "nm_legend_width_mm", exact = TRUE)
    h_attr <- attr(plot, "nm_legend_height_mm", exact = TRUE)
    if (!is.null(w_attr) && !is.null(h_attr)) {
      return(c(width = as.numeric(w_attr), height = as.numeric(h_attr)))
    }
  }

  dims <- nm_legend_dims[[size_key]]
  if (is.null(dims)) dims <- nm_legend_dims[["generic_single"]]
  dims
}

nm_clamp <- function(x, lo, hi) {
  max(lo, min(hi, x))
}

mm_to_in <- function(mm) {
  mm / 25.4
}

in_to_mm <- function(x) {
  x * 25.4
}

# Choose a panel size from its output filename. This keeps existing analysis
# calls intact while routing every figure through one manuscript design system.
nm_size_key_for_file <- function(filename_no_ext) {
  x <- gsub("\\\\", "/", as.character(filename_no_ext))
  b <- basename(sub("\\.(png|svg)$", "", x, ignore.case = TRUE))
  is_ratio <- grepl("Ratio_OldTrt|Ratio_|Insoluble.Soluble|Ins.Sol", x, ignore.case = TRUE)

  dplyr::case_when(
    grepl("^PCA$", b, ignore.case = TRUE) ~ "family_pca",
    grepl("Scree", b, ignore.case = TRUE) ~ "family_scree",
    grepl("Solubility_OldTreatments", b, ignore.case = TRUE) ~ "family_solubility",
    grepl("Volcano_Combined", b, ignore.case = TRUE) ~ "family_volcano_grid",
    grepl("Volcano_", b, ignore.case = TRUE) & is_ratio ~ "family_ratio_volcano",
    grepl("Volcano_", b, ignore.case = TRUE) ~ "family_volcano",
    grepl("Heatmap_UPR", b, ignore.case = TRUE) ~ "family_heatmap_upr",
    grepl("^Heatmap_", b, ignore.case = TRUE) ~ "family_heatmap_sig",
    grepl("GO_BP_Lollipop", b, ignore.case = TRUE) ~ "family_go",
    grepl("Female_vs_Male_effect|SexFx", b, ignore.case = TRUE) ~ "family_sex_single",
    grepl("SexFx_pair|Female_vs_Male_effect_pair", b, ignore.case = TRUE) ~ "family_sex_pair",
    grepl("Reversal_Pooled_Exercise_Rapamycin", b, ignore.case = TRUE) ~ "family_reversal_pair",
    grepl("Reversal_FemaleMale", b, ignore.case = TRUE) ~ "family_reversal_single",
    grepl("Reversal_CombinedSex", b, ignore.case = TRUE) ~ "family_reversal_single",
    grepl("Reversal_Pooled", b, ignore.case = TRUE) ~ "family_reversal_single",
    grepl("MeanSEM_.*_All$", b, ignore.case = TRUE) ~ "family_interaction_all",
    grepl("^MeanSEM_", b, ignore.case = TRUE) ~ "family_interaction_one",
    TRUE ~ "generic_single"
  )
}

# Dynamic height is used only for figures whose row count varies substantially.
# Width always comes from nm_dims; height remains bounded to preserve legibility.
nm_resolve_dims <- function(
  filename_no_ext,
  old_width_in = NULL,
  old_height_in = NULL,
  size_key = NULL,
  width_mm = NULL,
  height_mm = NULL
) {
  if (is.null(size_key)) {
    size_key <- nm_size_key_for_file(filename_no_ext)
  }

  dims <- nm_dims[[size_key]]
  if (is.null(dims)) stop("Unknown Nature Metabolism size key: ", size_key)

  w <- unname(dims[["width"]])
  h <- unname(dims[["height"]])

  # Preserve a reasonable aspect ratio for variable-length heatmaps/GO panels.
  if (
    !is.null(old_width_in) && !is.null(old_height_in) &&
    is.finite(old_width_in) && is.finite(old_height_in) && old_width_in > 0
  ) {
    aspect_h <- (old_height_in / old_width_in) * w

    if (identical(size_key, "family_heatmap_sig")) {
      h <- nm_clamp(aspect_h, 65, 135)
    }

    if (identical(size_key, "family_go")) {
      h <- nm_clamp(aspect_h, 52, 105)
    }

    if (identical(size_key, "family_interaction_all")) {
      h <- nm_clamp(aspect_h, 55, 95)
    }
  }

  if (!is.null(width_mm)) w <- width_mm
  if (!is.null(height_mm)) h <- height_mm

  list(
    key = size_key,
    width_mm = w,
    height_mm = h
  )
}

theme_nm <- function(
  legend_position = "right",
  plot_title = TRUE,
  plot_subtitle = TRUE
) {
  ggplot2::theme_classic(
    base_family = nm_style$font_family,
    base_size = nm_style$axis_text
  ) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        family = nm_style$font_family,
        size = nm_style$axis_title,
        colour = "black",
        face = "plain"
      ),
      axis.text = ggplot2::element_text(
        family = nm_style$font_family,
        size = nm_style$axis_text,
        colour = "black"
      ),
      axis.line = ggplot2::element_line(
        linewidth = nm_style$axis_line,
        colour = "black"
      ),
      axis.ticks = ggplot2::element_line(
        linewidth = nm_style$axis_line,
        colour = "black"
      ),
      axis.ticks.length = grid::unit(1.0, "mm"),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(
        family = nm_style$font_family,
        size = nm_style$legend_title,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(
        family = nm_style$font_family,
        size = nm_style$legend_text
      ),
      legend.key.height = grid::unit(3.0, "mm"),
      legend.key.width = grid::unit(3.0, "mm"),
      plot.title = if (plot_title) {
        ggplot2::element_text(
          family = nm_style$font_family,
          size = nm_style$plot_title,
          face = "bold",
          margin = ggplot2::margin(b = 0.8, unit = "mm")
        )
      } else {
        ggplot2::element_blank()
      },
      plot.subtitle = if (plot_subtitle) {
        ggplot2::element_text(
          family = nm_style$font_family,
          size = nm_style$stat_text,
          colour = "grey35",
          margin = ggplot2::margin(b = 0.8, unit = "mm")
        )
      } else {
        ggplot2::element_blank()
      },
      plot.caption = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = "white",
        colour = "black",
        linewidth = nm_style$axis_line
      ),
      strip.text = ggplot2::element_text(
        family = nm_style$font_family,
        size = nm_style$strip_text,
        face = "bold",
        colour = "black"
      ),
      plot.margin = ggplot2::margin(
        1.8, 1.8, 1.8, 1.8,
        unit = "mm"
      )
    )
}

# Rescale oversized legacy geoms so that old plots match the same final-size
# design language without changing the underlying data/statistics.
nm_cap_param <- function(x, max_value) {
  if (is.null(x) || !is.numeric(x) || length(x) != 1L || !is.finite(x)) return(x)
  min(x, max_value)
}

nm_rescale_ggplot_layers <- function(p) {
  if (!inherits(p, "ggplot")) return(p)

  for (ii in seq_along(p$layers)) {
    layer <- p$layers[[ii]]
    geom_name <- class(layer$geom)[1]

    if (grepl("Geom(Point|Jitter)", geom_name)) {
      if (!is.null(layer$aes_params$size)) {
        layer$aes_params$size <- nm_cap_param(layer$aes_params$size, nm_style$point_max)
      }
      if (!is.null(layer$aes_params$stroke)) {
        layer$aes_params$stroke <- nm_cap_param(layer$aes_params$stroke, 0.45)
      }
    }

    if (grepl("Geom(Text|Label|TextRepel|LabelRepel)", geom_name)) {
      if (!is.null(layer$aes_params$size)) {
        layer$aes_params$size <- nm_cap_param(
          layer$aes_params$size,
          nm_style$stat_text / 2.845276
        )
      }
    }

    if (grepl("Geom(Line|Path|Segment|Abline|Hline|Vline|Smooth|Errorbar|Linerange|Crossbar)", geom_name)) {
      if (!is.null(layer$aes_params$linewidth)) {
        layer$aes_params$linewidth <- nm_cap_param(layer$aes_params$linewidth, nm_style$data_line)
      }
      if (!is.null(layer$aes_params$size)) {
        layer$aes_params$size <- nm_cap_param(layer$aes_params$size, nm_style$data_line)
      }
    }

    p$layers[[ii]] <- layer
  }

  p
}

nm_prepare_plot <- function(plot, legend_position = "none") {
  if (inherits(plot, "patchwork")) {
    if (!is.null(plot$patches$plots) && length(plot$patches$plots) > 0) {
      plot$patches$plots <- lapply(
        plot$patches$plots,
        function(x) nm_prepare_plot(x, legend_position = legend_position)
      )
    }
    plot <- plot & theme_nm(legend_position = legend_position)
    return(plot)
  }

  if (inherits(plot, "ggplot")) {
    plot <- nm_rescale_ggplot_layers(plot)
    plot <- plot + theme_nm(legend_position = legend_position)
  }

  plot
}

make_svg_inkscape_safe <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")

  # Remove only svglite's device-level background rectangle.
  x <- x[!grepl("^\\s*<rect width='100%' height='100%'", x)]

  add_style_property <- function(line, property, value) {
    if (grepl(paste0(property, "\\s*:"), line, perl = TRUE)) return(line)

    if (grepl("style='", line, fixed = TRUE)) {
      return(
        sub(
          "style='",
          paste0("style='", property, ": ", value, "; "),
          line,
          fixed = TRUE
        )
      )
    }

    sub(
      "\\s*/>$",
      paste0(" style='", property, ": ", value, ";' />"),
      line,
      perl = TRUE
    )
  }

  in_defs <- FALSE
  in_glyphgroup <- FALSE

  for (i in seq_along(x)) {
    line <- x[i]

    if (grepl("<defs(?:\\s|>)", line, perl = TRUE)) in_defs <- TRUE
    if (grepl("<g[^>]*class='glyphgroup'", line, perl = TRUE)) in_glyphgroup <- TRUE

    is_shape <- grepl(
      "^\\s*<(line|polyline|polygon|rect|circle|path)\\b",
      line,
      perl = TRUE
    )
    is_text <- grepl("^\\s*<text\\b", line, perl = TRUE)

    if (is_shape && !in_defs) {
      if (in_glyphgroup && grepl("^\\s*<path\\b", line, perl = TRUE)) {
        line <- add_style_property(line, "fill", "inherit")
        line <- add_style_property(line, "stroke", "none")
      } else {
        line <- add_style_property(line, "fill", "none")
        line <- add_style_property(line, "stroke", "#000000")
        line <- add_style_property(line, "stroke-linecap", "round")
        line <- add_style_property(line, "stroke-linejoin", "round")
        line <- add_style_property(line, "stroke-miterlimit", "10.00")
      }
    }

    if (is_text && !in_defs) {
      line <- add_style_property(line, "white-space", "pre")
    }

    x[i] <- line

    if (grepl("</g>", line, fixed = TRUE) && in_glyphgroup) in_glyphgroup <- FALSE
    if (grepl("</defs>", line, fixed = TRUE)) in_defs <- FALSE
  }

  # Prevent editable Inkscape text from being stretched/compressed after edits.
  x <- gsub("\\s+textLength='[^']*'", "", x, perl = TRUE)
  x <- gsub("\\s+lengthAdjust='[^']*'", "", x, perl = TRUE)

  writeLines(x, file, useBytes = TRUE)
  invisible(file)
}

check_svg_inkscape_safe <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")
  in_defs <- FALSE
  bad <- character(0)

  for (line in x) {
    if (grepl("<defs(?:\\s|>)", line, perl = TRUE)) in_defs <- TRUE

    if (!in_defs && grepl(
      "^\\s*<(line|polyline|polygon|rect|circle)\\b",
      line,
      perl = TRUE
    )) {
      if (!grepl("stroke\\s*:", line, perl = TRUE)) bad <- c(bad, line)
    }

    if (grepl("</defs>", line, fixed = TRUE)) in_defs <- FALSE
  }

  if (length(bad) > 0L) {
    stop(
      "SVG still contains drawable elements with CSS-only stroke styling: ",
      basename(file)
    )
  }

  invisible(TRUE)
}



nm_extract_legend <- function(plot) {
  # A custom figure can attach a pre-built legend grob (used for the combined
  # reversal figures where sex and reversal-category legends are constructed
  # manually).
  custom <- attr(plot, "nm_legend_grob", exact = TRUE)
  if (!is.null(custom)) return(custom)

  p_for_legend <- tryCatch(
    nm_prepare_plot(plot, legend_position = "right"),
    error = function(e) NULL
  )
  if (is.null(p_for_legend)) return(NULL)

  leg <- tryCatch(
    cowplot::get_legend(p_for_legend),
    error = function(e) NULL
  )

  if (is.null(leg) || inherits(leg, "zeroGrob")) return(NULL)
  leg
}

save_grob_svg <- function(grob, file, width_mm, height_mm, background = "white") {
  if (is.null(grob)) return(invisible(NULL))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(file)) try(unlink(file), silent = TRUE)

  device_open <- FALSE
  err <- tryCatch(
    {
      svglite::svglite(
        file = file,
        width = mm_to_in(width_mm),
        height = mm_to_in(height_mm),
        bg = background,
        system_fonts = list(sans = svg_font_family)
      )
      device_open <- TRUE
      grid::grid.newpage()
      grid::grid.draw(grob)
      grDevices::dev.off()
      device_open <- FALSE
      NULL
    },
    error = function(e) e,
    finally = {
      if (device_open) try(grDevices::dev.off(), silent = TRUE)
    }
  )

  if (!is.null(err)) stop("Could not save legend SVG: ", conditionMessage(err))
  if (!file.exists(file)) stop("Legend SVG was not created: ", file)

  make_svg_inkscape_safe(file)
  check_svg_inkscape_safe(file)
  invisible(file)
}

save_grob_png <- function(grob, file, width_mm, height_mm, background = "white") {
  if (is.null(grob)) return(invisible(NULL))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  ragg::agg_png(
    filename = file,
    width = width_mm,
    height = height_mm,
    units = "mm",
    res = nm_export$dpi,
    background = background
  )
  grid::grid.newpage()
  grid::grid.draw(grob)
  grDevices::dev.off()
  invisible(file)
}

nm_save_separate_legend <- function(plot, stub, size_key) {
  if (!isTRUE(nm_export$separate_legends)) return(invisible(NULL))

  leg <- nm_extract_legend(plot)
  if (is.null(leg)) return(invisible(NULL))

  dims <- nm_legend_dims_for(size_key, plot = plot)
  suffix <- nm_export$legend_suffix

  save_grob_png(
    leg,
    paste0(stub, suffix, ".png"),
    width_mm = unname(dims[["width"]]),
    height_mm = unname(dims[["height"]]),
    background = nm_export$background
  )

  save_grob_svg(
    leg,
    paste0(stub, suffix, ".svg"),
    width_mm = unname(dims[["width"]]),
    height_mm = unname(dims[["height"]]),
    background = nm_export$background
  )

  invisible(leg)
}

save_plot_svg <- function(plot, file, width_mm, height_mm, background = "white") {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(file)) try(unlink(file), silent = TRUE)

  p_export <- nm_prepare_plot(plot)
  device_open <- FALSE

  err <- tryCatch(
    {
      svglite::svglite(
        file = file,
        width = mm_to_in(width_mm),
        height = mm_to_in(height_mm),
        bg = background,
        system_fonts = list(sans = svg_font_family)
      )
      device_open <- TRUE
      print(p_export)
      grDevices::dev.off()
      device_open <- FALSE
      NULL
    },
    error = function(e) e,
    finally = {
      if (device_open) try(grDevices::dev.off(), silent = TRUE)
    }
  )

  if (!is.null(err)) stop("Could not save editable SVG: ", conditionMessage(err))
  if (!file.exists(file)) stop("SVG was not created: ", file)

  make_svg_inkscape_safe(file)
  check_svg_inkscape_safe(file)
  invisible(file)
}

save_heatmap_svg <- function(ht, file, width_mm, height_mm, background = "white") {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(file)) try(unlink(file), silent = TRUE)

  device_open <- FALSE
  err <- tryCatch(
    {
      svglite::svglite(
        file = file,
        width = mm_to_in(width_mm),
        height = mm_to_in(height_mm),
        bg = background,
        system_fonts = list(sans = svg_font_family)
      )
      device_open <- TRUE
      ComplexHeatmap::draw(
        ht,
        heatmap_legend_side = "right",
        annotation_legend_side = "right",
        merge_legend = FALSE,
        padding = grid::unit(c(2.0, 2.0, 2.0, 2.0), "mm")
      )
      grDevices::dev.off()
      device_open <- FALSE
      NULL
    },
    error = function(e) e,
    finally = {
      if (device_open) try(grDevices::dev.off(), silent = TRUE)
    }
  )

  if (!is.null(err)) stop("Could not save heatmap SVG: ", conditionMessage(err))
  if (!file.exists(file)) stop("Heatmap SVG was not created: ", file)

  make_svg_inkscape_safe(file)
  check_svg_inkscape_safe(file)
  invisible(file)
}

# Existing analysis code still calls save_plot_png()/save_heatmap_png(). These
# wrappers now route every output through the centralized Nature design system.
save_plot_png <- function(
  plot,
  filename_no_ext,
  width = 7.8,
  height = 6.2,
  dpi = 300,
  make_svg = TRUE,
  size_key = NULL,
  width_mm = NULL,
  height_mm = NULL
) {
  stub <- sub("\\.(png|svg)$", "", filename_no_ext, ignore.case = TRUE)
  out_png <- paste0(stub, ".png")
  out_svg <- paste0(stub, ".svg")
  safe_dir_create(dirname(stub))

  dims <- nm_resolve_dims(
    filename_no_ext = stub,
    old_width_in = width,
    old_height_in = height,
    size_key = size_key,
    width_mm = width_mm,
    height_mm = height_mm
  )

  # Save the guide(s) BEFORE removing them from the panel.
  nm_save_separate_legend(plot, stub = stub, size_key = dims$key)

  p_export <- nm_prepare_plot(
    plot,
    legend_position = if (isTRUE(nm_export$separate_legends)) "none" else "right"
  )

  ggplot2::ggsave(
    filename = out_png,
    plot = p_export,
    width = dims$width_mm,
    height = dims$height_mm,
    units = "mm",
    dpi = nm_export$dpi,
    device = ragg::agg_png,
    bg = nm_export$background,
    limitsize = FALSE
  )

  if (!file.exists(out_png) || file.info(out_png)$size <= 0) {
    stop("PNG was not saved correctly: ", out_png)
  }

  if (isTRUE(make_svg)) {
    save_plot_svg(
      plot = p_export,
      file = out_svg,
      width_mm = dims$width_mm,
      height_mm = dims$height_mm,
      background = nm_export$background
    )
  }

  invisible(out_png)
}

save_heatmap_png <- function(
  ht,
  filename_no_ext,
  width = 12,
  height = 8,
  dpi = 300,
  make_svg = TRUE,
  size_key = NULL,
  width_mm = NULL,
  height_mm = NULL
) {
  stub <- sub("\\.(png|svg)$", "", filename_no_ext, ignore.case = TRUE)
  out_png <- paste0(stub, ".png")
  out_svg <- paste0(stub, ".svg")
  safe_dir_create(dirname(stub))

  dims <- nm_resolve_dims(
    filename_no_ext = stub,
    old_width_in = width,
    old_height_in = height,
    size_key = size_key,
    width_mm = width_mm,
    height_mm = height_mm
  )

  ragg::agg_png(
    filename = out_png,
    width = dims$width_mm,
    height = dims$height_mm,
    units = "mm",
    res = nm_export$dpi,
    background = nm_export$background
  )

  ComplexHeatmap::draw(
    ht,
    heatmap_legend_side = "right",
    annotation_legend_side = "right",
    merge_legend = FALSE,
    padding = grid::unit(c(2.0, 2.0, 2.0, 2.0), "mm")
  )
  grDevices::dev.off()

  if (!file.exists(out_png) || file.info(out_png)$size <= 0) {
    stop("Heatmap PNG was not saved correctly: ", out_png)
  }

  if (isTRUE(make_svg)) {
    save_heatmap_svg(
      ht = ht,
      file = out_svg,
      width_mm = dims$width_mm,
      height_mm = dims$height_mm,
      background = nm_export$background
    )
  }

  invisible(out_png)
}

# Compatibility aliases used by existing sex-effect plotting sections.
nm_svg_dims <- nm_dims

save_nm_svg <- function(plot, file, size_key, width_mm = NULL, height_mm = NULL) {
  dims <- nm_resolve_dims(
    filename_no_ext = file,
    size_key = size_key,
    width_mm = width_mm,
    height_mm = height_mm
  )

  stub <- sub("\\.svg$", "", file, ignore.case = TRUE)
  nm_save_separate_legend(plot, stub = stub, size_key = dims$key)

  p_export <- nm_prepare_plot(
    plot,
    legend_position = if (isTRUE(nm_export$separate_legends)) "none" else "right"
  )

  save_plot_svg(p_export, file, dims$width_mm, dims$height_mm)
}

save_nm_png <- function(plot, file, size_key, width_mm = NULL, height_mm = NULL, dpi = 600) {
  dims <- nm_resolve_dims(
    filename_no_ext = file,
    size_key = size_key,
    width_mm = width_mm,
    height_mm = height_mm
  )
  p_export <- nm_prepare_plot(
    plot,
    legend_position = if (isTRUE(nm_export$separate_legends)) "none" else "right"
  )
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = file,
    plot = p_export,
    width = dims$width_mm,
    height = dims$height_mm,
    units = "mm",
    dpi = nm_export$dpi,
    bg = nm_export$background,
    device = ragg::agg_png,
    limitsize = FALSE
  )
  invisible(file)
}

# -----------------------------------------------------------------------------
# FIGURE EXPORT REGISTRY — ALL DIMENSIONS ARE IN MILLIMETRES
# -----------------------------------------------------------------------------
# Family defaults are derived from top-level exact IDs, not edited here.
# Examples after the document has been run once:
#   set_family_size("family_interaction_one", 16, 16)
#   reexport_family("family_interaction_one")
#
# For one specific panel:
#   set_panel_size("MeanSEM_Capza2_Exercise", 16, 16)
#   reexport("MeanSEM_Capza2_Exercise")
figure_specs <- nm_dims  # compatibility alias; nm_dims is authoritative
panel_size_overrides <- list()
legend_size_overrides <- list()
panel_registry <- new.env(parent = emptyenv())

nm_panel_id <- function(path) {
  basename(sub("\\.(png|svg)$", "", as.character(path), ignore.case = TRUE))
}

set_family_size <- function(size_key, width_mm, height_mm) {
  if (!size_key %in% names(figure_specs)) {
    stop("Unknown figure family: ", size_key)
  }
  nm_dims[[size_key]] <<- c(width = width_mm, height = height_mm)
  figure_specs[[size_key]] <<- nm_dims[[size_key]]
  invisible(nm_dims[[size_key]])
}

set_panel_size <- function(panel_id, width_mm, height_mm) {
  stopifnot(is.character(panel_id), length(panel_id) == 1L,
            is.finite(width_mm), is.finite(height_mm),
            width_mm > 0, height_mm > 0)
  # Update the SAME settings list shown in the top Nature helper.
  nm_panel_dims[[panel_id]] <<- c(width = width_mm, height = height_mm)
  panel_size_overrides[[panel_id]] <<- NULL
  invisible(nm_panel_dims[[panel_id]])
}

set_legend_size <- function(panel_id, width_mm, height_mm) {
  x <- legend_size_overrides
  x[[panel_id]] <- c(width = width_mm, height = height_mm)
  legend_size_overrides <<- x
  invisible(x[[panel_id]])
}

nm_legend_dims_for <- function(size_key, plot = NULL, panel_id = NULL) {
  if (!is.null(panel_id)) {
    ov <- legend_size_overrides[[panel_id]]
    if (!is.null(ov)) return(ov)
  }

  if (!is.null(plot)) {
    w_attr <- attr(plot, "nm_legend_width_mm", exact = TRUE)
    h_attr <- attr(plot, "nm_legend_height_mm", exact = TRUE)
    if (!is.null(w_attr) && !is.null(h_attr)) {
      return(c(width = as.numeric(w_attr), height = as.numeric(h_attr)))
    }
  }

  dims <- nm_legend_dims[[size_key]]
  if (is.null(dims)) dims <- nm_legend_dims[["generic_single"]]
  dims
}

nm_resolve_dims <- function(
  filename_no_ext,
  old_width_in = NULL,
  old_height_in = NULL,
  size_key = NULL,
  width_mm = NULL,
  height_mm = NULL
) {
  if (is.null(size_key)) {
    size_key <- nm_size_key_for_file(filename_no_ext)
  }

  dims <- nm_dims[[size_key]]
  if (is.null(dims)) stop("Unknown Nature Metabolism size key: ", size_key)

  w <- unname(dims[["width"]])
  h <- unname(dims[["height"]])

  # Layout with data-dependent height is resolved in the heatmap exporter.
  # No fixed cap on protein heatmaps; other panels use the dimensions above.
  panel_id <- nm_panel_id(filename_no_ext)
  # The exact-ID entry shown in the top Nature helper ALWAYS wins.
  # An earlier session-only size cannot silently override it.
  ov <- nm_panel_dims[[panel_id]]
  if (is.null(ov)) ov <- panel_size_overrides[[panel_id]]
  if (!is.null(ov)) {
    w <- unname(ov[["width"]])
    h <- unname(ov[["height"]])
  }

  # Explicit arguments always have highest priority.
  if (!is.null(width_mm)) w <- width_mm
  if (!is.null(height_mm)) h <- height_mm

  list(
    key = size_key,
    panel_id = panel_id,
    width_mm = w,
    height_mm = h
  )
}

# Calculate exact heatmap height from number of protein rows. The same final
# size is used for PNG/SVG and for reexport() without touching fitted models.
nm_heatmap_row_count <- function(ht) {
  if (inherits(ht, "Heatmap") && !is.null(ht@matrix)) return(nrow(ht@matrix))
  if (inherits(ht, "HeatmapList") && length(ht@ht_list)) {
    x <- ht@ht_list[[1L]]
    if (inherits(x, "Heatmap") && !is.null(x@matrix)) return(nrow(x@matrix))
  }
  NA_integer_
}
nm_adjust_heatmap_dims <- function(dims, ht) {
  # Explicit finite panel height always wins; NA means automatic.
  if (is.finite(dims$height_mm)) return(dims)
  nr <- nm_heatmap_row_count(ht)
  if (!is.finite(nr)) nr <- 25L
  is_upr <- identical(dims$key, "family_heatmap_upr")
  base <- if (is_upr) nm_layout$upr_base_height_mm else nm_layout$heatmap_base_height_mm
  per <- if (is_upr) nm_layout$upr_mm_per_protein else nm_layout$heatmap_mm_per_protein
  dims$height_mm <- max(nm_layout$minimum_heatmap_height_mm, base + per * nr)
  dims
}

register_panel <- function(
  panel_id,
  object,
  stub,
  type = c("plot", "heatmap"),
  size_key,
  old_width_in = NULL,
  old_height_in = NULL
) {
  type <- match.arg(type)
  assign(
    panel_id,
    list(
      panel_id = panel_id,
      object = object,
      stub = stub,
      type = type,
      size_key = size_key,
      old_width_in = old_width_in,
      old_height_in = old_height_in
    ),
    envir = panel_registry
  )
  invisible(panel_id)
}

# -----------------------------------------------------------------------------
# SVG post-processing: keep native editable vectors, but remove the svglite
# full-page background rectangle. PNG exports remain white.
# -----------------------------------------------------------------------------
make_svg_inkscape_safe <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")

  is_page_bg <- grepl(
    "^[[:space:]]*<rect[[:space:]]+width=['\\\"]100%['\\\"][[:space:]]+height=['\\\"]100%['\\\"]",
    x,
    perl = TRUE
  )
  x <- x[!is_page_bg]

  add_style_property <- function(line, property, value) {
    if (grepl(paste0(property, "[[:space:]]*:"), line, perl = TRUE)) return(line)

    if (grepl("style='", line, fixed = TRUE)) {
      return(sub(
        "style='",
        paste0("style='", property, ": ", value, "; "),
        line,
        fixed = TRUE
      ))
    }

    sub(
      "[[:space:]]*/>$",
      paste0(" style='", property, ": ", value, ";' />"),
      line,
      perl = TRUE
    )
  }

  in_defs <- FALSE
  in_glyphgroup <- FALSE

  for (i in seq_along(x)) {
    line <- x[i]

    if (grepl("<defs(?:[[:space:]]|>)", line, perl = TRUE)) in_defs <- TRUE
    if (grepl("<g[^>]*class='glyphgroup'", line, perl = TRUE)) in_glyphgroup <- TRUE

    is_shape <- grepl(
      "^[[:space:]]*<(line|polyline|polygon|rect|circle|path)([[:space:]>])",
      line,
      perl = TRUE
    )
    is_text <- grepl("^[[:space:]]*<text([[:space:]>])", line, perl = TRUE)

    if (is_shape && !in_defs) {
      if (in_glyphgroup && grepl("^[[:space:]]*<path([[:space:]>])", line, perl = TRUE)) {
        line <- add_style_property(line, "fill", "inherit")
        line <- add_style_property(line, "stroke", "none")
      } else {
        line <- add_style_property(line, "fill", "none")
        line <- add_style_property(line, "stroke", "#000000")
        line <- add_style_property(line, "stroke-linecap", "round")
        line <- add_style_property(line, "stroke-linejoin", "round")
        line <- add_style_property(line, "stroke-miterlimit", "10.00")
      }
    }

    if (is_text && !in_defs) {
      line <- add_style_property(line, "white-space", "pre")
    }

    x[i] <- line

    if (grepl("</g>", line, fixed = TRUE) && in_glyphgroup) in_glyphgroup <- FALSE
    if (grepl("</defs>", line, fixed = TRUE)) in_defs <- FALSE
  }

  x <- gsub("[[:space:]]+textLength='[^']*'", "", x, perl = TRUE)
  x <- gsub("[[:space:]]+lengthAdjust='[^']*'", "", x, perl = TRUE)

  # Ungroup-safe: detach styles from the removable outer svglite group.
  # Clip-path definitions and internal groups remain intact.
  outer <- grep("^[[:space:]]*<g class=['\"]svglite['\"]>[[:space:]]*$", x)
  if (length(outer) == 1L) {
    closing <- grep("^[[:space:]]*</g>[[:space:]]*$", x)
    closing <- closing[closing > outer]
    if (length(closing)) {
      x <- gsub(".svglite ", "svg ", x, fixed = TRUE)
      x <- x[-c(outer, tail(closing, 1L))]
    }
  }
  writeLines(x, file, useBytes = TRUE)
  invisible(file)
}

check_svg_inkscape_safe <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")

  if (any(grepl(
    "^[[:space:]]*<rect[[:space:]]+width=['\\\"]100%['\\\"][[:space:]]+height=['\\\"]100%['\\\"]",
    x,
    perl = TRUE
  ))) {
    stop("SVG still contains a page/background rectangle: ", basename(file))
  }

  in_defs <- FALSE
  bad <- character(0)

  for (line in x) {
    if (grepl("<defs(?:[[:space:]]|>)", line, perl = TRUE)) in_defs <- TRUE

    if (
      !in_defs &&
      grepl("^[[:space:]]*<(line|polyline|polygon|rect|circle)([[:space:]>])", line, perl = TRUE)
    ) {
      if (!grepl("stroke[[:space:]]*:", line, perl = TRUE)) bad <- c(bad, line)
    }

    if (grepl("</defs>", line, fixed = TRUE)) in_defs <- FALSE
  }

  if (length(bad) > 0L) {
    stop(
      "SVG still contains drawable elements with CSS-only stroke styling: ",
      basename(file)
    )
  }

  invisible(TRUE)
}

nm_save_separate_legend <- function(plot, stub, size_key) {
  if (!isTRUE(nm_export$separate_legends)) return(invisible(NULL))

  leg <- nm_extract_legend(plot)
  if (is.null(leg)) return(invisible(NULL))

  panel_id <- nm_panel_id(stub)
  dims <- nm_legend_dims_for(size_key, plot = plot, panel_id = panel_id)
  # Ensure the standalone page is large enough for the actual guide grob.
  measured <- tryCatch(c(width = grid::convertWidth(grid::grobWidth(leg), "mm", TRUE),
                         height = grid::convertHeight(grid::grobHeight(leg), "mm", TRUE)),
                       error = function(e) c(width = NA_real_, height = NA_real_))
  if (all(is.finite(measured))) dims <- pmax(dims, measured + c(width = 8, height = 8))
  suffix <- nm_export$legend_suffix

  save_grob_png(
    leg,
    paste0(stub, suffix, ".png"),
    width_mm = unname(dims[["width"]]),
    height_mm = unname(dims[["height"]]),
    background = nm_export$background
  )

  save_grob_svg(
    leg,
    paste0(stub, suffix, ".svg"),
    width_mm = unname(dims[["width"]]),
    height_mm = unname(dims[["height"]]),
    background = nm_export$background
  )

  invisible(leg)
}

save_plot_png <- function(
  plot,
  filename_no_ext,
  width = 7.8,
  height = 6.2,
  dpi = 300,
  make_svg = TRUE,
  size_key = NULL,
  width_mm = NULL,
  height_mm = NULL
) {
  stub <- sub("\\.(png|svg)$", "", filename_no_ext, ignore.case = TRUE)
  out_png <- paste0(stub, ".png")
  out_svg <- paste0(stub, ".svg")
  safe_dir_create(dirname(stub))

  dims <- nm_resolve_dims(
    filename_no_ext = stub,
    old_width_in = width,
    old_height_in = height,
    size_key = size_key,
    width_mm = width_mm,
    height_mm = height_mm
  )

  register_panel(
    panel_id = dims$panel_id,
    object = plot,
    stub = stub,
    type = "plot",
    size_key = dims$key,
    old_width_in = width,
    old_height_in = height
  )

  nm_save_separate_legend(plot, stub = stub, size_key = dims$key)

  p_export <- nm_prepare_plot(
    plot,
    legend_position = if (isTRUE(nm_export$separate_legends)) "none" else "right"
  )

  ggplot2::ggsave(
    filename = out_png,
    plot = p_export,
    width = dims$width_mm,
    height = dims$height_mm,
    units = "mm",
    dpi = nm_export$dpi,
    device = ragg::agg_png,
    bg = nm_export$background,
    limitsize = FALSE
  )

  if (!file.exists(out_png) || file.info(out_png)$size <= 0) {
    stop("PNG was not saved correctly: ", out_png)
  }

  if (isTRUE(make_svg)) {
    save_plot_svg(
      plot = p_export,
      file = out_svg,
      width_mm = dims$width_mm,
      height_mm = dims$height_mm,
      background = nm_export$background
    )
  }

  invisible(out_png)
}

save_heatmap_png <- function(
  ht,
  filename_no_ext,
  width = 12,
  height = 8,
  dpi = 300,
  make_svg = TRUE,
  size_key = NULL,
  width_mm = NULL,
  height_mm = NULL
) {
  stub <- sub("\\.(png|svg)$", "", filename_no_ext, ignore.case = TRUE)
  out_png <- paste0(stub, ".png")
  out_svg <- paste0(stub, ".svg")
  safe_dir_create(dirname(stub))

  dims <- nm_resolve_dims(
    filename_no_ext = stub,
    old_width_in = width,
    old_height_in = height,
    size_key = size_key,
    width_mm = width_mm,
    height_mm = height_mm
  )

  dims <- nm_adjust_heatmap_dims(dims, ht)

  register_panel(
    panel_id = dims$panel_id,
    object = ht,
    stub = stub,
    type = "heatmap",
    size_key = dims$key,
    old_width_in = width,
    old_height_in = height
  )

  ragg::agg_png(
    filename = out_png,
    width = dims$width_mm,
    height = dims$height_mm,
    units = "mm",
    res = nm_export$dpi,
    background = nm_export$background
  )

  ComplexHeatmap::draw(
    ht,
    heatmap_legend_side = "right",
    annotation_legend_side = "right",
    merge_legend = FALSE,
    padding = grid::unit(c(2.0, 2.0, 2.0, 2.0), "mm")
  )
  grDevices::dev.off()

  if (!file.exists(out_png) || file.info(out_png)$size <= 0) {
    stop("Heatmap PNG was not saved correctly: ", out_png)
  }

  if (isTRUE(make_svg)) {
    save_heatmap_svg(
      ht = ht,
      file = out_svg,
      width_mm = dims$width_mm,
      height_mm = dims$height_mm,
      background = nm_export$background
    )
  }

  invisible(out_png)
}

save_nm_svg <- function(plot, file, size_key, width_mm = NULL, height_mm = NULL) {
  dims <- nm_resolve_dims(
    filename_no_ext = file,
    size_key = size_key,
    width_mm = width_mm,
    height_mm = height_mm
  )

  stub <- sub("\\.svg$", "", file, ignore.case = TRUE)
  register_panel(
    panel_id = dims$panel_id,
    object = plot,
    stub = stub,
    type = "plot",
    size_key = dims$key
  )

  nm_save_separate_legend(plot, stub = stub, size_key = dims$key)

  p_export <- nm_prepare_plot(
    plot,
    legend_position = if (isTRUE(nm_export$separate_legends)) "none" else "right"
  )

  save_plot_svg(p_export, file, dims$width_mm, dims$height_mm)
  invisible(file)
}

save_nm_png <- function(plot, file, size_key, width_mm = NULL, height_mm = NULL, dpi = 600) {
  dims <- nm_resolve_dims(
    filename_no_ext = file,
    size_key = size_key,
    width_mm = width_mm,
    height_mm = height_mm
  )

  stub <- sub("\\.png$", "", file, ignore.case = TRUE)
  register_panel(
    panel_id = dims$panel_id,
    object = plot,
    stub = stub,
    type = "plot",
    size_key = dims$key
  )

  p_export <- nm_prepare_plot(
    plot,
    legend_position = if (isTRUE(nm_export$separate_legends)) "none" else "right"
  )

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = file,
    plot = p_export,
    width = dims$width_mm,
    height = dims$height_mm,
    units = "mm",
    dpi = nm_export$dpi,
    bg = nm_export$background,
    device = ragg::agg_png,
    limitsize = FALSE
  )
  invisible(file)
}

reexport <- function(panel_id) {
  if (!exists(panel_id, envir = panel_registry, inherits = FALSE)) {
    if (panel_id %in% names(nm_dims)) {
      stop("'", panel_id, "' is a DIMENSION FAMILY, not a figure ID. ",
           "Use reexport_family(\"", panel_id, "\") or use a concrete ",
           "figure ID from nm_panel_dims / list_panels().")
    }
    available <- sort(ls(panel_registry))
    matches <- grep(panel_id, available, ignore.case = TRUE, value = TRUE, fixed = TRUE)
    show <- if (length(matches)) matches else utils::head(available, 20L)
    stop("Figure ID not registered: '", panel_id, "'. ",
         "Run its plotting chunk first. Valid IDs: ",
         paste(show, collapse = ", "),
         ". Use list_panels() to see all.")
  }

  rec <- get(panel_id, envir = panel_registry, inherits = FALSE)

  if (identical(rec$type, "heatmap")) {
    save_heatmap_png(
      rec$object,
      rec$stub,
      width = if (is.null(rec$old_width_in)) 12 else rec$old_width_in,
      height = if (is.null(rec$old_height_in)) 8 else rec$old_height_in,
      size_key = rec$size_key
    )
  } else {
    save_plot_png(
      rec$object,
      rec$stub,
      width = if (is.null(rec$old_width_in)) 7.8 else rec$old_width_in,
      height = if (is.null(rec$old_height_in)) 6.2 else rec$old_height_in,
      size_key = rec$size_key
    )
  }

  invisible(panel_id)
}

reexport_family <- function(size_key) {
  ids <- sort(ls(panel_registry))
  ids <- ids[vapply(
    ids,
    function(id) identical(get(id, envir = panel_registry)$size_key, size_key),
    logical(1)
  )]

  if (length(ids) == 0L) stop("No registered panels use size key: ", size_key)

  invisible(lapply(ids, reexport))
}

export_all_panels <- function() {
  ids <- sort(ls(panel_registry))
  if (length(ids) == 0L) stop("No panels have been registered yet.")

  for (i in seq_along(ids)) {
    message("Re-exporting panel ", i, "/", length(ids), ": ", ids[i])
    reexport(ids[i])
    gc(verbose = FALSE)
  }
  invisible(ids)
}

figure_export_manifest <- function(
  write_csv = TRUE,
  file = file.path(out_dir, "Figure_Export_Manifest.csv")
) {
  ids <- sort(ls(panel_registry))

  if (length(ids) == 0L) {
    out <- data.frame()
  } else {
    out <- do.call(
      rbind,
      lapply(ids, function(id) {
        rec <- get(id, envir = panel_registry, inherits = FALSE)
        dims <- nm_resolve_dims(
          rec$stub,
          old_width_in = rec$old_width_in,
          old_height_in = rec$old_height_in,
          size_key = rec$size_key
        )
        leg <- nm_legend_dims_for(rec$size_key, plot = if (rec$type == "plot") rec$object else NULL, panel_id = id)

        data.frame(
          Panel = id,
          Family = rec$size_key,
          Width_mm = dims$width_mm,
          Height_mm = dims$height_mm,
          Legend_width_mm = unname(leg[["width"]]),
          Legend_height_mm = unname(leg[["height"]]),
          PNG = paste0(rec$stub, ".png"),
          SVG = paste0(rec$stub, ".svg"),
          stringsAsFactors = FALSE
        )
      })
    )
  }

  if (isTRUE(write_csv)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(out, file, row.names = FALSE)
  }

  out
}

message(
  "Figure export registry loaded. Use set_panel_size()/reexport() for one panel, ",
  "or set_family_size()/reexport_family() for a figure family."
)
# Listing and re-export controls; family names are not individual IDs.
list_panels <- function(pattern = NULL) {
  ids <- sort(ls(panel_registry))
  if (!is.null(pattern)) ids <- grep(pattern, ids, value = TRUE, ignore.case = TRUE)
  if (!length(ids)) return(invisible(data.frame()))
  tab <- do.call(rbind, lapply(ids, function(id) {
    rec <- get(id, envir = panel_registry)
    dm <- nm_resolve_dims(rec$stub, rec$old_width_in, rec$old_height_in, rec$size_key)
    if (rec$type == "heatmap") dm <- nm_adjust_heatmap_dims(dm, rec$object)
    data.frame(ID = id, Family = rec$size_key,
               Width_mm = dm$width_mm, Height_mm = dm$height_mm,
               Reexport = paste0('reexport("', id, '")'),
               PNG = paste0(rec$stub, ".png"))
  }))
  print(tab, row.names = FALSE)
  invisible(tab)
}
panel_info <- function(id) {
  if (!exists(id, envir = panel_registry, inherits = FALSE)) stop("Unknown figure ID: ", id)
  x <- get(id, envir = panel_registry)
  dm <- nm_resolve_dims(x$stub, x$old_width_in, x$old_height_in, x$size_key)
  if (x$type == "heatmap") dm <- nm_adjust_heatmap_dims(dm, x$object)
  print(list(ID = id, Family = x$size_key,
             Dimensions_mm = c(width = dm$width_mm, height = dm$height_mm),
             Plot = x$stub))
  invisible(dm)
}
