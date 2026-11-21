# =============================================================================
# nm_export_engine_FN.R  (figure export engine for Enrichment_FN_v2.Rmd)
#
# Functions only, moved verbatim from the original chunk 'export-setup'.
# Figure sizes, fonts and line widths are NOT set here: edit the
# 'NATURE_FIGURE_HELPER_EDIT_HERE' chunk in the Rmd (nm_dims, nm_legend_dims, nm_style).
# Plot objects (p_fn_*), age_group_levels, group_*_cols, out_dir etc. are looked up when a
# function is CALLED, so this file can be sourced before the analysis has run.
# =============================================================================

# Reference-aligned export: no Inkscape command-line calls, SVG cropping,
# figure-wide ggplotGrob extraction, or external helper scripts.
# Exact SVG processing follows 8.AdOld_Prop.Rmd, including staged file writes.
mm_to_in <- function(mm) mm / 25.4
safe_dir_create <- function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)
svg_font_family <- "Arial"  # exported SVG text; keep ggplot fonts as generic sans
save_nm_panel_png <- function(
  plot,
  file,
  width_mm,
  height_mm,
  dpi = 600,
  background = "white"
) {
  safe_dir_create(dirname(file))

  if (file.exists(file)) {
    try(unlink(file), silent = TRUE)
  }

  device_open <- FALSE
  save_error <- tryCatch(
    {
      ragg::agg_png(
        filename = file,
        width = width_mm,
        height = height_mm,
        units = "mm",
        res = dpi,
        background = background
      )
      device_open <- TRUE
      print(plot)
      grDevices::dev.off()
      device_open <- FALSE
      NULL
    },
    error = function(e) e,
    finally = {
      if (device_open) try(grDevices::dev.off(), silent = TRUE)
    }
  )

  if (!is.null(save_error)) {
    stop(
      "Could not save manuscript PNG:\n",
      normalizePath(file, mustWork = FALSE),
      "\n\nGraphics-device message:\n",
      conditionMessage(save_error)
    )
  }

  if (!file.exists(file) || file.info(file)$size <= 0) {
    stop("PNG was not saved correctly: ", file)
  }

  invisible(file)
}


make_svg_inkscape_safe <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")

  # Drop ONLY svglite's full-page background, not plotted rectangles.
  x <- x[!grepl("^[[:space:]]*<rect[[:space:]]+width=['\"]100%['\"][[:space:]]+height=['\"]100%['\"]",
                x, perl = TRUE)]

  add_style_property <- function(line, property, value) {
    if (grepl(paste0(property, "\\s*:"), line, perl = TRUE)) return(line)

    if (grepl("style='", line, fixed = TRUE)) {
      return(sub(
        "style='",
        paste0("style='", property, ": ", value, "; "),
        line,
        fixed = TRUE
      ))
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
    is_device_bg <- grepl(
      "^\\s*<rect width='100%' height='100%'",
      line,
      perl = TRUE
    )

    if (is_shape && !in_defs && !is_device_bg) {
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

  # Remove exact text-length constraints so labels remain normally editable.
  x <- gsub("\\s+textLength='[^']*'", "", x, perl = TRUE)
  x <- gsub("\\s+lengthAdjust='[^']*'", "", x, perl = TRUE)

  # Inkscape may discard class-scoped CSS when a user ungroups the SVG.
  # Make CSS selectors root-scoped and eliminate ONLY the outer svglite group.
  # Internal clipped plot groups and defs remain intact.
  outer <- grep("^[[:space:]]*<g class=['\"]svglite['\"]>[[:space:]]*$", x)
  if (length(outer) == 1L) {
    closing <- grep("^[[:space:]]*</g>[[:space:]]*$", x)
    closing <- closing[closing > outer]
    if (!length(closing)) stop("SVG outer group lacks a closing tag: ", file)
    x <- gsub(".svglite ", "svg ", x, fixed = TRUE)
    x <- x[-c(outer, tail(closing, 1L))]
  }

  writeLines(x, file, useBytes = TRUE)
  invisible(file)
}


check_svg_inkscape_safe <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")
  in_defs <- FALSE
  bad <- character(0)

  for (line in x) {
    if (grepl("<defs(?:\\s|>)", line, perl = TRUE)) in_defs <- TRUE

    is_device_bg <- grepl(
      "^\\s*<rect width='100%' height='100%'",
      line,
      perl = TRUE
    )

    if (
      !in_defs &&
      !is_device_bg &&
      grepl("^\\s*<(line|polyline|polygon|rect|circle)\\b", line, perl = TRUE)
    ) {
      if (!grepl("stroke\\s*:", line, perl = TRUE)) {
        bad <- c(bad, line)
      }
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


nm_publish_svg <- function(staged_file, final_file) {
  safe_dir_create(dirname(final_file))
  if (!file.exists(staged_file) || is.na(file.info(staged_file)$size) ||
      file.info(staged_file)$size <= 0) {
    stop("SVG staging file is missing or empty: ", staged_file)
  }
  copied <- FALSE
  for (attempt in seq_len(5L)) {
    copied <- suppressWarnings(isTRUE(file.copy(
      from = staged_file, to = final_file, overwrite = TRUE
    )))
    if (copied && file.exists(final_file) &&
        !is.na(file.info(final_file)$size) &&
        file.info(final_file)$size > 0) break
    Sys.sleep(0.4)
  }
  if (!copied) {
    stop(
      "SVG rendered and post-processed successfully, but Windows could ",
      "not replace the destination file: ", final_file, "\n",
      "Close the SVG in Inkscape or other programs, pause Dropbox/OneDrive ",
      "synchronization if necessary, and retry. The statistical analysis ",
      "and plot construction are unaffected."
    )
  }
  invisible(final_file)
}


save_plot_svg <- function(
  plot,
  file,
  width_mm,
  height_mm,
  background = "white"
) {
  safe_dir_create(dirname(file))
  svg_stage <- tempfile(pattern = "nm_plot_",
                        tmpdir = tempdir(), fileext = ".svg")
  on.exit(unlink(svg_stage), add = TRUE)

  device_open <- FALSE
  save_error <- tryCatch(
    {
      svglite::svglite(
        file = svg_stage,
        width = mm_to_in(width_mm),
        height = mm_to_in(height_mm),
        bg = background,
        system_fonts = list(sans = svg_font_family)
      )
      device_open <- TRUE
      print(plot)
      grDevices::dev.off()
      device_open <- FALSE
      NULL
    },
    error = function(e) e,
    finally = {
      if (device_open) try(grDevices::dev.off(), silent = TRUE)
    }
  )

  if (!is.null(save_error)) {
    stop(
      "Could not save editable SVG:\n",
      normalizePath(file, mustWork = FALSE),
      "\n\nSVG-device message:\n",
      conditionMessage(save_error)
    )
  }

  if (!file.exists(svg_stage)) {
    stop("SVG device closed without creating: ", svg_stage)
  }

  make_svg_inkscape_safe(svg_stage)
  check_svg_inkscape_safe(svg_stage)
  nm_publish_svg(svg_stage, file)
  invisible(file)
}


save_grob_png <- function(
  grob,
  file,
  width_mm,
  height_mm,
  background = "white",
  dpi = 600
) {
  safe_dir_create(dirname(file))

  if (file.exists(file)) try(unlink(file), silent = TRUE)

  device_open <- FALSE
  save_error <- tryCatch(
    {
      ragg::agg_png(
        filename = file,
        width = width_mm,
        height = height_mm,
        units = "mm",
        res = dpi,
        background = background
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

  if (!is.null(save_error)) {
    stop("Could not save legend PNG: ", conditionMessage(save_error))
  }

  if (!file.exists(file) || file.info(file)$size <= 0) {
    stop("Legend PNG was not created: ", file)
  }

  invisible(file)
}


save_grob_svg <- function(
  grob,
  file,
  width_mm,
  height_mm,
  background = "white"
) {
  safe_dir_create(dirname(file))
  svg_stage <- tempfile(pattern = "nm_legend_",
                        tmpdir = tempdir(), fileext = ".svg")
  on.exit(unlink(svg_stage), add = TRUE)


  device_open <- FALSE
  save_error <- tryCatch(
    {
      svglite::svglite(
        file = svg_stage,
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

  if (!is.null(save_error)) {
    stop("Could not save editable legend SVG: ", conditionMessage(save_error))
  }

  if (!file.exists(svg_stage)) {
    stop("Legend SVG was not created: ", svg_stage)
  }

  make_svg_inkscape_safe(svg_stage)
  check_svg_inkscape_safe(svg_stage)
  nm_publish_svg(svg_stage, file)
  invisible(file)
}


nm_export <- list(dpi = 600, background = "white", separate_legends = TRUE,
                  legend_suffix = "_legend")

# Main artwork is always exported FIRST; standalone legends are optional and
# built from six or fewer dummy points, not the peptide-level violin dataset.
if (!exists("fn_panel_registry", inherits = FALSE) ||
    !is.environment(fn_panel_registry)) {
  fn_panel_registry <- new.env(parent = emptyenv())
}

fn_resolve_dims <- function(id) {
  dims <- nm_dims[[id]]
  if (is.null(dims) || any(!is.finite(dims)) || any(dims <= 0))
    stop("Unknown/invalid panel dimensions for ", id)
  dims
}

fn_dummy_legend <- function(id) {
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    message("Standalone legend skipped: cowplot is not installed")
    return(NULL)
  }
  groups <- if (identical(id, "fn_ao")) age_group_levels else treatment_group_levels
  dd <- data.frame(x = seq_along(groups), y = seq_along(groups), Group = groups)
  sx <- data.frame(x = 1:2, y = 1:2, Sex = c("Female", "Male"))
  label_map <- c("Adult sed" = "Adult", "Old sed" = if (identical(id, "fn_ao")) "Old" else "Control",
                 "Old ex" = "Exercise", "Old rapa" = "Rapamycin")
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = dd,
      ggplot2::aes(x = x, y = y, fill = Group, colour = Group),
      shape = 22, size = 2.5, stroke = 0.3) +
    ggplot2::geom_point(data = sx, ggplot2::aes(x = x, y = y, shape = Sex),
      fill = "white", colour = "black", size = 2.4) +
    ggplot2::scale_fill_manual(values = group_fill_cols[groups], breaks = groups,
                               labels = label_map[groups], drop = FALSE) +
    ggplot2::scale_colour_manual(values = group_border_cols[groups], breaks = groups,
                                 labels = label_map[groups], drop = FALSE) +
    ggplot2::scale_shape_manual(values = sex_shapes,
                                breaks = c("Female", "Male"), drop = FALSE) +
    ggplot2::guides(colour = "none", fill = ggplot2::guide_legend(order = 1,
                  override.aes = list(shape = 22, size = 3)),
                  shape = ggplot2::guide_legend(order = 2,
                  override.aes = list(fill = "white", colour = "black", size = 2.4))) +
    ggplot2::theme_void(base_family = "sans", base_size = nm_style$legend_text) +
    ggplot2::theme(legend.position = "right",
      legend.title = ggplot2::element_text(size = nm_style$legend_title),
      legend.text = ggplot2::element_text(size = nm_style$legend_text)) +
    ggplot2::labs(fill = "Group", shape = "Sex")
  cowplot::get_legend(p)
}

save_nm <- function(plot_obj, stem, width_mm = NULL, height_mm = NULL,
                    save_legend = nm_export$separate_legends, register = TRUE) {
  if (!inherits(plot_obj, "ggplot")) stop("Expected a ggplot for panel ", stem)
  dims <- fn_resolve_dims(stem)
  if (!is.null(width_mm)) dims[["width"]] <- width_mm
  if (!is.null(height_mm)) dims[["height"]] <- height_mm
  if (register) assign(stem, list(plot = plot_obj), envir = fn_panel_registry)
  file_stub <- file.path(out_dir, stem)
  main <- plot_obj + ggplot2::theme(legend.position = "none")
  message("[", stem, "] Rendering PNG")
  save_nm_panel_png(main, paste0(file_stub, ".png"),
                    dims[["width"]], dims[["height"]], dpi = nm_export$dpi)
  message("[", stem, "] Rendering SVG (staged; no Inkscape process)")
  save_plot_svg(main, paste0(file_stub, ".svg"),
                dims[["width"]], dims[["height"]], background = "white")
  if (isTRUE(save_legend)) {
    tryCatch({
      message("[", stem, "] Rendering small standalone legend")
      lg <- fn_dummy_legend(stem)
      if (!is.null(lg)) {
        leg_dim <- nm_legend_dims[[stem]]
        save_grob_png(lg, paste0(file_stub, "_legend.png"),
                      leg_dim[["width"]], leg_dim[["height"]], dpi = nm_export$dpi)
        save_grob_svg(lg, paste0(file_stub, "_legend.svg"),
                      leg_dim[["width"]], leg_dim[["height"]])
      }
    }, error = function(e) warning("Main figure saved; standalone legend failed: ",
                                  conditionMessage(e)))
  }
  message("[", stem, "] Done")
  invisible(file_stub)
}

list_panels <- function() {
  ids <- sort(ls(fn_panel_registry))
  if (!length(ids)) return(data.frame())
  data.frame(Panel = ids,
             Width_mm = vapply(ids, function(i) nm_resolve_dims(i)[["width"]], numeric(1)),
             Height_mm = vapply(ids, function(i) nm_resolve_dims(i)[["height"]], numeric(1)))
}
panel_info <- function(id) {
  if (!exists(id, fn_panel_registry, inherits = FALSE)) stop("Not registered: ", id)
  subset(list_panels(), Panel == id)
}
set_panel_size <- function(id, width_mm, height_mm) {
  if (is.null(nm_dims[[id]])) stop("Unknown panel: ", id)
  stopifnot(is.finite(width_mm), width_mm > 0, is.finite(height_mm), height_mm > 0)
  nm_dims[[id]] <<- c(width = width_mm, height = height_mm)
  invisible(nm_dims[[id]])
}
# Rebuilds ONLY plot geometry from existing prepared tables/statistics.
# No fraction-new calculation, statistical testing, or model fitting is repeated.
fn_rebuild_plot <- function(id) {
  switch(id,
    fn_ao = plot_fraction_new_global(
      dat_3tp, fn_animal, age_group_levels, age_stats_by_time,
      "Fraction New: Adult vs Old"),
    fn_ot = plot_fraction_new_global(
      dat_3tp, fn_animal, treatment_group_levels, trt_stats_holm,
      "Fraction New: Old control, exercise, and rapamycin"),
    fn_ot_pw = plot_fraction_new_pairwise(
      dat_3tp, fn_animal, treatment_group_levels, trt_stats_holm,
      "Fraction New: Treatment-vs-control comparisons within each fraction"),
    fn_ot_raw = plot_fraction_new_global(
      dat_3tp, fn_animal, treatment_group_levels, trt_stats_raw,
      "Fraction New: Old control, exercise, and rapamycin (raw p)"),
    fn_ot_pw_raw = plot_fraction_new_pairwise(
      dat_3tp, fn_animal, treatment_group_levels, trt_stats_raw,
      "Fraction New: Treatment-vs-control within each fraction (raw p)"),
    stop("Unknown plot builder: ", id)
  )
}

reexport <- function(id, rebuild = TRUE, save_legend = nm_export$separate_legends) {
  if (!exists(id, fn_panel_registry, inherits = FALSE))
    stop("Run the figure-export chunk at least once: ", id)
  plot <- if (isTRUE(rebuild)) fn_rebuild_plot(id) else
    get(id, envir = fn_panel_registry, inherits = FALSE)$plot
  save_nm(plot, id, save_legend = save_legend, register = TRUE)
}

set_legend_size <- function(id, width_mm, height_mm) {
  if (is.null(nm_legend_dims[[id]])) stop("Unknown legend: ", id)
  stopifnot(is.finite(width_mm), width_mm > 0,
            is.finite(height_mm), height_mm > 0)
  nm_legend_dims[[id]] <<- c(width = width_mm, height = height_mm)
  invisible(nm_legend_dims[[id]])
}

export_all_panels <- function(ids = sort(ls(fn_panel_registry))) {
  invisible(lapply(ids, reexport))
}

figure_export_manifest <- function(file = file.path(out_dir, "Figure_Export_Manifest.csv")) {
  x <- list_panels()
  if (nrow(x)) utils::write.csv(x, file, row.names = FALSE)
  invisible(x)
}
