# =============================================================================
# nm_export_engine_insoluble_AO.R
# Figure-export engine for 2.Insoluble_AdOld_v2.Rmd (moved verbatim out of the
# R Markdown; no code changes). Presentation/export only: it never touches data,
# models, or statistics.
#
# Sourced by the Rmd AFTER the settings chunks, because it reads out_dir,
# nm_style, nm_export, nm_dims, nm_svg_dims, nm_legend_dims, nm_layout,
# figure_specs and legend_size_overrides from the session.
#
# Plots are REGISTERED by the analysis chunks (register_plot / register_heatmap)
# and written once at the end by export_all_panels(). Console helpers:
#   list_panels()                  find exact panel IDs and their source chunk
#   panel_info("HM_Age")           inspect one panel
#   set_panel_size("PCA", 48, 46)  temporary size change (mm), then reexport("PCA")
#   set_legend_size("HM_Age", 35, 75)
#   font_audit("PCA")              flag exported SVG text larger than 8 pt
# =============================================================================

nm_heatmap_height <- function(n_rows, family = c("heatmap", "upr")) {
  family <- match.arg(family)
  base <- if (family == "upr") nm_layout$upr_base_height_mm else nm_layout$heatmap_base_height_mm
  per_row <- if (family == "upr") nm_layout$upr_mm_per_protein else nm_layout$heatmap_mm_per_protein
  max(65, base + max(1, n_rows) * per_row)
}

# SVG export below intentionally matches the working helper used in
# 8.AO_Sol_Comp_Agg_v26_NatureHelper_SeparateLegends and
# 7.OT_Prop_v19_DiamondPoints_Top5Decreases. Do not wrap the entire SVG
# in a custom group or post-process it with XML tools; svglite writes native
# vector primitives, and make_svg_inkscape_safe() only moves critical styling
# inline so the objects remain editable after import into Inkscape.

make_svg_inkscape_safe <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")

  # Remove only the full-page SVG device background rectangle (100% x 100%).
  # Native heatmap/plot rectangles must remain intact and editable.
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

    is_device_bg <- grepl(
      "^[[:space:]]*<rect[[:space:]]+width=['\"]100%['\"][[:space:]]+height=['\"]100%['\"]",
      line, perl = TRUE
    )

    if (is_device_bg) {
      x[i] <- NA_character_
      if (grepl("</g>", line, fixed = TRUE) && in_glyphgroup) in_glyphgroup <- FALSE
      if (grepl("</defs>", line, fixed = TRUE)) in_defs <- FALSE
      next
    }

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

  x <- x[!is.na(x)]

  # Remove exact text-length constraints so labels remain normally editable.
  x <- gsub("[[:space:]]+textLength='[^']*'", "", x, perl = TRUE)
  x <- gsub("[[:space:]]+lengthAdjust='[^']*'", "", x, perl = TRUE)

  writeLines(x, file, useBytes = TRUE)
  invisible(file)
}

check_svg_inkscape_safe <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")
  in_defs <- FALSE
  bad <- character(0)

  for (line in x) {
    if (grepl("<defs(?:[[:space:]]|>)", line, perl = TRUE)) in_defs <- TRUE

    is_device_bg <- grepl(
      "^[[:space:]]*<rect[[:space:]]+width=['\"](100%|[0-9.eE+-]+)['\"][[:space:]]+height=['\"](100%|[0-9.eE+-]+)['\"]",
      line,
      perl = TRUE
    ) && grepl(
      "fill[[:space:]]*:[[:space:]]*(#FFFFFF|white)|fill=['\"](#FFFFFF|white)['\"]",
      line,
      perl = TRUE
    )

    if (
      !in_defs &&
      !is_device_bg &&
      grepl("^[[:space:]]*<(line|polyline|polygon|rect|circle)([[:space:]>])", line, perl = TRUE)
    ) {
      if (!grepl("stroke[[:space:]]*:", line, perl = TRUE)) {
        bad <- c(bad, line)
      }
    }

    if (grepl("</defs>", line, fixed = TRUE)) in_defs <- FALSE
  }

  if (any(grepl("^[[:space:]]*<rect[[:space:]]+width=['\"]100%['\"][[:space:]]+height=['\"]100%['\"]", x, perl = TRUE))) {
    stop("SVG still contains a percentage-based page background rectangle: ", basename(file))
  }

  if (length(bad) > 0L) {
    stop(
      "SVG still contains drawable elements with CSS-only stroke styling: ",
      basename(file)
    )
  }

  invisible(TRUE)
}

# Use generic sans inside ggplot/grid so Windows graphics devices do not try to
# resolve Arial directly. During SVG export, map sans explicitly to Arial.
svg_font_family <- "Arial"

save_nm_panel_png <- function(plot, file, width_mm, height_mm,
                              dpi = nm_export$dpi,
                              background = nm_export$background) {
  safe_dir_create(dirname(file))
  if (file.exists(file)) try(unlink(file), silent = TRUE)

  device_open <- FALSE
  save_error <- tryCatch({
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
  }, error = function(e) e, finally = {
    if (device_open) try(grDevices::dev.off(), silent = TRUE)
  })

  if (!is.null(save_error)) stop("Could not save manuscript PNG: ", conditionMessage(save_error))
  if (!file.exists(file) || file.info(file)$size <= 0) stop("PNG was not saved correctly: ", file)
  invisible(file)
}

save_plot_svg <- function(
  plot,
  file,
  width_mm,
  height_mm,
  background = "transparent"
) {
  safe_dir_create(dirname(file))

  if (file.exists(file)) {
    try(unlink(file), silent = TRUE)
  }

  device_open <- FALSE
  save_error <- tryCatch(
    {
      svglite::svglite(
        file = file,
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

  if (!file.exists(file)) {
    stop("SVG device closed without creating: ", file)
  }

  make_svg_inkscape_safe(file)
  check_svg_inkscape_safe(file)
  invisible(file)
}

theme_nm <- function(legend_position = "right", plot_title = TRUE, plot_subtitle = TRUE) {
  ggplot2::theme_classic(
    base_family = nm_style$font_family,
    base_size = nm_style$axis_text
  ) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        family = nm_style$font_family, size = nm_style$axis_title, colour = "black"
      ),
      axis.text = ggplot2::element_text(
        family = nm_style$font_family, size = nm_style$axis_text, colour = "black"
      ),
      axis.line = ggplot2::element_line(linewidth = nm_style$axis_line, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = nm_style$axis_line, colour = "black"),
      axis.ticks.length = grid::unit(1.0, "mm"),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(
        family = nm_style$font_family, size = nm_style$legend_title, face = "bold"
      ),
      legend.text = ggplot2::element_text(
        family = nm_style$font_family, size = nm_style$legend_text
      ),
      legend.key.height = grid::unit(3.0, "mm"),
      legend.key.width = grid::unit(3.0, "mm"),
      plot.title = if (plot_title) ggplot2::element_text(
        family = nm_style$font_family, size = nm_style$plot_title, face = "bold",
        margin = ggplot2::margin(b = 0.8, unit = "mm")
      ) else ggplot2::element_blank(),
      plot.subtitle = if (plot_subtitle) ggplot2::element_text(
        family = nm_style$font_family, size = nm_style$stat_text, colour = "grey35",
        margin = ggplot2::margin(b = 0.8, unit = "mm")
      ) else ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = "white", colour = "black", linewidth = nm_style$axis_line
      ),
      strip.text = ggplot2::element_text(
        family = nm_style$font_family, size = nm_style$strip_text,
        face = "bold", colour = "black"
      ),
      plot.margin = ggplot2::margin(1.8, 1.8, 1.8, 1.8, unit = "mm")
    )
}

nm_set_legend_position <- function(plot, position) {
  if (inherits(plot, "patchwork")) {
    plot & ggplot2::theme(legend.position = position)
  } else {
    plot + ggplot2::theme(legend.position = position)
  }
}

# EXPORT-ONLY NATURE FORMATTING. Does not change fitted lines, p-values,
# data, scales, component colors, or plot geometry. Specific axis.text.x/y and
# axis.title.x/y settings override general axes in ggplot2; normalize both.
# ggtext::element_markdown must never be replaced by element_text mid-merge.
nm_apply_nature_theme <- function(plot, hide_legend = FALSE) {
  font_element <- function(existing, size) {
    if (inherits(existing, "element_blank")) return(NULL)
    if (inherits(existing, "element_markdown")) {
      return(ggtext::element_markdown(size = size, family = nm_style$font_family))
    }
    ggplot2::element_text(size = size, family = nm_style$font_family)
  }
  th <- ggplot2::theme(
    text = ggplot2::element_text(family = nm_style$font_family),
    axis.title = ggplot2::element_text(size = nm_style$axis_title,
                                      family = nm_style$font_family),
    axis.text = ggplot2::element_text(size = nm_style$axis_text,
                                     family = nm_style$font_family),
    axis.line = ggplot2::element_line(linewidth = nm_style$axis_line),
    axis.ticks = ggplot2::element_line(linewidth = nm_style$axis_line),
    legend.title = ggplot2::element_text(size = nm_style$legend_title,
                                         family = nm_style$font_family),
    legend.text = ggplot2::element_text(size = nm_style$legend_text,
                                        family = nm_style$font_family)
  )
  if (!inherits(plot, "patchwork")) {
    targets <- list(
      "axis.title.x" = nm_style$axis_title,
      "axis.title.y" = nm_style$axis_title,
      "axis.text.x" = nm_style$axis_text,
      "axis.text.y" = nm_style$axis_text,
      "strip.text" = nm_style$strip_text,
      "strip.text.x" = nm_style$strip_text,
      "strip.text.y" = nm_style$strip_text
    )
    fields <- list()
    for (name in names(targets)) {
      if (name %in% c("strip.text.x", "strip.text.y") &&
          is.null(plot$theme[[name]])) next
      new <- font_element(plot$theme[[name]], targets[[name]])
      if (!is.null(new)) fields[[name]] <- new
    }
    for (name in c("axis.line.x", "axis.line.y", "axis.ticks.x", "axis.ticks.y")) {
      if (!is.null(plot$theme[[name]]) &&
          !inherits(plot$theme[[name]], "element_blank")) {
        fields[[name]] <- ggplot2::element_line(linewidth = nm_style$axis_line)
      }
    }
    if (length(fields)) th <- th + do.call(ggplot2::theme, fields)
  }
  if (hide_legend) th <- th + ggplot2::theme(legend.position = "none")
  if (inherits(plot, "patchwork")) plot & th else plot + th
}

nm_extract_legend <- function(plot) {
  p_legend <- tryCatch(nm_apply_nature_theme(nm_set_legend_position(plot, "right")), error = function(e) NULL)
  if (is.null(p_legend)) return(NULL)
  leg <- tryCatch(cowplot::get_legend(p_legend), error = function(e) NULL)
  if (is.null(leg) || inherits(leg, "zeroGrob")) return(NULL)
  leg
}

save_grob_png <- function(grob, file, width_mm, height_mm,
                          background = nm_export$background,
                          dpi = nm_export$dpi) {
  safe_dir_create(dirname(file))
  if (file.exists(file)) try(unlink(file), silent = TRUE)

  device_open <- FALSE
  save_error <- tryCatch({
    ragg::agg_png(
      filename = file, width = width_mm, height = height_mm,
      units = "mm", res = dpi, background = background
    )
    device_open <- TRUE
    grid::grid.newpage()
    grid::grid.draw(grob)
    grDevices::dev.off()
    device_open <- FALSE
    NULL
  }, error = function(e) e, finally = {
    if (device_open) try(grDevices::dev.off(), silent = TRUE)
  })

  if (!is.null(save_error)) stop("Could not save legend PNG: ", conditionMessage(save_error))
  if (!file.exists(file) || file.info(file)$size <= 0) stop("Legend PNG was not created: ", file)
  invisible(file)
}

save_grob_svg <- function(
  grob,
  file,
  width_mm,
  height_mm,
  background = "transparent"
) {
  safe_dir_create(dirname(file))

  if (file.exists(file)) try(unlink(file), silent = TRUE)

  device_open <- FALSE
  save_error <- tryCatch(
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

  if (!is.null(save_error)) {
    stop("Could not save editable legend SVG: ", conditionMessage(save_error))
  }

  if (!file.exists(file)) {
    stop("Legend SVG was not created: ", file)
  }

  make_svg_inkscape_safe(file)
  check_svg_inkscape_safe(file)
  invisible(file)
}

nm_save_separate_legend <- function(plot, file_stub, size_key,
                                    legend_width_mm = NULL,
                                    legend_height_mm = NULL) {
  if (!isTRUE(nm_export$separate_legends)) return(NULL)
  leg <- nm_extract_legend(plot)
  if (is.null(leg)) return(NULL)

  dims <- nm_legend_dims[[size_key]]
  if (is.null(dims)) dims <- nm_legend_dims[["generic"]]
  if (is.null(legend_width_mm)) legend_width_mm <- unname(dims[["width"]])
  if (is.null(legend_height_mm)) legend_height_mm <- unname(dims[["height"]])
  suffix <- nm_export$legend_suffix

  save_grob_png(
    leg, paste0(file_stub, suffix, ".png"),
    width_mm = legend_width_mm, height_mm = legend_height_mm
  )
  save_grob_svg(
    leg, paste0(file_stub, suffix, ".svg"),
    width_mm = legend_width_mm, height_mm = legend_height_mm
  )
  leg
}

save_nm_both <- function(plot, file_stub, size_key,
                         width_mm = NULL, height_mm = NULL,
                         dpi = nm_export$dpi, save_legend = TRUE,
                         legend_width_mm = NULL, legend_height_mm = NULL) {
  dims <- nm_svg_dims[[size_key]]
  if (is.null(dims)) stop("Unknown Nature Metabolism size key: ", size_key)
  if (is.null(width_mm)) width_mm <- unname(dims[["width"]])
  if (is.null(height_mm)) height_mm <- unname(dims[["height"]])

  leg <- if (isTRUE(save_legend)) {
    nm_save_separate_legend(
      plot, file_stub = file_stub, size_key = size_key,
      legend_width_mm = legend_width_mm,
      legend_height_mm = legend_height_mm
    )
  } else NULL

  plot_export <- nm_apply_nature_theme(plot, hide_legend = !is.null(leg))

  save_nm_panel_png(
    plot_export, paste0(file_stub, ".png"),
    width_mm = width_mm, height_mm = height_mm, dpi = dpi
  )
  save_plot_svg(
    plot_export, paste0(file_stub, ".svg"),
    width_mm = width_mm, height_mm = height_mm
  )

  invisible(list(
    png = paste0(file_stub, ".png"),
    svg = paste0(file_stub, ".svg"),
    legend_png = if (!is.null(leg)) paste0(file_stub, nm_export$legend_suffix, ".png") else NULL,
    legend_svg = if (!is.null(leg)) paste0(file_stub, nm_export$legend_suffix, ".svg") else NULL
  ))
}

# Backward-compatible name used in the newer aggregation-propensity reports.
save_nm_svg <- function(plot, file, size_key, width_mm = NULL, height_mm = NULL,
                        save_legend = TRUE,
                        legend_width_mm = NULL, legend_height_mm = NULL) {
  file_stub <- sub("\\.[Ss][Vv][Gg]$", "", file)
  save_nm_both(
    plot = plot, file_stub = file_stub, size_key = size_key,
    width_mm = width_mm, height_mm = height_mm, save_legend = save_legend,
    legend_width_mm = legend_width_mm, legend_height_mm = legend_height_mm
  )
}

# ComplexHeatmap/grid export. The data panel is saved without legends, while a
# manually supplied legend grob is saved separately in both PNG and SVG.
save_nm_heatmap <- function(ht, file_stub, size_key,
                            width_mm = NULL, height_mm = NULL,
                            legend_grob = NULL,
                            legend_width_mm = NULL,
                            legend_height_mm = NULL,
                            padding_mm = c(2, 2, 2, 2)) {
  dims <- nm_svg_dims[[size_key]]
  if (is.null(dims)) stop("Unknown Nature Metabolism size key: ", size_key)
  if (is.null(width_mm)) width_mm <- unname(dims[["width"]])
  if (is.null(height_mm)) height_mm <- unname(dims[["height"]])
  safe_dir_create(dirname(file_stub))

  draw_heatmap <- function() {
    ComplexHeatmap::draw(
      ht,
      heatmap_legend_side = "right",
      annotation_legend_side = "right",
      merge_legend = FALSE,
      show_heatmap_legend = FALSE,
      show_annotation_legend = FALSE,
      padding = grid::unit(padding_mm, "mm")
    )
  }

  png_file <- paste0(file_stub, ".png")
  svg_file <- paste0(file_stub, ".svg")

  device_open <- FALSE
  err_png <- tryCatch({
    ragg::agg_png(
      filename = png_file, width = width_mm, height = height_mm,
      units = "mm", res = nm_export$dpi, background = nm_export$background
    )
    device_open <- TRUE
    draw_heatmap()
    grDevices::dev.off()
    device_open <- FALSE
    NULL
  }, error = function(e) e, finally = {
    if (device_open) try(grDevices::dev.off(), silent = TRUE)
  })
  if (!is.null(err_png)) stop("Could not save heatmap PNG: ", conditionMessage(err_png))

  device_open <- FALSE
  err_svg <- tryCatch({
    svglite::svglite(
      file = svg_file,
      width = mm_to_in(width_mm), height = mm_to_in(height_mm),
      bg = "transparent",
      system_fonts = list(sans = svg_font_family)
    )
    device_open <- TRUE
    draw_heatmap()
    grDevices::dev.off()
    device_open <- FALSE
    NULL
  }, error = function(e) e, finally = {
    if (device_open) try(grDevices::dev.off(), silent = TRUE)
  })
  if (!is.null(err_svg)) stop("Could not save heatmap SVG: ", conditionMessage(err_svg))
  make_svg_inkscape_safe(svg_file)
  check_svg_inkscape_safe(svg_file)

  if (!is.null(legend_grob) && isTRUE(nm_export$separate_legends)) {
    ldims <- nm_legend_dims[[size_key]]
    if (is.null(ldims)) ldims <- nm_legend_dims[["generic"]]
    if (is.null(legend_width_mm)) legend_width_mm <- unname(ldims[["width"]])
    if (is.null(legend_height_mm)) legend_height_mm <- unname(ldims[["height"]])
    save_grob_png(
      legend_grob, paste0(file_stub, nm_export$legend_suffix, ".png"),
      width_mm = legend_width_mm, height_mm = legend_height_mm
    )
    save_grob_svg(
      legend_grob, paste0(file_stub, nm_export$legend_suffix, ".svg"),
      width_mm = legend_width_mm, height_mm = legend_height_mm
    )
  }

  invisible(list(png = png_file, svg = svg_file))
}

# Style a copy of the registered ggplot for manuscript export, matching the
# central controls in Soluble AO v2. Does not modify the fitted models or data.
nm_style_plot <- function(p, id) {
  if (!inherits(p, "ggplot")) return(p)
  family <- if (grepl("^Vol_", id)) "volcano" else
    if (grepl("^GO_", id)) "go" else
    if (grepl("^Int_all_Page_", id)) "interaction_page" else
    if (id == "Int_all") "interaction_all" else
    if (grepl("^Int_", id)) "interaction_one" else "other"
  if (family == "go") {
    p <- suppressMessages(p + ggplot2::scale_size_continuous(
      name = "Protein count", range = nm_style$go_dot_range))
  }
  for (k in seq_along(p$layers)) {
    layer <- p$layers[[k]]
    klass <- class(layer$geom)[1]
    if (family == "volcano") {
      if (klass == "GeomPoint") {
        p$layers[[k]]$aes_params$size <- nm_style$volcano_point
        p$layers[[k]]$aes_params$stroke <- nm_style$volcano_stroke
      } else if (klass %in% c("GeomHline", "GeomVline")) {
        p$layers[[k]]$aes_params$linewidth <- nm_style$volcano_reference_line
      } else if (grepl("GeomTextRepel", klass)) {
        p$layers[[k]]$aes_params$size <- nm_style$volcano_label_mm
        p$layers[[k]]$geom_params$segment.size <- nm_style$volcano_segment
      }
    } else if (family %in% c("interaction_all", "interaction_page", "interaction_one")) {
      one <- family == "interaction_one"
      if (klass == "GeomLine") {
        p$layers[[k]]$aes_params$linewidth <- if (one) nm_style$interaction_one_line else nm_style$interaction_line
      } else if (klass == "GeomPoint") {
        p$layers[[k]]$aes_params$size <- if (one) nm_style$interaction_one_point else nm_style$interaction_point
      } else if (klass == "GeomErrorbar") {
        p$layers[[k]]$aes_params$linewidth <- if (one) nm_style$interaction_one_errorbar else nm_style$interaction_errorbar
      }
    } else if (family == "go") {
      if (klass == "GeomSegment") p$layers[[k]]$aes_params$linewidth <- nm_style$go_segment
      if (klass == "GeomVline") p$layers[[k]]$aes_params$linewidth <- nm_style$go_guide_line
    }
  }
  p
}

# -----------------------------------------------------------------------------
# PANEL REGISTRATION + ONE-LINE RE-EXPORT
# -----------------------------------------------------------------------------
# Plot construction and file export are intentionally separated. Analysis
# chunks register named panel objects; one centralized chunk near the end calls
# export_all_panels(). To resize a panel, edit nm_dims in NATURE_FIGURE_HELPER_EDIT_HERE
# and call reexport("Int_Capza2").
if (!exists("panel_registry", inherits = FALSE) ||
    !is.environment(panel_registry) ||
    !exists("nm_panel_context", inherits = FALSE) ||
    !identical(nm_panel_context, normalizePath(out_dir, winslash = "/", mustWork = FALSE))) {
  panel_registry <- new.env(parent = emptyenv())
}
nm_panel_context <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)

nm_current_chunk <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) return(NA_character_)
  z <- tryCatch(knitr::opts_current$get("label"), error = function(e) NULL)
  if (is.null(z) || !length(z) || !nzchar(z[1])) NA_character_ else as.character(z[1])
}

register_plot <- function(id, plot, file_stub, spec_id = id,
                          width_mm = NULL, height_mm = NULL,
                          save_legend = NULL) {
  assign(
    id,
    list(
      type = "ggplot",
      object = plot,
      file_stub = file_stub,
      spec_id = spec_id,
      width_mm = width_mm,
      height_mm = height_mm,
      save_legend = save_legend,
      legend_grob = NULL,
      created_in = nm_current_chunk()
    ),
    envir = panel_registry
  )
  invisible(plot)
}

register_heatmap <- function(id, ht, file_stub, spec_id = id,
                             width_mm = NULL, height_mm = NULL,
                             legend_grob = NULL,
                             save_legend = NULL) {
  assign(
    id,
    list(
      type = "heatmap",
      object = ht,
      file_stub = file_stub,
      spec_id = spec_id,
      width_mm = width_mm,
      height_mm = height_mm,
      save_legend = save_legend,
      legend_grob = legend_grob,
      created_in = nm_current_chunk()
    ),
    envir = panel_registry
  )
  invisible(ht)
}

panel_spec <- function(id) {
  if (!exists(id, envir = panel_registry, inherits = FALSE)) {
    stop("Panel is not registered: ", id, ". Run its plotting chunk first; use list_panels().")
  }
  entry <- get(id, envir = panel_registry, inherits = FALSE)
  spec <- figure_specs[[entry$spec_id]]
  if (is.null(spec)) stop("No metadata for panel: ", entry$spec_id)
  size_key <- spec$size_key
  template <- nm_svg_dims[[size_key]]
  if (is.null(template)) stop("Unknown size family: ", size_key)

  # Precedence: exact nm_dims[[id]] > family nm_dims[[spec_id]] >
  # data-driven original entry dimensions > template. NA means fall through.
  finite_or_null <- function(x) {
    if (is.null(x) || !length(x) || !is.finite(x[1])) NULL else as.numeric(x[1])
  }
  choose_dimension <- function(d) {
    exact <- nm_dims[[id]]
    family <- nm_dims[[entry$spec_id]]
    for (candidate in list(exact, family)) {
      if (!is.null(candidate)) {
        v <- finite_or_null(candidate[[d]])
        if (!is.null(v)) return(v)
      }
    }
    v <- finite_or_null(entry[[paste0(d, "_mm")]])
    if (!is.null(v)) return(v)
    unname(template[[d]])
  }
  width_mm <- choose_dimension("width")
  height_mm <- choose_dimension("height")
  if (any(!is.finite(c(width_mm, height_mm))) || any(c(width_mm, height_mm) <= 0)) {
    stop("Invalid dimensions for: ", id)
  }

  save_legend <- if (!is.null(entry$save_legend)) {
    isTRUE(entry$save_legend)
  } else isTRUE(spec$save_legend)

  ldims <- nm_legend_dims[[size_key]]
  if (is.null(ldims)) ldims <- nm_legend_dims[["generic"]]
  legend_width_mm <- unname(ldims[["width"]])
  legend_height_mm <- unname(ldims[["height"]])
  ld <- legend_size_overrides[[id]]
  if (!is.null(ld)) {
    if (is.finite(ld[["width"]])) legend_width_mm <- ld[["width"]]
    if (is.finite(ld[["height"]])) legend_height_mm <- ld[["height"]]
  }

  list(
    id = id, type = entry$type, file_stub = entry$file_stub,
    size_key = size_key, spec_id = entry$spec_id,
    description = if (!is.null(spec$description)) spec$description else "",
    plotting_chunk = if (!is.null(entry$created_in)) entry$created_in else NA_character_,
    width_mm = width_mm, height_mm = height_mm,
    save_legend = save_legend,
    legend_width_mm = legend_width_mm, legend_height_mm = legend_height_mm,
    entry = entry
  )
}

# Both interactive resizing and manual registry edits change the SAME nm_dims.
set_panel_size <- function(id, width_mm, height_mm) {
  if (!exists(id, envir = panel_registry, inherits = FALSE)) stop("Unknown panel: ", id)
  nm_dims[[id]] <<- c(width = width_mm, height = height_mm)
  invisible(panel_spec(id))
}
clear_panel_size <- function(id) {
  nm_dims[[id]] <<- NULL
  invisible(panel_spec(id))
}
set_family_size <- function(family, width_mm, height_mm) {
  # Accept either a spec ID ("interaction_single") or a physical size key
  # ("volcano_single"). Update the same nm_dims entries used by reexport().
  targets <- names(figure_specs)[vapply(figure_specs, function(spec) {
    identical(spec$size_key, family)
  }, logical(1))]
  if (!length(targets) && is.null(figure_specs[[family]])) {
    stop("Unknown panel family: ", family)
  }
  if (!length(targets)) targets <- family
  for (id in targets) nm_dims[[id]] <<- c(width = width_mm, height = height_mm)
  invisible(targets)
}
set_legend_size <- function(id, width_mm, height_mm) {
  if (!exists(id, envir = panel_registry, inherits = FALSE)) stop("Unknown panel: ", id)
  legend_size_overrides[[id]] <<- c(width = width_mm, height = height_mm)
  invisible(panel_spec(id))
}

export_panel <- function(id, verbose = TRUE) {
  spec <- panel_spec(id)
  entry <- spec$entry

  if (identical(entry$type, "ggplot")) {
    files <- save_nm_both(
      plot = nm_style_plot(entry$object, id),
      file_stub = entry$file_stub,
      size_key = spec$size_key,
      width_mm = spec$width_mm,
      height_mm = spec$height_mm,
      save_legend = spec$save_legend,
      legend_width_mm = spec$legend_width_mm,
      legend_height_mm = spec$legend_height_mm
    )
  } else if (identical(entry$type, "heatmap")) {
    files <- save_nm_heatmap(
      ht = entry$object,
      file_stub = entry$file_stub,
      size_key = spec$size_key,
      width_mm = spec$width_mm,
      height_mm = spec$height_mm,
      legend_grob = if (spec$save_legend) entry$legend_grob else NULL,
      legend_width_mm = spec$legend_width_mm,
      legend_height_mm = spec$legend_height_mm
    )
  } else {
    stop("Unsupported registered panel type: ", entry$type)
  }

  if (isTRUE(verbose)) {
    message(
      "Exported ", id, " at ", spec$width_mm, " × ", spec$height_mm,
      " mm -> ", normalizePath(dirname(entry$file_stub), mustWork = FALSE)
    )
  }
  invisible(files)
}

assert_export_helpers_loaded <- function() {
  required <- c(
    "save_grob_svg",
    "save_plot_svg",
    "save_nm_svg",
    "save_nm_both"
  )
  missing <- required[!vapply(
    required,
    exists,
    logical(1),
    mode = "function",
    inherits = TRUE
  )]
  if (length(missing) > 0L) {
    stop(
      "Nature/SVG export helper is not fully loaded. Missing: ",
      paste(missing, collapse = ", "),
      ". Start a fresh R session and run the 'nature-metabolism-svg-helpers' chunk before exporting."
    )
  }
  invisible(TRUE)
}

reexport <- function(id) {
  assert_export_helpers_loaded()
  export_panel(id, verbose = TRUE)
}

export_all_panels <- function(ids = sort(ls(panel_registry))) {
  assert_export_helpers_loaded()
  if (length(ids) == 0L) return(invisible(NULL))

  for (i in seq_along(ids)) {
    id <- ids[[i]]
    message("Exporting panel ", i, "/", length(ids), ": ", id)
    export_panel(id, verbose = FALSE)

    # Release device/grob memory between high-resolution exports. This is
    # particularly helpful for long RStudio sessions and ComplexHeatmap panels.
    invisible(gc(verbose = FALSE))
  }

  invisible(NULL)
}

figure_export_manifest <- function(file = file.path(out_dir, "Figure_Export_Manifest.csv")) {
  ids <- sort(ls(panel_registry))
  if (length(ids) == 0L) return(data.frame())

  manifest <- do.call(
    rbind,
    lapply(ids, function(id) {
      s <- panel_spec(id)
      data.frame(
        Panel = id,
        Description = s$description,
        Plotting_chunk = s$plotting_chunk,
        Family = s$spec_id,
        Type = s$type,
        Width_mm = s$width_mm,
        Height_mm = s$height_mm,
        Separate_legend = s$save_legend,
        Legend_width_mm = if (s$save_legend) s$legend_width_mm else NA_real_,
        Legend_height_mm = if (s$save_legend) s$legend_height_mm else NA_real_,
        PNG = paste0(s$file_stub, ".png"),
        SVG = paste0(s$file_stub, ".svg"),
        Legend_PNG = if (s$save_legend && file.exists(paste0(s$file_stub, "_legend.png"))) paste0(s$file_stub, "_legend.png") else "",
        Legend_SVG = if (s$save_legend && file.exists(paste0(s$file_stub, "_legend.svg"))) paste0(s$file_stub, "_legend.svg") else "",
        stringsAsFactors = FALSE
      )
    })
  )
  write.csv(manifest, file, row.names = FALSE)
  manifest
}

list_panels <- function(pattern = NULL) {
  ids <- sort(ls(panel_registry))
  if (!is.null(pattern)) ids <- grep(pattern, ids, value = TRUE, ignore.case = TRUE)
  if (!length(ids)) return(invisible(data.frame()))
  dat <- do.call(rbind, lapply(ids, function(id) {
    spec <- panel_spec(id)
    data.frame(Panel_ID = id, Description = spec$description,
               Plotting_chunk = spec$plotting_chunk, Family = spec$spec_id,
               Width_mm = spec$width_mm, Height_mm = spec$height_mm,
               PNG = paste0(spec$file_stub, ".png"), stringsAsFactors = FALSE)
  }))
  print(dat, row.names = FALSE)
  invisible(dat)
}
list_registered_panels <- list_panels

panel_info <- function(id) {
  spec <- panel_spec(id)
  print(list(Panel_ID = id, Description = spec$description,
             Plotting_chunk = spec$plotting_chunk, Family = spec$spec_id,
             Size_mm = c(width = spec$width_mm, height = spec$height_mm),
             PNG = paste0(spec$file_stub, ".png"),
             SVG = paste0(spec$file_stub, ".svg")))
  invisible(spec)
}

# Audit text elements that are too large for your assembled Nature panels.
font_audit <- function(id, threshold = 8) {
  svg <- paste0(panel_spec(id)$file_stub, ".svg")
  if (!file.exists(svg)) stop("Export this panel first: ", svg)
  lines <- readLines(svg, warn = FALSE, encoding = "UTF-8")
  m <- regmatches(lines, regexec("font-size:[[:space:]]*([0-9.]+)(px|pt)?", lines, perl = TRUE))
  values <- vapply(m, function(z) if (length(z) > 1L) as.numeric(z[2]) else NA_real_, numeric(1))
  ix <- which(is.finite(values) & values > threshold)
  if (!length(ix)) {
    message("No SVG font sizes above ", threshold, " were detected in ", id)
    return(invisible(data.frame()))
  }
  out <- data.frame(Font_size = values[ix], SVG_line = ix,
                    Text = substr(trimws(gsub("<[^>]+>", "", lines[ix])), 1, 75))
  print(out, row.names = FALSE)
  invisible(out)
}

message("In-document Nature export helper loaded; use list_panels(), panel_info(), reexport().")
