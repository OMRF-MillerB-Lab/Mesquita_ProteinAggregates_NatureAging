# =============================================================================
# nm_export_engine_soluble_AO.R
# Figure-export engine for 1.Soluble_AdOld_v2.Rmd (moved verbatim out of the
# R Markdown; no code changes). Presentation/export only: it never touches data,
# models, or statistics.
#
# Sourced automatically by the Rmd AFTER the settings chunks, because it reads
# out_dir, nm_export, nm_style, nm_dims, nm_family_*, nm_legend_dims, nm_layout,
# z_clip, age_colours, sex_colours and heatmap_col_fun from the session.
#
# Console helpers once the Rmd has been run:
#   list_panels()                    find exact panel IDs
#   panel_info("PCA")                inspect one panel
#   reexport("Vol_Age")              re-export one panel after editing nm_dims
#   set_panel_size("PCA", 60, 52)    temporary size change (mm)
# =============================================================================

if (!requireNamespace("svglite", quietly = TRUE)) {
  stop("Install svglite first: install.packages('svglite')", call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required")

# Preserve registered plots if you rerun this helper while the analysis is active.
# Switching output directories starts a new registry.
.nm_same_context <- exists("nm_registry", inherits = FALSE) &&
  is.environment(nm_registry) &&
  exists("nm_export_context", inherits = FALSE) &&
  identical(nm_export_context, normalizePath(out_dir, winslash = "/", mustWork = FALSE))
if (!.nm_same_context) {
  nm_registry <- new.env(parent = emptyenv())
  nm_legend_overrides <- list()
}
nm_export_context <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
rm(.nm_same_context)

nm_id <- function(file) tools::file_path_sans_ext(basename(file))
nm_png_path <- function(file) {
  if (grepl("\\.png$", file, ignore.case = TRUE)) file else paste0(file, ".png")
}

nm_current_chunk <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) return(NA_character_)
  z <- tryCatch(knitr::opts_current$get("label"), error = function(e) NULL)
  if (is.null(z) || !length(z) || !nzchar(z[1])) NA_character_ else as.character(z[1])
}

nm_family <- function(id) {
  if (!exists("nm_family_rules", inherits = TRUE)) return("default")
  for (i in seq_along(nm_family_rules)) {
    if (grepl(names(nm_family_rules)[i], id, ignore.case = TRUE)) {
      return(unname(nm_family_rules[[i]]))
    }
  }
  "default"
}

nm_dimensions <- function(id) {
  item <- nm_registry[[id]]
  if (is.null(item)) {
    stop("Unknown panel: ", id, ". Run the figure-creation chunk first. ",
         "Call list_panels() to see registered IDs.")
  }
  # THE SINGLE SOURCE OF TRUTH: individual ID > named family > original size.
  # Both values are looked up when reexport() runs, so rerunning the dimension
  # registry chunk immediately changes the next export (no stale copies).
  d <- item$original_mm
  family <- nm_family(id)
  if (exists("nm_family_dims", inherits = TRUE) &&
      !is.null(nm_family_dims[[family]])) d <- nm_family_dims[[family]]
  if (exists("nm_dims", inherits = TRUE) &&
      !is.null(nm_dims[[id]])) d <- nm_dims[[id]]
  # NA means retain the dynamically calculated original height (row-count based).
  missing_size <- is.na(d)
  d[missing_size] <- item$original_mm[names(d)[missing_size]]
  d <- as.numeric(d[c("width", "height")])
  if (length(d) != 2L || any(!is.finite(d)) || any(d <= 0)) {
    stop("Invalid width/height (mm) for panel: ", id)
  }
  stats::setNames(d, c("width", "height"))
}

# Interactive changes affect the SAME nm_dims object as manual registry edits.
set_panel_size <- function(id, width_mm, height_mm) {
  if (is.null(nm_registry[[id]])) stop("Unknown panel ID: ", id)
  nm_dims[[id]] <<- c(width = width_mm, height = height_mm)
  invisible(nm_dims[[id]])
}
clear_panel_size <- function(id) {
  nm_dims[[id]] <<- NULL
  invisible(id)
}
set_family_size <- function(family, width_mm, height_mm) {
  if (is.null(nm_family_dims[[family]])) stop("Unknown family: ", family)
  nm_family_dims[[family]] <<- c(width = width_mm, height = height_mm)
  invisible(nm_family_dims[[family]])
}

set_legend_size <- function(id, width_mm, height_mm) {
  nm_legend_overrides[[id]] <<- c(width = width_mm, height = height_mm)
  invisible(nm_legend_overrides[[id]])
}

nm_make_svg_inkscape_safe <- function(file) {
  # Same svglite inline-style approach as the working AO propensity helper,
  # followed by removal of the full-page rectangle used only by the device.
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")
  # Make CSS independent of the outer svglite group: ungrouping in Inkscape
  # must not remove axis, text, path, or clip-path styling.
  x <- gsub(".svglite ", "svg ", x, fixed = TRUE)
  add_property <- function(line, property, value) {
    if (grepl(paste0(property, "[[:space:]]*:"), line, perl = TRUE)) return(line)
    if (grepl("style='", line, fixed = TRUE)) {
      return(sub("style='", paste0("style='", property, ": ", value, "; "),
                 line, fixed = TRUE))
    }
    sub("[[:space:]]*/>$",
        paste0(" style='", property, ": ", value, ";' />"), line, perl = TRUE)
  }
  in_defs <- FALSE
  in_glyphgroup <- FALSE
  for (i in seq_along(x)) {
    line <- x[i]
    if (grepl("<defs([[:space:]>])", line, perl = TRUE)) in_defs <- TRUE
    if (grepl("<g[^>]*class='glyphgroup'", line, perl = TRUE)) in_glyphgroup <- TRUE
    is_bg <- grepl("^[[:space:]]*<rect([[:space:]>])", line) &&
      grepl("width=['\"]100%['\"]", line) &&
      grepl("height=['\"]100%['\"]", line)
    if (is_bg) {
      x[i] <- NA_character_
      next
    }
    is_shape <- grepl("^[[:space:]]*<(line|polyline|polygon|rect|circle|path)([[:space:]>])",
                      line, perl = TRUE)
    is_text <- grepl("^[[:space:]]*<text([[:space:]>])", line, perl = TRUE)
    if (is_shape && !in_defs) {
      if (in_glyphgroup && grepl("^[[:space:]]*<path([[:space:]>])", line, perl = TRUE)) {
        line <- add_property(line, "fill", "inherit")
        line <- add_property(line, "stroke", "none")
      } else {
        line <- add_property(line, "fill", "none")
        line <- add_property(line, "stroke", "#000000")
        line <- add_property(line, "stroke-linecap", "round")
        line <- add_property(line, "stroke-linejoin", "round")
        line <- add_property(line, "stroke-miterlimit", "10.00")
      }
    }
    if (is_text && !in_defs) line <- add_property(line, "white-space", "pre")
    x[i] <- line
    if (grepl("</g>", line, fixed = TRUE) && in_glyphgroup) in_glyphgroup <- FALSE
    if (grepl("</defs>", line, fixed = TRUE)) in_defs <- FALSE
  }
  x <- x[!is.na(x)]
  x <- gsub("[[:space:]]+textLength='[^']*'", "", x, perl = TRUE)
  x <- gsub("[[:space:]]+lengthAdjust='[^']*'", "", x, perl = TRUE)
  writeLines(x, file, useBytes = TRUE)
  invisible(file)
}
nm_save_svg <- function(file, width_mm, height_mm, draw) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  svglite::svglite(file, width = width_mm / 25.4, height = height_mm / 25.4,
                   bg = "transparent", system_fonts = list(sans = nm_export$font))
  opened <- TRUE
  tryCatch({
    draw()
    grDevices::dev.off()
    opened <- FALSE
  }, finally = {
    if (opened) try(grDevices::dev.off(), silent = TRUE)
  })
  nm_make_svg_inkscape_safe(file)
  invisible(file)
}

nm_save_png <- function(file, width_mm, height_mm, draw) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(file, width = width_mm, height = height_mm,
                  units = "mm", res = nm_export$dpi, background = "white")
  } else {
    grDevices::png(file, width = width_mm, height = height_mm,
                   units = "mm", res = nm_export$dpi, bg = "white")
  }
  opened <- TRUE
  tryCatch({
    draw()
    grDevices::dev.off()
    opened <- FALSE
  }, finally = {
    if (opened) try(grDevices::dev.off(), silent = TRUE)
  })
  invisible(file)
}

# Modify only ggplot geometry aesthetics on a COPY of the registered plot.
# All data, model fits, scales for statistics, and group selections are untouched.
nm_style_plot <- function(p, id) {
  fam <- nm_family(id)
  if (!inherits(p, "ggplot")) return(p)
  if (identical(fam, "go")) {
    # Replace only the display range for Count; the same Count values are mapped.
    p <- suppressMessages(p + ggplot2::scale_size_continuous(
      name = "Protein count", range = nm_style$go_dot_range))
  }
  for (k in seq_along(p$layers)) {
    layer <- p$layers[[k]]
    klass <- class(layer$geom)[1]
    if (identical(fam, "volcano")) {
      if (klass == "GeomPoint") {
        p$layers[[k]]$aes_params$size <- nm_style$volcano_point
        p$layers[[k]]$aes_params$stroke <- nm_style$volcano_stroke
      } else if (klass %in% c("GeomHline", "GeomVline")) {
        p$layers[[k]]$aes_params$linewidth <- nm_style$volcano_reference_line
      } else if (grepl("GeomTextRepel", klass)) {
        p$layers[[k]]$aes_params$size <- nm_style$volcano_label_mm
        p$layers[[k]]$geom_params$segment.size <- nm_style$volcano_segment
      }
    } else if (fam %in% c("interaction_all", "interaction_one", "interaction_page")) {
      one <- identical(fam, "interaction_one")
      if (klass == "GeomLine") {
        p$layers[[k]]$aes_params$linewidth <- if (one) nm_style$interaction_one_line else nm_style$interaction_line
      } else if (klass == "GeomPoint") {
        p$layers[[k]]$aes_params$size <- if (one) nm_style$interaction_one_point else nm_style$interaction_point
      } else if (klass == "GeomErrorbar") {
        p$layers[[k]]$aes_params$linewidth <- if (one) nm_style$interaction_one_errorbar else nm_style$interaction_errorbar
      }
    } else if (identical(fam, "go")) {
      if (klass == "GeomSegment") p$layers[[k]]$aes_params$linewidth <- nm_style$go_segment
      if (klass == "GeomVline") p$layers[[k]]$aes_params$linewidth <- nm_style$go_guide_line
    }
  }
  p
}

# Determine legend size on a temporary graphics device, not by rebuilding
# the data plots. User-specified legend minimum dimensions are preserved.
nm_measure_legend <- function(lg) {
  opened <- FALSE
  tryCatch({
    grDevices::pdf(file = NULL, width = 6, height = 4)
    opened <- TRUE
    grid::grid.newpage()
    grid::grid.draw(lg)
    out <- c(
      width = grid::convertWidth(sum(lg$widths), "mm", valueOnly = TRUE),
      height = grid::convertHeight(sum(lg$heights), "mm", valueOnly = TRUE))
    out
  }, error = function(e) c(width = NA_real_, height = NA_real_),
  finally = { if (opened) try(grDevices::dev.off(), silent = TRUE) })
}

nm_clear_panel_theme <- function(p, hide_legend = TRUE, width_mm = NA_real_) {
  # PRESENTATION ONLY: preserve plot data, fitted/statistical layers, scales,
  # shapes, colours, and all underlying analysis results.
  # ggplot2 theme inheritance matters: axis.text.y overrides axis.text and
  # axis.title.x overrides axis.title. Normalize the *specific* elements as well.
  # Keep element_markdown's class (never merge it with element_text).
  nm_font_element <- function(existing, size) {
    if (inherits(existing, "element_blank")) return(NULL)
    if (inherits(existing, "element_markdown")) {
      return(ggtext::element_markdown(size = size, family = nm_style$font_family))
    }
    ggplot2::element_text(size = size, family = nm_style$font_family)
  }
  nm_line_element <- function(existing, width = nm_style$axis_line) {
    if (inherits(existing, "element_blank")) return(NULL)
    ggplot2::element_line(linewidth = width, colour = "black")
  }

  th <- ggplot2::theme(
    text = ggplot2::element_text(family = nm_style$font_family),
    axis.title = ggplot2::element_text(
      family = nm_style$font_family, size = nm_style$axis_title,
      colour = "black"),
    axis.text = ggplot2::element_text(
      family = nm_style$font_family, size = nm_style$axis_text,
      colour = "black"),
    axis.line = ggplot2::element_line(
      colour = "black", linewidth = nm_style$axis_line),
    axis.ticks = ggplot2::element_line(
      colour = "black", linewidth = nm_style$axis_line),
    axis.ticks.length = grid::unit(1, "mm"),
    legend.title = ggplot2::element_text(
      family = nm_style$font_family, size = nm_style$legend_title),
    legend.text = ggplot2::element_text(
      family = nm_style$font_family, size = nm_style$legend_text),
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank()
  )

  # Override explicit axis-specific sizes present in older plots (often 10-15pt).
  # Blank axes must remain blank. The new elements merge with originals,
  # preserving angle, face, justification and margins when not redefined.
  if (!inherits(p, "patchwork")) {
    specifications <- list(
      "axis.title.x" = nm_style$axis_title,
      "axis.title.y" = nm_style$axis_title,
      "axis.text.x" = nm_style$axis_text,
      "axis.text.y" = nm_style$axis_text,
      "strip.text" = nm_style$strip_text,
      "strip.text.x" = nm_style$strip_text,
      "strip.text.y" = nm_style$strip_text,
      "legend.title" = nm_style$legend_title,
      "legend.text" = nm_style$legend_text
    )
    overrides <- list()
    for (key in names(specifications)) {
      # An absent strip.text.x/y must inherit the existing strip.text class.
      # Otherwise a new element_text child can conflict with an
      # element_markdown parent during ggplot2 theme merging.
      if (key %in% c("strip.text.x", "strip.text.y") &&
          is.null(p$theme[[key]])) next
      el <- nm_font_element(p$theme[[key]], specifications[[key]])
      if (!is.null(el)) overrides[[key]] <- el
    }
    for (key in c("axis.line.x", "axis.line.y", "axis.ticks.x", "axis.ticks.y")) {
      el <- nm_line_element(p$theme[[key]])
      if (!is.null(el)) overrides[[key]] <- el
    }
    # Preserve absent or deliberately blank backgrounds; thin an existing
    # facet border rather than retaining the previous 0.7mm heavy frame.
    sb <- p$theme$strip.background
    if (!is.null(sb) && inherits(sb, "element_rect")) {
      overrides[["strip.background"]] <- ggplot2::element_rect(
        linewidth = nm_style$axis_line)
    }
    if (length(overrides)) th <- th + do.call(ggplot2::theme, overrides)
  }

  if (isTRUE(nm_export$remove_plot_titles)) {
    th <- th + ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank())
  }
  if (is.finite(width_mm) && width_mm < 25) {
    th <- th + ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 5.5),
      plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
  }
  if (hide_legend) th <- th + ggplot2::theme(legend.position = "none")
  if (inherits(p, "patchwork")) return(p & th)
  p + th
}

nm_get_legend <- function(p) {
  if (!nm_export$separate_legends) return(NULL)
  q <- nm_clear_panel_theme(p, hide_legend = FALSE)
  g <- tryCatch(ggplot2::ggplotGrob(q), error = function(e) NULL)
  if (is.null(g)) return(NULL)
  ix <- grep("^guide-box", g$layout$name)
  ix <- ix[!vapply(ix, function(j) inherits(g$grobs[[j]], "zeroGrob"), logical(1))]
  if (!length(ix)) return(NULL)
  # Modern ggplot2 can create empty guide-box positions. Keep the first real one.
  g$grobs[[ix[1]]]
}

# The p-value Raw/FDR columns are annotations of individual proteins; they
# remain attached to the heatmap. Only the Z-score, Age, and Sex KEYS go into
# separate standalone legend files.
nm_draw_heatmap <- function(ht) {
  ComplexHeatmap::draw(
    ht,
    show_heatmap_legend = FALSE,
    show_annotation_legend = FALSE,
    padding = grid::unit(c(4, 4, 4, 4), "mm")
  )
  # Raw and FDR headers are already part of the p-value annotation.
  # Do not add floating decoration that can overlap labels in narrow panels.
}

# Standalone, editable ComplexHeatmap legend. The colours and limits below
# come from the very same variables used to construct the heatmaps.
nm_heatmap_legend <- function() {
  ComplexHeatmap::packLegend(
    ComplexHeatmap::Legend(
      title = "Z-score", col_fun = heatmap_col_fun,
      at = c(-z_clip, 0, z_clip),
      labels = c(paste0("-", z_clip), "0", as.character(z_clip)),
      legend_height = grid::unit(24, "mm"),
      title_gp = grid::gpar(fontsize = nm_style$legend_title, fontface = "bold"),
      labels_gp = grid::gpar(fontsize = nm_style$legend_text)
    ),
    ComplexHeatmap::Legend(
      title = "Age", at = names(age_colours),
      legend_gp = grid::gpar(fill = unname(age_colours)),
      grid_width = grid::unit(3.5, "mm"),
      grid_height = grid::unit(3.5, "mm"),
      title_gp = grid::gpar(fontsize = nm_style$legend_title, fontface = "bold"),
      labels_gp = grid::gpar(fontsize = nm_style$legend_text)
    ),
    ComplexHeatmap::Legend(
      title = "Sex", at = names(sex_colours),
      legend_gp = grid::gpar(fill = unname(sex_colours)),
      grid_width = grid::unit(3.5, "mm"),
      grid_height = grid::unit(3.5, "mm"),
      title_gp = grid::gpar(fontsize = nm_style$legend_title, fontface = "bold"),
      labels_gp = grid::gpar(fontsize = nm_style$legend_text)
    ),
    direction = "vertical", gap = grid::unit(2.5, "mm")
  )
}

nm_draw_heatmap_legend <- function() {
  lg <- nm_heatmap_legend()
  grid::grid.newpage()
  ComplexHeatmap::draw(
    lg,
    x = grid::unit(3, "mm"),
    y = grid::unit(1, "npc") - grid::unit(3, "mm"),
    just = c("left", "top")
  )
}

reexport <- function(id) {
  item <- nm_registry[[id]]
  if (is.null(item)) stop("Unknown panel: ", id, ". Available: ", paste(ls(nm_registry), collapse = ", "))
  sz <- nm_dimensions(id)
  png <- item$file
  svg <- sub("[.]png$", ".svg", png, ignore.case = TRUE)
  if (item$kind == "heatmap") {
    # Heatmap body only: legends are not drawn into the data panel.
    draw_fn <- function() nm_draw_heatmap(item$plot)
    nm_save_png(png, sz[["width"]], sz[["height"]], draw_fn)
    nm_save_svg(svg, sz[["width"]], sz[["height"]], draw_fn)

    # Export the three keys (Z-score / Age / Sex) together in a separate file.
    # set_legend_size("HM_Age", width_mm, height_mm) overrides the default.
    ld <- nm_legend_overrides[[id]]
    if (is.null(ld)) ld <- nm_legend_dims[[nm_family(id)]]
    if (is.null(ld)) ld <- c(width = 44, height = 74)
    base <- sub("[.]png$", "", png, ignore.case = TRUE)
    nm_save_png(paste0(base, "_legend.png"), ld[["width"]], ld[["height"]],
                nm_draw_heatmap_legend)
    nm_save_svg(paste0(base, "_legend.svg"), ld[["width"]], ld[["height"]],
                nm_draw_heatmap_legend)
  } else {
    # Plot construction and its statistical inputs are never modified.
    styled_plot <- nm_style_plot(item$plot, id)
    p <- nm_clear_panel_theme(styled_plot, hide_legend = nm_export$separate_legends,
                              width_mm = sz[["width"]])
    nm_save_png(png, sz[["width"]], sz[["height"]], function() print(p))
    nm_save_svg(svg, sz[["width"]], sz[["height"]], function() print(p))
    lg <- nm_get_legend(styled_plot)
    if (!is.null(lg)) {
      ld <- nm_legend_overrides[[id]]
      if (is.null(ld)) ld <- nm_legend_dims[[nm_family(id)]]
      if (is.null(ld)) ld <- c(width = nm_export$legend_width_mm,
                              height = nm_export$legend_height_mm)
      # Text-heavy legends grow automatically to avoid clipped content.
      measured <- nm_measure_legend(lg)
      if (all(is.finite(measured))) ld <- pmax(ld, measured + c(width = 8, height = 8))
      base <- sub("[.]png$", "", png, ignore.case = TRUE)
      nm_save_png(paste0(base, "_legend.png"), ld[["width"]], ld[["height"]],
                  function() grid::grid.draw(lg))
      nm_save_svg(paste0(base, "_legend.svg"), ld[["width"]], ld[["height"]],
                  function() grid::grid.draw(lg))
    }
  }
  message("Exported ", id, " [", round(sz[[1]], 1), " x ", round(sz[[2]], 1), " mm]")
  invisible(c(png = png, svg = svg))
}

nm_save_plot <- function(plot, file, width = 7.8, height = 6.2, dpi = 300) {
  png <- nm_png_path(file)
  id <- nm_id(png)
  existing <- nm_registry[[id]]
  if (!is.null(existing) && !identical(existing$file, png)) {
    id <- paste0(basename(dirname(png)), "__", id)
    if (!is.null(nm_registry[[id]]) && !identical(nm_registry[[id]]$file, png)) {
      stop("Ambiguous panel ID: ", id)
    }
  }
  nm_registry[[id]] <- list(kind = "ggplot", plot = plot, file = png,
                            created_in = nm_current_chunk(),
                            original_mm = c(width = width * 25.4, height = height * 25.4))
  reexport(id)
  invisible(png)
}

nm_save_heatmap <- function(ht, file_stem, width, height, dpi = 300) {
  png <- nm_png_path(file_stem)
  id <- nm_id(png)
  nm_registry[[id]] <- list(kind = "heatmap", plot = ht, file = png,
                            created_in = nm_current_chunk(),
                            original_mm = c(width = width * 25.4, height = height * 25.4))
  reexport(id)
  invisible(png)
}

export_all_panels <- function() invisible(lapply(ls(nm_registry), reexport))
reexport_family <- function(family) {
  ids <- ls(nm_registry)
  ids <- ids[vapply(ids, function(x) identical(nm_family(x), family), logical(1))]
  invisible(lapply(ids, reexport))
}
figure_export_manifest <- function(file = file.path(out_dir, "Figure_Export_Manifest.csv")) {
  ids <- ls(nm_registry)
  if (!length(ids)) return(invisible(data.frame()))
  df <- do.call(rbind, lapply(ids, function(id) {
    it <- nm_registry[[id]]
    d <- nm_dimensions(id)
    data.frame(Panel = id,
               Description = if (exists("nm_panel_notes", inherits = TRUE) &&
                                 !is.null(nm_panel_notes[[id]])) nm_panel_notes[[id]] else "",
               Plotting_chunk = if (is.null(it$created_in)) NA_character_ else it$created_in,
               Family = nm_family(id), Width_mm = unname(d[[1]]),
               Height_mm = unname(d[[2]]), PNG = it$file,
               SVG = sub("[.]png$", ".svg", it$file, ignore.case = TRUE),
               Legend_PNG = if (file.exists(sub("[.]png$", "_legend.png", it$file,
                                               ignore.case = TRUE)))
                 sub("[.]png$", "_legend.png", it$file, ignore.case = TRUE) else "",
               Legend_SVG = if (file.exists(sub("[.]png$", "_legend.svg", it$file,
                                               ignore.case = TRUE)))
                 sub("[.]png$", "_legend.svg", it$file, ignore.case = TRUE) else "",
               stringsAsFactors = FALSE)
  }))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, file, row.names = FALSE)
  invisible(df)
}

# Figure-finding utilities. Use list_panels() to locate exact panel IDs.
list_panels <- function(pattern = NULL) {
  ids <- ls(nm_registry)
  if (!is.null(pattern)) ids <- grep(pattern, ids, value = TRUE, ignore.case = TRUE)
  if (!length(ids)) return(data.frame())
  dat <- do.call(rbind, lapply(ids, function(id) {
    sz <- nm_dimensions(id)
    data.frame(
      Panel_ID = id,
      Description = if (exists("nm_panel_notes", inherits = TRUE) &&
                        !is.null(nm_panel_notes[[id]])) nm_panel_notes[[id]] else "",
      Plotting_chunk = nm_registry[[id]]$created_in,
      Family = nm_family(id), Width_mm = unname(sz[["width"]]),
      Height_mm = unname(sz[["height"]]),
      PNG = nm_registry[[id]]$file, stringsAsFactors = FALSE
    )
  }))
  print(dat, row.names = FALSE)
  invisible(dat)
}

panel_info <- function(id) {
  if (is.null(nm_registry[[id]])) {
    stop("Unknown panel: ", id, ". Try list_panels('", id, "').")
  }
  info <- nm_registry[[id]]
  sz <- nm_dimensions(id)
  print(list(
    Panel_ID = id, Family = nm_family(id),
    Description = if (exists("nm_panel_notes", inherits = TRUE)) nm_panel_notes[[id]] else NULL,
    Plotting_chunk = info$created_in,
    Size_mm = sz, PNG = info$file,
    SVG = sub("[.]png$", ".svg", info$file, ignore.case = TRUE)
  ))
  invisible(sz)
}


# Audit rendered text sizes in a specific SVG after export. This catches
# plot-specific geom_text or rich-text sizes not controlled by a ggplot theme.
# Example: font_audit("Effect_withinBin_bySex")
font_audit <- function(id, threshold = 8) {
  if (is.null(nm_registry[[id]])) stop("Unknown panel: ", id)
  svg <- sub("[.]png$", ".svg", nm_registry[[id]]$file, ignore.case = TRUE)
  if (!file.exists(svg)) stop("Export the panel before auditing its SVG: ", svg)
  x <- readLines(svg, warn = FALSE, encoding = "UTF-8")
  matches <- regmatches(
    x,
    regexec("font-size:[[:space:]]*([0-9.]+)(?:px|pt)", x, perl = TRUE)
  )
  sz <- vapply(matches, function(y) {
    if (length(y) < 2L) NA_real_ else as.numeric(y[2])
  }, numeric(1))
  keep <- which(is.finite(sz) & sz > threshold)
  if (!length(keep)) {
    message("No SVG text with font-size above ", threshold, " detected: ", id)
    return(invisible(data.frame()))
  }
  label <- trimws(gsub("<[^>]+>", "", x[keep]))
  result <- data.frame(Text = substr(label, 1, 70),
                       Font_size = sz[keep], stringsAsFactors = FALSE)
  message(nrow(result), " SVG text element(s) exceed ", threshold,
          ". Sizes are in SVG font units.")
  print(result, row.names = FALSE)
  invisible(result)
}

message("Embedded figure helpers ready; use list_panels(), panel_info(), reexport().")
