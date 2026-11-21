# =============================================================================
# nm_export_engine_OT_Prop.R  (figure export engine for 7.OldTrt_Prop_v2.Rmd)
#
# Functions only. Moved verbatim from the original Rmd chunks 'nature-metabolism-svg-helpers'
# and 'nm-inline-panel-registry'; no analysis code lives here.
# Sizes, fonts and line widths are NOT set here: edit 'nm-figure-config' and 'nm-figure-dimensions'
# in the Rmd (nm_style, nm_export, nm_svg_dims, nm_legend_dims, nm_dims).
# Source AFTER those chunks and after Settings (needs out_dir).
# =============================================================================

mm_to_in <- function(mm) mm / 25.4

safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

save_plot_png <- function(plot, file, width_mm, height_mm, dpi = 600) {
  safe_dir_create(dirname(file))
  ragg::agg_png(
    filename = file,
    width = width_mm,
    height = height_mm,
    units = "mm",
    res = dpi,
    background = "white"
  )
  print(plot)
  grDevices::dev.off()
  if (!file.exists(file) || file.info(file)$size <= 0) stop("PNG was not saved correctly: ", file)
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

    is_shape <- grepl("^\\s*<(line|polyline|polygon|rect|circle|path)\\b", line, perl = TRUE)
    is_text <- grepl("^\\s*<text\\b", line, perl = TRUE)
    is_device_bg <- grepl("^\\s*<rect width='100%' height='100%'", line, perl = TRUE)

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

    if (is_text && !in_defs) line <- add_style_property(line, "white-space", "pre")
    x[i] <- line

    if (grepl("</g>", line, fixed = TRUE) && in_glyphgroup) in_glyphgroup <- FALSE
    if (grepl("</defs>", line, fixed = TRUE)) in_defs <- FALSE
  }

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
    is_device_bg <- grepl("^\\s*<rect width='100%' height='100%'", line, perl = TRUE)
    if (!in_defs && !is_device_bg && grepl("^\\s*<(line|polyline|polygon|rect|circle)\\b", line, perl = TRUE)) {
      if (!grepl("stroke\\s*:", line, perl = TRUE)) bad <- c(bad, line)
    }
    if (grepl("</defs>", line, fixed = TRUE)) in_defs <- FALSE
  }

  if (length(bad) > 0L) {
    stop("SVG still contains drawable elements with CSS-only stroke styling: ", basename(file))
  }
  invisible(TRUE)
}

# Publish a fully processed SVG to its destination in one operation.
# Rendering/cleanup occurs in the local R temporary directory, so we do not
# reopen a Dropbox/OneDrive SVG for in-place text rewriting after svglite.
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

save_plot_svg <- function(plot, file, width_mm, height_mm, background = "white") {
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

  if (!is.null(save_error)) stop(
    "Could not save editable SVG:\n", normalizePath(file, mustWork = FALSE),
    "\n\nSVG-device message:\n", conditionMessage(save_error)
  )
  if (!file.exists(svg_stage)) stop("SVG device closed without creating: ", svg_stage)

  make_svg_inkscape_safe(svg_stage)
  check_svg_inkscape_safe(svg_stage)
  nm_publish_svg(svg_stage, file)
  invisible(file)
}

nm_heatmap_height <- function(n_proteins) {
  max(
    78,
    min(
      125,
      2.5 * n_proteins + 20
    )
  )
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
      # The detection-propensity facets use rich-text headers containing
      # separate title/stat font sizes; preserve them at export.
      if (startsWith(name, "strip.text") &&
          inherits(plot$theme[["strip.text"]], "element_markdown")) next
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
  p_legend <- tryCatch(
    nm_set_legend_position(plot, "right"),
    error = function(e) NULL
  )
  if (is.null(p_legend)) return(NULL)

  p_legend <- nm_apply_nature_theme(p_legend)

  leg <- tryCatch(
    cowplot::get_legend(p_legend),
    error = function(e) NULL
  )

  if (is.null(leg) || inherits(leg, "zeroGrob")) return(NULL)
  leg
}

save_grob_png <- function(grob, file, width_mm, height_mm, background = "white", dpi = 600) {
  if (is.null(grob)) return(invisible(NULL))
  safe_dir_create(dirname(file))

  ragg::agg_png(
    filename = file,
    width = width_mm,
    height = height_mm,
    units = "mm",
    res = dpi,
    background = background
  )
  grid::grid.newpage()
  grid::grid.draw(grob)
  grDevices::dev.off()

  if (!file.exists(file) || file.info(file)$size <= 0) {
    stop("Legend PNG was not saved correctly: ", file)
  }
  invisible(file)
}

save_grob_svg <- function(grob, file, width_mm, height_mm, background = "white") {
  if (is.null(grob)) return(invisible(NULL))
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

  if (!is.null(save_error)) stop(
    "Could not save editable legend SVG: ", conditionMessage(save_error)
  )
  if (!file.exists(svg_stage)) stop("Legend SVG was not created: ", svg_stage)

  make_svg_inkscape_safe(svg_stage)
  check_svg_inkscape_safe(svg_stage)
  nm_publish_svg(svg_stage, file)
  invisible(file)
}

nm_save_separate_legend <- function(plot, file_stub, size_key) {
  if (!isTRUE(nm_export$separate_legends)) return(NULL)

  leg <- nm_extract_legend(plot)
  if (is.null(leg)) return(NULL)

  dims <- nm_legend_dims[[size_key]]
  if (is.null(dims)) dims <- nm_legend_dims[["generic"]]
  if (exists("legend_size_overrides", inherits = TRUE)) {
    custom <- legend_size_overrides[[basename(file_stub)]]
    if (!is.null(custom)) dims <- custom
  }
  suffix <- nm_export$legend_suffix

  save_grob_png(
    leg,
    paste0(file_stub, suffix, ".png"),
    width_mm = unname(dims[["width"]]),
    height_mm = unname(dims[["height"]]),
    background = nm_export$background,
    dpi = nm_export$dpi
  )

  save_grob_svg(
    leg,
    paste0(file_stub, suffix, ".svg"),
    width_mm = unname(dims[["width"]]),
    height_mm = unname(dims[["height"]]),
    background = nm_export$background
  )

  leg
}

save_nm_both <- function(
  plot,
  file_stub,
  size_key,
  width_mm = NULL,
  height_mm = NULL,
  dpi = nm_export$dpi,
  save_legend = TRUE,
  register = TRUE
) {
  # Exact panel-ID dimensions take priority over any family/explicit dimensions.
  original_width <- width_mm
  original_height <- height_mm
  dims <- nm_resolve_dims(file_stub, size_key, width_mm, height_mm)
  width_mm <- unname(dims[["width"]])
  height_mm <- unname(dims[["height"]])
  if (isTRUE(register)) nm_register_plot(plot, file_stub, size_key,
                                        width_mm = original_width, height_mm = original_height,
                                        save_legend = save_legend)

  # Save a standalone legend only when requested. This matters for families of
  # plots that share the same legend (for example, the individual cellular-
  # component correlation panels): exporting a duplicate legend for every
  # panel is unnecessary and can create very long Windows/Dropbox paths.
  leg <- if (isTRUE(save_legend)) {
    nm_save_separate_legend(plot, file_stub = file_stub, size_key = size_key)
  } else {
    NULL
  }

  # When a separate legend was successfully written, remove it from the panel.
  # When save_legend = FALSE, preserve whatever legend setting is already on
  # the plot (the individual CC panels are explicitly created with no legend).
  plot_export <- if (!is.null(leg)) nm_set_legend_position(plot, "none") else plot

  plot_export <- nm_apply_nature_theme(plot_export)

  save_plot_png(
    plot = plot_export,
    file = paste0(file_stub, ".png"),
    width_mm = width_mm,
    height_mm = height_mm,
    dpi = dpi
  )

  save_plot_svg(
    plot = plot_export,
    file = paste0(file_stub, ".svg"),
    width_mm = width_mm,
    height_mm = height_mm,
    background = nm_export$background
  )

  invisible(list(
    png = paste0(file_stub, ".png"),
    svg = paste0(file_stub, ".svg"),
    legend_png = if (!is.null(leg)) paste0(file_stub, nm_export$legend_suffix, ".png") else NULL,
    legend_svg = if (!is.null(leg)) paste0(file_stub, nm_export$legend_suffix, ".svg") else NULL
  ))
}

# The registry is self-contained and survives reruns of the helper chunks.
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
  if (is.null(z) || !length(z) || !nzchar(z[[1]])) return(NA_character_)
  as.character(z[[1]])
}

nm_register_plot <- function(plot, file_stub, size_key, width_mm, height_mm,
                             save_legend) {
  id <- basename(file_stub)
  entry <- if (exists(id, envir = panel_registry, inherits = FALSE)) {
    get(id, envir = panel_registry, inherits = FALSE)
  } else NULL
  paths <- unique(c(if (!is.null(entry)) entry$paths, file_stub))
  assign(id, list(plot = plot, paths = paths, size_key = size_key,
                  original_width = width_mm, original_height = height_mm,
                  save_legend = save_legend,
                  chunk = if (!is.null(entry) && is.na(nm_current_chunk())) entry$chunk
                          else nm_current_chunk()), envir = panel_registry)
  invisible(id)
}

nm_resolve_dims <- function(file_stub, size_key, width_mm = NULL, height_mm = NULL) {
  id <- basename(file_stub)
  defaults <- nm_svg_dims[[size_key]]
  if (is.null(defaults)) stop("Unknown size family: ", size_key)
  per_panel <- nm_dims[[id]]
  family <- nm_dims[[size_key]]
  choose <- function(k, supplied) {
    choices <- list(if (!is.null(per_panel)) per_panel[[k]], supplied,
                    if (!is.null(family)) family[[k]], defaults[[k]])
    for (x in choices) if (length(x) && is.finite(x[[1]]) && x[[1]] > 0) return(as.numeric(x[[1]]))
    stop("Invalid ", k, " for panel: ", id)
  }
  c(width = choose("width", width_mm), height = choose("height", height_mm))
}

list_panels <- function(pattern = NULL) {
  ids <- sort(ls(panel_registry))
  if (!is.null(pattern)) ids <- grep(pattern, ids, value = TRUE)
  if (!length(ids)) return(data.frame())
  do.call(rbind, lapply(ids, function(id) {
    e <- get(id, envir = panel_registry, inherits = FALSE)
    d <- nm_resolve_dims(e$paths[[1]], e$size_key, e$original_width, e$original_height)
    data.frame(Panel = id, Width_mm = d[["width"]], Height_mm = d[["height"]],
               Size_family = e$size_key, Plotting_chunk = e$chunk,
               File_stub = paste(e$paths, collapse = "; "), stringsAsFactors = FALSE)
  }))
}

panel_info <- function(id) {
  if (!exists(id, envir = panel_registry, inherits = FALSE)) stop("Unknown panel: ", id)
  x <- list_panels()
  x[x$Panel == id, , drop = FALSE]
}

set_panel_size <- function(id, width_mm, height_mm) {
  if (!exists(id, panel_registry, inherits = FALSE)) stop("Unknown panel: ", id)
  nm_dims[[id]] <<- c(width = width_mm, height = height_mm)
  invisible(panel_info(id))
}

set_family_size <- function(size_key, width_mm, height_mm) {
  if (is.null(nm_svg_dims[[size_key]])) stop("Unknown figure family: ", size_key)
  nm_dims[[size_key]] <<- c(width = width_mm, height = height_mm)
  invisible(size_key)
}

set_legend_size <- function(id, width_mm, height_mm) {
  if (!exists(id, panel_registry, inherits = FALSE)) stop("Unknown panel: ", id)
  if (!exists("legend_size_overrides", inherits = FALSE)) legend_size_overrides <<- list()
  legend_size_overrides[[id]] <<- c(width = width_mm, height = height_mm)
  invisible(id)
}

reexport <- function(id) {
  if (!exists(id, panel_registry, inherits = FALSE)) {
    stop("Unknown panel: ", id, ". Run its plotting chunk first; call list_panels().")
  }
  e <- get(id, envir = panel_registry, inherits = FALSE)
  for (stub in e$paths) {
    save_nm_both(e$plot, stub, e$size_key,
                 width_mm = e$original_width, height_mm = e$original_height,
                 save_legend = e$save_legend, register = FALSE)
  }
  message("Re-exported ", id, ": PNG, SVG and separate legend (when applicable).")
  invisible(e$paths)
}

reexport_family <- function(size_key) {
  x <- list_panels()
  ids <- x$Panel[x$Size_family == size_key]
  invisible(lapply(ids, reexport))
}

export_all_panels <- function(ids = sort(ls(panel_registry))) {
  invisible(lapply(ids, reexport))
}

figure_export_manifest <- function(file = file.path(out_dir, "Figure_Export_Manifest.csv")) {
  info <- list_panels()
  if (nrow(info)) {
    safe_dir_create(dirname(file))
    utils::write.csv(info, file, row.names = FALSE)
  }
  invisible(info)
}

font_audit <- function(id, threshold = 8) {
  if (!exists(id, panel_registry, inherits = FALSE)) stop("Unknown panel: ", id)
  e <- get(id, panel_registry, inherits = FALSE)
  files <- paste0(e$paths, ".svg")
  files <- files[file.exists(files)]
  for (file in files) {
    txt <- paste(readLines(file, warn = FALSE), collapse = "\n")
    font <- unlist(regmatches(txt, gregexpr("font-size:[[:space:]]*[0-9.]+", txt)))
    values <- suppressWarnings(as.numeric(sub(".*:[[:space:]]*", "", font)))
    message(basename(file), ": ", sum(values > threshold, na.rm = TRUE),
            " text-size declarations exceed ", threshold)
  }
  invisible(files)
}
