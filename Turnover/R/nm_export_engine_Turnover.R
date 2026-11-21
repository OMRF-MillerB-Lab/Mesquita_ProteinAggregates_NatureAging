# =============================================================================
# nm_export_engine_Turnover.R  (figure export engine for the Turnover report)
#
# Functions only, moved verbatim from the original chunk 'packages'.
# Sizes (nm_dims) and fonts (nm_style) are NOT set here: edit chunk 'nm-figure-config' in the Rmd.
# Source AFTER that chunk (save_nm_plot reads nm_dims when it is called).
# =============================================================================

# ── Fully editable, Inkscape-safe SVG workflow ───────────────────────────────
# This is the same structure used for the revised aggregation-propensity figures.
mm_to_in <- function(mm) {
  mm / 25.4
}

make_svg_inkscape_safe <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")

  # Remove only svglite's device-level background rectangle.
  x <- x[
    !grepl(
      "^\\s*<rect width='100%' height='100%'",
      x
    )
  ]

  add_style_property <- function(line, property, value) {
    if (grepl(paste0(property, "\\s*:"), line, perl = TRUE)) {
      return(line)
    }

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

    if (grepl("<defs(?:\\s|>)", line, perl = TRUE)) {
      in_defs <- TRUE
    }

    if (grepl("<g[^>]*class='glyphgroup'", line, perl = TRUE)) {
      in_glyphgroup <- TRUE
    }

    is_shape <- grepl(
      "^\\s*<(line|polyline|polygon|rect|circle|path)\\b",
      line,
      perl = TRUE
    )

    is_text <- grepl(
      "^\\s*<text\\b",
      line,
      perl = TRUE
    )

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

    if (grepl("</g>", line, fixed = TRUE) && in_glyphgroup) {
      in_glyphgroup <- FALSE
    }

    if (grepl("</defs>", line, fixed = TRUE)) {
      in_defs <- FALSE
    }
  }

  # Remove attributes that can stretch/compress edited text in Inkscape.
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
    if (grepl("<defs(?:\\s|>)", line, perl = TRUE)) {
      in_defs <- TRUE
    }

    if (!in_defs && grepl(
      "^\\s*<(line|polyline|polygon|rect|circle)\\b",
      line,
      perl = TRUE
    )) {
      if (!grepl("stroke\\s*:", line, perl = TRUE)) {
        bad <- c(bad, line)
      }
    }

    if (grepl("</defs>", line, fixed = TRUE)) {
      in_defs <- FALSE
    }
  }

  if (length(bad) > 0L) {
    stop(
      "SVG still contains drawable elements with CSS-only stroke styling: ",
      basename(file)
    )
  }

  invisible(TRUE)
}

save_plot_svg <- function(
  plot,
  file,
  width_mm,
  height_mm,
  background = "white"
) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

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
        bg = background
      )
      device_open <- TRUE
      print(plot)
      grDevices::dev.off()
      device_open <- FALSE
      NULL
    },
    error = function(e) e,
    finally = {
      if (device_open) {
        try(grDevices::dev.off(), silent = TRUE)
      }
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
    stop("The SVG device closed without creating: ", file)
  }

  make_svg_inkscape_safe(file)
  check_svg_inkscape_safe(file)
  invisible(file)
}

save_nm_plot <- function(
  plot,
  file_stub,
  size_key,
  width_mm = NULL,
  height_mm = NULL,
  dpi = 600
) {
  dims <- nm_dims[[size_key]]

  if (is.null(dims)) {
    stop("Unknown Nature Metabolism size key: ", size_key)
  }

  if (is.null(width_mm)) width_mm <- unname(dims[["width"]])
  if (is.null(height_mm)) height_mm <- unname(dims[["height"]])

  png_file <- paste0(file_stub, ".png")
  svg_file <- paste0(file_stub, ".svg")

  dir.create(dirname(file_stub), recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    filename = png_file,
    plot = plot,
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = dpi,
    bg = "white",
    device = ragg::agg_png,
    limitsize = FALSE
  )

  save_plot_svg(
    plot = plot,
    file = svg_file,
    width_mm = width_mm,
    height_mm = height_mm,
    background = "white"
  )

  if (!file.exists(png_file) || file.info(png_file)$size <= 0) {
    stop("PNG was not saved correctly: ", png_file)
  }

  invisible(list(png = png_file, svg = svg_file))
}
