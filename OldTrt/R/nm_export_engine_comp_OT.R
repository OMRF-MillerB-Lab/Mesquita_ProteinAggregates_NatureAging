# =============================================================================
# nm_export_engine_comp_OT.R
# Figure-export engine for 4.Comp_OldTrt_v2.Rmd (moved verbatim out of the
# R Markdown; the only change is that the two lines that auto-installed svglite
# and ragg were removed, because the Rmd now checks for them up front).
# Presentation/export only: it never touches data, models, or statistics.
#
# Sourced by the Rmd AFTER the settings and figure-export settings chunks,
# because it reads out_dir, nm_export, nm_style, nm_dims and nm_family_dims from
# the session. Panels are registered by ID (= PNG file name without extension).
#
# Console helpers once the Rmd has been run:
#   list_panels()                          find exact panel IDs
#   panel_info("Mass_3x2_RawFDR")          inspect one panel
#   set_panel_size("Mass_3x2_RawFDR", 126, 82); reexport("Mass_3x2_RawFDR")
# =============================================================================

if (!exists("nm_comp_registry", inherits = FALSE)) {
  nm_comp_registry <- new.env(parent = emptyenv())
}
if (!exists("nm_comp_overrides", inherits = FALSE)) nm_comp_overrides <- list()
if (!exists("nm_comp_legend_overrides", inherits = FALSE)) nm_comp_legend_overrides <- list()

nm_comp_family <- function(id) {
  if (grepl("Mass_3x2_RawFDR|RawSum_3x2|_3x2_Raw", id)) return("grid")
  if (grepl("Effect|withinBin|percentagePoint|RawSum_TreatmentEffects", id)) return("effect")
  if (grepl("Detect|Delta_|Proteinwise", id)) return("detection")
  if (grepl("Top10|Top5|Top_10|Top_5|PooledControl", id)) return("top")
  if (grepl("Mass|Count", id)) return("composition")
  "general"
}

nm_comp_dims <- function(id, original_width_mm, original_height_mm) {
  fam <- nm_comp_family(id)
  dd <- c(width = original_width_mm, height = original_height_mm)
  if (!is.null(nm_family_dims[[fam]])) dd <- nm_family_dims[[fam]]
  # An exact-ID setting in the top Nature helper always wins.
  if (!is.null(nm_comp_overrides[[id]])) dd <- nm_comp_overrides[[id]]
  if (!is.null(nm_dims[[id]])) dd <- nm_dims[[id]]
  if (length(dd) != 2L || any(!is.finite(dd)) || any(dd <= 0)) {
    stop("Invalid panel dimensions for: ", id)
  }
  dd
}
set_panel_size <- function(id, width_mm, height_mm) {
  stopifnot(is.finite(width_mm), is.finite(height_mm), width_mm > 0, height_mm > 0)
  # Keep both manual top-chunk edits and function edits in ONE exact-ID list.
  nm_dims[[id]] <<- c(width = width_mm, height = height_mm)
  nm_comp_overrides[[id]] <<- NULL
  invisible(nm_dims[[id]])
}
set_family_size <- function(family, width_mm, height_mm) {
  if (is.null(nm_family_dims[[family]])) stop("Unknown figure family: ", family)
  nm_family_dims[[family]] <<- c(width = width_mm, height = height_mm)
  invisible(nm_family_dims[[family]])
}
set_legend_size <- function(id, width_mm, height_mm) {
  nm_comp_legend_overrides[[id]] <<- c(width = width_mm, height = height_mm)
  invisible(nm_comp_legend_overrides[[id]])
}

nm_comp_presentation <- function(p, legend = FALSE) {
  th <- ggplot2::theme(
    text = ggplot2::element_text(family = nm_style$font_family),
    axis.title = ggplot2::element_text(size = nm_style$axis_title),
    axis.text = ggplot2::element_text(size = nm_style$axis_text),
    strip.text = ggtext::element_markdown(size = nm_style$strip_text),
    # Preserve informative component and sex headings, but never the old
    # 12–16 pt titles/subtitles inside nested patchwork children.
    plot.title = ggplot2::element_text(size = nm_style$component_heading),
    plot.subtitle = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(linewidth = nm_style$axis_line),
    axis.ticks = ggplot2::element_line(linewidth = nm_style$axis_line),

    legend.title = ggplot2::element_text(size = nm_style$legend_title),
    legend.text = ggplot2::element_text(size = nm_style$legend_text),
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank()
  )
  # Keep the colored cellular-component headings inside patchwork figures,
  # but suppress old standalone ggplot titles and subtitles at export time.
  # Overall patchwork titles are omitted in the plotting chunks below.
  if (!legend) th <- th + ggplot2::theme(legend.position = "none")
  if (inherits(p, "patchwork")) {
    # Clear only the patchwork's outer heading, not child component labels.
    (p + patchwork::plot_annotation(title = NULL, subtitle = NULL)) & th
  } else {
    p + ggplot2::labs(title = NULL, subtitle = NULL) + th +
      ggplot2::theme(plot.title = ggplot2::element_blank(),
                     plot.subtitle = ggplot2::element_blank())
  }
}

nm_comp_clean_svg <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")
  # Keep the file editable and retain clip definitions, while eliminating the
  # svglite full-page background and dependence on a removable outer group.
  x <- x[!grepl("^[[:space:]]*<rect[^>]*width=['\\\"]100%['\\\"][^>]*height=['\\\"]100%['\\\"]", x, perl = TRUE)]
  x <- gsub("[[:space:]]+textLength=['\\\"][^'\\\"]*['\\\"]", "", x, perl = TRUE)
  x <- gsub("[[:space:]]+lengthAdjust=['\\\"][^'\\\"]*['\\\"]", "", x, perl = TRUE)
  # Unscoped rules survive Inkscape ungrouping.
  x <- gsub(".svglite ", "svg ", x, fixed = TRUE)
  outer <- grep("^[[:space:]]*<g class=['\\\"]svglite['\\\"]>[[:space:]]*$", x)
  if (length(outer) == 1L) {
    closing <- grep("^[[:space:]]*</g>[[:space:]]*$", x)
    closing <- closing[closing > outer]
    if (length(closing)) x <- x[-c(outer, tail(closing, 1L))]
  }
  writeLines(x, file, useBytes = TRUE)
  invisible(file)
}

nm_comp_save_png <- function(file, w, h, draw) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  ragg::agg_png(file, width = w, height = h, units = "mm",
                res = nm_export$dpi, background = "white")
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
nm_comp_save_svg <- function(file, w, h, draw) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "comp_ot_svg_", fileext = ".svg")
  on.exit(unlink(tmp), add = TRUE)
  svglite::svglite(tmp, width = w / 25.4, height = h / 25.4,
                   bg = "transparent",
                   system_fonts = list(sans = nm_export$svg_font))
  opened <- TRUE
  tryCatch({
    draw()
    grDevices::dev.off()
    opened <- FALSE
  }, finally = {
    if (opened) try(grDevices::dev.off(), silent = TRUE)
  })
  nm_comp_clean_svg(tmp)
  if (!file.copy(tmp, file, overwrite = TRUE)) stop("Could not replace SVG: ", file)
  invisible(file)
}

nm_comp_get_legend <- function(p) {
  if (!isTRUE(nm_export$separate_legends)) return(NULL)
  # Extract after primary figure exports, to keep the main figure available
  # even if a legend calculation takes longer than expected.
  q <- nm_comp_presentation(p, legend = TRUE)
  g <- tryCatch(ggplot2::ggplotGrob(q), error = function(e) NULL)
  if (is.null(g)) return(NULL)
  ix <- grep("^guide-box", g$layout$name)
  ix <- ix[!vapply(ix, function(k) inherits(g$grobs[[k]], "zeroGrob"), logical(1))]
  if (!length(ix)) return(NULL)
  g$grobs[[ix[1]]]
}

nm_comp_export <- function(id) {
  if (!exists(id, envir = nm_comp_registry, inherits = FALSE)) {
    if (id %in% names(nm_family_dims)) {
      stop("'", id, "' is a figure FAMILY. Use reexport_family(\"", id,
           "\") or copy an exact name from nm_dims.")
    }
    ids <- sort(ls(nm_comp_registry))
    stop("Figure ID not registered: '", id, "'. Run its plotting chunk first. ",
         "Available IDs: ", paste(utils::head(ids, 30), collapse = ", "))
  }
  item <- get(id, envir = nm_comp_registry)
  dims <- nm_comp_dims(id, item$width_mm, item$height_mm)
  p <- nm_comp_presentation(item$plot, legend = FALSE)
  fn <- sub("[.]png$", "", item$file, ignore.case = TRUE)
  message("[", id, "] PNG: ", dims[["width"]], " x ", dims[["height"]], " mm")
  nm_comp_save_png(item$file, dims[["width"]], dims[["height"]], function() print(p))
  message("[", id, "] editable SVG")
  nm_comp_save_svg(paste0(fn, ".svg"), dims[["width"]], dims[["height"]], function() print(p))
  lg <- nm_comp_get_legend(item$plot)
  if (!is.null(lg)) {
    dims_lg <- nm_comp_legend_overrides[[id]]
    if (is.null(dims_lg)) dims_lg <- c(width = nm_export$legend_min_width_mm,
                                      height = nm_export$legend_min_height_mm)
    measured <- tryCatch(c(width = grid::convertWidth(grid::grobWidth(lg), "mm", TRUE),
                            height = grid::convertHeight(grid::grobHeight(lg), "mm", TRUE)),
                         error = function(e) c(width = NA_real_, height = NA_real_))
    if (all(is.finite(measured))) dims_lg <- pmax(dims_lg, measured + nm_export$legend_margin_mm)
    nm_comp_save_png(paste0(fn, "_legend.png"), dims_lg[["width"]], dims_lg[["height"]],
                     function() grid::grid.draw(lg))
    nm_comp_save_svg(paste0(fn, "_legend.svg"), dims_lg[["width"]], dims_lg[["height"]],
                     function() grid::grid.draw(lg))
  }
  invisible(item$file)
}

nm_register_and_export <- function(p, file, width, height) {
  if (!grepl("[.]png$", file, ignore.case = TRUE)) file <- paste0(file, ".png")
  id <- tools::file_path_sans_ext(basename(file))
  if (exists(id, envir = nm_comp_registry, inherits = FALSE)) {
    old <- get(id, envir = nm_comp_registry)
    if (!identical(old$file, file)) {
      stop("Duplicate filename figure ID ", id, " from ", old$file, " and ", file)
    }
  }
  assign(id, list(plot = p, file = file,
                  width_mm = width * 25.4, height_mm = height * 25.4),
         envir = nm_comp_registry)
  nm_comp_export(id)
}
reexport <- function(id) nm_comp_export(id)
reexport_family <- function(family) {
  ids <- ls(nm_comp_registry)
  ids <- ids[vapply(ids, function(k) identical(nm_comp_family(k), family), logical(1))]
  if (!length(ids)) stop("No registered panels in family: ", family)
  invisible(lapply(ids, reexport))
}
list_panels <- function(pattern = NULL) {
  ids <- sort(ls(nm_comp_registry))
  if (!is.null(pattern)) ids <- grep(pattern, ids, value = TRUE, ignore.case = TRUE)
  if (!length(ids)) return(invisible(data.frame()))
  tb <- do.call(rbind, lapply(ids, function(id) {
    rec <- get(id, envir = nm_comp_registry)
    d <- nm_comp_dims(id, rec$width_mm, rec$height_mm)
    data.frame(ID = id, Family = nm_comp_family(id),
               Width_mm = d[["width"]], Height_mm = d[["height"]],
               Reexport = paste0('reexport("', id, '")'),
               PNG = rec$file)
  }))
  print(tb, row.names = FALSE)
  invisible(tb)
}
panel_info <- function(id) {
  x <- get(id, envir = nm_comp_registry)
  d <- nm_comp_dims(id, x$width_mm, x$height_mm)
  print(list(ID = id, Dimensions_mm = d, PNG = x$file))
  invisible(d)
}
figure_export_manifest <- function(file = file.path(out_dir, "Figure_Export_Manifest.csv")) {
  ids <- sort(ls(nm_comp_registry))
  if (!length(ids)) return(invisible(data.frame()))
  df <- do.call(rbind, lapply(ids, function(id) {
    rec <- get(id, envir = nm_comp_registry)
    dm <- nm_comp_dims(id, rec$width_mm, rec$height_mm)
    data.frame(ID = id, Family = nm_comp_family(id),
               Width_mm = dm[["width"]], Height_mm = dm[["height"]],
               PNG = rec$file,
               SVG = sub("[.]png$", ".svg", rec$file))
  }))
  utils::write.csv(df, file, row.names = FALSE)
  invisible(df)
}
message("Old-Treatments composition Nature helper ready: list_panels(), panel_info(), reexport().")
