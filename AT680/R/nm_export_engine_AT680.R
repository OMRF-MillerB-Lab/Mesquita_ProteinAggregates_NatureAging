# =============================================================================
# nm_export_engine_AT680.R  (figure export engine for AT680_AggregateQuant_v2.Rmd)
#
# Functions and the panel registry only, moved verbatim from the original chunk 'nature-helper'.
# Sizes, fonts and colours are NOT set here: edit 'nm-figure-config' in the Rmd
# (nm_style, nm_export, nm_dims_by_class, figure_families, colours).
# Source AFTER that chunk and after the 'settings' chunk (needs out_dir).
# =============================================================================

# Registry stores the *already built* ggplot/patchwork objects for quick re-export.
nm_registry <- new.env(parent = emptyenv())

list_configured_panels <- function() {
  data.frame(
    id = names(nm_dims),
    folder = rep(names(nm_dims_by_class), lengths(nm_dims_by_class)),
    family = unname(figure_families[names(nm_dims)]),
    width_mm = vapply(nm_dims, `[[`, numeric(1), "width"),
    height_mm = vapply(nm_dims, `[[`, numeric(1), "height"),
    row.names = NULL
  )
}

list_panels <- function(pattern = NULL) {
  x <- list_configured_panels()
  x$registered <- x$id %in% ls(nm_registry)
  if (!is.null(pattern)) x <- x[grepl(pattern, x$id, ignore.case = TRUE), ]
  x
}

panel_info <- function(id) {
  x <- list_panels()
  ans <- x[x$id == id, , drop = FALSE]
  if (nrow(ans) != 1) stop("Unknown panel ID: ", id)
  ans
}

set_panel_size <- function(id, width_mm, height_mm) {
  if (!id %in% names(nm_dims)) stop("Unknown panel ID: ", id)
  stopifnot(is.finite(width_mm), is.finite(height_mm),
            width_mm > 0, height_mm > 0)
  nm_dims[[id]] <<- c(width = width_mm, height = height_mm)
  invisible(panel_info(id))
}

set_family_size <- function(family, width_mm, height_mm) {
  ids <- names(figure_families)[figure_families == family]
  if (!length(ids)) stop("Unknown family: ", family)
  for (id in ids) set_panel_size(id, width_mm, height_mm)
  invisible(list_panels())
}

register_panel <- function(id, plot) {
  if (!id %in% names(nm_dims)) stop("Configure dimensions first: ", id)
  assign(id, plot, envir = nm_registry)
  invisible(plot)
}

reexport <- function(id) {
  if (!exists(id, envir = nm_registry, inherits = FALSE)) {
    stop("Panel not registered yet: ", id,
         ". Run the `build-and-export` chunk first.")
  }
  info <- panel_info(id)
  target <- file.path(out_dir, info$folder)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  p <- get(id, envir = nm_registry, inherits = FALSE)
  w <- info$width_mm / 25.4
  h <- info$height_mm / 25.4
  ggplot2::ggsave(file.path(target, paste0(id, ".png")), p,
                  width = w, height = h, units = "in", dpi = nm_export$dpi,
                  bg = nm_export$background, device = ragg::agg_png, limitsize = FALSE)
  ggplot2::ggsave(file.path(target, paste0(id, ".svg")), p,
                  width = w, height = h, units = "in",
                  bg = nm_export$background, device = svglite::svglite,
                  limitsize = FALSE)
  invisible(file.path(target, id))
}

reexport_family <- function(family) {
  ids <- intersect(names(figure_families)[figure_families == family], ls(nm_registry))
  if (!length(ids)) stop("No panels in registered family: ", family)
  invisible(lapply(ids, reexport))
}

export_all_panels <- function() {
  invisible(lapply(intersect(names(nm_dims), ls(nm_registry)), reexport))
}
