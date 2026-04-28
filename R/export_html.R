#' Export restaurants to an interactive Leaflet HTML map
#'
#' Builds a self-contained interactive map for embedding on a website or
#' viewing on a phone. Pins are colour-coded by hat tier (when present)
#' or by source-overlap count, with marker clustering for performance and
#' rich popups including rating, cuisine, price, source list, and a
#' truncated description. Suitable for hosting on GitHub Pages or any
#' static host.
#'
#' Requires the `leaflet`, `htmlwidgets` and `htmltools` packages
#' (Suggests).
#'
#' @param restaurants A tibble as returned by [geocode_restaurants()],
#'   typically after [harmonize_sources()] (which adds `rating_label`
#'   and `price_label`). If those columns are missing they will be
#'   computed automatically when a `source` column is present.
#' @param output_path Character. Output HTML path. Default `"map.html"`.
#' @param title Character. Browser tab title for the map. Default
#'   `"Restaurant Map"`.
#'
#' @return The output path (invisibly).
#' @export
export_html <- function(restaurants,
                        output_path = "map.html",
                        title = "Restaurant Map") {
  rlang::check_installed("leaflet", reason = "for HTML map output")
  rlang::check_installed("htmlwidgets", reason = "for HTML map output")
  rlang::check_installed("htmltools", reason = "for HTML map output")

  has_coords <- !is.na(restaurants$latitude) & !is.na(restaurants$longitude)
  n_skip <- sum(!has_coords)
  geo <- restaurants[has_coords, , drop = FALSE]

  if (n_skip > 0) {
    cli::cli_warn("Excluding {n_skip} venue{?s} without coordinates from HTML map")
  }
  if (nrow(geo) == 0) {
    cli::cli_abort("No venues with coordinates to export.")
  }

  # Add rating/price/cost labels if missing (only works when source column present)
  if (!"rating_label" %in% names(geo) && "source" %in% names(geo)) {
    geo <- harmonize_sources(geo)
  }

  # Pin colour: hats win when present (hat-tier signal), then source count,
  # finally a neutral default
  hats_col <- if ("hats" %in% names(geo)) geo$hats else NA_integer_
  ns_col   <- if ("n_sources" %in% names(geo)) geo$n_sources else 1L
  geo$pin_color <- dplyr::case_when(
    !is.na(hats_col) & hats_col >= 3                 ~ "darkpurple",
    !is.na(hats_col) & hats_col == 2                 ~ "purple",
    !is.na(hats_col) & hats_col == 1                 ~ "pink",
    !is.na(ns_col)   & ns_col   >= 5                 ~ "darkred",
    !is.na(ns_col)   & ns_col   >= 3                 ~ "red",
    !is.na(ns_col)   & ns_col   == 2                 ~ "orange",
    TRUE                                             ~ "blue"
  )

  geo$popup_html <- vapply(seq_len(nrow(geo)),
                           build_popup_html,
                           character(1),
                           geo = geo)

  bbox <- list(
    lng_min = min(geo$longitude, na.rm = TRUE),
    lng_max = max(geo$longitude, na.rm = TRUE),
    lat_min = min(geo$latitude,  na.rm = TRUE),
    lat_max = max(geo$latitude,  na.rm = TRUE)
  )

  m <- leaflet::leaflet(geo) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addAwesomeMarkers(
      lng         = ~longitude,
      lat         = ~latitude,
      popup       = ~popup_html,
      label       = ~name,
      icon        = leaflet::awesomeIcons(
        icon         = "cutlery",
        library      = "fa",
        markerColor  = ~pin_color,
        iconColor    = "white"
      ),
      clusterOptions = leaflet::markerClusterOptions(maxClusterRadius = 40)
    ) |>
    leaflet::fitBounds(
      lng1 = bbox$lng_min, lat1 = bbox$lat_min,
      lng2 = bbox$lng_max, lat2 = bbox$lat_max
    ) |>
    leaflet::addControl(
      html     = export_html_legend(),
      position = "bottomright"
    )

  output_path <- normalizePath(output_path, mustWork = FALSE, winslash = "/")
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  htmlwidgets::saveWidget(m, output_path, selfcontained = TRUE, title = title)
  cli::cli_alert_success("HTML map written to {.file {output_path}} ({nrow(geo)} venues)")
  invisible(output_path)
}


#' Build the HTML popup string for one row
#' @noRd
build_popup_html <- function(i, geo) {
  esc <- htmltools::htmlEscape

  parts <- character(0)
  parts <- c(parts, sprintf("<b>%s</b>", esc(geo$name[i])))

  if (!is.na(geo$suburb[i])) {
    parts <- c(parts, sprintf("<i>%s</i>", esc(geo$suburb[i])))
  }

  rating <- if ("rating_label" %in% names(geo)) geo$rating_label[i] else NA
  if (!is.na(rating) && nchar(rating) > 0) {
    parts <- c(parts, sprintf("⭐ %s", esc(rating)))
  }

  if (!is.na(geo$cuisine[i]) && nchar(geo$cuisine[i]) > 0) {
    parts <- c(parts, sprintf("\U0001F374 %s", esc(geo$cuisine[i])))
  }

  price <- if ("price_label" %in% names(geo)) geo$price_label[i] else NA
  if (!is.na(price) && nchar(price) > 0) {
    parts <- c(parts, sprintf("\U0001F4B0 %s", esc(price)))
  }

  if ("source" %in% names(geo) && !is.na(geo$source[i])) {
    parts <- c(parts, sprintf("<small>%s</small>", esc(geo$source[i])))
  }

  if (!is.na(geo$description[i]) && nchar(geo$description[i]) > 0) {
    blurb <- stringr::str_trunc(geo$description[i], 280)
    parts <- c(parts, sprintf("<p style='margin-top:6px;font-size:90%%;'>%s</p>",
                              esc(blurb)))
  }

  if (!is.na(geo$url[i]) && nchar(geo$url[i]) > 0) {
    parts <- c(parts, sprintf(
      "<a href='%s' target='_blank' rel='noopener'>Source link</a>",
      esc(geo$url[i])
    ))
  }

  paste(parts, collapse = "<br>")
}


#' Inline legend HTML, shown bottom-right of the map
#' @noRd
export_html_legend <- function() {
  paste0(
    "<div style='background:rgba(255,255,255,0.92);padding:8px 10px;",
    "border-radius:6px;font-family:sans-serif;font-size:12px;",
    "box-shadow:0 1px 4px rgba(0,0,0,0.15);line-height:1.5'>",
    "<b>Pin colour</b><br>",
    "\U0001F7E3 3 hats &nbsp; ",
    "\U0001F7E2 2 hats &nbsp; ",
    "\U0001F338 1 hat<br>",
    "\U0001F534 5+ guides &nbsp; ",
    "\U0001F7E0 2 guides<br>",
    "\U0001F535 single guide",
    "</div>"
  )
}
