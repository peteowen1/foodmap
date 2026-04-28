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

  # Pin colour + tier label, both derived from the same case_when so the
  # legend and the layer-control checkboxes stay in sync. Hats win when
  # present; otherwise classify by source-overlap count.
  hats_col <- if ("hats" %in% names(geo)) geo$hats else NA_integer_
  ns_col   <- if ("n_sources" %in% names(geo)) geo$n_sources else rep(1L, nrow(geo))
  geo$tier <- dplyr::case_when(
    !is.na(hats_col) & hats_col >= 3 ~ "3 hats",
    !is.na(hats_col) & hats_col == 2 ~ "2 hats",
    !is.na(hats_col) & hats_col == 1 ~ "1 hat",
    !is.na(ns_col)   & ns_col   >= 5 ~ "5+ guides",
    !is.na(ns_col)   & ns_col   >= 3 ~ "3-4 guides",
    !is.na(ns_col)   & ns_col   == 2 ~ "2 guides",
    TRUE                             ~ "single guide"
  )
  geo$pin_color <- dplyr::case_when(
    geo$tier == "3 hats"        ~ "darkpurple",
    geo$tier == "2 hats"        ~ "purple",
    geo$tier == "1 hat"         ~ "pink",
    geo$tier == "5+ guides"     ~ "darkred",
    geo$tier == "3-4 guides"    ~ "red",
    geo$tier == "2 guides"      ~ "orange",
    geo$tier == "single guide"  ~ "blue"
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

  # Order tiers from "best" to "long tail" so the layer control reads
  # naturally. Only include tiers that actually appear in the data.
  tier_order <- c("3 hats", "2 hats", "1 hat",
                  "5+ guides", "3-4 guides", "2 guides", "single guide")
  tier_order <- tier_order[tier_order %in% unique(geo$tier)]

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::fitBounds(
      lng1 = bbox$lng_min, lat1 = bbox$lat_min,
      lng2 = bbox$lng_max, lat2 = bbox$lat_max
    )

  # Add one cluster per tier so the layers control can independently
  # toggle each — flipping off "single guide" instantly thins out the
  # central Sydney cluster from 395 to ~100.
  for (tier_name in tier_order) {
    subset_geo <- geo[geo$tier == tier_name, , drop = FALSE]
    m <- m |> leaflet::addAwesomeMarkers(
      data        = subset_geo,
      lng         = ~longitude,
      lat         = ~latitude,
      popup       = ~popup_html,
      label       = ~name,
      group       = tier_name,
      icon        = leaflet::awesomeIcons(
        icon         = "cutlery",
        library      = "fa",
        markerColor  = ~pin_color,
        iconColor    = "white"
      ),
      clusterOptions = leaflet::markerClusterOptions(maxClusterRadius = 40)
    )
  }

  m <- m |>
    leaflet::addLayersControl(
      overlayGroups = tier_order,
      options       = leaflet::layersControlOptions(collapsed = FALSE),
      position      = "topright"
    ) |>
    leaflet::addControl(
      html     = export_html_legend(tier_order),
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
#'
#' Only renders rows for tiers actually present in the data so a Melbourne
#' map (no SMH GFG awards source, hence rare 3 hat tier) doesn't show
#' phantom legend entries.
#' @noRd
export_html_legend <- function(tier_order) {
  swatch <- list(
    "3 hats"       = "\U0001F7E3 3 hats",
    "2 hats"       = "\U0001F7E2 2 hats",
    "1 hat"        = "\U0001F338 1 hat",
    "5+ guides"    = "\U0001F534 5+ guides",
    "3-4 guides"   = "\U0001F7E5 3-4 guides",
    "2 guides"     = "\U0001F7E0 2 guides",
    "single guide" = "\U0001F535 single guide"
  )
  rows <- vapply(tier_order, function(t) swatch[[t]] %||% t, character(1))
  paste0(
    "<div style='background:rgba(255,255,255,0.92);padding:8px 10px;",
    "border-radius:6px;font-family:sans-serif;font-size:12px;",
    "box-shadow:0 1px 4px rgba(0,0,0,0.15);line-height:1.5;",
    "max-width:170px'>",
    "<b>Pin colour</b><br>",
    paste(rows, collapse = "<br>"),
    "</div>"
  )
}
