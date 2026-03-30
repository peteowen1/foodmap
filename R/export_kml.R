#' Add a Placemark node to a KML parent element
#' @noRd
add_placemark <- function(parent, row) {
  pm <- xml2::xml_add_child(parent, "Placemark")
  # xml2::xml_add_child auto-escapes text nodes; xml_escape() is only
  # needed below where we build raw HTML strings via glue for descriptions
  xml2::xml_add_child(pm, "name", row$name)

  desc_parts <- character(0)
  if (!is.na(row$address)) {
    desc_parts <- c(desc_parts, glue::glue("<b>Address:</b> {xml_escape(row$address)}"))
  }
  if (!is.na(row$cuisine) && nchar(row$cuisine) > 0) {
    desc_parts <- c(desc_parts, glue::glue("<b>Cuisine:</b> {xml_escape(row$cuisine)}"))
  }
  if (!is.na(row$category)) {
    desc_parts <- c(desc_parts, glue::glue("<b>Category:</b> {xml_escape(row$category)}"))
  }
  if (!is.na(row$price_range) && row$price_range > 0) {
    desc_parts <- c(desc_parts, glue::glue("<b>Price:</b> {format_price(row$price_range)}"))
  }
  if ("rating" %in% names(row) && !is.na(row$rating)) {
    scale_label <- row$rating_scale %||% ""
    desc_parts <- c(desc_parts, glue::glue("<b>Rating:</b> {row$rating} ({xml_escape(scale_label)})"))
  }
  if (!is.na(row$description) && nchar(row$description) > 0) {
    blurb <- stringr::str_trunc(row$description, 300)
    desc_parts <- c(desc_parts, glue::glue("<br>{xml_escape(blurb)}"))
  }
  if ("source" %in% names(row) && !is.na(row$source)) {
    desc_parts <- c(desc_parts, glue::glue("<br><i>Source: {xml_escape(row$source)}</i>"))
  }

  desc_html <- paste(desc_parts, collapse = "<br>")
  xml2::xml_add_child(pm, "description", desc_html)

  point <- xml2::xml_add_child(pm, "Point")
  coords <- glue::glue("{row$longitude},{row$latitude},0")
  xml2::xml_add_child(point, "coordinates", coords)

  invisible(pm)
}

#' Export restaurants to KML for Google My Maps
#'
#' Generates a KML file with one Placemark per restaurant. Each pin includes
#' a popup description with address, cuisine, category, and blurb.
#'
#' @param restaurants A tibble as returned by [geocode_restaurants()].
#' @param output_path Character. File path for the KML output.
#'   Defaults to `"{city}_hotlist.kml"` in the current directory.
#'
#' @return The output path (invisibly).
#' @export
export_kml <- function(restaurants, output_path = "hotlist.kml") {

  has_coords <- !is.na(restaurants$latitude) & !is.na(restaurants$longitude)
  n_skip <- sum(!has_coords)
  geo <- restaurants[has_coords, ]

  if (n_skip > 0) {
    cli::cli_warn(
      "Excluding {n_skip} venue{?s} without coordinates from KML"
    )
  }

  if (nrow(geo) == 0) {
    cli::cli_abort("No venues with coordinates to export.")
  }

  # Build KML document
  doc <- xml2::xml_new_root(
    "kml",
    xmlns = "http://www.opengis.net/kml/2.2"
  )

  document_node <- xml2::xml_add_child(doc, "Document")
  xml2::xml_add_child(document_node, "name", "Restaurant Map")
  xml2::xml_add_child(
    document_node, "description",
    glue::glue("Restaurant Map - {nrow(geo)} venues")
  )

  if ("source" %in% names(geo)) {
    # Group into folders by source
    for (src in unique(geo$source)) {
      folder <- xml2::xml_add_child(document_node, "Folder")
      xml2::xml_add_child(folder, "name", src)
      src_rows <- geo[geo$source == src, ]
      for (i in seq_len(nrow(src_rows))) {
        add_placemark(folder, src_rows[i, ])
      }
    }
  } else {
    # Flat list (single source)
    for (i in seq_len(nrow(geo))) {
      add_placemark(document_node, geo[i, ])
    }
  }

  xml2::write_xml(doc, output_path)
  cli::cli_alert_success("KML written to {.file {output_path}} ({nrow(geo)} venues)")
  invisible(output_path)
}
