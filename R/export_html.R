#' Export restaurants to an interactive Leaflet HTML map
#'
#' Builds a self-contained interactive map for embedding on a website or
#' viewing on a phone. Pins are colour-coded by hat tier (when present)
#' or by source-overlap count, with marker clustering for performance and
#' rich popups including rating, cuisine, price, source list, and a
#' truncated description. A custom filter panel in the top-right lets
#' the viewer:
#'
#' - Toggle each tier (3 hats / 2 hats / etc.) on or off
#' - Pick one or more guides (Broadsheet, Time Out, AGFG, ...) and
#'   choose whether the venue must appear in **any** selected guide
#'   (union) or **all** of them (intersection)
#'
#' Suitable for hosting on GitHub Pages or any static host.
#'
#' Requires the `leaflet`, `htmlwidgets`, `htmltools` and `jsonlite`
#' packages (Suggests).
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
  rlang::check_installed("jsonlite", reason = "for HTML map output")

  has_coords <- !is.na(restaurants$latitude) & !is.na(restaurants$longitude)
  n_skip <- sum(!has_coords)
  geo <- restaurants[has_coords, , drop = FALSE]

  if (n_skip > 0) {
    cli::cli_warn("Excluding {n_skip} venue{?s} without coordinates from HTML map")
  }
  if (nrow(geo) == 0) {
    cli::cli_abort("No venues with coordinates to export.")
  }

  if (!"rating_label" %in% names(geo) && "source" %in% names(geo)) {
    geo <- harmonize_sources(geo)
  }

  # Tier classification feeds both pin colour and the legend.
  # If n_sources isn't already on the data (e.g. loaded from an older
  # CSV that pre-dates harmonize_sources keeping it), derive it from
  # the source column.
  hats_col <- if ("hats" %in% names(geo)) geo$hats else NA_integer_
  ns_col   <- if ("n_sources" %in% names(geo)) {
    geo$n_sources
  } else if ("source" %in% names(geo)) {
    vapply(strsplit(geo$source, ",\\s*"), length, integer(1))
  } else {
    rep(1L, nrow(geo))
  }
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

  # Source list per venue, normalised to a vector for JS-side comparisons
  sources_split <- if ("source" %in% names(geo)) {
    lapply(strsplit(geo$source, ",\\s*"), function(x) x[!is.na(x) & nzchar(x)])
  } else {
    rep(list(character()), nrow(geo))
  }
  all_sources <- sort(unique(unlist(sources_split)))

  geo$popup_html <- vapply(seq_len(nrow(geo)),
                           build_popup_html,
                           character(1),
                           geo = geo)

  # Marker payload for JS — kept lean (popup HTML is the heaviest field)
  marker_records <- lapply(seq_len(nrow(geo)), function(i) {
    list(
      id        = paste0("v", i),
      lat       = geo$latitude[i],
      lng       = geo$longitude[i],
      name      = geo$name[i],
      popup     = geo$popup_html[i],
      color     = geo$pin_color[i],
      tier      = geo$tier[i],
      n_sources = length(sources_split[[i]]),
      # I() prevents jsonlite::toJSON(auto_unbox = TRUE) from collapsing
      # a length-1 character vector into a bare string (which would
      # break .some()/.every() on the JS side)
      sources   = I(sources_split[[i]])
    )
  })
  marker_json <- jsonlite::toJSON(marker_records, auto_unbox = TRUE,
                                  null = "null", na = "null")

  bbox <- list(
    lng_min = min(geo$longitude, na.rm = TRUE),
    lng_max = max(geo$longitude, na.rm = TRUE),
    lat_min = min(geo$latitude,  na.rm = TRUE),
    lat_max = max(geo$latitude,  na.rm = TRUE)
  )

  tier_order <- c("3 hats", "2 hats", "1 hat",
                  "5+ guides", "3-4 guides", "2 guides", "single guide")
  tier_order <- tier_order[tier_order %in% unique(geo$tier)]

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::fitBounds(
      lng1 = bbox$lng_min, lat1 = bbox$lat_min,
      lng2 = bbox$lng_max, lat2 = bbox$lat_max
    ) |>
    leaflet::addControl(
      html     = filter_panel_html(tier_order, all_sources),
      position = "topright",
      className = "foodmap-filter-control"
    ) |>
    htmlwidgets::onRender(filter_js(marker_json))

  # The custom onRender JS uses L.markerClusterGroup for clustering, but
  # leaflet R only injects that library when addMarkers(clusterOptions=)
  # is called from R. Attach the dep manually so the JS is loaded.
  cluster_src <- system.file(
    "htmlwidgets/plugins/Leaflet.markercluster", package = "leaflet"
  )
  if (nzchar(cluster_src)) {
    m$dependencies <- c(m$dependencies, list(
      htmltools::htmlDependency(
        name       = "leaflet-markercluster",
        version    = "1.0.5",
        src        = cluster_src,
        script     = "leaflet.markercluster.js",
        stylesheet = c("MarkerCluster.css", "MarkerCluster.Default.css")
      )
    ))
  }

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


#' Build the filter-panel HTML — tier checkboxes, source checkboxes,
#' AND/OR mode radio, min-guides select, all/none toggles, plus the
#' colour legend.
#' @noRd
filter_panel_html <- function(tier_order, all_sources) {
  esc <- htmltools::htmlEscape

  tier_swatch <- list(
    "3 hats"       = "\U0001F7E3",
    "2 hats"       = "\U0001F7E2",
    "1 hat"        = "\U0001F338",
    "5+ guides"    = "\U0001F534",
    "3-4 guides"   = "\U0001F7E5",
    "2 guides"     = "\U0001F7E0",
    "single guide" = "\U0001F535"
  )
  tier_rows <- vapply(tier_order, function(t) {
    sprintf(
      paste0("<label style='display:block;cursor:pointer;'>",
             "<input type='checkbox' class='fm-tier' value='%s' checked> ",
             "%s %s</label>"),
      esc(t), tier_swatch[[t]] %||% "", esc(t)
    )
  }, character(1))

  source_rows <- vapply(all_sources, function(s) {
    sprintf(
      paste0("<label style='display:block;cursor:pointer;'>",
             "<input type='checkbox' class='fm-source' value='%s' checked> ",
             "%s</label>"),
      esc(s), esc(s)
    )
  }, character(1))

  # Mini "All / None" toggle for a checkbox group, identified via target
  # CSS class. Tiny inline buttons to keep the panel compact.
  toggle_btns <- function(target_class) {
    btn_style <- paste0(
      "border:1px solid #ccc;background:#f7f7f7;color:#333;",
      "border-radius:4px;padding:1px 6px;font-size:10px;cursor:pointer;",
      "margin-left:4px;font-family:inherit"
    )
    sprintf(
      paste0("<button type='button' class='fm-toggle' data-target='%s' ",
             "data-value='1' style='%s'>all</button>",
             "<button type='button' class='fm-toggle' data-target='%s' ",
             "data-value='0' style='%s'>none</button>"),
      target_class, btn_style, target_class, btn_style
    )
  }

  # Min-guides select: 1 = "any", 2..N = "≥N guides"
  min_options <- vapply(seq_len(7), function(n) {
    sel <- if (n == 1) " selected" else ""
    label <- if (n == 1) "any" else paste0("≥ ", n)
    sprintf("<option value='%d'%s>%s</option>", n, sel, label)
  }, character(1))

  paste0(
    "<div class='foodmap-filter' style='background:rgba(255,255,255,0.96);",
    "padding:10px 12px;border-radius:8px;font-family:sans-serif;font-size:12px;",
    "box-shadow:0 1px 6px rgba(0,0,0,0.18);max-width:240px;line-height:1.55'>",
    # Tier section with all/none toggles
    "<div style='font-weight:600;margin-bottom:4px'>",
    "Tier", toggle_btns("fm-tier"), "</div>",
    paste(tier_rows, collapse = ""),
    # Source section with all/none toggles
    "<div style='font-weight:600;margin-top:10px;margin-bottom:4px'>",
    "Guide", toggle_btns("fm-source"), "</div>",
    paste(source_rows, collapse = ""),
    # Min-guides: must appear in at least N of the *selected* guides.
    # ≥1 acts like the old "any" mode, ≥N (where N = number of guides
    # ticked) acts like the old "all" mode.
    "<div style='font-weight:600;margin-top:10px;margin-bottom:4px'>",
    "Min selected guides matched</div>",
    "<select id='fm-min-sources' style='font-family:inherit;font-size:12px'>",
    paste(min_options, collapse = ""),
    "</select>",
    # Counter
    "<div id='fm-count' style='margin-top:10px;color:#666;font-size:11px'></div>",
    "</div>"
  )
}


#' JS injected post-render to:
#'   - parse the embedded marker JSON
#'   - build a single MarkerClusterGroup
#'   - wire filter checkboxes/radios to a re-render handler
#' @noRd
filter_js <- function(marker_json) {
  paste0("
function(el, x) {
  var map = this;
  var markerData = ", marker_json, ";

  // Build all marker objects up front; we'll add/remove them from the
  // cluster as filters change rather than recreating each time.
  var fmColors = {
    darkpurple: '#5b2c6f',
    purple:     '#8e44ad',
    pink:       '#e91e63',
    darkred:    '#922b21',
    red:        '#e74c3c',
    orange:     '#f39c12',
    blue:       '#3498db'
  };
  function fmIcon(color) {
    var c = fmColors[color] || fmColors.blue;
    var html =
      \"<div style='background:\" + c + \";width:26px;height:26px;\" +
      \"border-radius:50% 50% 50% 0;transform:rotate(-45deg);\" +
      \"border:2px solid white;box-shadow:0 1px 4px rgba(0,0,0,0.3);\" +
      \"display:flex;align-items:center;justify-content:center'>\" +
      \"<span style='transform:rotate(45deg);color:white;font-size:14px;\" +
      \"line-height:1'>\\u{1F374}</span></div>\";
    return L.divIcon({
      className: 'fm-pin',
      html: html,
      iconSize:   [26, 26],
      iconAnchor: [13, 26],
      popupAnchor: [0, -26]
    });
  }

  var allMarkers = markerData.map(function(d) {
    var marker = L.marker([d.lat, d.lng], { icon: fmIcon(d.color), title: d.name });
    marker.bindPopup(d.popup);
    marker._fmTier = d.tier;
    // Defensive: serializers sometimes collapse 1-element arrays to a
    // bare string. Promote to array so .some()/.every() work.
    var src = d.sources;
    if (typeof src === 'string') src = [src];
    if (!src) src = [];
    marker._fmSources = src;
    marker._fmNumSources = (typeof d.n_sources === 'number') ? d.n_sources : src.length;
    return marker;
  });

  var cluster = L.markerClusterGroup({ maxClusterRadius: 40 });
  map.addLayer(cluster);

  function getChecked(selector) {
    return Array.prototype.map.call(
      document.querySelectorAll(selector + ':checked'),
      function(el) { return el.value; }
    );
  }
  function getMinSources() {
    var sel = document.getElementById('fm-min-sources');
    return sel ? parseInt(sel.value, 10) || 1 : 1;
  }

  function applyFilter() {
    var tiers   = getChecked('.fm-tier');
    var sources = getChecked('.fm-source');
    var minN    = getMinSources();
    var tierSet = {};
    tiers.forEach(function(t) { tierSet[t] = true; });
    var sourceSet = {};
    sources.forEach(function(s) { sourceSet[s] = true; });

    // Min N is interpreted relative to the selected guide set: a venue
    // must appear in at least minN of the currently-ticked guides. With
    // minN=1 this is union (any selected guide); with minN=count(sources)
    // it's intersection (all selected guides must match).
    var visible = allMarkers.filter(function(m) {
      if (!tierSet[m._fmTier]) return false;
      if (sources.length === 0) return false;
      var hits = 0;
      for (var i = 0; i < m._fmSources.length; i++) {
        if (sourceSet[m._fmSources[i]]) {
          hits++;
          if (hits >= minN) return true;
        }
      }
      return false;
    });

    cluster.clearLayers();
    cluster.addLayers(visible);

    var counter = document.getElementById('fm-count');
    if (counter) {
      counter.textContent = visible.length + ' / ' + allMarkers.length + ' venues shown';
    }
  }

  // Wire up listeners — every checkbox/select re-applies the filter
  document.querySelectorAll('.fm-tier, .fm-source').forEach(function(input) {
    input.addEventListener('change', applyFilter);
  });
  var minSel = document.getElementById('fm-min-sources');
  if (minSel) minSel.addEventListener('change', applyFilter);

  // All/none toggle buttons set every checkbox in the named group at once
  document.querySelectorAll('.fm-toggle').forEach(function(btn) {
    btn.addEventListener('click', function(e) {
      e.preventDefault();
      var cls = btn.getAttribute('data-target');
      var on  = btn.getAttribute('data-value') === '1';
      document.querySelectorAll('.' + cls).forEach(function(cb) { cb.checked = on; });
      applyFilter();
    });
  });

  // Stop the filter panel from forwarding map drags / scrolls so the
  // checkboxes feel native rather than panning the map underneath
  var panel = document.querySelector('.foodmap-filter');
  if (panel) {
    L.DomEvent.disableClickPropagation(panel);
    L.DomEvent.disableScrollPropagation(panel);
  }

  applyFilter();
}
")
}
