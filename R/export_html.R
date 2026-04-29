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

  # Cuisine list per venue - one venue can list several (e.g. "Italian, Pizza")
  cuisines_split <- if ("cuisine" %in% names(geo)) {
    lapply(strsplit(geo$cuisine, ",\\s*"), function(x) {
      x <- x[!is.na(x) & nzchar(x)]
      tolower(x)
    })
  } else {
    rep(list(character()), nrow(geo))
  }
  all_cuisines <- sort(unique(unlist(cuisines_split)))

  geo$popup_html <- vapply(seq_len(nrow(geo)),
                           build_popup_html,
                           character(1),
                           geo = geo)

  # Price bin per venue: "$"/"$$"/"$$$"/"$$$$" or "?" for unknown.
  # Used by the JS-side filter; matches the values in the price filter
  # checkboxes built by `filter_panel_html()`.
  price_bin <- if ("price_range" %in% names(geo)) {
    pr <- geo$price_range
    ifelse(is.na(pr) | pr < 1L, "?", strrep("$", pmin(pmax(pr, 1L), 4L)))
  } else {
    rep("?", nrow(geo))
  }

  # Marker payload for JS - kept lean (popup HTML is the heaviest field)
  marker_records <- lapply(seq_len(nrow(geo)), function(i) {
    list(
      id        = paste0("v", i),
      lat       = geo$latitude[i],
      lng       = geo$longitude[i],
      name      = geo$name[i],
      popup     = geo$popup_html[i],
      color     = geo$pin_color[i],
      tier      = geo$tier[i],
      price     = price_bin[i],
      n_sources = length(sources_split[[i]]),
      # I() prevents jsonlite::toJSON(auto_unbox = TRUE) from collapsing
      # a length-1 character vector into a bare string (which would
      # break .some()/.every() on the JS side)
      sources   = I(sources_split[[i]]),
      cuisines  = I(cuisines_split[[i]])
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
      html     = filter_panel_html(
        tier_order, all_sources, all_cuisines,
        price_bins = sort(unique(price_bin))
      ),
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
    parts <- c(parts, sprintf("\u2b50 %s", esc(rating)))
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


#' Build the filter-panel HTML - collapsible header with venue counter,
#' name search, tier/guide/cuisine checkbox sections (each with all/none
#' toggles), and a min-selected-guides-matched dropdown.
#' @noRd
filter_panel_html <- function(tier_order, all_sources, all_cuisines = character(),
                              price_bins = character()) {
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

  cuisine_rows <- if (length(all_cuisines) > 0) {
    vapply(all_cuisines, function(c) {
      sprintf(
        paste0("<label style='display:block;cursor:pointer;'>",
               "<input type='checkbox' class='fm-cuisine' value='%s' checked> ",
               "%s</label>"),
        esc(c), esc(c)
      )
    }, character(1))
  } else character(0)

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

  # Min-guides select: 1 = "any", 2..N = "\u2265N guides"
  min_options <- vapply(seq_len(7), function(n) {
    sel <- if (n == 1) " selected" else ""
    label <- if (n == 1) "any" else paste0("\u2265 ", n)
    sprintf("<option value='%d'%s>%s</option>", n, sel, label)
  }, character(1))

  # Section helper: collapsible header (chevron + label + all/none) + body.
  # `body_html` is whatever should hide when the section is collapsed.
  section_html <- function(label, toggle_target, body_html) {
    paste0(
      "<div class='fm-section' style='margin-top:10px'>",
      "<div class='fm-section-header' style='font-weight:600;",
      "margin-bottom:4px;cursor:pointer;user-select:none;",
      "display:flex;align-items:center;gap:6px'>",
      "<span class='fm-section-toggle' style='display:inline-block;",
      "width:12px;text-align:center;font-size:10px'>\u25bc</span>",
      "<span style='flex:1'>", label, "</span>",
      toggle_btns(toggle_target),
      "</div>",
      "<div class='fm-section-body'>",
      body_html,
      "</div></div>"
    )
  }

  # Price section. Sort bins so cheap-to-expensive: $ < $$ < $$$ < $$$$,
  # with the unknown-price bucket "?" pinned to the end.
  price_order <- c("$", "$$", "$$$", "$$$$", "?")
  price_bins_sorted <- price_order[price_order %in% price_bins]
  price_label_for <- list(
    "$"    = "$ — budget",
    "$$"   = "$$ — moderate",
    "$$$"  = "$$$ — special occasion",
    "$$$$" = "$$$$ — premium",
    "?"    = "? — unknown"
  )
  price_rows <- vapply(price_bins_sorted, function(b) {
    sprintf(
      paste0("<label style='display:block;cursor:pointer;'>",
             "<input type='checkbox' class='fm-price' value='%s' checked> ",
             "%s</label>"),
      esc(b), esc(price_label_for[[b]] %||% b)
    )
  }, character(1))

  price_block <- if (length(price_rows) > 0) {
    section_html("Price", "fm-price", paste(price_rows, collapse = ""))
  } else ""

  cuisine_block <- if (length(cuisine_rows) > 0) {
    section_html(
      "Cuisine", "fm-cuisine",
      paste0(
        # Mini search to narrow the cuisine list (handy when there are
        # 30+ cuisines - type "french" to surface just that checkbox)
        "<input type='text' id='fm-cuisine-search' placeholder='filter cuisines...' ",
        "style='width:100%;box-sizing:border-box;font-family:inherit;",
        "font-size:11px;padding:3px 5px;margin-bottom:4px;",
        "border:1px solid #ccc;border-radius:4px'>",
        "<div style='max-height:120px;overflow-y:auto;border:1px solid #eee;",
        "border-radius:4px;padding:4px 6px;background:#fafafa'>",
        paste(cuisine_rows, collapse = ""),
        "</div>"
      )
    )
  } else ""

  paste0(
    "<div class='foodmap-filter' style='background:rgba(255,255,255,0.96);",
    "padding:0;border-radius:8px;font-family:sans-serif;font-size:12px;",
    "box-shadow:0 1px 6px rgba(0,0,0,0.18);max-width:260px;line-height:1.55'>",
    # Header - always visible, holds toggle + counter
    "<div class='fm-header' style='padding:8px 12px;display:flex;",
    "align-items:center;justify-content:space-between;gap:8px;",
    "cursor:pointer;user-select:none'>",
    "<span style='font-weight:700'>",
    "<span class='fm-toggle-icon' style='display:inline-block;width:14px'>",
    "&#9660;</span> foodmap</span>",
    "<span id='fm-count' style='color:#666;font-size:11px'></span>",
    "</div>",
    # Body - collapsible
    "<div class='fm-body' style='padding:0 12px 10px 12px;",
    "border-top:1px solid #eee'>",
    # Search input
    "<div style='font-weight:600;margin-top:8px;margin-bottom:4px'>Search</div>",
    "<input type='text' id='fm-search' placeholder='Venue name...' ",
    "style='width:100%;box-sizing:border-box;font-family:inherit;",
    "font-size:12px;padding:4px 6px;border:1px solid #ccc;border-radius:4px'>",
    # Tier / Guide sections - both wrapped in collapsible .fm-section
    section_html("Tier",  "fm-tier",   paste(tier_rows, collapse = "")),
    section_html("Guide", "fm-source", paste(source_rows, collapse = "")),
    # Min-guides: must appear in at least N of the *selected* guides
    "<div style='font-weight:600;margin-top:10px;margin-bottom:4px'>",
    "Min selected guides matched</div>",
    "<select id='fm-min-sources' style='font-family:inherit;font-size:12px'>",
    paste(min_options, collapse = ""),
    "</select>",
    # Price section (only if any prices)
    price_block,
    # Cuisine section (only if any cuisines)
    cuisine_block,
    "</div>",  # /.fm-body
    "</div>"   # /.foodmap-filter
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

  function asArray(v) {
    if (typeof v === 'string') return [v];
    if (!v) return [];
    return v;
  }

  var allMarkers = markerData.map(function(d) {
    var marker = L.marker([d.lat, d.lng], { icon: fmIcon(d.color), title: d.name });
    marker.bindPopup(d.popup);
    marker._fmTier        = d.tier;
    marker._fmName        = (d.name || '').toLowerCase();
    // Defensive: serializers sometimes collapse 1-element arrays to a
    // bare string. Promote to array so .some()/.every() work.
    marker._fmSources     = asArray(d.sources);
    marker._fmNumSources  = (typeof d.n_sources === 'number') ? d.n_sources : marker._fmSources.length;
    marker._fmCuisines    = asArray(d.cuisines);
    marker._fmPrice       = d.price || '?';
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

  // Cache the total cuisine count so we know when 'all are checked'
  var totalCuisines = document.querySelectorAll('.fm-cuisine').length;

  function applyFilter() {
    var tiers    = getChecked('.fm-tier');
    var sources  = getChecked('.fm-source');
    var prices   = getChecked('.fm-price');
    var cuisines = getChecked('.fm-cuisine');
    var minN     = getMinSources();
    var searchEl = document.getElementById('fm-search');
    var query    = searchEl ? searchEl.value.trim().toLowerCase() : '';

    var tierSet = {};
    tiers.forEach(function(t) { tierSet[t] = true; });
    var sourceSet = {};
    sources.forEach(function(s) { sourceSet[s] = true; });
    var priceSet = {};
    prices.forEach(function(p) { priceSet[p] = true; });
    var cuisineSet = {};
    cuisines.forEach(function(c) { cuisineSet[c] = true; });
    var cuisineActive = totalCuisines > 0 && cuisines.length < totalCuisines;

    var visible = allMarkers.filter(function(m) {
      if (!tierSet[m._fmTier]) return false;
      if (query && m._fmName.indexOf(query) === -1) return false;

      if (sources.length === 0) return false;
      var hits = 0;
      for (var i = 0; i < m._fmSources.length; i++) {
        if (sourceSet[m._fmSources[i]]) hits++;
      }
      if (hits < minN) return false;

      if (!priceSet[m._fmPrice]) return false;

      if (cuisineActive) {
        if (m._fmCuisines.length === 0) return cuisines.length > 0;
        var cuisineHit = false;
        for (var j = 0; j < m._fmCuisines.length; j++) {
          if (cuisineSet[m._fmCuisines[j]]) { cuisineHit = true; break; }
        }
        if (!cuisineHit) return false;
      }
      return true;
    });

    cluster.clearLayers();
    cluster.addLayers(visible);

    var counter = document.getElementById('fm-count');
    if (counter) {
      counter.textContent = visible.length + ' / ' + allMarkers.length;
    }
  }

  // Wire up listeners - every checkbox/select re-applies the filter
  document.querySelectorAll('.fm-tier, .fm-source, .fm-price, .fm-cuisine').forEach(function(input) {
    input.addEventListener('change', applyFilter);
  });
  var minSel = document.getElementById('fm-min-sources');
  if (minSel) minSel.addEventListener('change', applyFilter);
  var searchInput = document.getElementById('fm-search');
  if (searchInput) searchInput.addEventListener('input', applyFilter);

  // All/none toggle buttons. When applied to the cuisine group these
  // respect the visibility filter: clicking the all button after
  // typing french ticks only the visible French entries, leaving any
  // hidden cuisines as they were.
  document.querySelectorAll('.fm-toggle').forEach(function(btn) {
    btn.addEventListener('click', function(e) {
      e.preventDefault();
      e.stopPropagation();
      var cls = btn.getAttribute('data-target');
      var on  = btn.getAttribute('data-value') === '1';
      document.querySelectorAll('.' + cls).forEach(function(cb) {
        var label = cb.parentElement;
        if (label && label.style.display === 'none') return;
        cb.checked = on;
      });
      applyFilter();
    });
  });

  // Cuisine search - filters which cuisine checkboxes are visible
  // (does not change checked state). Combine with the All/None buttons
  // to chain: untick all, search french, tick all visible.
  var cuisineSearch = document.getElementById('fm-cuisine-search');
  if (cuisineSearch) {
    cuisineSearch.addEventListener('input', function() {
      var q = cuisineSearch.value.trim().toLowerCase();
      document.querySelectorAll('.fm-cuisine').forEach(function(cb) {
        var label = cb.parentElement;
        if (!label) return;
        var match = !q || cb.value.toLowerCase().indexOf(q) !== -1;
        label.style.display = match ? '' : 'none';
      });
    });
  }

  // Main panel header - toggles the entire body
  var header = document.querySelector('.foodmap-filter .fm-header');
  var body   = document.querySelector('.foodmap-filter .fm-body');
  var icon   = document.querySelector('.foodmap-filter .fm-toggle-icon');
  if (header && body) {
    header.addEventListener('click', function() {
      var hidden = body.style.display === 'none';
      body.style.display = hidden ? '' : 'none';
      if (icon) icon.textContent = hidden ? '\u25bc' : '\u25b6';
    });
  }

  // Per-section headers - each toggles its own .fm-section-body. Ignore
  // clicks that originated on the All/None buttons (they have their own
  // handlers and call stopPropagation, but a defensive tag check keeps
  // the rule explicit).
  document.querySelectorAll('.foodmap-filter .fm-section-header').forEach(function(h) {
    h.addEventListener('click', function(e) {
      if (e.target.tagName === 'BUTTON') return;
      var section = h.parentElement;
      var sbody = section.querySelector('.fm-section-body');
      var sicon = h.querySelector('.fm-section-toggle');
      if (!sbody) return;
      var hidden = sbody.style.display === 'none';
      sbody.style.display = hidden ? '' : 'none';
      if (sicon) sicon.textContent = hidden ? '\u25bc' : '\u25b6';
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
