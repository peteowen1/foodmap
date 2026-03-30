# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

An R package that scrapes restaurant guides (Broadsheet, Gourmet Traveller, Time Out, AGFG, Urban List, Good Food Guide), geocodes venues via Google Places API, and exports KML files for Google My Maps import.

## Development commands

```r
devtools::load_all()              # Load package for interactive use (Ctrl+Shift+L in RStudio)
devtools::document()              # Regenerate NAMESPACE and docs from roxygen2 comments
devtools::check()                 # Full R CMD check
devtools::test()                  # Run testthat test suite (169 tests)
```

CI: GitHub Actions runs `R CMD check` on push/PR to master.

## Architecture

Pipeline with four stages, orchestrated by `create_food_map()`:

```
scrape_restaurants(city, source)   # or source-specific: scrape_broadsheet(), scrape_timeout(), etc.
  → tibble: name, suburb, address, cuisine, category, description, price_range, rating, rating_scale, latitude, longitude, url
geocode_restaurants(restaurants, api_key)
  → same tibble + formatted_address, place_id (fills NA latitude/longitude via Google Places API)
export_kml(restaurants, path)   → .kml for Google My Maps (folders + colored pins by source)
export_csv(restaurants, path)   → .csv backup
```

Multi-source workflow:
```
scrape_all_sources(city)           → combined tibble with source column
deduplicate_restaurants(combined)  → merged tibble with n_sources column
geocode_restaurants(deduped)       → fill missing coordinates
export_kml(deduped, path)          → .kml with folders per source + "multiple" folder
```

### Sources

| Source | Function | Cities | Approach |
|--------|----------|--------|----------|
| Broadsheet | `scrape_broadsheet()` | syd, mel, bri, adl, per, hob | API → RSC → chromote cascade |
| Gourmet Traveller | `scrape_gourmet_traveller()` | syd, mel | Static HTML (WordPress listicle) |
| Time Out | `scrape_timeout()` | syd, mel | Static HTML (data-testid selectors) |
| AGFG | `scrape_agfg()` | syd, mel, bri, adl, per, hob, canberra, darwin, gold-coast | Static HTML + AJAX pagination + JSON-LD detail pages |
| Urban List | `scrape_urban_list()` | syd, mel | Static HTML (H2/H3+H4 pattern) |
| Good Food Guide | `scrape_good_food_guide()` | syd, mel | Static HTML (JSON-LD keywords, partial — paywall limits) |

All scrapers are dispatched via `scrape_restaurants(city, source)`.

### Broadsheet scraping strategy (3-strategy cascade)

1. **Direct API**: POST to `frontend-next.broadsheet.com.au/api/hotlist/{city}` with pagination.
2. **Static RSC extraction**: `self.__next_f.push()` payloads → bracket-counted JSON extraction.
3. **Chromote headless browser**: scrolls 20 times, then RSC or DOM extraction.

Note: Broadsheet does **not** use `<script id="__NEXT_DATA__">`.

### Geocoding

Uses Google Places API (New) Text Search endpoint. Query format: `"{name} {suburb} Australia"`. Rate-limited via `RATE_LIMIT_SECS` constant (0.2s). Idempotent — skips rows that already have coordinates. AGFG scraper can fetch coords from JSON-LD detail pages, reducing API usage.

### HTTP caching

`use_cache = TRUE` on scrapers stores responses in `cache/` directory (24h expiry). Implemented via `cached_fetch()` in `R/cache.R`. Good Food Guide excluded (uses chromote, not plain HTTP).

### Deduplication

`deduplicate_restaurants()` matches venues by normalized name + suburb (case-insensitive, punctuation-stripped, NA suburb treated as wildcard). Keeps the row with most non-NA fields, fills gaps from other copies, combines `source` values, picks longest description.

### KML output

`export_kml()` supports two modes:
- **Single source**: flat list of placemarks
- **Multi-source** (when `source` column present): `<Folder>` per source with color-coded pins. Multi-source merged venues get a gold "multiple" pin.

## Key files

- `R/scrape_restaurants.R` — unified dispatcher + `scrape_all_sources()`
- `R/scrape_broadsheet.R` — Broadsheet: API, RSC extraction, chromote fallback
- `R/scrape_gourmet_traveller.R` — Gourmet Traveller: WordPress listicle/block parsing
- `R/scrape_timeout.R` — Time Out: data-testid based card extraction
- `R/scrape_agfg.R` — AGFG: listing cards + AJAX pagination + JSON-LD detail pages
- `R/scrape_urban_list.R` — Urban List: heading-level heuristic (H2/H3 + H4)
- `R/scrape_good_food_guide.R` — Good Food Guide: section listing + JSON-LD keywords
- `R/geocode_restaurants.R` — Google Places Text Search, `places_text_search()`
- `R/deduplicate.R` — cross-source fuzzy deduplication
- `R/export_kml.R` — KML generation with folders, pin styles, HTML popups
- `R/export_csv.R` — CSV export with error handling
- `R/cache.R` — file-based HTTP response cache (`cached_fetch()`)
- `R/create_food_map.R` — pipeline orchestrator
- `R/utils.R` — helpers: URL builders, validation, `RATE_LIMIT_SECS`, `CUISINE_NAMES`
- `inst/examples/usage.R` — example script showing multi-source usage

## Typical usage

```r
devtools::load_all()
Sys.setenv(GOOGLE_PLACES_API_KEY = "your-key")

# Single source
create_food_map("sydney", output_dir = "output")
create_food_map("sydney", source = "timeout", output_dir = "output")

# All sources with caching + dedup
results <- scrape_all_sources("sydney", use_cache = TRUE)
deduped <- deduplicate_restaurants(results)
deduped <- geocode_restaurants(deduped)
export_kml(deduped, "output/sydney_all_sources.kml")
```

## Output files

`create_food_map()` writes `{city}_{source}.kml` and `{city}_{source}.csv` to the output directory.

## Non-package directories

- `analysis/` — ad-hoc analysis scripts (not part of the package)
- `debug/` — debugging/troubleshooting scripts (not part of the package)
- `cache/` — cached HTTP responses (gitignored, auto-created)

## Conventions

- All user-facing messages use `cli` (`cli_abort`, `cli_warn`, `cli_alert_*`)
- API key resolved from `GOOGLE_PLACES_API_KEY` env var or function argument
- `rlang::%||%` for NULL coalescing throughout
- Tibble in, tibble out at every pipeline stage
- Rate limiting via `RATE_LIMIT_SECS` constant (0.2s) between HTTP requests
- City/source validation via `validate_city_source()` in utils.R
- All scrapers report parse failure counts (not silent)
