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

CI: GitHub Actions runs `R CMD check` on push/PR to `main`.

## Git workflow

Solo repo: commit and push **directly to `main`** — no pull request or
feature branch needed. This overrides the PR-only rule in the global
`~/.claude/CLAUDE.md`. (Still only commit/push when explicitly asked.)

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
| Concrete Playground | `scrape_concrete_playground()` | syd, mel | JSON-LD venue cards across 3 sections: **restaurants + bars + cafes** |
| Sprudge (coffee) | `scrape_sprudge()` | sf, ny, la, london, syd, mel | WordPress search → article spotlights (Coffee Design / Sprudge Maps / Build-Outs) |
| Eater | `scrape_eater()` | sf, ny, la, london | Inline JSON in map pages. Per-city guide slugs include best-bakeries, best-coffee, best-cocktail-bars where Eater publishes them. |
| Michelin Guide | `scrape_michelin()` | sf, ny, la, london | JSON-LD detail pages, parsed in callr worker batches (≤15 URLs/batch) to isolate libxml2 segfaults from the parent R process |
| Broadsheet Guides | `scrape_broadsheet_guides()` | syd, mel, bri, adl, per, hob | `/{city}/guides/{slug}` editorial cafe/bar/pub lists — schema.org `ItemList` JSON-LD |
| Good Food Guide Awards | `scrape_gfg_awards()` | syd (NSW/ACT) | Curated SMH GFG 2026 hatted-restaurants list from the awards article (hat counts only) |
| Infatuation | `scrape_infatuation()` | sf, ny, la, london | City editorial guides — JSON-LD `ItemList` with full venue details (one fetch per guide) |
| Resy | `scrape_resy()` | ny, la (+ historical) | Monthly Hit List — `article.teaser2` blocks with `data-lat`/`data-lng` |
| Thrillist | `scrape_thrillist()` | per city | "Best Restaurants in CITY" — inline `Restaurant` JSON-LD with GeoCoordinates |
| Conde Nast Traveler | `scrape_cn_traveler()` | per city | "Best Restaurants" gallery articles — `<h3>` venue names, geocoder resolves location |
| Bon Appétit | `scrape_bonappetit()` | US-wide | Annual "Best New Restaurants in America" — `<strong>` venue/city blocks |
| World's 50 Best | `scrape_worlds50best()` | global | Ranked list (1-100) cards + `Restaurant` JSON-LD detail pages |
| James Beard Awards | `scrape_james_beard()` | US by city | Hand-curated embedded tribble of JBA winners/finalists (site is client-rendered) |
| 7x7 | `scrape_7x7()` | sf | Annual "Big Eat"/"Big Cheap" SF dish lists — numbered-paragraph entries |
| Hale 'Aina Awards | `scrape_hale_aina()` | honolulu | Honolulu Magazine reader-voted awards — Gold/Silver/Bronze/Finalist picks |
| HONOLULU Magazine | `scrape_honolulu_magazine()` | honolulu | Editorial "best of" roundups (two staple articles) |

All scrapers are dispatched via `scrape_restaurants(city, source)`.

### Broadsheet scraping strategy (3-strategy cascade)

1. **Direct API**: POST to `frontend-next.broadsheet.com.au/api/hotlist/{city}` with pagination.
2. **Static RSC extraction**: `self.__next_f.push()` payloads → bracket-counted JSON extraction.
3. **Chromote headless browser**: scrolls 20 times, then RSC or DOM extraction.

Note: Broadsheet does **not** use `<script id="__NEXT_DATA__">`.

### Geocoding

Two backends, selected via `geocode_restaurants(provider = ...)`:

- **`"osm"` (default)** — free OpenStreetMap Nominatim. No API key, no surprise bills, ~1 req/sec (`NOMINATIM_RATE_LIMIT_SECS = 1.1`). Weaker than Google for restaurant-by-name queries (~50–70% hit rate vs ~95%); strong on street addresses. Implementation in `R/geocode_nominatim.R`.
- **`"google"`** — Google Places API (New) Text Search. Requires `GOOGLE_PLACES_API_KEY`. Pay-per-call (Text Search Pro tier, ~A$0.015–0.048/call). Rate-limited via `RATE_LIMIT_SECS` (0.2s). Use only when explicitly needed — never use as an auto-fallback.

**There is no automatic fallback between backends.** A miss on the chosen provider leaves the row's coords as `NA` and prints a warning. This is the budget guarantee.

**OSM-only address-only retry**: when the name+address+suburb query misses, the loop retries with an address-only query (dropping the venue name). Nominatim is much stronger on pure addresses than venue names — a venue often has a building node even when its name isn't tagged. The retry costs +1.1s wall-clock but $0 (OSM only — gated by `provider == "osm"` so it never fires for Google).

Query format (both backends): `"{name} {address} {suburb} {state} {country}"`, built by `build_geocode_query()`. Idempotent — skips rows that already have coordinates. AGFG scraper can fetch coords from JSON-LD detail pages, reducing API usage for either backend.

### Diagnostics

`export_diagnostics(restaurants, path)` writes a `{city}_diagnostics.csv` listing every row missing one or more key fields (`latitude`, `address`, `description`, `price_range`, `cuisine`, `url`). Each row gets an `issues` column (comma-separated list of missing fields) and an `issue_count` column for worst-first sorting. Wired into every analysis script. Use the CSV to drive manual fixes via `inst/extdata/manual_excludes.csv` or one-off coord/description corrections.

### HTTP caching

`use_cache = TRUE` on scrapers stores responses in `cache/` directory (24h expiry). Implemented via `cached_fetch()` in `R/cache.R`. Good Food Guide excluded (uses chromote, not plain HTTP).

### Parsed-tibble caching

Layered above the HTML cache. When `scrape_restaurants(use_parsed_cache = TRUE, use_cache = TRUE)` (both default to TRUE / honored only together), the parsed tibble output of each scraper call is saved to `cache/parsed/{source}_{city}.rds` with a sidecar `{source}_{city}.manifest.json` listing every HTML cache file the scrape touched. Subsequent runs check the manifest: if every tracked HTML file still exists with matching mtime, the parsed tibble is returned without re-parsing.

URL tracking is automatic: `cached_fetch()` calls `cache_track_record(url)` after every retrieval, and `cached_scrape()` activates the tracker around the expression eval. Chromote-driven scrapers (Good Food Guide, Broadsheet fallback) bypass `cached_fetch()`, so their URL list is empty and the cache falls back to a 24h TTL on the parsed file itself.

Storage format: RDS (justified per the data-conventions carve-out for R-specific object types — tibbles carry class metadata and source-typed columns that don't round-trip cleanly through CSV/parquet without a schema declaration). The cache is internal and never shipped externally.

Disable per-run with `scrape_all_sources(use_parsed_cache = FALSE)` or by setting `use_cache = FALSE` (which implicitly disables the parsed cache too — fresh HTTP means fresh parse).

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

## Committed CI fixtures (`inst/extdata/`)

`inst/extdata/*` is **deliberately tracked** (see `.gitignore`'s `!inst/extdata/*.csv`) — a narrow exception to the "bulk data stays out of git" rule. These are small, deterministic baselines CI loads when its environment can't reproduce local coverage:

- `expected_counts.csv` — per-city venue-count floors for `assert_venue_count()`.
- `geocodes_seed.csv` — Google-quality coordinate baseline. CI starts on a cold cache + lower-yield Nominatim, so without it sydney/honolulu drop below floor. `geocode_cache_apply()` layers the live run cache over it.
- `michelin_snapshots/*.rds` — Michelin results captured locally, used when Michelin's WAF blocks CI's datacenter IP.

Refresh `geocodes_seed.csv` by copying `cache/geocodes.csv` after a local run; refresh the Michelin snapshots via `analysis/refresh_michelin_snapshot.R`. Bulk analytics data still belongs in Releases, not git.

## Conventions

- All user-facing messages use `cli` (`cli_abort`, `cli_warn`, `cli_alert_*`)
- API key resolved from `GOOGLE_PLACES_API_KEY` env var or function argument
- `rlang::%||%` for NULL coalescing throughout
- Tibble in, tibble out at every pipeline stage
- Rate limiting via `RATE_LIMIT_SECS` constant (0.2s) between HTTP requests
- City/source validation via `validate_city_source()` in utils.R
- All scrapers report parse failure counts (not silent)
