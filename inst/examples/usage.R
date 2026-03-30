# foodmap usage example
# --------------------
# Scrape restaurant guides, geocode venues, and export KML files
# you can import into Google My Maps to see all the pins on your phone.

library(foodmap)

# Set your Google Places API key (get one at https://console.cloud.google.com)
Sys.setenv(GOOGLE_PLACES_API_KEY = "your-api-key-here")

# --- Quick one-liner (default: Broadsheet) ---
create_food_map("sydney", output_dir = "~/maps")
# Outputs: ~/maps/sydney_broadsheet.kml + ~/maps/sydney_broadsheet.csv

# --- Other sources ---
create_food_map("sydney", source = "gourmet_traveller", output_dir = "~/maps")
create_food_map("sydney", source = "timeout", output_dir = "~/maps")
create_food_map("sydney", source = "agfg", output_dir = "~/maps")
create_food_map("sydney", source = "urban_list", output_dir = "~/maps")
create_food_map("sydney", source = "good_food_guide", output_dir = "~/maps")

# --- Step-by-step for more control ---

# 1. Scrape using the unified dispatcher
restaurants <- scrape_restaurants("sydney", source = "timeout")
restaurants

# Or call the source-specific function directly
restaurants <- scrape_gourmet_traveller("sydney")
restaurants <- scrape_timeout("melbourne")
restaurants <- scrape_agfg("brisbane")
restaurants <- scrape_urban_list("sydney")
restaurants <- scrape_good_food_guide("sydney")

# 2. Geocode only the rows missing lat/lng
restaurants <- geocode_restaurants(restaurants)

# 3. Export to KML and CSV
export_kml(restaurants, "sydney_timeout.kml")
export_csv(restaurants, "sydney_timeout.csv")

# 4. Import KML at https://mymaps.google.com
#    - Click "Create a new map" → "Import" → upload the .kml file
#    - Share the map and open the link on your phone in Google Maps
