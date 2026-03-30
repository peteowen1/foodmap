devtools::load_all()
restaurants <- scrape_broadsheet("sydney")
nrow(restaurants)  # should be ~100
restaurants

devtools::load_all()
create_food_map("melbourne", output_dir = "output")
