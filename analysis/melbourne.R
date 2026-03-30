devtools::load_all()
restaurants <- scrape_broadsheet("melbourne")
nrow(restaurants)  # should be ~100
restaurants
