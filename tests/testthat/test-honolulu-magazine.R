# Tests for HONOLULU Magazine scraper helpers in R/scrape_honolulu_magazine.R

# --- honolulu_mag_entry_body -------------------------------------------------

test_that("body starts at entry-title H1 and ends before Related sidebar", {
  html <- paste0(
    '<head>noise</head>',
    '<h1 class="entry-title">Best Restaurants in Honolulu</h1>',
    '<h1 style="text-align: center;">Aburiya Ibushi</h1>',
    '<p>Some prose.</p>',
    '<h2>Related</h2>',
    '<div>sidebar content</div>'
  )
  body <- honolulu_mag_entry_body(html)
  expect_true(grepl("Aburiya Ibushi", body))
  expect_false(grepl("sidebar content", body))
})

test_that("falls back to full HTML when no entry-title H1 found", {
  html <- "<h1>No entry title</h1><h2>Venue</h2>"
  expect_equal(honolulu_mag_entry_body(html), html)
})


# --- honolulu_mag_extract_headings ------------------------------------------

test_that("H1 venues with text-align style are extracted", {
  body <- paste0(
    '<h1 class="entry-title">Page Header</h1>',
    '<h1 style="text-align: center;">Aburiya Ibushi</h1>',
    '<p>Prose.</p>',
    '<h1 style="text-align: center;">Bar Maze</h1>'
  )
  res <- honolulu_mag_extract_headings(body, level = 1L, attr_filter = "text-align")
  expect_equal(res$names, c("Aburiya Ibushi", "Bar Maze"))
})

test_that("bare H2 venues are extracted (best-new-restaurants layout)", {
  body <- paste0(
    '<h2>Chao Hawai&lsquo;i</h2>',
    '<p>...</p>',
    '<h2>Faria</h2>',
    '<h2 class="widgettitle">Subscribe</h2>'
  )
  res <- honolulu_mag_extract_headings(body, level = 2L, attr_filter = NULL,
                                        require_bare = TRUE)
  expect_equal(length(res$names), 2)
  expect_equal(res$names[1], "Chao Hawai‘i")
  expect_equal(res$names[2], "Faria")
})

test_that("HTML wrappers inside the heading are stripped", {
  body <- '<h1 style="text-align: center;"><strong>Mud Hen Water</strong></h1>'
  res <- honolulu_mag_extract_headings(body, level = 1L, attr_filter = "text-align")
  expect_equal(res$names, "Mud Hen Water")
})


# --- honolulu_mag_extract_description ----------------------------------------

test_that("first non-trivial paragraph becomes the description", {
  block <- "<p>tiny</p><p>This is a longer paragraph of editorial prose about the restaurant.</p>"
  result <- honolulu_mag_extract_description(block)
  expect_true(grepl("longer paragraph", result))
})

test_that("description longer than 500 chars is truncated", {
  long_text <- paste(rep("word ", 200), collapse = "")
  block <- paste0("<p>", long_text, "</p>")
  result <- honolulu_mag_extract_description(block)
  expect_true(nchar(result) <= 500)
  expect_true(endsWith(result, "..."))
})

test_that("description returns NA when no paragraph is long enough", {
  block <- "<p>tiny</p><p>also short</p>"
  expect_true(is.na(honolulu_mag_extract_description(block)))
})
