context("Test functions in the package")

filename <- system.file("NOAAearthquakes.txt", package = "earthquakesNOAA")
data <- readr::read_delim("NOAAearthquakes.txt", delim = "\t")

test_that("eq_clean_data returns data frame", {
  expect_is(eq_clean_data(data), "data.frame")
})

test_that("eq_clean_data$DATE is Date type", {
  expect_is(eq_clean_data(data)$DATE, "Date")
})

test_that("eq_clean_data returns numeric coordinates", {
  expect_is(eq_clean_data(data)$LATITUDE, "numeric")
  expect_is(eq_clean_data(data)$LONGITUDE, "numeric")
})

test_that("eq_location_clean returns a data frame", {
  expect_is(eq_location_clean(data), "data.frame")
})

test_that("eq_map returns leaflet object", {
  l <- data %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")
  expect_is(l, "leaflet")
})
