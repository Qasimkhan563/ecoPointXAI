test_that('leaflet dependency check', {
  skip_if_not_installed('leaflet')
  expect_true(requireNamespace('leaflet', quietly = TRUE))
})
