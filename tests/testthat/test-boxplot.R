context("boxplot")

test_that("gene-level boxplot with sample level faceting works", {
  genes <- c("800", "1009", "1289", "50509", "2191", "2335", "5159")
  genes <- "800"

  efds <- FacileData::exampleFacileDataSet()
  dat <- FacileData::fetch_assay_data(efds, genes, normalized = TRUE) |>
    FacileData::with_sample_covariates(c("indication", "sample_type")) |>
    mutate(sample_type = as.character(sample_type))

  dat$sample_type[sample(nrow(dat), 5)] <- NA

  fb <- fboxplot(dat, "sample_type", "value", with_points = TRUE,
                 # color_aes = "sample_type",
                 na_x = "remove")
  expect_class(fb, "FacileBoxPlotViz")
  fb <- fboxplot(dat, "sample_type", "value", with_points = TRUE,
                 # color_aes = "sample_type",
                 na_x = "keep")
  expect_class(fb, "FacileBoxPlotViz")
  
  fbf <- fboxplot(dat, "sample_type", "value", with_points = TRUE,
                  color_aes = "sample_type", facet_aes = "indication")
  expect_class(fbf, "FacileBoxPlotViz")
})
