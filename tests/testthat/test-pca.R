context("Facile PCA Vizualization")

test_that("plot(fpca) is baller", {
  m <- matrix(rnorm(100 * 10), nrow = 100)
  colnames(m) <- letters[1:10]
  pdat <- as.data.frame(FacileAnalysis:::example_aes_data_table(10, n.cats = 5))
  rownames(pdat) <- colnames(m)

  res <- fpca(m, col_covariates = pdat)
  fp2 <- visualize(res, 1:2, color_aes = "category")
  fp3 <- visualize(res, 1:3, color_aes = "category")
})

