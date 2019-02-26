# This was just a handy function developed for the mbl2018 pacakge, so putting
# it here until we have a better FacileViz version

#' Plot expression of a gene/feature (this function is not facile)
#'
#' @description
#' This is a conveniencde function that takes your expression object
#' (`DGEList`, or "voomed" object) and plots the expression of one gene (`gene`)
#' across the entire dataset grouped by the sample covariate specified by
#' `group`. You can optionally color each point by a second sample covariate
#' specified by the `color_by` parameter.
#'
#' @export
#' @seealso [mbl_tidy()]
#'
#' @param y the DGEList or voomed object your data is in
#' @param gene the name of the gene identifier or symbol you want to plot.
#' @param group what column in the $samples (or $targets, for vm) pheno table
#'   to use to plot split expression across the x axis
#' @param color_by name of column (like `group` param) to color your points by
#' @param wrap_by the column in the dataset to wrap the results by. If
#'   `length(gene) > 1`, you're going to need to wrwap.
#' @return a ggplot object
#'
#' @examples
#' y <- mbl_load_rnaseq("mouse", dataset = "mbl")
#' plot_expression(y, gene = "Fxyd6", group = "group",
#'                 color_by = "source")
#' plot_expression(y, gene = "Fxyd6", group = "genotype",
#'                 color_by = "source")
plot_expression <- function(y, gene, group = "group",
                            color_by = NULL,
                            wrap_by = "symbol",
                            inorder = TRUE, ...) {
  assert_character(group, min.len = 1, max.len = 2)
  assert_character(gene, min.len = 1)

  if (is(y, "DGEList")) {
    stopifnot(group %in% colnames(y$samples))
  } else if (is(y, "EList")) {
    stopifnot(group %in% colnames(y$targets))
  } else {
    stop("y must be either a DGEList or EList")
  }

  gidx <- sapply(gene, function(g) .search_df_columns(y$genes, g))
  isna <- is.na(gidx)
  if (all(isna)) {
    stop("No (gene) queries found in $genes metadata")
  } else if (any(isna)) {
    warning("Some genes not found from query, only keeping: ",
            paste(gene[!isna], collapse = ","))
    gidx <- gidx[!isna]
    gene <- gene[!isna]
  }

  dat <- tidy(y[gidx,])

  # Add column to data.frame to indicate group of observations
  group_cols <- .aes_varval_colnames(".groupby.", dat)
  dat <- .with_aes_columns(dat, group, group_cols)

  if (!is.null(color_by)) {
    assert_character(color_by, min.len = 1, max.len = 2)
    assert_subset(color_by, colnames(dat))
    dat <- with_color(dat, color_by)
  }

  # gg <- ggplot(dat, aes(x = !!group_cols[["variable"]], y = cpm)) +
  aes.string <- aes_string(x = group_cols[["variable"]], y = "cpm")
  gg <- ggplot(dat, aes.string) +
    geom_boxplot(outlier.size = 0) +
    ylab("log2(cpm)") +
    xlab(paste(group, collapse = "_")) +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    ggtitle(paste(gene, "Expression"))

  if (length(gidx) > 1L) {
    wrap.by <- unique(wrap_by, c("symbol", "gene_name", "gene"))
    wrap.idx <- match(wrap.by, tolower(colnames(dat)))
    if (all(is.na(wrap.idx))) {
      # find the column with a gene identifier in it.
      find.me <- rownames(y)[gidx]
      wrap.idx <- which(sapply(dat, function(cval) {
        find.me %in% cval
      }))
      wrap.idx <- wrap.idx[1L]
    } else {
      wrap.idx <- wrap.idx[!is.na(wrap.idx)][1L]
    }
    if (length(wrap.idx) == 0 || is.na(wrap.idx)) {
      stop("Don't know how to wrap multiple gene plots")
    }
    wrap.by <- colnames(dat)[wrap.idx]
    if (inorder) {
      wrap.lvls <- c(gene, sort(unique(dat[[wrap.by]])))
      wrap.lvls <- intersect(wrap.lvls, dat[[wrap.by]])
      dat[[wrap.by]] <- factor(dat[[wrap.by]], wrap.lvls)
    }

    gg <- gg + facet_wrap(wrap.by)
  }

  color_map <- aes_map(dat)$color
  if (!is.null(color_map)) {
    column <- attr(color_map, "columns")
    gg <- gg +
      geom_jitter(aes_string(color = column$variable), width = 0.25) +
      scale_color_manual(values = color_map)

  } else {
    gg <- gg + geom_jitter(width = 0.25)
  }

  gg
}

# Utilify functions ============================================================
# # Some serious voodoo is going on here
# .with_aes_columns <- function(x, aesthetic, out_column, ...) {
#   assert_class(x, "data.frame")
#   assert_character(aesthetic, min.len = 1, max.len = min(3, ncol(x)))
#   assert_subset(aesthetic, colnames(x))
#
#   if (length(aesthetic) == 1L) {
#     x[[out_column]] <- x[[aesthetic]]
#   } else {
#     is.cat <- sapply(x[, aesthetic], test_categorical)
#     assert_true(all(is.cat))
#     x <- tidyr::unite_(x, out_column, aesthetic, remove = FALSE)
#   }
#
#   x
# }

#' Fast and loose way to identify which row contains a query across a data.frame
#'
#' @param x `data.frame` to search
#' @param query the string/element to search for
.search_df_columns <- function(x, query) {
  assert_data_frame(x)
  assert_string(query)
  oquery <- query

  gi <- c(list(rn = rownames(x)), as.list(x))
  query <- tolower(query)

  idx <- NA_integer_
  for (cname in names(gi)) {
    vals <- tolower(gi[[cname]])
    i <- which(vals == query)
    if (length(i) > 1) {
      warning("More than one row for `", query, "` found -- taking first one",
              call. = TRUE)
      idx <- i[1L]
      break
    } else if (length(i) == 1) {
      idx <- i
    }
  }

  idx
}
