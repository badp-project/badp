# Visual comparison: ggpubr::ggarrange vs arrange_plots_common_legend
#
# Run this script manually to generate reference PNGs for human inspection.
# Requires ggpubr to be installed for the "before" panel.
#
# Usage (from project root):
#   Rscript inst/visual_comparison.R

devtools::load_all()
library(magrittr)
library(ggplot2)

outdir <- file.path("inst", "visual_comparison_output")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# ── Prepare data ─────────────────────────────────────────────────────────────
data_prepared <- badp::economic_growth[, 1:6] %>%
  badp::feature_standardization(
    excluded_cols = c(country, year, gdp)
  ) %>%
  badp::feature_standardization(
    group_by_col  = year,
    excluded_cols = country,
    scale         = FALSE
  )

bma_results <- bma(small_model_space, round = 3, dilution = 0)

# ── Build the two individual ggplots (shared by both approaches) ─────────────
R   <- bma_results[[4]]
EMS <- bma_results[[8]]
PMPs   <- bma_results[[10]][, (R + 1):(R + 2)]
Priors <- bma_results[[11]]
M <- bma_results[[5]]

build_graphs <- function() {
  PMP_uniform <- cbind(PMPs[, 1], Priors[, 1])
  PMP_random  <- cbind(PMPs[, 2], Priors[, 2])
  PMP_uniform <- PMP_uniform[order(PMP_uniform[, 1], decreasing = TRUE), ]
  PMP_random  <- PMP_random[order(PMP_random[, 1], decreasing = TRUE), ]
  ranking <- seq_len(M)
  PMP_uniform <- cbind(ranking, PMP_uniform)
  PMP_random  <- cbind(ranking, PMP_random)
  colnames(PMP_uniform) <- colnames(PMP_random) <- c("ID", "Posterior", "Prior")
  fg1 <- tidyr::gather(as.data.frame(PMP_uniform), key = "Probability", value = "Value", -ID)
  fg2 <- tidyr::gather(as.data.frame(PMP_random),  key = "Probability", value = "Value", -ID)

  g1 <- ggplot(fg1, aes(x = ID, y = Value)) +
    geom_line(aes(color = Probability, linetype = Probability)) +
    scale_color_manual(values = c("darkred", "steelblue")) +
    ylab("Prior, Posterior") + xlab("Model number in the ranking") +
    ggtitle(paste0("Results with binomial model prior (EMS = ", EMS, ")"))

  g2 <- ggplot(fg2, aes(x = ID, y = Value)) +
    geom_line(aes(color = Probability, linetype = Probability)) +
    scale_color_manual(values = c("darkred", "steelblue")) +
    ylab("Prior, Posterior") + xlab("Model number in the ranking") +
    ggtitle(paste0("Results with binomial-beta model prior (EMS = ", EMS, ")"))

  list(g1, g2)
}

graphs <- build_graphs()

# ── Current version: arrange_plots_common_legend ─────────────────────────────
png(file.path(outdir, "current_model_pmp.png"), width = 800, height = 800)
arrange_plots_common_legend(graphs[[1]], graphs[[2]])
dev.off()

# ── model_sizes ──────────────────────────────────────────────────────────────
png(file.path(outdir, "current_model_sizes.png"), width = 800, height = 800)
model_sizes(bma_results)
dev.off()

# ── ggpubr version (if available) ───────────────────────────────────────────
if (requireNamespace("ggpubr", quietly = TRUE)) {
  png(file.path(outdir, "ggpubr_model_pmp.png"), width = 800, height = 800)
  print(ggpubr::ggarrange(
    graphs[[1]], graphs[[2]],
    labels = c("a)", "b)"),
    ncol = 1,
    common.legend = TRUE,
    legend = "bottom"
  ))
  dev.off()

  # model_sizes equivalent
  sizePriors       <- bma_results[[9]]
  modelPosterior   <- bma_results[[10]]
  dilution         <- bma_results[[12]]
  reg_ID           <- modelPosterior[, 1:R]
  uniform_post     <- matrix(modelPosterior[, R + 1], nrow = M)
  random_post      <- matrix(modelPosterior[, R + 2], nrow = M)
  sizes <- sapply(0:R, function(k) choose(R, k))
  ind   <- cumsum(sizes)
  for_sizes <- cbind(rowSums(reg_ID), reg_ID, uniform_post, random_post)
  for_sizes <- for_sizes[order(for_sizes[, 1]), ]
  mp <- matrix(for_sizes[, (R + 2):(R + 3)], nrow = M, ncol = 2)
  Posterior_sizes <- matrix(0, nrow = R + 1, ncol = 2)
  for (i in seq_len(R + 1)) {
    if (i == 1) {
      Posterior_sizes[i, ] <- mp[1, ]
    } else {
      Posterior_sizes[i, ] <- colSums(mp[(ind[i - 1] + 1):ind[i], , drop = FALSE])
    }
  }
  fg1s <- tidyr::gather(
    as.data.frame(setNames(as.data.frame(cbind(0:R, sizePriors[, 1], Posterior_sizes[, 1])),
                           c("ID", "Prior", "Posterior"))),
    key = "Probability", value = "Value", -ID)
  fg2s <- tidyr::gather(
    as.data.frame(setNames(as.data.frame(cbind(0:R, sizePriors[, 2], Posterior_sizes[, 2])),
                           c("ID", "Prior", "Posterior"))),
    key = "Probability", value = "Value", -ID)
  gs1 <- ggplot(fg1s, aes(x = ID, y = Value)) +
    geom_line(aes(color = Probability, linetype = Probability)) +
    scale_color_manual(values = c("darkred", "steelblue")) +
    ylab("Prior, Posterior") + xlab("Model size (number of regressors)") +
    ggtitle(paste0("Results with binomial model prior (EMS = ", EMS, ")"))
  gs2 <- ggplot(fg2s, aes(x = ID, y = Value)) +
    geom_line(aes(color = Probability, linetype = Probability)) +
    scale_color_manual(values = c("darkred", "steelblue")) +
    ylab("Prior, Posterior") + xlab("Model size (number of regressors)") +
    ggtitle(paste0("Results with binomial-beta model prior (EMS = ", EMS, ")"))

  png(file.path(outdir, "ggpubr_model_sizes.png"), width = 800, height = 800)
  print(ggpubr::ggarrange(gs1, gs2, labels = c("a)", "b)"),
                           ncol = 1, common.legend = TRUE, legend = "bottom"))
  dev.off()

  message("Generated 4 PNGs in ", outdir, "/ — compare *_model_pmp.png and *_model_sizes.png pairs")
} else {
  message("ggpubr not installed — only 'current' versions generated in ", outdir, "/")
  message("Install ggpubr to generate the reference images: install.packages('ggpubr')")
}
