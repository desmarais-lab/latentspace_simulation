pkgs <- c("dplyr", "reshape2", "ggplot2", "stringr", "coda")
invisible(lapply(pkgs, library, character.only = TRUE))

results <- read.csv("results.csv", stringsAsFactors = FALSE) %>%
  select(-one_of("X", "id", "prob", "repl")) %>%
  rename(model = algo)

results$model <- ifelse(!is.na(results$scale) & results$scale, "lsm_scaled", results$model)

results$bias <- results$estimate - results$beta
results$error <- ifelse(results$beta == 0, 1 - results$coverage, results$coverage)
results$type <- ifelse(results$beta == 0, 1, 2)

ggplot(results[results$beta == 1, ], aes(eta, loss, color = model, linetype = model)) +
  scale_x_log10() +
  stat_smooth(method = "loess", se = TRUE) +
  facet_grid(family ~ nodes + latent_space, scales = "free", labeller = label_context) +
  labs(x = expression(eta), y = "Generalization Error (MSE)")
ggsave("generalization.png", width = 10, height = 8)

ggplot(results[results$beta == 1, ], aes(eta, bias, color = model, linetype = model)) +
  scale_x_log10() +
  stat_smooth(method = "loess", se = TRUE) +
  facet_grid(family ~ nodes + latent_space, scales = "free", labeller = label_context) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  labs(x = expression(eta), y = "Bias")
ggsave("estimation.png", width = 10, height = 8)

ggplot(results, aes(eta, error, color = model, linetype = model)) +
  stat_smooth(method = "loess", se = TRUE) +
  facet_grid(family + type ~ nodes + latent_space, scales = "free", labeller = label_context) +
  labs(x = expression(eta), y = expression(paste("Error rate at ", alpha, " = .05"))) +
  scale_x_log10()
ggsave("inference.png", width = 10, height = 8)
