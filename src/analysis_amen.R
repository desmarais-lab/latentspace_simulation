pkgs = c("dplyr", "ggplot2", "scales", "data.table")
invisible(lapply(pkgs, library, character.only = TRUE))

results = fread("results_amen.csv") %>%
  filter(eta != .1 & !(algorithm == "truth" & latent_space == 0) &
    algorithm != "lsm" & !(latent_space == -1) & family != "poisson" & nodes != 50) %>%
  mutate(bias = estimate - beta,
    error = ifelse(beta == 0, 1 - coverage, coverage),
    type = ifelse(beta == 0, 1, 2),
    family = ifelse(family == "binomial", "bernoulli", family)) %>%
  group_by(algorithm, family, eta, beta, type, latent_space, nodes) %>%
  summarise(n = length(estimate[!is.na(estimate)]),
            estimate = mean(estimate, na.rm = TRUE),
            loss = mean(loss, na.rm = TRUE),
            coverage = mean(coverage, na.rm = TRUE),
            bias = mean(bias, na.rm = TRUE),
            error = mean(error, na.rm = TRUE))

ggplot(results, aes(eta, n, color = algorithm, linetype = algorithm)) +
  scale_x_log10(breaks = c(1, 100, 1000000), labels = c("uniform correlation", "low correlation", "independence")) +
  scale_y_log10(breaks = c(10, 50, 100, 500)) +
  geom_point() + geom_line() +
  facet_grid(family + beta ~ nodes + latent_space, scales = "free", labeller = label_context) +
  labs(x = expression(eta), y = "Iterations Completed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
ggsave("iterations_amen.png", width = 12, height = 8)

ggplot(filter(results, beta == 1), aes(eta, loss,
  color = algorithm, linetype = algorithm)) +
  scale_x_log10(breaks = c(1, 100, 1000000), labels = c("uniform correlation", "low correlation", "independence")) +
  geom_point() + geom_line() +
  facet_grid(family ~ nodes + latent_space, scales = "free", labeller = label_context) +
  labs(x = expression(eta), y = "Mean Squared Error", title = "Generalization Error") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
ggsave("generalization_amen.png", width = 12, height = 8)

ggplot(filter(results, beta == 1, latent_space == 1),
  aes(eta, bias, color = algorithm, linetype = algorithm)) +
  scale_x_log10(breaks = c(1, 100, 1000000), labels = c("uniform correlation", "low correlation", "independence")) +
  geom_point() + geom_line() +
  facet_grid(family ~ nodes, scales = "free", labeller = label_context) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  labs(x = expression(eta), y = "Bias", title = "Estimation Error w/ Latent Space") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
ggsave("estimation_ls_amen.png", width = 10, height = 8)

ggplot(filter(results, beta == 1, latent_space == 0),
  aes(eta, bias, color = algorithm, linetype = algorithm)) +
  scale_x_log10(breaks = c(1, 100, 1000000), labels = c("uniform correlation", "low correlation", "independence")) +
  geom_point() + geom_line() +
  facet_grid(family ~ nodes, scales = "free", labeller = label_context) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  labs(x = expression(eta), y = "Bias", title = "Estimation Error w/o Latent Space") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
ggsave("estimation_nls_amen.png", width = 10, height = 8)

ggplot(filter(results, type == 1),
  aes(eta, error, color = algorithm, linetype = algorithm)) +
  scale_x_log10(breaks = c(1, 100, 1000000), labels = c("uniform correlation", "low correlation", "independence")) +
  geom_point() + geom_line() +
  facet_grid(family ~ nodes + latent_space, scales = "free", labeller = label_context) +
  labs(x = expression(eta), y = expression(paste("Type 1 Error rate at ", alpha, " = 0.05")),
       title = "Type-1 Inferential Error") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
ggsave("inference_type_1_amen.png", width = 12, height = 8)

ggplot(filter(results, type == 2), aes(eta, error, color = algorithm, linetype = algorithm)) +
  scale_x_log10(breaks = c(1, 100, 1000000), labels = c("uniform correlation", "low correlation", "independence")) +
  geom_point() + geom_line() +
  facet_grid(family ~ nodes + latent_space, scales = "free", labeller = label_context) +
  labs(x = expression(eta), y = expression(paste("Type 2 Error rate at ", alpha, " = 0.05")),
       title = "Type-2 Inferential Error") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
ggsave("inference_type_2_amen.png", width = 12, height = 8)
