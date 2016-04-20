pkgs <- c("dplyr", "reshape2", "ggplot2", "stringr", "coda", "scales")
invisible(lapply(pkgs, library, character.only = TRUE))

results <- read.csv("results.csv", stringsAsFactors = FALSE) %>%
  select(-one_of("X", "id", "prob", "repl")) %>%
  rename(model = algo) %>%
  mutate(model = ifelse(!is.na(scale) & scale, "lsm_scaled", model)) %>%
  mutate(model = ifelse(!is.na(beta.var), paste0(model, " (beta.var = ", beta.var, ")"), model)) %>%
  mutate(bias = estimate - beta, error = ifelse(beta == 0, 1 - coverage, coverage),
         type = ifelse(beta == 0, 1, 2)) %>%
  group_by(model, family, eta, beta, type, latent_space, nodes) %>%
  summarise(estimate = mean(estimate), loss = mean(loss), coverage = mean(coverage), bias = mean(bias),
            error = mean(error))

ggplot(results[results$beta == 1, ], aes(eta, loss, color = model, linetype = model)) +
  scale_x_log10(label = scientific_format()) +
  geom_point() + geom_line() +
  facet_grid(family ~ nodes + latent_space, scales = "free", labeller = label_context) +
  labs(x = expression(eta), y = "Generalization Error (MSE)")
ggsave("generalization.png", width = 12, height = 8)

ggplot(results[results$beta == 1 & results$latent_space == -1, ], aes(eta, bias, color = model, linetype = model)) +
  scale_x_log10(label = scientific_format()) +
  geom_point() + geom_line() +
  facet_grid(family ~ nodes, scales = "free", labeller = label_context) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  labs(x = expression(eta), y = "Bias", title = "Estimation Error w/ Latent Space")
ggsave("estimation_ls.png", width = 10, height = 8)

ggplot(results[results$beta == 1 & results$latent_space == 0, ], aes(eta, bias, color = model, linetype = model)) +
  scale_x_log10(label = scientific_format()) +
  geom_point() + geom_line() +
  facet_grid(family ~ nodes, scales = "free", labeller = label_context) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  labs(x = expression(eta), y = "Bias", title = "Estimation Error w/o Latent Space")
ggsave("estimation_nls.png", width = 10, height = 8)

ggplot(results[results$type == 1, ], aes(eta, error, color = model, linetype = model)) +
  scale_x_log10(label = scientific_format()) + 
  geom_point() + geom_line() +
  facet_grid(family ~ nodes + latent_space, scales = "free", labeller = label_context) +
  labs(x = expression(eta), y = expression(paste("Type 1 Error rate at ", alpha, " = .05")),
       title = "Type-1 Inferential Error")
ggsave("inference_type_1.png", width = 12, height = 8)

ggplot(results[results$type == 2, ], aes(eta, error, color = model, linetype = model)) +
  scale_x_log10(label = scientific_format()) + 
  geom_point() + geom_line() +
  facet_grid(family ~ nodes + latent_space, scales = "free", labeller = label_context) +
  labs(x = expression(eta), y = expression(paste("Type 1 Error rate at ", alpha, " = .05")),
       title = "Type-2 Inferential Error")
ggsave("inference_type_2.png", width = 12, height = 8)
