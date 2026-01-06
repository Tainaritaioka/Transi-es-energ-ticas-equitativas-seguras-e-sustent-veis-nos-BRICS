

set.seed(123)

# -----------------------
# Pacotes
# -----------------------
pkgs <- c(
  "dplyr","tidyr","ggplot2","ggrepel","factoextra","cluster","NbClust",
  "readxl","readr","janitor","gridExtra","RColorBrewer","scales","grid","tibble","kableExtra"
)
inst <- pkgs %in% rownames(installed.packages())
if(any(!inst)) install.packages(pkgs[!inst], dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)

# -----------------------
# Importar base
# -----------------------
db <- read_excel("SiNTESE-INDICADORES-CLUSTER-15-10.xlsx", sheet = "Base") %>%
  clean_names() %>%
  mutate(pais_ano = paste0(pais, "_", ano)) %>%
  select(pais_ano, everything())

anos <- c(2010, 2015, 2022)

# -----------------------
# Variáveis por dimensão
# -----------------------
equidade_vars <- c(
  "acesso_a_eletricidade",
  "acesso_a_eletricidade_rural",
  "acesso_a_combustiveis_e_tecnologias_limpas_para_cozinhar",
  "importacoes_combustiveis",
  "uso_de_energia_per_capita"
)

seguranca_vars <- c(
  "importacoes_de_energia",
  "diversidade_do_consumo_de_energia",
  "diversidade_da_geracao_de_eletricidade",
  "perdas_no_setor_eletrico",
  "vantagem_comparativa_lct"
)

sustent_vars <- c(
  "intensidade_de_emissoes_no_setor_eletrico",
  "intensidade_de_carbono_na_producao_de_energia",
  "intensidade_energetica",
  "participacao_das_fontes_de_baixo_carbono_na_geracao_de_eletricidade",
  "emissoes_co2_per_capita"
)

# -----------------------
# Labels legíveis
# -----------------------
equidade_labels <- c(
  "Acesso à eletricidade",
  "Acesso à eletricidade (área rural)",
  "Acesso a combustíveis limpos para cozinhar",
  "Importações de combustíveis",
  "Uso de energia per capita"
)

seguranca_labels <- c(
  "Importações de energia",
  "Diversidade do consumo de energia",
  "Diversidade da geração de eletricidade",
  "Perdas no setor elétrico",
  "Vantagem comparativa LCT"
)

sustent_labels <- c(
  "Intensidade de emissões do setor elétrico",
  "Intensidade de carbono na produção de energia",
  "Intensidade energética",
  "Participação de fontes de baixo carbono na geração",
  "Emissões de CO2 per capita"
)

# Lookup
lookup_labels <- tibble(
  var = c(equidade_vars, seguranca_vars, sustent_vars),
  label = c(equidade_labels, seguranca_labels, sustent_labels)
)

label_var_single <- function(x) {
  idx <- match(x, lookup_labels$var)
  ifelse(is.na(idx), x, lookup_labels$label[idx])
}

label_var <- function(x) sapply(x, label_var_single, USE.NAMES = FALSE)

# Funções auxiliares
zscore <- function(x) as.data.frame(scale(x, center = TRUE, scale = TRUE))

cluster_centers_original <- function(data_orig, cluster_vec) {
  dfc <- as.data.frame(data_orig)
  dfc$cluster <- cluster_vec
  dfc %>% group_by(cluster) %>% summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)))
}

dim_colors <- c(
  "Equidade" = "red",
  "Seguranca" = scales::alpha("blue", 0.6),
  "Sustentabilidade" = scales::alpha("green", 0.6)
)

# =====================================================================
#   FUNÇÃO PRINCIPAL — com ajustes solicitados
# =====================================================================
explore_dimension_all_years <- function(db, vars, dim_name, k_try = 2:6, fixed_k = NULL,
                                        save_path = "outputs_explore") {
  
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
  
  sub <- db %>% select(any_of(c("pais_ano","pais","codigo","ano")), all_of(vars))
  rownames(sub) <- sub$pais_ano
  
  data_scaled <- scale(sub[vars], center = TRUE, scale = TRUE)
  pca_res <- prcomp(data_scaled, center = FALSE, scale. = FALSE)
  
  # --- Scree plot ---
  eig_val <- (pca_res$sdev)^2
  eig_perc <- eig_val / sum(eig_val) * 100
  df_scree <- data.frame(PC = paste0("PC", seq_along(eig_perc)), Variancia = round(eig_perc, 2))
  
  scree_plot <- ggplot(df_scree, aes(factor(PC), Variancia)) +
    geom_bar(stat = "identity", fill = dim_colors[dim_name], alpha = .85) +
    geom_text(aes(label = paste0(Variancia, "%")), vjust = -0.5) +
    labs(title = paste0("Variância Explicada - ", dim_name), y = "Variância (%)", x = "") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(scree_plot)
  
  # --- Tabela PCA ---
  loadings <- pca_res$rotation
  top_vars <- apply(loadings, 2, function(x) names(sort(abs(x), decreasing = TRUE)[1:2]))
  top_vars_legivel <- apply(top_vars, 2, function(v) paste(label_var(v), collapse = ", "))
  
  legenda_df <- data.frame(PC = colnames(loadings),
                           `Variáveis principais` = top_vars_legivel,
                           check.names = FALSE)
  
  legenda_table <- gridExtra::tableGrob(legenda_df, rows = NULL)
  top_title <- grid::textGrob(paste0("PCs — Variáveis principais (", dim_name, ")"),
                              gp = grid::gpar(fontface = "bold"))
  grid::grid.newpage()
  gridExtra::grid.arrange(legenda_table, top = top_title)
  
  # --- Biplot ---
  scores_df <- as.data.frame(pca_res$x[, 1:2])
  scores_df$pais_ano <- sub$pais_ano
  
  load_df <- as.data.frame(pca_res$rotation[, 1:2])
  load_df$var <- label_var(rownames(load_df))
  
  p_biplot <- ggplot() +
    geom_point(data = scores_df, aes(PC1, PC2), size = 3) +
    geom_segment(data = load_df,
                 aes(x = 0, y = 0,
                     xend = PC1 * max(abs(scores_df$PC1)),
                     yend = PC2 * max(abs(scores_df$PC2))),
                 arrow = arrow(length = unit(0.25, "cm")),
                 color = dim_colors[dim_name]) +
    ggrepel::geom_text_repel(data = scores_df, aes(PC1, PC2, label = pais_ano), size = 3) +
    ggrepel::geom_text_repel(data = load_df,
                             aes(PC1 * max(abs(scores_df$PC1)),
                                 PC2 * max(abs(scores_df$PC2)),
                                 label = var),
                             color = dim_colors[dim_name]) +
    labs(title = paste0("Biplot PCA - ", dim_name)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p_biplot)
  
  # --- Silhueta ---
  X_pcs <- pca_res$x[, 1:min(5, ncol(pca_res$x)), drop = FALSE]
  
  sil <- sapply(k_try, function(k) {
    km_tmp <- kmeans(X_pcs, centers = k, nstart = 25)
    mean(cluster::silhouette(km_tmp$cluster, dist(X_pcs))[, 3])
  })
  
  df_sil <- data.frame(k = k_try, sil = sil)
  
  p_sil <- ggplot(df_sil, aes(k, sil)) +
    geom_line(color = dim_colors[dim_name]) +
    geom_point(size = 3) +
    labs(title = paste0("Silhueta Média - ", dim_name),
         x = "k", y = "Média da Silhueta") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p_sil)
  
  # --- escolha de k ---
  if (!is.null(fixed_k)) k_choice <- fixed_k else k_choice <- k_try[which.max(sil)]
  
  # --- Kmeans e PAM ---
  km_final <- kmeans(X_pcs, centers = k_choice, nstart = 200)
  pam_final <- pam(dist(X_pcs), k = k_choice)
  
  results_df <- sub %>%
    mutate(
      PC1 = X_pcs[,1], PC2 = X_pcs[,2],
      kmeans_cluster = factor(km_final$cluster),
      kmedoids_cluster = factor(pam_final$clustering)
    )
  
  # --- PCA Kmeans (com limpeza de página) ---
  grid::grid.newpage()
  p_km <- ggplot(results_df, aes(PC1, PC2, color = kmeans_cluster, label = pais_ano)) +
    geom_point(size = 4) +
    ggrepel::geom_text_repel(size = 3) +
    scale_color_manual(values = RColorBrewer::brewer.pal(8, "Set2")[1:k_choice]) +
    labs(title = paste0("PCA - Kmeans Clusters de ", dim_name), color = "Cluster") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  print(p_km)
  
  # --- PCA Kmedoids (com limpeza de página) ---
  grid::grid.newpage()
  p_pam <- ggplot(results_df, aes(PC1, PC2, color = kmedoids_cluster, label = pais_ano)) +
    geom_point(size = 4) +
    ggrepel::geom_text_repel(size = 3) +
    scale_color_manual(values = RColorBrewer::brewer.pal(8, "Set2")[1:k_choice]) +
    labs(title = paste0("PCA - Kmedoids Clusters de ", dim_name), color = "Cluster") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  print(p_pam)
  
  # --- Tabelas ---
  perfis <- sub %>%
    mutate(cluster = km_final$cluster) %>%
    group_by(cluster) %>%
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE))) %>%
    arrange(cluster) %>%
    rename_with(~ label_var(.x), all_of(vars))
  
  centers_km <- cluster_centers_original(sub[vars], km_final$cluster) %>%
    rename_with(~ label_var(.x), all_of(vars))
  
  centers_pam <- cluster_centers_original(sub[vars], pam_final$clustering) %>%
    rename_with(~ label_var(.x), all_of(vars))
  
  write_csv(perfis, file.path(save_path, paste0("perfil_por_indicador_", dim_name, ".csv")))
  write_csv(centers_km, file.path(save_path, paste0("centros_kmeans_", dim_name, ".csv")))
  write_csv(centers_pam, file.path(save_path, paste0("centros_pam_", dim_name, ".csv")))
  
  invisible(list(
    results_df = results_df,
    pca = pca_res,
    km = km_final,
    pam = pam_final,
    perfis = perfis,
    centers_km = centers_km,
    centers_pam = centers_pam
  ))
}

# =====================================================================
# LOOP PRINCIPAL — Equidade com k = 4
# =====================================================================
all_results <- list()

dims <- list(
  Equidade = equidade_vars,
  Seguranca = seguranca_vars,
  Sustentabilidade = sustent_vars
)

for (dim_name in names(dims)) {
  message("\nRodando dimensão: ", dim_name)
  
  if (dim_name == "Equidade") {
    res <- explore_dimension_all_years(db, dims[[dim_name]], dim_name,
                                       k_try = 2:6,
                                       fixed_k = 5)
  } else {
    res <- explore_dimension_all_years(db, dims[[dim_name]], dim_name,
                                       k_try = 2:6)
  }
  
  all_results[[dim_name]] <- res
}

# FIM
 
