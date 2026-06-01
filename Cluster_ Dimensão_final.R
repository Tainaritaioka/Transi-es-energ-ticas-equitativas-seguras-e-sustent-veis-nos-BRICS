set.seed(123)

# =======================
# PACOTES
# =======================

pkgs <- c(
  "dplyr","tidyr","ggplot2","ggrepel",
  "FactoMineR","cluster","clusterSim",
  "readxl","janitor","gridExtra",
  "grid","gtable","Cairo"
)

inst <- pkgs %in% rownames(installed.packages())

if(any(!inst)){
  install.packages(
    pkgs[!inst],
    dependencies = TRUE
  )
}

invisible(
  lapply(
    pkgs,
    library,
    character.only = TRUE
  )
)

select <- dplyr::select
filter <- dplyr::filter

# =======================
# FUNÇÕES
# =======================

formatar_nome <- function(x){
  
  x <- gsub("_"," ",x)
  
  x <- tools::toTitleCase(x)
  
  x <- gsub(
    "\\bLct\\b",
    "LCT",
    x
  )
  
  x
}

salvar_figura <- function(
    plot_obj,
    out_dir,
    nome,
    width,
    height
){
  
  # PNG
  ggsave(
    filename = file.path(
      out_dir,
      paste0(nome,".png")
    ),
    
    plot = plot_obj,
    
    width = width,
    height = height,
    
    dpi = 600,
    
    bg = "white"
  )
  
  # PDF vetorial
  ggsave(
    filename = file.path(
      out_dir,
      paste0(nome,".pdf")
    ),
    
    plot = plot_obj,
    
    width = width,
    height = height,
    
    device = cairo_pdf,
    
    bg = "white"
  )
}

# =======================
# DADOS
# =======================

db <- read_excel(
  "SiNTESE-INDICADORES-CLUSTER-15-10.xlsx",
  sheet = "Base"
) %>%
  clean_names() %>%
  mutate(
    pais_ano = paste0(
      codigo,
      "-",
      ano
    )
  )

# =======================
# VARIÁVEIS
# =======================

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

dims <- list(
  
  Equidade1 = list(
    vars = equidade_vars,
    k = 4
  ),
  
  Seguranca2 = list(
    vars = seguranca_vars,
    k = 6
  ),
  
  Sustentabilidade3 = list(
    vars = sustent_vars,
    k = 6
  )
)

# =======================
# FUNÇÃO PRINCIPAL
# =======================

explore_dimension <- function(
    data,
    vars,
    dim_name,
    k_fixed
){
  
  out_dir <- file.path(
    "figuras_pca_cluster",
    dim_name
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  df <- data %>%
    select(
      pais_ano,
      all_of(vars)
    ) %>%
    drop_na()
  
  vars_validas <- vars[
    sapply(
      df[vars],
      function(x) sd(x)>0
    )
  ]
  
  df <- df %>%
    select(
      pais_ano,
      all_of(vars_validas)
    )
  
  X <- scale(
    df[vars_validas]
  )
  
  # PCA
  
  pca <- PCA(
    X,
    graph = FALSE
  )
  
  scores <- as.data.frame(
    pca$ind$coord
  )
  
  colnames(scores) <- paste0(
    "PC",
    1:ncol(scores)
  )
  
  scores$pais_ano <- df$pais_ano
  
  loadings <- as.data.frame(
    pca$var$coord
  )
  
  loadings$variavel <- rownames(
    loadings
  )
  
  loadings$nome_bonito <- formatar_nome(
    loadings$variavel
  )
  
  colnames(loadings)[1:ncol(X)] <- paste0(
    "PC",
    1:ncol(X)
  )
  
  # Scree
  
  var_exp <- pca$eig[,2]
  
  scree_df <- data.frame(
    PC = factor(
      paste0(
        "PC",
        1:length(var_exp)
      ),
      levels =
        paste0(
          "PC",
          1:length(var_exp)
        )
    ),
    var = var_exp
  )
  
  p_scree <- ggplot(
    scree_df,
    aes(PC,var)
  )+
    geom_bar(
      stat="identity",
      fill="#2e86c1"
    )+
    geom_line(
      aes(group=1)
    )+
    geom_point()+
    geom_text(
      aes(
        label=paste0(
          round(var,1),
          "%"
        )
      ),
      size=3.5,
      vjust=-0.3
    )+
    expand_limits(
      y=max(var_exp)*1.10
    )+
    theme_minimal(
      base_size=14
    )+
    labs(
      x="Componentes Principais",
      y="Variância Explicada (%)"
    )
  
  salvar_figura(
    p_scree,
    out_dir,
    "01_scree",
    8,
    6
  )
  
  # PCA plot
  
  p_pca <- ggplot(
    scores,
    aes(
      PC1,
      PC2
    )
  )+
    geom_point(
      color="#2e86c1"
    )+
    geom_text_repel(
      aes(label=pais_ano),
      size=3,
      show.legend=FALSE
    )+
    geom_segment(
      data=loadings,
      aes(
        x=0,
        y=0,
        xend=PC1*3,
        yend=PC2*3
      ),
      arrow=arrow(
        length=unit(
          0.25,
          "cm"
        )
      ),
      color="blue"
    )+
    geom_text_repel(
      data=loadings,
      aes(
        x=PC1*3,
        y=PC2*3,
        label=nome_bonito
      ),
      color="blue",
      show.legend=FALSE
    )+
    theme_minimal()
  
  salvar_figura(
    p_pca,
    out_dir,
    "02_pca",
    10,
    8
  )
  
  # ======================
  # MÉTRICAS CLUSTER
  # ======================
  
  k_vals <- 1:6
  k_vals <- k_vals[k_vals < nrow(X)]
  
  wss <- sil <- ch <- dbi <- rep(
    NA,
    length(k_vals)
  )
  
  for(i in seq_along(k_vals)){
    
    k <- k_vals[i]
    
    km_tmp <- kmeans(
      X,
      centers=k,
      nstart=20
    )
    
    wss[i] <- km_tmp$tot.withinss
    
    if(k>1){
      
      sil[i] <- mean(
        silhouette(
          km_tmp$cluster,
          dist(X)
        )[,3]
      )
      
      ch[i] <- index.G1(
        X,
        km_tmp$cluster
      )
      
      dbi[i] <- index.DB(
        X,
        km_tmp$cluster
      )$DB
    }
  }
  
  gap <- clusGap(
    X,
    FUN = kmeans,
    K.max = max(k_vals),
    B = 50
  )
  
  gap_df <- as.data.frame(
    gap$Tab
  )
  
  gap_df$k <- 1:nrow(gap_df)
  
  plot_k <- function(df,y,name){
    
    nomes <- list(
      wss="Soma dos Quadrados Intra-Clusters (WSS)",
      sil="Silhueta Média",
      ch="Índice de Calinski-Harabasz",
      dbi="Índice de Davies-Bouldin"
    )
    
    p <- ggplot(
      df,
      aes(
        k,
        .data[[y]]
      )
    )+
      geom_line(color="#2e86c1")+
      geom_point(color="#2e86c1")+
      scale_x_continuous(
        breaks=k_vals
      )+
      theme_minimal()+
      labs(
        x="Número de Clusters (K)",
        y=nomes[[y]]
      )
    
    salvar_figura(
      p,
      out_dir,
      name,
      8,
      6
    )
  }
  
  plot_k(data.frame(k=k_vals,wss=wss),"wss","03_elbow")
  plot_k(data.frame(k=k_vals,sil=sil),"sil","04_silhouette")
  plot_k(data.frame(k=k_vals,ch=ch),"ch","06_calinski")
  plot_k(data.frame(k=k_vals,dbi=dbi),"dbi","07_davies")
  
  p_gap <- ggplot(
    gap_df,
    aes(k,gap)
  )+
    geom_line(color="#2e86c1")+
    geom_point(color="#2e86c1")+
    theme_minimal()
  
  salvar_figura(
    p_gap,
    out_dir,
    "05_gap",
    8,
    6
  )
  
  # KMEANS
  
  cores <- c(
    "#0b3c5d",
    "#1d5fa2",
    "#2e86c1",
    "#5dade2",
    "#85c1e9",
    "#aed6f1"
  )
  
  km <- kmeans(
    X,
    centers=k_fixed,
    nstart=50
  )
  
  scores$cluster <- factor(
    km$cluster
  )
  
  p_kmeans <- ggplot(
    scores,
    aes(
      PC1,
      PC2,
      color=cluster
    )
  )+
    geom_point(size=3)+
    geom_text_repel(
      aes(label=pais_ano),
      show.legend=FALSE,
      size=3
    )+
    scale_color_manual(
      values=cores
    )+
    guides(
      color=guide_legend(
        override.aes=list(
          label=""
        )
      )
    )+
    theme_minimal()
  
  salvar_figura(
    p_kmeans,
    out_dir,
    "08_kmeans",
    10,
    8
  )
  
  # KMEDOIDS
  
  pam_fit <- pam(
    X,
    k_fixed
  )
  
  scores$cluster <- factor(
    pam_fit$clustering
  )
  
  p_kmedoids <- ggplot(
    scores,
    aes(
      PC1,
      PC2,
      color=cluster
    )
  )+
    geom_point(size=3)+
    geom_text_repel(
      aes(label=pais_ano),
      show.legend=FALSE,
      size=3
    )+
    scale_color_manual(
      values=cores
    )+
    guides(
      color=guide_legend(
        override.aes=list(
          label=""
        )
      )
    )+
    theme_minimal()
  
  salvar_figura(
    p_kmedoids,
    out_dir,
    "09_kmedoids",
    10,
    8
  )
  
  
  # ======================
  # PDF ÚNICO - 4 PÁGINAS
  # ======================
  
  CairoPDF(
    file.path(
      out_dir,
      "11_figuras_principais.pdf"
    ),
    
    width = 12,
    height = 8
  )
  
  # Página 1
  print(
    p_scree
  )
  
  # Página 2
  print(
    p_pca
  )
  
  # Página 3
  print(
    p_kmeans
  )
  
  # Página 4
  print(
    p_kmedoids
  )
  
  dev.off()
  
  # LOADINGS
  
  tabela <- cbind(
    Indicador=
      formatar_nome(
        loadings$variavel
      ),
    round(
      loadings[
        ,
        grepl(
          "^PC",
          colnames(loadings)
        )
      ],
      3
    )
  )
  
  tabela_plot <- tableGrob(
    tabela,
    rows=NULL
  )
  
  pdf(
    file.path(
      out_dir,
      "10_loadings.pdf"
    ),
    width=16,
    height=8
  )
  
  grid.draw(
    tabela_plot
  )
  
  dev.off()
  
  png(
    file.path(
      out_dir,
      "10_loadings.png"
    ),
    width=3200,
    height=1600,
    res=600
  )
  
  grid.draw(
    tabela_plot
  )
  
  dev.off()
}

# =======================
# EXECUTAR
# =======================

for(d in names(dims)){
  
  explore_dimension(
    db,
    dims[[d]]$vars,
    d,
    dims[[d]]$k
  )
}


