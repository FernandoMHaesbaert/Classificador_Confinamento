# =========================================================
# MÓDULO 12 — PLOTS
# =========================================================
# Objetivo:
# Consolidar visualizações finais do pipeline.
#
# Inclui:
# - ROC plot;
# - calibração;
# - forest plot;
# - importância relativa;
# - curvas marginais;
# - estabilidade estrutural;
# - distribuição bootstrap;
# - painel integrado.
#
# Saídas:
# - gráficos publication-ready;
# - figuras para artigo;
# - dashboard;
# - relatórios.
# =========================================================

# =========================================================
# TEMA PADRÃO
# =========================================================

tema_classificador <- function() {
      
      ggplot2::theme_minimal(
            base_size = 14
      ) +
            
            ggplot2::theme(
                  
                  plot.title =
                        ggplot2::element_text(
                              face = "bold",
                              size = 16
                        ),
                  
                  axis.title =
                        ggplot2::element_text(
                              face = "bold"
                        ),
                  
                  legend.position =
                        "bottom",
                  
                  panel.grid.minor =
                        ggplot2::element_blank()
            )
}

# =========================================================
# ROC PLOT
# =========================================================

plot_roc <- function(
            
      roc_resultado,
      
      titulo = "ROC Curve"
) {
      
      roc_obj <- roc_resultado$roc
      
      dados_roc <- tibble::tibble(
            
            sensibilidade =
                  roc_obj$sensitivities,
            
            especificidade =
                  roc_obj$specificities
      )
      
      grafico <- ggplot2::ggplot(
            
            dados_roc,
            
            ggplot2::aes(
                  
                  x = 1 - especificidade,
                  
                  y = sensibilidade
            )
      ) +
            
            ggplot2::geom_line(
                  linewidth = 1
            ) +
            
            ggplot2::geom_abline(
                  
                  slope = 1,
                  
                  intercept = 0,
                  
                  linetype = "dashed"
            ) +
            
            ggplot2::labs(
                  
                  title = titulo,
                  
                  x = "1 - Especificidade",
                  
                  y = "Sensibilidade"
            ) +
            
            ggplot2::annotate(
                  
                  "text",
                  
                  x = 0.7,
                  
                  y = 0.1,
                  
                  label = paste(
                        "AUC =",
                        round(
                              roc_resultado$auc,
                              3
                        )
                  )
            ) +
            
            tema_classificador()
      
      return(grafico)
}

# =========================================================
# CALIBRAÇÃO
# =========================================================

plot_calibracao <- function(
            
      roc_resultado,
      
      titulo = "Calibração"
) {
      
      calibracao <- roc_resultado$calibracao
      
      grafico <- ggplot2::ggplot(
            
            calibracao,
            
            ggplot2::aes(
                  
                  x = predito,
                  
                  y = observado
            )
      ) +
            
            ggplot2::geom_point(
                  size = 3
            ) +
            
            ggplot2::geom_line() +
            
            ggplot2::geom_abline(
                  
                  slope = 1,
                  
                  intercept = 0,
                  
                  linetype = "dashed"
            ) +
            
            ggplot2::labs(
                  
                  title = titulo,
                  
                  x = "Probabilidade predita",
                  
                  y = "Probabilidade observada"
            ) +
            
            tema_classificador()
      
      return(grafico)
}

# =========================================================
# FOREST PLOT
# =========================================================

plot_forest <- function(
            
      tabela_or,
      
      titulo = "Forest Plot"
) {
      
      grafico <- ggplot2::ggplot(
            
            tabela_or,
            
            ggplot2::aes(
                  
                  x = reorder(
                        Variavel,
                        OR
                  ),
                  
                  y = OR
            )
      ) +
            
            ggplot2::geom_point(
                  size = 3
            ) +
            
            ggplot2::geom_errorbar(
                  
                  ggplot2::aes(
                        
                        ymin = IC_inf,
                        
                        ymax = IC_sup
                  ),
                  
                  width = 0.2
            ) +
            
            ggplot2::geom_hline(
                  
                  yintercept = 1,
                  
                  linetype = "dashed"
            ) +
            
            ggplot2::scale_y_log10() +
            
            ggplot2::coord_flip() +
            
            ggplot2::labs(
                  
                  title = titulo,
                  
                  x = NULL,
                  
                  y = "Odds Ratio (log)"
            ) +
            
            tema_classificador()
      
      return(grafico)
}

# =========================================================
# IMPORTÂNCIA RELATIVA
# =========================================================

plot_importancia <- function(
            
      tabela_interpretacao,
      
      titulo = "Importância Relativa"
) {
      
      grafico <- ggplot2::ggplot(
            
            tabela_interpretacao,
            
            ggplot2::aes(
                  
                  x = reorder(
                        Variavel,
                        importancia_relativa
                  ),
                  
                  y = importancia_relativa
            )
      ) +
            
            ggplot2::geom_col() +
            
            ggplot2::coord_flip() +
            
            ggplot2::labs(
                  
                  title = titulo,
                  
                  x = NULL,
                  
                  y = "Importância relativa"
            ) +
            
            tema_classificador()
      
      return(grafico)
}

# =========================================================
# CURVA MARGINAL
# =========================================================

plot_predicao_marginal <- function(
            
      predicoes,
      
      variavel_alvo,
      
      titulo = "Predição marginal"
) {
      
      grafico <- ggplot2::ggplot(
            
            predicoes,
            
            ggplot2::aes_string(
                  
                  x = variavel_alvo,
                  
                  y = "prob_media"
            )
      ) +
            
            ggplot2::geom_ribbon(
                  
                  ggplot2::aes(
                        
                        ymin = IC_inf,
                        
                        ymax = IC_sup
                  ),
                  
                  alpha = 0.2
            ) +
            
            ggplot2::geom_line(
                  linewidth = 1
            ) +
            
            ggplot2::labs(
                  
                  title = titulo,
                  
                  x = variavel_alvo,
                  
                  y = "Probabilidade predita"
            ) +
            
            tema_classificador()
      
      return(grafico)
}

# =========================================================
# ESTABILIDADE ESTRUTURAL
# =========================================================

plot_estabilidade <- function(
            
      tabela_estabilidade,
      
      titulo = "Stability Selection"
) {
      
      grafico <- ggplot2::ggplot(
            
            tabela_estabilidade,
            
            ggplot2::aes(
                  
                  x = reorder(
                        Variavel,
                        Frequencia
                  ),
                  
                  y = Frequencia
            )
      ) +
            
            ggplot2::geom_col() +
            
            ggplot2::coord_flip() +
            
            ggplot2::geom_hline(
                  
                  yintercept = 0.6,
                  
                  linetype = "dashed"
            ) +
            
            ggplot2::labs(
                  
                  title = titulo,
                  
                  x = NULL,
                  
                  y = "Frequência de seleção"
            ) +
            
            tema_classificador()
      
      return(grafico)
}

# =========================================================
# DISTRIBUIÇÃO BOOTSTRAP
# =========================================================

plot_bootstrap <- function(
            
      bootstrap_resultado,
      
      variavel,
      
      titulo = NULL
) {
      
      dados <- bootstrap_resultado$or_boot |>
            
            dplyr::select(
                  dplyr::all_of(
                        variavel
                  )
            ) |>
            
            dplyr::rename(
                  OR = 1
            )
      
      if(is.null(titulo)) {
            
            titulo <- paste(
                  "Distribuição Bootstrap —",
                  variavel
            )
      }
      
      grafico <- ggplot2::ggplot(
            
            dados,
            
            ggplot2::aes(
                  x = OR
            )
      ) +
            
            ggplot2::geom_histogram(
                  
                  bins = 30
            ) +
            
            ggplot2::geom_vline(
                  
                  xintercept = 1,
                  
                  linetype = "dashed"
            ) +
            
            ggplot2::labs(
                  
                  title = titulo,
                  
                  x = "Odds Ratio",
                  
                  y = "Frequência"
            ) +
            
            tema_classificador()
      
      return(grafico)
}

# =========================================================
# EXPORTAÇÃO DE PLOTS
# =========================================================

exportar_plot <- function(
            
      grafico,
      
      caminho,
      
      largura = 8,
      
      altura = 6,
      
      dpi = 300
) {
      
      ggplot2::ggsave(
            
            filename = caminho,
            
            plot = grafico,
            
            width = largura,
            
            height = altura,
            
            dpi = dpi
      )
      
      message(
            paste(
                  "Plot exportado:",
                  caminho
            )
      )
}

# =========================================================
# PAINEL INTEGRADO
# =========================================================

painel_modelo <- function(
            
      roc_plot,
      
      forest_plot,
      
      importancia_plot,
      
      calibracao_plot
) {
      
      painel <- patchwork::wrap_plots(
            
            roc_plot,
            
            forest_plot,
            
            importancia_plot,
            
            calibracao_plot,
            
            ncol = 2
      )
      
      return(painel)
}