# =========================================================
# MÓDULO 10 — INTERPRETABILIDADE
# =========================================================
# Objetivo:
# Gerar visualizações interpretáveis
# e métricas de explicabilidade do modelo.
#
# Inclui:
# - importância relativa;
# - forest plot;
# - odds ratios plot;
# - curvas marginais integradas;
# - explainability tabular.
# =========================================================

interpretar_modelo <- function(
            
      modelo_firth,
      
      bootstrap_resultado = NULL,
      
      predicoes = NULL,
      
      titulo_modelo = "Modelo"
) {
      
      # ===================================================
      # VERIFICAÇÕES
      # ===================================================
      
      if(!inherits(modelo_firth, "logistf")) {
            
            stop(
                  "modelo_firth deve ser objeto logistf."
            )
      }
      
      # ===================================================
      # EXTRAÇÃO DOS COEFICIENTES
      # ===================================================
      
      coeficientes <- modelo_firth$coefficients
      
      variaveis <- names(coeficientes)
      
      # Remover intercepto
      
      idx <- variaveis != "(Intercept)"
      
      coeficientes <- coeficientes[idx]
      
      variaveis <- variaveis[idx]
      
      # ===================================================
      # ODDS RATIOS
      # ===================================================
      
      OR <- exp(coeficientes)
      
      IC_inf <- exp(
            modelo_firth$ci.lower[idx]
      )
      
      IC_sup <- exp(
            modelo_firth$ci.upper[idx]
      )
      
      p_valor <- modelo_firth$prob[idx]
      
      # ===================================================
      # TABELA PRINCIPAL
      # ===================================================
      
      tabela_interpretacao <- tibble::tibble(
            
            Variavel = variaveis,
            
            Coeficiente = coeficientes,
            
            OR = OR,
            
            IC_inf = IC_inf,
            
            IC_sup = IC_sup,
            
            p_valor = p_valor,
            
            Importancia =
                  abs(coeficientes)
      ) |>
            
            dplyr::arrange(
                  dplyr::desc(
                        Importancia
                  )
            )
      
      # ===================================================
      # IMPORTÂNCIA RELATIVA
      # ===================================================
      
      tabela_interpretacao <-
            tabela_interpretacao |>
            
            dplyr::mutate(
                  
                  importancia_relativa =
                        Importancia /
                        sum(Importancia)
            )
      
      # ===================================================
      # CLASSIFICAÇÃO DE EFEITO
      # ===================================================
      
      tabela_interpretacao <-
            tabela_interpretacao |>
            
            dplyr::mutate(
                  
                  direcao =
                        dplyr::if_else(
                              
                              OR > 1,
                              
                              "Aumenta aptidão",
                              
                              "Reduz aptidão"
                        )
            )
      
      # ===================================================
      # FOREST PLOT
      # ===================================================
      
      forest_plot <- ggplot2::ggplot(
            
            tabela_interpretacao,
            
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
                  
                  title = paste(
                        "Forest Plot |",
                        titulo_modelo
                  ),
                  
                  x = NULL,
                  
                  y = "Odds Ratio (escala log)"
            ) +
            
            ggplot2::theme_minimal()
      
      # ===================================================
      # IMPORTÂNCIA RELATIVA
      # ===================================================
      
      grafico_importancia <- ggplot2::ggplot(
            
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
                  
                  title = paste(
                        "Importância relativa |",
                        titulo_modelo
                  ),
                  
                  x = NULL,
                  
                  y = "Importância relativa"
            ) +
            
            ggplot2::theme_minimal()
      
      # ===================================================
      # OR PLOT
      # ===================================================
      
      or_plot <- ggplot2::ggplot(
            
            tabela_interpretacao,
            
            ggplot2::aes(
                  
                  x = reorder(
                        Variavel,
                        OR
                  ),
                  
                  y = OR
            )
      ) +
            
            ggplot2::geom_point(
                  size = 4
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
            
            ggplot2::coord_flip() +
            
            ggplot2::scale_y_log10() +
            
            ggplot2::labs(
                  
                  title = paste(
                        "Odds Ratios |",
                        titulo_modelo
                  ),
                  
                  x = NULL,
                  
                  y = "Odds Ratio"
            ) +
            
            ggplot2::theme_minimal()
      
      # ===================================================
      # EXPLAINABILITY
      # ===================================================
      
      explainability <- tabela_interpretacao |>
            
            dplyr::mutate(
                  
                  interpretacao =
                        dplyr::case_when(
                              
                              OR > 1 ~
                                    paste(
                                          Variavel,
                                          "aumenta a probabilidade de aptidão"
                                    ),
                              
                              OR < 1 ~
                                    paste(
                                          Variavel,
                                          "reduz a probabilidade de aptidão"
                                    ),
                              
                              TRUE ~
                                    "Efeito neutro"
                        )
            )
      
      # ===================================================
      # CURVAS MARGINAIS INTEGRADAS
      # ===================================================
      
      curvas_marginais <- NULL
      
      if(!is.null(predicoes)) {
            
            curvas_marginais <- ggplot2::ggplot(
                  
                  predicoes,
                  
                  ggplot2::aes(
                        
                        x = variavel_plot,
                        
                        y = prob_media
                  )
            ) +
                  
                  ggplot2::geom_line(
                        linewidth = 1
                  ) +
                  
                  ggplot2::geom_ribbon(
                        
                        ggplot2::aes(
                              
                              ymin = IC_inf,
                              
                              ymax = IC_sup
                        ),
                        
                        alpha = 0.2
                  ) +
                  
                  ggplot2::labs(
                        
                        title = paste(
                              "Curva marginal integrada |",
                              titulo_modelo
                        ),
                        
                        x = "Variável",
                        
                        y = "Probabilidade predita"
                  ) +
                  
                  ggplot2::theme_minimal()
      }
      
      # ===================================================
      # BOOTSTRAP SUMMARY
      # ===================================================
      
      resumo_bootstrap <- NULL
      
      if(!is.null(bootstrap_resultado)) {
            
            resumo_bootstrap <-
                  bootstrap_resultado$resultados
      }
      
      # ===================================================
      # MENSAGENS
      # ===================================================
      
      message(
            "\n========== INTERPRETABILIDADE =========="
      )
      
      message(
            paste(
                  "Variáveis interpretadas:",
                  nrow(
                        tabela_interpretacao
                  )
            )
      )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(
            
            list(
                  
                  tabela =
                        tabela_interpretacao,
                  
                  explainability =
                        explainability,
                  
                  forest_plot =
                        forest_plot,
                  
                  grafico_importancia =
                        grafico_importancia,
                  
                  or_plot =
                        or_plot,
                  
                  curvas_marginais =
                        curvas_marginais,
                  
                  bootstrap =
                        resumo_bootstrap
            )
      )
}