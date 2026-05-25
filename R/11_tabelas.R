# =========================================================
# MÓDULO 11 — TABELAS
# =========================================================
# Objetivo:
# Gerar tabelas padronizadas para:
# - publicação científica;
# - relatórios;
# - dashboards;
# - interpretação operacional.
#
# Inclui:
# - tabela de OR;
# - tabela bootstrap;
# - tabela ROC;
# - tabela estabilidade;
# - tabela resumo final.
# =========================================================

# =========================================================
# TABELA — ODDS RATIOS
# =========================================================

tabela_or <- function(
            
      modelo_firth,
      
      digits = 3
) {
      
      # ===================================================
      # VERIFICAÇÃO
      # ===================================================
      
      if(!inherits(modelo_firth, "logistf")) {
            
            stop(
                  "modelo_firth deve ser objeto logistf."
            )
      }
      
      # ===================================================
      # EXTRAÇÃO
      # ===================================================
      
      coeficientes <- modelo_firth$coefficients
      
      variaveis <- names(coeficientes)
      
      idx <- variaveis != "(Intercept)"
      
      tabela <- tibble::tibble(
            
            Variavel =
                  variaveis[idx],
            
            Coeficiente =
                  coeficientes[idx],
            
            OR =
                  exp(coeficientes[idx]),
            
            IC_inf =
                  exp(
                        modelo_firth$ci.lower[idx]
                  ),
            
            IC_sup =
                  exp(
                        modelo_firth$ci.upper[idx]
                  ),
            
            p_valor =
                  modelo_firth$prob[idx]
      ) |>
            
            dplyr::mutate(
                  
                  dplyr::across(
                        
                        where(is.numeric),
                        
                        ~ round(.x, digits)
                  )
            )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(tabela)
}

# =========================================================
# TABELA — BOOTSTRAP
# =========================================================

tabela_bootstrap <- function(
            
      bootstrap_resultado,
      
      digits = 3
) {
      
      # ===================================================
      # VERIFICAÇÃO
      # ===================================================
      
      if(is.null(
            bootstrap_resultado$resultados
      )) {
            
            stop(
                  "Objeto bootstrap inválido."
            )
      }
      
      # ===================================================
      # TABELA
      # ===================================================
      
      tabela <- bootstrap_resultado$resultados |>
            
            dplyr::mutate(
                  
                  dplyr::across(
                        
                        where(is.numeric),
                        
                        ~ round(.x, digits)
                  )
            )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(tabela)
}

# =========================================================
# TABELA — ROC
# =========================================================

tabela_roc <- function(
            
      roc_resultado,
      
      digits = 3
) {
      
      # ===================================================
      # VERIFICAÇÃO
      # ===================================================
      
      if(is.null(roc_resultado$auc)) {
            
            stop(
                  "Objeto ROC inválido."
            )
      }
      
      # ===================================================
      # TABELA
      # ===================================================
      
      tabela <- tibble::tibble(
            
            Metrica = c(
                  
                  "AUC",
                  
                  "Cutoff",
                  
                  "Sensibilidade",
                  
                  "Especificidade",
                  
                  "Acurácia"
            ),
            
            Valor = c(
                  
                  roc_resultado$auc,
                  
                  roc_resultado$cutoff,
                  
                  roc_resultado$sensibilidade,
                  
                  roc_resultado$especificidade,
                  
                  roc_resultado$acuracia
            )
      ) |>
            
            dplyr::mutate(
                  
                  Valor =
                        round(
                              Valor,
                              digits
                        )
            )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(tabela)
}

# =========================================================
# TABELA — STABILITY SELECTION
# =========================================================

tabela_estabilidade <- function(
            
      stability_resultado,
      
      digits = 3
) {
      
      # ===================================================
      # VERIFICAÇÃO
      # ===================================================
      
      if(is.null(
            stability_resultado$tabela_final
      )) {
            
            stop(
                  "Objeto stability inválido."
            )
      }
      
      # ===================================================
      # TABELA
      # ===================================================
      
      tabela <- stability_resultado$tabela_final |>
            
            dplyr::mutate(
                  
                  dplyr::across(
                        
                        where(is.numeric),
                        
                        ~ round(.x, digits)
                  )
            )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(tabela)
}

# =========================================================
# TABELA — RESUMO FINAL
# =========================================================

tabela_resumo_modelo <- function(
            
      modelo_firth,
      
      roc_resultado,
      
      bootstrap_resultado = NULL,
      
      titulo_modelo = "Modelo"
) {
      
      # ===================================================
      # NÚMERO DE VARIÁVEIS
      # ===================================================
      
      n_variaveis <-
            length(
                  coef(
                        modelo_firth
                  )
            ) - 1
      
      # ===================================================
      # PSEUDO R²
      # ===================================================
      
      ll_full <- as.numeric(
            modelo_firth$loglik["full"]
      )
      
      ll_null <- as.numeric(
            modelo_firth$loglik["null"]
      )
      
      pseudo_r2 <- 1 - (
            ll_full / ll_null
      )
      
      # ===================================================
      # TABELA BASE
      # ===================================================
      
      tabela <- tibble::tibble(
            
            Indicador = c(
                  
                  "Modelo",
                  
                  "Variáveis",
                  
                  "Pseudo R²",
                  
                  "AUC",
                  
                  "Sensibilidade",
                  
                  "Especificidade",
                  
                  "Acurácia"
            ),
            
            Valor = as.character(
                  c(
                        
                        titulo_modelo,
                        
                        n_variaveis,
                        
                        round(
                              pseudo_r2,
                              3
                        ),
                        
                        round(
                              roc_resultado$auc,
                              3
                        ),
                        
                        round(
                              roc_resultado$sensibilidade,
                              3
                        ),
                        
                        round(
                              roc_resultado$especificidade,
                              3
                        ),
                        
                        round(
                              roc_resultado$acuracia,
                              3
                        )
                  )
            )
      )
      
      # ===================================================
      # BOOTSTRAP MÉDIO
      # ===================================================
      
      if(!is.null(bootstrap_resultado)) {
            
            media_estabilidade <-
                  mean(
                        bootstrap_resultado$resultados$IC_sup -
                              bootstrap_resultado$resultados$IC_inf,
                        na.rm = TRUE
                  )
            
            tabela <- dplyr::bind_rows(
                  
                  tabela,
                  
                  tibble::tibble(
                        
                        Indicador =
                              "Amplitude média IC bootstrap",
                        
                        Valor =
                              as.character(
                                    round(
                                          media_estabilidade,
                                          3
                                    )
                              )
                  )
            )
      }
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(tabela)
}

# =========================================================
# EXPORTAÇÃO OPCIONAL
# =========================================================

exportar_tabela_csv <- function(
            
      tabela,
      
      caminho
) {
      
      readr::write_csv(
            
            tabela,
            
            file = caminho
      )
      
      message(
            paste(
                  "Tabela exportada para:",
                  caminho
            )
      )
}