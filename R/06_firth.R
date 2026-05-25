# =========================================================
# MÓDULO 06 — REGRESSÃO LOGÍSTICA DE FIRTH
# =========================================================

ajustar_firth <- function(
            dados,
            variaveis_finais,
            desfecho = "apto_bin",
            remover_correlacionadas = FALSE,
            cor_limite = 0.80
) {
      
      # ===================================================
      # VERIFICAÇÕES INICIAIS
      # ===================================================
      
      if(!is.data.frame(dados)) {
            
            stop(
                  "O objeto informado não é um data.frame."
            )
      }
      
      if(!desfecho %in% names(dados)) {
            
            stop(
                  "Variável desfecho não encontrada."
            )
      }
      
      # ===================================================
      # VARIÁVEIS EXISTENTES
      # ===================================================
      
      variaveis_validas <- variaveis_finais[
            variaveis_finais %in% names(dados)
      ]
      
      if(length(variaveis_validas) == 0) {
            
            stop(
                  "Nenhuma variável válida encontrada."
            )
      }
      
      # ===================================================
      # BASE DO MODELO
      # ===================================================
      
      dados_modelo <- dados |>
            
            dplyr::select(
                  dplyr::all_of(
                        c(
                              desfecho,
                              variaveis_validas
                        )
                  )
            ) |>
            
            tidyr::drop_na()
      
      # ===================================================
      # REMOVER VARIÁVEIS SEM VARIABILIDADE
      # ===================================================
      
      vars_validas <- sapply(
            
            dados_modelo,
            
            function(v) {
                  
                  if(is.factor(v) || is.character(v)) {
                        
                        length(unique(v)) > 1
                        
                  } else {
                        
                        stats::sd(
                              v,
                              na.rm = TRUE
                        ) > 0
                  }
            }
      )
      
      dados_modelo <- dados_modelo[ , vars_validas]
      
      # ===================================================
      # REMOVER FATORES DEGENERADOS
      # ===================================================
      
      dados_modelo <- dados_modelo |>
            
            dplyr::mutate(
                  
                  dplyr::across(
                        
                        where(is.factor),
                        
                        droplevels
                  )
            )
      
      # ===================================================
      # REDEFINIR VARIÁVEIS
      # ===================================================
      
      variaveis_modelo <- setdiff(
            names(dados_modelo),
            desfecho
      )
      
      # ===================================================
      # VERIFICAÇÃO FINAL
      # ===================================================
      
      if(length(variaveis_modelo) == 0) {
            
            stop(
                  "Nenhuma variável restante para modelagem."
            )
      }
      
      # ===================================================
      # REMOÇÃO DE CORRELAÇÃO
      # ===================================================
      
      if(remover_correlacionadas) {
            
            vars_num <- dados_modelo |>
                  
                  dplyr::select(
                        where(is.numeric)
                  ) |>
                  
                  dplyr::select(
                        -dplyr::all_of(desfecho)
                  )
            
            if(ncol(vars_num) >= 2) {
                  
                  matriz_cor <- cor(
                        vars_num,
                        use = "pairwise.complete.obs"
                  )
                  
                  remover <- caret::findCorrelation(
                        matriz_cor,
                        cutoff = cor_limite
                  )
                  
                  if(length(remover) > 0) {
                        
                        vars_remover <- names(vars_num)[remover]
                        
                        dados_modelo <- dados_modelo |>
                              
                              dplyr::select(
                                    -dplyr::all_of(
                                          vars_remover
                                    )
                              )
                        
                        variaveis_modelo <- setdiff(
                              names(dados_modelo),
                              desfecho
                        )
                  }
            }
      }
      
      # ===================================================
      # FÓRMULA
      # ===================================================
      
      formula_modelo <- as.formula(
            
            paste(
                  desfecho,
                  "~",
                  paste(
                        variaveis_modelo,
                        collapse = " + "
                  )
            )
      )
      
      # ===================================================
      # AJUSTE FIRTH
      # ===================================================
      
      modelo_firth <- logistf::logistf(
            
            formula = formula_modelo,
            
            data = dados_modelo
      )
      
      # ===================================================
      # RESULTADOS
      # ===================================================
      
      resultados <- tibble::tibble(
            
            Variavel =
                  names(modelo_firth$coefficients),
            
            Coeficiente =
                  modelo_firth$coefficients,
            
            OR =
                  exp(modelo_firth$coefficients),
            
            IC_inf =
                  exp(modelo_firth$ci.lower),
            
            IC_sup =
                  exp(modelo_firth$ci.upper),
            
            p_valor =
                  modelo_firth$prob
      ) |>
            
            dplyr::filter(
                  Variavel != "(Intercept)"
            )
      
      # ===================================================
      # PSEUDO R²
      # ===================================================
      
      ll_full <- as.numeric(
            modelo_firth$loglik["full"]
      )
      
      ll_null <- as.numeric(
            modelo_firth$loglik["null"]
      )
      
      pseudo_r2_mcfadden <-
            1 - (ll_full / ll_null)
      
      # ===================================================
      # PROBABILIDADES
      # ===================================================
      
      dados_modelo$probabilidade <- predict(
            
            modelo_firth,
            
            type = "response"
      )
      
      # ===================================================
      # CLASSIFICAÇÃO
      # ===================================================
      
      dados_modelo$classificacao <-
            
            ifelse(
                  dados_modelo$probabilidade >= 0.50,
                  "APTO",
                  "NÃO APTO"
            )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(
            
            list(
                  
                  modelo =
                        modelo_firth,
                  
                  formula =
                        formula_modelo,
                  
                  resultados =
                        resultados,
                  
                  pseudo_r2_mcfadden =
                        pseudo_r2_mcfadden,
                  
                  dados_modelo =
                        dados_modelo,
                  
                  variaveis =
                        variaveis_modelo
            )
      )
}