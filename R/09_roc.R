# =========================================================
# MÓDULO 09 — ROC E PERFORMANCE
# =========================================================
# Objetivo:
# Avaliar performance discriminatória
# do modelo logístico de Firth.
#
# Responsabilidades:
# - Curva ROC;
# - AUC;
# - cutoff ótimo;
# - sensibilidade;
# - especificidade;
# - matriz de confusão;
# - calibração.
# =========================================================

avaliar_roc <- function(
            
      modelo_firth,
      
      dados,
      
      desfecho = "apto_bin",
      
      cutoff = NULL
) {
      
      # ===================================================
      # VERIFICAÇÕES
      # ===================================================
      
      if(!inherits(modelo_firth, "logistf")) {
            
            stop(
                  "modelo_firth deve ser objeto logistf."
            )
      }
      
      if(!desfecho %in% names(dados)) {
            
            stop(
                  "Variável desfecho não encontrada."
            )
      }
      
      # ===================================================
      # PROBABILIDADES PREDITAS
      # ===================================================
      
      probabilidades <- predict(
            
            modelo_firth,
            
            type = "response"
      )
      
      # ===================================================
      # DESFECHO OBSERVADO
      # ===================================================
      
      y_true <- dados[[desfecho]]
      
      # ===================================================
      # CURVA ROC
      # ===================================================
      
      roc_obj <- pROC::roc(
            
            response = y_true,
            
            predictor = probabilidades,
            
            quiet = TRUE
      )
      
      # ===================================================
      # AUC
      # ===================================================
      
      auc_valor <- as.numeric(
            pROC::auc(roc_obj)
      )
      
      # ===================================================
      # CUTOFF ÓTIMO
      # ===================================================
      # Critério de Youden
      # ===================================================
      
      if(is.null(cutoff)) {
            
            melhor_cutoff <- pROC::coords(
                  
                  roc_obj,
                  
                  x = "best",
                  
                  best.method = "youden",
                  
                  transpose = FALSE
            )
            
            cutoff <- melhor_cutoff$threshold
            
      } else {
            
            melhor_cutoff <- pROC::coords(
                  
                  roc_obj,
                  
                  x = cutoff,
                  
                  input = "threshold",
                  
                  transpose = FALSE
            )
      }
      
      # ===================================================
      # CLASSIFICAÇÃO FINAL
      # ===================================================
      
      classificacao <- ifelse(
            
            probabilidades >= cutoff,
            
            1,
            
            0
      )
      
      # ===================================================
      # MATRIZ DE CONFUSÃO
      # ===================================================
      
      matriz_confusao <- table(
            
            Observado = y_true,
            
            Predito = classificacao
      )
      
      # ===================================================
      # SENSIBILIDADE
      # ===================================================
      
      sensibilidade <- melhor_cutoff$sensitivity
      
      # ===================================================
      # ESPECIFICIDADE
      # ===================================================
      
      especificidade <- melhor_cutoff$specificity
      
      # ===================================================
      # ACURÁCIA
      # ===================================================
      
      acuracia <- mean(
            classificacao == y_true
      )
      
      # ===================================================
      # CALIBRAÇÃO
      # ===================================================
      
      dados_calibracao <- tibble::tibble(
            
            observado = y_true,
            
            predito = probabilidades
      ) |>
            
            dplyr::mutate(
                  
                  decil = dplyr::ntile(
                        predito,
                        10
                  )
            ) |>
            
            dplyr::group_by(
                  decil
            ) |>
            
            dplyr::summarise(
                  
                  observado =
                        mean(observado),
                  
                  predito =
                        mean(predito),
                  
                  .groups = "drop"
            )
      
      # ===================================================
      # GRÁFICO ROC
      # ===================================================
      
      grafico_roc <- ggplot2::ggplot(
            
            data.frame(
                  sens = roc_obj$sensitivities,
                  spec = roc_obj$specificities
            ),
            
            ggplot2::aes(
                  x = 1 - spec,
                  y = sens
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
                  
                  title = paste(
                        "Curva ROC | AUC =",
                        round(auc_valor, 3)
                  ),
                  
                  x = "1 - Especificidade",
                  
                  y = "Sensibilidade"
            ) +
            
            ggplot2::theme_minimal()
      
      # ===================================================
      # GRÁFICO CALIBRAÇÃO
      # ===================================================
      
      grafico_calibracao <- ggplot2::ggplot(
            
            dados_calibracao,
            
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
                  
                  title = "Calibração do modelo",
                  
                  x = "Probabilidade predita",
                  
                  y = "Probabilidade observada"
            ) +
            
            ggplot2::theme_minimal()
      
      # ===================================================
      # RESULTADOS
      # ===================================================
      
      resultados <- list(
            
            roc = roc_obj,
            
            auc = auc_valor,
            
            cutoff = cutoff,
            
            sensibilidade = sensibilidade,
            
            especificidade = especificidade,
            
            acuracia = acuracia,
            
            matriz_confusao = matriz_confusao,
            
            probabilidades = probabilidades,
            
            classificacao = classificacao,
            
            calibracao = dados_calibracao,
            
            grafico_roc = grafico_roc,
            
            grafico_calibracao =
                  grafico_calibracao
      )
      
      # ===================================================
      # MENSAGENS
      # ===================================================
      
      message(
            "\n========== PERFORMANCE ROC =========="
      )
      
      message(
            paste(
                  "AUC:",
                  round(auc_valor, 3)
            )
      )
      
      message(
            paste(
                  "Cutoff ótimo:",
                  round(cutoff, 3)
            )
      )
      
      message(
            paste(
                  "Sensibilidade:",
                  round(sensibilidade, 3)
            )
      )
      
      message(
            paste(
                  "Especificidade:",
                  round(especificidade, 3)
            )
      )
      
      message(
            paste(
                  "Acurácia:",
                  round(acuracia, 3)
            )
      )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(resultados)
}