# =========================================================
# MÓDULO 04 — ELASTIC NET
# =========================================================
# Objetivo:
# Realizar seleção preliminar de variáveis
# via Elastic Net penalizado.
#
# Finalidade:
# - exploração estrutural;
# - redução inicial de dimensionalidade;
# - inspeção de variáveis relevantes;
# - preparação para Stability Selection.
#
# IMPORTANTE:
# Este módulo NÃO define as variáveis finais.
# A robustez estrutural será avaliada no:
#
# 05_stability_selection.R
# =========================================================

ajustar_elasticnet <- function(
            
      dados,
      
      desfecho = "apto_bin",
      
      alpha = 0.5,
      
      nfolds = 5,
      
      usar_lambda_1se = TRUE,
      
      seed = 1234
) {
      
      # ===================================================
      # REPRODUTIBILIDADE
      # ===================================================
      
      set.seed(seed)
      
      # ===================================================
      # VERIFICAÇÃO DO DATAFRAME
      # ===================================================
      
      if(!is.data.frame(dados)) {
            
            stop(
                  "O objeto informado não é um data.frame."
            )
      }
      
      # ===================================================
      # VERIFICAÇÃO DO DESFECHO
      # ===================================================
      
      if(!desfecho %in% names(dados)) {
            
            stop(
                  "Variável desfecho não encontrada."
            )
      }
      
      # ===================================================
      # VARIÁVEIS PREDITORAS
      # ===================================================
      # Apenas variáveis disponíveis
      # antes do confinamento.
      #
      # Evita:
      # - data leakage;
      # - circularidade;
      # - uso de variáveis pós-desempenho.
      # ===================================================
      
      variaveis_modelo <- c(
            
            "peso_inicial",
            
            "ecc_inicial",
            
            "idade_fct",
            
            "grupo_genetico",
            
            "famacha_ini",
            
            "log_opg",
            
            "log_oopg",
            
            "indice_parasitologico"
      )
      
      # ===================================================
      # Variáveis existentes
      # ===================================================
      
      vars_existentes <- c(
            desfecho,
            variaveis_modelo
      )
      
      vars_existentes <- vars_existentes[
            vars_existentes %in% names(dados)
      ]
      
      # ===================================================
      # Construção da base
      # ===================================================
      
      dados_modelo <- dados |>
            
            dplyr::select(
                  dplyr::all_of(
                        vars_existentes
                  )
            ) |>
            
            tidyr::drop_na()
      
      # ===================================================
      # Verificação pós-NA
      # ===================================================
      
      if(nrow(dados_modelo) < 10) {
            
            stop(
                  "Número insuficiente de observações."
            )
      }
      
      # ===================================================
      # Variável resposta
      # ===================================================
      
      y <- dados_modelo[[desfecho]]
      
      # ===================================================
      # Verificação binária
      # ===================================================
      
      if(length(unique(y)) != 2) {
            
            stop(
                  "O desfecho deve possuir duas classes."
            )
      }
      
      # ===================================================
      # Ajuste automático dos folds
      # ===================================================
      
      if(nrow(dados_modelo) < 30) {
            
            nfolds <- 3
            
            warning(
                  paste(
                        "Base pequena detectada.",
                        "nfolds ajustado automaticamente para 3."
                  )
            )
      }
      
      # ===================================================
      # Menor frequência de classe
      # ===================================================
      
      n_min_classe <- min(table(y))
      
      # ===================================================
      # Verificação crítica
      # ===================================================
      
      if(n_min_classe <= 2) {
            
            stop(
                  paste(
                        "Número insuficiente de observações",
                        "por classe."
                  )
            )
      }
      
      # ===================================================
      # Ajuste final dos folds
      # ===================================================
      
      if(n_min_classe < nfolds) {
            
            nfolds <- max(
                  2,
                  n_min_classe
            )
            
            warning(
                  paste(
                        "nfolds reajustado para",
                        nfolds
                  )
            )
      }
      
      # ===================================================
      # Base preditora
      # ===================================================
      
      dados_x <- dados_modelo |>
            
            dplyr::select(
                  -dplyr::all_of(
                        desfecho
                  )
            )
      
      # ===================================================
      # Remover variáveis sem variabilidade
      # ===================================================
      
      vars_validas <- sapply(
            
            dados_x,
            
            function(v) {
                  
                  length(
                        unique(
                              stats::na.omit(v)
                        )
                  ) > 1
            }
      )
      
      dados_x <- dados_x[ , vars_validas]
      
      # ===================================================
      # Verificação final
      # ===================================================
      
      if(ncol(dados_x) == 0) {
            
            stop(
                  "Nenhuma variável válida restante."
            )
      }
      
      # ===================================================
      # MATRIZ PREDITORA
      # ===================================================
      
      x <- model.matrix(
            ~ .,
            data = dados_x
      )
      
      # ===================================================
      # Remover intercepto
      # ===================================================
      
      x <- x[ , -1]
      
      # ===================================================
      # Garantir matriz numérica
      # ===================================================
      
      x <- as.matrix(x)
      
      # ===================================================
      # AJUSTE ELASTIC NET
      # ===================================================
      
      modelo_enet <- glmnet::cv.glmnet(
            
            x = x,
            
            y = y,
            
            family = "binomial",
            
            alpha = alpha,
            
            nfolds = nfolds,
            
            type.measure = "deviance",
            
            standardize = TRUE
      )
      
      # ===================================================
      # Escolha do lambda
      # ===================================================
      
      lambda_escolhido <- ifelse(
            
            usar_lambda_1se,
            
            "lambda.1se",
            
            "lambda.min"
      )
      
      # ===================================================
      # Coeficientes penalizados
      # ===================================================
      
      coeficientes <- coef(
            
            modelo_enet,
            
            s = lambda_escolhido
      )
      
      # ===================================================
      # Tabela de coeficientes
      # ===================================================
      
      tabela_coef <- tibble::tibble(
            
            Variavel = rownames(coeficientes),
            
            Coeficiente =
                  as.numeric(
                        coeficientes
                  )
      ) |>
            
            dplyr::filter(
                  Variavel != "(Intercept)"
            ) |>
            
            dplyr::mutate(
                  abs_coef =
                        abs(Coeficiente)
            ) |>
            
            dplyr::filter(
                  Coeficiente != 0
            ) |>
            
            dplyr::arrange(
                  dplyr::desc(abs_coef)
            )
      
      # ===================================================
      # Variáveis selecionadas
      # ===================================================
      
      variaveis_selecionadas <-
            tabela_coef$Variavel
      
      # ===================================================
      # MENSAGENS
      # ===================================================
      
      message(
            "\n========== ELASTIC NET =========="
      )
      
      message(
            paste(
                  "Observações:",
                  nrow(x)
            )
      )
      
      message(
            paste(
                  "Preditores:",
                  ncol(x)
            )
      )
      
      message(
            paste(
                  "Variáveis selecionadas:",
                  length(
                        variaveis_selecionadas
                  )
            )
      )
      
      message(
            paste(
                  "Lambda utilizado:",
                  lambda_escolhido
            )
      )
      
      # ===================================================
      # Variáveis selecionadas
      # ===================================================
      
      print(
            variaveis_selecionadas
      )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(
            
            list(
                  
                  modelo =
                        modelo_enet,
                  
                  x = x,
                  
                  y = y,
                  
                  coeficientes =
                        tabela_coef,
                  
                  variaveis =
                        variaveis_selecionadas,
                  
                  lambda_utilizado =
                        lambda_escolhido,
                  
                  lambda_min =
                        modelo_enet$lambda.min,
                  
                  lambda_1se =
                        modelo_enet$lambda.1se,
                  
                  alpha = alpha,
                  
                  nfolds = nfolds,
                  
                  n_observacoes =
                        nrow(x),
                  
                  n_preditores =
                        ncol(x)
            )
      )
}