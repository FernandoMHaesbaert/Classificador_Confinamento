# =========================================================
# MÓDULO 04 — ELASTIC NET
# =========================================================
# Objetivo:
# Seleção estrutural de variáveis via Elastic Net
# penalizado com validação cruzada.
# =========================================================
ajustar_elasticnet <- function(
            dados,
            desfecho = "apto_bin",
            variaveis_excluir = NULL,
            alpha = 0.5,
            nfolds = 5,
            max_variaveis = 3,
            seed = 1234
) {
      # ===================================================
      # Reprodutibilidade
      # ===================================================
      set.seed(seed)
      # ===================================================
      # Verificação do desfecho
      # ===================================================
      if(!desfecho %in% names(dados)) {
            stop(
                  "Variável desfecho não encontrada."
            )
      }
      # ===================================================
      # Remover variáveis proibidas
      # ===================================================
      # Evita:
      # - data leakage;
      # - IDs;
      # - variáveis derivadas do desfecho.
      # ===================================================
      if(!is.null(variaveis_excluir)) {
            dados <- dados |>
                  dplyr::select(
                        -dplyr::any_of(
                              variaveis_excluir
                        )
                  )
      }
      # ===================================================
      # Construção da fórmula
      # ===================================================
      formula_modelo <- as.formula(
            
            paste(
                  desfecho,
                  "~ ."
            )
      )
      # ===================================================
      # Matriz do modelo
      # ===================================================
      # model.matrix:
      # - converte fatores em dummies;
      # - cria matriz numérica;
      # - remove intercepto.
      # ===================================================
      x <- model.matrix(
            formula_modelo,
            data = dados
      )[ , -1]
      # ===================================================
      # Variável resposta
      # ===================================================
      y <- dados[[desfecho]]
      # ===================================================
      # Ajuste Elastic Net
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
      # Coeficientes penalizados
      # ===================================================
      coeficientes <- coef(
            modelo_enet,
            s = "lambda.1se"
      )
      # ===================================================
      # Tabela de coeficientes
      # ===================================================
      tabela_coef <- tibble::tibble(
            Variavel = rownames(coeficientes),
            Coeficiente =
                  as.numeric(coeficientes)
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
      # Limitar número máximo de variáveis
      # ===================================================
      tabela_coef <- tabela_coef |>
            
            dplyr::slice_head(
                  n = max_variaveis
            )
      # ===================================================
      # Variáveis selecionadas
      # ===================================================
      variaveis_selecionadas <-
            tabela_coef$Variavel
      # ===================================================
      # Mensagens
      # ===================================================
      message(
            "\n========== ELASTIC NET =========="
      )
      message(
            paste(
                  "Variáveis selecionadas:",
                  length(variaveis_selecionadas)
            )
      )
      print(
            variaveis_selecionadas
      )
      # ===================================================
      # Retorno
      # ===================================================
      return(
            list(
                  modelo = modelo_enet,
                  x = x,
                  y = y,
                  variaveis =
                        variaveis_selecionadas,
                  coeficientes =
                        tabela_coef,
                  lambda_min =
                        modelo_enet$lambda.min,
                  lambda_1se =
                        modelo_enet$lambda.1se
            )
      )
}
