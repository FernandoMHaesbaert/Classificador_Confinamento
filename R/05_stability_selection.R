# =========================================================
# MÓDULO 05 — STABILITY SELECTION
# =========================================================
# Objetivo:
# Avaliar robustez estrutural das variáveis via Bootstrap Stability Selection utilizando Elastic Net penalizado.
#
# Estratégia:
# 1. Reamostragem bootstrap;
# 2. Ajuste Elastic Net em cada amostra;
# 3. Registro das variáveis selecionadas;
# 4. Frequência de seleção;
# 5. Classificação da estabilidade;
# 6. Definição das variáveis robustas finais.
#
# Saídas principais:
# - frequência de seleção;
# - estabilidade;
# - variáveis robustas finais.
#
# IMPORTANTE:
# Este módulo DEFINE as variáveis que irão para o modelo final.
# =========================================================
executar_stability_selection <- function(
      dados,
      desfecho = "apto_bin",
      alpha = 0.5,
      nfolds = 3,
      n_boot = 1000,
      limiar_frequencia = 0.50,
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
            stop("O objeto informado não é um data.frame.")
      }
      # ===================================================
      # VERIFICAÇÃO DO DESFECHO
      # ===================================================
      if(!desfecho %in% names(dados)) {
            stop("Variável desfecho não encontrada.")
      }
      # ===================================================
      # VARIÁVEIS PREDITORAS
      # ===================================================
      # Apenas variáveis disponíveis antes
      # do confinamento.
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
      vars_existentes <- c(desfecho, variaveis_modelo)
      vars_existentes <- vars_existentes[vars_existentes %in% names(dados)]
      # ===================================================
      # Construção da base
      # ===================================================
      dados_modelo <- dados |>
            dplyr::select(
                  dplyr::all_of(
                        vars_existentes)) |>
            tidyr::drop_na()
      # ===================================================
      # Verificação pós-NA
      # ===================================================
      if(nrow(dados_modelo) < 10) {
            stop("Número insuficiente de observações.")
      }
      # ===================================================
      # Variável resposta
      # ===================================================
      y <- dados_modelo[[desfecho]]
      # ===================================================
      # Verificação binária
      # ===================================================
      if(length(unique(y)) != 2) {
            stop("O desfecho deve possuir duas classes.")
      }
      # ===================================================
      # Menor frequência de classe
      # ===================================================
      n_min_classe <- min(table(y))
      # ===================================================
      # Verificação crítica
      # ===================================================
      if(n_min_classe <= 2) {
            stop(paste("Número insuficiente de observações","por classe."))
      }
      # ===================================================
      # Ajuste automático dos folds
      # ===================================================
      if(n_min_classe < nfolds) {
            nfolds <- max(2, n_min_classe)
            warning(
                  paste("nfolds reajustado para", nfolds))
      }
      # ===================================================
      # Base preditora
      # ===================================================
      dados_x <- dados_modelo |>
            dplyr::select(-dplyr::all_of(desfecho))
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
            stop("Nenhuma variável válida restante.")
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
      # MATRIZ DE SELEÇÃO
      # ===================================================
      selecoes <- matrix(0, nrow = n_boot, ncol = ncol(x))
      colnames(selecoes) <- colnames(x)
      # ===================================================
      # CONTADORES
      # ===================================================
      modelos_validos <- 0
      modelos_invalidos <- 0
      # ===================================================
      # MENSAGEM INICIAL
      # ===================================================
      message("\n========== STABILITY SELECTION ==========")
      message(paste("Observações:", nrow(x)))
      message(paste("Preditores:", ncol(x)))
      message(paste("Bootstrap:", n_boot))
      # ===================================================
      # LOOP PRINCIPAL
      # ===================================================
      for(i in 1:n_boot) {
            # -----------------------------------------------
            # Bootstrap
            # -----------------------------------------------
            idx <- sample(
                  1:nrow(x),
                  size = floor(0.8 * nrow(x)),
                  replace = FALSE
            )
            x_boot <- x[idx, ]
            y_boot <- y[idx]
            # -----------------------------------------------
            # Verificação das classes
            # -----------------------------------------------
            if(length(unique(y_boot)) < 2) {
                  modelos_invalidos <-
                        modelos_invalidos + 1
                  next
            }
            # -----------------------------------------------
            # Ajuste Elastic Net
            # -----------------------------------------------
            modelo_boot <- tryCatch(
                  suppressWarnings(
                        glmnet::cv.glmnet(
                              x = x_boot,
                              y = y_boot,
                              family = "binomial",
                              alpha = alpha,
                              nfolds = nfolds,
                              type.measure = "deviance",
                              standardize = TRUE
                        )
                  ),
                  error = function(e) NULL
            )
            # -----------------------------------------------
            # Falha do modelo
            # -----------------------------------------------
            if(is.null(modelo_boot)) {
                  modelos_invalidos <-
                        modelos_invalidos + 1
                  next
            }
            modelos_validos <-
                  modelos_validos + 1
            # -----------------------------------------------
            # Escolha do lambda
            # -----------------------------------------------
            lambda_escolhido <- ifelse(
                  usar_lambda_1se,
                  "lambda.1se",
                  "lambda.min"
            )
            # -----------------------------------------------
            # Coeficientes
            # -----------------------------------------------
            coef_boot <- coef(
                  modelo_boot,
                  s = lambda_escolhido
            )
            # -----------------------------------------------
            # Variáveis selecionadas
            # -----------------------------------------------
            vars_selecionadas <- rownames(
                  coef_boot
            )[
                  which(
                        coef_boot[, 1] != 0
                  )
            ]
            # -----------------------------------------------
            # Remover intercepto
            # -----------------------------------------------
            vars_selecionadas <- setdiff(
                  vars_selecionadas,
                  "(Intercept)"
            )
            # -----------------------------------------------
            # Registrar seleção
            # -----------------------------------------------
            selecoes[
                  i,
                  colnames(selecoes) %in%
                        vars_selecionadas
            ] <- 1
      }
      # ===================================================
      # VERIFICAÇÃO FINAL
      # ===================================================
      if(modelos_validos == 0) {
            stop("Nenhum modelo bootstrap válido.")
      }
      # ===================================================
      # FREQUÊNCIA DE SELEÇÃO
      # ===================================================
      freq_selecao <- tibble::tibble(
            Variavel = colnames(selecoes),
            Frequencia = colMeans(selecoes),
            Frequencia_percentual = round(Frequencia * 100, 1)) |>
            dplyr::arrange(dplyr::desc(Frequencia))
      # ===================================================
      # CLASSIFICAÇÃO DA ESTABILIDADE
      # ===================================================
      freq_selecao <- freq_selecao |>
            dplyr::mutate(
                  Estabilidade = dplyr::case_when(
                        Frequencia >= 0.95 ~ "Muito alta",
                        Frequencia >= 0.80 ~ "Alta",
                        Frequencia >= 0.65 ~ "Moderada",
                        Frequencia >= 0.50 ~ "Baixa",
                        TRUE ~ "Muito baixa"
                  )
            )
      # ===================================================
      # VARIÁVEIS ROBUSTAS FINAIS
      # ===================================================
      variaveis_finais <- freq_selecao |>
            dplyr::filter(
                  Frequencia >=
                        limiar_frequencia
            ) |>
            dplyr::pull(
                  Variavel
            )
      # ===================================================
      # RESUMO FINAL
      # ===================================================
      message("\n========== RESULTADO FINAL ==========")
      message(paste("Modelos válidos:", modelos_validos))
      message(paste("Modelos inválidos:", modelos_invalidos))
      message(paste("Variáveis robustas:", length(variaveis_finais)))
      print(variaveis_finais)
      # ===================================================
      # RETORNO
      # ===================================================
      return(list(
                  tabela_final = freq_selecao,
                  variaveis_finais = variaveis_finais,
                  selecoes = selecoes,
                  modelos_validos = modelos_validos,
                  modelos_invalidos = modelos_invalidos,
                  x = x,
                  y = y,
                  alpha = alpha,
                  nfolds = nfolds,
                  n_boot = n_boot,
                  limiar_frequencia = limiar_frequencia))
}