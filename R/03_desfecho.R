# =========================================================
# MÓDULO 03 — CONSTRUÇÃO DO DESFECHO
# =========================================================
# Objetivo:
# Construir o desfecho operacional binário de aptidão
# ao confinamento a partir de critérios zootécnicos
# parametrizáveis definidos pelo usuário.
#
# Saídas principais:
# - apto_bin
# - apto_label
# - n_criterios_atendidos
# - indicadores individuais dos critérios
#
# Aplicação:
# - Modelagem logística penalizada
# - Elastic Net
# - Stability selection
# - ROC
# - Mapas de decisão
# =========================================================
criar_desfecho <- function(
            dados,
            # ===================================================
            # Critérios mínimos
            # ===================================================
            peso_final_min,
            pcf_min,
            rcf_min,
            ecc_final_min,
            acabamento_min,
            conformacao_min,
            # ===================================================
            # Ativação dos critérios
            # ===================================================
            usar_peso_final = TRUE,
            usar_pcf = TRUE,
            usar_rcf = TRUE,
            usar_ecc_final = TRUE,
            usar_acabamento = TRUE,
            usar_conformacao = TRUE
) {
      # ===================================================
      # Verificações iniciais
      # ===================================================
      if(!is.data.frame(dados)) {
            stop(
                  "O objeto informado não é um data.frame."
            )
      }
      # ===================================================
      # Verificação das variáveis obrigatórias
      # ===================================================
      vars_obrigatorias <- c(
            "peso_final",
            "pcf_kg",
            "rcf_percent",
            "ecc_final",
            "acabamento",
            "conformacao"
      )
      vars_faltantes <- vars_obrigatorias[
            !vars_obrigatorias %in% names(dados)
      ]
      if(length(vars_faltantes) > 0) {
            stop(
                  paste(
                        "Variáveis ausentes:",
                        paste(
                              vars_faltantes,
                              collapse = ", "
                        )
                  )
            )
      }
      # ===================================================
      # Construção dos critérios individuais
      # ===================================================
      dados <- dados |>
            dplyr::mutate(
                  criterio_peso_final =
                        if (usar_peso_final) {
                              peso_final >= peso_final_min
                        } else {
                              TRUE
                        },
                  criterio_pcf =
                        if (usar_pcf) {
                              pcf_kg >= pcf_min
                        } else {
                              TRUE
                        },
                  criterio_rcf =
                        if (usar_rcf) {
                              rcf_percent*100 >= rcf_min
                        } else {
                              TRUE
                        },
                  criterio_ecc =
                        if (usar_ecc_final) {
                              ecc_final >= ecc_final_min
                        } else {
                              TRUE
                        },
                  criterio_acabamento =
                        if (usar_acabamento) {
                              acabamento >= acabamento_min
                        } else {
                              TRUE
                        },
                  criterio_conformacao =
                        if (usar_conformacao) {
                              conformacao >= conformacao_min
                        } else {
                              TRUE
                        }
            )
      # ===================================================
      # Número de critérios atendidos
      # ===================================================
      # Score operacional ordinal
      # ===================================================
      dados <- dados |>
            dplyr::rowwise() |>
            dplyr::mutate(
                  n_criterios_atendidos =
                        sum(
                              dplyr::c_across(
                                    starts_with(
                                          "criterio_"
                                    )
                              )
                        )
            ) |>
            dplyr::ungroup()
      # ===================================================
      # Construção do desfecho final
      # ===================================================
      dados <- dados |>
            dplyr::mutate(
                  apto_bin = dplyr::if_else(
                        criterio_peso_final &
                              criterio_pcf &
                              criterio_rcf &
                              criterio_ecc &
                              criterio_acabamento &
                              criterio_conformacao,
                        1L,
                        0L
                  ),
                  apto_label = dplyr::if_else(
                        apto_bin == 1L,
                        "APTO",
                        "NÃO APTO"
                  ),
                  apto_label = factor(
                        apto_label,
                        levels = c(
                              "NÃO APTO",
                              "APTO"
                        )
                  )
            )
      # ===================================================
      # Diagnóstico operacional
      # ===================================================
      resumo <- dados |>
            dplyr::count(
                  apto_label
            ) |>
            dplyr::mutate(
                  percentual =
                        round(
                              100 * n / sum(n),
                              1
                        )
            )
      message(
            "\n========== DESFECHO OPERACIONAL =========="
      )
      print(resumo)
      # ===================================================
      # Proporção de aptos
      # ===================================================
      prop_apto <- mean(
            dados$apto_bin
      )
      # ===================================================
      # Alertas de balanceamento
      # ===================================================
      if(prop_apto < 0.25) {
            warning(
                  paste(
                        "Critérios excessivamente restritivos.",
                        "Menos de 25% dos animais",
                        "foram classificados como aptos."
                  )
            )
      }
      if(prop_apto > 0.75) {
            warning(
                  paste(
                        "Critérios excessivamente permissivos.",
                        "Mais de 75% dos animais",
                        "foram classificados como aptos."
                  )
            )
      }
      # ===================================================
      # Resumo adicional
      # ===================================================
      message(
            paste(
                  "\nProporção de aptos:",
                  round(
                        100 * prop_apto,
                        1
                  ),
                  "%"
            )
      )
      message(
            paste(
                  "Média de critérios atendidos:",
                  round(
                        mean(
                              dados$n_criterios_atendidos
                        ),
                        2
                  )
            )
      )
      # ===================================================
      # Retorno
      # ===================================================
      return(dados)
}