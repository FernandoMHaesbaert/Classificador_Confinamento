# =========================================================
# MÓDULO 02 — CONSTRUÇÃO DO DESFECHO
# =========================================================
# Objetivo:
# Construir o desfecho binário operacional de aptidão
# ao confinamento com critérios parametrizáveis.
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
            # Ativar/desativar critérios
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
      # Inicialização
      # ===================================================
      dados <- dados |>
            dplyr::mutate(
                  criterio_peso_final = ifelse(
                        usar_peso_final,
                        peso_final >= peso_final_min,
                        TRUE
                  ),
                  criterio_pcf = ifelse(
                        usar_pcf,
                        pcf >= pcf_min,
                        TRUE
                  ),
                  criterio_rcf = ifelse(
                        usar_rcf,
                        rcf_percentual >= rcf_min,
                        TRUE
                  ),
                  criterio_ecc = ifelse(
                        usar_ecc_final,
                        ecc_final >= ecc_final_min,
                        TRUE
                  ),
                  criterio_acabamento = ifelse(
                        usar_acabamento,
                        acabamento >= acabamento_min,
                        TRUE
                  ),
                  criterio_conformacao = ifelse(
                        usar_conformacao,
                        conformacao >= conformacao_min,
                        TRUE
                  )
            )
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
                  )
            )
      # ===================================================
      # Diagnóstico do desfecho
      # ===================================================
      resumo <- dados |>
            dplyr::count(apto_bin) |>
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
      # Alerta para classes raras
      # ===================================================
      prop_apto <- mean(dados$apto_bin)
      if(prop_apto < 0.25) {
            warning(
                  paste(
                        "Critérios excessivamente restritivos.",
                        "Menos de 25% dos animais classificados",
                        "como aptos."
                  )
            )
      }
      if(prop_apto > 0.75) {
            warning(
                  paste(
                        "Critérios excessivamente permissivos.",
                        "Mais de 75% dos animais classificados",
                        "como aptos."
                  )
            )
      }
      # ===================================================
      # Retorno
      # ===================================================
      return(dados)
}