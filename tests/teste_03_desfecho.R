# =========================================================
# TESTE — MÓDULO 03
# Construção do desfecho operacional
# =========================================================

rm(list = ls())
gc()
cat("\014")

# ---------------------------------------------------------
# Carregar pacotes
# ---------------------------------------------------------
source("R/00_pacotes.R")

# ---------------------------------------------------------
# Carregar módulos
# ---------------------------------------------------------
source("R/01_preprocessamento.R")
source("R/02_subgrupos.R")
source("R/03_desfecho.R")

# =========================================================
# PREPROCESSAMENTO
# =========================================================

dados <- preprocessar_dados()

# =========================================================
# SUBGRUPOS
# =========================================================

subgrupos <- separar_subgrupos(dados)

machos  <- subgrupos$machos
femeas  <- subgrupos$femeas

# =========================================================
# FUNÇÃO AUXILIAR DE TESTE
# =========================================================

testar_desfecho <- function(
            dados,
            sexo_label,
            peso_final_min,
            pcf_min,
            rcf_min,
            ecc_final_min,
            acabamento_min,
            conformacao_min
) {
      
      # ===================================================
      # Construção do desfecho
      # ===================================================
      
      dados_desfecho <- criar_desfecho(
            
            dados = dados,
            
            # -----------------------------------------------
            # Critérios mínimos
            # -----------------------------------------------
            
            peso_final_min = peso_final_min,
            pcf_min = pcf_min,
            rcf_min = rcf_min,
            ecc_final_min = ecc_final_min,
            acabamento_min = acabamento_min,
            conformacao_min = conformacao_min,
            
            # -----------------------------------------------
            # Critérios ativos
            # -----------------------------------------------
            
            usar_peso_final = TRUE,
            usar_pcf = TRUE,
            usar_rcf = TRUE,
            usar_ecc_final = TRUE,
            usar_acabamento = TRUE,
            usar_conformacao = TRUE
      )
      
      # ===================================================
      # CABEÇALHO
      # ===================================================
      
      message(
            paste0(
                  "\n========== TESTE DESFECHO — ",
                  toupper(sexo_label),
                  " =========="
            )
      )
      
      # ===================================================
      # DISTRIBUIÇÃO DO DESFECHO
      # ===================================================
      
      message("\n--- Distribuição do desfecho ---")
      
      print(
            table(
                  dados_desfecho$apto_label
            )
      )
      
      # ===================================================
      # PERCENTUAIS
      # ===================================================
      
      message("\n--- Percentuais ---")
      
      print(
            round(
                  prop.table(
                        table(
                              dados_desfecho$apto_label
                        )
                  ) * 100,
                  1
            )
      )
      
      # ===================================================
      # CRITÉRIOS ATENDIDOS
      # ===================================================
      
      message("\n--- Número de critérios atendidos ---")
      
      print(
            summary(
                  dados_desfecho$n_criterios_atendidos
            )
      )
      
      # ===================================================
      # CONSISTÊNCIA DOS APTOS
      # ===================================================
      
      message("\n--- Verificação de consistência ---")
      
      checagem <- dados_desfecho |>
            
            dplyr::filter(
                  apto_bin == 1
            ) |>
            
            dplyr::summarise(
                  
                  n_apto = dplyr::n(),
                  
                  min_criterios =
                        min(
                              n_criterios_atendidos,
                              na.rm = TRUE
                        ),
                  
                  max_criterios =
                        max(
                              n_criterios_atendidos,
                              na.rm = TRUE
                        )
            )
      
      print(checagem)
      
      # ===================================================
      # TABELA OPERACIONAL
      # ===================================================
      
      message("\n--- Tabela operacional ---")
      
      tabela_operacional <- dados_desfecho |>
            
            dplyr::select(
                  animal_id,
                  apto_bin,
                  apto_label,
                  n_criterios_atendidos,
                  starts_with("criterio_")
            )
      
      print(
            head(
                  tabela_operacional,
                  10
            )
      )
      
      # ===================================================
      # GLIMPSE
      # ===================================================
      
      message("\n--- Estrutura final ---")
      
      dplyr::glimpse(
            dados_desfecho
      )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(dados_desfecho)
}

# =========================================================
# TESTE — FÊMEAS
# =========================================================

femeas_desfecho <- testar_desfecho(
      
      dados = femeas,
      
      sexo_label = "Fêmeas",
      
      peso_final_min = 30,
      pcf_min = 14,
      rcf_min = 0.45,
      ecc_final_min = 3,
      acabamento_min = 3,
      conformacao_min = 3
)

# =========================================================
# TESTE — MACHOS
# =========================================================

machos_desfecho <- testar_desfecho(
      
      dados = machos,
      
      sexo_label = "Machos",
      
      peso_final_min = 35,
      pcf_min = 15,
      rcf_min = 0.45,
      ecc_final_min = 3,
      acabamento_min = 3,
      conformacao_min = 3
)

# =========================================================
# RESUMO FINAL
# =========================================================

message(
      "\n================================================="
)

message(
      "Teste do módulo 03 concluído com sucesso."
)

message(
      "=================================================\n"
)

