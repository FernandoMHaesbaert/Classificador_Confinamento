# =========================================================
# TESTE — MÓDULO 09
# ROC, AUC E PERFORMANCE
# =========================================================

rm(list = ls())

gc()

cat("\014")

# ---------------------------------------------------------
# PACOTES
# ---------------------------------------------------------

source("R/00_pacotes.R")

# ---------------------------------------------------------
# MÓDULOS
# ---------------------------------------------------------

source("R/01_preprocessamento.R")

source("R/02_subgrupos.R")

source("R/03_desfecho.R")

source("R/04_elasticnet.R")

source("R/05_stability_selection.R")

source("R/06_firth.R")

source("R/07_bootstrap_inferencial.R")

source("R/08_predicao.R")

source("R/09_roc.R")

# =========================================================
# PREPROCESSAMENTO
# =========================================================

dados <- preprocessar_dados()

# =========================================================
# SUBGRUPOS
# =========================================================

subgrupos <- separar_subgrupos(dados)

machos <- subgrupos$machos

femeas <- subgrupos$femeas

# =========================================================
# DESFECHO
# =========================================================

femeas_desfecho <- criar_desfecho(
      
      dados = femeas,
      
      peso_final_min = 30,
      
      pcf_min = 14,
      
      rcf_min = 0.45,
      
      ecc_final_min = 3,
      
      acabamento_min = 3,
      
      conformacao_min = 3
)

machos_desfecho <- criar_desfecho(
      
      dados = machos,
      
      peso_final_min = 35,
      
      pcf_min = 15,
      
      rcf_min = 0.45,
      
      ecc_final_min = 3,
      
      acabamento_min = 3,
      
      conformacao_min = 3
)

# =========================================================
# VARIÁVEIS FINAIS
# =========================================================

variaveis_femeas <- c(
      
      "indice_parasitologico",
      
      "peso_inicial",
      
      "ecc_inicial"
)

variaveis_machos <- c(
      
      "peso_inicial",
      
      "grupo_genetico",
      
      "idade_fct"
)

# =========================================================
# AJUSTE FIRTH
# =========================================================

modelo_femeas <- ajustar_firth(
      
      dados = femeas_desfecho,
      
      variaveis_finais =
            variaveis_femeas
)

modelo_machos <- ajustar_firth(
      
      dados = machos_desfecho,
      
      variaveis_finais =
            variaveis_machos
)

# =========================================================
# FUNÇÃO AUXILIAR DE TESTE
# =========================================================

testar_roc <- function(
            
      modelo,
      
      dados,
      
      sexo_label
) {
      
      # ===================================================
      # CABEÇALHO
      # ===================================================
      
      message(
            "\n================================================="
      )
      
      message(
            paste(
                  "ROC E PERFORMANCE —",
                  toupper(sexo_label)
            )
      )
      
      message(
            "=================================================\n"
      )
      
      # ===================================================
      # AVALIAÇÃO ROC
      # ===================================================
      
      roc_resultados <- avaliar_roc(
            
            modelo_firth =
                  modelo$modelo,
            
            dados = dados
      )
      
      # ===================================================
      # AUC
      # ===================================================
      
      message(
            "\n========== AUC =========="
      )
      
      print(
            roc_resultados$auc
      )
      
      # ===================================================
      # CUTOFF
      # ===================================================
      
      message(
            "\n========== CUTOFF ÓTIMO =========="
      )
      
      print(
            roc_resultados$cutoff
      )
      
      # ===================================================
      # SENSIBILIDADE
      # ===================================================
      
      message(
            "\n========== SENSIBILIDADE =========="
      )
      
      print(
            roc_resultados$sensibilidade
      )
      
      # ===================================================
      # ESPECIFICIDADE
      # ===================================================
      
      message(
            "\n========== ESPECIFICIDADE =========="
      )
      
      print(
            roc_resultados$especificidade
      )
      
      # ===================================================
      # ACURÁCIA
      # ===================================================
      
      message(
            "\n========== ACURÁCIA =========="
      )
      
      print(
            roc_resultados$acuracia
      )
      
      # ===================================================
      # MATRIZ DE CONFUSÃO
      # ===================================================
      
      message(
            "\n========== MATRIZ DE CONFUSÃO =========="
      )
      
      print(
            roc_resultados$matriz_confusao
      )
      
      # ===================================================
      # CALIBRAÇÃO
      # ===================================================
      
      message(
            "\n========== CALIBRAÇÃO =========="
      )
      
      print(
            roc_resultados$calibracao
      )
      
      # ===================================================
      # PLOT ROC
      # ===================================================
      
      print(
            roc_resultados$grafico_roc
      )
      
      # ===================================================
      # PLOT CALIBRAÇÃO
      # ===================================================
      
      print(
            roc_resultados$grafico_calibracao
      )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(roc_resultados)
}

# =========================================================
# TESTE — FÊMEAS
# =========================================================

roc_femeas <- testar_roc(
      
      modelo = modelo_femeas,
      
      dados = femeas_desfecho,
      
      sexo_label = "Fêmeas"
)

# =========================================================
# TESTE — MACHOS
# =========================================================

roc_machos <- testar_roc(
      
      modelo = modelo_machos,
      
      dados = machos_desfecho,
      
      sexo_label = "Machos"
)

# =========================================================
# RESUMO FINAL
# =========================================================

message(
      "\n================================================="
)

message(
      "Teste do módulo 09 concluído com sucesso."
)

message(
      "=================================================\n"
)