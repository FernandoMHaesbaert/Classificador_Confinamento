# =========================================================
# TESTE — MÓDULO 06
# REGRESSÃO LOGÍSTICA DE FIRTH
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
# FUNÇÃO AUXILIAR DE TESTE
# =========================================================

testar_firth <- function(
            dados,
            variaveis,
            sexo_label
) {
      
      message(
            "\n================================================="
      )
      
      message(
            paste(
                  "MODELO FIRTH —",
                  toupper(sexo_label)
            )
      )
      
      message(
            "=================================================\n"
      )
      
      # ---------------------------------------------------
      # DISTRIBUIÇÃO DO DESFECHO
      # ---------------------------------------------------
      
      message(
            "--- Distribuição do desfecho ---"
      )
      
      print(
            table(
                  dados$apto_label
            )
      )
      
      # ---------------------------------------------------
      # AJUSTE
      # ---------------------------------------------------
      
      modelo <- ajustar_firth(
            
            dados = dados,
            
            variaveis_finais = variaveis
      )
      
      # ---------------------------------------------------
      # FÓRMULA
      # ---------------------------------------------------
      
      message(
            "\n--- Fórmula do modelo ---"
      )
      
      print(
            modelo$formula
      )
      
      # ---------------------------------------------------
      # VARIÁVEIS UTILIZADAS
      # ---------------------------------------------------
      
      message(
            "\n--- Variáveis utilizadas ---"
      )
      
      print(
            modelo$variaveis
      )
      
      # ---------------------------------------------------
      # RESULTADOS
      # ---------------------------------------------------
      
      message(
            "\n========== ODDS RATIOS =========="
      )
      
      print(
            modelo$resultados
      )
      
      # ---------------------------------------------------
      # PSEUDO R²
      # ---------------------------------------------------
      
      message(
            "\n========== PSEUDO R² =========="
      )
      
      print(
            round(
                  modelo$pseudo_r2_mcfadden,
                  4
            )
      )
      
      # ---------------------------------------------------
      # PROBABILIDADES
      # ---------------------------------------------------
      
      message(
            "\n========== PROBABILIDADES =========="
      )
      
      print(
            head(
                  modelo$dados_modelo |>
                        
                        dplyr::select(
                              probabilidade,
                              classificacao
                        ),
                  10
            )
      )
      
      # ---------------------------------------------------
      # HISTOGRAMA DAS PROBABILIDADES
      # ---------------------------------------------------
      
      grafico <- modelo$dados_modelo |>
            
            ggplot2::ggplot(
                  
                  ggplot2::aes(
                        x = probabilidade
                  )
            ) +
            
            ggplot2::geom_histogram(
                  bins = 10
            ) +
            
            ggplot2::labs(
                  
                  title = paste(
                        "Distribuição das probabilidades —",
                        sexo_label
                  ),
                  
                  x = "Probabilidade predita",
                  
                  y = "Frequência"
            )
      
      print(grafico)
      
      # ---------------------------------------------------
      # RETORNO
      # ---------------------------------------------------
      
      return(modelo)
}

# =========================================================
# TESTE — FÊMEAS
# =========================================================

modelo_femeas <- testar_firth(
      
      dados = femeas_desfecho,
      
      variaveis = variaveis_femeas,
      
      sexo_label = "Fêmeas"
)

# =========================================================
# TESTE — MACHOS
# =========================================================

modelo_machos <- testar_firth(
      
      dados = machos_desfecho,
      
      variaveis = variaveis_machos,
      
      sexo_label = "Machos"
)

# =========================================================
# FINAL
# =========================================================

message(
      "\n================================================="
)

message(
      "Teste do módulo 06 concluído com sucesso."
)

message(
      "=================================================\n"
)
