# =========================================================
# TESTE — MÓDULO 04
# ELASTIC NET
# =========================================================
# Objetivo:
# Validar:
# - ajuste do Elastic Net;
# - construção da matriz preditora;
# - seleção preliminar de variáveis;
# - coeficientes penalizados;
# - comportamento dos lambdas.
#
# IMPORTANTE:
# Este módulo realiza apenas:
# seleção preliminar exploratória.
#
# A robustez estrutural será avaliada
# posteriormente no:
#
# 05_stability_selection.R
# =========================================================

rm(list = ls())

gc()

cat("\014")

# ---------------------------------------------------------
# CARREGAR PACOTES
# ---------------------------------------------------------

source("R/00_pacotes.R")

# ---------------------------------------------------------
# CARREGAR MÓDULOS
# ---------------------------------------------------------

source("R/01_preprocessamento.R")
source("R/02_subgrupos.R")
source("R/03_desfecho.R")
source("R/04_elasticnet.R")

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
# DESFECHO — FÊMEAS
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

# =========================================================
# DESFECHO — MACHOS
# =========================================================

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
# FUNÇÃO AUXILIAR DE TESTE
# =========================================================

testar_elasticnet <- function(
            
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
                  "ELASTIC NET —",
                  toupper(sexo_label)
            )
      )
      
      message(
            "=================================================\n"
      )
      
      # ===================================================
      # DISTRIBUIÇÃO DO DESFECHO
      # ===================================================
      
      message(
            "--- Distribuição do desfecho ---"
      )
      
      print(
            table(
                  dados$apto_label
            )
      )
      
      # ===================================================
      # PROPORÇÕES
      # ===================================================
      
      message(
            "\n--- Proporção das classes ---"
      )
      
      print(
            round(
                  prop.table(
                        table(
                              dados$apto_label
                        )
                  ) * 100,
                  1
            )
      )
      
      # ===================================================
      # AJUSTE ELASTIC NET
      # ===================================================
      
      modelo <- ajustar_elasticnet(
            
            dados = dados,
            
            desfecho = "apto_bin",
            
            alpha = 0.5,
            
            nfolds = 5,
            
            usar_lambda_1se = TRUE,
            
            seed = 1234
      )
      
      # ===================================================
      # DIMENSÕES DA MATRIZ
      # ===================================================
      
      message(
            "\n--- Dimensões da matriz X ---"
      )
      
      print(
            dim(modelo$x)
      )
      
      # ===================================================
      # NOMES DAS VARIÁVEIS
      # ===================================================
      
      message(
            "\n--- Variáveis da matriz ---"
      )
      
      print(
            colnames(modelo$x)
      )
      
      # ===================================================
      # VARIÁVEIS SELECIONADAS
      # ===================================================
      
      message(
            "\n--- Variáveis selecionadas ---"
      )
      
      print(
            modelo$variaveis
      )
      
      # ===================================================
      # COEFICIENTES PENALIZADOS
      # ===================================================
      
      message(
            "\n--- Coeficientes penalizados ---"
      )
      
      print(
            modelo$coeficientes
      )
      
      # ===================================================
      # LAMBDAS
      # ===================================================
      
      message(
            "\n--- Lambdas ---"
      )
      
      message(
            paste(
                  "Lambda mínimo:",
                  round(
                        modelo$lambda_min,
                        6
                  )
            )
      )
      
      message(
            paste(
                  "Lambda 1se:",
                  round(
                        modelo$lambda_1se,
                        6
                  )
            )
      )
      
      message(
            paste(
                  "Lambda utilizado:",
                  modelo$lambda_utilizado
            )
      )
      
      # ===================================================
      # INFORMAÇÕES GERAIS
      # ===================================================
      
      message(
            "\n--- Informações do modelo ---"
      )
      
      message(
            paste(
                  "Observações:",
                  modelo$n_observacoes
            )
      )
      
      message(
            paste(
                  "Preditores:",
                  modelo$n_preditores
            )
      )
      
      message(
            paste(
                  "Alpha:",
                  modelo$alpha
            )
      )
      
      message(
            paste(
                  "Folds:",
                  modelo$nfolds
            )
      )
      
      # ===================================================
      # PLOT VALIDAÇÃO CRUZADA
      # ===================================================
      
      plot(modelo$modelo)
      
      # ===================================================
      # LINHAS DOS LAMBDAS
      # ===================================================
      
      abline(
            
            v = log(modelo$lambda_min),
            
            col = 2,
            
            lty = 2,
            
            lwd = 2
      )
      
      abline(
            
            v = log(modelo$lambda_1se),
            
            col = 4,
            
            lty = 2,
            
            lwd = 2
      )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(modelo)
}

# =========================================================
# TESTE — FÊMEAS
# =========================================================

modelo_femeas <- testar_elasticnet(
      
      dados = femeas_desfecho,
      
      sexo_label = "Fêmeas"
)

# =========================================================
# TESTE — MACHOS
# =========================================================

modelo_machos <- testar_elasticnet(
      
      dados = machos_desfecho,
      
      sexo_label = "Machos"
)

# =========================================================
# COMPARAÇÃO ENTRE SEXOS
# =========================================================

message(
      "\n================================================="
)

message(
      "COMPARAÇÃO ENTRE SEXOS"
)

message(
      "=================================================\n"
)

# ---------------------------------------------------------
# Variáveis selecionadas — Fêmeas
# ---------------------------------------------------------

message(
      "--- FÊMEAS ---"
)

print(
      modelo_femeas$variaveis
)

# ---------------------------------------------------------
# Variáveis selecionadas — Machos
# ---------------------------------------------------------

message(
      "\n--- MACHOS ---"
)

print(
      modelo_machos$variaveis
)

# =========================================================
# COMPARAÇÃO DOS COEFICIENTES
# =========================================================

message(
      "\n--- Coeficientes — Fêmeas ---"
)

print(
      modelo_femeas$coeficientes
)

message(
      "\n--- Coeficientes — Machos ---"
)

print(
      modelo_machos$coeficientes
)

# =========================================================
# RESUMO FINAL
# =========================================================

message(
      "\n================================================="
)

message(
      "Teste do módulo 04 concluído com sucesso."
)

message(
      "=================================================\n"
)
