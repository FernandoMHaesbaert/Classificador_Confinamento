# =========================================================
# TESTE — MÓDULO 11
# TABELAS
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

source("R/10_interpretabilidade.R")

source("R/11_tabelas.R")

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
# BOOTSTRAP INFERENCIAL
# =========================================================

bootstrap_femeas <- bootstrap_firth(
      
      modelo_firth =
            modelo_femeas$modelo,
      
      n_boot = 500,
      
      seed = 123
)

bootstrap_machos <- bootstrap_firth(
      
      modelo_firth =
            modelo_machos$modelo,
      
      n_boot = 500,
      
      seed = 123
)

# =========================================================
# ROC
# =========================================================

roc_femeas <- avaliar_roc(
      
      modelo_firth =
            modelo_femeas$modelo,
      
      dados =
            femeas_desfecho
)

roc_machos <- avaliar_roc(
      
      modelo_firth =
            modelo_machos$modelo,
      
      dados =
            machos_desfecho
)

# =========================================================
# STABILITY SELECTION
# =========================================================

stability_femeas <- executar_stability_selection(
      
      dados = femeas_desfecho,
      
      desfecho = "apto_bin",
      
      n_boot = 500,
      
      alpha = 0.5
)

stability_machos <- executar_stability_selection(
      
      dados = machos_desfecho,
      
      desfecho = "apto_bin",
      
      n_boot = 500,
      
      alpha = 0.5
)

# =========================================================
# FUNÇÃO AUXILIAR
# =========================================================

testar_tabelas <- function(
            
      modelo,
      
      bootstrap,
      
      roc,
      
      stability,
      
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
                  "TABELAS —",
                  toupper(sexo_label)
            )
      )
      
      message(
            "=================================================\n"
      )
      
      # ===================================================
      # TABELA OR
      # ===================================================
      
      tabela_or_modelo <- tabela_or(
            
            modelo_firth =
                  modelo$modelo
      )
      
      message(
            "\n========== ODDS RATIOS =========="
      )
      
      print(
            tabela_or_modelo
      )
      
      # ===================================================
      # TABELA BOOTSTRAP
      # ===================================================
      
      tabela_boot <- tabela_bootstrap(
            
            bootstrap_resultado =
                  bootstrap
      )
      
      message(
            "\n========== BOOTSTRAP =========="
      )
      
      print(
            tabela_boot
      )
      
      # ===================================================
      # TABELA ROC
      # ===================================================
      
      tabela_roc_modelo <- tabela_roc(
            
            roc_resultado =
                  roc
      )
      
      message(
            "\n========== PERFORMANCE ROC =========="
      )
      
      print(
            tabela_roc_modelo
      )
      
      # ===================================================
      # TABELA ESTABILIDADE
      # ===================================================
      
      tabela_estab <- tabela_estabilidade(
            
            stability_resultado =
                  stability
      )
      
      message(
            "\n========== ESTABILIDADE =========="
      )
      
      print(
            tabela_estab
      )
      
      # ===================================================
      # TABELA RESUMO
      # ===================================================
      
      tabela_resumo <- tabela_resumo_modelo(
            
            modelo_firth =
                  modelo$modelo,
            
            roc_resultado =
                  roc,
            
            bootstrap_resultado =
                  bootstrap,
            
            titulo_modelo =
                  sexo_label
      )
      
      message(
            "\n========== RESUMO FINAL =========="
      )
      
      print(
            tabela_resumo
      )
      
      # ===================================================
      # EXPORTAÇÃO OPCIONAL
      # ===================================================
      
      dir.create(
            
            "reports",
            
            showWarnings = FALSE
      )
      
      exportar_tabela_csv(
            
            tabela_or_modelo,
            
            paste0(
                  "reports/tabela_or_",
                  tolower(sexo_label),
                  ".csv"
            )
      )
      
      exportar_tabela_csv(
            
            tabela_boot,
            
            paste0(
                  "reports/tabela_bootstrap_",
                  tolower(sexo_label),
                  ".csv"
            )
      )
      
      exportar_tabela_csv(
            
            tabela_roc_modelo,
            
            paste0(
                  "reports/tabela_roc_",
                  tolower(sexo_label),
                  ".csv"
            )
      )
      
      exportar_tabela_csv(
            
            tabela_estab,
            
            paste0(
                  "reports/tabela_estabilidade_",
                  tolower(sexo_label),
                  ".csv"
            )
      )
      
      exportar_tabela_csv(
            
            tabela_resumo,
            
            paste0(
                  "reports/tabela_resumo_",
                  tolower(sexo_label),
                  ".csv"
            )
      )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(
            list(
                  
                  tabela_or =
                        tabela_or_modelo,
                  
                  tabela_bootstrap =
                        tabela_boot,
                  
                  tabela_roc =
                        tabela_roc_modelo,
                  
                  tabela_estabilidade =
                        tabela_estab,
                  
                  tabela_resumo =
                        tabela_resumo
            )
      )
}

# =========================================================
# TESTE — FÊMEAS
# =========================================================

tabelas_femeas <- testar_tabelas(
      
      modelo =
            modelo_femeas,
      
      bootstrap =
            bootstrap_femeas,
      
      roc =
            roc_femeas,
      
      stability =
            stability_femeas,
      
      sexo_label =
            "Fêmeas"
)

# =========================================================
# TESTE — MACHOS
# =========================================================

tabelas_machos <- testar_tabelas(
      
      modelo =
            modelo_machos,
      
      bootstrap =
            bootstrap_machos,
      
      roc =
            roc_machos,
      
      stability =
            stability_machos,
      
      sexo_label =
            "Machos"
)

# =========================================================
# RESUMO FINAL
# =========================================================

message(
      "\n================================================="
)

message(
      "Teste do módulo 11 concluído com sucesso."
)

message(
      "=================================================\n"
)