# =========================================================
# TESTE — MÓDULO 12
# PLOTS
# =========================================================

rm(list = ls())

gc()

cat("\014")

# =========================================================
# PACOTES
# =========================================================

source("R/00_pacotes.R")

# =========================================================
# MÓDULOS
# =========================================================

source("R/01_preprocessamento.R")

source("R/02_subgrupos.R")

source("R/03_desfecho.R")

source("R/05_stability_selection.R")

source("R/06_firth.R")

source("R/07_bootstrap_inferencial.R")

source("R/08_predicao.R")

source("R/09_roc.R")

source("R/10_interpretabilidade.R")

source("R/11_tabelas.R")

source("R/12_plots.R")

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
# BOOTSTRAP
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
# INTERPRETABILIDADE
# =========================================================

interpretacao_femeas <- interpretar_modelo(
      
      modelo_firth =
            modelo_femeas$modelo,
      
      bootstrap_resultado =
            bootstrap_femeas,
      
      titulo_modelo =
            "Fêmeas"
)

interpretacao_machos <- interpretar_modelo(
      
      modelo_firth =
            modelo_machos$modelo,
      
      bootstrap_resultado =
            bootstrap_machos,
      
      titulo_modelo =
            "Machos"
)

# =========================================================
# PREDIÇÃO MARGINAL
# =========================================================

predicoes_femeas <- gerar_predicoes(
      
      modelo_firth =
            modelo_femeas$modelo,
      
      dados =
            femeas_desfecho,
      
      variavel_alvo =
            "indice_parasitologico",
      
      n_boot = 500
)

predicoes_machos <- gerar_predicoes(
      
      modelo_firth =
            modelo_machos$modelo,
      
      dados =
            machos_desfecho,
      
      variavel_alvo =
            "peso_inicial",
      
      n_boot = 500
)

# =========================================================
# TABELAS
# =========================================================

tabela_or_femeas <- tabela_or(
      
      modelo_firth =
            modelo_femeas$modelo
)

tabela_or_machos <- tabela_or(
      
      modelo_firth =
            modelo_machos$modelo
)

# =========================================================
# FUNÇÃO AUXILIAR
# =========================================================

testar_plots <- function(
            
      roc_resultado,
      
      tabela_or_modelo,
      
      tabela_interpretacao,
      
      predicoes,
      
      variavel_alvo,
      
      stability_resultado,
      
      bootstrap_resultado,
      
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
                  "PLOTS —",
                  toupper(sexo_label)
            )
      )
      
      message(
            "=================================================\n"
      )
      
      # ===================================================
      # ROC
      # ===================================================
      
      grafico_roc <- plot_roc(
            
            roc_resultado =
                  roc_resultado,
            
            titulo =
                  paste(
                        "ROC |",
                        sexo_label
                  )
      )
      
      print(grafico_roc)
      
      # ===================================================
      # CALIBRAÇÃO
      # ===================================================
      
      grafico_calibracao <- plot_calibracao(
            
            roc_resultado =
                  roc_resultado,
            
            titulo =
                  paste(
                        "Calibração |",
                        sexo_label
                  )
      )
      
      print(grafico_calibracao)
      
      # ===================================================
      # FOREST
      # ===================================================
      
      grafico_forest <- plot_forest(
            
            tabela_or =
                  tabela_or_modelo,
            
            titulo =
                  paste(
                        "Forest Plot |",
                        sexo_label
                  )
      )
      
      print(grafico_forest)
      
      # ===================================================
      # IMPORTÂNCIA
      # ===================================================
      
      grafico_importancia <- plot_importancia(
            
            tabela_interpretacao =
                  interpretacao_femeas$tabela_interpretacao,
            
            titulo =
                  paste(
                        "Importância |",
                        sexo_label
                  )
      )
      
      print(grafico_importancia)
      
      # ===================================================
      # CURVA MARGINAL
      # ===================================================
      
      grafico_predicao <- plot_predicao_marginal(
            
            predicoes =
                  predicoes,
            
            variavel_alvo =
                  variavel_alvo,
            
            titulo =
                  paste(
                        "Predição marginal |",
                        sexo_label
                  )
      )
      
      print(grafico_predicao)
      
      # ===================================================
      # ESTABILIDADE
      # ===================================================
      
      grafico_estabilidade <- plot_estabilidade(
            
            tabela_estabilidade =
                  stability_resultado$tabela_final,
            
            titulo =
                  paste(
                        "Stability Selection |",
                        sexo_label
                  )
      )
      
      print(grafico_estabilidade)
      
      # ===================================================
      # BOOTSTRAP
      # ===================================================
      
      variavel_boot <-
            bootstrap_resultado$resultados$Variavel[1]
      
      grafico_bootstrap <- plot_bootstrap(
            
            bootstrap_resultado =
                  bootstrap_resultado,
            
            variavel =
                  variavel_boot
      )
      
      print(grafico_bootstrap)
      
      # ===================================================
      # PAINEL INTEGRADO
      # ===================================================
      
      painel <- painel_modelo(
            
            roc_plot =
                  grafico_roc,
            
            forest_plot =
                  grafico_forest,
            
            importancia_plot =
                  grafico_importancia,
            
            calibracao_plot =
                  grafico_calibracao
      )
      
      print(painel)
      
      # ===================================================
      # EXPORTAÇÃO
      # ===================================================
      
      dir.create(
            
            "reports/plots",
            
            recursive = TRUE,
            
            showWarnings = FALSE
      )
      
      exportar_plot(
            
            grafico_roc,
            
            paste0(
                  "reports/plots/roc_",
                  tolower(sexo_label),
                  ".png"
            )
      )
      
      exportar_plot(
            
            grafico_forest,
            
            paste0(
                  "reports/plots/forest_",
                  tolower(sexo_label),
                  ".png"
            )
      )
      
      exportar_plot(
            
            painel,
            
            paste0(
                  "reports/plots/painel_",
                  tolower(sexo_label),
                  ".png"
            ),
            
            largura = 12,
            
            altura = 10
      )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(
            list(
                  
                  roc =
                        grafico_roc,
                  
                  calibracao =
                        grafico_calibracao,
                  
                  forest =
                        grafico_forest,
                  
                  importancia =
                        grafico_importancia,
                  
                  predicao =
                        grafico_predicao,
                  
                  estabilidade =
                        grafico_estabilidade,
                  
                  bootstrap =
                        grafico_bootstrap,
                  
                  painel =
                        painel
            )
      )
}

# =========================================================
# TESTE — FÊMEAS
# =========================================================

plots_femeas <- testar_plots(
      
      roc_resultado =
            roc_femeas,
      
      tabela_or_modelo =
            tabela_or_femeas,
      
      tabela_interpretacao =
            interpretacao_femeas$tabela_interpretacao,
      
      predicoes =
            predicoes_femeas,
      
      variavel_alvo =
            "indice_parasitologico",
      
      stability_resultado =
            stability_femeas,
      
      bootstrap_resultado =
            bootstrap_femeas,
      
      sexo_label =
            "Fêmeas"
)

# =========================================================
# TESTE — MACHOS
# =========================================================

plots_machos <- testar_plots(
      
      roc_resultado =
            roc_machos,
      
      tabela_or_modelo =
            tabela_or_machos,
      
      tabela_interpretacao =
            interpretacao_machos$tabela_interpretacao,
      
      predicoes =
            predicoes_machos,
      
      variavel_alvo =
            "peso_inicial",
      
      stability_resultado =
            stability_machos,
      
      bootstrap_resultado =
            bootstrap_machos,
      
      sexo_label =
            "Machos"
)

# =========================================================
# FINALIZAÇÃO
# =========================================================

message(
      "\n================================================="
)

message(
      "Teste do módulo 12 concluído com sucesso."
)

message(
      "=================================================\n"
)