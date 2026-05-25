# =========================================================
# TESTE — MÓDULO 14
# PIPELINE COMPLETO
# =========================================================
# Objetivo:
# Validar:
# - integração completa do sistema;
# - interoperabilidade entre módulos;
# - consistência estrutural;
# - outputs finais;
# - robustez do backend.
#
# Este é o principal teste sistêmico do projeto.
# =========================================================

rm(list = ls())

gc()

cat("\014")

# =========================================================
# PACOTES
# =========================================================

source("R/00_pacotes.R")

# =========================================================
# MÓDULOS ANALÍTICOS
# =========================================================

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

source("R/12_graficos.R")

# =========================================================
# PIPELINE
# =========================================================

source("R/14_pipeline.R")

# =========================================================
# CABEÇALHO
# =========================================================

message(
      "\n================================================="
)

message(
      "TESTE SISTÊMICO — PIPELINE COMPLETO"
)

message(
      "=================================================\n"
)

# =========================================================
# EXECUÇÃO DO PIPELINE
# =========================================================

message(
      "\n========== EXECUÇÃO DO PIPELINE =========="
)

tempo_inicio <- Sys.time()

resultado_pipeline <- executar_pipeline_confinamento(
      
      caminho_base =
            "data/dados_brutos/base_limpa.rds",
      
      # ==============================================
      # CONTROLE
      # ==============================================
      
      seed = 123,
      
      verbose = TRUE,
      
      # ==============================================
      # PERFORMANCE
      # ==============================================
      
      n_boot_stability = 200,
      
      n_boot_inferencial = 200,
      
      n_pontos_predicao = 50
)

tempo_fim <- Sys.time()

tempo_execucao <- round(
      
      as.numeric(
            
            difftime(
                  
                  tempo_fim,
                  
                  tempo_inicio,
                  
                  units = "secs"
            )
      ),
      
      2
)

# =========================================================
# ESTRUTURA GERAL
# =========================================================

message(
      "\n========== ESTRUTURA DO OBJETO =========="
)

print(
      names(
            resultado_pipeline
      )
)

# =========================================================
# VALIDAÇÃO — FÊMEAS
# =========================================================

message(
      "\n================================================="
)

message(
      "VALIDAÇÃO — FÊMEAS"
)

message(
      "=================================================\n"
)

# =========================================================
# VARIÁVEIS FINAIS
# =========================================================

message(
      "\n========== VARIÁVEIS FINAIS =========="
)

print(
      resultado_pipeline$femeas$variaveis_finais
)

# =========================================================
# AUC
# =========================================================

message(
      "\n========== PERFORMANCE ROC =========="
)

auc_femeas <- resultado_pipeline$femeas$roc$auc

print(auc_femeas)

# =========================================================
# TABELA OR
# =========================================================

message(
      "\n========== ODDS RATIOS =========="
)

print(
      resultado_pipeline$femeas$tabelas$or
)

# =========================================================
# ESTABILIDADE
# =========================================================

message(
      "\n========== ESTABILIDADE =========="
)

print(
      resultado_pipeline$femeas$tabelas$estabilidade
)

# =========================================================
# BOOTSTRAP
# =========================================================

message(
      "\n========== BOOTSTRAP =========="
)

print(
      resultado_pipeline$femeas$tabelas$bootstrap
)

# =========================================================
# ROC
# =========================================================

message(
      "\n========== ROC =========="
)

print(
      resultado_pipeline$femeas$tabelas$roc
)

# =========================================================
# PREDIÇÕES
# =========================================================

message(
      "\n========== PREDIÇÕES =========="
)

print(
      head(
            resultado_pipeline$femeas$predicoes,
            10
      )
)

# =========================================================
# INTERPRETABILIDADE
# =========================================================

message(
      "\n========== INTERPRETABILIDADE =========="
)

print(
      resultado_pipeline$femeas$
            interpretabilidade$
            tabela_interpretacao
)

# =========================================================
# VALIDAÇÃO — MACHOS
# =========================================================

message(
      "\n================================================="
)

message(
      "VALIDAÇÃO — MACHOS"
)

message(
      "=================================================\n"
)

# =========================================================
# VARIÁVEIS FINAIS
# =========================================================

message(
      "\n========== VARIÁVEIS FINAIS =========="
)

print(
      resultado_pipeline$machos$variaveis_finais
)

# =========================================================
# AUC
# =========================================================

message(
      "\n========== PERFORMANCE ROC =========="
)

auc_machos <- resultado_pipeline$machos$roc$auc

print(auc_machos)

# =========================================================
# TABELA OR
# =========================================================

message(
      "\n========== ODDS RATIOS =========="
)

print(
      resultado_pipeline$machos$tabelas$or
)

# =========================================================
# ESTABILIDADE
# =========================================================

message(
      "\n========== ESTABILIDADE =========="
)

print(
      resultado_pipeline$machos$tabelas$estabilidade
)

# =========================================================
# BOOTSTRAP
# =========================================================

message(
      "\n========== BOOTSTRAP =========="
)

print(
      resultado_pipeline$machos$tabelas$bootstrap
)

# =========================================================
# ROC
# =========================================================

message(
      "\n========== ROC =========="
)

print(
      resultado_pipeline$machos$tabelas$roc
)

# =========================================================
# PREDIÇÕES
# =========================================================

message(
      "\n========== PREDIÇÕES =========="
)

print(
      head(
            resultado_pipeline$machos$predicoes,
            10
      )
)

# =========================================================
# INTERPRETABILIDADE
# =========================================================

message(
      "\n========== INTERPRETABILIDADE =========="
)

print(
      resultado_pipeline$machos$
            interpretabilidade$
            tabela_interpretacao
)

# =========================================================
# VALIDAÇÕES ESTRUTURAIS
# =========================================================

message(
      "\n================================================="
)

message(
      "VALIDAÇÕES ESTRUTURAIS"
)

message(
      "=================================================\n"
)

# =========================================================
# FUNÇÃO AUXILIAR
# =========================================================

validar_objeto <- function(
            
      objeto,
      
      nome
) {
      
      valido <- !is.null(objeto)
      
      status <- ifelse(
            
            valido,
            
            "OK",
            
            "FALHA"
      )
      
      tibble::tibble(
            
            Objeto = nome,
            
            Status = status
      )
}

# =========================================================
# LISTA DE VALIDAÇÕES
# =========================================================

validacoes <- dplyr::bind_rows(
      
      validar_objeto(
            
            resultado_pipeline$femeas$modelo_firth,
            
            "Modelo Firth — Fêmeas"
      ),
      
      validar_objeto(
            
            resultado_pipeline$machos$modelo_firth,
            
            "Modelo Firth — Machos"
      ),
      
      validar_objeto(
            
            resultado_pipeline$femeas$bootstrap,
            
            "Bootstrap — Fêmeas"
      ),
      
      validar_objeto(
            
            resultado_pipeline$machos$bootstrap,
            
            "Bootstrap — Machos"
      ),
      
      validar_objeto(
            
            resultado_pipeline$femeas$roc,
            
            "ROC — Fêmeas"
      ),
      
      validar_objeto(
            
            resultado_pipeline$machos$roc,
            
            "ROC — Machos"
      ),
      
      validar_objeto(
            
            resultado_pipeline$femeas$plots$painel,
            
            "Painel gráfico — Fêmeas"
      ),
      
      validar_objeto(
            
            resultado_pipeline$machos$plots$painel,
            
            "Painel gráfico — Machos"
      )
)

print(validacoes)

# =========================================================
# PLOTS
# =========================================================

message(
      "\n================================================="
)

message(
      "PLOTS INTEGRADOS"
)

message(
      "=================================================\n"
)

# =========================================================
# PAINEL — FÊMEAS
# =========================================================

print(
      resultado_pipeline$femeas$plots$painel
)

# =========================================================
# PAINEL — MACHOS
# =========================================================

print(
      resultado_pipeline$machos$plots$painel
)

# =========================================================
# EXPORTAÇÃO
# =========================================================

message(
      "\n================================================="
)

message(
      "EXPORTAÇÃO"
)

message(
      "=================================================\n"
)

dir.create(
      
      "reports/pipeline",
      
      recursive = TRUE,
      
      showWarnings = FALSE
)

# =========================================================
# EXPORTAÇÃO — FÊMEAS
# =========================================================

exportar_plot(
      
      grafico =
            resultado_pipeline$femeas$plots$painel,
      
      caminho =
            "reports/pipeline/painel_femeas.png",
      
      largura = 12,
      
      altura = 10
)

# =========================================================
# EXPORTAÇÃO — MACHOS
# =========================================================

exportar_plot(
      
      grafico =
            resultado_pipeline$machos$plots$painel,
      
      caminho =
            "reports/pipeline/painel_machos.png",
      
      largura = 12,
      
      altura = 10
)

# =========================================================
# MÉTRICAS GERAIS
# =========================================================

message(
      "\n================================================="
)

message(
      "MÉTRICAS GERAIS"
)

message(
      "=================================================\n"
)

metricas <- tibble::tibble(
      
      Grupo = c(
            "Fêmeas",
            "Machos"
      ),
      
      AUC = c(
            resultado_pipeline$femeas$roc$auc,
            resultado_pipeline$machos$roc$auc
      ),
      
      Variaveis = c(
            length(
                  resultado_pipeline$femeas$
                        variaveis_finais
            ),
            
            length(
                  resultado_pipeline$machos$
                        variaveis_finais
            )
      )
)

print(metricas)

# =========================================================
# TEMPO DE EXECUÇÃO
# =========================================================

message(
      "\n================================================="
)

message(
      "TEMPO DE EXECUÇÃO"
)

message(
      "=================================================\n"
)

message(
      paste(
            "Tempo total:",
            tempo_execucao,
            "segundos"
      )
)

# =========================================================
# FINALIZAÇÃO
# =========================================================

message(
      "\n================================================="
)

message(
      "TESTE DO PIPELINE CONCLUÍDO COM SUCESSO"
)

message(
      "=================================================\n"
)