# =========================================================
# TESTE — MÓDULO 10
# INTERPRETABILIDADE
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
# PREDIÇÕES
# =========================================================

predicoes_femeas <- gerar_predicoes(
      
      modelo_firth =
            modelo_femeas$modelo,
      
      dados =
            femeas_desfecho,
      
      variavel_alvo =
            "indice_parasitologico",
      
      n_pontos = 100,
      
      n_boot = 500
)

predicoes_femeas$variavel_plot <-
      predicoes_femeas$indice_parasitologico

predicoes_machos <- gerar_predicoes(
      
      modelo_firth =
            modelo_machos$modelo,
      
      dados =
            machos_desfecho,
      
      variavel_alvo =
            "peso_inicial",
      
      n_pontos = 100,
      
      n_boot = 500
)

predicoes_machos$variavel_plot <-
      predicoes_machos$peso_inicial

# =========================================================
# FUNÇÃO AUXILIAR DE TESTE
# =========================================================

testar_interpretabilidade <- function(
            
      modelo,
      
      bootstrap,
      
      predicoes,
      
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
                  "INTERPRETABILIDADE —",
                  toupper(sexo_label)
            )
      )
      
      message(
            "=================================================\n"
      )
      
      # ===================================================
      # INTERPRETAÇÃO
      # ===================================================
      
      interpretacao <- interpretar_modelo(
            
            modelo_firth =
                  modelo$modelo,
            
            bootstrap_resultado =
                  bootstrap,
            
            predicoes =
                  predicoes,
            
            titulo_modelo =
                  sexo_label
      )
      
      # ===================================================
      # TABELA PRINCIPAL
      # ===================================================
      
      message(
            "\n========== TABELA DE INTERPRETAÇÃO =========="
      )
      
      print(
            interpretacao$tabela
      )
      
      # ===================================================
      # EXPLAINABILITY
      # ===================================================
      
      message(
            "\n========== EXPLAINABILITY =========="
      )
      
      print(
            interpretacao$explainability |>
                  dplyr::select(
                        Variavel,
                        OR,
                        interpretacao
                  )
      )
      
      # ===================================================
      # FOREST PLOT
      # ===================================================
      
      print(
            interpretacao$forest_plot
      )
      
      # ===================================================
      # IMPORTÂNCIA RELATIVA
      # ===================================================
      
      print(
            interpretacao$grafico_importancia
      )
      
      # ===================================================
      # OR PLOT
      # ===================================================
      
      print(
            interpretacao$or_plot
      )
      
      # ===================================================
      # CURVAS MARGINAIS
      # ===================================================
      
      if(!is.null(
            interpretacao$curvas_marginais
      )) {
            
            print(
                  interpretacao$curvas_marginais
            )
      }
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(interpretacao)
}

# =========================================================
# TESTE — FÊMEAS
# =========================================================

interpretacao_femeas <-
      testar_interpretabilidade(
            
            modelo =
                  modelo_femeas,
            
            bootstrap =
                  bootstrap_femeas,
            
            predicoes =
                  predicoes_femeas,
            
            sexo_label =
                  "Fêmeas"
      )

# =========================================================
# TESTE — MACHOS
# =========================================================

interpretacao_machos <-
      testar_interpretabilidade(
            
            modelo =
                  modelo_machos,
            
            bootstrap =
                  bootstrap_machos,
            
            predicoes =
                  predicoes_machos,
            
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
      "Teste do módulo 10 concluído com sucesso."
)

message(
      "=================================================\n"
)