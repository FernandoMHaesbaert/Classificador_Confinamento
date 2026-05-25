# =========================================================
# TESTE — MÓDULO 08
# PREDIÇÃO E CURVAS MARGINAIS
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
# AJUSTE DOS MODELOS FIRTH
# =========================================================

modelo_femeas <- ajustar_firth(
      
      dados = femeas_desfecho,
      
      variaveis_finais = variaveis_femeas
)

modelo_machos <- ajustar_firth(
      
      dados = machos_desfecho,
      
      variaveis_finais = variaveis_machos
)

# =========================================================
# FUNÇÃO AUXILIAR DE TESTE
# =========================================================
# =========================================================
# FUNÇÃO AUXILIAR DE TESTE
# Predição marginal + visualização
# =========================================================

testar_predicao <- function(
            
      modelo,
      
      dados,
      
      variavel_alvo,
      
      sexo_label,
      
      n_boot = 2000
) {
      
      # ===================================================
      # CABEÇALHO
      # ===================================================
      
      message(
            "\n================================================="
      )
      
      message(
            paste(
                  "PREDIÇÃO MARGINAL —",
                  toupper(sexo_label)
            )
      )
      
      message(
            "=================================================\n"
      )
      
      # ===================================================
      # GERAÇÃO DAS PREDIÇÕES
      # ===================================================
      
      predicoes <- gerar_predicoes(
            
            modelo_firth =
                  modelo$modelo,
            
            dados = dados,
            
            variavel_alvo =
                  variavel_alvo,
            
            n_pontos = 100,
            
            n_boot = n_boot
      )
      
      # ===================================================
      # VARIÁVEL AUXILIAR PARA PLOT
      # ===================================================
      # Evita problemas com:
      # - aes_string() deprecated
      # - tidy evaluation
      # - .data
      # - NSE do ggplot2
      # ===================================================
      
      predicoes$variavel_plot <-
            predicoes[[variavel_alvo]]
      
      # ===================================================
      # VISUALIZAÇÃO DA TABELA
      # ===================================================
      
      message(
            "\n========== TABELA DE PREDIÇÕES =========="
      )
      
      print(
            head(
                  predicoes,
                  10
            )
      )
      
      # ===================================================
      # GRÁFICO — VARIÁVEL CONTÍNUA
      # ===================================================
      
      if(is.numeric(dados[[variavel_alvo]])) {
            
            grafico <- ggplot2::ggplot(
                  
                  predicoes,
                  
                  ggplot2::aes(
                        x = variavel_plot,
                        y = prob_media
                  )
            ) +
                  
                  ggplot2::geom_ribbon(
                        
                        ggplot2::aes(
                              ymin = IC_inf,
                              ymax = IC_sup
                        ),
                        
                        alpha = 0.2
                  ) +
                  
                  ggplot2::geom_line(
                        linewidth = 1
                  ) +
                  
                  ggplot2::labs(
                        
                        title = paste(
                              "Curva marginal —",
                              variavel_alvo,
                              "|",
                              sexo_label
                        ),
                        
                        x = variavel_alvo,
                        
                        y = "Probabilidade predita"
                  ) +
                  
                  ggplot2::theme_minimal()
            
            print(grafico)
            
      } else {
            
            # ==============================================
            # GRÁFICO — VARIÁVEL CATEGÓRICA
            # ==============================================
            
            grafico <- ggplot2::ggplot(
                  
                  predicoes,
                  
                  ggplot2::aes(
                        x = variavel_plot,
                        y = prob_media
                  )
            ) +
                  
                  ggplot2::geom_point(
                        size = 4
                  ) +
                  
                  ggplot2::geom_errorbar(
                        
                        ggplot2::aes(
                              ymin = IC_inf,
                              ymax = IC_sup
                        ),
                        
                        width = 0.2
                  ) +
                  
                  ggplot2::labs(
                        
                        title = paste(
                              "Efeito predito —",
                              variavel_alvo,
                              "|",
                              sexo_label
                        ),
                        
                        x = variavel_alvo,
                        
                        y = "Probabilidade predita"
                  ) +
                  
                  ggplot2::theme_minimal()
            
            print(grafico)
      }
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(predicoes)
}
# =========================================================
# TESTE — FÊMEAS
# CURVA DO ÍNDICE PARASITOLÓGICO
# =========================================================

pred_femeas <- testar_predicao(
      
      modelo = modelo_femeas,
      
      dados = femeas_desfecho,
      
      variavel_alvo =
            "indice_parasitologico",
      
      sexo_label = "Fêmeas",
      
      n_boot = 2000
)

# =========================================================
# TESTE — MACHOS
# CURVA DE PESO INICIAL
# =========================================================

pred_machos <- testar_predicao(
      
      modelo = modelo_machos,
      
      dados = machos_desfecho,
      
      variavel_alvo =
            "peso_inicial",
      
      sexo_label = "Machos",
      
      n_boot = 2000
)

# =========================================================
# TESTE — MACHOS
# EFEITO DO GRUPO GENÉTICO
# =========================================================

pred_genetico <- testar_predicao(
      
      modelo = modelo_machos,
      
      dados = machos_desfecho,
      
      variavel_alvo =
            "grupo_genetico",
      
      sexo_label = "Machos",
      
      n_boot = 2000
)

# =========================================================
# RESUMO FINAL
# =========================================================

message(
      "\n================================================="
)

message(
      "Teste do módulo 08 concluído com sucesso."
)

message(
      "=================================================\n"
)

