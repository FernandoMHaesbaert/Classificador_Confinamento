# =========================================================
# TESTE — MÓDULO 05
# STABILITY SELECTION
# =========================================================
# Objetivo:
# Validar:
# - estabilidade estrutural;
# - frequência de seleção;
# - robustez das variáveis;
# - comportamento do bootstrap;
# - variáveis finais robustas.
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
source("R/05_stability_selection.R")

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

testar_stability_selection <- function(
            
      dados,
      
      sexo_label,
      
      n_boot = 1000,
      
      limiar_frequencia = 0.70
) {
      
      # ===================================================
      # CABEÇALHO
      # ===================================================
      
      message(
            "\n================================================="
      )
      
      message(
            paste(
                  "STABILITY SELECTION —",
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
      # AJUSTE DO MODELO
      # ===================================================
      
      modelo <- executar_stability_selection(
            
            dados = dados,
            
            desfecho = "apto_bin",
            
            alpha = 0.5,
            
            nfolds = 3,
            
            n_boot = n_boot,
            
            limiar_frequencia =
                  limiar_frequencia,
            
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
      # FREQUÊNCIA DE SELEÇÃO
      # ===================================================
      
      message(
            "\n--- Frequência de seleção ---"
      )
      
      print(
            modelo$frequencia
      )
      
      # ===================================================
      # VARIÁVEIS ROBUSTAS
      # ===================================================
      
      message(
            "\n--- Variáveis robustas finais ---"
      )
      
      print(
            modelo$variaveis_finais
      )
      
      # ===================================================
      # NÚMERO DE VARIÁVEIS FINAIS
      # ===================================================
      
      message(
            paste(
                  "\nNúmero de variáveis robustas:",
                  length(
                        modelo$variaveis_finais
                  )
            )
      )
      
      # ===================================================
      # MODELOS VÁLIDOS E INVÁLIDOS
      # ===================================================
      
      message(
            "\n--- Diagnóstico bootstrap ---"
      )
      
      message(
            paste(
                  "Modelos válidos:",
                  modelo$modelos_validos
            )
      )
      
      message(
            paste(
                  "Modelos inválidos:",
                  modelo$modelos_invalidos
            )
      )
      
      # ===================================================
      # GRÁFICO DE ESTABILIDADE
      # ===================================================
      
      grafico <- modelo$frequencia |>
            
            dplyr::mutate(
                  
                  Variavel = forcats::fct_reorder(
                        Variavel,
                        Frequencia
                  )
            ) |>
            
            ggplot2::ggplot(
                  
                  ggplot2::aes(
                        
                        x = Variavel,
                        
                        y = Frequencia_percentual
                  )
            ) +
            
            ggplot2::geom_col() +
            
            ggplot2::geom_hline(
                  
                  yintercept =
                        limiar_frequencia * 100,
                  
                  linetype = "dashed",
                  
                  linewidth = 1
            ) +
            
            ggplot2::coord_flip() +
            
            ggplot2::labs(
                  
                  title =
                        paste(
                              "Stability Selection —",
                              sexo_label
                        ),
                  
                  subtitle =
                        paste(
                              "Bootstrap =",
                              n_boot
                        ),
                  
                  x = NULL,
                  
                  y =
                        "Frequência de seleção (%)"
            ) +
            
            ggplot2::theme_minimal()
      
      print(grafico)
      
      # ===================================================
      # MATRIZ DE SELEÇÃO
      # ===================================================
      
      message(
            "\n--- Dimensão da matriz de seleção ---"
      )
      
      print(
            dim(modelo$selecoes)
      )
      
      # ===================================================
      # RESUMO INTERPRETATIVO
      # ===================================================
      
      message(
            "\n--- Resumo interpretativo ---"
      )
      
      if(length(modelo$variaveis_finais) == 0) {
            
            warning(
                  paste(
                        "Nenhuma variável atingiu",
                        "o limiar de estabilidade."
                  )
            )
            
      } else {
            
            message(
                  paste(
                        "As variáveis robustas para",
                        sexo_label,
                        "foram:"
                  )
            )
            
            print(
                  modelo$variaveis_finais
            )
      }
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(modelo)
}

# =========================================================
# TESTE — FÊMEAS
# =========================================================

modelo_femeas <- testar_stability_selection(
      
      dados = femeas_desfecho,
      
      sexo_label = "Fêmeas",
      
      n_boot = 1000,
      
      limiar_frequencia = 0.70
)

# =========================================================
# TESTE — MACHOS
# =========================================================

modelo_machos <- testar_stability_selection(
      
      dados = machos_desfecho,
      
      sexo_label = "Machos",
      
      n_boot = 1000,
      
      limiar_frequencia = 0.70
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
# Variáveis robustas — Fêmeas
# ---------------------------------------------------------

message(
      "--- FÊMEAS ---"
)

print(
      modelo_femeas$variaveis_finais
)

# ---------------------------------------------------------
# Variáveis robustas — Machos
# ---------------------------------------------------------

message(
      "\n--- MACHOS ---"
)

print(
      modelo_machos$variaveis_finais
)

# =========================================================
# TABELA RESUMO FINAL
# =========================================================

tabela_final <- dplyr::bind_rows(
      
      modelo_femeas$frequencia |>
            
            dplyr::mutate(
                  Sexo = "Fêmeas"
            ),
      
      modelo_machos$frequencia |>
            
            dplyr::mutate(
                  Sexo = "Machos"
            )
)

message(
      "\n--- Frequências consolidadas ---"
)

print(tabela_final)

# =========================================================
# RESUMO FINAL
# =========================================================

message(
      "\n================================================="
)

message(
      "Teste do módulo 05 concluído com sucesso."
)

message(
      "=================================================\n"
)

