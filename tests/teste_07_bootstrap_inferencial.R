# =========================================================
# TESTE — MÓDULO 07
# BOOTSTRAP INFERENCIAL DO MODELO FIRTH
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

testar_bootstrap <- function(
            
      modelo,
      
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
                  "BOOTSTRAP INFERENCIAL —",
                  toupper(sexo_label)
            )
      )
      
      message(
            "=================================================\n"
      )
      
      # ===================================================
      # EXECUÇÃO DO BOOTSTRAP
      # ===================================================
      
      boot <- bootstrap_firth(
            
            modelo_firth =
                  modelo$modelo,
            
            n_boot = n_boot,
            
            nivel_ic = 0.95,
            
            seed = 1234
      )
      
      # ===================================================
      # RESUMO DOS COEFICIENTES
      # ===================================================
      
      message(
            "\n========== COEFICIENTES BOOTSTRAP =========="
      )
      
      print(
            boot$resumo_coef
      )
      
      # ===================================================
      # RESUMO DOS OR
      # ===================================================
      
      message(
            "\n========== ODDS RATIOS BOOTSTRAP =========="
      )
      
      print(
            boot$resumo_or
      )
      
      # ===================================================
      # ESTABILIDADE
      # ===================================================
      
      message(
            "\n========== ESTABILIDADE INFERENCIAL =========="
      )
      
      print(
            boot$estabilidade
      )
      
      # ===================================================
      # HISTOGRAMAS DOS COEFICIENTES
      # ===================================================
      
      message(
            "\n========== DISTRIBUIÇÕES BOOTSTRAP =========="
      )
      
      vars <- names(
            boot$beta_boot
      )
      
      for(v in vars) {
            
            grafico <- ggplot2::ggplot(
                  
                  boot$beta_boot,
                  
                  ggplot2::aes_string(
                        x = v
                  )
            ) +
                  
                  ggplot2::geom_histogram(
                        bins = 40
                  ) +
                  
                  ggplot2::labs(
                        
                        title = paste(
                              "Distribuição Bootstrap —",
                              v,
                              "|",
                              sexo_label
                        ),
                        
                        x = "Coeficiente bootstrap",
                        
                        y = "Frequência"
                  )
            
            print(grafico)
      }
      
      # ===================================================
      # BOXPLOT DOS OR
      # ===================================================
      
      or_long <- boot$or_boot |>
            
            tidyr::pivot_longer(
                  
                  cols = everything(),
                  
                  names_to = "Variavel",
                  
                  values_to = "OR"
            )
      
      grafico_or <- ggplot2::ggplot(
            
            or_long,
            
            ggplot2::aes(
                  x = Variavel,
                  y = OR
            )
      ) +
            
            ggplot2::geom_boxplot() +
            
            ggplot2::coord_flip() +
            
            ggplot2::labs(
                  
                  title = paste(
                        "Distribuição dos OR Bootstrap —",
                        sexo_label
                  ),
                  
                  x = "",
                  
                  y = "Odds Ratio"
            )
      
      print(grafico_or)
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(boot)
}

# =========================================================
# TESTE — FÊMEAS
# =========================================================

boot_femeas <- testar_bootstrap(
      
      modelo = modelo_femeas,
      
      sexo_label = "Fêmeas",
      
      n_boot = 2000
)

# =========================================================
# TESTE — MACHOS
# =========================================================

boot_machos <- testar_bootstrap(
      
      modelo = modelo_machos,
      
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
      "Teste do módulo 07 concluído com sucesso."
)

message(
      "=================================================\n"
)