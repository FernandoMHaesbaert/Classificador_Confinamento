# =========================================================
# MÓDULO 07 — BOOTSTRAP INFERENCIAL
# =========================================================
# Objetivo:
# Avaliar a robustez inferencial dos coeficientes
# do modelo logístico penalizado de Firth.
#
# O módulo:
# - utiliza bootstrap paramétrico;
# - preserva a estrutura penalizada do modelo;
# - propaga corretamente a incerteza;
# - produz IC bootstrap robustos;
# - avalia estabilidade inferencial.
#
# Entrada:
# - modelo Firth ajustado.
#
# Saída:
# - distribuições bootstrap;
# - OR bootstrap;
# - IC robustos;
# - estabilidade inferencial.
# =========================================================

bootstrap_firth <- function(
            
      modelo_firth,
      
      n_boot = 5000,
      
      nivel_ic = 0.95,
      
      seed = 1234
) {
      
      # ===================================================
      # REPRODUTIBILIDADE
      # ===================================================
      
      set.seed(seed)
      
      # ===================================================
      # VERIFICAÇÃO DO MODELO
      # ===================================================
      
      if(!inherits(modelo_firth, "logistf")) {
            
            stop(
                  "O objeto informado não é um modelo logistf."
            )
      }
      
      # ===================================================
      # EXTRAÇÃO DOS COEFICIENTES
      # ===================================================
      
      beta_hat <- coef(modelo_firth)
      
      # ===================================================
      # MATRIZ VARIÂNCIA-COVARIÂNCIA
      # ===================================================
      
      vcov_mat <- vcov(modelo_firth)
      
      # ===================================================
      # SIMULAÇÃO PARAMÉTRICA
      # ===================================================
      # beta* ~ MVN(beta_hat, vcov)
      # ===================================================
      
      beta_boot <- MASS::mvrnorm(
            
            n = n_boot,
            
            mu = beta_hat,
            
            Sigma = vcov_mat
      )
      
      # ===================================================
      # CONVERTER PARA TIBBLE
      # ===================================================
      
      beta_boot <- as.data.frame(beta_boot)
      
      colnames(beta_boot) <- names(beta_hat)
      
      beta_boot <- tibble::as_tibble(beta_boot)
      
      # ===================================================
      # REMOVER INTERCEPTO
      # ===================================================
      
      beta_boot_sem_intercepto <- beta_boot |>
            
            dplyr::select(
                  -dplyr::matches(
                        "^\\(Intercept\\)$"
                  )
            )
      
      # ===================================================
      # ODDS RATIOS BOOTSTRAP
      # ===================================================
      
      or_boot <- beta_boot_sem_intercepto |>
            
            dplyr::mutate(
                  
                  dplyr::across(
                        
                        .cols = everything(),
                        
                        .fns = exp
                  )
            )
      
      # ===================================================
      # LIMITES DO IC
      # ===================================================
      
      alpha_inf <- (1 - nivel_ic) / 2
      
      alpha_sup <- 1 - alpha_inf
      
      # ===================================================
      # RESUMO DOS COEFICIENTES
      # ===================================================
      
      resumo_coef <- purrr::map_dfr(
            
            names(beta_boot_sem_intercepto),
            
            function(v) {
                  
                  valores <- beta_boot_sem_intercepto[[v]]
                  
                  tibble::tibble(
                        
                        Variavel = v,
                        
                        Media =
                              mean(valores),
                        
                        Mediana =
                              median(valores),
                        
                        DP =
                              sd(valores),
                        
                        IC_inf =
                              quantile(
                                    valores,
                                    probs = alpha_inf
                              ),
                        
                        IC_sup =
                              quantile(
                                    valores,
                                    probs = alpha_sup
                              ),
                        
                        Prob_coef_positivo =
                              mean(valores > 0),
                        
                        Prob_coef_negativo =
                              mean(valores < 0)
                  )
            }
      )
      
      # ===================================================
      # RESUMO DOS OR
      # ===================================================
      
      resumo_or <- purrr::map_dfr(
            
            names(or_boot),
            
            function(v) {
                  
                  valores <- or_boot[[v]]
                  
                  tibble::tibble(
                        
                        Variavel = v,
                        
                        OR_medio =
                              mean(valores),
                        
                        OR_mediano =
                              median(valores),
                        
                        DP =
                              sd(valores),
                        
                        IC_inf =
                              quantile(
                                    valores,
                                    probs = alpha_inf
                              ),
                        
                        IC_sup =
                              quantile(
                                    valores,
                                    probs = alpha_sup
                              ),
                        
                        Prob_OR_maior_1 =
                              mean(valores > 1),
                        
                        Prob_OR_menor_1 =
                              mean(valores < 1)
                  )
            }
      )
      
      # ===================================================
      # ESTABILIDADE INFERENCIAL
      # ===================================================
      
      estabilidade <- resumo_or |>
            
            dplyr::mutate(
                  
                  Estabilidade = dplyr::case_when(
                        
                        Prob_OR_maior_1 >= 0.95 |
                              Prob_OR_menor_1 >= 0.95 ~
                              "Muito alta",
                        
                        Prob_OR_maior_1 >= 0.80 |
                              Prob_OR_menor_1 >= 0.80 ~
                              "Alta",
                        
                        Prob_OR_maior_1 >= 0.65 |
                              Prob_OR_menor_1 >= 0.65 ~
                              "Moderada",
                        
                        TRUE ~
                              "Baixa"
                  )
            )
      
      # ===================================================
      # DIAGNÓSTICOS
      # ===================================================
      
      message(
            "\n========== BOOTSTRAP INFERENCIAL =========="
      )
      
      message(
            paste(
                  "Simulações bootstrap:",
                  n_boot
            )
      )
      
      message(
            paste(
                  "Variáveis avaliadas:",
                  ncol(beta_boot_sem_intercepto)
            )
      )
      
      # ===================================================
      # RETORNO
      # ===================================================
      
      return(
            
            list(
                  
                  beta_boot =
                        beta_boot_sem_intercepto,
                  
                  or_boot =
                        or_boot,
                  
                  resumo_coef =
                        resumo_coef,
                  
                  resumo_or =
                        resumo_or,
                  
                  estabilidade =
                        estabilidade,
                  
                  n_boot =
                        n_boot,
                  
                  nivel_ic =
                        nivel_ic
            )
      )
}