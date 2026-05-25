# =========================================================
# MÓDULO 08 — PREDIÇÃO
# =========================================================
# Objetivo:
# Gerar probabilidades preditas robustas com
# propagação bootstrap da incerteza.
#
# O módulo:
# - calcula probabilidades preditas;
# - gera curvas marginais;
# - produz IC bootstrap;
# - suporta variáveis contínuas e categóricas;
# - evita dependência frágil do ggeffects.
#
# Estrutura:
# 1. criar_grade_predicao()
# 2. bootstrap_marginal()
# 3. gerar_predicoes()
# =========================================================

# =========================================================
# FUNÇÃO 1 — CRIAR GRADE DE PREDIÇÃO
# =========================================================

criar_grade_predicao <- function(
            
      dados,
      
      variavel_alvo,
      
      n_pontos = 100
) {
      
      # ===================================================
      # VERIFICAÇÕES
      # ===================================================
      
      if(!variavel_alvo %in% names(dados)) {
            
            stop(
                  "Variável alvo não encontrada."
            )
      }
      
      # ===================================================
      # VARIÁVEL CONTÍNUA
      # ===================================================
      
      if(is.numeric(dados[[variavel_alvo]])) {
            
            grade <- tibble::tibble(
                  
                  !!variavel_alvo := seq(
                        
                        min(
                              dados[[variavel_alvo]],
                              na.rm = TRUE
                        ),
                        
                        max(
                              dados[[variavel_alvo]],
                              na.rm = TRUE
                        ),
                        
                        length.out = n_pontos
                  )
            )
            
      } else {
            
            # ==============================================
            # VARIÁVEL CATEGÓRICA
            # ==============================================
            
            grade <- tibble::tibble(
                  
                  !!variavel_alvo :=
                        unique(
                              dados[[variavel_alvo]]
                        )
            )
      }
      
      # ===================================================
      # DEMAIS VARIÁVEIS FIXADAS
      # ===================================================
      
      outras_vars <- setdiff(
            names(dados),
            variavel_alvo
      )
      
      for(v in outras_vars) {
            
            # -----------------------------------------------
            # Numéricas
            # -----------------------------------------------
            
            if(is.numeric(dados[[v]])) {
                  
                  grade[[v]] <- mean(
                        dados[[v]],
                        na.rm = TRUE
                  )
                  
            } else {
                  
                  # ----------------------------------------
                  # Fatores
                  # ----------------------------------------
                  
                  moda <- names(
                        sort(
                              table(dados[[v]]),
                              decreasing = TRUE
                        )
                  )[1]
                  
                  grade[[v]] <- moda
                  
                  # Preservar níveis
                  
                  if(is.factor(dados[[v]])) {
                        
                        grade[[v]] <- factor(
                              
                              grade[[v]],
                              
                              levels =
                                    levels(dados[[v]])
                        )
                  }
            }
      }
      
      return(grade)
}

# =========================================================
# FUNÇÃO 2 — BOOTSTRAP MARGINAL
# =========================================================

bootstrap_marginal <- function(
            
      modelo_firth,
      
      novos_dados,
      
      n_boot = 5000,
      
      nivel_ic = 0.95,
      
      seed = 1234
) {
      
      # ===================================================
      # REPRODUTIBILIDADE
      # ===================================================
      
      set.seed(seed)
      
      # ===================================================
      # VERIFICAÇÃO
      # ===================================================
      
      if(!inherits(modelo_firth, "logistf")) {
            
            stop(
                  "Modelo deve ser da classe logistf."
            )
      }
      
      # ===================================================
      # COEFICIENTES
      # ===================================================
      
      beta_hat <- coef(modelo_firth)
      
      # ===================================================
      # MATRIZ VCOV
      # ===================================================
      
      vcov_mat <- vcov(modelo_firth)
      
      # ===================================================
      # MATRIZ DO MODELO
      # ===================================================
      
      x <- model.matrix(
            
            formula(modelo_firth),
            
            data = novos_dados
      )
      
      # ===================================================
      # SIMULAÇÃO DOS COEFICIENTES
      # ===================================================
      
      beta_boot <- MASS::mvrnorm(
            
            n = n_boot,
            
            mu = beta_hat,
            
            Sigma = vcov_mat
      )
      
      # ===================================================
      # PREDITOR LINEAR
      # ===================================================
      
      eta_boot <- x %*% t(beta_boot)
      
      # ===================================================
      # FUNÇÃO LOGÍSTICA
      # ===================================================
      
      prob_boot <- 1 / (1 + exp(-eta_boot))
      
      # ===================================================
      # LIMITES
      # ===================================================
      
      alpha_inf <- (1 - nivel_ic) / 2
      
      alpha_sup <- 1 - alpha_inf
      
      # ===================================================
      # RESUMO DAS PROBABILIDADES
      # ===================================================
      
      resumo_prob <- tibble::tibble(
            
            prob_media =
                  apply(
                        prob_boot,
                        1,
                        mean
                  ),
            
            prob_mediana =
                  apply(
                        prob_boot,
                        1,
                        median
                  ),
            
            IC_inf =
                  apply(
                        prob_boot,
                        1,
                        quantile,
                        probs = alpha_inf
                  ),
            
            IC_sup =
                  apply(
                        prob_boot,
                        1,
                        quantile,
                        probs = alpha_sup
                  )
      )
      
      # ===================================================
      # CONCATENAR GRADE
      # ===================================================
      
      resultado <- dplyr::bind_cols(
            
            novos_dados,
            
            resumo_prob
      )
      
      return(resultado)
}

# =========================================================
# FUNÇÃO 3 — GERAR PREDIÇÕES
# =========================================================

gerar_predicoes <- function(
            
      modelo_firth,
      
      dados,
      
      variavel_alvo,
      
      n_pontos = 100,
      
      n_boot = 5000
) {
      
      # ===================================================
      # GRADE
      # ===================================================
      
      grade <- criar_grade_predicao(
            
            dados = dados,
            
            variavel_alvo =
                  variavel_alvo,
            
            n_pontos = n_pontos
      )
      
      # ===================================================
      # BOOTSTRAP
      # ===================================================
      
      predicoes <- bootstrap_marginal(
            
            modelo_firth =
                  modelo_firth,
            
            novos_dados =
                  grade,
            
            n_boot = n_boot
      )
      
      return(predicoes)
}