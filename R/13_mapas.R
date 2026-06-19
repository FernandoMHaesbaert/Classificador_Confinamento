# =========================================================
# MÓDULO 13 — MAPAS
# =========================================================
gerar_mapa_principal <- function(resultado){
      # =====================================================
      # OBJETOS
      # =====================================================
      modelo <- resultado$modelo_firth$modelo
      cat("\nTERMOS DO MODELO:\n")
      print(
            attr(
                  terms(modelo),
                  "term.labels"
            )
      )
      dados <- resultado$dados
      # =====================================================
      # GRID
      # =====================================================
      grid <- expand.grid(
            ecc_inicial = seq(1, 5, by = 0.05),
            peso_inicial = seq(
                        floor(min(dados$peso_inicial, na.rm = TRUE)),
                        ceiling(max(dados$peso_inicial, na.rm = TRUE)),
                        by = 0.10
                  )
      )
      
      # =====================================================
      # FIXAR VARIÁVEIS AUSENTES
      # =====================================================
      termos_modelo <- attr(
            terms(modelo),
            "term.labels"
      )
      for(v in termos_modelo){
            if(!v %in% names(grid)){
                  if(v %in% names(dados)){
                        if(is.numeric(dados[[v]])){
                              grid[[v]] <- median(
                                    dados[[v]],
                                    na.rm = TRUE
                              )
                        } else {
                              grid[[v]] <- names(
                                    sort(
                                          table(dados[[v]]),
                                          decreasing = TRUE
                                    )
                              )[1]
                        }
                  }
            }
      }
      # =====================================================
      # DIAGNÓSTICO
      # =====================================================
      cat("\nTERMOS DO MODELO:\n")
      print(termos_modelo)
      
      cat("\nCOLUNAS DO GRID:\n")
      print(names(grid))
      
      # =====================================================
      # PROTEÇÃO
      # =====================================================
      grid <- grid |>
            dplyr::select(
                  dplyr::all_of(
                        unique(
                              c(
                                    termos_modelo,
                                    "ecc_inicial",
                                    "peso_inicial"
                              )
                        )
                  )
            )
      
      # =====================================================
      # PREDIÇÃO
      # =====================================================
      grid$prob_apto <- tryCatch(
            predict(
                  modelo,
                  newdata = grid,
                  type = "response"
            ),
            error = function(e){
                  cat(
                        "\nERRO NO PREDICT:\n"
                  )
                  print(e)
                  return(
                        rep(
                              NA_real_,
                              nrow(grid)
                        )
                  )
            }
      )
      
      # =====================================================
      # REMOVER NAs
      # =====================================================
      grid <- grid |>
            dplyr::filter(
                  !is.na(prob_apto)
            )
      if(nrow(grid) == 0){
            return(
                  ggplot2::ggplot() +
                        ggplot2::annotate(
                              "text",
                              x = 0,
                              y = 0,
                              label =
                                    "Não foi possível gerar o mapa",
                              size = 8
                        ) +
                        ggplot2::theme_void()
            )
      }
      
      # =====================================================
      # CLASSIFICAÇÃO
      # =====================================================
      grid <- grid |>
            dplyr::mutate(
                  faixa =
                        dplyr::ntile(
                              prob_apto,
                              4
                        )
            ) |>
            dplyr::mutate(
                  classe =
                        dplyr::case_when(faixa == 1 ~ "Não recomendado",
                              faixa == 2 ~ "Pouco recomendado",
                              faixa == 3 ~ "Apto potencial",
                              TRUE ~ "Apto prioritário"))
      
      # =====================================================
      # MAPA
      # =====================================================
      ggplot2::ggplot(grid, ggplot2::aes(x = peso_inicial, y = ecc_inicial, fill = classe)) +
            ggplot2::geom_tile(alpha = 0.82) +
            ggplot2::geom_point(data = dados, 
                                ggplot2::aes(x = peso_inicial, y = ecc_inicial, shape = factor(apto_bin)),
                  inherit.aes = FALSE, color = "black", size = 2.5, stroke = 0.8) +
            # Grades desenhadas SOBRE os tiles
            ggplot2::geom_vline(xintercept = seq(19, 34, by = 1),
                  color = "gray30", linewidth = 0.3, linetype = "solid") +
            ggplot2::geom_hline(yintercept = seq(1, 5, by = 0.5), color = "gray30", linewidth = 0.3, linetype = "solid") +
            ggplot2::scale_fill_manual(values = c(
                        "Não recomendado"  = "#C0392B",
                        "Pouco recomendado" = "#F39C12",
                        "Apto potencial"   = "#7DCEA0",
                        "Apto prioritário" = "#009639")) +
            ggplot2::scale_shape_manual(values = c(1, 16),labels = c("Não apto", "Apto")) +
            ggplot2::scale_x_continuous(limits = c(19, 34), breaks = seq(19, 34, by = 1)) +
            ggplot2::scale_y_continuous(limits = c(1, 5),  breaks = seq(1, 5,  by = 0.5)) +
            ggplot2::labs(
                  title    = "Mapa de Seleção de Animais",
                  subtitle = "Peso Inicial × ECC Inicial",
                  x        = "Peso inicial (kg)",
                  y        = "ECC inicial",
                  fill     = "Classificação",
                  shape    = "Observação") +
            # theme_classic() PRIMEIRO, customizações DEPOIS
            ggplot2::theme_classic() +
            ggplot2::theme(
                  legend.position  = "bottom",
                  plot.title       = ggplot2::element_text(face = "bold", size = 16),
                  plot.subtitle    = ggplot2::element_text(size = 11),
                  axis.line        = ggplot2::element_line(color = "black", linewidth = 0.8))
}