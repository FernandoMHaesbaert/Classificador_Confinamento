# =========================================================
# MÓDULO 14 — PIPELINE COMPLETO
# =========================================================
# Objetivo:
# Orquestrar todo o fluxo analítico do sistema.
#
# Responsabilidades:
# - preprocessamento;
# - criação do desfecho;
# - separação por sexo;
# - elastic net;
# - stability selection;
# - modelo Firth;
# - bootstrap inferencial;
# - predição;
# - ROC;
# - interpretabilidade;
# - tabelas;
# - plots.
#
# Este módulo será:
# - núcleo do Shiny;
# - backend principal;
# - ponto único de execução.
# =========================================================

# =========================================================
# FUNÇÃO PRINCIPAL
# =========================================================

executar_pipeline_confinamento <- function(
            
      # ==========================================
      # MACHOS
      # ==========================================
      
      peso_final_machos = 35,
      
      pcf_machos = 15,
      
      rcf_machos = 0.45,
      
      ecc_machos = 3,
      
      acabamento_machos = 3,
      
      conformacao_machos = 3,
      
      # ==========================================
      # FÊMEAS
      # ==========================================
      
      peso_final_femeas = 30,
      
      pcf_femeas = 14,
      
      rcf_femeas = 0.45,
      
      ecc_femeas = 3,
      
      acabamento_femeas = 3,
      
      conformacao_femeas = 3,
      
      # ==========================================
      # PARÂMETROS DO PIPELINE
      # ==========================================
      
      seed = 123,
      
      alpha_elastic = 0.5,
      
      n_boot_stability = 500,
      
      n_boot_inferencial = 500,
      
      verbose = TRUE
) {
      
      # ===================================================
      # SEED
      # ===================================================
      
      set.seed(seed)
      
      # ===================================================
      # LOG AUXILIAR
      # ===================================================
      
      log_mensagem <- function(...) {
            
            if(verbose) {
                  
                  message(...)
            }
      }
      
      # ===================================================
      # CARREGAMENTO
      # ===================================================
      
      log_mensagem(
            "\n================================================="
      )
      
      log_mensagem(
            "PIPELINE — CLASSIFICADOR CONFINAMENTO"
      )
      
      log_mensagem(
            "=================================================\n"
      )

      
      # ===================================================
      # PREPROCESSAMENTO
      # ===================================================
      
      log_mensagem(
            "\n========== PREPROCESSAMENTO =========="
      )
      
      dados_processados <- preprocessar_dados()
      
      # ===================================================
      # SUBGRUPOS
      # ===================================================
      
      log_mensagem(
            "\n========== SUBGRUPOS =========="
      )
      
      subgrupos <- separar_subgrupos(
            dados_processados
      )
      
      femeas <- subgrupos$femeas
      
      machos <- subgrupos$machos
      
      # ===================================================
      # DESFECHO
      # ===================================================
      
      log_mensagem(
            "\n========== DESFECHO =========="
      )
      
      femeas_desfecho <- criar_desfecho(
            
            dados = femeas,
            
            peso_final_min =
                  peso_final_femeas,
            
            pcf_min =
                  pcf_femeas,
            
            rcf_min =
                  rcf_femeas,
            
            ecc_final_min =
                  ecc_femeas,
            
            acabamento_min =
                  acabamento_femeas,
            
            conformacao_min =
                  conformacao_femeas
      )
      
      machos_desfecho <- criar_desfecho(
            
            dados = machos,
            
            peso_final_min =
                  peso_final_machos,
            
            pcf_min =
                  pcf_machos,
            
            rcf_min =
                  rcf_machos,
            
            ecc_final_min =
                  ecc_machos,
            
            acabamento_min =
                  acabamento_machos,
            
            conformacao_min =
                  conformacao_machos
      )
      
      # ===================================================
      # FUNÇÃO AUXILIAR
      # ===================================================
      
      executar_fluxo <- function(
            
            dados_subgrupo,
            
            sexo_label
      ) {
            
            log_mensagem(
                  paste0(
                        "\n========================================\n",
                        "FLUXO ANALÍTICO — ",
                        toupper(sexo_label),
                        "\n========================================"
                  )
            )
            
            # ==============================================
            # ELASTIC NET
            # ==============================================
            
            log_mensagem(
                  "\n--- Elastic Net ---"
            )
            
            elastic <- ajustar_elasticnet(
                  
                  dados =
                        dados_subgrupo,
                  
                  desfecho =
                        "apto_bin",
                  
                  alpha =
                        alpha_elastic,
                  
                  nfolds = 5
            )
            
            # ==============================================
            # STABILITY SELECTION
            # ==============================================
            
            log_mensagem(
                  "\n--- Stability Selection ---"
            )
            
            stability <- executar_stability_selection(
                  
                  dados =
                        dados_subgrupo,
                  
                  usar_lambda_1se = FALSE,
                  
                  desfecho =
                        "apto_bin",
                  
                  alpha =
                        alpha_elastic,
                  
                  n_boot =
                        n_boot_stability
            )
            
            variaveis_finais <-
                  stability$variaveis_finais
            
            if(length(variaveis_finais) == 0) {
                  
                  stop(
                        paste(
                              "Nenhuma variável robusta encontrada:",
                              sexo_label
                        )
                  )
            }
            
            # ==============================================
            # FIRTH
            # ==============================================
            
            log_mensagem(
                  "\n--- Modelo Firth ---"
            )
            
            modelo_firth <- ajustar_firth(
                  
                  dados =
                        dados_subgrupo,
                  
                  variaveis_finais =
                        variaveis_finais
            )
            
            # ==============================================
            # BOOTSTRAP
            # ==============================================
            
            log_mensagem(
                  "\n--- Bootstrap Inferencial ---"
            )
            
            bootstrap <- bootstrap_firth(
                  
                  modelo_firth =
                        modelo_firth$modelo,
                  
                  n_boot =
                        n_boot_inferencial,
                  
                  seed =
                        seed
            )
            
            # ==============================================
            # ROC
            # ==============================================
            
            log_mensagem(
                  "\n--- ROC ---"
            )
            
            roc <- avaliar_roc(
                  
                  modelo_firth =
                        modelo_firth$modelo,
                  
                  dados =
                        dados_subgrupo
            )
            
            # ==============================================
            # INTERPRETABILIDADE
            # ==============================================
            
            log_mensagem(
                  "\n--- Interpretabilidade ---"
            )
            
            interpretabilidade <- interpretar_modelo(
                  
                  modelo_firth =
                        modelo_firth$modelo,
                  
                  bootstrap_resultado =
                        bootstrap,
                  
                  titulo_modelo =
                        sexo_label
            )
            
            # ==============================================
            # TABELAS
            # ==============================================
            
            log_mensagem(
                  "\n--- Tabelas ---"
            )
            
            tabela_or_modelo <- tabela_or(
                  
                  modelo_firth =
                        modelo_firth$modelo
            )
            
            tabela_boot <- tabela_bootstrap(
                  
                  bootstrap_resultado =
                        bootstrap
            )
            
            tabela_roc_modelo <- tabela_roc(
                  
                  roc_resultado =
                        roc
            )
            
            tabela_estab <- tabela_estabilidade(
                  
                  stability_resultado =
                        stability
            )
            
            # ==============================================
            # PREDIÇÕES
            # ==============================================
            
            log_mensagem(
                  "\n--- Predições ---"
            )
            
            variavel_predicao <- variaveis_finais[1]
            
            predicoes <- gerar_predicoes(
                  
                  modelo_firth =
                        modelo_firth$modelo,
                  
                  dados =
                        dados_subgrupo,
                  
                  variavel_alvo =
                        variavel_predicao,
                  
                  n_pontos = 100,
                  
                  n_boot =
                        n_boot_inferencial
            )
            # ==============================================
            # PLOTS
            # ==============================================
            
            log_mensagem(
                  "\n--- Plots ---"
            )
            
            grafico_roc <- plot_roc(
                  
                  roc_resultado =
                        roc,
                  
                  titulo =
                        paste(
                              "ROC |",
                              sexo_label
                        )
            )
            
            grafico_forest <- plot_forest(
                  
                  tabela_or =
                        tabela_or_modelo,
                  
                  titulo =
                        paste(
                              "Forest Plot |",
                              sexo_label
                        )
            )
            
            grafico_importancia <- plot_importancia(
                  
                  tabela_interpretacao =
                        interpretabilidade$tabela,
                  
                  titulo =
                        paste(
                              "Importância |",
                              sexo_label
                        )
            )
            
            grafico_calibracao <- plot_calibracao(
                  
                  roc_resultado =
                        roc,
                  
                  titulo =
                        paste(
                              "Calibração |",
                              sexo_label
                        )
            )
            
            grafico_predicao <- plot_predicao_marginal(
                  
                  predicoes =
                        predicoes,
                  
                  variavel_alvo =
                        variavel_predicao,
                  
                  titulo =
                        paste(
                              "Predição |",
                              sexo_label
                        )
            )
            
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
            
            # ==============================================
            # RETORNO
            # ==============================================
            
            return(
                  list(
                        
                        dados =
                              dados_subgrupo,
                        
                        elasticnet =
                              elastic,
                        
                        stability =
                              stability,
                        
                        variaveis_finais =
                              variaveis_finais,
                        
                        modelo_firth =
                              modelo_firth,
                        
                        bootstrap =
                              bootstrap,
                        
                        roc =
                              roc,
                        
                        interpretabilidade =
                              interpretabilidade,
                        
                        predicoes =
                              predicoes,
                        
                        tabelas =
                              list(
                                    
                                    or =
                                          tabela_or_modelo,
                                    
                                    bootstrap =
                                          tabela_boot,
                                    
                                    roc =
                                          tabela_roc_modelo,
                                    
                                    estabilidade =
                                          tabela_estab
                              ),
                        
                        plots =
                              list(
                                    
                                    roc =
                                          grafico_roc,
                                    
                                    forest =
                                          grafico_forest,
                                    
                                    importancia =
                                          grafico_importancia,
                                    
                                    calibracao =
                                          grafico_calibracao,
                                    
                                    predicao =
                                          grafico_predicao,
                                    
                                    painel =
                                          painel
                              )
                  )
            )
      }
      
      # ===================================================
      # EXECUÇÃO — FÊMEAS
      # ===================================================
      
      resultados_femeas <- executar_fluxo(
            
            dados_subgrupo =
                  femeas_desfecho,
            
            sexo_label =
                  "Fêmeas"
      )
      
      # ===================================================
      # EXECUÇÃO — MACHOS
      # ===================================================
      
      resultados_machos <- executar_fluxo(
            
            dados_subgrupo =
                  machos_desfecho,
            
            sexo_label =
                  "Machos"
      )
      
      # ===================================================
      # FINALIZAÇÃO
      # ===================================================
      
      log_mensagem(
            "\n================================================="
      )
      
      log_mensagem(
            "PIPELINE CONCLUÍDO COM SUCESSO"
      )
      
      log_mensagem(
            "=================================================\n"
      )
      
      # ===================================================
      # RETORNO FINAL
      # ===================================================
      
      return(
            list(
                  
                  dados_processados =
                        dados_processados,
                  
                  subgrupos =
                        subgrupos,
                  
                  femeas =
                        resultados_femeas,
                  
                  machos =
                        resultados_machos,
                  
                  parametros =
                        list(
                              
                              alpha_elastic =
                                    alpha_elastic,
                              
                              n_boot_stability =
                                    n_boot_stability,
                              
                              n_boot_inferencial =
                                    n_boot_inferencial,
                              
                              seed =
                                    seed
                        )
            )
      )
}