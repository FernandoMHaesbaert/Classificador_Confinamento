# =========================================================
# APP — CLASSIFICADOR CONFINAMENTO
# =========================================================
# Aplicação Shiny
# Versão operacional inicial
# =========================================================

# =========================================================
# GLOBAL
# =========================================================

source("global.R")

# =========================================================
# UI
# =========================================================

ui <- shiny::fluidPage(
      
      # ===================================================
      # TÍTULO
      # ===================================================
      
      shiny::titlePanel(
            title = "Classificador de Aptidão ao Confinamento"
      ),
      
      # ===================================================
      # LAYOUT PRINCIPAL
      # ===================================================
      
      shiny::sidebarLayout(
            
            # ==============================================
            # SIDEBAR
            # ==============================================
            
            shiny::sidebarPanel(
                  
                  width = 3,
                  
                  # ----------------------------------------
                  # CRITÉRIOS — MACHOS
                  # ----------------------------------------
                  
                  shiny::h3(
                        "Machos"
                  ),
                  
                  shiny::sliderInput(
                        
                        inputId = "peso_final_machos",
                        
                        label = "Peso final mínimo (kg)",
                        
                        min = 30,
                        
                        max = 38,
                        
                        value = 35,
                        
                        step = 0.5
                  ),
                  
                  shiny::sliderInput(
                        
                        inputId = "pcf_machos",
                        
                        label = "PCF mínimo (kg)",
                        
                        min = 13,
                        
                        max = 16,
                        
                        value = 15,
                        
                        step = 0.5
                  ),
                  
                  shiny::sliderInput(
                        
                        inputId = "rcf_machos",
                        
                        label = "RCF mínimo (%)",
                        
                        min = 42,
                        
                        max = 48,
                        
                        value = 45,
                        
                        step = 0.5
                  ),
                  
                  shiny::selectInput(
                        
                        inputId = "ecc_machos",
                        
                        label = "ECC mínimo",
                        
                        choices = c(
                              3.0,
                              3.5
                        ),
                        
                        selected = 3.0
                  ),
                  
                  shiny::selectInput(
                        
                        inputId = "acabamento_machos",
                        
                        label = "Acabamento mínimo",
                        
                        choices = c(
                              3.0,
                              3.5
                        ),
                        
                        selected = 3.0
                  ),
                  
                  shiny::selectInput(
                        
                        inputId = "conformacao_machos",
                        
                        label = "Conformação mínima",
                        
                        choices = c(
                              3.0,
                              3.5
                        ),
                        
                        selected = 3.0
                  ),
                  
                  shiny::hr(),
                  
                  # ----------------------------------------
                  # CRITÉRIOS — FÊMEAS
                  # ----------------------------------------
                  
                  shiny::h3(
                        "Fêmeas"
                  ),
                  
                  shiny::sliderInput(
                        
                        inputId = "peso_final_femeas",
                        
                        label = "Peso final mínimo (kg)",
                        
                        min = 28,
                        
                        max = 32,
                        
                        value = 30,
                        
                        step = 0.5
                  ),
                  
                  shiny::sliderInput(
                        
                        inputId = "pcf_femeas",
                        
                        label = "PCF mínimo (kg)",
                        
                        min = 12,
                        
                        max = 15,
                        
                        value = 14,
                        
                        step = 0.5
                  ),
                  
                  shiny::sliderInput(
                        
                        inputId = "rcf_femeas",
                        
                        label = "RCF mínimo (%)",
                        
                        min = 42,
                        
                        max = 48,
                        
                        value = 45,
                        
                        step = 0.5
                  ),
                  
                  shiny::selectInput(
                        
                        inputId = "ecc_femeas",
                        
                        label = "ECC mínimo",
                        
                        choices = c(
                              3.0,
                              3.5
                        ),
                        
                        selected = 3.0
                  ),
                  
                  shiny::selectInput(
                        
                        inputId = "acabamento_femeas",
                        
                        label = "Acabamento mínimo",
                        
                        choices = c(
                              3.0,
                              3.5
                        ),
                        
                        selected = 3.0
                  ),
                  
                  shiny::selectInput(
                        
                        inputId = "conformacao_femeas",
                        
                        label = "Conformação mínima",
                        
                        choices = c(
                              3.0,
                              3.5
                        ),
                        
                        selected = 3.0
                  ),
                  
                  shiny::hr(),
                  
                  # ----------------------------------------
                  # EXECUÇÃO
                  # ----------------------------------------
                  
                  shiny::actionButton(
                        
                        inputId = "executar",
                        
                        label = "Executar Classificação",
                        
                        class = "btn-primary btn-lg"
                  ),
                  
                  shiny::br(),
                  
                  shiny::br(),
                  
                  shiny::verbatimTextOutput(
                        "status_execucao"
                  )
            ),
            
            # ==============================================
            # MAIN PANEL
            # ==============================================
            
            shiny::mainPanel(
                  
                  width = 9,
                  
                  shiny::tabsetPanel(
                        
                        # ==================================
                        # RESUMO
                        # ==================================
                        
                        shiny::tabPanel(
                              
                              title = "Resumo",
                              
                              shiny::br(),
                              
                              shiny::fluidRow(
                                    
                                    shiny::column(
                                          
                                          width = 6,
                                          
                                          shiny::h3(
                                                "Fêmeas"
                                          ),
                                          
                                          shiny::tableOutput(
                                                "metricas_femeas"
                                          )
                                    ),
                                    
                                    shiny::column(
                                          
                                          width = 6,
                                          
                                          shiny::h3(
                                                "Machos"
                                          ),
                                          
                                          shiny::tableOutput(
                                                "metricas_machos"
                                          )
                                    )
                              )
                        ),
                        
                        # ==================================
                        # ROC
                        # ==================================
                        
                        shiny::tabPanel(
                              
                              title = "ROC",
                              
                              shiny::br(),
                              
                              shiny::fluidRow(
                                    
                                    shiny::column(
                                          
                                          width = 6,
                                          
                                          shiny::plotOutput(
                                                "roc_femeas",
                                                height = "500px"
                                          )
                                    ),
                                    
                                    shiny::column(
                                          
                                          width = 6,
                                          
                                          shiny::plotOutput(
                                                "roc_machos",
                                                height = "500px"
                                          )
                                    )
                              )
                        ),
                        
                        # ==================================
                        # INTERPRETABILIDADE
                        # ==================================
                        
                        shiny::tabPanel(
                              
                              title = "Interpretabilidade",
                              
                              shiny::br(),
                              
                              shiny::fluidRow(
                                    
                                    shiny::column(
                                          
                                          width = 6,
                                          
                                          shiny::plotOutput(
                                                "forest_femeas",
                                                height = "600px"
                                          )
                                    ),
                                    
                                    shiny::column(
                                          
                                          width = 6,
                                          
                                          shiny::plotOutput(
                                                "forest_machos",
                                                height = "600px"
                                          )
                                    )
                              )
                        ),
                        
                        # ==================================
                        # PREDIÇÕES
                        # ==================================
                        
                        shiny::tabPanel(
                              
                              title = "Predições",
                              
                              shiny::br(),
                              
                              shiny::fluidRow(
                                    
                                    shiny::column(
                                          
                                          width = 6,
                                          
                                          shiny::plotOutput(
                                                "predicao_femeas",
                                                height = "500px"
                                          )
                                    ),
                                    
                                    shiny::column(
                                          
                                          width = 6,
                                          
                                          shiny::plotOutput(
                                                "predicao_machos",
                                                height = "500px"
                                          )
                                    )
                              )
                        ),
                        
                        # ==================================
                        # TABELAS
                        # ==================================
                        
                        shiny::tabPanel(
                              
                              title = "Tabelas",
                              
                              shiny::br(),
                              
                              shiny::h3(
                                    "Odds Ratios — Fêmeas"
                              ),
                              
                              shiny::tableOutput(
                                    "tabela_or_femeas"
                              ),
                              
                              shiny::hr(),
                              
                              shiny::h3(
                                    "Odds Ratios — Machos"
                              ),
                              
                              shiny::tableOutput(
                                    "tabela_or_machos"
                              )
                        )
                  )
            )
      )
)

# =========================================================
# SERVER
# =========================================================

server <- function(
            
      input,
      
      output,
      
      session
) {
      
      # ===================================================
      # STATUS
      # ===================================================
      
      output$status_execucao <- shiny::renderText({
            
            "Sistema pronto para execução."
      })
      
      # ===================================================
      # PIPELINE
      # ===================================================
      
      resultado_pipeline <- shiny::eventReactive(
            
            input$executar,
            
            {
                  
                  shiny::withProgress(
                        
                        message = "Executando pipeline analítico...",
                        
                        value = 0,
                        
                        {
                              
                              shiny::incProgress(
                                    0.2,
                                    detail = "Inicializando..."
                              )
                              
                              resultado <- tryCatch(
                                    
                                    {
                                          
                                          executar_pipeline_confinamento(
                                    
                                    # ==================
                                    # MACHOS
                                    # ==================
                                    
                                    peso_final_machos =
                                          input$peso_final_machos,
                                    
                                    pcf_machos =
                                          input$pcf_machos,
                                    
                                    rcf_machos =
                                          input$rcf_machos / 100,
                                    
                                    ecc_machos =
                                          as.numeric(
                                                input$ecc_machos
                                          ),
                                    
                                    acabamento_machos =
                                          as.numeric(
                                                input$acabamento_machos
                                          ),
                                    
                                    conformacao_machos =
                                          as.numeric(
                                                input$conformacao_machos
                                          ),
                                    
                                    # ==================
                                    # FÊMEAS
                                    # ==================
                                    
                                    peso_final_femeas =
                                          input$peso_final_femeas,
                                    
                                    pcf_femeas =
                                          input$pcf_femeas,
                                    
                                    rcf_femeas =
                                          input$rcf_femeas / 100,
                                    
                                    ecc_femeas =
                                          as.numeric(
                                                input$ecc_femeas
                                          ),
                                    
                                    acabamento_femeas =
                                          as.numeric(
                                                input$acabamento_femeas
                                          ),
                                    
                                    conformacao_femeas =
                                          as.numeric(
                                                input$conformacao_femeas
                                          ),
                                    
                                    # ==================
                                    # PARÂMETROS FIXOS
                                    # ==================
                                    
                                    seed = CONFIG$seed,
                                    
                                    alpha_elastic =
                                          CONFIG$alpha_elastic,
                                    
                                    n_boot_stability =
                                          CONFIG$n_boot_stability,
                                    
                                    n_boot_inferencial =
                                          CONFIG$n_boot_inferencial,
                                    
                                    verbose = FALSE
                              )
                                    },
                              
                              error = function(e) {
                                    
                                    print("================================")
                                    print("ERRO NO PIPELINE")
                                    print("================================")
                                    
                                    print(e)
                                    
                                    return(NULL)
                              }
                  )
                        }
                  )
            }
      )
      
      # ===================================================
      # MÉTRICAS
      # ===================================================
      
      output$metricas_femeas <- shiny::renderTable({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            tibble::tibble(
                  
                  Métrica = c(
                        "AUC",
                        "Sensibilidade",
                        "Especificidade",
                        "Acurácia"
                  ),
                  
                  Valor = c(
                        
                        round(
                              resultado_pipeline()$
                                    femeas$
                                    roc$
                                    auc,
                              3
                        ),
                        
                        round(
                              resultado_pipeline()$
                                    femeas$
                                    roc$
                                    sensibilidade,
                              3
                        ),
                        
                        round(
                              resultado_pipeline()$
                                    femeas$
                                    roc$
                                    especificidade,
                              3
                        ),
                        
                        round(
                              resultado_pipeline()$
                                    femeas$
                                    roc$
                                    acuracia,
                              3
                        )
                  )
            )
      })
      
      output$metricas_machos <- shiny::renderTable({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            tibble::tibble(
                  
                  Métrica = c(
                        "AUC",
                        "Sensibilidade",
                        "Especificidade",
                        "Acurácia"
                  ),
                  
                  Valor = c(
                        
                        round(
                              resultado_pipeline()$
                                    machos$
                                    roc$
                                    auc,
                              3
                        ),
                        
                        round(
                              resultado_pipeline()$
                                    machos$
                                    roc$
                                    sensibilidade,
                              3
                        ),
                        
                        round(
                              resultado_pipeline()$
                                    machos$
                                    roc$
                                    especificidade,
                              3
                        ),
                        
                        round(
                              resultado_pipeline()$
                                    machos$
                                    roc$
                                    acuracia,
                              3
                        )
                  )
            )
      })
      
      # ===================================================
      # ROC
      # ===================================================
      
      output$roc_femeas <- shiny::renderPlot({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            print(
                  resultado_pipeline()$
                        femeas$
                        plots$
                        roc
            )
      })
      
      output$roc_machos <- shiny::renderPlot({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            print(
                  resultado_pipeline()$
                        machos$
                        plots$
                        roc
            )
      })
      
      # ===================================================
      # FOREST
      # ===================================================
      
      output$forest_femeas <- shiny::renderPlot({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            print(
                  resultado_pipeline()$
                        femeas$
                        plots$
                        forest
            )
      })
      
      output$forest_machos <- shiny::renderPlot({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            print(
                  resultado_pipeline()$
                        machos$
                        plots$
                        forest
            )
      })
      
      # ===================================================
      # PREDIÇÕES
      # ===================================================
      
      output$predicao_femeas <- shiny::renderPlot({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            print(
                  resultado_pipeline()$
                        femeas$
                        plots$
                        predicao
            )
      })
      
      output$predicao_machos <- shiny::renderPlot({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            print(
                  resultado_pipeline()$
                        machos$
                        plots$
                        predicao
            )
      })
      
      # ===================================================
      # TABELAS
      # ===================================================
      
      output$tabela_or_femeas <- shiny::renderTable({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            resultado_pipeline()$
                  femeas$
                  tabelas$
                  or
      })
      
      output$tabela_or_machos <- shiny::renderTable({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            resultado_pipeline()$
                  machos$
                  tabelas$
                  or
      })
}

# =========================================================
# EXECUÇÃO
# =========================================================

shiny::shinyApp(
      
      ui = ui,
      
      server = server
)