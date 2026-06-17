# =========================================================
# APP - CLASSIFICADOR CONFINAMENTO
# =========================================================
# Aplicacao Shiny
# Interface operacional APTA-Ov
# =========================================================

# =========================================================
# GLOBAL
# =========================================================

source("global.R")

# =========================================================
# CONFIGURACAO DA INTERFACE
# =========================================================

criterios_confinamento <- list(
      
      pcf = list(
            rotulo = "Peso carcaca fria",
            operador = ">=",
            unidade = "kg",
            minimo = 12,
            maximo = 18,
            valor = 15,
            passo = 0.5,
            ativo = TRUE
      ),
      
      peso_final = list(
            rotulo = "Peso final",
            operador = ">=",
            unidade = "kg",
            minimo = 30,
            maximo = 40,
            valor = 35,
            passo = 0.5,
            ativo = FALSE
      ),
      
      rcf = list(
            rotulo = "Rend. carc. fria",
            operador = ">=",
            unidade = "%",
            minimo = 45,
            maximo = 55,
            valor = 48,
            passo = 0.5,
            ativo = FALSE
      ),
      
      acabamento = list(
            rotulo = "Acabamento",
            operador = ">=",
            unidade = "pontos",
            minimo = 2,
            maximo = 5,
            valor = 3,
            passo = 0.5,
            ativo = FALSE
      ),
      
      ecc = list(
            rotulo = "ECC",
            operador = ">=",
            unidade = "pontos",
            minimo = 2,
            maximo = 5,
            valor = 3,
            passo = 0.5,
            ativo = FALSE
      ),
      
      conformacao = list(
            rotulo = "Conformacao",
            operador = ">=",
            unidade = "pontos",
            minimo = 2,
            maximo = 5,
            valor = 3,
            passo = 0.5,
            ativo = TRUE
      )
)

criar_linha_criterio <- function(
      
      id,
      
      criterio
) {
      
      shiny::tags$div(
            
            class = "criterio-linha",
            
            shiny::tags$div(
                  
                  class = "criterio-check",
                  
                  shiny::checkboxInput(
                        
                        inputId = paste0("usar_", id),
                        
                        label = NULL,
                        
                        value = criterio$ativo
                  )
            ),
            
            shiny::tags$div(
                  
                  class = "criterio-nome",
                  
                  shiny::tags$span(
                        criterio$rotulo
                  )
            ),
            
            shiny::tags$div(
                  
                  class = "criterio-meta",
                  
                  shiny::tags$strong(
                        criterio$operador
                  ),
                  
                  shiny::tags$span(
                        paste(
                              criterio$valor,
                              criterio$unidade
                        )
                  )
            ),
            
            shiny::tags$div(
                  
                  class = "criterio-slider",
                  
                  shiny::sliderInput(
                        
                        inputId = paste0("valor_", id),
                        
                        label = NULL,
                        
                        min = criterio$minimo,
                        
                        max = criterio$maximo,
                        
                        value = criterio$valor,
                        
                        step = criterio$passo,
                        
                        ticks = FALSE
                  )
            )
      )
}

# =========================================================
# UI
# =========================================================

ui <- shiny::fluidPage(
      
      shiny::tags$head(
            
            shiny::tags$style(
                  shiny::HTML(
                        "
                        body {
                              background: #eaf5ed;
                              color: #12351f;
                              font-family: 'Segoe UI', Arial, sans-serif;
                        }
                        .container-fluid {
                              max-width: 1220px;
                        }
                        .app-title {
                              color: #3f73c8;
                              font-weight: 800;
                              text-align: center;
                              text-transform: uppercase;
                              margin-top: 12px;
                              margin-bottom: 0;
                              letter-spacing: 0;
                        }
                        .app-subtitle {
                              color: #3f73c8;
                              font-weight: 800;
                              text-align: center;
                              margin-top: 0;
                              margin-bottom: 10px;
                        }
                        .apta-board {
                              background: #ffffff;
                              border: 1px solid #d9e6dc;
                              padding: 18px;
                              margin-bottom: 18px;
                        }
                        .fluxo-coluna {
                              min-height: 520px;
                              position: relative;
                        }
                        .bloco-verde,
                        .bloco-azul,
                        .bloco-saida {
                              border-radius: 6px;
                              font-weight: 800;
                              text-align: center;
                        }
                        .bloco-verde {
                              border: 2px solid #0e5428;
                              color: #0e5428;
                              background: #f8fff9;
                              padding: 15px 12px;
                              margin-bottom: 18px;
                        }
                        .bloco-azul {
                              background: #062d78;
                              color: #ffffff;
                              padding: 24px 12px;
                              font-size: 28px;
                              margin: 32px 0 22px 0;
                        }
                        .bloco-saida {
                              border: 2px solid #0b2f86;
                              color: #0b2f86;
                              background: #ffffff;
                              padding: 12px;
                              margin-bottom: 16px;
                        }
                        .seta {
                              color: #0e5428;
                              font-size: 34px;
                              font-weight: 800;
                              text-align: center;
                              line-height: 1;
                              margin: -8px 0 10px 0;
                        }
                        .painel-controle {
                              border: 2px solid #0e5428;
                              border-radius: 6px;
                              padding: 12px 14px;
                              background: #fbfffc;
                        }
                        .painel-controle h3,
                        .painel-controle h4 {
                              color: #0e5428;
                              font-weight: 800;
                              text-transform: uppercase;
                              margin-top: 0;
                        }
                        .sexo-box {
                              border: 2px solid #0e5428;
                              border-radius: 6px;
                              padding: 12px;
                              background: #ffffff;
                              min-height: 170px;
                        }
                        .criterio-cabecalho,
                        .criterio-linha {
                              display: grid;
                              grid-template-columns: 42px 1.3fr 95px 1.8fr;
                              gap: 8px;
                              align-items: center;
                        }
                        .criterio-cabecalho {
                              color: #0e5428;
                              font-weight: 800;
                              text-transform: uppercase;
                              border-bottom: 1px solid #c9dfcf;
                              padding-bottom: 6px;
                              margin-bottom: 6px;
                        }
                        .criterio-linha {
                              border-bottom: 1px solid #eef3ef;
                              min-height: 52px;
                        }
                        .criterio-check .checkbox,
                        .criterio-slider .form-group {
                              margin: 0;
                        }
                        .criterio-nome {
                              font-weight: 700;
                              text-transform: uppercase;
                              font-size: 13px;
                        }
                        .criterio-meta {
                              display: flex;
                              gap: 8px;
                              justify-content: flex-start;
                              white-space: nowrap;
                        }
                        .btn-primary {
                              background: #0b2f86;
                              border-color: #0b2f86;
                              font-weight: 800;
                        }
                        .nav-tabs > li > a {
                              color: #0b2f86;
                              font-weight: 700;
                        }
                        .table {
                              font-size: 14px;
                        }
                        @media (max-width: 900px) {
                              .criterio-cabecalho,
                              .criterio-linha {
                                    grid-template-columns: 36px 1fr;
                              }
                              .criterio-meta,
                              .criterio-slider {
                                    grid-column: 2;
                              }
                        }
                        "
                  )
            )
      ),
      
      shiny::h1(
            
            class = "app-title",
            
            "Modelo de Predicao de Aptidao ao Confinamento"
      ),
      
      shiny::h2(
            
            class = "app-subtitle",
            
            "APTA-Ov"
      ),
      
      shiny::fluidRow(
            
            class = "apta-board",
            
            shiny::column(
                  
                  width = 3,
                  
                  class = "fluxo-coluna",
                  
                  shiny::div(
                        class = "bloco-verde",
                        "CAMINHAO"
                  ),
                  
                  shiny::div(
                        class = "seta",
                        "↓"
                  ),
                  
                  shiny::div(
                        class = "bloco-verde",
                        "RECEBIMENTO DOS ANIMAIS"
                  ),
                  
                  shiny::div(
                        class = "seta",
                        "↓"
                  ),
                  
                  shiny::div(
                        class = "bloco-verde",
                        shiny::tags$strong("CARACTERIZACAO"),
                        shiny::tags$ul(
                              shiny::tags$li("Peso"),
                              shiny::tags$li("Sexo"),
                              shiny::tags$li("ECC"),
                              shiny::tags$li("Raca"),
                              shiny::tags$li("Idade")
                        )
                  )
            ),
            
            shiny::column(
                  
                  width = 6,
                  
                  shiny::fluidRow(
                        
                        shiny::column(
                              
                              width = 4,
                              
                              shiny::div(
                                    
                                    class = "sexo-box",
                                    
                                    shiny::h4("Sexo"),
                                    
                                    shiny::checkboxGroupInput(
                                          
                                          inputId = "sexos_alvo",
                                          
                                          label = NULL,
                                          
                                          choices = c(
                                                "Macho inteiro",
                                                "Macho castrado",
                                                "Femea"
                                          ),
                                          
                                          selected = c(
                                                "Macho inteiro",
                                                "Macho castrado",
                                                "Femea"
                                          )
                                    )
                              )
                        ),
                        
                        shiny::column(
                              
                              width = 8,
                              
                              shiny::div(
                                    
                                    class = "painel-controle",
                                    
                                    shiny::h3("Definicao do objetivo do confinamento"),
                                    
                                    shiny::div(
                                          
                                          class = "criterio-cabecalho",
                                          
                                          shiny::span(""),
                                          shiny::span("Variavel"),
                                          shiny::span("Meta"),
                                          shiny::span("Valor")
                                    ),
                                    
                                    shiny::tagList(
                                          Map(
                                                criar_linha_criterio,
                                                names(criterios_confinamento),
                                                criterios_confinamento
                                          )
                                    )
                              )
                        )
                  ),
                  
                  shiny::div(
                        class = "bloco-azul",
                        "APTA-Ov"
                  ),
                  
                  shiny::actionButton(
                        
                        inputId = "executar",
                        
                        label = "Executar classificacao",
                        
                        class = "btn-primary btn-lg"
                  ),
                  
                  shiny::br(),
                  shiny::br(),
                  
                  shiny::verbatimTextOutput(
                        "status_execucao"
                  )
            ),
            
            shiny::column(
                  
                  width = 3,
                  
                  shiny::br(),
                  shiny::br(),
                  
                  shiny::div(
                        class = "bloco-saida",
                        "ABATE DIRETO"
                  ),
                  
                  shiny::div(
                        class = "bloco-saida",
                        "CONFINAMENTO",
                        shiny::tags$ul(
                              shiny::tags$li("Identificacao"),
                              shiny::tags$li("Classificacao em lotes homogeneos")
                        )
                  ),
                  
                  shiny::div(
                        class = "bloco-saida",
                        "DESCARTE",
                        shiny::tags$ul(
                              shiny::tags$li("Outros destinos")
                        )
                  )
            )
      ),
      
      shiny::tabsetPanel(
            
            shiny::tabPanel(
                  
                  title = "Classificacao",
                  
                  shiny::br(),
                  
                  shiny::fluidRow(
                        
                        shiny::column(
                              
                              width = 6,
                              
                              shiny::h3("Criterios ativos"),
                              
                              shiny::tableOutput("criterios_ativos")
                        ),
                        
                        shiny::column(
                              
                              width = 6,
                              
                              shiny::h3("Aptos e nao aptos"),
                              
                              shiny::tableOutput("resumo_classificacao")
                        )
                  )
            ),
            
            shiny::tabPanel(
                  
                  title = "Resumo",
                  
                  shiny::br(),
                  
                  shiny::fluidRow(
                        
                        shiny::column(
                              width = 6,
                              shiny::h3("Femeas"),
                              shiny::tableOutput("metricas_femeas")
                        ),
                        
                        shiny::column(
                              width = 6,
                              shiny::h3("Machos"),
                              shiny::tableOutput("metricas_machos")
                        )
                  )
            ),
            
            shiny::tabPanel(
                  
                  title = "ROC",
                  
                  shiny::br(),
                  
                  shiny::fluidRow(
                        
                        shiny::column(
                              width = 6,
                              shiny::plotOutput("roc_femeas", height = "500px")
                        ),
                        
                        shiny::column(
                              width = 6,
                              shiny::plotOutput("roc_machos", height = "500px")
                        )
                  )
            ),
            
            shiny::tabPanel(
                  
                  title = "Interpretabilidade",
                  
                  shiny::br(),
                  
                  shiny::fluidRow(
                        
                        shiny::column(
                              width = 6,
                              shiny::plotOutput("forest_femeas", height = "600px")
                        ),
                        
                        shiny::column(
                              width = 6,
                              shiny::plotOutput("forest_machos", height = "600px")
                        )
                  )
            ),
            
            shiny::tabPanel(
                  
                  title = "Predicoes",
                  
                  shiny::br(),
                  
                  shiny::fluidRow(
                        
                        shiny::column(
                              width = 6,
                              shiny::plotOutput("predicao_femeas", height = "500px")
                        ),
                        
                        shiny::column(
                              width = 6,
                              shiny::plotOutput("predicao_machos", height = "500px")
                        )
                  )
            ),
            
            shiny::tabPanel(
                  
                  title = "Tabelas",
                  
                  shiny::br(),
                  
                  shiny::h3("Odds Ratios - Femeas"),
                  
                  shiny::tableOutput("tabela_or_femeas"),
                  
                  shiny::hr(),
                  
                  shiny::h3("Odds Ratios - Machos"),
                  
                  shiny::tableOutput("tabela_or_machos")
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
      
      output$status_execucao <- shiny::renderText({
            
            "Selecione o sexo/categoria, marque as metas do confinamento e execute a classificacao."
      })
      
      criterio_ativo <- function(id) {
            
            isTRUE(
                  input[[paste0("usar_", id)]]
            )
      }
      
      valor_criterio <- function(id) {
            
            input[[paste0("valor_", id)]]
      }
      
      limiar_criterio <- function(
            
            id,
            
            valor_permissivo = 0
      ) {
            
            if(criterio_ativo(id)) {
                  
                  valor_criterio(id)
                  
            } else {
                  
                  valor_permissivo
            }
      }
      
      criterios_ativos_tabela <- shiny::reactive({
            
            ids <- names(criterios_confinamento)
            
            ativos <- vapply(
                  ids,
                  criterio_ativo,
                  logical(1)
            )
            
            valores <- vapply(
                  ids,
                  valor_criterio,
                  numeric(1)
            )
            
            tibble::tibble(
                  
                  Variavel = vapply(
                        criterios_confinamento,
                        function(x) x$rotulo,
                        character(1)
                  ),
                  
                  Usar = ifelse(
                        ativos,
                        "Sim",
                        "Nao"
                  ),
                  
                  Meta = paste(
                        vapply(
                              criterios_confinamento,
                              function(x) x$operador,
                              character(1)
                        ),
                        valores,
                        vapply(
                              criterios_confinamento,
                              function(x) x$unidade,
                              character(1)
                        )
                  )
            )
      })
      
      output$criterios_ativos <- shiny::renderTable({
            
            criterios_ativos_tabela()
      })
      
      resultado_pipeline <- shiny::eventReactive(
            
            input$executar,
            
            {
                  
                  if(!any(vapply(names(criterios_confinamento), criterio_ativo, logical(1)))) {
                        
                        shiny::showNotification(
                              
                              "Selecione pelo menos uma variavel-alvo para definir o desfecho operacional.",
                              
                              type = "error"
                        )
                        
                        return(NULL)
                  }
                  
                  if(length(input$sexos_alvo) == 0) {
                        
                        shiny::showNotification(
                              
                              "Selecione pelo menos uma categoria de sexo.",
                              
                              type = "error"
                        )
                        
                        return(NULL)
                  }
                  
                  shiny::withProgress(
                        
                        message = "Executando pipeline analitico...",
                        
                        value = 0,
                        
                        {
                              
                              shiny::incProgress(
                                    0.2,
                                    detail = "Construindo desfecho com os criterios selecionados..."
                              )
                              
                              tryCatch(
                                    
                                    {
                                          
                                          executar_pipeline_confinamento(
                                                
                                                peso_final_machos =
                                                      limiar_criterio("peso_final"),
                                                
                                                pcf_machos =
                                                      limiar_criterio("pcf"),
                                                
                                                rcf_machos =
                                                      limiar_criterio("rcf") / 100,
                                                
                                                ecc_machos =
                                                      limiar_criterio("ecc"),
                                                
                                                acabamento_machos =
                                                      limiar_criterio("acabamento"),
                                                
                                                conformacao_machos =
                                                      limiar_criterio("conformacao"),
                                                
                                                peso_final_femeas =
                                                      limiar_criterio("peso_final"),
                                                
                                                pcf_femeas =
                                                      limiar_criterio("pcf"),
                                                
                                                rcf_femeas =
                                                      limiar_criterio("rcf") / 100,
                                                
                                                ecc_femeas =
                                                      limiar_criterio("ecc"),
                                                
                                                acabamento_femeas =
                                                      limiar_criterio("acabamento"),
                                                
                                                conformacao_femeas =
                                                      limiar_criterio("conformacao"),
                                                
                                                seed = CONFIG$seed,
                                                
                                                alpha_elastic = CONFIG$alpha_elastic,
                                                
                                                n_boot_stability = CONFIG$n_boot_stability,
                                                
                                                n_boot_inferencial = CONFIG$n_boot_inferencial,
                                                
                                                verbose = FALSE
                                          )
                                    },
                                    
                                    error = function(e) {
                                          
                                          shiny::showNotification(
                                                
                                                paste(
                                                      "Erro no pipeline:",
                                                      e$message
                                                ),
                                                
                                                type = "error",
                                                
                                                duration = 10
                                          )
                                          
                                          NULL
                                    }
                              )
                        }
                  )
            }
      )
      
      resumir_classificacao <- function(
            
            dados,
            
            grupo
      ) {
            
            total <- nrow(dados)
            
            aptos <- sum(
                  dados$apto_bin == 1,
                  na.rm = TRUE
            )
            
            nao_aptos <- total - aptos
            
            tibble::tibble(
                  
                  Grupo = grupo,
                  
                  Total = total,
                  
                  Aptos = aptos,
                  
                  `Nao aptos` = nao_aptos,
                  
                  `% aptos` = ifelse(
                        total > 0,
                        round(100 * aptos / total, 1),
                        NA_real_
                  )
            )
      }
      
      output$resumo_classificacao <- shiny::renderTable({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            resultado <- resultado_pipeline()
            
            tabelas <- list()
            
            if(any(input$sexos_alvo %in% c("Macho inteiro", "Macho castrado"))) {
                  
                  tabelas[[length(tabelas) + 1]] <- resumir_classificacao(
                        
                        dados = resultado$machos$dados,
                        
                        grupo = "Machos"
                  )
            }
            
            if("Femea" %in% input$sexos_alvo) {
                  
                  tabelas[[length(tabelas) + 1]] <- resumir_classificacao(
                        
                        dados = resultado$femeas$dados,
                        
                        grupo = "Femeas"
                  )
            }
            
            dplyr::bind_rows(tabelas)
      })
      
      output$metricas_femeas <- shiny::renderTable({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            tibble::tibble(
                  
                  Metrica = c(
                        "AUC",
                        "Sensibilidade",
                        "Especificidade",
                        "Acuracia"
                  ),
                  
                  Valor = c(
                        round(resultado_pipeline()$femeas$roc$auc, 3),
                        round(resultado_pipeline()$femeas$roc$sensibilidade, 3),
                        round(resultado_pipeline()$femeas$roc$especificidade, 3),
                        round(resultado_pipeline()$femeas$roc$acuracia, 3)
                  )
            )
      })
      
      output$metricas_machos <- shiny::renderTable({
            
            shiny::req(
                  resultado_pipeline()
            )
            
            tibble::tibble(
                  
                  Metrica = c(
                        "AUC",
                        "Sensibilidade",
                        "Especificidade",
                        "Acuracia"
                  ),
                  
                  Valor = c(
                        round(resultado_pipeline()$machos$roc$auc, 3),
                        round(resultado_pipeline()$machos$roc$sensibilidade, 3),
                        round(resultado_pipeline()$machos$roc$especificidade, 3),
                        round(resultado_pipeline()$machos$roc$acuracia, 3)
                  )
            )
      })
      
      output$roc_femeas <- shiny::renderPlot({
            
            shiny::req(resultado_pipeline())
            
            print(resultado_pipeline()$femeas$plots$roc)
      })
      
      output$roc_machos <- shiny::renderPlot({
            
            shiny::req(resultado_pipeline())
            
            print(resultado_pipeline()$machos$plots$roc)
      })
      
      output$forest_femeas <- shiny::renderPlot({
            
            shiny::req(resultado_pipeline())
            
            print(resultado_pipeline()$femeas$plots$forest)
      })
      
      output$forest_machos <- shiny::renderPlot({
            
            shiny::req(resultado_pipeline())
            
            print(resultado_pipeline()$machos$plots$forest)
      })
      
      output$predicao_femeas <- shiny::renderPlot({
            
            shiny::req(resultado_pipeline())
            
            print(resultado_pipeline()$femeas$plots$predicao)
      })
      
      output$predicao_machos <- shiny::renderPlot({
            
            shiny::req(resultado_pipeline())
            
            print(resultado_pipeline()$machos$plots$predicao)
      })
      
      output$tabela_or_femeas <- shiny::renderTable({
            
            shiny::req(resultado_pipeline())
            
            resultado_pipeline()$femeas$tabelas$or
      })
      
      output$tabela_or_machos <- shiny::renderTable({
            
            shiny::req(resultado_pipeline())
            
            resultado_pipeline()$machos$tabelas$or
      })
}

# =========================================================
# EXECUCAO
# =========================================================

shiny::shinyApp(
      
      ui = ui,
      
      server = server
)
