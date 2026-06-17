# =========================================================
# GLOBAL — CLASSIFICADOR CONFINAMENTO
# =========================================================
# Responsabilidade:
# Inicialização completa do sistema analítico.
#
# Este arquivo:
# - carrega pacotes;
# - carrega módulos;
# - define opções globais;
# - configura ambiente Shiny;
# - centraliza parâmetros do sistema.
#
# O global.R NÃO deve:
# - executar pipeline;
# - gerar modelos;
# - criar gráficos;
# - rodar bootstrap.
#
# Ele apenas prepara a infraestrutura do sistema.
# =========================================================

# =========================================================
# LIMPEZA INICIAL
# =========================================================

options(
      
      # ==============================================
      # Evita notação científica excessiva
      # ==============================================
      
      scipen = 999,
      
      # ==============================================
      # Silencia mensagens excessivas do dplyr
      # ==============================================
      
      dplyr.summarise.inform = FALSE,
      
      # ==============================================
      # Limite de upload do Shiny
      # ==============================================
      
      shiny.maxRequestSize = 100 * 1024^2
)

# =========================================================
# INFORMAÇÕES DO SISTEMA
# =========================================================

message(
      "\n================================================="
)

message(
      "CLASSIFICADOR CONFINAMENTO"
)

message(
      "Inicializando ambiente analítico..."
)

message(
      "=================================================\n"
)

# =========================================================
# DIRETÓRIO DO PROJETO
# =========================================================

message(
      "Diretório atual:"
)

message(
      getwd()
)

# =========================================================
# PACOTES
# =========================================================

message(
      "\n========== PACOTES =========="
)

source(
      "R/00_pacotes.R"
)

# =========================================================
# MÓDULOS ANALÍTICOS
# =========================================================

message(
      "\n========== CARREGANDO MÓDULOS =========="
)

modulos <- c(
      
      # ==============================================
      # Backend analítico
      # ==============================================
      
      "R/01_preprocessamento.R",
      
      "R/02_desfecho.R",
      
      "R/03_subgrupos.R",
      
      "R/04_elasticnet.R",
      
      "R/05_stability_selection.R",
      
      "R/06_firth.R",
      
      "R/07_bootstrap_inferencial.R",
      
      "R/08_predicao.R",
      
      "R/09_roc.R",
      
      "R/10_interpretabilidade.R",
      
      "R/11_tabelas.R",
      
      "R/12_graficos.R",
      
      "R/13_mapas.R",
      
      "R/14_pipeline.R"
)

# =========================================================
# LOOP DE CARREGAMENTO
# =========================================================

for(modulo in modulos) {
      
      if(file.exists(modulo)) {
            
            source(modulo)
            
            message(
                  paste(
                        "OK:",
                        basename(modulo)
                  )
            )
            
      } else {
            
            warning(
                  paste(
                        "Módulo não encontrado:",
                        modulo
                  )
            )
      }
}

# =========================================================
# TEMA GLOBAL
# =========================================================

message(
      "\n========== TEMA GLOBAL =========="
)

ggplot2::theme_set(
      
      ggplot2::theme_minimal(
            
            base_size = 12
      )
)

message(
      "Tema gráfico definido."
)

# =========================================================
# CONFIGURAÇÕES GLOBAIS
# =========================================================

message(
      "\n========== CONFIGURAÇÕES =========="
)

CONFIG <- list(
      
      # ==============================================
      # Identificação
      # ==============================================
      
      sistema = "Classificador Confinamento",
      
      versao = "0.1.0",
      
      autor = "Fernando Haesbaert",
      
      # ==============================================
      # Reprodutibilidade
      # ==============================================
      
      seed = 123,
      
      # ==============================================
      # Elastic Net
      # ==============================================
      
      alpha_elastic = 0.5,
      
      nfolds_elastic = 5,
      
      # ==============================================
      # Stability Selection
      # ==============================================
      
      n_boot_stability = 500,
      
      limiar_estabilidade = 0.6,
      
      # ==============================================
      # Bootstrap inferencial
      # ==============================================
      
      n_boot_inferencial = 500,
      
      # ==============================================
      # Predições
      # ==============================================
      
      n_pontos_predicao = 100,
      
      # ==============================================
      # Paths
      # ==============================================
      
      caminho_dados =
            "data/dados_brutos/base_limpa.rds",
      
      caminho_reports =
            "reports/",
      
      caminho_modelos =
            "data/modelos_salvos/"
)

message(
      "Configurações globais carregadas."
)

# =========================================================
# CRIAÇÃO DE DIRETÓRIOS
# =========================================================

message(
      "\n========== ESTRUTURA DE DIRETÓRIOS =========="
)

dirs <- c(
      
      "reports",
      
      "reports/figuras",
      
      "reports/tabelas",
      
      "reports/pipeline",
      
      "data/modelos_salvos"
)

invisible(
      
      lapply(
            
            dirs,
            
            function(x) {
                  
                  dir.create(
                        
                        x,
                        
                        recursive = TRUE,
                        
                        showWarnings = FALSE
                  )
            }
      )
)

message(
      "Diretórios verificados."
)

# =========================================================
# VERIFICAÇÃO DA BASE
# =========================================================

message(
      "\n========== VERIFICAÇÃO DA BASE =========="
)

if(file.exists(CONFIG$caminho_dados)) {
      
      message(
            "Base encontrada:"
      )
      
      message(
            CONFIG$caminho_dados
      )
      
} else {
      
      warning(
            paste(
                  "Base não encontrada:",
                  CONFIG$caminho_dados
            )
      )
}

# =========================================================
# FUNÇÃO AUXILIAR DE LOG
# =========================================================

log_sistema <- function(
            
      texto
) {
      
      message(
            paste0(
                  "[",
                  format(Sys.time(), "%H:%M:%S"),
                  "] ",
                  texto
            )
      )
}

# =========================================================
# INFORMAÇÕES DA SESSÃO
# =========================================================

message(
      "\n========== AMBIENTE =========="
)

message(
      paste(
            "R version:",
            R.version.string
      )
)

message(
      paste(
            "Sistema operacional:",
            Sys.info()["sysname"]
      )
)

message(
      paste(
            "Usuário:",
            Sys.info()["user"]
      )
)

# =========================================================
# SEED GLOBAL
# =========================================================

set.seed(
      CONFIG$seed
)

message(
      "\nSeed global definida."
)

# =========================================================
# FINALIZAÇÃO
# =========================================================

message(
      "\n================================================="
)

message(
      "Sistema carregado com sucesso."
)

message(
      paste(
            "Versão:",
            CONFIG$versao
      )
)

message(
      "Ambiente analítico pronto."
)

message(
      "=================================================\n"
)