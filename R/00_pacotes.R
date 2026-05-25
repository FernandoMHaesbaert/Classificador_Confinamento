# =========================================================
# MÓDULO 00 — PACOTES
# =========================================================
# Objetivo:
# Instalar e carregar automaticamente os pacotes
# necessários para o projeto.
# =========================================================
carregar_pacotes <- function() {
      # ===================================================
      # Pacotes do projeto
      # ===================================================
      pkgs <- c(
            # Manipulação e wrangling
            "tidyverse",
            "janitor",
            "readxl",
            "purrr",
            # Exploração e diagnóstico
            "skimr",
            "visdat",
            "GGally",
            "corrr",
            "moments",
            "rstatix",
            "effsize",
            # Visualização
            "patchwork",
            "scales",
            "ggpubr",
            "ggstatsplot",
            "leaflet",
            # Modelagem
            "glmnet",
            "logistf",
            "MASS",
            "pROC",
            "pscl",
            "ggeffects",
            # Tabelas
            "gt",
            "gtsummary",
            "broom",
            "broom.helpers"
      )
      # ===================================================
      # Verificar pacotes ausentes
      # ===================================================
      pkgs_faltantes <- pkgs[
            !pkgs %in%
                  installed.packages()[, "Package"]
      ]
      # ===================================================
      # Instalar ausentes
      # ===================================================
      if(length(pkgs_faltantes) > 0) {
            message(
                  "\nInstalando pacotes ausentes:"
            )
            print(pkgs_faltantes)
            install.packages(
                  pkgs_faltantes,
                  dependencies = TRUE
            )
      }
      # ===================================================
      # Carregar pacotes
      # ===================================================
      invisible(
            lapply(
                  pkgs,
                  library,
                  character.only = TRUE
            )
      )
      # ===================================================
      # Mensagem final
      # ===================================================
      message(
            "\nPacotes carregados com sucesso."
      )
}

