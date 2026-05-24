# =========================================================
# MÓDULO 01 — PREPROCESSAMENTO
# =========================================================
# Objetivo:
# Padronizar, validar e preparar os dados para
# modelagem estatística no aplicativo Shiny.
# =========================================================
preprocessar_dados <- function(
      caminho_arquivo =
            "data/dados_brutos/base_limpa.rds"
) {
      # ===================================================
      # Verificação do arquivo
      # ===================================================
      if(!file.exists(caminho_arquivo)) {
            stop(
                  paste(
                        "Arquivo não encontrado:",
                        caminho_arquivo
                  )
            )
      }
      # ===================================================
      # Leitura do arquivo
      # ===================================================
      message(
            paste(
                  "Carregando base:",
                  caminho_arquivo
            )
      )
      dados <- readRDS(caminho_arquivo)
      # ===================================================
      # Verificações iniciais
      # ===================================================
      if(!is.data.frame(dados)) {
            stop(
                  "O objeto carregado não é um data.frame."
            )
      }
      # ===================================================
      # Padronização de nomes
      # ===================================================
      dados <- dados |>
            janitor::clean_names()
      # ===================================================
      # Conversão de variáveis categóricas
      # ===================================================
      dados <- dados |>
            dplyr::mutate(
                  sexo_fct =
                        factor(sexo_fct),
                  grupo_genetico =
                        factor(grupo_genetico),
                  lote_fct =
                        factor(lote_fct),
                  idade_fct =
                        factor(idade_fct)
            )
      # ===================================================
      # Transformações parasitológicas
      # ===================================================
      dados <- dados |>
            dplyr::mutate(
                  log_opg =
                        log1p(opg_inicial),
                  log_oopg =
                        log1p(oopg_inicial),
                  z_opg =
                        as.numeric(
                              scale(log_opg)
                        ),
                  z_oopg =
                        as.numeric(
                              scale(log_oopg)
                        ),
                  indice_parasitologico =
                        z_opg + z_oopg
            )
      # ===================================================
      # Variáveis críticas
      # ===================================================
      vars_criticas <- c(
            "peso_inicial",
            "ecc_inicial",
            "sexo_fct",
            "grupo_genetico"
      )
      # ===================================================
      # Remover linhas incompletas
      # ===================================================
      dados <- dados |>
            
            tidyr::drop_na(
                  dplyr::all_of(vars_criticas)
            )
      # ===================================================
      # Filtros biológicos
      # ===================================================
      dados <- dados |>
            dplyr::filter(
                  peso_inicial > 0,
                  peso_final > 0,
                  ecc_inicial >= 0
            )
      # ===================================================
      # Identificador único
      # ===================================================
      dados <- dados |>
            dplyr::mutate(
                  animal_id =
                        dplyr::row_number()
            )
      # ===================================================
      # Resumo informativo
      # ===================================================
      message(
            paste(
                  "Número de linhas:",
                  nrow(dados)
            )
      )
      message(
            paste(
                  "Número de colunas:",
                  ncol(dados)
            )
      )
      # ===================================================
      # Retorno
      # ===================================================
      return(dados)
}