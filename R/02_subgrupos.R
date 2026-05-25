# =========================================================
# MÓDULO 02 — SUBGRUPOS
# =========================================================
# Objetivo:
# Separar os dados em subconjuntos biológicos para
# modelagens independentes por sexo.
#
# Justificativa:
# Machos e fêmeas apresentam diferenças:
# - fisiológicas;
# - produtivas;
# - metabólicas;
# - zootécnicas.
#
# Portanto, os modelos devem ser ajustados
# separadamente para reduzir heterogeneidade
# estrutural e melhorar interpretabilidade.
# =========================================================
separar_subgrupos <- function(dados) {
      # ===================================================
      # Verificações iniciais
      # ===================================================
      if(!is.data.frame(dados)) {
            stop(
                  "O objeto informado não é um data.frame."
            )
      }
      # ===================================================
      # Verificar existência da variável sexo
      # ===================================================
      if(!"sexo_fct" %in% names(dados)) {
            
            stop(
                  "Variável 'sexo' não encontrada."
            )
      }
      # ===================================================
      # Padronização preventiva
      # ===================================================
      # Remove espaços e garante letras maiúsculas
      # ===================================================
      dados <- dados |>
            dplyr::mutate(
                  sexo_fct = toupper(
                        trimws(sexo_fct)
                  )
            )
      # ===================================================
      # Subgrupo — Machos
      # ===================================================
      df_machos <- dados |>
            dplyr::filter(
                  sexo_fct == "M"
            ) |>
            droplevels()
      # ===================================================
      # Subgrupo — Fêmeas
      # ===================================================
      df_femeas <- dados |>
            dplyr::filter(
                  sexo_fct == "F"
            ) |>
            droplevels()
      # ===================================================
      # Diagnóstico rápido
      # ===================================================
      message(
            "\n========== SUBGRUPOS =========="
      )
      message(
            paste(
                  "Machos:",
                  nrow(df_machos)
            )
      )
      message(
            paste(
                  "Fêmeas:",
                  nrow(df_femeas)
            )
      )
      # ===================================================
      # Verificação de subconjuntos vazios
      # ===================================================
      if(nrow(df_machos) == 0) {
            warning(
                  "Nenhum macho encontrado."
            )
      }
      if(nrow(df_femeas) == 0) {
            warning(
                  "Nenhuma fêmea encontrada."
            )
      }
      # ===================================================
      # Retorno
      # ===================================================
      return(
            list(
                  machos = df_machos,
                  femeas = df_femeas
            )
      )
}