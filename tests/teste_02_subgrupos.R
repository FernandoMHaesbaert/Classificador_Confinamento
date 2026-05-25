# =========================================================
# TESTE — MÓDULO 02
# Separação por subgrupos
# =========================================================
rm(list = ls())
gc()
cat("\014")
# ---------------------------------------------------------
# Carregar pacotes
# ---------------------------------------------------------
source("R/00_pacotes.R")
# ---------------------------------------------------------
# Carregar módulos
# ---------------------------------------------------------
source("R/01_preprocessamento.R")
source("R/02_subgrupos.R")
# ---------------------------------------------------------
# Executar preprocessamento
# ---------------------------------------------------------
dados <- preprocessar_dados()
# ---------------------------------------------------------
# Executar separação dos subgrupos
# ---------------------------------------------------------
subgrupos <- separar_subgrupos(dados)
# =========================================================
# VERIFICAÇÕES GERAIS
# =========================================================
message("\n========== TESTE SUBGRUPOS ==========")
# ---------------------------------------------------------
# Estrutura da saída
# ---------------------------------------------------------
print(names(subgrupos))
# ---------------------------------------------------------
# Objetos internos
# ---------------------------------------------------------
machos <- subgrupos$machos
femeas <- subgrupos$femeas
# =========================================================
# DIMENSÕES
# =========================================================
message("\n--- Número de animais ---")
message(
      paste(
            "Machos:",
            nrow(machos)
      )
)
message(
      paste(
            "Fêmeas:",
            nrow(femeas)
      )
)
message(
      paste(
            "Total:",
            nrow(machos) + nrow(femeas)
      )
)
# =========================================================
# VERIFICAÇÃO DE SEXO
# =========================================================
message("\n--- Verificação dos sexos ---")
print(
      table(machos$sexo_fct)
)
print(
      table(femeas$sexo_fct)
)
# =========================================================
# CONSISTÊNCIA DAS LINHAS
# =========================================================
message("\n--- Consistência da separação ---")
if(
      nrow(machos) + nrow(femeas) == nrow(dados)
) {
      message(
            "Separação consistente."
      )
} else {
      warning(
            "Diferença no número total de linhas."
      )
}
# =========================================================
# GLIMPSE DOS DADOS
# =========================================================
message("\n--- Estrutura MACHOS ---")
dplyr::glimpse(machos)
message("\n--- Estrutura FÊMEAS ---")
dplyr::glimpse(femeas)
# =========================================================
# RESUMO FINAL
# =========================================================
message(
      "\nTeste do módulo 02 concluído com sucesso."
)
