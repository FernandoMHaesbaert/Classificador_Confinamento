# =========================================================
# TESTE — MÓDULO 13
# MAPAS
# =========================================================

rm(list = ls())

gc()

cat("\014")

# =========================================================
# PACOTES
# =========================================================

source("R/00_pacotes.R")

# =========================================================
# MÓDULOS
# =========================================================

source("R/01_preprocessamento.R")

source("R/02_subgrupos.R")

source("R/03_desfecho.R")

source("R/06_firth.R")

source("R/08_predicao.R")

source("R/13_mapas.R")

# =========================================================
# PREPROCESSAMENTO
# =========================================================

dados <- preprocessar_dados()

# =========================================================
# SUBGRUPOS
# =========================================================

subgrupos <- separar_subgrupos(dados)

machos <- subgrupos$machos

femeas <- subgrupos$femeas

# =========================================================
# DESFECHO
# =========================================================

femeas_desfecho <- criar_desfecho(
      
      dados = femeas,
      
      peso_final_min = 30,
      
      pcf_min = 14,
      
      rcf_min = 0.45,
      
      ecc_final_min = 3,
      
      acabamento_min = 3,
      
      conformacao_min = 3
)

# =========================================================
# VARIÁVEIS FINAIS
# =========================================================

variaveis_femeas <- c(
      
      "indice_parasitologico",
      
      "peso_inicial",
      
      "ecc_inicial"
)

# =========================================================
# MODELO FIRTH
# =========================================================

modelo_femeas <- ajustar_firth(
      
      dados = femeas_desfecho,
      
      variaveis_finais =
            variaveis_femeas
)

# =========================================================
# PROBABILIDADES PREDITAS
# =========================================================

femeas_desfecho$probabilidade <- predict(
      
      modelo_femeas$modelo,
      
      type = "response"
)

# =========================================================
# COORDENADAS ESPACIAIS FICTÍCIAS
# =========================================================
# OBS:
# Como a base original ainda não possui GPS,
# serão simuladas coordenadas para teste
# estrutural do módulo.
# =========================================================

set.seed(123)

femeas_desfecho <- femeas_desfecho |>
      
      dplyr::mutate(
            
            longitude =
                  runif(
                        n = nrow(femeas_desfecho),
                        min = -48.5,
                        max = -48.0
                  ),
            
            latitude =
                  runif(
                        n = nrow(femeas_desfecho),
                        min = -10.5,
                        max = -10.0
                  )
      )

# =========================================================
# OBJETO SF
# =========================================================

dados_sf <- criar_objeto_sf(
      
      dados =
            femeas_desfecho,
      
      longitude =
            "longitude",
      
      latitude =
            "latitude"
)

# =========================================================
# CABEÇALHO
# =========================================================

message(
      "\n================================================="
)

message(
      "MAPAS — TESTE DO MÓDULO 13"
)

message(
      "=================================================\n"
)

# =========================================================
# MAPA DE PROBABILIDADE
# =========================================================

message(
      "\n========== MAPA DE PROBABILIDADE =========="
)

mapa_prob <- mapa_probabilidade(
      
      dados_sf =
            dados_sf,
      
      variavel_probabilidade =
            "probabilidade",
      
      titulo =
            "Probabilidade predita de aptidão"
)

print(mapa_prob)

# =========================================================
# MAPA SANITÁRIO
# =========================================================

message(
      "\n========== MAPA SANITÁRIO =========="
)

mapa_parasitario <- mapa_sanitario(
      
      dados_sf =
            dados_sf,
      
      variavel_sanitaria =
            "indice_parasitologico",
      
      titulo =
            "Índice parasitológico"
)

print(mapa_parasitario)

# =========================================================
# MAPA CATEGÓRICO
# =========================================================

message(
      "\n========== MAPA CATEGÓRICO =========="
)

mapa_apto <- mapa_categorico(
      
      dados_sf =
            dados_sf,
      
      variavel_categoria =
            "apto_label",
      
      titulo =
            "Classificação operacional"
)

print(mapa_apto)

# =========================================================
# HEATMAP ESPACIAL
# =========================================================

message(
      "\n========== HEATMAP ESPACIAL =========="
)

heatmap <- heatmap_espacial(
      
      dados =
            femeas_desfecho,
      
      longitude =
            "longitude",
      
      latitude =
            "latitude",
      
      variavel =
            "probabilidade",
      
      titulo =
            "Heatmap de risco"
)

print(heatmap)

# =========================================================
# CLUSTERS ESPACIAIS
# =========================================================

message(
      "\n========== CLUSTERS ESPACIAIS =========="
)

clusters <- mapa_clusters(
      
      dados =
            femeas_desfecho,
      
      longitude =
            "longitude",
      
      latitude =
            "latitude",
      
      n_clusters = 3
)

print(
      clusters$grafico
)

# =========================================================
# SUPERFÍCIE ESPACIAL
# =========================================================

message(
      "\n========== SUPERFÍCIE ESPACIAL =========="
)

superficie <- superficie_risco(
      
      dados =
            femeas_desfecho,
      
      longitude =
            "longitude",
      
      latitude =
            "latitude",
      
      variavel =
            "probabilidade"
)

print(superficie)

# =========================================================
# LEAFLET INTERATIVO
# =========================================================

message(
      "\n========== MAPA INTERATIVO =========="
)

leaflet_mapa <- mapa_leaflet(
      
      dados =
            femeas_desfecho,
      
      longitude =
            "longitude",
      
      latitude =
            "latitude",
      
      popup_variavel =
            "animal_id",
      
      variavel_cor =
            "probabilidade"
)

print(leaflet_mapa)

# =========================================================
# EXPORTAÇÃO
# =========================================================

dir.create(
      
      "reports/mapas",
      
      recursive = TRUE,
      
      showWarnings = FALSE
)

exportar_mapa(
      
      grafico =
            mapa_prob,
      
      caminho =
            "reports/mapas/mapa_probabilidade.png"
)

exportar_mapa(
      
      grafico =
            mapa_parasitario,
      
      caminho =
            "reports/mapas/mapa_parasitologico.png"
)

exportar_mapa(
      
      grafico =
            mapa_apto,
      
      caminho =
            "reports/mapas/mapa_categorico.png"
)

exportar_mapa(
      
      grafico =
            heatmap,
      
      caminho =
            "reports/mapas/heatmap.png"
)

exportar_mapa(
      
      grafico =
            clusters$grafico,
      
      caminho =
            "reports/mapas/clusters.png"
)

exportar_mapa(
      
      grafico =
            superficie,
      
      caminho =
            "reports/mapas/superficie.png"
)

# =========================================================
# FINALIZAÇÃO
# =========================================================

message(
      "\n================================================="
)

message(
      "Teste do módulo 13 concluído com sucesso."
)

message(
      "=================================================\n"
)