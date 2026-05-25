# =========================================================
# MÓDULO 13 — MAPAS
# =========================================================
# Objetivo:
# Gerar mapas espaciais e superfícies de risco
# aplicadas ao confinamento ovino.
#
# Inclui:
# - mapas de probabilidade predita;
# - mapas sanitários;
# - mapas de desempenho;
# - mapas de clusters;
# - heatmaps espaciais;
# - integração com leaflet.
#
# Estrutura preparada para:
# - GPS de lotes;
# - sensores;
# - integração futura com VANTs;
# - dashboards Shiny.
# =========================================================

# =========================================================
# VERIFICAÇÃO DE PACOTES
# =========================================================

if(!requireNamespace("sf", quietly = TRUE)) {
      
      stop(
            "Pacote 'sf' não instalado."
      )
}

if(!requireNamespace("leaflet", quietly = TRUE)) {
      
      stop(
            "Pacote 'leaflet' não instalado."
      )
}

# =========================================================
# CONVERSÃO PARA OBJETO ESPACIAL
# =========================================================

criar_objeto_sf <- function(
            
      dados,
      
      longitude,
      
      latitude,
      
      crs = 4326
) {
      
      # ===================================================
      # VALIDAÇÃO
      # ===================================================
      
      if(!all(
            c(longitude, latitude) %in%
            names(dados)
      )) {
            
            stop(
                  "Longitude/Latitude não encontradas."
            )
      }
      
      # ===================================================
      # CONVERSÃO
      # ===================================================
      
      dados_sf <- sf::st_as_sf(
            
            dados,
            
            coords = c(
                  longitude,
                  latitude
            ),
            
            crs = crs
      )
      
      return(dados_sf)
}

# =========================================================
# MAPA DE PROBABILIDADE PREDITA
# =========================================================

mapa_probabilidade <- function(
            
      dados_sf,
      
      variavel_probabilidade = "probabilidade",
      
      titulo = "Mapa de Probabilidade Predita"
) {
      
      grafico <- ggplot2::ggplot(
            
            dados_sf
      ) +
            
            ggplot2::geom_sf(
                  
                  ggplot2::aes_string(
                        color = variavel_probabilidade
                  ),
                  
                  size = 3
            ) +
            
            ggplot2::scale_color_viridis_c() +
            
            ggplot2::labs(
                  
                  title = titulo,
                  
                  color = "Probabilidade"
            ) +
            
            ggplot2::theme_minimal()
      
      return(grafico)
}

# =========================================================
# MAPA SANITÁRIO
# =========================================================

mapa_sanitario <- function(
            
      dados_sf,
      
      variavel_sanitaria,
      
      titulo = "Mapa Sanitário"
) {
      
      grafico <- ggplot2::ggplot(
            
            dados_sf
      ) +
            
            ggplot2::geom_sf(
                  
                  ggplot2::aes_string(
                        color = variavel_sanitaria
                  ),
                  
                  size = 3
            ) +
            
            ggplot2::scale_color_viridis_c() +
            
            ggplot2::labs(
                  
                  title = titulo,
                  
                  color = variavel_sanitaria
            ) +
            
            ggplot2::theme_minimal()
      
      return(grafico)
}

# =========================================================
# MAPA CATEGÓRICO
# =========================================================

mapa_categorico <- function(
            
      dados_sf,
      
      variavel_categoria,
      
      titulo = "Mapa Categórico"
) {
      
      grafico <- ggplot2::ggplot(
            
            dados_sf
      ) +
            
            ggplot2::geom_sf(
                  
                  ggplot2::aes_string(
                        fill = variavel_categoria
                  ),
                  
                  shape = 21,
                  
                  size = 4
            ) +
            
            ggplot2::labs(
                  
                  title = titulo,
                  
                  fill = variavel_categoria
            ) +
            
            ggplot2::theme_minimal()
      
      return(grafico)
}

# =========================================================
# HEATMAP ESPACIAL
# =========================================================

heatmap_espacial <- function(
            
      dados,
      
      longitude,
      
      latitude,
      
      variavel,
      
      titulo = "Heatmap Espacial"
) {
      
      grafico <- ggplot2::ggplot(
            
            dados,
            
            ggplot2::aes_string(
                  
                  x = longitude,
                  
                  y = latitude
            )
      ) +
            
            ggplot2::stat_density_2d(
                  
                  ggplot2::aes(
                        fill = ..level..
                  ),
                  
                  geom = "polygon",
                  
                  alpha = 0.5
            ) +
            
            ggplot2::geom_point(
                  
                  ggplot2::aes_string(
                        color = variavel
                  ),
                  
                  size = 2
            ) +
            
            ggplot2::scale_fill_viridis_c() +
            
            ggplot2::scale_color_viridis_c() +
            
            ggplot2::labs(
                  
                  title = titulo
            ) +
            
            ggplot2::theme_minimal()
      
      return(grafico)
}

# =========================================================
# CLUSTERS ESPACIAIS
# =========================================================

mapa_clusters <- function(
            
      dados,
      
      longitude,
      
      latitude,
      
      n_clusters = 3,
      
      seed = 123
) {
      
      set.seed(seed)
      
      # ===================================================
      # MATRIZ ESPACIAL
      # ===================================================
      
      coords <- dados |>
            
            dplyr::select(
                  dplyr::all_of(
                        c(
                              longitude,
                              latitude
                        )
                  )
            ) |>
            
            as.matrix()
      
      # ===================================================
      # KMEANS
      # ===================================================
      
      kmeans_fit <- stats::kmeans(
            
            coords,
            
            centers = n_clusters
      )
      
      dados$cluster <- as.factor(
            kmeans_fit$cluster
      )
      
      # ===================================================
      # PLOT
      # ===================================================
      
      grafico <- ggplot2::ggplot(
            
            dados,
            
            ggplot2::aes_string(
                  
                  x = longitude,
                  
                  y = latitude,
                  
                  color = "cluster"
            )
      ) +
            
            ggplot2::geom_point(
                  size = 3
            ) +
            
            ggplot2::labs(
                  
                  title = "Clusters Espaciais"
            ) +
            
            ggplot2::theme_minimal()
      
      return(
            list(
                  
                  dados = dados,
                  
                  modelo = kmeans_fit,
                  
                  grafico = grafico
            )
      )
}

# =========================================================
# MAPA INTERATIVO LEAFLET
# =========================================================

mapa_leaflet <- function(
            
      dados,
      
      longitude,
      
      latitude,
      
      popup_variavel = NULL,
      
      variavel_cor = NULL
) {
      
      # ===================================================
      # PALETA
      # ===================================================
      
      if(!is.null(variavel_cor)) {
            
            pal <- leaflet::colorNumeric(
                  
                  palette = "viridis",
                  
                  domain = dados[[variavel_cor]]
            )
      }
      
      # ===================================================
      # MAPA
      # ===================================================
      
      mapa <- leaflet::leaflet(
            
            dados
      ) |>
            
            leaflet::addTiles()
      
      # ===================================================
      # MARCADORES
      # ===================================================
      
      if(is.null(variavel_cor)) {
            
            mapa <- mapa |>
                  
                  leaflet::addCircleMarkers(
                        
                        lng = dados[[longitude]],
                        
                        lat = dados[[latitude]],
                        
                        popup = if(!is.null(popup_variavel))
                              as.character(
                                    dados[[popup_variavel]]
                              )
                        else NULL,
                        
                        radius = 5
                  )
            
      } else {
            
            mapa <- mapa |>
                  
                  leaflet::addCircleMarkers(
                        
                        lng = dados[[longitude]],
                        
                        lat = dados[[latitude]],
                        
                        color = pal(
                              dados[[variavel_cor]]
                        ),
                        
                        popup = if(!is.null(popup_variavel))
                              as.character(
                                    dados[[popup_variavel]]
                              )
                        else NULL,
                        
                        radius = 6
                  )
      }
      
      return(mapa)
}

# =========================================================
# SUPERFÍCIE ESPACIAL SIMPLIFICADA
# =========================================================

superficie_risco <- function(
            
      dados,
      
      longitude,
      
      latitude,
      
      variavel,
      
      grid_n = 100
) {
      
      # ===================================================
      # GRID
      # ===================================================
      
      grid_x <- seq(
            
            min(dados[[longitude]]),
            
            max(dados[[longitude]]),
            
            length.out = grid_n
      )
      
      grid_y <- seq(
            
            min(dados[[latitude]]),
            
            max(dados[[latitude]]),
            
            length.out = grid_n
      )
      
      grade <- expand.grid(
            
            x = grid_x,
            
            y = grid_y
      )
      
      # ===================================================
      # INTERPOLAÇÃO SIMPLIFICADA
      # ===================================================
      
      ajuste <- stats::loess(
            
            formula = as.formula(
                  
                  paste(
                        variavel,
                        "~",
                        longitude,
                        "+",
                        latitude
                  )
            ),
            
            data = dados
      )
      
      novo_dado <- data.frame(
            
            x_coord = grade$x,
            
            y_coord = grade$y
      )
      
      names(novo_dado) <- c(
            longitude,
            latitude
      )
      
      grade$pred <- predict(
            
            ajuste,
            
            newdata = novo_dado
      )
      
      # ===================================================
      # PLOT
      # ===================================================
      
      grafico <- ggplot2::ggplot(
            
            grade,
            
            ggplot2::aes(
                  
                  x = x,
                  
                  y = y,
                  
                  fill = pred
            )
      ) +
            
            ggplot2::geom_raster() +
            
            ggplot2::scale_fill_viridis_c() +
            
            ggplot2::labs(
                  
                  title = "Superfície Espacial de Risco",
                  
                  fill = "Predição"
            ) +
            
            ggplot2::theme_minimal()
      
      return(grafico)
}

# =========================================================
# EXPORTAÇÃO DE MAPAS
# =========================================================

exportar_mapa <- function(
            
      grafico,
      
      caminho,
      
      largura = 10,
      
      altura = 8,
      
      dpi = 300
) {
      
      ggplot2::ggsave(
            
            filename = caminho,
            
            plot = grafico,
            
            width = largura,
            
            height = altura,
            
            dpi = dpi
      )
      
      message(
            paste(
                  "Mapa exportado:",
                  caminho
            )
      )
}