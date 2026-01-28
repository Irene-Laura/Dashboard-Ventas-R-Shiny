# --- CARGA DE LIBRERÍAS ---
library(shiny)      
library(dplyr)      
library(ggplot2)    
library(scales)     
library(readxl)     
library(lubridate)  
library(bslib)      
library(tidyr)      
library(bsicons)    
library(plotly)     

options(scipen = 999) 

# --- FUNCIONES DE APOYO ---
formato_arg <- function(x, digitos = 0) {
  formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = digitos)
}

punto_miles <- label_number(big.mark = ".", decimal.mark = ",")

tema_dashboard <- theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text = element_text(size = 12, color = "black"),
    panel.grid.minor = element_blank()
  )

# --- CARGA DE DATOS ---
path_excel <- "Datos/Facturacion_Golosinas.xlsx"
base <- read_excel(path_excel, sheet = "Base")
productos <- read_excel(path_excel, sheet = "Productos", col_names = c("articulo", "nombre_articulo"))
canales <- read_excel(path_excel, sheet = "Canales", col_names = c("canal", "nombre_canal"))
vendedores <- read_excel(path_excel, sheet = "Vendedores", col_names = c("vendedor", "nombre_vendedor"))

ventas <- base %>%
  mutate(
    fecha = as.Date(fecha),
    anio = year(fecha),
    trimestre = paste0("T", quarter(fecha)),
    mes_label = format(fecha, "%Y-%m"),
    articulo = as.character(articulo),
    canal = as.character(canal),
    vendedor = as.character(vendedor),
    unidades = as.integer(cantidadho)
  ) %>%
  left_join(productos, by = "articulo") %>%
  left_join(canales, by = "canal") %>%
  left_join(vendedores, by = "vendedor") %>%
  mutate(across(starts_with("nombre_"), ~replace_na(.x, "Sin dato")))

# --- INTERFAZ (UI) ---
# --- INTERFAZ (UI) RESPONSIVA COMPLETA ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "cosmo") %>%
    bs_add_rules("
      .value-box-title { font-size: 0.85rem !important; text-align: center; }
      .value-box-value { font-size: 1.1rem !important; text-align: center; }
      .table-responsive { overflow-x: auto; display: block; width: 100%; }
      .insight-box { padding: 20px; background: #ffffff; border-radius: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-top: 15px; }
      .btn-download { margin-top: 10px; margin-bottom: 10px; }
      @media (max-width: 576px) {
        .value-box { height: auto !important; min-height: 90px; }
      }
    "),
  title = "Análisis de Ventas de Tienda de Golosinas",
  
  sidebar = sidebar(
    title = "Filtros",
    selectInput("anio", "Años:", choices = sort(unique(ventas$anio)), selected = unique(ventas$anio), multiple = TRUE),
    radioButtons("metrica_ranking", "Ver por:", choices = list("Facturación ($)" = "fact", "Unidades (Cantidad)" = "unid")),
    selectInput("top_n", "Top N:", choices = list("5" = 5, "10" = 10, "15" = 15), selected = 5)
  ),
  
  # Usar ancho fijo en px hace que en móvil se apilen (uno abajo del otro)
  layout_column_wrap(
    width = "180px", fill = FALSE,
    value_box(title = "Facturación Total", value = textOutput("txt_fact"), showcase = bs_icon("currency-dollar"), theme = "primary"),
    value_box(title = "Unidades (MM)", value = textOutput("txt_unid_total"), showcase = bs_icon("box-seam"), theme = "info"),
    value_box(title = "Margen Total", value = textOutput("txt_margen"), showcase = bs_icon("graph-up"), theme = "success"),
    value_box(title = "Productos", value = textOutput("txt_prod_dist"), showcase = bs_icon("tags"), theme = "primary"),
    value_box(title = "Vendedores", value = textOutput("txt_vend_dist"), showcase = bs_icon("people"), theme = "info"),
    value_box(title = "Canales", value = textOutput("txt_canales_dist"), showcase = bs_icon("shop"), theme = "success")
  ),
  
  navset_pill(
    nav_panel("⏱ Evolución",
              layout_column_wrap(width = "400px", card(plotlyOutput("plot_trim_barras")), card(plotlyOutput("plot_unid_linea"))),
              card(card_header("Resumen Anual"), 
                   div(class="table-responsive", tableOutput("tabla_anual")), 
                   downloadButton("dl_anual", "Descargar Excel", class = "btn-download btn-info"))
    ),
    
    nav_panel("🏆 Top Productos", 
              card(plotlyOutput("bubble_prod")), 
              card(card_header("Detalle Productos"), 
                   div(class="table-responsive", tableOutput("tabla_productos")), 
                   downloadButton("dl_prod", "Descargar Excel", class = "btn-download btn-info"))
    ),
    
    nav_panel("👤 Top Vendedores", 
              card(plotlyOutput("bubble_vend")), 
              card(card_header("Detalle Vendedores"), 
                   div(class="table-responsive", tableOutput("tabla_vendedores")), 
                   downloadButton("dl_vend", "Descargar Excel", class = "btn-download btn-info"))
    ),
    
    nav_panel("🏪 Canales",
              layout_column_wrap(width = "400px", card(plotlyOutput("plot_canal_bar")), card(plotlyOutput("plot_eficiencia"))),
              card(plotlyOutput("plot_prod_canal_dist"))
    ),
    
    nav_panel("⚠️ Anomalías",
              card(full_screen = TRUE, card_header("Análisis de Vendedores y Productos - Febrero 2024"), plotlyOutput("plot_outlier", height = "500px")),
              div(class = "insight-box",
                  h3("Insights Específicos", style="color: #2C7BE5; border-bottom: 2px solid #2C7BE5; padding-bottom: 10px;"),
                  HTML("<p style='font-size: 15px; line-height: 1.7; margin-top: 15px;'>
                        <b>Feb-2024:</b> Se observa que el mes de febrero de 2024 fue el de mayor facturación y la explicación corresponde a una vendedora (Julieta Castro), un canal (Máquina expendedora) y un producto (Milka oreo alfajor) específicos. Se destaca que el margen de ganancias también es un valor outlier respecto al periodo analizado, sin embargo las unidades vendidas no se corresponden con los montos de ventas, dado que el producto con mayor cantidad de ventas vendió 10 veces más unidades que el de mayor facturación, lo que confirma la anomalía.<br><br>
                        <b>Canales:</b> Separando del análisis el caso comentado, se observa que el canal Kiosko es el que combina tanto mayor facturación como mayor cantidad de unidades vendidas sin que se destaque algún producto en particular, lo que podría apuntar a fortalecer este canal para sostener los niveles de ventas.<br><br>
                        <b>Margen:</b> Se destacan también los productos con margen de ganancias negativo, que deberían analizarse para revisar los precios de ventas. Se puede profundizar el análisis para saber si son productos que se venden en combos con otros de mejores márgenes de ganancias o bien considerar la posibilidad de discontinuar su venta.</p>")
              )
    ),
    
    nav_panel("📉 Riesgo", 
              card(card_header("Productos con Menor Margen"), 
                   div(class="table-responsive", tableOutput("tabla_riesgo")), 
                   downloadButton("dl_riesgo", "Descargar Excel", class = "btn-download btn-info")))
  )
)

# --- SERVIDOR ---
server <- function(input, output) {
  
  datos_f <- reactive({ req(input$anio); ventas %>% filter(anio %in% as.numeric(input$anio)) })
  
  # KPIs
  output$txt_fact <- renderText({ req(datos_f()); paste0(formato_arg(sum(datos_f()$totPes)/1e6, 0), " MM $") })
  output$txt_unid_total <- renderText({ req(datos_f()); paste0(formato_arg(sum(datos_f()$unidades)/1e6, 2), " MM") })
  output$txt_margen <- renderText({ req(datos_f()); paste0(formato_arg(sum(datos_f()$margen)/1e6, 0), " MM $") })
  output$txt_prod_dist <- renderText({ req(datos_f()); n_distinct(datos_f()$articulo) })
  output$txt_vend_dist <- renderText({ req(datos_f()); n_distinct(datos_f()$vendedor) })
  output$txt_canales_dist <- renderText({ req(datos_f()); n_distinct(datos_f()$canal) })
  
  # Gráficos Evolución
  output$plot_trim_barras <- renderPlotly({
    p <- datos_f() %>% group_by(anio, trimestre) %>% summarise(Facturación = sum(totPes)/1e6, Margen = sum(margen)/1e6, .groups = "drop") %>%
      pivot_longer(cols = c(Facturación, Margen)) %>% mutate(periodo = paste(anio, trimestre)) %>% 
      ggplot(aes(x = periodo, y = value, fill = name, text = paste(name, ": $", formato_arg(value, 0), "MM"))) +
      geom_col(position = "dodge") + scale_fill_manual(values = c("#2C7BE5", "#6C757D")) +
      scale_y_continuous(labels = punto_miles) + labs(title = "Facturación y Margen Trimestral", y = "MM $", x = "", fill = "") + tema_dashboard
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 80, b = 50))
  })
  
  output$plot_unid_linea <- renderPlotly({
    p <- datos_f() %>% group_by(anio, trimestre) %>% summarise(u = sum(unidades)/1000, .groups = "drop") %>%
      mutate(periodo = paste(anio, trimestre)) %>% 
      ggplot(aes(x = factor(periodo), y = u, group = 1, text = paste("Unidades:", formato_arg(u, 0), "mil"))) +
      geom_line(color = "#2C7BE5", linewidth = 1) + geom_point(size = 3) +
      scale_y_continuous(labels = punto_miles) + labs(title = "Unidades Vendidas", x = "", y = "Miles") + tema_dashboard
    ggplotly(p, tooltip = "text") %>% layout(margin = list(b = 50))
  })
  
  # Rankings
  output$bubble_prod <- renderPlotly({
    df_res <- datos_f() %>% group_by(nombre_articulo) %>% summarise(fact = sum(totPes)/1e6, unid = sum(unidades)/1000, marg = sum(margen)/1e6, .groups = "drop")
    df_res$val_x <- if(input$metrica_ranking == "fact") df_res$fact else df_res$unid
    lab_x <- if(input$metrica_ranking == "fact") "Facturación (MM $)" else "Unidades (Miles)"
    df_top <- df_res %>% arrange(desc(val_x)) %>% head(as.numeric(input$top_n))
    p <- ggplot(df_top, aes(x = val_x, y = reorder(nombre_articulo, val_x), size = marg, text = paste("Producto:", nombre_articulo, "<br>Margen: $", formato_arg(marg, 0), "MM"))) +
      geom_point(color = "#2C7BE5", alpha = 0.6) + scale_x_continuous(labels = punto_miles) + labs(title = "Ranking Productos", x = lab_x, y = "") + tema_dashboard
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 200, b = 50))
  })
  
  output$bubble_vend <- renderPlotly({
    df_res <- datos_f() %>% group_by(nombre_vendedor) %>% summarise(fact = sum(totPes)/1e6, unid = sum(unidades)/1000, marg = sum(margen)/1e6, .groups = "drop")
    df_res$val_x <- if(input$metrica_ranking == "fact") df_res$fact else df_res$unid
    lab_x <- if(input$metrica_ranking == "fact") "Facturación (MM $)" else "Unidades (Miles)"
    df_top <- df_res %>% arrange(desc(val_x)) %>% head(as.numeric(input$top_n))
    p <- ggplot(df_top, aes(x = val_x, y = reorder(nombre_vendedor, val_x), size = marg, text = paste("Vendedor:", nombre_vendedor, "<br>Margen: $", formato_arg(marg, 0), "MM"))) +
      geom_point(color = "#6C757D", alpha = 0.6) + scale_x_continuous(labels = punto_miles) + labs(title = "Ranking Vendedores", x = lab_x, y = "") + tema_dashboard
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 200, b = 50))
  })
  
  # Canales
  output$plot_canal_bar <- renderPlotly({
    df <- datos_f() %>% group_by(nombre_canal) %>% summarise(v = sum(totPes)/1e6)
    p <- ggplot(df, aes(x = v, y = reorder(nombre_canal, v), text = paste("Facturación: $", formato_arg(v, 0), "MM"))) + 
      geom_col(fill = "#2C7BE5") + scale_x_continuous(labels = punto_miles) + labs(title = "Facturación por Canal", x = "MM $", y = "") + tema_dashboard
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 150, b = 50))
  })
  
  output$plot_eficiencia <- renderPlotly({
    df <- datos_f() %>% group_by(nombre_canal) %>% summarise(r = sum(margen)/sum(totPes))
    p <- ggplot(df, aes(x = r, y = reorder(nombre_canal, r), text = paste("Eficiencia:", round(r*100, 1), "%"))) + 
      geom_col(fill = "#6C757D") + scale_x_continuous(labels = label_percent(decimal.mark = ",")) + labs(title = "Eficiencia por Canal", x = "%", y = "") + tema_dashboard
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 150, b = 50))
  })
  
  output$plot_prod_canal_dist <- renderPlotly({
    m_type <- if(input$metrica_ranking == "fact") "totPes" else "unidades"
    factor_val <- if(input$metrica_ranking == "fact") 1e6 else 1000
    lab_y_dyn <- if(input$metrica_ranking == "fact") "Facturación (MM $)" else "Unidades (Miles)"
    
    df <- datos_f() %>% group_by(nombre_canal, nombre_articulo) %>% summarise(val = sum(get(m_type))/factor_val, .groups = "drop")
    ord_c <- df %>% group_by(nombre_canal) %>% summarise(t = sum(val)) %>% arrange(desc(t)) %>% pull(nombre_canal)
    df_plot <- df %>% group_by(nombre_canal) %>% mutate(rank = rank(-val, ties.method = "first"), Producto = ifelse(rank <= 5, nombre_articulo, "Otros")) %>%
      group_by(nombre_canal, Producto) %>% summarise(val = sum(val), .groups = "drop")
    df_plot$nombre_canal <- factor(df_plot$nombre_canal, levels = ord_c)
    
    p <- ggplot(df_plot, aes(x = nombre_canal, y = val, fill = Producto, text = paste("Producto:", Producto, "<br>Val:", formato_arg(val, 0)))) +
      geom_col() + scale_y_continuous(labels = punto_miles) + labs(title = "Distribución por Canal", x = "", y = lab_y_dyn) + tema_dashboard + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text") %>% layout(margin = list(b = 100))
  })
  
  # Anomalías
  output$plot_outlier <- renderPlotly({
    m2 <- ventas %>% filter(mes_label == "2024-02")
    top_10_v <- m2 %>% group_by(nombre_vendedor) %>% summarise(t = sum(totPes)) %>% arrange(desc(t)) %>% head(10) %>% pull(nombre_vendedor)
    df_out <- m2 %>% filter(nombre_vendedor %in% top_10_v) %>% group_by(nombre_vendedor, nombre_articulo) %>% summarise(f = sum(totPes)/1e6, .groups = "drop") %>%
      group_by(nombre_vendedor) %>% mutate(rank = rank(-f, ties.method = "first"), Producto = ifelse(rank <= 3, nombre_articulo, "Otros")) %>%
      group_by(nombre_vendedor, Producto) %>% summarise(f = sum(f), .groups = "drop")
    df_out$nombre_vendedor <- factor(df_out$nombre_vendedor, levels = top_10_v)
    p <- ggplot(df_out, aes(x = nombre_vendedor, y = f, fill = Producto, text = paste("Vendedor:", nombre_vendedor, "<br>Fact.: $", formato_arg(f, 0), "MM"))) +
      geom_col() + scale_y_continuous(labels = punto_miles) + labs(x = "", y = "Facturación (MM $)") + tema_dashboard + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text") %>% layout(margin = list(b = 120, r = 250), legend = list(orientation = "v", y = 0.5, x = 1.05, xanchor = "left"))
  })
  
  # --- TABLAS (RENDER) ---
  output$tabla_anual <- renderTable({ 
    datos_f() %>% 
      group_by(Año = as.character(anio)) %>% 
      summarise(
        `Facturación (MM $)` = sum(totPes)/1e6, 
        `Margen (MM $)` = sum(margen)/1e6,
        `Unidades (Miles)` = sum(unidades)/1000
      ) %>% 
      mutate(across(where(is.numeric), ~formato_arg(.x, 0))) 
  })
  
    output$tabla_productos <- renderTable({
    df <- datos_f() %>% 
      group_by(Producto = nombre_articulo) %>% 
      summarise(`Facturacion (MM $)` = sum(totPes)/1e6, `Unidades (Miles)` = sum(unidades)/1000, `Margen (MM $)` = sum(margen)/1e6)
    
    col_ord <- if(input$metrica_ranking == "fact") "`Facturacion (MM $)`" else "`Unidades (Miles)`"
    df %>% arrange(desc(!!rlang::parse_expr(col_ord))) %>% 
      head(as.numeric(input$top_n)) %>% 
      mutate(across(where(is.numeric), ~formato_arg(.x, 0))) # 0  para que sea entero
  })
  
  output$tabla_vendedores <- renderTable({
    df <- datos_f() %>% 
      group_by(Vendedor = nombre_vendedor) %>% 
      summarise(`Facturacion (MM $)` = sum(totPes)/1e6, `Unidades (Miles)` = sum(unidades)/1000, `Margen (MM $)` = sum(margen)/1e6)
    
    col_ord <- if(input$metrica_ranking == "fact") "`Facturacion (MM $)`" else "`Unidades (Miles)`"
    df %>% arrange(desc(!!rlang::parse_expr(col_ord))) %>% 
      head(as.numeric(input$top_n)) %>% 
      mutate(across(where(is.numeric), ~formato_arg(.x, 0)))
  })
  
  output$tabla_riesgo <- renderTable({
    ventas %>% 
      group_by(Producto = nombre_articulo) %>% 
      summarise(`Margen (Miles $)` = sum(margen)/1000) %>% 
      arrange(`Margen (Miles $)`) %>% 
      head(10) %>% 
      mutate(`Margen (Miles $)` = formato_arg(`Margen (Miles $)`, 0))
  })
  
  # --- DESCARGAS (LOGICA CORREGIDA) ---
  
  output$dl_anual <- downloadHandler(
    filename = function() { paste0("Resumen_Anual_Puro_", Sys.Date(), ".csv") },
    content = function(file) {
      tabla <- datos_f() %>% 
        group_by(Año = as.character(anio)) %>% 
        summarise(
          Facturacion = sum(totPes), 
          Margen = sum(margen), 
          Unidades = sum(unidades)
        )
      write.table(tabla, file, sep=";", dec=",", row.names=F, fileEncoding="latin1")
    }
  )
  
  output$dl_prod <- downloadHandler(
    filename = function() { paste0("Top_Productos_", Sys.Date(), ".csv") },
    content = function(file) {
      tabla <- datos_f() %>% group_by(Producto = nombre_articulo) %>% summarise(Facturacion = sum(totPes), Unidades = sum(unidades), Margen = sum(margen))
      col_ord <- if(input$metrica_ranking == "fact") "Facturacion" else "Unidades"
      tabla <- tabla %>% arrange(desc(get(col_ord))) %>% head(as.numeric(input$top_n))
      write.table(tabla, file, sep=";", dec=",", row.names=F, fileEncoding="latin1")
    }
  )
  
  output$dl_vend <- downloadHandler(
    filename = function() { paste0("Top_Vendedores_", Sys.Date(), ".csv") },
    content = function(file) {
      tabla <- datos_f() %>% group_by(Vendedor = nombre_vendedor) %>% summarise(Facturacion = sum(totPes), Unidades = sum(unidades), Margen = sum(margen))
      col_ord <- if(input$metrica_ranking == "fact") "Facturacion" else "Unidades"
      tabla <- tabla %>% arrange(desc(get(col_ord))) %>% head(as.numeric(input$top_n))
      write.table(tabla, file, sep=";", dec=",", row.names=F, fileEncoding="latin1")
    }
  )
  
  output$dl_riesgo <- downloadHandler(
    filename = function() { paste0("Riesgo_Margen_", Sys.Date(), ".csv") },
    content = function(file) {
      tabla <- ventas %>% group_by(Producto = nombre_articulo) %>% summarise(Margen = sum(margen)) %>% arrange(Margen) %>% head(10)
      write.table(tabla, file, sep=";", dec=",", row.names=F, fileEncoding="latin1")
    }
  )
}

shinyApp(ui, server)