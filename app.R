
# Bibliotecas -------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(highcharter)

# Dados -------------------------------------------------------------------

source("Analise inicial.R", encoding = "utf-8")
source("tema.R")

# Header ------------------------------------------------------------------

logo_ifood = "https://s2.glbimg.com/JUFyH6TyUquy-G2zZrjY0kq0SYQ=/0x0:512x512/512x512/s.glbimg.com/po/tt2/f/original/2020/08/17/unnamed_2.png"

Header <-
  dashboardHeader(titleWidth = 150,
     title = tagList(
       tags$span(class = "logo-lg"
         ,tags$img(src=logo_ifood, class = "hlogo", height = 50, width = 150)
         ,tags$span("Informações Ifood"))
       ,tags$img(src=logo_ifood, class = "logo-mini", height = 50, width = 50))
    #,titleWidth = 360
    )


# Sidebar -----------------------------------------------------------------

Sidebar <-
  dashboardSidebar(width = 150,disable = T,
    sidebarUserPanel("Leandro",
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Noivo"),
                    
                     image = "meubem.jpg"
    ),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Geral", tabName = "geral", icon = icon("dashboard")),
      menuItem("Tipos", tabName = "tipos", icon = icon("th"))
    )
    )


# Footer ------------------------------------------------------------------


Footer = dashboardFooter(
   left_text = "Desenvolvido por Érika S.M."
  
)
# Body --------------------------------------------------------------------

Body <-
  dashboardBody(
    # Tema Dashboard
    Tema,
    
    tabItems(
    
 # Geral -------------------------------------------------------------------
  
  tabItem(
    tabName = "geral"
    ,fluidRow(
      column(width = 6,
             valueBoxOutput("total_pedidos", 6),
             valueBoxOutput("total_valor", 6),
             valueBoxOutput( "med_p_m", 6),
             valueBoxOutput("med_v_m", 6 ),
             valueBoxOutput("media_sem_p", 6)
             ,valueBoxOutput("media_sem_v", 6)
    )
    ,boxPlus( width = 6,
               highchartOutput("hist_valor", height = 320))
  )
    ,fluidRow(
      boxPlus(width =  5, highchartOutput("ped_dia")),
      boxPlus(width = 7, highchartOutput("serie_valor"))
      
    )
    ),

# Tipo --------------------------------------------------------------------

  tabItem(
    tabName = "tipos",
    fluidRow(plotly::plotlyOutput("plt_mar_aca", height = "100px" ) ),
    fluidRow(boxPlus(width = 6, highchartOutput("hist_quant_tipo")),
             boxPlus(width = 6, plotly::plotlyOutput("barra_ti"))),
    fluidRow(boxPlus(width = 6, dataTableOutput("rest")),
             boxPlus(width = 6, dataTableOutput("top_rest"))
      ) 
    
    )
  ) 
  
  )



# User Interface ----------------------------------------------------------

UI <-
  dashboardPagePlus(
     header = Header
    ,sidebar = Sidebar
    ,body = Body
    ,footer = Footer
    ,title = "Simulador da Carreia do MPGO"
  )



# Server ------------------------------------------------------------------

Server <- function(input, output){
    
  output$total_pedidos <- renderValueBox({
    
    valueBox(tot_ped, subtitle = "Total Pedidos",
             color = "red", icon = icon("shopping-cart") ) })
  
  output$med_p_m <- renderValueBox({
    
    valueBox(media_ped_mes, subtitle = "Média Pedidos/Mês",
             color = "red", icon = icon("shopping-cart") )
    
  })
  
  output$med_v_m <- renderValueBox({
    
    valueBox(media_val_mes, subtitle = "Média Valor/Mês",
             color = "red", icon = icon("shopping-cart") )
    
  })
    
  output$total_valor <- renderValueBox({
    
    valueBox(tot_valor, subtitle = "Valor total Gasto",
             color = "red", icon = icon("money-bill-wave-alt")) })
  
  output$media_sem_p <- renderValueBox({
    
    valueBox(round(med_ped_sem, 2), subtitle = "Média semanal pedido",
             color = "red", icon = icon("cart-plus"))
  })
  
  output$media_sem_v <- renderValueBox({
    
    valueBox(round(med_valo_sem, 2), subtitle = "Média valor semanal",
             color = "red", icon = icon("cart-plus"))
  })
  
  output$hist_valor <- renderHighchart({ hist_valor })
  
  output$ped_dia <- renderHighchart({ Quant_pe_sem })
  
  output$serie_valor <- renderHighchart({ plot_his_valo})
  
  output$marmita <- renderHighchart({ mar })
  
  output$acai <- renderHighchart({ aca })
  
  output$hist_quant_tipo <- renderHighchart({ p_tipo_mes })
  
  output$rest <- renderDataTable({ 
    tipo_rest %>% datatable(class = "cell-border stripe",
                            colnames = c("Tipo", "Quantidade",
                                         "Valor Médio Gasto",
                                         "Valor Total Gasto"),
                            options = list(pageLength = nrow(tipo_rest))
                            )
    })
  
  output$top_rest <- renderDataTable({
    top10 %>% datatable(class = "cell-border stripe",
                        colnames = c("Restaurante", "Quantidade",
                                     "Valor Médio Gasto",
                                     "Valor Total Gasto")
                        )
  })
  
  output$barra_ti <- plotly::renderPlotly({ barra_tipo   })
  
  
  output$plt_mar_aca <- plotly::renderPlotly({ plot_mar_acai})
    
  }


shinyApp(ui = UI, server = Server)