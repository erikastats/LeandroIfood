
# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(readr)
library(lubridate)
library(plotly)
library(highcharter)


# Dados -------------------------------------------------------------------

pedidos_ <- read_csv("pedidos..csv") %>% select(-id_usuario)

pedidos_ = pedidos_ %>% 
  mutate(restaurante = restaurante %>% tolower()) %>% 
  mutate(tipo = case_when(
    str_detect(restaurante, "crema") ~ "sorvete",
    str_detect(restaurante, "sorvete") ~ "sorvete",
    str_detect(restaurante, "acai") ~ "açaí",
    str_detect(restaurante, "pamonharia") ~ "pamonha",
    str_detect(restaurante, "pastel") ~ "pastel",
    str_detect(restaurante, "supermer") ~ "supermercado",
    str_detect(restaurante, "burger") ~ "sanduíche",
    str_detect(restaurante, "subway") ~ "sanduíche",
    str_detect(restaurante, "salgado") ~ "salgado",
    str_detect(restaurante, "china") ~ "asiática",
    str_detect(restaurante, "japonesa") ~ "asiática",
    str_detect(restaurante, "koni") ~ "asiática",
    str_detect(restaurante, "restaurante") ~ "marmita",
    str_detect(restaurante, "grill") ~ "marmita",
    str_detect(restaurante, "jantinha") ~ "marmita",
    str_detect(restaurante, "espeto") ~ "marmita",
    str_detect(restaurante, "jantinha") ~ "marmita",
    str_detect(restaurante, "strogo") ~ "strogonofe",
    str_detect(restaurante, "pizza") ~ "pizza",
    str_detect(restaurante, "doce") ~ "doce",
    str_detect(restaurante, "emporio") ~ "supermercado",
    str_detect(restaurante, "marmit") ~ "marmita",
    str_detect(restaurante, "sanduic") ~ "sanduíche",
    TRUE ~ "outros"
    
  )) %>% 
  mutate(tipo = tipo %>% str_to_title())

# Análise descritiva ------------------------------------------------------


# Total de pedidos

tot_ped <- pedidos_ %>% nrow()

# Média quantidade de pedidos por mês

media_ped_mes <- pedidos_ %>% 
  mutate(mes = data_pedido %>% month(),
         ano = data_pedido %>% year()) %>% 
  group_by(mes,ano) %>% 
  summarise(Quant = n()) %>% 
  ungroup() %>% 
  summarise( Media = mean(Quant)) %>% 
  pull

# pedidos_ %>% 
#   mutate(mes = data_pedido %>% month(),
#          ano = data_pedido %>% year()) %>% 
#   group_by(mes,ano) %>% 
#   summarise(Quant = n()) %>% 
#   filter(ano == 2020) %>% 
#   ungroup() %>% 
#   summarise( Media = mean(Quant)) %>% 
#   pull

# pedidos_ %>% 
#   summarise(Media = mean(valor),
#             Desvio_padrao = sd(valor) )   

# Média valor mensal

media_val_mes <- pedidos_ %>% 
  mutate(mes = data_pedido %>% month(),
         ano = data_pedido %>% year()) %>% 
  group_by(mes,ano) %>% 
  summarise(Quant = sum(valor)) %>% 
  ungroup() %>% 
  summarise( Media = round(mean(Quant), 2) ) %>% 
  pull

# Valor Total gasto

tot_valor <- pedidos_ %>% summarise(Valor = sum(valor)) %>% pull

# Média pedido semanal

med_ped_sem <- pedidos_ %>% arrange(data_pedido) %>% 
  mutate(semana = data_pedido %>% week(),
         ano =  data_pedido %>% year()
          )  %>% 
  group_by(ano, semana) %>% 
  summarise(Quant = n()) %>% 
  ungroup() %>% 
  summarise(Media = mean(Quant)) %>% 
  pull
 


# Média valor semanal 

med_valo_sem <- pedidos_ %>% arrange(data_pedido) %>% 
  mutate(semana = data_pedido %>% week(),
         ano =  data_pedido %>% year()
  )  %>% 
  group_by(ano, semana) %>% 
  summarise(Quant = sum(valor)) %>% 
  ungroup() %>% 
  summarise(Media = mean(Quant)) %>% 
  pull

# Histograma do valor dos pedidos

hist_valor <-   hchart(pedidos_$valor, color = "#6b34eb",
                       name = "Valor de Pedido", showInLegend = F  ) %>% 
  hc_title( text = "Histograma do valor dos pedidos")
  
# Quantidade de pedido por semana

Quant_pe_sem <- pedidos_ %>% arrange(data_pedido) %>% 
  mutate(semana = data_pedido %>% week(),
         ano =  data_pedido %>% year()
  )  %>% 
  group_by(ano, semana) %>% 
  summarise(Quant = n()) %>%  
  ungroup() %>% 
  group_by(Quant) %>% 
  summarise(Q = n()) %>% 
  # plot_ly()
  hchart("pie", hcaes(x = paste0(Quant, " Ped."), y = Q),
         name = "Pedido por semana", 
         dataLabels = list(enabled = TRUE, 
                           format = '{point.percentage:.1f} %')) %>% 
  hc_title( text = "Quantidade de pedido por semana")



# Série Histórica quantidade Pedido

# quant_ped <- pedidos_ %>% 
#   group_by(data_pedido) %>% 
#   summarise(Quant = n()) %>% 
#   ungroup()
# quant_ped <- xts::xts(x = quant_ped$Quant, order.by = quant_ped$data_pedido)
# plot_hist_pedido <- highchart() %>% 
#   hc_add_series(quant_ped)
  
  #plot_ly(x = ~data_pedido, y = ~Quant, trace = "scatter", mode = "lines")
  
# Série Histórica Valor Pedido

# pedidos_ %>% 
#   group_by(data_pedido) %>% 
#   summarise(Quant = sum(valor)) %>% 
#   plot_ly(x = ~data_pedido, y = ~Quant, trace = "scatter", 
#           mode = 'lines')  
 
plot_his_valo <- pedidos_ %>% 
  group_by(data_pedido) %>% 
  summarise(Quant = sum(valor)) %>% 
  hchart("line", hcaes(x = data_pedido, y = Quant)) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_yAxis(title = list(text = "Valor"))

# valorPe = pedidos_ %>% 
#   group_by(data_pedido) %>% 
#   summarise(Quant = sum(valor)) %>% 
#   column_to_rownames(var = "data_pedido") %>% 
#   dygraphs::dygraph() %>% 
#   dygraphs::dyRangeSelector()


# Tipo de restaurantes

tipo_rest <- pedidos_ %>% 
  group_by(tipo) %>% 
  summarise(Quant = n(),
            Media_tipo = round(mean(valor), 2),
            Total_tipo = sum(valor)) %>% 
  arrange(desc(Quant)) 

# marmita e acai

D_mas_aca <-  tipo_rest %>% ungroup() %>% 
  mutate(tipo2 = case_when(
    tipo == "Marmita" ~ "Marmita",
    tipo == "Açaí" ~ "Açaí",
    TRUE ~ "Outros"
  )) %>% 
  group_by(tipo2) %>% 
  summarise(quant = sum(Quant)) %>% 
  ungroup() %>% 
  mutate(Porc = (quant/sum(quant))*100 ) 
valor_acai = D_mas_aca %>% filter(tipo2 == "Açaí") %>% pull(Porc)
valor_marmita = valor_acai = D_mas_aca %>% filter(tipo2 == "Marmita") %>% pull(Porc)
valor_acai = D_mas_aca %>% filter(tipo2 == "Outros") %>% pull(Porc)
tipo_d = "T"

plot_mar_acai <- D_mas_aca %>% select(-quant) %>% 
  spread(tipo2, Porc) %>% bind_cols(TI = c("Tipo")) %>% 
  plotly::plot_ly(y = ~TI, x = ~`Açaí`, type = "bar", name = "Açaí", orientation = 'h',
          text = paste0("Açaí: ",  D_mas_aca$Porc[1],"%<br>"),
          textposition = 'auto') %>% 
  plotly::add_trace(x = ~Marmita, name = "Marmita",
            text = paste0("Marmita: ",D_mas_aca$Porc[2],"%<br>"),
            textposition = 'auto') %>% 
  plotly::add_trace(x = ~Outros, name = "Outros",
            text = paste0("Outros: ",D_mas_aca$Porc[3],"%<br>"),
            textposition = 'auto') %>% 
  plotly::layout(xaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
         barmode = 'stack',
         showlegend = FALSE)  %>% config(displayModeBar = F)

  
# mar <-  tipo_rest %>% ungroup() %>% 
#   mutate(tipo2 = ifelse(tipo == "Marmita", tipo, "Outros")) %>% 
#   group_by(tipo2) %>% 
#   summarise(quant = sum(Quant)) %>% 
#   ungroup() %>% 
#   hchart("pie", hcaes(x = tipo2, y = quant), name = "Marmita",
#          showInLegend = F, 
#          dataLabels = list(enabled = TRUE, 
#                            format = '{point.percentage:.1f} %',
#                            distance = -30)) %>% 
#   hc_title( text = "Porcentagem \nde Marmita")
# 
# aca <- tipo_rest %>% ungroup() %>% 
#   mutate(tipo2 = ifelse(tipo == "Açaí", tipo, "Outros")) %>% 
#   group_by(tipo2) %>% 
#   summarise(quant = sum(Quant)) %>% 
#   ungroup() %>% 
#   hchart("pie", hcaes(x = tipo2, y = quant), name = "Açaí",
#          showInLegend = F, 
#          dataLabels = list(enabled = TRUE, 
#                            format = '{point.percentage:.1f} %',
#                            distance = -30)
#          ) %>% 
#   hc_title( text = "Porcentagem \nde Açaí")

# Quantidade por mês por tipo
tipo_mes <-   pedidos_ %>% 
  mutate(mes = data_pedido %>% month(),
         ano = data_pedido %>% year()) %>% 
  group_by(ano, mes, tipo) %>% 
  summarise(Quant = n()) %>% 
  mutate(date  = paste(ano,mes,"01") %>% ymd())

p_tipo_mes <- tipo_mes %>%
  hchart("line", hcaes(x = date, y = Quant, group = tipo))


quant_mes <- pedidos_ %>% 
  mutate(mes = data_pedido %>% month(),
         ano = data_pedido %>% year()) %>% 
  group_by(ano, mes) %>% 
  summarise(Quant = n()) %>% 
  mutate(date  = paste(ano,mes,"01") %>% ymd())

quant_mes %>% hchart("line", hcaes(x = date, y = Quant))


# Top 10 restaurantes
  
 top10 <-  pedidos_ %>% 
  group_by(restaurante) %>% 
  summarise(Quant = n(),
            Media_restaurante = round(mean(valor),2),
            Total_valor = sum(valor)) %>% 
  arrange(desc(Quant)) %>% 
    head(10)
  
# frequência de pedidos por tipo

  # pedidos_ %>% group_by(tipo) %>% 
  #   summarise( Quant = n()) %>% 
  #   ungroup() %>% 
  #   mutate(tipo = ifelse(Quant<4, "outros", tipo)) %>% 
  #   # filter(Quant >4) %>% 
  #   plot_ly(labels = ~tipo, values = ~Quant) %>% 
  #   add_pie(hole = 0.7)

# Valor por tipo
  
barra_tipo <-   pedidos_ %>% group_by(tipo) %>% 
    summarise( Valor = sum(valor)) %>% 
    filter(Valor >140) %>%
    # hchart(type = "bar",
    #        hcaes(x = Valor, y = tipo), names = "Valor por tipo")
        plotly::plot_ly(x = ~reorder(tipo, desc(Valor) ),
            y = ~Valor, type = "bar") %>% 
      plotly::layout(xaxis = list(title = "Tipo"),
           yaxis = list(title = "Valor Total"))
  
# Quantidade por mês
  # pedidos_ %>% 
  #   mutate(mes = data_pedido %>% month(),
  #                     ano = data_pedido %>% year()) %>% 
  #   group_by(ano, mes) %>% 
  #   summarise(Quant = n()) %>% 
  #   mutate(date  = paste(ano,mes,"01") %>% ymd()) %>% 
  #   hchart("line", hcaes(x = date, y = Quant))
  

    
  
# Série Histórica por tipo valor
  # pedidos_ %>% 
  #   plotly:: plot_ly(type = 'scatter', mode = "lines",
  #           x = ~data_pedido, y = ~valor, split = ~tipo)

# Quantidade média de cada tipo por mês
  
 mes_tipo <- sapply(tipo_mes %>% ungroup() 
                    %>% pull(tipo) %>% unique(),
        function(tipou){
          tipo_mes %>%  ungroup() %>%  filter(tipo == tipou) %>% 
            summarise(meses = interval(min(date), max(date)) %/% months(1)) %>% 
                             pull(meses)
        }) %>% as.data.frame() %>% rownames_to_column("tipo")
names(mes_tipo)[2] <- "Quant_mes"
 
# tipo_mes %>% ungroup() %>%   group_by(tipo) %>% 
#   summarise(Total_ped = sum(Quant)) %>% 
#   left_join(mes_tipo) %>% 
#   mutate(Media_tipo = Total_ped/Quant_mes)

# Acumulada da compra por mês
# tipo_mes %>% ungroup() %>% 
#   group_by(tipo) %>%   
#   arrange(tipo, date) %>% 
#   mutate(acumulada = cumsum(Quant)) %>% 
#   hchart("line", hcaes(x = date, y = acumulada, group = tipo))
