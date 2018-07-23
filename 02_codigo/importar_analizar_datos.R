### Paquetes ----
library(pacman)
p_load(animation, cowplot, curl, extrafont, 
       forcats, gganimate, ggforce, ggmap, 
       ggraph, ggrepel, ggridges, janitor, 
       lubridate, mapdata, maps, maptools, 
       purrr, readxl, rgdal, rgeos, scales, sf,
       sp, stringi, stringr, stringdist, tweenr, 
       tidyr, tidygraph, tidyverse, treemapify, zoo)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)

### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family="Didact Gothic Regular"))

### Importar datos ----
ingresos_efectivo <- read_excel("01_datos/CGor201807-18-rp-5-4-a1 sin macro.xlsx")

ingresos_cheque <- read_excel("01_datos/CGor201807-18-rp-5-4-a2.XLSX", 
                              range = "a5:e158", 
                              col_types = c("numeric", "text", "text", "date", "numeric"))

egresos_cheque <- read_excel("01_datos/CGor201807-18-rp-5-4-a3.XLSX", range = "a4:e173")


### "Limpiar" nombres de columnas ----
ingresos_efectivo <- clean_names(ingresos_efectivo)
ingresos_cheque <- clean_names(ingresos_cheque)
egresos_cheque <- clean_names(egresos_cheque)


### Construir variable fecha para el df ingresos_efectivo----
ingresos_efectivo <- ingresos_efectivo %>% 
  mutate(fecha = make_date(ano, mes, dia))

### Calcular depósitos diarios en efectivo ----
ingresos_efectivo %>% 
  filter(fecha < "2018-06-01") %>% 
  group_by(fecha) %>% 
  summarise(total_diario = sum(monto)) %>% 
  ungroup() %>% 
  mutate(total_acumuldo = cumsum(total_diario)) %>% 
  ggplot() +
  geom_line(aes(fecha, total_diario/1000000)) +
  labs(title = "Depósitos en efectivo al Fideoicomiso",
       x = "\nFecha",
       y = "Millones de pesos\n")


### Treemap de ingresos via cheque ----
ingresos_cheque %>% 
  group_by(nombre_emisor) %>% 
  summarise(monto_por_aportante = sum(importe)) %>% 
  ungroup() %>% 
  mutate(por = round((monto_por_aportante/sum(monto_por_aportante)*100), 1)) %>% 
  rename(nombre = nombre_emisor) %>% 
  ggplot() +
  geom_treemap(aes(area = monto_por_aportante, fill = monto_por_aportante), col = "white") +
  geom_treemap_text(aes(area = monto_por_aportante, label = nombre), fontface = "bold", color = "white") +
  geom_treemap_text(aes(area = monto_por_aportante, label = paste("$", comma(round(monto_por_aportante, 0)), sep = "")), color = "white", padding.y = unit(8, "mm"), size = 16) +
  geom_treemap_text(aes(area = monto_por_aportante, label = paste(por, "% del total", sep = "")), color = "white", padding.y = unit(14.5, "mm"), size = 15) +
  scale_fill_gradient(low = "grey80", high = "#a50300", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "¿QUIÉNES APORTARON DINERO CON CHEQUE AL FIDEICOMISO POR LOS DEMÁS?",
       subtitle = "El tamaño de cada rectángulo es proporcional al monto de la aportación total realizada por cada persona al fideicomiso Por Los Demás.\nMientras más grande y rojo el recuadro, mayor la aportación hecha por dicha persona.",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: Anexo 2 del punto 5.4 de la sesión del 18 de julio de 2018 del Consejo General del INE (https://bit.ly/2A0VMnb).") +
  tema +
  theme(legend.position = "none")

ggsave(filename = "ingresos_cheque.png", path = "03_graficas/", width = 15, height = 10)


### Treemap de egresos via cheque ----
egresos_cheque %>% 
  group_by(nombre) %>% 
  summarise(monto_por_aportante = sum(monto)) %>% 
  ungroup() %>% 
  mutate(por = round((monto_por_aportante/sum(monto_por_aportante)*100), 1)) %>%
  ggplot() +
  geom_treemap(aes(area = monto_por_aportante, fill = monto_por_aportante), col = "white") +
  geom_treemap_text(aes(area = monto_por_aportante, label = nombre), fontface = "bold", color = "white") +
  geom_treemap_text(aes(area = monto_por_aportante, label = paste("$", comma(round(monto_por_aportante, 0)), sep = "")), color = "white", padding.y = unit(8, "mm"), size = 16) +
  geom_treemap_text(aes(area = monto_por_aportante, label = paste(por, "% del total", sep = "")), color = "white", padding.y = unit(14.5, "mm"), size = 15) +
  scale_fill_gradient(low = "grey80", high = "#a50300", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = "¿QUIÉNES RECIBIERON DINERO DEL FIDEICOMISO POR LOS DEMÁS?",
       subtitle = "El tamaño de cada rectángulo es proporcional al monto total del o los cheques de caja emitidos por el fideicomiso Por Los Demás a favor de dicha persona.\nMientras más grande y rojo el recuadro, mayor el monto de los recursos que recibió dicha persona.",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: Anexo 3 del punto 5.4 de la sesión del 18 de julio de 2018 del Consejo General del INE (https://bit.ly/2A0VMnb).") +
  tema +
  theme(legend.position = "none")

ggsave(filename = "egresos_cheque.png", path = "03_graficas/", width = 15, height = 10)
