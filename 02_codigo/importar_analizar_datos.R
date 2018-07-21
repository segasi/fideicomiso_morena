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
  geom_line(aes(fecha, total_diario)) +
  tema


