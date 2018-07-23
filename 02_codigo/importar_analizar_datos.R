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
                              range = "a5:e163", 
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
  labs(title = "¿QUIÉNES APORTARON DINERO CON CHEQUE AL FIDEICOMISO \"POR LOS DEMÁS\"?",
       subtitle = "El tamaño de cada rectángulo es proporcional al monto de la aportación total realizada por cada persona al fideicomiso \"Por Los Demás\".\nMientras más grande y rojo el recuadro, mayor la aportación hecha por dicha persona.",
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
  labs(title = "¿QUIÉNES RECIBIERON DINERO DEL FIDEICOMISO \"POR LOS DEMÁS\"?",
       subtitle = "El tamaño de cada rectángulo es proporcional al monto total del o los cheques de caja emitidos por el fideicomiso \"Por Los Demás\" a favor de dicha persona.\nMientras más grande y rojo el recuadro, mayor el monto de los recursos que recibió dicha persona.",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: Anexo 3 del punto 5.4 de la sesión del 18 de julio de 2018 del Consejo General del INE (https://bit.ly/2A0VMnb).") +
  tema +
  theme(legend.position = "none")

ggsave(filename = "egresos_cheque.png", path = "03_graficas/", width = 15, height = 10)

### Scatterplot de monto de donativos vs. número de donativos ----
ingresos_efectivo %>% 
  count(monto, sort = T) %>% 
  mutate(total_donativos = sum(n), 
         por_donativos = round((n/total_donativos)*100, 1),
         total_por_monto = monto * n,
         total = sum(total_por_monto, na.rm = T),
         por_acumulado_por_monto = round((total_por_monto/total)*100, 1)) %>% 
  ggplot() +
  geom_point(aes(x = monto, y = n, size = total_por_monto/1000000), color = "#a50300", alpha = 0.6) +
  geom_curve(aes(x = 125000, y = 330, xend = 60000, yend = 360),
             arrow = arrow(length = unit(0.03, "npc")),
             size = 1.5, 
             color = "grey50") +
  annotate(geom = "text", x = 125000, y = 265, size = 5, label = "De acuerdo con el INE, el fideicomiso recibió\n$17.55 millones en efectivo a través de 351\ndepósitos de $50,000.\n\nEstos $17.55 millones representan el 39.5%\nde los $44.4 millones que en total recibió el\nfideicomiso en efectivo.", family="Didact Gothic Regular", hjust = 0) +
  scale_x_continuous(breaks = c(seq(0, 200000, 50000), 700000), limits = c(0, 705000), labels = comma) +
  scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, 50)) +
  scale_size_continuous(guide = guide_legend(direction = "horizontal", 
                                             title.position = "top"),
                        range = c(1, 10),
                        breaks = c(0.5, 5, 10, 15)) +
  labs(title = "NÚMERO DE DONATIVOS AL FIDEICOMISO \"POR LOS DEMÁS\", POR MONTO",
       x = "\nMonto del donativo",
       y = "Núm. de donativos\n",
       size = "Suma de donativos recabados\npor cada monto (millones)",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: Anexo 1 del punto 5.4 de la sesión del 18 de julio de 2018 del Consejo General del INE (https://bit.ly/2A0VMnb).\n\nNota: El Anexo 1 indica que 14 de los depósitos de $50,000 ocurrieron el 28 de diciembre de 2018. Si bien es probable que la fecha correcta sea 27 de\ndiciembre de 2017, esto debe ser aclarado por la autoridad electoral.") +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = c(0.8, 0.85))


ggsave(filename = "montos_vs_total_recabado_efectivo.png", path = "03_graficas/", width = 15, height = 10)


### Gráficas de líneas de depósitos diarios y depósitos acumulacos por tipo de depósito ----

# Primero, construir un nuevo dataframe usando sólo las columnas que necesito de los ingresos_efectivo e ingresos_cheque 

datos_efectivo <- ingresos_efectivo %>% 
  select(fecha, monto) %>% 
  mutate(tipo = "Depósitos en efectivo")

datos_cheques <- ingresos_cheque %>% 
  select(fecha_sello, importe) %>% 
  rename(fecha = fecha_sello,
         monto = importe) %>% 
  mutate(tipo = "Depósitos con cheque",
         fecha = as_date(fecha))

# Unir columnas seleccionadas y generar un nuevo dataframe llamado datos
datos <- rbind(datos_efectivo, datos_cheques)

# Gráfica de depósitos totales diarios
datos %>%
  filter(fecha > "2017-09-01",
         fecha < "2018-06-01") %>%  # Con este filtro elimino las observaciones con fechas que me parece son equivocadas en los anexos del INE
  group_by(tipo, fecha) %>% 
  summarise(total_diario = sum(monto)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(fecha, total_diario/1000000, group = tipo, color = tipo), size = 1.2, alpha = 0.8) +
  scale_x_date(date_breaks = "1 months", date_labels = ("%b")) +
  scale_y_continuous(breaks = seq(0, 8, 1)) +
  scale_color_manual(values = c("grey60", "#a50300")) +
  labs(title = "DEPÓSITOS DIARIOS AL FIDEICOMISO \"POR LOS DEMÁS\", POR TIPO DE DEPÓSITO",
       subtitle = "Datos del 26 de septiembre de 2017 al 31 de mayo de 2018.",
       x = NULL,
       y = "Depósitos totales diarios\n(Millones de pesos)\n",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: Anexos 1 y 2 del punto 5.4 de la sesión del 18 de julio de 2018 del Consejo General del INE (https://bit.ly/2A0VMnb).\n\nNota: El Anexo 1 indica que 14 de los depósitos de $50,000 ocurrieron el 28 de diciembre de 2018. Si bien es probable que la fecha correcta sea 27 de diciembre\nde 2017, esto debe ser aclarado por la autoridad electoral. De igual forma, el Anexo 2 indica que un depósito en cheque ocurrió el 27 de abril de 2017, cuando\nprobablemente fue el 27 de abril de 2018. La gráfica no incluye estas 15 observaciones. Tampoco incluye 15 depósitos en cheque por un total de $2.2 millones para los cuales no hay fecha.",
       color = NULL) +
  tema +
  theme(legend.position = c(0.85, 0.9),
        legend.direction = "vertical",
        legend.text = element_text(size = 15))

ggsave(filename = "depositos_diarios_por_tipo.png", path = "03_graficas/", width = 16, height = 11)


# Gráfica de depósitos acumulados diariamente
datos %>%
  filter(fecha > "2017-09-01",
         fecha < "2018-06-01") %>%  # Con este filtro elimino las observaciones con fechas que me parece son equivocadas en los anexos del INE
  group_by(tipo, fecha) %>% 
  summarise(total_diario = sum(monto, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(tipo, fecha) %>% 
  group_by(tipo) %>% 
  mutate(total_acumulado = cumsum(total_diario)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(fecha, total_acumulado/1000000, group = tipo, color = tipo), size = 1.2, alpha = 0.8) +
  scale_x_date(date_breaks = "1 months", date_labels = ("%b")) +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0, 50)) +
  scale_color_manual(values = c("grey60", "#a50300")) +
  labs(title = "DEPÓSITOS ACUMULADOS DIARIAMENTE AL FIDEICOMISO \"POR LOS DEMÁS\", POR TIPO DE DEPÓSITO",
       subtitle = "Datos del 26 de septiembre de 2017 al 31 de mayo de 2018.",
       x = NULL,
       y = "Depósitos acumulados diariamente\n(Millones de pesos)\n",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: Anexos 1 y 2 del punto 5.4 de la sesión del 18 de julio de 2018 del Consejo General del INE (https://bit.ly/2A0VMnb).\n\nNota: El Anexo 1 indica que 14 de los depósitos de $50,000 ocurrieron el 28 de diciembre de 2018. Si bien es probable que la fecha correcta sea 27 de diciembre de 2017,\nesto debe ser aclarado por la autoridad electoral. De igual forma, el Anexo 2 indica que un depósito en cheque ocurrió el 27 de abril de 2017, cuando probablemente\nfue el 27 de abril de 2018. La gráfica no incluye estas 15 observaciones. Tampoco incluye 15 depósitos en cheque por un total de $2.2 millones para los cuales no hay fecha.",
       color = NULL) +
  tema +
  theme(legend.position = c(0.15, 0.9),
        legend.direction = "vertical",
        legend.text = element_text(size = 18))

ggsave(filename = "depositos_acumulados_por_tipo.png", path = "03_graficas/", width = 17, height = 12)








