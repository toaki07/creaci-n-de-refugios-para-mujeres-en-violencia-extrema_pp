### Ensayo: La violencia machista y la defensa/resistencia ante ello


### Código elaborado por Toaki Hoz Canabal 



# Links de datos e información --------------------------------------------


# Censo de alojamiento de asistencia social -->  https://www.inegi.org.mx/programas/caas/2015/#Herramientas 

# Meta datos del censo de alojamiento de asisencia social --> https://www.inegi.org.mx/rnm/index.php/catalog/217/study-description

# Carpetas de investigación PGJ De la CDMX --> https://datos.cdmx.gob.mx/dataset/carpetas-de-investigacion-pgj-cdmx

# Porcentaje de casos impunes -->  https://www.eluniversal.com.mx/nacion/impunidad-delitos-contra-mujeres-quedan-sin-castigo

# Catálogo de delitos contra la mujer --> https://catedraunescodh.unam.mx/catedra/mujeres_ORIGINAL/menu_superior/Feminicidio/2_Info_nac/informe/35.pdf

# Delitos contra las mujeres 2011/ INEGI -->  http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/metodologias/est/DCM_2011.pdf

# Principales delitos contra las mujeres CNDH --> https://www.cndh.org.mx/sites/all/doc/programas/mujer/IndicadoresViolencia/Delitoscontramujeres_1ersem_2014.pdf

# En casi todo el territorio mexicano, se localizan 72 refugios subsidiados por el CNEGSR (Centro Nacional de Equidad de Género y Salud Reproductiva) y operados por Organizaciones de la Sociedad Civil, además de aquellos que no son gubernamentales.

# Encuesta de enkoll sobre la violencia machista --> https://www.excelsior.com.mx/nacional/mujeres-acoso-sexual-encuesta/1502557

# Encuesta de reforma sobre la percepción de la pandemia --> https://gruporeforma-blogs.com/encuestas/?p=8748

# Colonias (revisar):

## https://datos.cdmx.gob.mx/dataset/coloniascdmx/resource/03368e1e-f05e-4bea-ac17-58fc650f6fee
## https://idegeo.centrogeo.org.mx/layers/geonode%3Acolonias_seduvi_inv_viviendas_2016_inegi
## https://idegeo.centrogeo.org.mx/layers/geonode%3Acolonias_seduvi

#### Programa a usar ---------------------------------------------------------
# path <- "C:\\Users\\BryanToaki\\Documents\\Polakas\\Violencia contra la mujer\\Ensayo\\POLITICAS_CEAMEG.pdf"

# De la página 31 de path "Programa de Apoyo a Refugios para Mujeres, sus Hijos e Hijas que viven en Situación de Violencia Extrema"



# Setup -------------------------------------------------------------------



# Librerias ---------------------------------------------------------------
pacman::p_load(geofacet,lubridate,readxl,tidyverse)





# Separamos latitud y longitud  -------------------------------------------

# si estuvieran en una misma columna
var0 <- c("25.555555,-99.1969", "28.9555,-79.19")
lat <- NA
lon <- NA
for (i in seq_along(var0)){
  aux <- unlist(strsplit(var0[i], split = ","))
  lon[i] <- aux[2]; lat[i] <- aux[1]
}
print(lon)
print(lat)


### Cargamos bases -----------

carpetas <- 
  read_excel("01_datos/Carpetas de investigación abiertas sobre delitos cometidos contra mujeres CDMX.xlsx", 
                       sheet = 2, skip = 16) %>% 
  janitor::clean_names()


refugios <- read_excel("01_datos/principales_resultados_por_alojamiento_de_asistencia_social_xls/Alojamientos de asistencia social.xls")
(refugios <- refugios[-1,])

#### Entidades por clave, entidades por número, diccionario de entidades, estados por número, diccionario de estados por clave
clasi_ent <- tibble(
                   Clave.entidad = c("01","02","03","04","05","06","07","08",
                                     "09","10","11","12","13","14","15",
                                     "16","17","18","19","20","21","22",
                                     "23","24","25","26","27","28","29",
                                     "30","31","32"),
               Nombre.de.entidad = c("Aguascalientes","Baja California",
                                     "Baja California Sur","Campeche",
                                     "Coahuila de Zaragoza","Colima","Chiapas","Chihuahua",
                                     "Ciudad de México","Durango","Guanajuato",
                                     "Guerrero","Hidalgo","Jalisco","México",
                                     "Michoacán de Ocampo","Morelos",
                                     "Nayarit","Nuevo León","Oaxaca","Puebla",
                                     "Querétaro","Quintana Roo",
                                     "San Luis Potosí","Sinaloa","Sonora","Tabasco",
                                     "Tamaulipas","Tlaxcala",
                                     "Veracruz de Ignacio de la Llave","Yucatán","Zacatecas")) %>% 
  janitor::clean_names() %>% 
  mutate(clave_entidad = as.numeric(clave_entidad))

unique(carpetas$tipo_de_delito)
names(carpetas)
unique(carpetas$ano_de_inicio)
names(refugios)
# ### ### ### ## # # # # # # # # # #  ## # # # # # ## cómo puedo hacer el mes como mes de date no de character
# carpetas$mes_de_inicio
# as.Date(carpetas$mes_de_inicio)
# as.Date(carpetas$mes_de_inicio, format = "%B")
# 
# 
# carpetas %>% 
#   mutate_at(.vars = c("fecha_de_inicio","fecha_del_hecho"), .funs = as.Date)
# 
# carpetas %>% 
# mutate_at(.vars = c("fecha_de_inicio","fecha_del_hecho"), .funs = as.Date(format = "%d,%m,%Y")) # como puedo cambiar las variables de un jalon 
# 
# 
# 
  carpetas %>%
    # rename(hora_de_inicio = hora_de_inicio_6) %>%
    # select(-hora_de_inicio_18) %>%
    mutate(hora_de_inicio = as.POSIXct(strptime(hora_de_inicio, "%H:%m"))) ### Cómo arreglo la hora


# modificamos bases -------------------------------------------------------

carpetas <- carpetas %>% 
    rename(hora_de_inicio = hora_de_inicio_6) %>% 
    select(-hora_de_inicio_18) %>% 
    mutate(fecha_de_inicio = as.Date(fecha_de_inicio, format = "%d/%m/%Y"),
           fecha_del_hecho = as.Date(fecha_del_hecho, format = "%d/%m/%Y")) %>% 
    filter(estado == "Ciudad de México",
           alcaldia != "Chapultepec")


write.csv(carpetas, "01_datos/carpeta_limpia.csv")

refugios <- refugios %>% 
  filter(clasealoja  %in%  c(5,11)) 




# Analizamos y  graficamos --------------------------------------------------------------
carpetas %>% 
  group_by(alcaldia) %>% 
  summarise(total = n()) %>% 
  arrange(-total) # Total de carpetas por alcaldia, ahora a relacionarlo con la capacidad de los albergues

refugios %>% 
  mutate(capacidad = as.numeric(capacidad),
         entidad = as.numeric(entidad)) %>% 
  arrange(entidad) %>% 
  left_join(clasi_ent,by = c("entidad" = "clave_entidad")) %>% 
  select(nombre_de_entidad, everything()) %>% 
  filter(entidad == 9) %>% 
  select(capacidad) %>% 
  arrange(-capacidad)
  
400/6321 * 100 # Ratio capacidad de aceptadas/carpetas judiciales de una alcaldia * 100
400-6321

18/3627*1000

unique(refugios$entidad)

refugios %>% 
  mutate(capacidad = as.numeric(capacidad),
         entidad = as.numeric(entidad)) %>% 
  arrange(entidad) %>% 
  left_join(clasi_ent,by = c("entidad" = "clave_entidad")) %>% 
  select(nombre_de_entidad, everything()) %>% 
  group_by(inicio_act) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(inicio_act, total, fill = total)) +
  geom_col()+
  geom_text(aes(inicio_act, total+0.5,
                label = total)) +
  scale_fill_gradient(low =  "black", high = "pink") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1.1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Año de creación del refugio",
       y = "Total de refugios fundados",
       title = "Figura 2: Distribucion de creación de refugios por año 1907-2015",
       subtitle = "Refugios creados a nivel nacional",
       caption = "Fuente: Elaboración propia con datos del Censo de Alojamientos de Asistencia\n Social con corte al 2016 y actualizados el 2021. (Junio,2022)")



refugios %>% 
  mutate(capacidad = as.numeric(capacidad),
         entidad = as.numeric(entidad)) %>% 
  arrange(entidad) %>% 
  left_join(clasi_ent,by = c("entidad" = "clave_entidad")) %>% 
  select(nombre_de_entidad, everything()) %>% 
  group_by(nombre_de_entidad, inicio_act) %>% 
  summarise(total = n()) %>% 
  filter(inicio_act >= 2001) %>% 
  ggplot(aes(inicio_act, total, fill = total)) +
  geom_col() + 
  geom_text(aes(inicio_act, total +0.3, label = total), size =4) +
  facet_wrap(~nombre_de_entidad, ncol = 5) +
  scale_fill_gradient(low = "black", high = "pink") +
  theme_bw()+
  labs(x = "Año de creación de refugio",
       y = "Total de refugios fundados",
       title = "Refugios por entidad federativa y año de fundación",
       caption = "Fuente: Elaboración propia con datos del Censo de Alojamientos de Asistencia\n Social con corte al 2016 y actualizados el 2021. (Junio,2022)")+
  theme(axis.text.x = element_text(angle = 65, size = 8, hjust = 1.7, vjust =1.8),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) 


### faceteamos con posición del Edo, pero con mx_state_grid2 -----------------
refugios %>% 
  mutate(capacidad = as.numeric(capacidad),
         entidad = as.numeric(entidad)) %>% 
  arrange(entidad) %>% 
  left_join(clasi_ent,by = c("entidad" = "clave_entidad")) %>% 
  select(nombre_de_entidad, everything()) %>% 
  group_by(nombre_de_entidad, inicio_act) %>% 
  summarise(total = n()) %>% 
  filter(inicio_act >= 2001) %>% 
  mutate(inicio_act = factor(case_when(inicio_act == "2001" ~ "1",
                                inicio_act == "2002" ~ "2",
                                inicio_act == "2003" ~ "3",  
                                inicio_act == "2004" ~ "4",  
                                inicio_act == "2005" ~ "5",  
                                inicio_act == "2006" ~ "6",  
                                inicio_act == "2007" ~ "7",  
                                inicio_act == "2008" ~ "8",  
                                inicio_act == "2009" ~ "9",  
                                inicio_act == "2010" ~ "10",  
                                inicio_act == "2011" ~ "11",  
                                inicio_act == "2012" ~ "12",  
                                inicio_act == "2013" ~ "13",  
                                inicio_act == "2014" ~ "14",  
                                inicio_act == "2015" ~ "15"),
                             levels =c("1","2","3","4","5",
                                       "6","7","8","9","10",
                                       "11","12","13","14","15"))) %>% 
  ggplot(aes(inicio_act, total, fill = total)) +
  geom_col() + 
  geom_text(aes(inicio_act, total +0.3, label = total), size =4) +
  facet_geo(~nombre_de_entidad ,
            grid = mx_state_grid2,
            label="name")+
  scale_fill_gradient(low="black", high = "pink")+
  theme_light()+
  labs(x = "Año de creación de refugio",
       y = "Total de refugios fundados",
       title = "Refugios por entidad federativa y año de fundación",
       caption = "Fuente: Elaboración propia con datos del Censo de Alojamientos de Asistencia\n Social con corte al 2016 y actualizados el 2021. (Junio,2022)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size=5, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6, face = "bold"),
        strip.text = element_text(size = 10, face="bold", color="#ffc0cb"),
        strip.background = element_rect( fill = "black"),
        text=element_text("Century Gothic"),
        plot.title = element_text(hjust = 0.5))



### faceteamos con posición del Edo, pero con mx_state_grid3 ----------------------
refugios %>% 
  mutate(capacidad = as.numeric(capacidad),
         entidad = as.numeric(entidad)) %>% 
  arrange(entidad) %>% 
  left_join(clasi_ent,by = c("entidad" = "clave_entidad")) %>% 
  select(nombre_de_entidad, everything()) %>% 
  group_by(nombre_de_entidad, inicio_act) %>% 
  summarise(total = n()) %>% 
  filter(inicio_act >= 2001) %>% 
  mutate(inicio_act = factor(case_when(inicio_act == "2001" ~ "1",
                                inicio_act == "2002" ~ "2",
                                inicio_act == "2003" ~ "3",  
                                inicio_act == "2004" ~ "4",  
                                inicio_act == "2005" ~ "5",  
                                inicio_act == "2006" ~ "6",  
                                inicio_act == "2007" ~ "7",  
                                inicio_act == "2008" ~ "8",  
                                inicio_act == "2009" ~ "9",  
                                inicio_act == "2010" ~ "10",  
                                inicio_act == "2011" ~ "11",  
                                inicio_act == "2012" ~ "12",  
                                inicio_act == "2013" ~ "13",  
                                inicio_act == "2014" ~ "14",  
                                inicio_act == "2015" ~ "15"),
                             levels =c("1","2","3","4","5",
                                       "6","7","8","9","10",
                                       "11","12","13","14","15"))) %>% 
  ggplot(aes(inicio_act, total, fill = total)) +
  geom_col() + 
  geom_text(aes(inicio_act, total +0.3, label = total), size =4) +
  facet_geo(~nombre_de_entidad ,
            grid = mx_state_grid3,
            label="name")+
  scale_fill_gradient(low="black", high = "pink")+
  theme_light()+
  labs(x = "Año de creación de refugio",
       y = "Total de refugios fundados",
       title = "Refugios por entidad federativa y año de fundación",
       caption = "Fuente: Elaboración propia con datos del Censo de Alojamientos de Asistencia\n Social con corte al 2016 y actualizados el 2021. (Junio,2022)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size=5, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6, face = "bold"),
        strip.text = element_text(size = 10, face="bold", color="#ffc0cb"),
        strip.background = element_rect( fill = "black"),
        text=element_text("Century Gothic"),
        plot.title = element_text(hjust = 0.5))


refugios %>% 
  mutate(capacidad = as.numeric(capacidad),
         entidad = as.numeric(entidad)) %>% 
  arrange(entidad) %>% 
  left_join(clasi_ent,by = c("entidad" = "clave_entidad")) %>% 
  select(nombre_de_entidad, everything()) %>% 
  group_by(nombre_de_entidad, inicio_act) %>% 
  summarise(total = n()) %>% 
  filter(inicio_act >= 2001) %>% 
  mutate(inicio_act = case_when(inicio_act == "2001" ~ "1",
                                inicio_act == "2002" ~ "2",
                                inicio_act == "2003" ~ "3",  
                                inicio_act == "2004" ~ "4",  
                                inicio_act == "2005" ~ "5",  
                                inicio_act == "2006" ~ "6",  
                                inicio_act == "2007" ~ "7",  
                                inicio_act == "2008" ~ "8",  
                                inicio_act == "2009" ~ "9",  
                                inicio_act == "2010" ~ "10",  
                                inicio_act == "2011" ~ "11",  
                                inicio_act == "2012" ~ "12",  
                                inicio_act == "2013" ~ "13",  
                                inicio_act == "2014" ~ "14",  
                                inicio_act == "2015" ~ "15"))%>% 
  mutate(inicio_act = factor(inicio_act,
                             levels = c("1","2","3","4","5",
                                        "6","7","8","9","10",
                                        "11","12","13","14","15"))) %>% 
  ggplot(aes(inicio_act, total, fill = total)) +
  geom_col() + 
  # geom_text(aes(inicio_act, total +0.3, label = total), size =4) +
  facet_geo(~nombre_de_entidad ,
            grid = mx_state_grid2,
            label="name")+
  scale_fill_gradient(low="black", high = "pink")+
  theme_light()+
  labs(x = "Año de creación de refugio",
      y = "Total de refugios fundados",
      title = "Refugios por entidad federativa y año de fundación",
      caption = "Fuente: Elaboración propia con datos del Censo de Alojamientos de Asistencia\n Social con corte al 2016 y actualizados el 2021. (Junio,2022)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size=7, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6, face = "bold"),
        strip.text = element_text(size = 10, face="bold", color="#ffc0cb"),
        strip.background = element_rect( fill = "black"),
        text=element_text("Century Gothic"),
        plot.title = element_text(hjust = 0.5))




refugios %>% 
  mutate(capacidad = as.numeric(capacidad),
         entidad = as.numeric(entidad)) %>% 
  arrange(entidad) %>% 
  left_join(clasi_ent,by = c("entidad" = "clave_entidad")) %>% 
  select(nombre_de_entidad, everything()) %>% 
  group_by(nombre_de_entidad) %>% 
  summarise(total = n()) %>% 
  mutate(nombre_de_entidad = case_when(nombre_de_entidad == "Coahuila de Zaragoza"~ "Coahuila",
                                       nombre_de_entidad == "Michoacán de Ocampo"~"Michoacán",
                                       nombre_de_entidad == "Veracruz de Ignacio de la Llave"~"Veracruz",
                                       TRUE ~ nombre_de_entidad)) %>% 
  ggplot(aes(fct_reorder(nombre_de_entidad, total), total, fill = total)) +
  geom_col() +
  geom_text(aes(fct_reorder(nombre_de_entidad, total), 
                total + 0.8,
                label = total))+
  scale_fill_gradient(low = "black", high = "pink")+
  theme_bw() +
  labs(x = "Entidad",
       y = "Total de refugios",
       title = "Figura 1: Refugios totales por entidad federativa",
       caption = "Fuente: Elaboración propia con datos del Censo de Alojamientos de Asistencia\n Social con corte al 2016 y actualizados el 2021. (Junio,2022)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1.1, size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")




refugios %>% 
  mutate(capacidad = as.numeric(capacidad),
         entidad = as.numeric(entidad)) %>% 
  arrange(entidad) %>% 
  left_join(clasi_ent,by = c("entidad" = "clave_entidad")) %>% 
  select(nombre_de_entidad, everything()) %>% 
  filter(entidad == 9) %>% 
  group_by(inicio_act) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(inicio_act, total, fill = total)) +
  geom_col()+
  geom_text(aes(inicio_act, total+0.1,
                label = total)) +
  scale_fill_gradient(low =  "black", high = "pink") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1.1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Año de creación del refugio",
       y = "Total de refugios fundados",
       title = "Figura 2: Distribucion de creación de refugios por año 1907-2015",
       subtitle = "Refugios creados a nivel nacional",
       caption = "Fuente: Elaboración propia con datos del Censo de Alojamientos de Asistencia\n Social con corte al 2016 y actualizados el 2021. (Junio,2022)")


refugios %>% 
  mutate(capacidad = as.numeric(capacidad),
         entidad = as.numeric(entidad)) %>% 
  arrange(entidad) %>% 
  left_join(clasi_ent,by = c("entidad" = "clave_entidad")) %>% 
  select(nombre_de_entidad, everything()) %>% 
  group_by(inicio_act, nombre_de_entidad) %>% 
  filter(inicio_act >= 2013) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(inicio_act, total, fill = total)) +
  geom_col()+
  geom_text(aes(inicio_act, total+0.1,
                label = total)) +
  facet_wrap(~nombre_de_entidad) +
  scale_fill_gradient(low = "black", high = "pink") +
  theme_bw() +
  labs(x = "Año de creación de refugio",
       y = "Total de refugios fundados",
       caption = "Fuente: Elaboración propia con datos del Censo de Alojamientos de Asistencia\n Social con corte al 2016 y actualizados el 2021. (Junio,2022)",
       title = "Figura 3: Refugios construidos desde 2013 a 2015") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
  

refugios %>% 
  mutate(capacidad = as.numeric(capacidad),
         entidad = as.numeric(entidad)) %>% 
  arrange(entidad) %>% 
  left_join(clasi_ent,by = c("entidad" = "clave_entidad")) %>% 
  select(nombre_de_entidad, everything()) %>% 
  filter(entidad == 13) 

refugios %>% 
  mutate(capacidad = as.numeric(capacidad),
         entidad = as.numeric(entidad)) %>% 
  arrange(entidad) %>% 
  left_join(clasi_ent,by = c("entidad" = "clave_entidad")) %>% 
  select(nombre_de_entidad, everything()) %>% 
  filter(entidad == 1) %>% 
  select(inicio_act)


unique(refugios$inicio_act)
range(refugios$inicio_act)
refugios %>% 
  filter(inicio_act == 1907)

unique(carpetas$alcaldia)
class(refugios$inicio_act)
summary(refugios)

refugios %>% 
  mutate_at()



carpetas %>%  #### Separar todos los delitos de Violencia familiar, que es un outlier
  group_by(tipo_de_delito, alcaldia) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(tipo_de_delito, total)) +
  geom_col() +
  facet_wrap(~tipo_de_delito, scales = "free") +
  theme(legend.position = "none")




  
  
# mapeamos ----------------------------------------------------------------
# CDMX 2019-2020



refugios <- read_excel("01_datos/principales_resultados_por_alojamiento_de_asistencia_social_xls/Alojamientos de asistencia social.xls")
(refugios <- refugios[-1,])


refugios <- refugios %>% 
    filter(entidad == ("09"),
           clasealoja  %in%  c(5,11)) # %>% 
  # mutate(inicio_act = as.numeric(inicio_act)
  
  
  
refugios <- refugios %>% 
  mutate(lon = round(as.numeric(substr(longitud, 1,3)), 5) -
             as.numeric(substr(longitud, 5,6))/60 -
             as.numeric(substr(longitud, 9,12))/3600,
          lat = round(as.numeric(substr(latitud, 4,5))/60 +
                         as.numeric(substr(latitud, 8,11))/3600 +
                         as.numeric(substr(latitud, 1,2)), 5)) 
  
write.csv(refugios, "01_datos/principales_resultados_por_alojamiento_de_asistencia_social_xls/refugios_limpios.csv")

