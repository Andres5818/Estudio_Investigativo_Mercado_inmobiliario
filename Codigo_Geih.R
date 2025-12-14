library(tidyverse)
library(ggplot2)
library(Hmisc)
library(gridExtra) #Para añadir dos gráficas en una sola salida.
library(knitr) # para la creación de tablas
library(psych) # para el uso de describe
library(lmtest)
library(MASS)
library(readxl)
library(kableExtra)

# Ingresos promedio -------------------------------------------------------

## Base de ocupados
ocupados <- read_csv2("Ocupados.csv")
ocupados <- filter(ocupados, DPTO == "08")

## Base de Hogares
hogar <- read_csv2("Datos del hogar y la vivienda.csv")
hogar <- filter(hogar, DPTO == "08")

## Base de otros ingresos
otros_ingresos <- read_csv("Otros ingresos e impuestos.csv")
otros_ingresos <- filter(otros_ingresos, DPTO == "8")


ocupados <- ocupados %>% select(DIRECTORIO, HOGAR, SECUENCIA_P,FEX_C18,
                                INGLABO)

ingreso_hogar <- ocupados %>%
  group_by(DIRECTORIO, SECUENCIA_P, HOGAR) %>% 
  summarise(ingreso_total = sum(INGLABO, na.rm = T)) %>% 
  transmute(DIRECTORIO, SECUENCIA_P, HOGAR, ingreso_total)

otros_ingresos_merge <- otros_ingresos %>% 
  left_join(ingreso_hogar, by = c("DIRECTORIO", "SECUENCIA_P", "HOGAR"))


otros_ingresos_merge$ingresos_lab_y_nlab <- rowSums(
  otros_ingresos_merge[, c("ingreso_total", "P7500S1A1", "P7500S2A1", "P7500S3A1",
                           "P7510S1A1", "P7510S2A1", "P7510S3A1", "P750S1A1",
                           "P7510S6A1", "P750S2A1", "P750S3A1", "P7510S5A1", "P7510S7A1")],
  na.rm = TRUE
)

otros_ingresos_merge <- otros_ingresos_merge %>% select(DIRECTORIO,
                          SECUENCIA_P, HOGAR, ingresos_lab_y_nlab)



hogar_merge <- hogar %>%
  left_join(otros_ingresos_merge, by = c("DIRECTORIO","SECUENCIA_P","HOGAR"))



resumen <- hogar_merge %>%
  filter(!is.na(P4030S1A1)) %>%
  group_by(P4030S1A1) %>%
  summarise(
    promedio_ponderado = sum(ingresos_lab_y_nlab * FEX_C18, na.rm = TRUE) / sum(FEX_C18, na.rm = TRUE),
    hogares_ponderados = sum(FEX_C18, na.rm = TRUE),
    n_muestra = n(),
    .groups = "drop"
  )

# 12) Mediana ponderada (función simple)
weighted_median <- function(x, w) {
  idx <- order(x)
  x <- x[idx]; w <- w[idx]
  cumw <- cumsum(w)
  cutoff <- sum(w)/2
  x[which(cumw >= cutoff)[1]]
}

medianas <- hogar_merge %>%
  filter(!is.na(P4030S1A1)) %>%
  group_by(P4030S1A1) %>%
  summarise(mediana_ponderada = weighted_median(ingresos_lab_y_nlab, FEX_C18), .groups = "drop")

final <- left_join(resumen, medianas, by = "P4030S1A1")
print(final)


quantiles <- hogar_merge %>%
  filter(!is.na(P4030S1A1)) %>%
  group_by(P4030S1A1) %>%
  summarise(
    q25 = wtd.quantile(ingresos_lab_y_nlab, weights = FEX_C18, probs = 0.25, na.rm = TRUE),
    mediana = wtd.quantile(ingresos_lab_y_nlab, weights = FEX_C18, probs = 0.50, na.rm = TRUE),
    q75 = wtd.quantile(ingresos_lab_y_nlab, weights = FEX_C18, probs = 0.75, na.rm = TRUE),
    .groups = "drop"
  )

quantiles

# Nivel de estudios ----------------------------------------------------------------

personas <- read_csv2("Características generales, seguridad social en salud y educación.csv")
personas <- personas %>% filter(DPTO == "08")
personas$P3042 <- factor(personas$P3042,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 99),
                         labels = c("Ninguno", 
                                    "Preescolar", 
                                    "Básica primaria (1o - 5o)",
                                    "Básica secundaria (6o - 9o)",
                                    "Media académica (Bachillerato clásico)",
                                    "Media técnica (Bachillerato técnico)",
                                    "Normalista",
                                    "Técnica profesional",
                                    "Tecnológica",
                                    "Universitaria",
                                    "Especialización",
                                    "Maestría",
                                    "Doctorado",
                                    "No sabe, no informa"))

personas_modificado <- personas %>%
  select(P3042, FEX_C18) %>%
  drop_na() %>%
  group_by(P3042) %>%
  summarise(personas_pond = sum(FEX_C18), .groups = "drop") %>%
  mutate(pct = personas_pond / sum(personas_pond))  # guardamos pct en fracción (0-1)

ggplot(personas_modificado,
       aes(x = reorder(P3042, pct),
           y = pct, fill = P3042)) +
  geom_col() +
  coord_flip() +
  # etiquetas encima de la barra
  geom_text(aes(label = paste0(round(pct*100,1), "%")),
            hjust = -0.1, size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + # eje en porcentaje normal
  labs(x = "Nivel educativo",
       y = "Porcentaje de personas",
       title = "Nivel educativo de la población en el Atlántico") +
  theme_minimal() +
  theme(legend.position = "none")


# Tipo de cargos laborales ----------------------------------------------------------------

df <- read_csv2("Ocupados.csv")
df <- df %>% filter(DPTO == "08")
df$P6430 <- factor(df$P6430,
                   levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                   labels = c("Obrero o empleado de empresa particular", 
                              "Obrero o empleado del gobierno", 
                              "Empleado doméstico",
                              "Trabajador por cuenta propia",
                              "Patrón o empleador",
                              "Trabajador familiar sin remuneración",
                              "Trabajador sin remuneración en empresas o negocios de otros hogares",
                              "Jornalero o peón",
                              "Otro"))

Empleados <- df %>% select(P6430, FEX_C18) %>% 
  transmute(P6430, FEX_C18)

Empleados_modificado <- Empleados %>%
  select(P6430, FEX_C18) %>%
  drop_na() %>%
  group_by(P6430) %>%
  summarise(empleos_pond = sum(FEX_C18), .groups = "drop") %>%
  mutate(pct = empleos_pond / sum(empleos_pond))  # guardamos pct en fracción (0-1)

ggplot(Empleados_modificado,
       aes(x = reorder(P6430, pct),
           y = pct, fill = P6430)) +
  geom_col() +
  coord_flip() +
  # etiquetas encima de la barra
  geom_text(aes(label = paste0(round(pct*100,1), "%")),
            hjust = -0.1, size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + # eje en porcentaje normal
  labs(x = " ",
       y = "Porcentaje de personas",
       title = "Cargos de la población en el Atlántico") +
  theme_minimal() +
  theme(legend.position = "none")
  


# Tipo de Hogar -----------------------------------------------------------

df_tipo_de_hogar <- read_csv2("Características generales, seguridad social en salud y educación.csv")

df_tipo_de_hogar <- df_tipo_de_hogar %>% select(
  DIRECTORIO, P6050, HOGAR, SECUENCIA_P
)

df_tipo_de_hogar$P6050 <- recode(df_tipo_de_hogar$P6050, `1` = "Jefe (a) de hogar",
                                  `2` = "Pareja, esposo(a), cónyuge, compañero(a)", `3` = "Hijo(a), hijastro(a)",
                                  `4` = "Nieto(a)",
                                  `5` = "Otro pariente",
                                  `6` = "Empleado(a) del servicio doméstico y sus parientes",`7` = "Pensionista",
                                  `8` = "Trabajador",`9` = "Otro no pariente")

df_tipo_de_hogar <- df_tipo_de_hogar %>% rename("Parentesco"= P6050)

df_tipo_de_hogar <- df_tipo_de_hogar %>% group_by(DIRECTORIO, HOGAR, SECUENCIA_P) %>% 
  summarise(Parentesco = paste(Parentesco, collapse = ", "), .groups = "drop") %>% 
  transmute(DIRECTORIO, HOGAR, SECUENCIA_P, Parentesco)


df_tipo_de_hogar <- df_tipo_de_hogar %>%
  mutate(
    Tipo_de_hogar = case_when(
      # 1 sola persona (sin coma)
      !str_detect(Parentesco, ",") ~ "Unipersonal",
      # hay Jefe y Pareja pero sin hijos
      str_detect(Parentesco, "Jefe.*Pareja") & !str_detect(Parentesco, "Hijo") ~ "Pareja sin hijos",
      # hay Jefe y Hijo(s) pero sin Pareja
      str_detect(Parentesco, "Jefe.*Hijo") & !str_detect(Parentesco, "Pareja") ~ "Monoparental con hijos",
      # hay Jefe, Pareja e Hijos
      str_detect(Parentesco, "Jefe.*Pareja") & str_detect(Parentesco, "Hijo") ~ "Nuclear",
      # si hay parientes adicionales
      str_detect(Parentesco, "Otro pariente|Nieto") ~ "Extenso",
      # si hay no parientes
      str_detect(Parentesco, "Otro no pariente|Trabajador|Empleado") ~ "Compuesto",
      TRUE ~ "Otros"
    )
  )
# Resumir cantidad de hogares por tipo
hogares_resumen <- df_tipo_de_hogar %>%
  group_by(Tipo_de_hogar) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pct = n / sum(n))  # porcentaje en fracción 0-1

# Gráfica de barras horizontales con porcentajes
ggplot(hogares_resumen,
       aes(x = reorder(Tipo_de_hogar, pct),
           y = pct, fill = Tipo_de_hogar)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(round(pct*100,1), "%")),
            hjust = -0.1, size = 3) +
  labs(x = "Tipo de hogar",
       y = "Porcentaje de hogares",
       title = "Distribución porcentual de los tipos de hogar en el Atlántico") +
  theme_minimal() +
  theme(legend.position = "none")

# Regresión Hogares -------------------------------------------------------


reg_hogares <- read_excel("Datos recolectados.xlsx")
modelo <- lm(Hogares ~ 
               Tasa_fecundidad + IPM , data = reg_hogares)
summary(modelo)

nuevos <- data.frame( IPM = 11.5, Tasa_fecundidad = 26237)
predict(modelo, nuevos, interval = "confidence")

# Hogares en arriendo --------------------------------------------------------
Hogares_arriendo <- read_csv2("Datos del hogar y la vivienda.csv")
Hogares_arriendo <- filter(Hogares_arriendo, DPTO == "08")
Hogares_arriendo <- Hogares_arriendo %>% select(DIRECTORIO, SECUENCIA_P, P5090, FEX_C18)

Hogares_arriendo$P5090 <- recode(Hogares_arriendo$P5090, `1` = "Propia, totalmente pagada",
                                 `2` = "Propia, la están pagando", `3` = "En arriendo o subarriendo",
                                 `4` = "En usufructo*",
                                 `5` = "Posesión sin titulo",
                                 `6` = "Otra")
Hogares_arriendo <- Hogares_arriendo %>% rename("vivienda"= P5090)
Hogares_arriendo <- Hogares_arriendo %>% group_by(vivienda) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pct = n / sum(n))  # porcentaje en fracción 0-1

# Gráfica de barras horizontales con porcentajes
ggplot(Hogares_arriendo,
       aes(x = reorder(vivienda, pct),
           y = pct, fill = vivienda)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(round(pct*100,1), "%")),
            hjust = -0.1, size = 3) +
  labs(x = "Tipo de tenencia de vivienda",
       y = "Porcentaje",
       title = "Distribución porcentual de tenencia de vivienda") +
  theme_minimal() +
  theme(legend.position = "none")

#Edades de ocupados

edades_ocupados <- read_csv2("Características generales, seguridad social en salud y educación.csv")

edades_ocupados <- edades_ocupados %>% select(DIRECTORIO, SECUENCIA_P, ORDEN, HOGAR, AREA, P6040, P6050) 
edades_ocupados <- filter(edades_ocupados, AREA == "08", P6050 == 1)

# Numero de carros demandados en el departamento para visibilizar el parqueadero
carros <- read_excel("RUNT.xlsx")
carros <- carros %>% filter(NOMBRE_DEPARTAMENTO == "ATLANTICO",
                            NOMBRE_DE_LA_CLASE == "AUTOMOVIL",
                            ESTADO_DEL_VEHICULO == "ACTIVO") %>% group_by(NOMBRE_MUNICIPIO) %>% 
  summarise(cantidad_automoviles = sum(CANTIDAD, na.rm = T)) %>% 
  transmute(NOMBRE_MUNICIPIO, cantidad_automoviles) %>% 
  arrange(desc(cantidad_automoviles))

kbl(carros) %>% kable_styling()
 