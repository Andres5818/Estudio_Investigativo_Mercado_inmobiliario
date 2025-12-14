#libreria
#install.packages("factoextra")
#install.packages("hunspell")
#install.packages("fuzzyjoin")
#install.packages("FactoMineR")
library(hunspell)
library(readxl)
library(tidyverse)
library(readxl)
library(ggplot2)
library(stringdist)
library(fuzzyjoin)
library(ggrepel)
library(cluster)
library(factoextra)
library(FactoMineR)
library(sf)


#Cargue de la base.
setwd("G:\\Mi unidad\\4. CAMACOL - ANDRES BARRIOS\\Codigos y automatizacion") #Directorio
df <- read_excel("Feria VIS 2025_Consolidado.xlsx") #Lectura de base

#Limpieza de datos
apply(df, 2, function(x) sum(is.na(x))) # Detección de NA's
df <- df[,c(-1:-4,-6,-12:-18)]
df$Edad <- as.numeric(df$Edad)

# Crear rangos redondeados
df$RangoEdad <- cut(
  df$Edad,
  breaks = 10,
  include.lowest = TRUE,
  labels = paste(
    round(head(seq(min(df$Edad), max(df$Edad), length.out = 11), -1)),
    "-",
    round(tail(seq(min(df$Edad), max(df$Edad), length.out = 11), -1))
  )
)

# Gráfico de rango de edades
ggplot(df, aes(x = RangoEdad, fill = RangoEdad)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3, size = 3) +
  labs(x = "Rango de edades", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


#AE: Situación laboral
df$`Situación Laboral` <- recode(df$`Situación Laboral`,
                                 "PENSIONADO" = "PENSIONADO",
                                 "INFORMAL (SIN CONTRATO LABORAL)" = "INFORMAL",
                                 "INDEPENDIENTE (REALIZAS CUALQUIER LABOR QUE TE GENERE INGRESOS)" = "INDEPENDIENTE",
                                 "FORMAL (CON CONTRATO LABORAL)" = "FORMAL",
                                 "EMPRENDEDOR" = "EMPRENDEDOR"
)


ggplot(df, aes(x = reorder(`Situación Laboral`, -table(`Situación Laboral`)[`Situación Laboral`]),
               fill = `Situación Laboral`)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(
    x = "Situación Laboral",
    y = "Inscritos",
    title = "Situación laboral de las personas inscritas"
  ) +
  ylim(0, 3000) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")

# REGEX -------tibble()# REGEX -------------------------------------------------------------------
list_pract <- c("ESTILISTA", "MODISTA")
str_detect(list_pract, "ESTILISTA|MODISTA")



df_regex <- select(df, Profesión)

df_regex <- df_regex %>%
  mutate(
    Profesión_limpia = Profesión %>%
      str_trim() %>%
      str_to_upper() %>%  # convertir a mayúsculas primero
      str_replace_all("[ÁÀÄÂ]", "A") %>%
      str_replace_all("[ÉÈËÊ]", "E") %>%
      str_replace_all("[ÍÌÏÎ]", "I") %>%
      str_replace_all("[ÓÒÖÔ]", "O") %>%
      str_replace_all("[ÚÙÜÛ]", "U") %>%
      str_replace_all("[^A-Z ]", "")  # elimina símbolos raros, ajustado a mayúsculas
  )

df_regex <- df_regex %>% 
  mutate(new = ifelse(str_detect(Profesión, "^ABOGAD|DER{1,2}ECHO"), "ABOGADO",
         ifelse(str_detect(Profesión, "A[SC]+ESO.|VENTA.|VENDEDOR.?|COMERCIAL|comerciante|MERCADO|COMERCI.|NEGOCIADOR"), "ASESOR COMERCIAL", 
         ifelse(str_detect(Profesión,"^AD.|FINANZAS|RECURSOS HUMANOS|^ACION|CONSULTOR NOMINA"), "ADMINISTRADOR DE EMPRESAS",
         ifelse(str_detect(Profesión, "CUSTOMER SERVICE|\\bBIL{0,2}ING.E|\\b[CAO]{1,3}L{1,2}[- ]?CENTER\\b"),"CALL CENTER",
         ifelse(str_detect(Profesión, "A.?X|^AU\\s. |DIGITADOR"), "AUXILIAR", 
         ifelse(str_detect(Profesión,"AGENTE|ATENCI.N AL CLIENTE|ATENCION"), "AGENTE", 
         ifelse(str_detect(Profesión,"AN?A?LISTA|SECRETAR.A|AN.LISISTA"), "ANALISTA", 
         ifelse(str_detect(Profesión,"CONTADOR.|CONTADODORA|CONTABILIDAD|C.NTADURIA|CONTABLE"), "CONTADOR", 
         ifelse(str_detect(Profesión,"ESTI?UDI?ANTY?E|UNIVERSITARI.|PROFE[SC]IONAL"), "ESTUDIANTE", 
         ifelse(str_detect(Profesión,"ENFERMER.|CUIDADOR."), "ENFERMERA", 
         ifelse(str_detect(Profesión,"ARQUITECT.|ARTITECTO|ARQUICTETO"), "ARQUITECTO", 
         ifelse(str_detect(Profesión,"ASISTENTE.|OFICINISTA|^ASIS."), "ASISTENTE", 
         ifelse(str_detect(Profesión,"BACHILLE.|COLEGIO|BALLICHER|BACHICHER|SECUNDARIA"), "BACHILLER",
         ifelse(str_detect(Profesión,"PEN[SC]IONAD."), "PENSIONADO",
         ifelse(str_detect(Profesión,"DESARROLLADOR|ING.|SISTEMA|AEROMECANICA|BIOMEDICO"), "INGENIERO",
         ifelse(str_detect(Profesión,"ESTILISTA|CONFECC?IONE?S?|MODISTA|COSTU.ERA|COS?METOLOG.|CONFECCIONES|MANICURISTA"), "BELLEZA",
         ifelse(str_detect(Profesión,"^N[AO]$|NO TIENE"), "NO TIENE",
         ifelse(str_detect(Profesión,"EMPLEAD.|PRESTACI.N DE SERVICIOS"), "EMPLEADO",
         ifelse(str_detect(Profesión,"OPERARI.|DOMICILI.|CONTROL|ALMA{1,2}CEN|CON?DUCTOR|OPERADOR|CAJER.|LOG.STIC[OA]|MANTENIMIENTO|DESPACHADOR|MEC.NICO|CORTADOR|SOLDADOR|MONTACARGA|TRAN?SPORTADOR.?"), "OPERARIO",
         ifelse(str_detect(Profesión,"TEC.|TECNIC.|TECNOLOG.|ZAPATERA|CARPINTERIA|CERRAJERO|MANIPULADORA DE ALIMENTOS|ISLERO|LITOGRAFIA"), "TECNICO-TECNOLOGO",
         ifelse(str_detect(Profesión,"VIGU?ILA?NTE|GUARDI?A DE SEGURIDAD|CURSO DE VIGILANCIA|ESCOLTA|MILITAR|POLICIA|SEGURIDAD|SUPERVISOR"), "SEGURIDAD",
         ifelse(str_detect(Profesión,"MERCADERISTA|COMERCIANTE|COMERCIO|COOMECIANTE"), "COMERCIANTE",
         ifelse(str_detect(Profesión,"DISEÑA?D?OR?.?|COMUNITY"), "DISEÑADOR GRAFICO",
         ifelse(str_detect(Profesión,"COMUNICADOR.|PERIODISTA|COMUNICACI.N"), "COMUNICADOR SOCIAL",
         ifelse(str_detect(Profesión,"LIC.|LICENCIATURA|DONCENCIA|CONFERENCISTA|CONSULTOR.|DOCENC?I?A?TE|ENTRENADOR|EDUCADOR.?|LICENCIAD.|MAESTR.|PROFESOR.?"), "DOCENCIA",
         ifelse(str_detect(Profesión,"OFICIOS? [BV]ARIOS|SERVICIOS GENERALES|ASEADORA?|ARCHIVERA|ARCHIVO|CONSERGE"), "OFICIOS VARIOS",
         ifelse(str_detect(Profesión,"COCI{1,2}NER.|CHET?F|MESERO|COCINA"), "COCINERO",
         ifelse(str_detect(Profesión,"AMA.|DOMESTICA|CASA DE FAMILIA|MADRE|Ama De Casa"), "AMA DE CASA",
         ifelse(str_detect(Profesión,"ELECTRI[CS]ISTA"), "ELECTRICISTA",
         ifelse(str_detect(Profesión,"PSIC.LOGI?A|PISOCOLOGA"), "PSICOLOGA",
         ifelse(str_detect(Profesión,"ALBAÑIL|CONSTRUC.?I?.N?R?|ACABADOS|CONTRATISTA|CONTRUCTOR"), "CONSTRUCCION",
         ifelse(str_detect(Profesión,"INDEPENDIENTE|EMPREN?DEDOR|EMPRESA.?"), "EMPRENDEDOR",
         ifelse(str_detect(Profesión,"DIRECTOR.?|JEF[EA]|GERENTE"), "DIRECTIVOS",
         ifelse(str_detect(Profesión,"INTERNACIONAL"), "NEGOCIOS INTERNACIONALES",
         ifelse(str_detect(Profesión,"FUNCIONARIO P.BLICO|SERVIDOR P.BLICO"), "FUNCIONARIO PUBLICO",
         ifelse(str_detect(Profesión,"SALUD OCUPACIONAL|SST"), "SALUD OCUPACIONAL",
         ifelse(str_detect(Profesión,"F[AO]R?M[AC]{1,2}E?U?T[IA]C[AO]|QUIMIC[A,O]"), "FARMACEUTICA",
         ifelse(str_detect(Profesión,"ODONT.LOG.|DENTOLOGA"), "ODONTOLOGO",
         ifelse(str_detect(Profesión,"COO?RDINADOR.?|LIDER"), "COORDINADORES",
         ifelse(str_detect(Profesión,"ESPECIALISTA"), "ESPECIALISTAS",
         ifelse(str_detect(Profesión,"AUDITOR"), "AUDITOR",
         ifelse(str_detect(Profesión,"FOTOGRAFO|ARTISTA|DAN.A|ARTESANA|ARTESANA DEL TOTUMO|BARBERO|BARTENDER|BOXEO|AUDIO VISUAL"), "ARTES Y ENTRETENIMIENTO",
         ifelse(str_detect(Profesión,"BACTERIOLOG.|INSTRUMENTADORA QUIRURGICA|NUTRICIONISTA|FISIOTERAPEUTA|MEDICO VETERINARIO|CARDIOLOGO|MEDICO"), "CIENCIAS DE LA SALUD",
         ifelse(str_detect(Profesión, "GESTIO?N?."),"GESTOR", Profesión)))))))))))))))))))))))))))))))))))))))))))))


df_regex$new <- as.factor(df_regex$new)
# Instalar si no lo tienes
# install.packages("ggrepel")



profesiones <- read_csv2("Profesiones_Feria.csv", locale = locale(encoding = "UTF-8"))

# Seleccionar los puntos que tendrán etiqueta (ej: top 10)
profesiones <- profesiones %>%
  mutate(label = ifelse(row_number() <= 20, Categoria, NA))

# Gráfico con etiquetas que no se solapan
ggplot(profesiones, aes(x = Frecuencia, y = Categoria)) +
  geom_point(size = 3, color = "#0073C2FF") +
  geom_text_repel(aes(label = label), 
                  hjust = -0.1, 
                  size = 3.5,
                  box.padding = 0.4, 
                  point.padding = 0.5,
                  max.overlaps = Inf) +
  labs(
    title = "Frecuencia de Profesiones",
    x = "Frecuencia",
    y = NULL  # Quita la etiqueta del eje Y
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),  # Oculta las categorías del eje Y
    axis.ticks.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  ) +
  xlim(0, max(profesiones$Frecuencia) * 1.1)



# Barrios que son los mas atractivos --------------------------------------

# Seleccionar columnas relevantes y renombrar
df_barrios <- df %>%
  select(6, 12) %>% 
  rename(
    Barrios = "Mencione 3 barrios donde le gustaría comprar su vivienda:"
  )

# Separar barrios en filas, conservar estrato
df_barrios_2 <- df_barrios %>%
  separate_rows(Barrios, sep = ",") %>%   # Divide por comas
  mutate(
    Barrios = str_trim(Barrios),          # Quita espacios
    Barrios = str_to_title(Barrios)       # Pone formato tipo título
  ) %>%
  filter(!is.na(Barrios), Barrios != "")  # Elimina vacíos o NA

df_barrios_2$Barrios <- toupper(df_barrios_2$Barrios)

barrios_right <- read_csv("Barrios_correctos.csv")

# Hacer el emparejamiento sin perder el estrato
df_resultado <- df_barrios_2 %>%
  rowwise() %>%
  mutate(
    Barrio_corregido = barrios_right$NOMBRE[
      which.min(stringdist(Barrios, barrios_right$NOMBRE, method = "jw"))
    ]
  ) %>%
  ungroup()

write.csv2(df_resultado, "Barrios_corregidos.csv")

Barrios_analisis <- read_csv2("V2.Barrios_corregidos.csv")

barrios_freq <- Barrios_analisis %>%
  count(Estrato, Barrios, name = "Frecuencia") %>%
  arrange(desc(Frecuencia)) 
#install.packages("wordcloud2")

barrios_porcentaje_1 <- barrios_freq %>% 
  mutate(
    nivel_socioeconomico = ifelse(Estrato>=1 & Estrato <=2, "Segmento bajo",
                                  ifelse(Estrato > 2 & Estrato <=4, "Segmento medio",
                                         ifelse(Estrato > 4, "Segmento alto", Estrato )
                                         )
                                  )
    ) 

barrios_porcentaje_1 <- barrios_porcentaje_1 %>% 
  dplyr::select(nivel_socioeconomico, Barrios, Frecuencia) %>% 
  group_by(nivel_socioeconomico, Barrios) %>% 
  summarise(new_freq = sum(Frecuencia)) %>% transmute(nivel_socioeconomico, Barrios, new_freq)


bajo <- barrios_porcentaje_1 %>% filter(nivel_socioeconomico == "Segmento bajo")

bajo$new_freq <- as.numeric(bajo$new_freq)

bajo <- bajo %>% ungroup()

medio <- barrios_porcentaje_1 %>% filter(nivel_socioeconomico == "Segmento medio")

medio$new_freq <- as.numeric(medio$new_freq)

alto <- barrios_porcentaje_1 %>% filter(nivel_socioeconomico == "Segmento alto")

alto$new_freq <- as.numeric(alto$new_freq)

#write.csv2(bajo, "Barrios_bajo.csv")
#write.csv2(medio, "Barrios_medio.csv")
#write.csv2(alto, "Barrios_alto.csv")


# desglose de preguntas ---------------------------------------------------

# La muestra elegida es de 2399 hogares
# ¿Cuánto tiempo lleva buscando vivienda?

match("¿Cuánto tiempo lleva buscando vivienda?",names(df) )

tiempo <- df %>% dplyr::select(7) %>% drop_na() %>% 
  rename("busqueda"= "¿Cuánto tiempo lleva buscando vivienda?") %>% 
  filter(busqueda != 0) %>% 
  mutate(
    valor = as.numeric(str_extract(busqueda, "\\d+")), # Identificar el numero
    unidad = str_extract(busqueda, "[A-Za-zÁÉÍÓÚáéíóúÑñ]+$") # Identificar el tiempo
  ) %>% filter(valor != 0) # Eliminar los 0

apply(tiempo, 2, function(x) sum(is.na(x))) # Corroborar los N/A's

tiempo$unidad <- toupper(tiempo$unidad) #Dar uniformidad a las palabras
unique(tiempo$unidad) #Identificación de patrones

v2tiempo <- tiempo %>% mutate(
  v2unidad = str_replace(unidad, "(?i)^m+e?s+e*d?s*$", "MESES"),
  v2unidad = str_replace(v2unidad, "(?i)^a?d[ií]a?s*$", "DIAS"),
  v2unidad = str_replace(v2unidad, "^A*S?Q?Ñ.?.S*O?A?$", "AÑOS"),
  v2unidad = str_replace(v2unidad, "^(SEMANAS|VEZ)$", "SEMANAS")
) %>% 
  filter(v2unidad %in% c("AÑOS","MESES", "DIAS", "SEMANAS"))

unique(v2tiempo$v2unidad) #Corroboración de patrones

v3tiempo <- v2tiempo %>% mutate(
  v3valor = ifelse(v2unidad == "AÑOS", valor*360,
                   ifelse(
                     v2unidad == "MESES", valor*30,
                     ifelse(
                       v2unidad == "SEMANAS", valor*7,
                       valor
                       )
                     )
                   ),
  valor_años = round(v3valor/360,2)
)

# Grafica

#quitar datos atipicos
# Calcular límites
Q1 <- quantile(v3tiempo$valor_años, 0.25, na.rm = TRUE)
Q3 <- quantile(v3tiempo$valor_años, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lim_inf <- Q1 - 1.5 * IQR
lim_sup <- Q3 + 1.5 * IQR

v4tiempo <- v3tiempo %>% 
  filter(valor_años >= lim_inf & valor_años <= lim_sup )

ggplot(v4tiempo,
        aes( y = valor_años)
        )+
  geom_boxplot(fill = "lightblue", color = "black")+
  theme_minimal()+
  ylab("Años")+ xlab("")+
  coord_cartesian(ylim = c(0, 6.5)) 

ggplot(v4tiempo, aes(x = valor_años))+
  geom_histogram(bins = 12, color = "black", fill = "lightblue")+
  theme_minimal()+
  ylab(" ")+ xlab("Años")

# ¿Pagaría por una vivienda amigable al medio ambiente?


ambiente <- df %>% dplyr::select(8) %>% 
  rename("ambiente" = "¿Pagaria por una vivienda amigable al medio ambiente?") %>% 
  mutate(v2ambiente = str_replace(ambiente, "(?i)s[iíIÍ]", "SI"),
         v2ambiente = str_replace(v2ambiente,"(?i)N[Oo]", "NO")
  ) %>% drop_na()

porc_ambiente <- ambiente %>%
count(v2ambiente) %>%
  mutate(porc = n / sum(n) * 100)

ggplot(porc_ambiente, aes(x = "", y = porc, fill = v2ambiente)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(porc, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 5) +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_void()

#Si su apartamento en obra gris incluyera un acabado, ¿cuál escogería: pisos, cielo raso o pared con pañete?

acabados <- df %>% dplyr::select(9) %>% drop_na() %>% 
  rename("acabados" = "Si su apartamento en obra gris incluyera un acabado, ¿cuál escogería: pisos, cielo raso o pared con pañete?" ) %>% 
  mutate(
    v2acabados = str_replace(acabados, "(?i)^Pisos", "PISOS"),
    v2acabados = str_replace(v2acabados, "(?i)^Cielo Raso", "CIELO RASO"),
    v2acabados = str_replace(v2acabados, "(?i).*paredes", "PAREDES")
  ) %>% filter(v2acabados != "Sí")

por_acabados <- acabados %>%
  count(v2acabados) %>%
  mutate(porc = n / sum(n) * 100)

ggplot(por_acabados, aes(x = "", y = porc, fill = v2acabados)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(porc, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 5) +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_void()

# Pregunta 10

aspecto <- df %>% dplyr::select(10) %>% 
  rename("aspecto" = "¿Qué aspecto de una vivienda nueva lo motivaría más a comprar: ubicación, distribución del apartamento, seguridad, vías de acceso, metraje o zonas sociales?") %>% 
  drop_na() %>% mutate(
    v2aspecto = str_replace(aspecto, "(?i)DISTRIBUCIÓN DEL? APARTAMENTO",
                            "DISTRIBUCIÓN DEL APARTAMENTO"),
    v2aspecto = str_to_upper(v2aspecto)
  )

por_aspecto <- aspecto %>%
  count(v2aspecto) %>%
  mutate(porc = n / sum(n) * 100) %>% arrange(desc(porc))

ggplot(por_aspecto, aes(x = v2aspecto, y = porc, fill = v2aspecto)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(porc, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_void()

#Pregunta 11
names(df)[11]

descuento <- df %>% dplyr::select(11) %>% 
  rename("descuento" = "Si la constructora le otorgara un bono, ¿cuál preferiría: bono de escrituración, bono de acabados o descuento en el precio del apartamento?") %>% 
  drop_na() %>% mutate(
    v2descuento = str_to_upper(descuento),
    v2descuento = str_replace(v2descuento, "(?i).*ESCRITURACIÓN", "ESCRITURACIÓN"),
    v2descuento = str_replace(v2descuento,"(?i).*ACABADOS", "ACABADOS"),
    v2descuento = str_replace(v2descuento,"(?i).*PRECIO DEL APARTAMENTO", "PRECIO DEL APARTAMENTO")
  ) %>% filter(v2descuento != "UBICACIÓN")
unique(descuento$v2descuento)


porcentaje_descuento <- descuento %>% 
  count(v2descuento) %>% 
  mutate(
    porcentaje = n/sum(n)*100
  ) %>% arrange(desc(porcentaje))

ggplot(porcentaje_descuento, aes(x = v2descuento, y = porcentaje, fill = v2descuento)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_void()

# encuesta de preferencias 2

library("boot")
library("forecats")

preferencias <- read_csv2("Encuesta preferencias.csv")
preferencias <- drop_na(preferencias)

v2preferencias <- preferencias %>%
  mutate(across(where(is.character), as.factor)) %>% 
  dplyr::select(c(2:10, 12:20, 22))

set.seed(123)
n_nuevo <- 2399

v3preferencias <- v2preferencias[sample(1:nrow(v2preferencias),
                                 size = n_nuevo,
                                 replace = TRUE), ]

# Tipo de vivienda
names(v3preferencias)[2]

porcentaje_tipo <- v3preferencias %>%
  count(`¿Qué tipo de vivienda preferiría?`) %>%
  mutate(porc = n / sum(n))

ggplot(porcentaje_tipo,
       aes(x = `¿Qué tipo de vivienda preferiría?`,
           y = porc,
           fill = `¿Qué tipo de vivienda preferiría?`)) +
  geom_col(color = "black") +
  geom_text(aes(label = scales::percent(porc)),
            vjust = -0.5) +
  scale_y_continuous(labels = scales::percent)+
  xlab("")+ylab("")+labs(fill = NULL)+
  theme_minimal()


porcentaje_inversion <- v3preferencias %>%
  count(`¿Cuánto estaría dispuesto a invertir en vivienda nueva?`) %>%
  mutate(porc = n / sum(n))

ggplot(porcentaje_inversion,
       aes(x = `¿Cuánto estaría dispuesto a invertir en vivienda nueva?`,
           y = porc,
           fill = `¿Cuánto estaría dispuesto a invertir en vivienda nueva?`)) +
  geom_col(color = "black") +
  geom_text(aes(label = scales::percent(porc)),
            vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  xlab("") + ylab("") + labs(fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tamaño de vivienda
names(v3preferencias)[3]

porcentaje_tamaño <- v3preferencias %>%
  count(`¿Qué tamaño de vivienda prefiere en metros cuadrados?`) %>%
  mutate(porc = n / sum(n))


ggplot(porcentaje_tamaño,
       aes(x = `¿Qué tamaño de vivienda prefiere en metros cuadrados?`,
           y = porc,
           fill = `¿Qué tamaño de vivienda prefiere en metros cuadrados?`)) +
  geom_col(color = "black") +
  geom_text(aes(label = scales::percent(porc)),
            vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  xlab("") + ylab("") + labs(fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Tabla de contingencia metros cuadrados vs tipologia
tabla_cont <- table(
  v3preferencias$`¿Qué tamaño de vivienda prefiere en metros cuadrados?`,
  v3preferencias$`¿Qué tipo de vivienda preferiría?`
)

round(prop.table(tabla_cont, 2),3)*100

#Tabla de contingencia metros cuadrados vs inversión
tabla_cont2 <- table(
  v3preferencias$`¿Cuánto estaría dispuesto a invertir en vivienda nueva?`,
  v3preferencias$`¿Qué tamaño de vivienda prefiere en metros cuadrados?`
 
)

round(prop.table(tabla_cont2, 1),3)*100

# zonas de interes

zonas_interes <- v3preferencias %>% 
  dplyr::select(4:8) %>%
  rename("Trabajo" = "¿Qué zonas de interés son más importantes cerca de su vivienda? (1 = nada importante, 5 = muy importante)  [Lugar de trabajo]",
         "Transmetro" = "¿Qué zonas de interés son más importantes cerca de su vivienda? (1 = nada importante, 5 = muy importante)  [Estaciones de transmetro]",
         "Centros Comerciales" = "¿Qué zonas de interés son más importantes cerca de su vivienda? (1 = nada importante, 5 = muy importante)  [Centros comerciales]",
         "Hospitales" = "¿Qué zonas de interés son más importantes cerca de su vivienda? (1 = nada importante, 5 = muy importante)  [Hospitales]",
         "Zonas Verdes" = "¿Qué zonas de interés son más importantes cerca de su vivienda? (1 = nada importante, 5 = muy importante)  [Parques o Zonas Verdes]")

v2zonas_interes <- pivot_longer(zonas_interes, cols = 1:5, 
                      names_to = "Zonas_Interes",
                      values_to = "Calificación")

v3zonas_interes <- v2zonas_interes %>% 
  group_by(Zonas_Interes, Calificación) %>% 
  summarise(conteo = n()) %>% ungroup()
  
nota_perfecta <- 2399 * 5

v4zonas_interes <- v3zonas_interes %>%
  group_by(Zonas_Interes) %>%
  mutate(multiplicacion = Calificación*conteo) %>% ungroup() %>% 
  group_by(Zonas_Interes) %>% 
  mutate(nota_final = sum((multiplicacion/nota_perfecta))*5)

v5zonas_interes <- v4zonas_interes %>% 
  dplyr::select(Zonas_Interes, nota_final) %>% 
  distinct()

v5zonas_interes$nota_final <- as.numeric(v5zonas_interes$nota_final)

ggplot(v5zonas_interes,
       aes(x = Zonas_Interes,
           y = nota_final)) +
  geom_col(position = "dodge", color = "black", fill = "lightblue") +
  geom_text(aes(label = round(nota_final, 2)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "black")+
  labs(title = "Calificaciones dentro de Cada Zona de Interés",
       x = "Zonas de Interés",
       y = "Calificaciones") +
  theme_minimal() 

#administracion vs zonas comunes
names(v3preferencias)[9]
porcentaje_adm <- v3preferencias %>%
  count(`¿Prefiere pagar más administración con zonas comunes (piscina, gimnasio, salón social, juegos) o menos administración sin estas zonas?`) %>%
  mutate(porc = n / sum(n))

ggplot(porcentaje_adm,
       aes(x = "",
           y = porc,
           fill = `¿Prefiere pagar más administración con zonas comunes (piscina, gimnasio, salón social, juegos) o menos administración sin estas zonas?`)) +
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = scales::percent(porc, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 4) +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  labs(fill = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

#decisión de compra
names(v3preferencias)[16]
porcentaje_decision <- v3preferencias %>%
  count(`¿Qué frase describe mejor su decisión de compra?`) %>%
  mutate(porc = n / sum(n))

ggplot(porcentaje_decision,
       aes(x = "",
           y = porc,
           fill = `¿Qué frase describe mejor su decisión de compra?`)) +
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = scales::percent(porc, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 4) +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  labs(fill = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

#administracion vs zonas comunes
names(v3preferencias)[9]
porcentaje_adm <- v3preferencias %>%
  count(`¿Prefiere pagar más administración con zonas comunes (piscina, gimnasio, salón social, juegos) o menos administración sin estas zonas?`) %>%
  mutate(porc = n / sum(n))

ggplot(porcentaje_adm,
       aes(x = "",
           y = porc,
           fill = `¿Prefiere pagar más administración con zonas comunes (piscina, gimnasio, salón social, juegos) o menos administración sin estas zonas?`)) +
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = scales::percent(porc, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 4) +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  labs(fill = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

#pensamiento
names(v3preferencias)[17]
porcentaje_pensamiento <- v3preferencias %>%
  count(`¿Qué frase lo identifica más?`) %>%
  mutate(porc = n / sum(n))

ggplot(porcentaje_pensamiento,
       aes(x = "",
           y = porc,
           fill = `¿Qué frase lo identifica más?`)) +
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = scales::percent(porc, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 4) +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  labs(fill = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

#pensamiento
names(v3preferencias)[18]
porcentaje_uso <- v3preferencias %>%
  count(`¿Qué se acerca más a su forma de pensar?`) %>%
  mutate(porc = n / sum(n))

ggplot(porcentaje_uso,
       aes(x = "",
           y = porc,
           fill = `¿Qué se acerca más a su forma de pensar?`)) +
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = scales::percent(porc, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 4) +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  labs(fill = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right")+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

#Innovación
names(v3preferencias)[19]
innovacion <- v3preferencias %>% 
  dplyr::select(19) %>% mutate(
    Innovar = str_to_upper(`¿Qué innovación le gustaría ver en los proyectos de vivienda nueva?`),
    Innovar = str_replace_na(Innovar, "NO SABE"),
    Innovar = str_replace(Innovar, ".*SEGURIDAD.*|.*CAI.*|.*SISTEMA DE SEGURIDAD.*|.*MAS SEGURIDAD.*", "SEGURIDAD"),
    Innovar = str_replace(Innovar, ".*TECNO.*|.*SMART.*|.*INTELIGENTE.*|.*ROBOT.*|.*ROBOTICA.*|.*TECNOLÓG.*|.*TECNOLÓGICA.*|TECNOLOGIA.|.*ÚLTIMA TECNOLOGÍA.*|.*MODERN.*", "TECNOLOGIA"),
    Innovar = str_replace(Innovar, ".*SOSTENIBLE.*|.*ENERGÍAS.*|.*AMBIENTAL.*|ARBOLIZADA|.*SOLAR.*|.*PANELES.*|.*PANEL.*|VSOSTENI.*|.*SOSTENIBIL.*|.*AHORRO DE AGUA.*|.*AHORRO DE ENERGÍA.*|.*MEDIO AMBIENTE.*|.*LUZ SOLAR.*", "SOSTENIBILIDAD (PANELES SOLARES, AHORRO DE AGUA)"),
    Innovar = str_replace(Innovar, ".*MATERIAL.*|.*CALIDAD.*|.*MATERIALES RESISTENTES.*|.*MATERIALES DE ALTA CALIDAD.*", "MATERIALES/CALIDAD"),
    Innovar = str_replace(Innovar, ".*ESPACIO.*|.*ESPACIOS.*|.*GRANDE.*|.*MÁS AMPLIA.*|.*MAYOR TAMAÑO.*|.*PATIO.*|.*DISEÑO.*|.*ÁREA.*|.*AMPLIAS.*", "DISEÑO/ESPACIOS (COCINA, PATIO, ZONA DE LABORES)"),
    Innovar = str_replace(Innovar, ".*IGLESIA.*|.*LOCAL.*|.*PET FRIENDLY.*|.*ESTUFA.*|.*CUARTO.*|.*COCINA.*|.*AMPLIA.*|.*ZONA.*|.*JARDIN.*|.*ZONAS.*|.*ZONAS COMUNES.*|.*CAFETERÍA.*|.*COWORK.*|.*SALÓN.*|.*GIMNASIO.*|.*PISCINA.*|.*MASCOT.*", "ZONAS COMUNES (CAFETERIA, PET FRIENDLY, IGLESIA)"),
    Innovar = str_replace(Innovar, ".*MOVILIDAD.*|.*MÁS ACCESIBILIDAD.*|.*TRANSP.*|.*RUTA|TRANSPORTE.*|.*RUTAS.*", "MOVILIDAD"),
    Innovar = str_replace(Innovar, ".*SALUD.*|.*UNIDADES INTERNAS DE SALUD.*|.*SALUD.*", "SALUD"),
    Innovar = str_replace(Innovar, "^NA$|^NO$|^MA$|.*NINGUNO.*|.*NO SABE.*|.*NADA.*|.*NINGUNA.*", "NO SABE"),
    Innovar = str_replace(Innovar, "ACCESIBILIDAD PARA PAGO|.*PAGO.*|.*CAJEROS.*", "ACCESIBILIDAD PARA PAGO (CAJEROS Y CORRESPONSALES)"),
    
  )

unique(innovacion$Innovar)
v2innovacion <- innovacion %>% filter(Innovar %in% c(
  "NO SABE", "SEGURIDAD", "SALUD","SOSTENIBILIDAD (PANELES SOLARES, AHORRO DE AGUA)",
  "MATERIALES/CALIDAD", "TECNOLOGIA","DISEÑO/ESPACIOS (COCINA, PATIO, ZONA DE LABORES)",
  "ZONAS COMUNES (CAFETERIA, PET FRIENDLY)", "MOVILIDAD", "ACCESIBILIDAD PARA PAGO (CAJEROS Y CORRESPONSALES)"
)) %>% filter(Innovar != "NO SABE")

v3innovacion <- v2innovacion %>%
  count(Innovar)


#Grafica de innovaciones

  ggplot(v3innovacion, aes(x = n, y = Innovar)) +
    geom_point(size = 3, color = "#0073C2FF") +
    geom_text_repel(aes(label = Innovar), 
                    hjust = -0.1, 
                    size = 3.5,
                    box.padding = 0.4, 
                    point.padding = 0.5,
                    max.overlaps = Inf) + xlab(NULL)+
    labs(
      title = " ",
      x = "Frecuencia",
      y = NULL  # Quita la etiqueta del eje Y
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_blank(),  # Oculta las categorías del eje Y
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    ) 
apply(v3preferencias, 2, function(x) typeof(x))

#Modelo GOW - PAM


v4preferencias <- v3preferencias %>% 
  mutate(across(everything(), as.factor)) #Convertir todas las variables a factor

v4preferencias <- v4preferencias %>% 
  rename(
    "Tipo" = "¿Qué tipo de vivienda preferiría?",
    "Rango_precio" = "¿Cuánto estaría dispuesto a invertir en vivienda nueva?",
    "Rango_area" = "¿Qué tamaño de vivienda prefiere en metros cuadrados?",
    "Trabajo" = "¿Qué zonas de interés son más importantes cerca de su vivienda? (1 = nada importante, 5 = muy importante)  [Lugar de trabajo]",
    "Transmetro" = "¿Qué zonas de interés son más importantes cerca de su vivienda? (1 = nada importante, 5 = muy importante)  [Estaciones de transmetro]",
    "Centros_comerciales" = "¿Qué zonas de interés son más importantes cerca de su vivienda? (1 = nada importante, 5 = muy importante)  [Centros comerciales]",
    "Hospitales" = "¿Qué zonas de interés son más importantes cerca de su vivienda? (1 = nada importante, 5 = muy importante)  [Hospitales]",
    "Zonas_Verdes" = "¿Qué zonas de interés son más importantes cerca de su vivienda? (1 = nada importante, 5 = muy importante)  [Parques o Zonas Verdes]",
    "Administracion" = "¿Prefiere pagar más administración con zonas comunes (piscina, gimnasio, salón social, juegos) o menos administración sin estas zonas?",
    "Parqueadero_privado" = "¿Qué características valora más en una vivienda? (1 = nada importante, 5 = muy importante)  [Parqueadero privado]",
    "Balcon/Terraza" = "¿Qué características valora más en una vivienda? (1 = nada importante, 5 = muy importante)  [Balcón/Terraza]",
    "Vestier" = "¿Qué características valora más en una vivienda? (1 = nada importante, 5 = muy importante)  [Vestier]",
    "Cocina_abierta" = "¿Qué características valora más en una vivienda? (1 = nada importante, 5 = muy importante)  [Cocina abierta]",
    "Cocina_cerrada" = "¿Qué características valora más en una vivienda? (1 = nada importante, 5 = muy importante)  [Cocina cerrada]",
    "Cuarto_servicio" = "¿Qué características valora más en una vivienda? (1 = nada importante, 5 = muy importante)  [Cuarto de servicio]",
    "Pagar_mas/menos_adm" = "¿Qué frase describe mejor su decisión de compra?",
    "Entrega" = "¿Qué frase lo identifica más?",
    "Destino" = "¿Qué se acerca más a su forma de pensar?",
    "Innovacion_op" = "¿Qué innovación le gustaría ver en los proyectos de vivienda nueva?"
  )

v4preferencias <- v4preferencias %>% mutate(
    Innovar = str_to_upper(Innovacion_op),
    Innovar = str_replace_na(Innovar, "NO SABE"),
    Innovar = str_replace(Innovar, ".*SEGURIDAD.*|.*CAI.*|.*SISTEMA DE SEGURIDAD.*|.*MAS SEGURIDAD.*", "SEGURIDAD"),
    Innovar = str_replace(Innovar, ".*TECNO.*|.*SMART.*|.*INTELIGENTE.*|.*ROBOT.*|.*ROBOTICA.*|.*TECNOLÓG.*|.*TECNOLÓGICA.*|TECNOLOGIA.|.*ÚLTIMA TECNOLOGÍA.*|.*MODERN.*", "TECNOLOGIA"),
    Innovar = str_replace(Innovar, ".*SOSTENIBLE.*|.*ENERGÍAS.*|.*AMBIENTAL.*|ARBOLIZADA|.*SOLAR.*|.*PANELES.*|.*PANEL.*|VSOSTENI.*|.*SOSTENIBIL.*|.*AHORRO DE AGUA.*|.*AHORRO DE ENERGÍA.*|.*MEDIO AMBIENTE.*|.*LUZ SOLAR.*", "SOSTENIBILIDAD (PANELES SOLARES, AHORRO DE AGUA)"),
    Innovar = str_replace(Innovar, ".*MATERIAL.*|.*CALIDAD.*|.*MATERIALES RESISTENTES.*|.*MATERIALES DE ALTA CALIDAD.*", "MATERIALES/CALIDAD"),
    Innovar = str_replace(Innovar, ".*ESPACIO.*|.*ESPACIOS.*|.*GRANDE.*|.*MÁS AMPLIA.*|.*MAYOR TAMAÑO.*|.*PATIO.*|.*DISEÑO.*|.*ÁREA.*|.*AMPLIAS.*", "DISEÑO/ESPACIOS (COCINA, PATIO, ZONA DE LABORES)"),
    Innovar = str_replace(Innovar, ".*IGLESIA.*|.*LOCAL.*|.*PET FRIENDLY.*|.*ESTUFA.*|.*CUARTO.*|.*COCINA.*|.*AMPLIA.*|.*ZONA.*|.*JARDIN.*|.*ZONAS.*|.*ZONAS COMUNES.*|.*CAFETERÍA.*|.*COWORK.*|.*SALÓN.*|.*GIMNASIO.*|.*PISCINA.*|.*MASCOT.*", "ZONAS COMUNES (CAFETERIA, PET FRIENDLY, IGLESIA)"),
    Innovar = str_replace(Innovar, ".*MOVILIDAD.*|.*MÁS ACCESIBILIDAD.*|.*TRANSP.*|.*RUTA|TRANSPORTE.*|.*RUTAS.*", "MOVILIDAD"),
    Innovar = str_replace(Innovar, ".*SALUD.*|.*UNIDADES INTERNAS DE SALUD.*|.*SALUD.*", "SALUD"),
    Innovar = str_replace(Innovar, "^NA$|^NO$|^MA$|.*NINGUNO.*|.*NO SABE.*|.*NADA.*|.*NINGUNA.*", "NO SABE"),
    Innovar = str_replace(Innovar, "ACCESIBILIDAD PARA PAGO|.*PAGO.*|.*CAJEROS.*", "ACCESIBILIDAD PARA PAGO (CAJEROS Y CORRESPONSALES)"),
    
  )

v4preferencias <- v4preferencias %>% 
  mutate(across(everything(), as.factor))

v4preferencias <- v4preferencias %>% dplyr::select(1:18, 20) 
#Calcular distancias Gower
gower_dist <- daisy(v4preferencias, metric = "gower")

# Convertir a matriz si tú lo necesitas
gower_mat <- as.matrix(gower_dist)

# ----------------------------------------------------------
# 5. Determinar el número óptimo de clusters (silhouette)
# ----------------------------------------------------------

sil_width <- c()

for(k in 2:8){
  pam_fit <- pam(gower_dist, diss = TRUE, k = k)
  sil_width[k] <- pam_fit$silinfo$avg.width
}

# Gráfico de silueta
plot(2:8, sil_width[2:8], type = "b",
     xlab = "Número de clusters",
     ylab = "Anchura promedio de Silhouette")

k_optimo <- 2

pam_final <- pam(gower_dist, diss = TRUE, k = k_optimo)

# Agregar la asignación de cluster al dataframe
v4preferencias$cluster <- factor(pam_final$clustering)

#visualización de clustering con reducción de dimensiones
mds <- cmdscale(gower_dist, k = 2) %>% as.data.frame()

colnames(mds) <- c("Dim1", "Dim2")
mds$cluster <- factor(pam_final$clustering)

ggplot(mds, aes(x = Dim1, y = Dim2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(type = "t", size = 1, linetype = 2) +
  theme_minimal() +
  labs(title = "Clusters con PAM + Gower (MDS + Elipses)")

perfil_clusters <- v4preferencias %>%
  mutate(cluster = pam_final$clustering) %>%
  group_by(cluster) %>%
  summarise(across(everything(), ~ names(sort(table(.), decreasing = TRUE))[1]))

perfil_clusters

#Mapa de hurtos en el atlántico


# Cargar el shapefile con sf
Municipal <- st_read("SHP_MPIO_DEFVIV.shp")
Municipal <- Municipal[Municipal$DPTO_CNMBR == "ATLÁNTICO",]


df <- read_csv2("hurtos.csv")
df$Código <- paste0("0",df$Código)
df <- df[,3:4]

Municipal_2 <- mutate(Municipal, 
                      hurtos = left_join(Municipal, df, by = c("MPIO_CCDGO" = "Código")))

Municipal_2_sf <- st_as_sf(Municipal_2)
hurtos_df <- as.data.frame(Municipal_2_sf$hurtos)

Municipal_2_sf$MPIO_CNMBR <- str_replace(Municipal_2_sf$MPIO_CNMBR,
                                         "DISTRITO ESPECIAL, INDUSTRIAL Y PORTUARIO DE BARRANQUILLA",
                                         "B/QUILLA")

# Visualizar el shapefile con ggplot2

Municipal_2_sf <- Municipal_2_sf %>% 
  left_join(hurtos_df, by = "MPIO_CCDGO")

geom_sf(data = Municipal_2_sf, aes(fill = Valor))
str(Municipal_2_sf$Valor)

ggplot() +
  geom_sf(data = Municipal_2_sf, aes(fill = Valor)) +
  geom_sf_text(data = Municipal_2_sf, aes(label = MPIO_CNMBR.x), size = 2, color = "black") +
  scale_fill_gradient(
    low = "#FFF8DC",
    high = "#CD3333"
  ) +
  ggtitle("Hurtos por municipio en el Atlántico") +
  labs(x = "", y = "", fill = "Hurtos") +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 16),
    plot.caption = element_text(size = 12)
  )
summary(Municipal_2_sf$Valor)
