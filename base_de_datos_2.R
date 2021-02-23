# Base de datos 2
pacman::p_load(purrr, stringr, dplyr, tidyr, ggplot2, lubridate)

# Importar al entorno el texto de los PDFs diarios (obtenidos previamente mediante web scraping)
load("base_de_datos_2.RData")

# 2. Con la información proporcionada a nivel mundial, cree un tabla y muestre un gráfico que visualice la información mensual.

# Extraer el texto del que se obtiene la informacion por regiones de la OMS
regions_info <- map_depth(pdf_files, 2, pluck, 1)

# Extraer los datos de cada continente

# America
america <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "1[3-6],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_if(~ length(.x) == 2, pluck, 1) %>%
  # Completar manualmente los numero faltantes
  map_at(24, ~ "15,872,421") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Europa
europe <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "(?<=\\n|\\s)[4-5],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(c(1, 7), pluck, 1) %>%
  map_if(~ length(.x) > 1, pluck, 2) %>%
  # Corregir manualmente ciertos numeros
  map_at(9, ~ "4,600,967") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Asia Sudoriental
southeastern_asia <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "(?<=\\s)[4-6],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(7, pluck, 2) %>%
  map_if(~ length(.x) > 1, pluck, 1) %>%
  # Corregir manualmente ciertos numeros
  map_at(24, ~ "6,436,394") %>%
  map_at(28, ~ "6,810,494") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# África
africa <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "(?<=\\n)1,([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(c(2, 4, 23), pluck, 1) %>%
  map_at(5:6, pluck, 2) %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Mediterráneo Oriental
eastern_mediterranean <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "[1-2],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(c(1:2, 17, 18, 28), pluck, 1) %>%
  map_at(c(22:27, 30), pluck, 2) %>%
  map_at(3, pluck, 3) %>%
  map_if(~ length(.x) == 2, pluck, 1) %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Pacífico Occidental
western_pacific <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "(?<=\\n)[4-6][:digit:]{2},[:digit:]{3}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_if(~ length(.x) == 2, pluck, 1) %>%
  # Completar manualmente los numero faltantes
  map_at(3, ~ "505,156") %>%
  map_at(11, ~ "535,413") %>%
  map_at(14, ~ "550,664") %>%
  map_at(16, ~ "560,287") %>%
  map_at(19, ~ "573,120") %>%
  map_at(20, ~ "577,905") %>%
  map_at(26, ~ "598,060") %>%
  map_at(27, ~ "600,891") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Almacenar los resultados en una tabla
worldwide_info <- as_tibble(
  list(
    "Días" = seq(from = ymd('2020-09-01'), to = ymd('2020-09-30'), by = 'days'),
    "América" = america,
    "Asia Sudoriental" = southeastern_asia,
    "Europa" = europe,
    "Mediterráneo Oriental" = eastern_mediterranean,
    "África" = africa,
    "Pacífico Occidental" = western_pacific
  )
)

worldwide_info

# Crear grafico para visualizar la información mensual
worldwide_info %>%
  pivot_longer(-1, names_to = "Regiones de la OMS", values_to = "cases") %>%
  ggplot(aes(x = `Días`, y = cases, color = `Regiones de la OMS`)) +
    geom_line() +
    scale_colour_manual(values = c("green", "red2", "purple", "blue2", "green4", "yellow3")) +
    labs(y = "Número de casos", title = "Casos acumulados de COVID-19 por SARS-CoV-2 por regiones de la OMS\n(Septiembre 2020)")

# 3. Con la información de defunciones positivas, cree un tabla y muestre un gráfico que visualice la información mensual.

# Extraer el texto del que se obtiene la informacion por entidad federativa
entities_info <- map_depth(pdf_files, 2, pluck, 2) %>%
  map(str_extract_all, pattern = "([:alpha:]+\\s){1,3}\\| [:digit:]+")

## Chihuahua, Chiapas, Campeche y Michoacan ##
ch <- entities_info %>%
  # Elegir los valores que contengan "CH"
  map_depth(2, str_subset, pattern = "[C|c][H|h]") %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Chihuahua
chihuahua <- ch %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "CHIHU", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  map_depth(1, flatten_chr) %>%
  # Extraer los numeros de 4 digitos de las cadenas
  map_depth(1, str_extract_all, pattern = "[:digit:]{4}") %>%
  # Poner los resultados en un solo arreglo por dia
  map_depth(1, flatten_chr) %>%
  # Eliminar los resultados repetido
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(c(21, 25), pluck, 1) %>%
  # Completar manualmente ciertos numeros
  map_at(1, ~ "1147") %>%
  map_at(2, ~ "1159") %>%
  map_at(3, ~ "1169") %>%
  map_at(4, ~ "1177") %>%
  map_at(5, ~ "1187") %>%
  map_at(6, ~ "1198") %>%
  map_at(8, ~ "1211") %>%
  map_at(11, ~ "1233") %>%
  map_at(13, ~ "1240") %>%
  map_at(14, ~ "1241") %>%
  map_at(17, ~ "1258") %>%
  map_at(22, ~ "1291") %>%
  map_at(29, ~ "1371") %>%
  map_at(30, ~ "1382") %>%
  # Corregir manualmente ciertos numeros
  map_at(24, ~ "1318") %>%
  map_at(25, ~ "1336") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Chiapas
chiapas <- ch %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "CHIAPAS", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  map_depth(1, flatten_chr) %>%
  # Extraer los numeros de las cadenas
  map_depth(1, str_extract_all, pattern = "[:digit:]+") %>%
  # Poner los resultados en un solo arreglo por dia
  map_depth(1, flatten_chr) %>%
  # Eliminar los resultados repetido
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(3, pluck, 1) %>%
  map_at(c(21, 23, 25), pluck, 2) %>%
  map_at(c(22, 24), pluck, 3) %>%
  # Completar manualmente ciertos numeros
  map_at(1, ~ "1002") %>%
  map_at(c(5, 8, 9, 11), ~ "1009") %>%
  map_at(16, ~ "1016") %>%
  map_at(26, ~ "1018") %>%
  map_at(27, ~ "1019") %>%
  map_at(29:30, ~ "1020") %>%
  # Corregir manualmente ciertos numeros
  map_at(19:20, ~ "1016") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Michoacan
michoacan <- ch %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "MICH", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  map_depth(1, flatten_chr) %>%
  # Extraer los numeros de 4 digitos de las cadenas
  map_depth(1, str_extract_all, pattern = "[:digit:]{4}") %>%
  # Poner los resultados en un solo arreglo por dia
  map_depth(1, flatten_chr) %>%
  # Eliminar los resultados repetido
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(24:25, pluck, 1) %>%
  # Completar manualmente ciertos numeros
  map_at(1, ~ "1187") %>%
  map_at(2, ~ "1227") %>%
  map_at(4, ~ "1259") %>%
  map_at(5, ~ "1277") %>%
  map_at(6, ~ "1281") %>%
  map_at(7, ~ "1288") %>%
  map_at(8, ~ "1303") %>%
  map_at(9, ~ "1328") %>%
  map_at(12, ~ "1380") %>%
  map_at(14, ~ "1399") %>%
  map_at(17, ~ "1452") %>%
  map_at(18, ~ "1468") %>%
  map_at(19, ~ "1480") %>%
  map_at(21, ~ "1489") %>%
  map_at(22, ~ "1517") %>%
  map_at(27, ~ "1588") %>%
  # Corregir manualmente ciertos numeros
  map_at(10, ~ "1345") %>%
  map_at(16, ~ "1441") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

## Sonora, Sinaloa y San Luis Potosi ##
sn <- entities_info %>%
  # Elegir los valores que comiencen con "S"
  map_depth(2, str_subset, pattern = "^[S|s]") %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Sonora
sonora <- sn %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "SONO", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  map_depth(1, flatten_chr) %>%
  # Extraer los numeros de 4 digitos de las cadenas
  map_depth(1, str_extract_all, pattern = "[:digit:]{4}") %>%
  # Poner los resultados en un solo arreglo por dia
  map_depth(1, flatten_chr) %>%
  # Eliminar los resultados repetido
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(2, pluck, 1) %>%
  map_at(7, pluck, 2) %>%
  # Completar manualmente ciertos numeros
  map_at(5, ~ "1277") %>%
  map_at(9, ~ "2771") %>%
  map_at(11, ~ "2794") %>%
  map_at(13, ~ "2804") %>%
  map_at(14, ~ "2806") %>%
  map_at(17, ~ "2814") %>%
  map_at(23, ~ "2868") %>%
  map_at(25, ~ "2879") %>%
  map_at(26, ~ "2883") %>%
  map_at(27, ~ "2884") %>%
  map_at(28, ~ "2886") %>%
  map_at(29, ~ "2897") %>%
  map_at(30, ~ "2899") %>%
  # Corregir manualmente ciertos numeros
  map_at(5, ~ "2693") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Sinaloa
sn %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "SINA", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  map_depth(1, flatten_chr) %>%
  # Extraer los numeros de 4 digitos de las cadenas
  map_depth(1, str_extract_all, pattern = "[:digit:]{4}") %>%
  # Poner los resultados en un solo arreglo por dia
  map_depth(1, flatten_chr) %>%
  # Eliminar los resultados repetido
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(c(3:4, 11, 13), pluck, 1) %>%
  # Completar manualmente ciertos numeros
  map_at(5, ~ "2868") %>%
  map_at(7, ~ "2879") %>%
  map_at(9, ~ "2914") %>%
  map_at(10, ~ "2949") %>%
  map_at(15, ~ "2993") %>%
  map_at(17, ~ "3004") %>%
  # Corregir manualmente ciertos numeros
  map_at(12, ~ "2669") %>%
  map_at(20, ~ "3052")

## Ciudad de Mexico y Estado de Mexico ##
mex <- entities_info %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "MEXICO", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

## Baja California y Baja California Sur ##
bc <- entities_info %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "CALIFORNIA", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

## Quintana Roo y Queretaro ##
qr <- entities_info %>%
  # Elegir los valores que comiencen con "QU"
  map_depth(2, str_subset, pattern = "^[Q|q][U|u]") %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

## Tabasco y Tamaulipas ##
ta <- entities_info %>%
  # Elegir los valores que comiencen con "TA"
  map_depth(2, str_subset, pattern = "^[T|t][A|a]") %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

## Guanajuato y Guerrero ##
gu <- entities_info %>%
  # Elegir los valores que comiencen con "GU"
  map_depth(2, str_subset, pattern = "^[G|g][U|u]") %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

## Colima y Coahuila ##
co <- entities_info %>%
  # Elegir los valores que comiencen con "CO"
  map_depth(2, str_subset, pattern = "^[C|c][O|o]") %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

## Tlaxcala y Oaxaca ##
xla <- entities_info %>%
  # Elegir los valores que contengan "XA o XC"
  map_depth(2, str_subset, pattern = "[X|x]([A|a]|[C|c])") %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Veracruz
ver <- entities_info %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "VERACRUZ", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Puebla
pue <- entities_info %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "PUEBLA", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Jalisco
ja <- entities_info %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "JALISCO", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Nuevo Leon
nl <- entities_info %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "LEON", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Hidalgo
hi <- entities_info %>%
  # Elegir los valores que comiencen con "H"
  map_depth(2, str_subset, pattern = "^[H|h]") %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Yucatan
yu <- entities_info %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "YUC", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Morelos
mo <- entities_info %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "MORELOS", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Nayarit
na <- entities_info %>%
  # Aproximar la busqueda de texto usando logica difusa (Usando la distancia de edicion de Levenshtein)
  map_depth(2, agrep, pattern = "NAYAR", value = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Zacatecas
za <- entities_info %>%
  # Elegir los valores que contengan "ZA"
  map_depth(2, str_subset, pattern = "[Z|z][A|a]") %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Durango
du <- entities_info %>%
  # Elegir los valores que contengan "DU"
  map_depth(2, str_subset, pattern = "[D|d][U|u]") %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Aguascalientes
ag <- entities_info %>%
  # Elegir los valores que comiencen con "AG"
  map_depth(2, str_subset, pattern = "^[A|a][G|g]") %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los valores repetidos
  map(unique)

# Almacenar los resultados en una tabla
positive_deaths_info <- as_tibble(
  list(
    "Días" = seq(from = ymd('2020-09-01'), to = ymd('2020-09-30'), by = 'days'),
    "Chiapas" = chiapas,
    "Chihuahua" = chihuahua,
    "Michoacán" = michoacan,
    "Sonora" = sonora
  )
)

# Crear grafico para visualizar la información mensual
positive_deaths_info %>%
  pivot_longer(-1, names_to = "Entidades federativas", values_to = "cases") %>%
  ggplot(aes(x = `Días`, y = cases, color = `Entidades federativas`)) +
  geom_line() +
  labs(y = "Defunciones positivas", title = "Defunciones positivas a COVID-19 por entidad federativa\n(Septiembre 2020)")

