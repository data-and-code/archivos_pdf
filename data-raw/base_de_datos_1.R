# Base de datos 1
pacman::p_load(fs, magrittr, purrr, tibble, dplyr, stringr, quanteda, pdftools)

# 1. Extraer la información que se encuentre en la primera base de datos.

# Leer el contenido de los archivos PDFs y almacenarlos en una sola lista
pdf_files_1 <- dir_ls("extdata/Temarios IDeIO finales/", recurse = TRUE, glob = "*.pdf") %>%
  map(pdf_text) %>%
  # Colapsar el texto de cada PDF en una sola cadena
  map_chr(str_c, collapse = " ")

# Extraer los nombres de los ciclos en orden
cycles <- names(pdf_files_1) %>%
  str_remove("^../") %>%
  str_split_fixed(pattern = "/", n = 4) %>%
  extract(, 3) %>%
  unique() %>%
  extract(c(2:4, 1))

# Obtener una lista de listas con el contenido de los PDFs agrupados por ciclos
syllabus <- cycles %>%
  # Separar y agrupar los PDFs por ciclos
  map(~ keep(pdf_files_1, str_detect(names(pdf_files_1), pattern = .x))) %>%
  # Asignar el nombre de cada ciclo
  set_names(nm = cycles) %>%
  # Renombrar los PDFs extrayendo solamente el nombre de la materia
  map_depth(1, ~ set_names(.x, str_extract(names(.x), pattern = "[A-Z]{2}[0-9]{3}.*(?=.pdf)")))

# Visualizar el resultado 1
View(syllabus)

# 2. Crear una tabla con el número de palabras totales por ciclo.

# Importar al entorno los terminos de parada (stop words) en español
stop_words_es <- stopwords(language = "es")

# Composicion de funciones para obtener las palabras del texto
get_words <- compose(
  # Convertir el texto a minusculas
  str_to_lower,
  # Dividir y extraer las subcadenas de texto que correspondan a palabras
  ~ tokens(.x, remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE),
  # Convertir el resultado a un vector de caracteres
  as.character,
  # Eliminar los terminos de parada de las palabras
  ~ .x[!.x %in% stop_words_es],
  # Eliminar los tokens que solo estan compuestos por un caracter
  ~ .x[str_length(.x) > 1],
  # Ejecutar las funciones en el orden que fueron declaradas
  .dir = "forward"
)

# Extraer todas las palabras por temario
words_by_syllabus <- syllabus %>%
  map_depth(2, get_words)

# Obtener el numero de palabras totales por ciclo
num_words_by_cycle <- words_by_syllabus %>%
  # Contar el numero de palabras de cada temario
  map_depth(2, length) %>%
  # Obtener el numero de palabras por ciclo
  map_depth(1, reduce, sum) %>%
  # Convertir la lista a una tabla
  as_tibble()

# Visualizar el resultado 2
View(num_words_by_cycle)

# 3. Crear una tabla con las 10 palabras con mayor frecuencia que coinciden por ciclo.
most_freq_by_cycle <- words_by_syllabus %>%
  # Eliminar un nivel para tener las palabras de todos los temarios por ciclo
  map_depth(1, ~ reduce(.x, append)) %>%
  # Convertir los arreglos de palabras en una matriz dispersa de termino-documento por ciclo
  map(dfm) %>%
  # Obtener las 10 palabras mas frecuentes por ciclo
  map(textstat_frequency, n = 10) %>%
  # Seleccionar ciertas columnas
  map(as_tibble) %>%
  map(~ select(.x, Palabras = feature, Frecuencia = docfreq)) %>%
  # Agregar una columna con el nombre del ciclo
  map2(cycles, ~ add_column(.x, Ciclo = .y)) %>%
  # Convertir el resultado en una sola tabla
  reduce(bind_rows)

# Visualizar el resultado 3
View(most_freq_by_cycle)

# 4. Crear una tabla con las palabras que son únicas en cada archivo.

# Tener las palabras por temario eliminando la agrupacion por ciclo
words_by_syllabus_no_cycle <- words_by_syllabus %>%
  flatten()

# Obtener las palabras que son unicas en cada archivo
uniq_words_by_syllabus <- words_by_syllabus_no_cycle %>%
  map2(names(words_by_syllabus_no_cycle), ~ .x[!.x %in% reduce(words_by_syllabus_no_cycle[names(words_by_syllabus_no_cycle) != .y], append)]) %>%
  # Filtrar las palabras para que sean unicas
  map(unique) %>%
  # Agregar una columna con el nombre del ciclo
  map(as_tibble) %>%
  map2(names(words_by_syllabus_no_cycle), ~ add_column(.x, Archivo = .y)) %>%
  # Convertir el resultado en una sola tabla
  reduce(bind_rows) %>%
  # Renombrar la columna con las palabras
  rename(Palabras = value)

# Visualizar el resultado 4
View(uniq_words_by_syllabus)


# 5. Crear una tabla con la siguiente información: nombre de la asignatura, clave, ciclo, créditos.

subjects_info <-
  # Extraer la informacion con expresiones regulares
  list(
    "Nombre de la asignatura" = map_depth(syllabus, 1, ~ str_trim(str_extract(names(.x), pattern = "(?<=[A-Z]{2}[0-9]{4}).*"))),
    "Clave" = map_depth(syllabus, 1, ~ str_extract(names(.x), pattern = "[A-Z]{2}[0-9]{4}")),
    "Ciclo" = map_depth(syllabus, 1, ~ str_extract(.x, pattern = "(?<=Área de formación curricular\n\\s?)[1-4]-[1-4]")),
    "Créditos" = map_depth(syllabus, 1, ~ str_trim(str_extract(.x, pattern = "(?<=[A-Z]{2}[0-9]{4}\\s{2,10}).*(?=\\s{2,10}(Profesional Asociado|Licenciatura (Básica|Elección|Preespecialidad)))")))
  ) %>%
  # Reducir las dimensiones para eliminar la agrupacion por ciclos
  map_depth(1, ~ reduce(.x, append)) %>%
  # Convertir el resultado a una tabla
  as_tibble()

# Visualizar el resultado 5
View(subjects_info)

# Almacenar el resultado en un archivo RData
save(pdf_files_1, most_freq_by_cycle, num_words_by_cycle, subjects_info, syllabus, uniq_words_by_syllabus,
     words_by_syllabus, words_by_syllabus_no_cycle, file = "app/base_de_datos_1.RData")
