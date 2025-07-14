# Paquetes necesarios
install.packages("googlesheets4")
install.packages("tidyverse")
install.packages("agricolae")

library(googlesheets4)
library(tidyverse)
library(agricolae)

# Leer datos desde Google Sheets
url <- "https://docs.google.com/spreadsheets/d/1hHv-J6V39lUt5uhSaVRD6w1rYm8M4Rz90MDVHG_K6WQ/edit?gid=1683251465"
datos <- read_sheet(url, sheet = "fb")

# Convertir factores
datos <- datos %>%
  mutate(
    tiempo = as.factor(tiempo),
    tetrazolio = as.factor(tetrazolio),
    block = as.factor(block),
    trat_comb = interaction(tetrazolio, tiempo)
  )

# Lista de variables de respuesta
variables <- c("via_sindef", "via_leve", "via_sev", "no_via", "via")

# 🔁 Bucle para análisis de todas las variables
for (var in variables) {
  cat("\n============================================\n")
  cat("🔬 ANÁLISIS PARA:", var, "\n")
  cat("============================================\n\n")
  
  # Crear fórmula dinámica
  formula <- as.formula(paste(var, "~ trat_comb + block"))
  
  # ANOVA
  modelo <- aov(formula, data = datos)
  print(summary(modelo))
  
  # Tukey HSD
  tukey <- HSD.test(modelo, "trat_comb", group = TRUE)
  print(tukey)
  $ groups
  
  # Gráfico
  bar.group(tukey,
            col = "lightblue",
            main = paste("Comparación de medias -", var),
            ylab = var,
            ylim = c(0, max(datos[[var]], na.rm = TRUE) + 5))
  
  # Espera para ver gráfico
  readline(prompt = "Presiona [Enter] para continuar con la siguiente variable...")
}
datos %>%
  group_by(trat_comb) %>%
  summarise(media = mean(.data[[var]], na.rm = TRUE),
            sd = sd(.data[[var]], na.rm = TRUE)) %>%
  ggplot(aes(x = trat_comb, y = media)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd), width = 0.2) +
  labs(title = paste("Media y SD de", var),
       x = "Tratamientos",
       y = var) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


formula <- as.formula(paste(var, "~ trat_comb + block"))

# ANOVA
modelo <- aov(formula, data = datos)
print(summary(modelo))

# Tukey
tukey <- HSD.test(modelo, "trat_comb", group = TRUE)

letras <- tukey$groups %>%
  rownames_to_column("trat_comb") %>%
  rename(letra = groups)

# Calcular medias y desviaciones estándar
resumen <- datos %>%
  group_by(trat_comb) %>%
  summarise(
    media = mean(.data[[var]], na.rm = TRUE),
    sd = sd(.data[[var]], na.rm = TRUE)
  ) %>%
  left_join(letras, by = "trat_comb")

# Crear gráfico
p <- ggplot(resumen, aes(x = trat_comb, y = media)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd), width = 0.2) +
  geom_text(aes(label = letra, y = media + sd + 1), size = 5) +
  labs(
    title = paste("Comparación de tratamientos para", var),
    x = "Tratamiento (Tetrazolio × Tiempo)",
    y = var
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)









datos <- datos %>%
  mutate(
    tetrazolio = as.factor(tetrazolio),
    tiempo = as.factor(tiempo),
    trat_comb = interaction(tetrazolio, tiempo, sep = " × ")
  )

# Variables de interés
variables <- c("via_sindef", "via_leve", "via_sev", "no_via")

# Pivotear datos a formato largo para graficar múltiples variables
datos_largo <- datos %>%
  select(trat_comb, all_of(variables)) %>%
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "valor"
  )

# Calcular medias por tratamiento y variable
resumen <- datos_largo %>%
  group_by(trat_comb, variable) %>%
  summarise(
    media = mean(valor, na.rm = TRUE),
    sd = sd(valor, na.rm = TRUE),
    .groups = "drop"
  )

# 🎨 Gráfico combinado: barras agrupadas
ggplot(resumen, aes(x = trat_comb, y = media, fill = variable)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(
    title = "Comparación de tratamientos para todas las variables",
    x = "Tratamiento (Tetrazolio × Tiempo)",
    y = "Media",
    fill = "Variable evaluada"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










# Paquetes necesarios
install.packages("googlesheets4")
install.packages("tidyverse")
install.packages("agricolae")

library(googlesheets4)
library(tidyverse)
library(agricolae)

# Leer datos
url <- "https://docs.google.com/spreadsheets/d/1hHv-J6V39lUt5uhSaVRD6w1rYm8M4Rz90MDVHG_K6WQ/edit?gid=1683251465"
datos <- read_sheet(url, sheet = "fb")

# Convertir factores y crear columna de tratamientos
datos <- datos %>%
  mutate(
    tetrazolio = as.factor(tetrazolio),
    tiempo = as.factor(tiempo),
    trat_comb = interaction(tetrazolio, tiempo, sep = " × "),
    block = as.factor(block)
  )

# Variables a analizar
variables <- c("via_sindef", "via_leve", "via_sev", "no_via")

# Crear dataframe vacío para guardar resultados
resultado_final <- data.frame()

# Bucle para calcular medias, SD y letras de Tukey
for (var in variables) {
  
  # ANOVA y Tukey
  formula <- as.formula(paste(var, "~ trat_comb + block"))
  modelo <- aov(formula, data = datos)
  tukey <- HSD.test(modelo, "trat_comb", group = TRUE)
  
  # Letras
  letras <- tukey$groups %>%
    rownames_to_column("trat_comb") %>%
    rename(letra = groups)
  
  # Medias y SD
  resumen <- datos %>%
    group_by(trat_comb) %>%
    summarise(
      media = mean(.data[[var]], na.rm = TRUE),
      sd = sd(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(letras, by = "trat_comb") %>%
    mutate(variable = var)
  
  # Agregar al resultado final
  resultado_final <- bind_rows(resultado_final, resumen)
}

# Gráfico final con todas las variables y letras
ggplot(resultado_final, aes(x = trat_comb, y = media, fill = variable)) +
  geom_col(position = position_dodge(0.9), width = 0.8) +
  geom_errorbar(
    aes(ymin = media - sd, ymax = media + sd),
    position = position_dodge(0.9), width = 0.2
  ) +
  geom_text(
    aes(label = letra, y = media + sd + 1),
    position = position_dodge(0.9),
    size = 5
  ) +
  labs(
    title = "Comparación de tratamientos con letras de Tukey",
    x = "Tratamiento (Tetrazolio × Tiempo)",
    y = "Media",
    fill = "Variable evaluada"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
