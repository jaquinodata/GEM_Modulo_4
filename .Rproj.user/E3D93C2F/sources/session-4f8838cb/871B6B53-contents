# Librerías a utilziar
install.packages("readxl")
install.packages("normtest")
library(readxl)
library(tidyverse)
library(dplyr)
library(magrittr)
library(tseries)
library(purrr)
library(tibble)
library(dplyr)
library(lmtest)
library(caret)




datos <- read_excel("../store24_data.xls", sheet ="Store24")
datos31 <- datos |>
  mutate(
    interaccion_CTenure_CrewSkill = CTenure * CrewSkill,
    interaccion_MTenure_MgrSkill  = MTenure * MgrSkill
  ) |> 
    select(c(Sales, Profit, MTenure, CTenure, Pop, Comp, CrewSkill, MgrSkill, ServQual, interaccion_CTenure_CrewSkill,
             interaccion_MTenure_MgrSkill  )) |>
  scale() |>
  as.data.frame()





# Establecer semilla para reproducibilidad
set.seed(123)

# Dividir el conjunto de datos en entrenamiento (80%) y prueba (20%)
indice_entrenamiento <- createDataPartition(datos31$Profit, p = 0.8, list = FALSE)

datos31_entrenamiento <- datos3[indice_entrenamiento, ]
datos31_prueba <- datos3[-indice_entrenamiento, ]

# Separar las variables predictoras (X) y la variable objetivo (y)
X31_entrenamiento <- datos31_entrenamiento[, !(names(datos31_entrenamiento) %in% "Profit")]
y31_entrenamiento <- datos31_entrenamiento[, "Profit", drop = FALSE] 
X31_prueba <- datos31_prueba[, !(names(datos31_prueba) %in% "Profit")]
y31_prueba <- datos31_prueba$Profit

# 1. Ajustar el modelo
modelo31 <- lm(data = cbind(X31_entrenamiento, y31_entrenamiento), formula = Profit ~ Sales + MTenure + CTenure + Pop + Comp + CrewSkill + MgrSkill + ServQual + 
                 interaccion_CTenure_CrewSkill + interaccion_MTenure_MgrSkill ) 




# Predecir utilizando el modelo
y31_pred <- predict(modelo31, newdata = X3_prueba)

# Calcular RMSE
rmse31 <- sqrt(mean((y31_prueba - y31_pred)^2))

# Calcular R²
r31 <- cor(y31_pred, y31_prueba)^2

# Crear un data frame con los resultados
resultados31 <- data.frame(
  "Métrica" = c("RMSE (Error Cuadrático Medio Raíz)", "R² (Coeficiente de Determinación)"),
  "Valor" = c(rmse31, r31)
)

# Obtener los coeficientes del modelo
coeficientes31 <- coef(modelo31)

# Crear un dataframe con los coeficientes
df31_coeficientes <- data.frame(
  "Variable" = names(coeficientes31),
  "Coeficiente" = coeficientes31
)

# Formatear la tabla con kable
tabla31_coeficientes <- knitr::kable(df31_coeficientes, align = "c", caption = "Coeficientes del Modelo de Regresión")

# Imprimir la tabla
print(tabla31_coeficientes)


# Gráfico de los coeficientes

# Extraer los coeficientes del modelo
coeficientes31 <- coef(modelo31)[-1]  # Excluyendo el intercepto

# Crear un dataframe con los nombres de las variables y los coeficientes
coeficientes31_df <- data.frame(Variable = names(coeficientes31), Coeficiente = coeficientes31)

# Ordenar el dataframe por los valores de los coeficientes
coeficientes31_df <- coeficientes31_df[order(coeficientes31_df$Coeficiente), ]

# Crear el gráfico de barras
grafico_coefs31 <- ggplot(coeficientes31_df, aes(x = reorder(Variable, Coeficiente), y = Coeficiente)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Coeficientes del Modelo de Regresión Lineal",
       x = "Variable",
       y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x

# Mostrar el gráfico
print(grafico_coefs31)













































