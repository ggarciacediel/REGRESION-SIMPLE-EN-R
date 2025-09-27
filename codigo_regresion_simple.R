install.packages("ggplot2")  # Para gráficos

library(readxl)
library(ggplot2)


#Explorar datos
head(datos)        # primeras filas
summary(datos)     # resumen estadísticdatos <- read_excel("G:/Mi unidad/portatil/tematicas/econometria/clases/LISTAS/CLASES II 2025/DATOS1.xlsx")
o
names(datos)       # nombres de las variables

# 7. Histograma de escolaridad
hist(datos$ESCOLARIDAD,
     breaks = 10,
     col = "lightgreen",
     border = "black",
     main = "Histograma de Años de Escolaridad",
     xlab = "Años de escolaridad",
     ylab = "Frecuencia")
abline(v = mean(datos$ESCOLARIDAD), col = "red", lwd = 2, lty = 2)

# 8. Gráfico de correlación entre salario y escolaridad
# Método 1: Correlación numérica
correlacion <- cor(datos$ESCOLARIDAD, datos$SALARIO, use = "complete.obs")
correlacion

# Método 2: Gráfico de correlación con GGally
ggpairs(datos[, c("ESCOLARIDAD", "SALARIO")],
        title = "Matriz de Correlación: Escolaridad y Salario")

# Método 3: Mapa de calor de correlación
corr <- cor(datos[, c("ESCOLARIDAD", "SALARIO")], use = "complete.obs")
ggplot(data = as.data.frame(as.table(corr)),
       aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1)) +
  labs(title = "Mapa de calor de correlación",
       x = "", y = "")


# 4. Estimar regresión lineal simple
# Suponemos: SALARIO ~ ESCOLARIDAD
modelo_simple <- lm(SALARIO ~ ESCOLARIDAD, data = datos)

# 5. Resultados de la regresión
summary(modelo_simple)


# 6. Visualización de la regresión
ggplot(datos, aes(x = ESCOLARIDAD, y = SALARIO)) +
  geom_point(color = "blue") +                          # puntos de datos
  geom_smooth(method = "lm", se = FALSE, color = "red") + # recta de regresión
  labs(title = "Regresión lineal simple: Salario vs Escolaridad",
       x = "Años de escolaridad",

       y = "Salario")
