library(readxl)
library(dplyr)
library(ggplot2)  
library(lattice)
library(GGally)
library(car)
library(gridExtra)
library(stats)
library(visreg)

###### Anális en zona con patrones ######
datos_p <- read.csv("datos_p.csv", header = TRUE, row.names = 1)
str(datos_p)
View(datos_p)

# Test de normalidad 
pruebas <- c("H_den", "H_fue", "T_den", "T_fue", "Pen", "N_ind", "T_par")
shapiro_tests1 <- lapply(datos_p[pruebas], shapiro.test)
shapiro_tests1

# Prueba de Levene para homocedasticidad
datos_p$Gru <- as.factor(datos_p$Gru)

variables1 <- c("H_den", "H_fue", "T_den", "T_fue", "Pen", "N_ind", "T_par", "N_par")

levene_results1 <- lapply(variables1, function(var) {
  formula <- as.formula(paste(var, "~ Gru"))
  leveneTest(formula, data = datos_p)
})

names(levene_results1) <- variables1 
levene_results1

datos_p$Gru <- NULL  ## eliminar columna 
View(datos_p)

hum_f <- datos_p$H_fue
hum_d <- datos_p$H_den 
tem_d <- datos_p$T_den
tem_f <- datos_p$T_fue 
pen <- datos_p$Pen

t_pa<- datos_p$T_par
n_in <- datos_p$N_ind
n_pa <- datos_p$N_par

# Analisis exploratorio 

list(Humedad = t.test(datos_p$H_den, datos_p$H_fue), Temperatura = t.test(datos_p$T_den, datos_p$T_fue))
par(mfrow = c(1, 2))
boxplot(datos_p$H_den, datos_p$H_fue, col=c("skyblue", "orange"), main="Humedad", names=c("Dentro del parche", "Fuera del parche"))
boxplot(datos_p$T_den, datos_p$T_fue, col=c("blue", "red"), main="Temperatura", names=c("Dentro del parche", "Fuera del parche"))

pru_Whi <- wilcox.test(datos_p$T_den, datos_p$T_fue)
pru_Whi



##### R lineal múltiple tamaño de parches vs f. abióticos  #####
modelo_1 <- lm(t_pa ~ tem_d + tem_f + hum_d + hum_f + pen, data = datos_p)
summary(modelo_1)  
### Metodo stepwise
step(modelo_1, direction = "backward", test = "F")   

modelo_2 <- lm(t_pa ~ pen, data = datos_p)
summary(modelo_2)  


 # Plot
library(visreg)
visreg(fit = modelo_2, xvar = "pen", scale="response",
       partial = TRUE,
       gg = TRUE,
       labs(x = "Pendiente", 
            y = "Tamaño del parche") +
       theme_light() +            
       theme(axis.title = element_text(size = 12)))


v <- visreg(modelo_2, "pen", scale = "response", plot = FALSE)
plot(v, 
     gg = TRUE, 
     partial = TRUE,
     fill = list(fill = "lightgrey")) + 
  labs(x = "Pendiente", 
       y = "Tamaño del parche",
       title = "Efecto de la pendiente en el tamaño del parche") +
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank()
  )


# 1. Generar predicciones marginales
pred_pen <- ggpredict(modelo_2, terms = "pen")

# 2. Función optimizada con marco profesional y estilo limpio
plot_glm_completo <- function(preds, original_data, x_var, title, xtitle) {
  ggplot() +
    # Puntos observados con jitter
    geom_jitter(data = original_data, 
                aes_string(x = x_var, y = "t_pa"), 
                alpha = 0.3, size = 1.5, width = 0.2, color = "gray40") +
    # Cinta de error (IC 95%)
    geom_ribbon(data = preds, 
                aes(x = x, ymin = conf.low, ymax = conf.high), 
                fill = "#2c7fb8", alpha = 0.25) +
    # Línea de tendencia
    geom_line(data = preds, 
              aes(x = x, y = predicted), 
              color = "#2c7fb8", size = 1.2) +
    # Etiquetas y títulos
    labs(title = title, x = xtitle, y = "Tamaño del parche (m)") +
    # Estética profesional
    theme_minimal(base_size = 12) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.text.x = element_text(size = 12, color = "black"), 
      axis.text.y = element_text(size = 12, color = "black"), 
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"), 
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      axis.title = element_text(size = 13),
      axis.ticks = element_line(colour = "black")
    )
}

# 3. Crear el gráfico base
p1 <- plot_glm_completo(pred_pen, datos_p, "pen", "", "Pendiente (grad)")


# 4. Ajustar escalas y PROPORCIÓN (Aquí está el truco)
p1 <- p1 + 
  scale_x_continuous(
    breaks = seq(0, 25, by = 5), 
    expand = expansion(mult = c(0.02, 0.02)) # Pequeño margen para no pegar los puntos al borde
  ) + 
  scale_y_continuous(
    breaks = seq(0, 6, by = 1.5), 
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 6)) +
  # Fuerza al gráfico a tener una forma armónica (puedes probar con 0.5 o 0.6)
  theme(aspect.ratio = 0.7) 

# 5. Combinación final con limpieza de títulos
plot_final <- p1 + 
  labs(
    title = "Efecto de la pendiente sobre el tamaño del parche",
    caption = "Modelo de regresión lineal múltiple en la zona con patrones"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")
  )

# Visualizar
print(plot_final)



#### Modelo de Poisson en zona con patrones ####
M1 <- glm(n_in ~ hum_d+hum_f+tem_d+tem_f+pen, 
          data = datos_p, 
          family = poisson(link = "log")) 
summary(M1)
# Equivalente a R2 ( D2) es: 
1-(deviance(M1)/M1$null.deviance) 
pseudo_R <- (M1$null.deviance-M1$deviance)/M1$null.deviance*100
pseudo_R


### Metodo stepwise
step(M1, direction = "backward", test = "Chisq")

M2 <- glm(n_in ~ hum_d+hum_f+tem_d+pen, 
          data = datos_p, 
          family = poisson(link = "log")) 
summary(M2)
# Equivalente a R2 ( D2) es: 
1-(deviance(M2)/M2$null.deviance) 
pseudo_R_2 <- (M2$null.deviance-M2$deviance)/M2$null.deviance*100
pseudo_R_2



M3 <- glm(n_pa ~ hum_d+hum_f+tem_d+tem_f+pen, 
          data = datos_p, 
          family = poisson(link = "log")) 
summary(M3)

1-(deviance(M3)/M3$null.deviance) 
pseudo_R1 <- (M3$null.deviance-M3$deviance)/M3$null.deviance*100
pseudo_R1
### Metodo stepwise
step(M3, direction = "backward", test = "Chisq")

M4 <- glm(n_pa ~ hum_d + pen, 
          data = datos_p, 
          family = poisson(link = "log")) 
summary(M4)

1-(deviance(M4)/M4$null.deviance) 
pseudo_R1 <- (M4$null.deviance-M4$deviance)/M4$null.deviance*100
pseudo_R1



#Plot numero de individuos 

library(ggplot2)
library(ggeffects)
library(patchwork)

#  Generar predicciones marginales
# Usamos ggpredict para obtener la curva ajustada del GLM
pred_hum_d <- ggpredict(M2, terms = "hum_d")
pred_hum_f <- ggpredict(M2, terms = "hum_f")
pred_tem_d <- ggpredict(M2, terms = "tem_d")
pred_pen   <- ggpredict(M2, terms = "pen")

#  Función para incluir puntos observados
plot_glm_completo <- function(preds, original_data, x_var, title, xtitle) {
  ggplot() +
    # Puntos observados (usamos jitter para evitar solapamiento de conteos)
    geom_jitter(data = original_data, 
                aes_string(x = x_var, y = "n_in"), 
                alpha = 0.3, size = 1.5, width = 0.2, color = "gray40") +
    # Cinta de error (Intervalo de confianza 95%)
    geom_ribbon(data = preds, 
                aes(x = x, ymin = conf.low, ymax = conf.high), 
                fill = "#2c7fb8", alpha = 0.25) +
    # Línea de tendencia del modelo
    geom_line(data = preds, 
              aes(x = x, y = predicted), 
              color = "#2c7fb8", size = 1.2) +
    coord_cartesian(ylim = c(0,100)) +
    # Theme y etiquetas
    labs(title = title, x = xtitle, y = "N° Individuos") +
    theme_minimal(base_size = 12) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(size = 12, color = "black"), # Tamaño números eje X
      axis.text.y = element_text(size = 12, color = "black"), # Tamaño números eje Y
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"), # Cuadrícula muy sutil
      plot.title = element_text(face = "bold", size = 11),
      axis.title = element_text(size = 12),
      axis.ticks = element_line(colour = "black") # Añade pequeñas marcas en los ejes
    )
}

#  Crear los 4 gráficos individuales

p1 <- plot_glm_completo(pred_hum_d, datos_p, "hum_d", "HDP", "Humedad (%)")
p2 <- plot_glm_completo(pred_hum_f, datos_p, "hum_f", "HFP", "Humedad (%)")
p3 <- plot_glm_completo(pred_tem_d, datos_p, "tem_d", "TDP", "Temperatura (°C)")
p4 <- plot_glm_completo(pred_pen,   datos_p, "pen",   "", "Pendiente (grad)")

# Eliminar etiquetas del eje y
p2 <- p2 + theme(axis.title.y = element_blank())
p4 <- p4 + theme(axis.title.y = element_blank())

#Modificar la escala del eje X solo para p2
p4 <- p4 + scale_x_continuous(breaks = seq(0, 30, by = 5))

#  Combinación final con Patchwork
plot_final <- (p1 + p2) / (p3 + p4) + 
  plot_annotation(
    title = "Efecto de las variables abiótica sobre el número de inviduos",
    caption = "Modelo de Poisson en la zona con patrones: número de individuos",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Visualizar
print(plot_final)


## Grafico para el numero de parches vs variables abioticas 


#  Generar predicciones marginales 
pred_hum_d <- ggpredict(M4, terms = "hum_d")
pred_pen   <- ggpredict(M4, terms = "pen")

#  Función para incluir puntos observados
plot_glm_completo <- function(preds, original_data, x_var, title, xtitle) {
  ggplot() +
    # Puntos observados (usamos jitter para evitar solapamiento de conteos)
    geom_jitter(data = original_data, 
                aes_string(x = x_var, y = "n_pa"), 
                alpha = 0.3, size = 1.5, width = 0.2, color = "gray40") +
    # Cinta de error (Intervalo de confianza 95%)
    geom_ribbon(data = preds, 
                aes(x = x, ymin = conf.low, ymax = conf.high), 
                fill = "#2c7fb8", alpha = 0.25) +
    # Línea de tendencia del modelo
    geom_line(data = preds, 
              aes(x = x, y = predicted), 
              color = "#2c7fb8", size = 1.2) +
    coord_cartesian(ylim = c(0,15)) +
    # Theme y etiquetas
    labs(title = title, x = xtitle, y = "N° parches") +
    theme_minimal(base_size = 12) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(size = 12, color = "black"), # Tamaño números eje X
      axis.text.y = element_text(size = 12, color = "black"), # Tamaño números eje Y
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"), # Cuadrícula muy sutil
      plot.title = element_text(face = "bold", size = 12),
      axis.title = element_text(size = 13),
      axis.ticks = element_line(colour = "black") # Añade pequeñas marcas en los ejes
    )
}

#  Crear los gráficos individuales

p1 <- plot_glm_completo(pred_hum_d, datos_p, "hum_d", "HDP", "Humedad (%)")
p2 <- plot_glm_completo(pred_pen,   datos_p, "pen",   "", "Pendiente (grad)")

# Eliminar etiquetas del eje y
p2 <- p2 + theme(axis.title.y = element_blank())

#Modificar la escala del eje X solo para p2
p2 <- p2 + scale_x_continuous(breaks = seq(0, 30, by = 5))

#  Combinación final con Patchwork
plot_final <- p1 + p2 + 
  plot_annotation(
    title = "Efecto de las variables abiótica sobre el número de parches",
    caption = "Modelo de Poisson en la zona con patrones: número de parches",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Visualizar
print(plot_final)






###### Análisis en zona sin patrones ######
library(readxl)
library(dplyr)
library(ggplot2)  
library(lattice)
library(car)
library(visreg)

s_pa <- read.csv("sin_patrones.csv", header = TRUE, row.names = 1 )
str(s_pa)
View(s_pa)

# Test de normalidad 
columnas <- c("H_den", "H_fue", "T_den", "T_fue", "Pen", "N_ind", "T_par", "N_par")
shapiro_tests <- lapply(s_pa[columnas], shapiro.test)
shapiro_tests
 
p_values <- sapply(s_pa[columnas], function(x) shapiro.test(x)$p.value)
p_values


hum_f2 <- s_pa$H_fue
hum_d2 <- s_pa$H_den 
tem_d2 <- s_pa$T_den
tem_f2 <- s_pa$T_fue 
pen2 <- s_pa$Pen
N_pa<- s_pa$N_par
N_in <- s_pa$N_ind
T_pa <- s_pa$T_par

##  Analisis exploratorio ###
#T-student
list(humedad = t.test(hum_d2, hum_f2, data = s_pa), temperatura = t.test(tem_d2, tem_f2, data = s_pa))

# Prueba de Levene para homocedasticidad
s_pa$Gru <- as.factor(s_pa$Gru)

variables <- c("H_den", "H_fue", "T_den", "T_fue", "Pen", "N_ind", "T_par", "N_par")

levene_results <- lapply(variables, function(var) {
  formula <- as.formula(paste(var, "~ Gru"))
  leveneTest(formula, data = s_pa)
})

names(levene_results) <- variables 
levene_results

s_pa$Gru <- NULL
View(s_pa)

pru_Whi2 <- wilcox.test(s_pa$T_den, s_pa$T_fue)



### Regresión lineal múltiple tamaño de parches vs f. abióticos ###
modelo_3 <- lm(T_pa ~ tem_d2 + tem_f2 + hum_d2 + hum_f2 + pen2, data = s_pa)
summary(modelo_3)  
## Metodo stepwise
step(modelo_3, direction = "backward", test = "F") 
modelo_4 <- lm(T_pa ~ tem_d2 + tem_f2, data = s_pa)
summary(modelo_4) 

## PLot 


# tamaño de parche en zona sin patrones vs v abioticas
pred_tem_d2 <- ggpredict(modelo_4, terms = "tem_d2")
pred_tem_f2 <- ggpredict(modelo_4, terms = "tem_f2")

p1 <- ggplot(pred_tem_d2, aes(x = x, y = predicted)) +
  geom_line(color = "#2c3e50", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2c3e50") +
  labs(
    title = "TDP",
    x = "Temperatura dentro del parche",
    y = "Tamaño del parche"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 13), # Título eje X
    axis.title.y = element_text(size = 13), # Título eje Y
    axis.text.x = element_text(size = 13),    # Números eje X
    axis.text.y = element_text(size = 13)     # Números eje Y
  )

p2 <- ggplot(pred_tem_f2, aes(x = x, y = predicted)) +
  geom_line(color = "#e74c3c", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#e74c3c") +
  labs(
    title = "TDP",
    x = "Temperatura fuera del parche",
    y = "Tamaño del parche"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

grafico_final <- p1 + p2 + 
  plot_annotation(
    title = 'Análisis de Regresión Múltiple: Tamaño de Parche',
    subtitle = ' ',
    caption = ' Zona sin patrones ',
    
    theme = theme(plot.title = element_text(size = 18, face = "bold"))
  )

print(grafico_final)

## Plot

#  Generar predicciones marginales
pred_t_f  <- ggpredict(modelo_4, terms = "tem_f2")
pred_t_d  <- ggpredict(modelo_4, terms = "tem_d2")

#  Función 
plot_lm_pro <- function(preds, original_data, x_var, title, xtitle) {
  ggplot() +
    # Datos observados 
    geom_jitter(data = original_data, 
                aes_string(x = x_var, y = "T_pa"), 
                alpha = 0.35, size = 1.6, width = 0.2, color = "gray40") +
    # Intervalo de confianza 
    geom_ribbon(data = preds, 
                aes(x = x, ymin = conf.low, ymax = conf.high), 
                fill = "#87CEEB", alpha = 0.3) +
    # Línea de tendencia 
    geom_line(data = preds, 
              aes(x = x, y = predicted), 
              color = "#007BA7", size = 1.3) + 
    
    #CONFIGURACIÓN DE ESCALAS 
    coord_cartesian(ylim = c(0, 2)) + 
    # Eje Y
    scale_y_continuous(breaks = seq(0, 2, by = 0.5), expand = c(0, 0)) +
    # Eje X
    scale_x_continuous(breaks = seq(22, 36, by = 2), expand = c(0.02, 0.02)) +
    
    # Etiquetas y Estética
    labs(title = title, x = xtitle, y = "Tamaño del Parche (m)") +
    theme_bw(base_size = 12) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
      axis.text.x = element_text(size = 13, color = "black"), # Números eje X
      axis.text.y = element_text(size = 13, color = "black"), # Números eje Y
      axis.text = element_text(size = 13, color = "black"),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray96"),
      axis.ticks = element_line(colour = "black")
    )
}

# Crear los gráficos individuales
p1 <- plot_lm_pro(pred_t_f, s_pa, "tem_f2", "TFP ", "Temperatura (°C)")
p2 <- plot_lm_pro(pred_t_d, s_pa, "tem_d2", "TDP ", "Temperatura (°C)")

#  Eliminar etiqueta Y del segundo gráfico 
p2 <- p2 + theme(axis.title.y = element_blank())

#  Combinación final 
plot_2m <- (p1 | p2) + 
  plot_annotation(
    title = "Efecto de la temperatura sobre el tamaño de parche",
    caption = "Modelo de Regresión lineal múltiple | Zona sin patrones: número de individuos",
    theme = theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
  )

# Visualizar
print(plot_2m)


#### Modelo Poisson sin_patrones #### 

M6 <- glm(N_in ~ hum_d2 + hum_f2 + tem_d2 + tem_f2 + pen2, 
          data = s_pa, 
          family = poisson(link = "log"))
summary(M6)
#equivalente a R2 ( D2) es
1-(deviance(M6)/M6$null.deviance) 
pseudo_R2 <- (M6$null.deviance-M6$deviance)/M6$null.deviance*100
pseudo_R2

## Metodo stepwise
step(M6, direction = "backward", test = "Chisq")

M7 <- glm(N_in ~ hum_d2 + hum_f2 + tem_d2 + tem_f2 + pen2, 
          data = s_pa, 
          family = poisson(link = "log"))
summary(M7)
#equivalente a R2 ( D2) es
1-(deviance(M7)/M7$null.deviance) 
pseudo_R2 <- (M7$null.deviance-M7$deviance)/M7$null.deviance*100
pseudo_R2




M8 <- glm(N_pa ~ hum_d2 + hum_f2 + tem_d2 + tem_f2 + pen2, 
          data = s_pa, 
          family = poisson(link = "log"))
summary(M8)
## Metodo stepwise
step(M8, direction = "backward", test = "Chisq")

#equivalente a R2 ( D2) es
1-(deviance(M8)/M8$null.deviance) 
pseudo_R3 <- (M8$null.deviance-M8$deviance)/M8$null.deviance*100
pseudo_R3


M9 <- glm(N_pa ~ hum_d2 + hum_f2 + tem_d2, 
          data = s_pa, 
          family = poisson(link = "log"))
summary(M9)

#equivalente a R2 ( D2) es
1-(deviance(M9)/M9$null.deviance) 
pseudo_R4 <- (M9$null.deviance-M9$deviance)/M9$null.deviance*100
pseudo_R4


#Plot numero de individuos vs variable abioticas sin patrones
library(ggplot2)
library(ggeffects)
library(patchwork)

#  Generar las 5 predicciones marginales 
pred_h_d  <- ggpredict(M7, terms = "hum_d2")
pred_h_f  <- ggpredict(M7, terms = "hum_f2")
pred_t_d  <- ggpredict(M7, terms = "tem_d2")
pred_t_f  <- ggpredict(M7, terms = "tem_f2")
pred_pen  <- ggpredict(M7, terms = "pen2")

# Función optimizada para visualización profesional
plot_glm_5 <- function(preds, original_data, x_var, title, xtitle, color_hex = "#2c7fb8") {
  ggplot() +
    # Datos observados con jitter (transparencia para ver densidad)
    geom_jitter(data = original_data, 
                aes_string(x = x_var, y = "N_in"), 
                alpha = 0.25, size = 1.2, width = 0.2, color = "gray50") +
    # Intervalo de confianza
    geom_ribbon(data = preds, 
                aes(x = x, ymin = conf.low, ymax = conf.high), 
                fill = color_hex, alpha = 0.2) +
    # Línea de tendencia (Predicción del Modelo)
    geom_line(data = preds, 
              aes(x = x, y = predicted), 
              color = color_hex, size = 1.1) +
    scale_x_continuous(expand = expansion(mult = 0.05)) +  ## expandir 
    # AJUSTE DE ESCALA
    coord_cartesian(ylim = c(0, 100)) + 
    labs(title = title, x = xtitle, y = "N° Individuos") +
    theme_minimal(base_size = 12) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(size = 12, color = "black"), # Tamaño números eje X
      axis.text.y = element_text(size = 12, color = "black"), # Tamaño números eje Y
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"), # Cuadrícula muy sutil
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.ticks = element_line(colour = "black") # Añade pequeñas marcas en los ejes
    )
}

# Crear los 5 gráficos individuales
p1 <- plot_glm_5(pred_h_d, s_pa, "hum_d2", "HDP", "Humedad (%)", "#87CEEB")
p2 <- plot_glm_5(pred_h_f, s_pa, "hum_f2", "HFP", "Humedad (%)", "#87CEEB")
p3 <- plot_glm_5(pred_t_d, s_pa, "tem_d2", "TDP", "Temperatura (°C)", "#87CEEB")
p4 <- plot_glm_5(pred_t_f, s_pa, "tem_f2", "TFP", "Temperatura (°C)", "#87CEEB")
p5 <- plot_glm_5(pred_pen, s_pa, "pen2",   " ",  "Pendiente (grad)",  "#87CEEB")

#  Ajuste específico 
p4 <- p4 + scale_x_continuous(expand = expansion(mult = c(0.1, 0.05)))

## eliminar etiquetas de p2, p3 y p5
p2 <- p2 + theme(axis.title.y = element_blank())
p3 <- p3 + theme(axis.title.y = element_blank())
p5 <- p5 + theme(axis.title.y = element_blank())


#  Combinación final con Patchwork 
# plot_spacer() llena el hueco vacío para mantener la simetría
plot_m7 <- (p1 + p2 + p3) / (p4 + p5 + plot_spacer()) + 
  plot_annotation(
    title = "Efecto de variables abióticas sobre el número de individuos",
    subtitle = " ",
    caption = " Modelo de Poisson en la zona sin patrones",
    theme = theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
  )


print(plot_m7)


library(ggplot2)
library(ggeffects)
library(patchwork)

# Predicciones
pred_h_d  <- ggpredict(M9, terms = "hum_d2")
pred_h_f  <- ggpredict(M9, terms = "hum_f2")
pred_t_d  <- ggpredict(M9, terms = "tem_d2")

plot_glm_m9 <- function(preds, original_data, x_var, title, xtitle) {
  ggplot() +
    # Puntos observados
    geom_jitter(data = original_data, 
                aes_string(x = x_var, y = "N_pa"), 
                alpha = 0.3, size = 1.5, width = 0.2, color = "gray40") +
    # Cinta de error (Intervalo de confianza 95%) 
    geom_ribbon(data = preds, 
                aes(x = x, ymin = conf.low, ymax = conf.high), 
                fill = "#87CEEB", alpha = 0.3) +
    # Línea de tendencia 
    geom_line(data = preds, 
              aes(x = x, y = predicted), 
              color = "#87CEEB", size = 1.2) +
    # AJUSTE DE ESCALA
    coord_cartesian(ylim = c(0, 20)) + 
    # Theme y etiquetas
    labs(title = title, x = xtitle, y = "N° de Parches") +
    theme_minimal(base_size = 12) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(size = 12, color = "black"), # Tamaño números eje X
      axis.text.y = element_text(size = 12, color = "black"), # Tamaño números eje Y
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"), # Cuadrícula muy sutil
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.ticks = element_line(colour = "black") # Añade pequeñas marcas en los ejes
    )
}

# Crear los 3 gráficos individuales
p1 <- plot_glm_m9(pred_h_d, s_pa, "hum_d2", "HDP", "Humedad (%)")
p2 <- plot_glm_m9(pred_h_f, s_pa, "hum_f2", "HFP", "Humedad (%)")
p3 <- plot_glm_m9(pred_t_d, s_pa, "tem_d2", "TDP", "Temperatura (°C)")

## eliminar etiquetas de p2
p2 <- p2 + theme(axis.title.y = element_blank())

# Combinación final con Patchwork
plot_m9 <- (p1 | p2) / (p3 + plot_spacer()) + 
  plot_annotation(
    title = "Efecto de las variables abióticas sobre el número de parches",
    caption = "Modelo de Poisson M9 | Zona sin patrones",
    theme = theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
  )

# Visualizar
print(plot_m9)


####### Regresión logística #######

total_pa <- read.csv("total_parcelas.csv", header = TRUE, row.names = 1)
str(total_pa)
View(total_pa)

table(total_pa$Pre_aus)
total_pa$PA <- ifelse(total_pa$Pre_aus > 0, 1, 0)
table(total_pa$PA)
pre_au <- total_pa$Pre_aus
hum_f <- total_pa$H_fue
hum_d <- total_pa$H_den 

tem_d <- total_pa$T_den
tem_f <- total_pa$T_fue 

pen <- total_pa$Pen
N_pa <- total_pa$N_par
T_pa <- total_pa$T_par
N_in <- total_pa$N_ind


M10 <- glm(total_pa$Pre_aus ~ tem_d + tem_f+ hum_d + hum_f+ pen, 
              data = total_pa, family = binomial)
summary(M10)
# Equivalente a R2 ( D2) es
1-(deviance(M10)/M10$null.deviance) 
pseudo_R5 <- (M10$null.deviance-M10$deviance)/M10$null.deviance*100
pseudo_R5
## Stepwise
step(M10, direction = "backward", test = "Chisq")


M11 <- glm(total_pa$Pre_aus ~ tem_d + tem_f+ hum_d + pen, 
          data = total_pa, family = binomial)
summary(M11)
# Equivalente a R2 ( D2) es
1-(deviance(M11)/M11$null.deviance) 
pseudo_R6 <- (M11$null.deviance-M11$deviance)/M11$null.deviance*100
pseudo_R6

#Graph

library(ggplot2)
library(visreg)
library(patchwork)




# Funcion 
plot_logistico_4 <- function(fit_obj, x_var_name, xtitle, ytitle = "") {
  visreg(fit_obj, x_var_name, scale = "response", gg = TRUE, 
         line = list(col = "#007BA7"), 
         fill = list(fill = "#E0F2F7")) +
    # Rug plot: Visualiza los puntos observados 
    geom_rug(aes_string(x = x_var_name), sides = "tb", alpha = 0.4, color = "black") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    labs(x = xtitle, y = ytitle) +
    theme_bw() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 12, color = "black"),
      panel.grid = element_blank()
    )
}

# Crear los 4 gráficos individuales

p1 <- plot_logistico_4(M11, "tem_d", "TDP (°C)", "Prob. de Presencia")
p2 <- plot_logistico_4(M11, "tem_f", "TFP (°C)", "")
p3 <- plot_logistico_4(M11, "hum_d", "HDP (%)", "Prob. de Presencia")
p4 <- plot_logistico_4(M11, "pen",   "Pendiente (grad)", "")

# Combinación final en cuadrícula 2x2
plot_m11 <- (p1 + p2) / (p3 + p4) + 
  plot_annotation(
    title = "Probabilidad de Presencia vs. Variables Ambientales",
    subtitle = "Modelos de Regresión Logística",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

# Visualizar
print(plot_m11)



# Gráfico T dentro del parche vs presencia ausencia 
visreg(fit = M5 , xvar = "tem_d", scale="response",
       ylab = "Probabilidad de presencia o ausencia de patrones",
       xlab = "Temperatura (°C) dentro del tillandsial",
       cex.lab = 0.9)

library(ggplot2)
visreg(fit = M5, 
       xvar = "tem_d", 
       scale = "response", 
       gg = TRUE) +  
  labs(x = "Temperatura (°C) dentro del tillandsial", 
       y = "Probabilidad de presencia o ausencia de patrones") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Gráfico T fuera del parche vs presencia ausencia 
visreg(fit = M5, 
       xvar = "tem_f", 
       scale = "response", 
       gg = TRUE) +  
  labs(x = "Temperatura (°C) fuera del tillandsial", 
       y = "Probabilidad de presencia o ausencia de patrones") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Gráfico pendiente vs presencia ausencia 
visreg(fit = M5, 
       xvar = "pen", 
       scale = "response", 
       gg = TRUE) +  
  labs(x = "Pendiente", 
       y = "Probabilidad de presencia o ausencia de patrones") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Gráfico humedad dentro vs presencia ausencia 
visreg(fit = M5, 
       xvar = "hum_d", 
       scale = "response", 
       gg = TRUE) +  
  labs(x = "Humedad dentro del tillandsial", 
       y = "Probabilidad de presencia o ausencia de patrones") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# Gráfico humedad fuera vs presencia ausencia 
visreg(fit = M5, 
       xvar = "hum_f", 
       scale = "response", 
       gg = TRUE) +  
  labs(x = "Humedad fuera del tillandsial", 
       y = "Probabilidad de presencia o ausencia de patrones") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
### fin
#######


### hshdllddlñsñdjsnsfsfjj
####Jack
