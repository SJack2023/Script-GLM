# -*- Coding: utf8 -*-
# Author: Jack Barboza

#"""
# Language: R script
# This a temporary script file
#"""


#1. Libraries. Esta es una propuesta de cambios
library(readxl)
library(dplyr)
library(ggplot2)  
library(lattice)
library(GGally)
library(car)
library(gridExtra)
library(stats)
library(visreg)

#2. Cargar datos   
bandas <- read.csv("datos_p.csv", header = TRUE, row.names = 1)
manchas <- read.csv("sin_patrones.csv", header = TRUE, row.names = 1)

#3.Media y desviación estándar
#3.1 Para bandas
medias_b <- sapply(bandas[c("V_te", "V_hu", "Pen", "N_ind", "T_par", "N_par")], mean)
ds_b <- sapply(bandas[c("V_te", "V_hu", "Pen", "N_ind", "T_par", "N_par")], sd)
#3.2 Para manchas
medias_m <- sapply(manchas[c("V_te", "V_hu", "Pen", "N_ind", "T_par", "N_par")], mean)
ds_m <- sapply(manchas[c("V_te", "V_hu", "Pen", "N_ind", "T_par", "N_par")], sd)

#4 Comparacion de la temperatura y humedad
#4.1 Para bandas
# Normalidad
var_p <- c("V_te", "V_hu")
shapiro_tests1 <- lapply(bandas[var_p], shapiro.test)
shapiro_tests1
# Prueba de Wilcoxon pareado 
twip <- wilcox.test(bandas$T_den, bandas$T_fue, paired = TRUE)
hwip <- wilcox.test(bandas$H_den, bandas$H_fue, paired = TRUE)


# 4.2 Para manchas
#Normalidad
var_s <- c("V_te","V_hu")
shapiro_tests1 <- lapply(manchas[var_s], shapiro.test)
shapiro_tests1

# Prueba de Wilcoxon pareado 
tWis <- wilcox.test(manchas$T_den, manchas$T_fue, paired = TRUE)
hwis <- wilcox.test(manchas$H_den, manchas$H_fue, paired = TRUE)


#5. Intervalos de confianza
#5.1 Error estandar
n_plots <- length(manchas$T_par)

SEs_bandas <- ds_b/sqrt(n_plots)
CIs_bandas <- qt(.975, df=n_plots - 1) * SEs_bandas

SEs_manchas <- ds_m/sqrt(n_plots)
CIs_manchas <- qt(.975, df=n_plots - 1) * SEs_manchas


#5.2 Graficos de los intervalos de confianza
vars <- c("Variación temperatura", "Variación humedad", "Pendiente",
          "Número de individuos", "Longitud parche", "Número de parches")

df_plot <- data.frame(
  variable = rep(vars, 2),
  media = c(medias_b, medias_m),
  CI = c(CIs_bandas, CIs_manchas),
  grupo = rep(c("Bandas", "Manchas"), each = length(vars))
)

df_plot$grupo <- factor(df_plot$grupo, levels = c("Bandas", "Manchas"))

ggplot(df_plot, aes(x = grupo, y = media, color = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = media - CI, ymax = media + CI),
                width = 0.2) +
  facet_wrap(~variable,
             scales = "free_y", strip.position = "left",
             labeller = labeller(variable = vars)) +  
  labs(color = "Tipo de patrón", x = NULL, y = NULL)+
  scale_color_manual(values = c("Bandas" = "blue", "Manchas" = "red"))+
  scale_y_continuous(n.breaks = 5) +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  theme(axis.text.x = element_blank(),
        strip.placement = "outside",        
        strip.text = element_text(hjust = 0.5, face = "bold", vjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

#6. Permanova
library(vegan)
data_total <- rbind(bandas[5:10], manchas[5:10])
data_total$Gru <- rep(c("Bandas", "Manchas"), each = n_plots)
data_varnames <- colnames(data_total)
scale_data <- scale(data_total[, data_varnames[-7]])

adonis2(scale_data ~ Gru, data = data_total, permutations = 999, method = "euclidean")

#7. PCA
pca_data <- prcomp(scale_data, center = TRUE)
summary(pca_data)

#7.1 PCA plot
pca_scores <- as.data.frame(pca_data$x)
pca_scores$Gru <- data_total$Gru

ggplot(pca_scores, aes(x = PC1, y = PC2, color = Gru))+
  geom_point(size = 3)+
  stat_ellipse(level = 0.95) +
  labs(x = "PC1", y = "PC2", color = "Tipo de patrón")+
  coord_cartesian(xlim = c(-1.7*max(abs(pca_scores$PC1)), 1.7*max(abs(pca_scores$PC1))),
                  ylim = c(-1.5*max(abs(pca_scores$PC2)), 1.5*max(abs(pca_scores$PC2))))+
  theme_minimal()+
  scale_color_manual(values = c("Bandas"="blue", "Manchas"="red"))+
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

#7.2 Biplot
pca_loads <- as.data.frame(pca_data$rotation)
pca_loads$var <- c("VT", "VH", "P",
                   "NI", "LP", "NP")

ggplot(pca_scores, aes(x = PC1, y = PC2, color = Gru))+
  geom_point(size = 3)+
  stat_ellipse(level = 0.95) +
  geom_segment(data = pca_loads,
               aes(x = 0, y = 0, xend = PC1*2.5, yend = PC2*2.5),
               arrow = arrow(length = unit(.2, "cm")),
               color = "black",
               inherit.aes = FALSE) +
  geom_text(data = pca_loads,
            aes(x = PC1*2, y = PC2*3, label=var),
            color = "black",
            size = 3,
            inherit.aes = FALSE)+
  labs(x = "PC1", y = "PC2", color = "Tipo de patrón")+
  theme_minimal()+
  scale_color_manual(values = c("Bandas"="blue", "Manchas"="red"))+
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

#simper(scale_data, data_total$Gru)

## Regresion logistica
std_data <- as.data.frame(scale_data)
std_data$Gru <- factor(data_total$Gru)

log_model <- glm(Gru ~ V_te + V_hu + Pen,
                 data = std_data,
                 family = binomial)
summary(log_model)

## Logistic Regressión Plot
newdata <- data.frame(
  V_te = seq(min(std_data$V_te), max(std_data$V_te), length.out = 200),
  V_hu = 0,
  Pen = 0)

predicts <- predict(log_model, newdata = newdata, type = "link", se.fit = TRUE)

fit_link <- predicts$fit
se_fit <- predicts$se.fit

CI_lower <- fit_link - 1.96 * se_fit
CI_upper <- fit_link + 1.96 * se_fit

newdata$probs <- plogis(fit_link)
newdata$CI_lower <- plogis(CI_lower)
newdata$CI_upper <- plogis(CI_upper)

ggplot(std_data, aes(x = V_te, y = as.numeric(Gru)-1)) +
  geom_point(aes(color = Gru),
             position = position_jitter(height = .01)) +
  geom_ribbon(data = newdata,
              aes(x = V_te, ymin = CI_lower, ymax = CI_upper),
              alpha = .2,
              fill = "gray50",
              inherit.aes = FALSE) +
  geom_line(data = newdata,
            aes(x = V_te, y = probs),
            color = "black",
            linewidth = 1) +
  labs(x = "Variación temperatura (estandarizada)",
       y = "Probabilidad",
       color = "Tipo de patrón") +
  theme_minimal() +
  scale_color_manual(values = c("Bandas" = "blue", "Manchas" = "red"))+
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))












hum <- datos_p$V_hu 
tem <- datos_p$V_te
pen <- datos_p$Pen
hu_d <- datos_p$H_den
hu_f <- datos_p$H_fue
te_f <- datos_p$T_fue
te_d <- datos_p$T_den

t_pa<- datos_p$T_par
n_in <- datos_p$N_ind
n_pa <- datos_p$N_par





# Media y desviación estándar. Irbin
data_spotted <- read.csv("datos_p.csv", header = TRUE, row.names = 1)
data_striped <- read.csv("sin_patrones.csv", header = TRUE, row.names = 1)

n_plots <- length(data_spotted$Gru)
means_spotted <- sapply(data_spotted[c("V_te", "V_hu", "Pen", "N_ind", "T_par", "N_par")], mean)
stdes_spotted <- sapply(data_spotted[c("V_te", "V_hu", "Pen", "N_ind", "T_par", "N_par")], sd)

means_striped <- sapply(data_striped[c("V_te", "V_hu", "Pen", "N_ind", "T_par", "N_par")], mean)
stdes_striped <- sapply(data_striped[c("V_te", "V_hu", "Pen", "N_ind", "T_par", "N_par")], sd)

# Intervalos de confianza
SEs_spotted <- stdes_spotted/sqrt(n_plots)
CIs_spotted <- qt(.975, df=n_plots - 1) * SEs_spotted

SEs_striped <- stdes_striped/sqrt(n_plots)
CIs_striped <- qt(.975, df=n_plots - 1) * SEs_striped

# Graficos
vars <- c("Variación temperatura", "Variación humedad", "Pendiente",
          "Número de individuos", "Longitud parche", "Número de parches")

df_plot <- data.frame(
  variable = rep(vars, 2),
  media = c(means_spotted, means_striped),
  CI = c(CIs_spotted, CIs_striped),
  grupo = rep(c("Manchas", "Bandas"), each = length(vars))
)

df_plot$grupo <- factor(df_plot$grupo, levels = c("Manchas", "Bandas"))

ggplot(df_plot, aes(x = grupo, y = media, color = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = media - CI, ymax = media + CI),
                width = 0.2) +
  facet_wrap(~variable,
             scales = "free_y", strip.position = "left",
             labeller = labeller(variable = vars)) +  
  labs(color = "Tipo de patrón", x = NULL, y = NULL)+
  scale_color_manual(values = c("Manchas" = "blue", "Bandas" = "red"))+
  scale_y_continuous(n.breaks = 5) +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  theme(axis.text.x = element_blank(),
        strip.placement = "outside",        
        strip.text = element_text(hjust = 0.5, face = "bold", vjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))


## Permanova
library(vegan)
data_total <- rbind(data_spotted, data_striped)
data_total$Gru <- rep(c("Manchas", "Bandas"), each = n_plots)
data_varnames <- colnames(data_total)
scale_data <- scale(data_total[, data_varnames[-1]])

adonis2(scale_data ~ Gru, data = data_total, permutations = 999, method = "euclidean")


## PCA
pca_data <- prcomp(scale_data, center = TRUE)
summary(pca_data)

## PCA plot
pca_scores <- as.data.frame(pca_data$x)
pca_scores$Gru <- data_total$Gru

ggplot(pca_scores, aes(x = PC1, y = PC2, color = Gru))+
  geom_point(size = 3)+
  stat_ellipse(level = 0.95) +
  labs(x = "PC1", y = "PC2", color = "Tipo de patrón")+
  coord_cartesian(xlim = c(-1.7*max(abs(pca_scores$PC1)), 1.7*max(abs(pca_scores$PC1))),
                  ylim = c(-1.5*max(abs(pca_scores$PC2)), 1.5*max(abs(pca_scores$PC2))))+
  theme_minimal()+
  scale_color_manual(values = c("Manchas"="blue", "Bandas"="red"))+
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

## Biplot
pca_loads <- as.data.frame(pca_data$rotation)
pca_loads$var <- c("VT", "VH", "P",
          "NI", "LP", "NP")

ggplot(pca_scores, aes(x = PC1, y = PC2, color = Gru))+
  geom_point(size = 3)+
  stat_ellipse(level = 0.95) +
  geom_segment(data = pca_loads,
               aes(x = 0, y = 0, xend = PC1*2.5, yend = PC2*2.5),
               arrow = arrow(length = unit(.2, "cm")),
               color = "black",
               inherit.aes = FALSE) +
  geom_text(data = pca_loads,
            aes(x = PC1*2, y = PC2*3, label=var),
            color = "black",
            size = 3,
            inherit.aes = FALSE)+
  labs(x = "PC1", y = "PC2", color = "Tipo de patrón")+
  theme_minimal()+
  scale_color_manual(values = c("Manchas"="blue", "Bandas"="red"))+
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

simper(scale_data, data_total$Gru)

## Regresion logistica
std_data <- as.data.frame(scale_data)
std_data$Gru <- factor(data_total$Gru)

log_model <- glm(Gru ~ V_te + V_hu + Pen,
                 data = std_data,
                 family = binomial)
summary(log_model)

## Logistic Regressión Plot
newdata <- data.frame(
  V_te = seq(min(std_data$V_te), max(std_data$V_te), length.out = 200),
  V_hu = 0,
  Pen = 0)

predicts <- predict(log_model, newdata = newdata, type = "link", se.fit = TRUE)

fit_link <- predicts$fit
se_fit <- predicts$se.fit

CI_lower <- fit_link - 1.96 * se_fit
CI_upper <- fit_link + 1.96 * se_fit

newdata$probs <- plogis(fit_link)
newdata$CI_lower <- plogis(CI_lower)
newdata$CI_upper <- plogis(CI_upper)

ggplot(std_data, aes(x = V_te, y = as.numeric(Gru)-1)) +
  geom_point(aes(color = Gru),
             position = position_jitter(height = .01)) +
  geom_ribbon(data = newdata,
              aes(x = V_te, ymin = CI_lower, ymax = CI_upper),
              alpha = .2,
              fill = "gray50",
              inherit.aes = FALSE) +
  geom_line(data = newdata,
            aes(x = V_te, y = probs),
            color = "black",
            linewidth = 1) +
  labs(x = "Variación temperatura (estandarizada)",
       y = "Probabilidad",
       color = "Tipo de patrón") +
  theme_minimal() +
  scale_color_manual(values = c("Manchas" = "blue", "Bandas" = "red"))+
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))



##### R lineal múltiple tamaño de parches vs f. abióticos  #####
modelo_1 <- lm(t_pa ~ tem + hum + pen, data = datos_p)
summary(modelo_1)  
### Metodo stepwise
step(modelo_1, direction = "backward", test = "F")   

modelo_2 <- lm(t_pa ~ pen, data = datos_p)
summary(modelo_2)  

 # Plot
library(visreg)
v <- visreg(modelo_2, "hum_d", scale = "response", plot = FALSE)
plot(v, 
     gg = TRUE, 
     partial = TRUE,
     fill = list(fill = "lightgrey")) + 
  labs(x = "HDP", 
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

library(ggplot2)
library(ggeffects)
library(patchwork)

# 1. Generar predicciones marginales
pred_pen <- ggpredict(modelo_2, terms = "pen")


# 2. Función optimizada (Sin errores de duplicación)
plot_glm_completo <- function(preds, original_data, x_var, xtitle) {
  ggplot() +
    # Puntos observados
    geom_jitter(data = original_data, 
                aes(x = .data[[x_var]], y = t_pa), 
                alpha = 0.2, size = 1.2, width = 0.2, color = "gray50") +
    # Intervalo de confianza
    geom_ribbon(data = preds, 
                aes(x = x, ymin = conf.low, ymax = conf.high), 
                fill = "#2c7fb8", alpha = 0.2) +
    # Línea de tendencia
    geom_line(data = preds, 
              aes(x = x, y = predicted), 
              color = "#2c7fb8", linewidth = 1) +
    labs(x = xtitle, y = "Tamaño del parche (m)") +
    # Usamos theme_bw o theme_minimal 
    theme_minimal() + 
    theme(
      # Bordes y líneas
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray96"),
      axis.ticks = element_line(colour = "black"),
    
      axis.title = element_text(size = 16, face = "bold", color = "black"), # Títulos ejes
      axis.text = element_text(size = 13, color = "black"),                # Números ejes
      strip.text = element_text(size = 14, face = "bold")                  # Por si usas facets
    )
}

# 3. Crear los gráficos 
p1 <- plot_glm_completo(pred_pen, datos_p, "Pen", "Pendiente (grad)") + 
  coord_cartesian(ylim = c(0, 7.5)) 

# 4. Combinación final
plot_final <- p1 + 
  plot_annotation(
    title = " ",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    )
  )

print(plot_final)




#### Modelo de Poisson en zona con patrones ####
M1 <- glm(n_in ~ hum + tem +pen, 
          data = datos_p, 
          family = poisson(link = "log")) 
summary(M1)
# Equivalente a R2 ( D2) es: 
1-(deviance(M1)/M1$null.deviance) 
pseudo_R <- (M1$null.deviance-M1$deviance)/M1$null.deviance*100
pseudo_R


### Metodo stepwise
step(M1, direction = "backward", test = "Chisq")

M2 <- glm(n_in ~ hum_f, 
          data = datos_p, 
          family = poisson(link = "log")) 
summary(M2)
# Equivalente a R2 ( D2) es: 
1-(deviance(M2)/M2$null.deviance) 
pseudo_R_2 <- (M2$null.deviance-M2$deviance)/M2$null.deviance*100
pseudo_R_2
?deviance 


M3 <- glm(n_pa ~ hum_d+hum_f+tem_d+tem_f+pen, 
          data = datos_p, 
          family = poisson(link = "log")) 
summary(M3)

1-(deviance(M3)/M3$null.deviance) 
pseudo_R1 <- (M3$null.deviance-M3$deviance)/M3$null.deviance*100
pseudo_R1
### Metodo stepwise
step(M3, direction = "backward", test = "Chisq")

M4 <- glm(n_pa ~ tem_d, 
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

pred_hum_f <- ggpredict(M2, terms = "hum_f")


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
    coord_cartesian(ylim = c(0,75)) +
    # Theme y etiquetas
    labs(title = title, x = xtitle, y = "N° Individuos") +
    theme_minimal(base_size = 16) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(size = 16, color = "black"), # Tamaño números eje X
      axis.text.y = element_text(size = 16, color = "black"), # Tamaño números eje Y
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"), # Cuadrícula muy sutil
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(size = 16),
      axis.ticks = element_line(colour = "black") # Añade pequeñas marcas en los ejes
    )
}

#  Crear los  gráficos individuales

p1 <- plot_glm_completo(pred_hum_f, datos_p, "hum_f", " ", "Humedad fuera del parche (%)")



#  Combinación final con Patchwork
plot_final <- p1  
  plot_annotation(
    title = " ",
    caption = " ",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Visualizar
print(plot_final)


## Grafico para el numero de parches vs variables abioticas 


#  Generar predicciones marginales 
pred_te_d <- ggpredict(M4, terms = "tem_d")


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
    theme_minimal(base_size = 16) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(size = 16, color = "black"), # Tamaño números eje X
      axis.text.y = element_text(size = 16, color = "black"), # Tamaño números eje Y
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"), # Cuadrícula muy sutil
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(size = 16),
      axis.ticks = element_line(colour = "black") # Añade pequeñas marcas en los ejes
    )
}

#  Crear los gráficos individuales

p1 <- plot_glm_completo(pred_te_d, datos_p, "tem_d", " ", "Temperatura dentro del parche (°C)")

# Eliminar etiquetas del eje y
#p2 <- p2 + theme(axis.title.y = element_blank())

#Modificar la escala del eje X solo para p2
#p2 <- p2 + scale_x_continuous(breaks = seq(0, 30, by = 5))

#  Combinación final con Patchwork
plot_final <- p1  
  plot_annotation(
    title = " ",
    caption = " ",
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

hum2 <- s_pa$V_hu
tem2 <- s_pa$V_te
pen2 <- s_pa$Pen
N_pa<- s_pa$N_par
N_in <- s_pa$N_ind
T_pa <- s_pa$T_par


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

pru_Whi2 <- wilcox.test(s_pa$T_den, s_pa$T_fue, exact = FALSE)
pru_Whi2

#### T-test pareado 
t_test3 <- t.test(s_pa$T_den, s_pa$T_fue,
                  paired = TRUE,
                  conf.level = 0.95)
print(t_test3)
t_test4 <- t.test(s_pa$H_den, s_pa$H_fue,
                  paired = TRUE,
                  conf.level = 0.95)
print(t_test4)


### Regresión lineal múltiple tamaño de parches vs f. abióticos ###
modelo_3 <- lm(T_pa ~ tem2 + hum2 + pen2, data = s_pa)
summary(modelo_3)  
## Metodo stepwise
step(modelo_3, direction = "backward", test = "F") 
modelo_4 <- lm(T_pa ~ hum2, data = s_pa)
summary(modelo_4) 

## PLot 

library(patchwork)
# tamaño de parche en zona sin patrones vs v abioticas
pred_hum_2 <- ggpredict(modelo_4, terms = "hum2")

p1 <- ggplot(pred_hum_2, aes(x = x, y = predicted)) +
  geom_line(color = "#2c3e50", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2c3e50") +
  labs(
    title = " ",
    x = " Variacion de la humedad ",
    y = "Tamaño del parche"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 13), # Título eje X
    axis.title.y = element_text(size = 13), # Título eje Y
    axis.text.x = element_text(size = 13),    # Números eje X
    axis.text.y = element_text(size = 13)     # Números eje Y
  )

grafico_final <- p1 
  plot_annotation(
    title = ' ',
    subtitle = ' ',
    caption = '  ',
    
    theme = theme(plot.title = element_text(size = 18, face = "bold"))
  )

print(grafico_final)

## Plot  modelo final
# 1. Generar predicciones 
pred_h_2  <- ggpredict(modelo_4, terms = "hum2")

plot_lm <- function(preds, original_data, x_var, title, xtitle) {
  ggplot() +
    # Datos observados (puntos)
    geom_jitter(data = original_data, 
                aes_string(x = x_var, y = "T_pa"), 
                alpha = 0.4, size = 1.8, width = 0.1, color = "gray40") +
    # Intervalo de confianza 
    geom_ribbon(data = preds, 
                aes(x = x, ymin = conf.low, ymax = conf.high), 
                fill = "#87CEEB", alpha = 0.2) +
    # Línea de tendencia
    geom_line(data = preds, 
              aes(x = x, y = predicted), 
              color = "#007BA7", linewidth = 1.2) +
    
    # ESCALA UNIFICADA
    coord_cartesian(ylim = c(0, 3)) + 
    scale_y_continuous(breaks = seq(0, 3, by = 0.5), expand = c(0, 0)) +
    
    labs(title = title, x = xtitle, y = "Tamaño del Parche (m)") +
    theme_bw(base_size = 14) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2),
      axis.text = element_text(size = 13, color = "black"),
      axis.title = element_text(size = 15, face = "bold"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray96"),
      axis.ticks = element_line(colour = "black")
    )
}

#  Crear gráficos individuales
p1 <- plot_lm(pred_h_2, s_pa, "hum2", " ", "Variación de humedad")

# Combinación Final 
plot_final <- p1 +  
  plot_layout(heights = c(1, 1)) + # Filas de igual altura
  plot_annotation(
    title = " ",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  )

print(plot_final)



#### Modelo Poisson sin_patrones #### 

M6 <- glm(N_in ~ hum2 + tem2 + pen2, 
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


M9 <- glm(N_pa ~ pen2 + tem_f2, 
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

# Función optimizada 
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
    theme_minimal(base_size = 16) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(size = 16, color = "black"), # Tamaño números eje X
      axis.text.y = element_text(size = 16, color = "black"), # Tamaño números eje Y
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"), # Cuadrícula muy sutil
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(size = 16),
      axis.ticks = element_line(colour = "black") # Añade pequeñas marcas en los ejes
    )
}

# Crear los 5 gráficos individuales
p1 <- plot_glm_5(pred_h_d, s_pa, "hum_d2", " ", "Humedad dentro del parche(%)", "#87CEEB")
p2 <- plot_glm_5(pred_h_f, s_pa, "hum_f2", " ", "Humedad fuera del parche(%)", "#87CEEB")
p3 <- plot_glm_5(pred_t_d, s_pa, "tem_d2", " ", "Temperatura dentro del parche(°C)", "#87CEEB")
p4 <- plot_glm_5(pred_t_f, s_pa, "tem_f2", " ", "Temperatura fuera del parche (°C)", "#87CEEB")
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
    title = " ",
    subtitle = " ",
    caption = " ",
    theme = theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
  )


print(plot_m7)


library(ggplot2)
library(ggeffects)
library(patchwork)

# Predicciones
pred_t_f  <- ggpredict(M9, terms = "tem_f2")
pred_pen_2 <- ggpredict(M9, terms = "pen2")



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
    coord_cartesian(ylim = c(0, 15)) + 
    # Theme y etiquetas
    labs(title = title, x = xtitle, y = "N° de Parches") +
    theme_minimal(base_size = 16) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(size = 16, color = "black"), # Tamaño números eje X
      axis.text.y = element_text(size = 16, color = "black"), # Tamaño números eje Y
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"), # Cuadrícula muy sutil
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(size = 16),
      axis.ticks = element_line(colour = "black") # Añade pequeñas marcas en los ejes
    )
}

# Crear los gráficos individuales
p1 <- plot_glm_m9(pred_t_f, s_pa, "tem_f2", " ", "Temperatura fuera del parche(°C)")
p2 <- plot_glm_m9(pred_pen_2, s_pa, "pen2", " ", " Pendiente")


## eliminar etiquetas de p2
p2 <- p2 + theme(axis.title.y = element_blank())

# Combinación final con Patchwork
plot_m9 <- (p1 | p2) + 
  plot_annotation(
    title = "",
    caption = " ",
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
hum <- total_pa$V_hu
tem <- total_pa$V_te
pen <- total_pa$Pen
N_pa <- total_pa$N_par
T_pa <- total_pa$T_par
N_in <- total_pa$N_ind


M10 <- glm(total_pa$Pre_aus ~ tem+ hum+ pen, 
              data = total_pa, family = binomial)
summary(M10)
# Equivalente a R2 ( D2) es
1-(deviance(M10)/M10$null.deviance) 
pseudo_R5 <- (M10$null.deviance-M10$deviance)/M10$null.deviance*100
pseudo_R5
## Stepwise
step(M10, direction = "backward", test = "Chisq")


M11 <- glm(total_pa$Pre_aus ~ tem, 
          data = total_pa, family = binomial)
summary(M11)

# Equivalente a R2 ( D2) es
1-(deviance(M11)/M11$null.deviance) 
pseudo_R6 <- (M11$null.deviance-M11$deviance)/M11$null.deviance*100
pseudo_R6


M12 <- glm(total_pa$Pre_aus ~ tem+ hum+ pen + N_in +N_pa +T_pa, 
           data = total_pa, family = binomial)
summary(M12)


#Graph
library(ggplot2)
library(visreg)
library(patchwork)


plot_logistico <- function(fit_obj, x_var_name, y_var_name, data, xtitle, ytitle = "") {
  
  visreg(fit_obj, x_var_name, scale = "response", gg = TRUE,
         line = list(col = "#007BA7"),
         fill = list(fill = "#E0F2F7")) +
    
    #  Observaciones reales 
    geom_point(data = total_pa,
               aes_string(x = x_var_name, y = y_var_name),
               alpha = 0.3,
               size = 1.5,
               color = "black",
               position = position_jitter(height = 0.02)) +
    
    # Rug plot
    geom_rug(data = total_pa,
             aes_string(x = x_var_name),
             sides = "tb",
             alpha = 0.4,
             color = "black") +
    
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, 0.25)) +
    
    labs(x = xtitle, y = ytitle) +
    
    theme_bw() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12, color = "black"),
      panel.grid = element_blank()
    )
}


p1 <- plot_logistico(
  M11,
  "tem",             
  "Pre_aus",       
  total_pa,
  "Variación de temperatura (°C)",
  "Prob. de Presencia"
)

print(p1)




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

####


