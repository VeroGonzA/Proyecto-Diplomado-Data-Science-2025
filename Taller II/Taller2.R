#### PARTE I - TEST DE HIPOTESIS Y MODELAMIENTO ####

# Intalar librerias
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Abrir la base encla y revisión de columnas
encla <- read_excel("Taller II/encla.xlsx")
View(encla)
names(encla)

# Ajustar nombre de columnas
encla <- encla %>%
  rename (Privado = `Sector(1=Privado)`,
          Jornada = `Jornada (1=parcial)`,
          Casado = `Casado (1=si)`,
          sexo = `Sexo (1=fem)`)

##### PREGUNTA 1 #####
#Recategorizar las siguientes variables:
#Años de educación en dos grupos: Ed.media <=12 versus >12 Ed.superior
#Edad en tres grupos: joven <30; adulto 30-50; mayor >50.

encla<-encla |> 
  mutate(año_educacion=ifelse(educacion<=12, "Educacion media", "Educacion superior"),
         edad_cat=case_when(
           Edad<30~"Joven",
           Edad>=30 & Edad <=50 ~"Adulto",
           Edad>50~"Mayor"
         ))

#(a)#Obtenga una descripción de las variables cuantitativas: experiencia laboral e ingreso según sexo, sector laboral y edad categorizada.

# Tabla resumen de todos las variables
descriptivos_encla <- encla %>%
  group_by(sexo, Privado, edad_cat) %>%
  summarise(
    experiencia_media = mean(Experiencia, na.rm = T),
    ingreso_media = mean(Ingreso, na.rm = T),
    .groups = "drop"
  )

print(descriptivos_encla)

# Resumen General por variable
resultado_sector <- encla %>%
  group_by(Privado) %>%
  summarise(
    Experiencia_media = mean(Experiencia, na.rm=T),
    Ingreso_medio = mean(Ingreso, ra.rm=T))
print(resultado_sector)

# Resumen Edad Categorizada
resultados_exp <- encla %>%
  group_by(edad_cat) %>%
  summarise(
    experiecia = mean(Experiencia, na.rm=T),
    ingresos = mean (Ingreso, na.rm = T))

print(resultados_exp)

experiencia_rangos <- encla %>%
  group_by(edad_cat) %>%
  summarise(
    Experiencia_min = min(Experiencia, na.rm = TRUE),
    Experiencia_max = max(Experiencia, na.rm = TRUE)
  )
print(experiencia_rangos)

#(b)#Obtenga tabla % que permita inferir potencial asociación entre el nivel educacional y sector laboral.
tabla_encla <- encla %>%
  group_by(año_educacion, Privado) %>%
  summarise(Frecuencia = n(), .groups = "drop") %>%
  mutate(porcentaje = round(100 * Frecuencia / sum(Frecuencia), 1))

print(tabla_encla)

##### PREGUNTA 2 #####
#Test de hipótesis (e idealmente represente en forma gráfica) (1 punto)
###### (a) ¿existe discriminación salarial según sexo? #####

# Convertir sexo a factor para analisis de ANOVA
table(encla$sexo)

encla$sexo <- factor(encla$sexo, labels = c("Hombre", "Mujer"))
mod_1 <- aov(Ingreso ~ sexo, data=encla)
summary(mod_1)
TukeyHSD(mod_1)

# Calcular media y error estándar por sexo
resumen_mod1 <- encla %>%
  group_by(sexo) %>%
  summarise(
    media_ingreso = mean(Ingreso, na.rm = TRUE),
    error_estandar = sd(Ingreso, na.rm = TRUE) / sqrt(n())
  )
print(resumen_mod1)

# Boxplot sexo - ingreso
grafico_1 <- ggplot(encla, aes(x = sexo, y = Ingreso, fill = sexo)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.6) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(
    title = "Comparación de Ingresos por Sexo",
    x = NULL,
    y = "Ingreso ($miles)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

grafico_1

###### (b) ¿A mayor educación mayor ingreso? ######
mod_2=aov(Ingreso ~ año_educacion, data=encla)
summary(mod_2)
TukeyHSD(mod_2)

resumen_mod2 <- encla %>%
  group_by(año_educacion) %>%
  summarise(
    media_ingreso = mean(Ingreso, na.rm = TRUE),
    error_estandar = sd(Ingreso, na.rm = TRUE) / sqrt(n())
  )
print(resumen_mod2)

grafico_2 <- ggplot(encla, aes(x = año_educacion, y = Ingreso, fill = año_educacion)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.6) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(
    title = "Comparación de Ingresos por Nivel Educacional",
    x = NULL,
    y = "Ingreso ($miles)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

grafico_2

###### (c) ¿Existe diferencia en el salario según edad (categorizada)? ######
mod_3=aov(Ingreso ~ edad_cat, data=encla)
summary(mod_3)
TukeyHSD(mod_3)

resumen_mod3 <- encla %>%
  group_by(edad_cat) %>%
  summarise(
    media_ingreso = mean(Ingreso, na.rm = TRUE),
    error_estandar = sd(Ingreso, na.rm = TRUE) / sqrt(n())
  )
print(resumen_mod3)

grafico_3 <- ggplot(encla, aes(x = edad_cat, y = Ingreso, fill = edad_cat)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.6) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "green")) +
  labs(
    title = "Comparación de Ingresos por Edad Categorizada",
    x = NULL,
    y = "Ingreso ($miles)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

grafico_3

##### PREGUNTA 3 ######
###### (a) Modelos regresion lineal #####

regr_0=lm(Ingreso~1, data=encla)
regr_1=lm(Ingreso ~ Experiencia, data=encla )
regr_2=lm(Ingreso ~ Edad, data=encla)
regr_3=lm(Ingreso ~ educacion, data=encla)
regr_4=lm(Ingreso ~ sexo, data=encla)

summary(regr_1)
summary(regr_2)
summary(regr_3)
summary(regr_4)


###### (b) Modelos regresion lineal multiple #####

regr_5=lm(Ingreso~ Experiencia + Edad+ educacion + sexo, data=encla)
summary(regr_5)

MASS::stepAIC(regr_0, scope = formula(regr_5),direction = "both")
m_both <- MASS::stepAIC(regr_0, scope = formula(regr_5),direction = "both")
summary(m_both)

###### (c) Son válidos los supuestos en este último modelo ######

# a. Normalidad 
#H0: Los datos siguen una distribución Normal.
#H1: Los datos no siguen una distribución Normal.
res_regr_5 = regr_5$residuals 
ggplot(data=NULL, aes(sample = res_regr_5 ))+
  stat_qq() + stat_qq_line()

install.packages("nortest")
library(nortest)
shapiro.test(res_regr_5)
nortest::lillie.test(res_regr_5)
##no normalidad

# b. Independencia

#H0: Los errores no están autocorrelacionados.
#H1: Los errores presentan correlación a un paso.
install.packages("lmtest")
library(lmtest)
lmtest::dwtest(regr_5)
##aceptamos H0


# c. Homocedasticidad
#H0: Los errores son homocedásticos
#H1: Los errores no son heterocedásticos
lmtest::bptest(regr_5)
##no aceptamos

#4.
#Repita todo [3] pero usando como variable respuesta log(ingreso) (2 puntos)
encla<-encla |> 
  mutate(log_Ingreso=log(Ingreso))

log_regr_0=lm(log_Ingreso~1, data=encla)
log_regr_1=lm(log_Ingreso ~ Experiencia, data=encla )
log_regr_2=lm(log_Ingreso ~ Edad, data=encla)
log_regr_3=lm(log_Ingreso ~ educacion, data=encla)
log_regr_4=lm(log_Ingreso ~ sexo, data=encla)

summary(log_regr_1)
summary(log_regr_2)
summary(log_regr_3)
summary(log_regr_4)

log_regr_5=lm(log_Ingreso~ Experiencia + Edad+ educacion + sexo, data=encla)
summary(log_regr_5)

MASS::stepAIC(log_regr_0, scope = formula(log_regr_5),direction = "both")
m_both <- MASS::stepAIC(log_regr_0, scope = formula(log_regr_5),direction = "both")
summary(m_both)


# a. Normalidad 
#H0: Los datos siguen una distribución Normal.
#H1: Los datos no siguen una distribución Normal.
log_res_regr_5 = log_regr_5$residuals 
ggplot(data=NULL, aes(sample = log_res_regr_5 ))+
  stat_qq() + stat_qq_line()

shapiro.test(log_res_regr_5)
nortest::lillie.test(log_res_regr_5)

##no normalidad

# b. Independencia

#H0: Los errores no están autocorrelacionados.
#H1: Los errores presentan correlación a un paso.
lmtest::dwtest(log_regr_5)
##aceptamos H0


# c. Homocedasticidad
#H0: Los errores son homocedásticos
#H1: Los errores no son heterocedásticos
lmtest::bptest(log_regr_5)
##si aceptamos


#(a)
#¿hay diferencias respecto a lo obtenido en 3a, 3b y 3c? ¿cuáles?
#  Trate de ser lo más conciso al responder cada pregunta. En su análisis debe considerar lo visto en el curso y en especial lo relativo a análisis de residuos.


#PARTE 2

dpm <- read_excel("Taller II/dpm.xlsx")
View(dpm)



#AIC
#Stepwise
#fit<-lm(…., family=logistic)
#AIC(fit)
#BIC(fit)

#a)
#Obtenga el mejor modelo de regresión logística con solo 2 predictores (2 puntos)
#(b)
#Obtenga el mejor modelo de regresión logística (con todas las variables que sean significativas (2 puntos).
#(c)
#Compare los modelos obtenidos - ROC e indicadores de la tabla de confusión (2 puntos).
