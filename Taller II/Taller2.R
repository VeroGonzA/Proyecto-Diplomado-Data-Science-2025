####PARTE I####
library(readxl)
encla <- read_excel("Taller II/encla.xlsx")
View(encla)
library(tidyverse)
names(encla)

encla<-encla |> 
  mutate(año_educacion=ifelse(educacion<=12, "Educacion media", "Educacion superior"),
         edad_cat=case_when(
           Edad<30~"Joven",
           Edad>=30 & Edad <=50 ~"Adulto",
           Edad>50~"Mayor"
         ))





####PREGUNTA 1####
#Recategorizar las siguientes variables:
#Años de educación en dos grupos: Ed.media <=12 versus >12 Ed.superior
#Edad en tres grupos: joven <30; adulto 30-50; mayor >50.

#(a)#Obtenga una descripción de las variables cuantitativas: experiencia laboral e ingreso según sexo, sector laboral y edad categorizada.
#(b)#Obtenga tabla % que permita inferir potencial asociación entre el nivel educacional y sector laboral.

#### PREGUNTA 2####
#Test de hipótesis (e idealmente represente en forma gráfica) (1 punto)
#(a)#Es válida la afirmación: ¿existe discriminación salarial según sexo?
# (b)#Es válida la afirmación: ¿A mayor educación mayor ingreso?
#  (c)
#¿Existe diferencia en el salario según edad (categorizada)?
#  En cada caso indique sus resultados – test, valor-p y medias y sus errores estándar.


encla=encla |> 
  rename(sexo="Sexo (1=fem)")
modelo = aov(Salario ~ Industria, data =df)
summary(modelo)


mod_1= aov(Ingreso ~ sexo, data=encla)
summary(mod_1)

mod_2=aov(Ingreso ~ año_educacion, data=encla)
summary(mod_2)

prom_sexo=encla |> 
  group_by(sexo) |> 
  summarise(media=mean(Ingreso))

prom_edu=encla |> 
  group_by(año_educacion) |> 
  summarise(media=mean(Ingreso))


mod_3=aov(Ingreso~edad_cat, data=encla)
summary(mod_3)

prom_año=encla |> 
  group_by(edad_cat) |> 
  summarise(media=mean(Ingreso))

TukeyHSD(mod_3)

##media y error estandar falta

# Test Scheffe
DescTools::ScheffeTest(modelo, "Industria")

# H0: Todas las medias son iguales
# H1: al menos dos medias son distintas
# En este caso el valor p es 0.0003555 es menor a 0.05
# Por lo tanto, rechazamos H0
# En Conclusión, las medias salarios son diferentes para al menos 
# dos tipos de industrias


#Ajuste modelos lineales para ingreso (2 puntos)
#(a)
#Lleve a cabo los modelos de regresión lineal simple del ingreso en función de experiencia laboral, edad, educación, sexo. Con base a los resultados obtenidos, ¿qué variables son estadísticamente significativas?
regr_0=lm(Ingreso~1, data=encla)
regr_1=lm(Ingreso ~ Experiencia, data=encla )
regr_2=lm(Ingreso ~ Edad, data=encla)
regr_3=lm(Ingreso ~ educacion, data=encla)
regr_4=lm(Ingreso ~ sexo, data=encla)

summary(regr_1)
summary(regr_2)
summary(regr_3)
summary(regr_4)




#  (b)
#Obtenga un modelo de regresión lineal múltiple en función de todas las variables previas que resultaron estadísticamente significativas. ¿Siguen siendo significativas?

regr_5=lm(Ingreso~ Experiencia + Edad+ educacion + sexo, data=encla)
summary(regr_5)
MASS::stepAIC(regr_0, scope = formula(regr_5),direction = "forward")
m_forw <- MASS::stepAIC(regr_0, scope = formula(regr_5),direction = "forward")
summary(m_forw)


#  (c)
#Son válidos los supuestos en este último modelo.

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

MASS::stepAIC(log_regr_0, scope = formula(log_regr_5),direction = "forward")
m_forw <- MASS::stepAIC(log_regr_0, scope = formula(log_regr_5),direction = "forward")
summary(m_forw)


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




