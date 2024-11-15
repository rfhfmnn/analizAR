library(tidyverse)
library(paqueteadp)
library(skimr)

data("datos_municipales")

ggplot(data    = datos_municipales %>% filter(anio == c(2004, 2008, 2012)),
       mapping = aes(x = anio, y = pobreza)) +
  geom_boxplot() +
  facet_wrap(genero ~ zona)

ggplot(data    = datos_municipales %>% filter(anio == c(2004, 2008, 2012)),
       mapping = aes(x = pobreza, y = ingreso)) +
  geom_point() +
  scale_y_log10()

plot_a <- ggplot(datos_municipales, mapping = aes(x = genero))
plot_a + 
  geom_bar(mapping = aes(y = ..prop.., group = 1)) +
  facet_wrap(~zona, nrow = 1)

plot_a + 
  geom_bar(mapping = aes(y = ..prop.., group = 1)) +
  facet_wrap(~zona, nrow = 1) +
  labs(title = " Proporción de hombres y mujeres elegidos como alcaldes 
       (2004-2012)\n Por zonas económicas de Chile", 
       x = "Género", y = "Proporción", 
       caption = " Fuente: Basado en datos de SERVEL y SINIM (2018)") 

plot_a + 
  geom_bar(mapping = aes(y = ..prop.., group = 1)) +
  facet_wrap(~zona, nrow = 1) +
  scale_x_discrete(labels = c("Hombres", "Mujeres")) +
  labs(title = " Proporción de hombres y mujeres elegidos como alcaldes
       (2004-2012)\n Por zonas económicas de Chile ", 
       x = "Género", y = "Proporción", 
       caption = " Fuente: Basado en datos de SERVEL y SINIM (2018)") 

plot_b <- ggplot(data    = datos_municipales, 
                 mapping = aes(x = anio, y = ingreso)) 
medias <- datos_municipales %>% 
  group_by(zona) %>% 
  summarize(media_ingreso = mean(ingreso, na.rm = T))

plot_b + 
  geom_line(color = "gray70", aes(group = municipalidad)) +
  geom_hline(aes(yintercept = media_ingreso), 
             data = medias, color = "dodgerblue3") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_log10(labels = scales::dollar) +
  facet_wrap(~ zona, nrow = 1) +
  labs(title = " Ingresos municipales en los años electorales (2004-2012)",
       y = " Ingresos",
       x = "Año") +
  theme(panel.spacing = unit(2, "lines"))

plot_c <- ggplot(data    = datos_municipales %>% 
                   filter(anio %in% c(2004, 2008, 2012)),
                 mapping = aes(x = zona, y = ingreso, color = zona)) +
  geom_boxplot() +
  facet_wrap(~anio, ncol = 1)

plot_c

#MODELOS LINEALES
library(ggcorrplot)
data("bienestar")
ls()
skimr::skim(bienestar)

corr_selected <- bienestar %>% 
  select(gini, gasto_educ, dualismo_sectorial, inversion_extranjera, pib, 
         diversidad_etnica, tipo_regimen, gasto_salud, gasto_segsocial,
         bal_legislativo, poblacion) %>% 
  # calcular la matriz de correlación y redondear a un decimal
  cor(use = "pairwise") %>% 
  round(1)

ggcorrplot(corr_selected, type = "lower", lab = T, show.legend = F)

#disitribucion de las variables con las que queremos trabajar
ggplot(bienestar, aes(x = gini, na.rm = T)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Gini Index", y = " Frecuencia",
       title = " Distribución de la variable dependiente",
       caption = "Fuente: Huber et al (2012)")
ggplot(bienestar, aes(x = gasto_educ, na.rm = T))+
  geom_histogram(binwidth = 1) +
  labs(caption = "Fuente: Huber et al (2012))",
       x = "Gasto en educacion",
       y = "Frecuencia")

#relacion entre variables
ggplot(bienestar, aes(gasto_educ, gini)) + 
  geom_point(alpha=0.5) +
  labs(x = " Gasto en educación (% del PIB)", y = "Gini", 
       caption = "Fuente: Huber and Stephens, 2012")

#se estima el modelo
model_1 <- lm(gini ~ 1 + gasto_educ, data = bienestar) # después de la coma indicamos el data.frame que contiene los datos

class(model_1)
summary(model_1)
library(texreg)
screenreg(model_1, 
          custom.model.names = "Modelo 1",  
          custom.coef.names = c("Constante", "Gasto en educación"))
htmlreg(model_1, file = "modelo_1.doc", 
        custom.model.names = "Modelo 1",  
        custom.coef.names = c("Constante", "Gasto en educación"),
        inline.css = FALSE, doctype = T, html.tag = T, 
        head.tag = T, body.tag = T) #guardamos en un word

#representacion del modelo
ggplot(data = bienestar, # seleccionamos la base de datos
       aes(x = gasto_educ, y = gini))+ # variables independientes y dependientes
  geom_point() + # los valores observados son graficados
  geom_smooth(method = "lm", # La línea de regresión se superpone
              se = F, # el área de error no se grafica con un IC del 95%
              color = "blue")+ # color de la línea
  labs (x = "Gasto en educación", y = "Desigualdad")

ggplot(data = bienestar, aes(x = gasto_educ, y = gini))+ 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue",
              se = T) + # añadimos la predicción 
  labs(x = "Gasto en educación", y = "Desigualdad",
       title = " Ajuste lineal entre el gasto en educación y la desigualdad") 

#modelo lineal MULTIPLE
bienestar_no_na <- bienestar %>% 
  drop_na(gini, gasto_educ , inversion_extranjera , gasto_salud , gasto_segsocial , poblacion, 
          dualismo_sectorial, diversidad_etnica, pib, tipo_regimen, bal_legislativo)
model_2 <- lm(gini ~ 1 + gasto_educ + inversion_extranjera + gasto_salud + gasto_segsocial + 
                poblacion + dualismo_sectorial + diversidad_etnica + pib + 
                factor(tipo_regimen) + bal_legislativo,
              data = bienestar_no_na)
screenreg(model_2)

#comparamos modelos
models <- list(model_1, model_2)

screenreg(models,
          custom.model.names = c("Modelo 1", "Modelo 2"),  
          custom.coef.names = c(
            "Constante", "Gasto en educación", "IED", 
            "Gasto en salud", "Gasto en seg. social", 
            "Población jóven", "Dualismo en economía",
            "División étnica", "PBI pc", "Reg. democrático", "Reg. mixto",
            "Reg. autoritario", "Balance entre poderes"))

#modelo restringido
model_2_restricted<- lm(gini~1+gasto_educ + diversidad_etnica, 
                        data = bienestar_no_na)


screenreg(model_2_restricted) # observamos nuevos coeficientes

install.packages("prediction")
library(prediction)
pred_model_2_restricted <- as_tibble(prediction(model_2_restricted)) 

ggplot(data = pred_model_2_restricted) + # los nuevos valores predichos
  geom_point(mapping = aes(x = gasto_educ, y = gini,
                           color = factor(diversidad_etnica))) +
  # se dibujan las líneas de regresión (diferenciadas por color):
  geom_line(mapping = aes(x = gasto_educ, y = fitted, 
                          color = factor(diversidad_etnica), 
                          group = factor(diversidad_etnica))) +
  labs(x = "Gasto en educación", y = "Desigualdad")

#BONDAD DE AJUSTE

model_2_restrained <- lm(gini ~ 1 + gasto_educ + inversion_extranjera + gasto_salud + gasto_segsocial +
                           poblacion+ dualismo_sectorial  + pib,
                         data = bienestar_no_na)
anova(model_2, model_2_restrained)

#SUPUESTOS MCO-LINEALIDAD
ggplot(mapping = aes(x = model_1$fitted.values, y = model_1$residuals)) +
  labs(x = "Valores predichos", y = "Residuos") +
  geom_point() +
  geom_hline(mapping = aes(yintercept = 0), color = "red")

model_1_log <- lm(log(gini) ~ 1 + gasto_educ, data = bienestar)

screenreg(model_1_log)
library(car)
crPlots(model_1)

bienestar_no_na <- bienestar_no_na %>% mutate(cseduc2 = gasto_educ*gasto_educ)

model_1_quadratic <- lm(gini ~ 1 + cseduc2 + gasto_educ, 
                        data = bienestar_no_na)


crPlots(model_1_quadratic)

#RAMSEY TEST PARA VER SI EL MODELO ESTA MAL ESPECIFICADO
library(lmtest)
resettest(model_1, power = 2, type = "fitted", data = bienestar_no_na)

#MULTICOLINEALIDAD TEST VIF
vif(model_2)
sqrt(vif(model_2)) > 2

#HOMOCEDASTICIDAD
ggplot(bienestar_no_na, aes(gasto_educ, gini)) + 
  geom_point() +
  theme_bw()+ 
  geom_smooth(method = "lm")
plot(model_1, which = 1)
car::residualPlots(model_2)

bptest(model_2, studentize = T)

#HETEROCEDASTICIDAD CLUSTERS
library(ggplot2)

ggplot(bienestar_no_na, aes(gasto_educ, gini)) + 
  geom_point() + 
  facet_wrap(~pais) 

ggplot(bienestar_no_na, aes(gasto_educ, gini, color = pais)) + 
  geom_point() + 
  theme_bw()

library(miceadds)
model_2_cluster <- miceadds::lm.cluster(
  data = bienestar_no_na, 
  formula = gini ~ 1 + gasto_educ + dualismo_sectorial + inversion_extranjera + pib + 
    diversidad_etnica + tipo_regimen + gasto_salud + gasto_segsocial + bal_legislativo, 
  cluster = "pais"
)

summary(model_2_cluster)

#NORMALIDAD DE LOS ERRORES
qqPlot(model_2$residuals)
