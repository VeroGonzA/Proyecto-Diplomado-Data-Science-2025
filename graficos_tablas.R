


#GRAFICO DE REMUNERACION POR ESTAMENTO Y SEXO
library(dplyr)
library(tidyr)

tabla_remu <- datos_proyecto %>%
  select(starts_with("rem")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("estamento", "sexo"),
    names_pattern = "rem_(.*)_(h|m)",
    values_to = "remuneracion"
  ) %>%
  group_by(estamento, sexo) %>%
  summarise(
    promedio = mean(remuneracion, na.rm = TRUE),
    mediana  = median(remuneracion, na.rm = TRUE),
    n = n(),
    ic_inf = t.test(remuneracion)$conf.int[1],
    ic_sup = t.test(remuneracion)$conf.int[2],
    .groups = "drop"
  )

tabla_remu_sin_total <- tabla_remu %>%
  filter(estamento != "total")
tabla_remu_sin_total <- tabla_remu_sin_total %>%
  mutate(estamento = recode(estamento,
                            "admin"     = "Administrativo",
                            "profesional"  = "Profesional",
                            "tecnico"       = "Técnico",
                            "venta"= "Venta",
                            "agro"      = "Agropecuario y pesquero",
                            "directivo"= "Directivo",
                            "noesp"= "No especializado",
                            "op"= "Operario"))|> 
           mutate(estamento=factor(estamento,
                                   levels=c("Directivo",
                                            "Profesional",
                                            "Técnico",
                                            "Administrativo",
                                            "Venta",
                                            "Operario",
                                            "Agropecuario y pesquero",
                                            "No especializado")))


brechas <- tabla_remu_sin_total %>%
  select(-c("ic_inf", "ic_sup")) |> 
  pivot_wider(names_from = sexo, values_from = promedio) %>%
  mutate(brecha = m - h)

g_remu_est <- ggplot(tabla_remu_sin_total, 
                     aes(x = estamento, y = promedio, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup),
                position = position_dodge(width = 0.9),
                width = 0.2, color = "black") +
  scale_fill_manual(values = c("m" = "#996CA9", "h" = "#56B4E9"),
                    labels = c("m" = "Mujer", "h" = "Hombre")) +
  labs(
    title = "Remuneración promedio por estamento y sexo",
    x = "Estamento",
    y = "Remuneración promedio",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

ggsave("grafico_5.png", g_remu_est, width = 8, height = 6, dpi = 300)


#ggplot(brechas, aes(x = estamento, y = brecha)) +
#  geom_col(fill = "steelblue") +
#  geom_hline(yintercept = 0, linetype = "dashed") +
#  labs(
#    title = "Brecha salarial (Mujer - Hombre) por estamento",
#    x = "Estamento",
#    y = "Diferencia en remuneración"
#  ) +
#  theme_minimal()


g_remu_est<-ggplot(tabla_remu_sin_total, aes(x = estamento, y = promedio, fill = sexo)) +
  geom_col(position = "dodge") +
  #geom_text(aes(label = round(promedio, 0)), 
   #         position = position_dodge(width = 0.9), 
    #        vjust = -0.2, size = 3.5) +
  scale_fill_manual(values = c("m" = "#996CA9", "h" = "#56B4E9"),
                    labels= c("m"="Mujer", "h"="Hombre")) + # Colores personalizados
  labs(
    title = "Remuneración promedio por estamento y sexo",
    x = "Estamento",
    y = "Remuneración promedio",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

print(g_remu_est)
ggsave("grafico3.png", g_remu_est, width = 8, height = 6, dpi = 300)


#GRAFICO DE PERSONAS CONTRATADAS POR ESTAMENTO

tabla_ocupadas <- datos_proyecto %>%
  select(starts_with(c("directivo_",
                     "profesional_",
                     "tecnico_",
                     "admin_",
                     "venta_",
                     "agro_",
                     "op_",
                     "noesp_",
                     "total_ocupado"))) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("estamento", "sexo"),
    names_pattern = "(.*)_(h|m)",
    values_to = "ocupados"
  ) %>%
  group_by(estamento, sexo) %>%
  summarise(
    promedio = mean(ocupados, na.rm = TRUE),
    mediana  = median(ocupados, na.rm = TRUE),
    total = sum(ocupados, na.rm=TRUE),
    n = n(),
    .groups = "drop"
  )

tabla_ocupadas <- tabla_ocupadas %>%
  filter(estamento != "total_ocupado")
tabla_ocupadas <- tabla_ocupadas %>%
  mutate(estamento = recode(estamento,
                            "admin"     = "Administrativo",
                            "profesional"  = "Profesional",
                            "tecnico"       = "Técnico",
                            "venta"= "Venta",
                            "agro"      = "Agropecuario y pesquero",
                            "directivo"= "Directivo",
                            "noesp"= "No especializado",
                            "op"= "Operario")) |> 
  mutate(estamento=factor(estamento,
                          levels=c("Directivo",
                                   "Profesional",
                                   "Técnico",
                                   "Administrativo",
                                   "Venta",
                                   "Operario",
                                   "Agropecuario y pesquero",
                                   "No especializado")))

tabla_ocupadas <- tabla_ocupadas %>%
    group_by(estamento) %>%
  mutate(porcentaje = round(total / sum(total) * 100, 1)) %>%
  ungroup()



g_pct_ocupados<-ggplot(tabla_ocupadas, aes(x = estamento, y = porcentaje, fill = sexo)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5) +
  #scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = c("m" = "#996CA9", "h" = "#56B4E9"),
                     labels= c("m"="Mujer", "h"="Hombre")) + # Colores personalizados
  labs(
    title = "Distribución porcentual de mujeres y hombres ocupados por estamento",
    x = "Estamento",
    y = "Porcentaje",
    fill = "Sexo"
  ) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", linewidth = 0.75)+
theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold") # 👈 aquí sí giras los nombres del eje X
  )

g_pct_ocupados

ggsave("grafico_4a.png", g_pct_ocupados, width = 8, height = 6, dpi = 300)
ggsave("grafico_4.png", g_ocupado_est, width = 8, height = 6, dpi = 300)

#g_ocupado_est<-ggplot(tabla_ocupadas, aes(x = estamento, y =total , fill = sexo)) +
#  geom_col(position = "dodge") +
#  #geom_text(aes(label = round(promedio, 0)), 
#  #         position = position_dodge(width = 0.9), 
#  #        vjust = -0.2, size = 3.5) +
#  scale_fill_manual(values = c("m" = "#996CA9", "h" = "#56B4E9"),
#                    labels= c("m"="Mujer", "h"="Hombre")) + # Colores personalizados
#  labs(
#    title = "Número de personas ocupadas en la empresa por estamento y sexo",
#    x = "Estamento",
#    y = "Número de personas ocupadas",
#    fill = "Sexo"
#  ) +
#  theme_minimal() +
#  theme(
#    plot.title = element_text(size = 14, face = "bold"),
#    axis.text.x = element_text(angle = 25, hjust = 1)
#  )
#
#print(g_ocupado_est)
#
#ggsave("grafico_4.png", g_ocupado_est, width = 8, height = 6, dpi = 300)


#### tabla de % muejres en directorio segun ciiu y tamaño

tabla_ciiu_tamano <- datos_proyecto %>%
  group_by(CIIU_FINAL, TAMANO) %>%
  summarise(
    promedio_mujeres_dir = mean(pct_directoras, na.rm = TRUE),
    n_empresas = n()
  ) %>%
  ungroup()

library(tidyr)

tabla_ciiu_tamano_ancha <- tabla_ciiu_tamano %>%
  select(-n_empresas) %>%  # si no quieres mostrar N
  pivot_wider(
    names_from = TAMANO,
    values_from = promedio_mujeres_dir
  )


library(scales)
library(knitr)
library(kableExtra)

t_ciiu_tamano<-tabla_ciiu_tamano_ancha %>%
  mutate(across(where(is.numeric), ~ percent(.x, accuracy = 0.1))) %>%
  kable(caption = "Porcentaje promedio de mujeres en directorios según CIIU y tamaño de empresa") %>%
  kable_styling(full_width = FALSE)


library(writexl)
write_xlsx(tabla_ciiu_tamano_ancha,
  path = "Porcentaje_mujeres.xlsx"
)





###


tabla_promedios <- datos_proyecto %>%
  select(starts_with("rem"),
         starts_with("directivo_"),
         starts_with("profesional_"),
         starts_with("tecnico_"),
         starts_with("admin_"),
         starts_with("venta_"),
         starts_with("agro_"),
         starts_with("op_"),
         starts_with("noesp_"),
         starts_with("total_ocupado")) |>  
  select(-contains("total")) |> 
  pivot_longer(cols = everything(),
               names_to = "variable", values_to = "valor") %>%
  # 👇 agregar prefijo n_ a las que no son remuneración
  mutate(variable = if_else(!str_starts(variable, "rem"),
                            paste0("n_", variable),
                            variable)) %>%
  separate(variable, into = c("tipo", "estamento", "sexo"), sep = "_") %>%
  group_by(tipo, estamento, sexo) |> 
  summarise(total=sum(valor, na.rm=TRUE)) |> 
  pivot_wider(names_from = tipo, values_from = total) |> 
  mutate(rem_promedio = rem / n) %>%
  select(estamento, sexo, rem_promedio)

tabla_promedios <- tabla_promedios%>%
  mutate(estamento = recode(estamento,
                            "admin"     = "Administrativo",
                            "profesional"  = "Profesional",
                            "tecnico"       = "Técnico",
                            "venta"= "Venta",
                            "agro"      = "Agropecuario y pesquero",
                            "directivo"= "Directivo",
                            "noesp"= "No especializado",
                            "op"= "Operario")) |> 
  mutate(estamento=factor(estamento,
                          levels=c("Directivo",
                                   "Profesional",
                                   "Técnico",
                                   "Administrativo",
                                   "Venta",
                                   "Operario",
                                   "Agropecuario y pesquero",
                                   "No especializado")))


g_rem_est<-ggplot(tabla_promedios, aes(x = estamento, y = rem_promedio, fill = sexo)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("m" = "#996CA9", "h" = "#56B4E9"),
                    labels = c("m"="Mujer", "h"="Hombre")) +
  labs(
    title = "Remuneración promedio según estamento y sexo",
    x = "Estamento",
    y = "Promedio",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        plot.title = element_text(size = 14, face = "bold") # 👈 aquí sí giras los nombres del eje X
        )

brechas <- tabla_promedios %>%
  pivot_wider(names_from = sexo, values_from = rem_promedio) %>%
  mutate(brecha = m - h)

ggsave("grafico_6.png", g_rem_est, width = 8, height = 6, dpi = 300)
