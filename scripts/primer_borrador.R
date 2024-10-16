#Cargar paquetes
library(haven)
library(dplyr)

#Cargar datos
data_2017 <- read_stata("datos/findex2017.sav")
data_2021 <- read_stata("datos/findex2021.dta")


#La global 2017 no posee la variable anydigpayment, por lo tanto, se recodifica mediante las otras variables y
#se selecionan las que son de interes por el momento
global_2017 <- data_2017 %>% mutate(anydigpayment=factor(case_when(fin14a==1 |
                                                              fin5==1   |
                                                              fin14b==1 |
                                                              fin14c==1 |
                                                              fin27b==1 |
                                                              fin29b==1 |
                                                              fin31b==1 |
                                                              fin34b==1 | 
                                                              fin39b==1 |
                                                              fin43b==1 |
                                                              fin47b==1 |
                                                              fin47c4==1~ 1, TRUE~0), levels = c(0, 1), 
                                                            labels = c("No", "Si")),
                                    account_fin=factor(account_fin,levels = c(0, 1), 
                                                       labels = c("No", "Si")),
                                    año=2017) %>% 
  select(año,female,age,educ,inc_q,emp_in,account_fin,anydigpayment)

#Seleccionando variables de interes global_2021
global_2021 <- data_2021 %>% 
                  mutate(año=2021,
                         anydigpayment=factor(anydigpayment,levels = c(0, 1), 
                                              labels = c("No", "Si")),
                         account_fin=factor(account_fin,levels = c(0, 1), 
                                            labels = c("No", "Si"))) %>%  
                  select(año,female,age,educ,inc_q,emp_in,account_fin,anydigpayment) 

#Union de ambas datas
global_2017 <- global_2017 %>% mutate(across(where(is.labelled), haven::zap_labels))
global_2021 <- global_2021 %>% mutate(across(where(is.labelled), haven::zap_labels))
global_2017_2021 <- rbind(global_2017, global_2021)

#Pagos digitales por año
table(global_2017_2021$año,global_2017_2021$anydigpayment)

#Tenencia de cuentas en institución financiera
table(global_2017_2021$año,global_2017_2021$account_fin)


#Prueba chi cuadrado
# Crear la tabla de contingencia
tabla_contingencia <- table(global_2017_2021$anydigpayment, global_2017_2021$account_fin)
chi_cuadrado <- chisq.test(tabla_contingencia)
chi_cuadrado
