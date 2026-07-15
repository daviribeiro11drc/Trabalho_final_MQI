library(readxl)
library(dplyr)
library(ggplot2)
dados <- read_excel("BASE_cargos2.xlsx", sheet = "BASE_cargos")

#2

#2.1
dim(dados)
#1038 linhas, 26 colunas

#2.2
names(dados)

#2.3
table(dados$orgao_sup)

#2.4
sum(is.na(dados$exp_adm))
#0
sum(is.na(dados$exp_car))
#1
sum(is.na(dados$nivel))
#0
sum(is.na(dados$instr))
#0
sum(is.na(dados$indicacao))
#0

#2.5
table(dados$exp_adm)
#`1(3); 3(1);  NA(11)
table(dados$exp_car)
#5(253); NA(75)
table(dados$nivel)
# níveis 4, 5, e 6, aparentemente sem valores aberrantes.
table(dados$instr)
#NA(95)
table(dados$indicacao)
#sem valores aberrantes.

#Identificação do erro e correção: 

#exp_adm:
#Erro: Nessa variável binária, cujos resultados esperados eram 0 e 1, aparecem, além dos valores esperados, os valores: "1" (3 vezes); "3" (1 vez); "NA" (11 vezes). O valor "NA" provavelmente foi colocado onde havia valor ausente. O valor "`1" provavelmente é um "1" digitado de forma equivocada e o valor "3" é um erro para o qual não tenho hipótese.
#Correção: Transformar "`1" em "1", e "NA" e "3" em valores ausentes.
dados <- dados %>%
  mutate(exp_adm = case_when(
    exp_adm == "`1" ~ "1",
    exp_adm == "3" ~ NA_character_,
    exp_adm == "NA" ~ NA_character_,
    TRUE ~ as.character(exp_adm)
  ))
dados$exp_adm <- as.numeric(dados$exp_adm)

#exp_car:
#Erro: Nessa variável, há uma confusão. No artigo, é dito que os valores esperados, conforme nível do cargo ocupado, são: "nenhum = 0, níveis 1 e 2 = 1; níveis 3 e 4 = 2; níveis 5 e 6 = 5". Já na aba "Codebook" do banco de dados, é colocado que o valor atribuido para quem ocupou cargos de nível 5 e 6 é "3", não "5". Provavelmente, portanto, os valores "3" e "5" referem-se ao mesmo nível de cargo ocupado e, portanto, devem ser unificados. Já o valor "NA", assim como na variável anterior, provavelmente foi colocado onde havia valor ausente.
#Correção: Transformar "5" em "3", e "NA" em valores ausentes.
dados <- dados %>%
  mutate(exp_car = case_when(
    exp_car == "5" ~ "3",
    exp_car == "NA" ~ NA_character_,
    TRUE ~ as.character(exp_car)
  ))
dados$exp_car <- as.numeric(dados$exp_car)

#instr:
#Erro: Nessa variável aparece, além dos valores esperados, o valor NA", que provavelmente foi colocado onde havia valor ausente.
#Correção: Transformar "NA" em valores ausentes.
dados <- dados %>%
  mutate(instr = na_if(instr, "NA"))

#3

library(dplyr)
dados <- dados %>%
  mutate(exp_cp = case_when(
    is.na(exp_adm) | is.na(exp_car) ~ NA_real_,
    exp_adm == 0 & exp_car == 0 ~ 0,
    TRUE ~ 1
  ))
table(dados$exp_cp)
dados <- dados %>%
  filter(orgao_sup %in% c("mapa", "minc"))

tabela_exp <- dados %>%
  group_by(orgao_sup) %>%
  summarise(
    total = sum(!is.na(exp_cp)),
    com_experiencia = sum(exp_cp == 1, na.rm = TRUE),
    proporcao = mean(exp_cp, na.rm = TRUE)
  )

mapa <- tabela_exp %>% filter(orgao_sup == "mapa")

res_mapa <- prop.test(mapa$com_experiencia, mapa$total)

minc <- tabela_exp %>% filter(orgao_sup == "minc")

res_minc <- prop.test(minc$com_experiencia, minc$total)

tabela_exp$IC_inf <- c(res_mapa$conf.int[1], res_minc$conf.int[1])
tabela_exp$IC_sup <- c(res_mapa$conf.int[2], res_minc$conf.int[2])
tabela_exp$margem_erro <- (tabela_exp$IC_sup - tabela_exp$IC_inf) / 2
