library(readxl)
library(dplyr)
library(ggplot2)
dados <- read_excel("BASE_cargos2.xlsx", sheet = "BASE_cargos")

#2

#2.1
dim(dados)

#2.2
names(dados)

#2.3
table(dados$orgao_sup)

#2.4
sum(is.na(dados$exp_adm))
sum(is.na(dados$exp_car))
sum(is.na(dados$nivel))
sum(is.na(dados$instr))
sum(is.na(dados$indicacao))

#2.5

#exp_adm:
table(dados$exp_adm)
#`1(3); 3(1);  NA(11)
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
table(dados$exp_car)
#5(253); NA(75)
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
table(dados$instr)
#NA(95)
#Erro: Nessa variável aparece, além dos valores esperados, o valor NA", que provavelmente foi colocado onde havia valor ausente.
#Correção: Transformar "NA" em valores ausentes.
dados <- dados %>%
  mutate(instr = na_if(instr, "NA"))

#nivel:
table(dados$nivel)
#Sem valores inválidos.

#indicacao
table(dados$indicacao)
#Sem valores inválidos.

#3

library(dplyr)

#Criação da variável "exp_cp" (experiência em cargos públicos) a partir das variáveis "exp_adm" e "exp_car", dado que ambas indicam se nomeado já participou de cargos públicos.
dados <- dados %>%
  mutate(exp_cp = case_when(
    is.na(exp_adm) | is.na(exp_car) ~ NA_real_,
    exp_adm == 0 & exp_car == 0 ~ 0,
    TRUE ~ 1
  ))

table(dados$exp_cp)

#Filtrando a variável "orgao_sup" para que fiquem apenas os valores "mapa" e "minc" (excluindo valores "mcti")
dados <- dados %>%
  filter(orgao_sup %in% c("mapa", "minc"))

#Criação de tabela com: número total de nomeados de cada pasta; número de nomeados com experiência prévia em cargos públicos (exp_cp = 1); proporção de nomeados com experiência em cargos públicos.
tabela_exp <- dados %>%
  group_by(orgao_sup) %>%
  summarise(
    total = sum(!is.na(exp_cp)),
    com_experiencia = sum(exp_cp == 1, na.rm = TRUE),
    proporcao = mean(exp_cp, na.rm = TRUE)
  )

#Calculando intervalo de confiança dos nomeados para o MAPA:
mapa_3 <- tabela_exp %>% filter(orgao_sup == "mapa")
res_mapa_3 <- prop.test(mapa_3$com_experiencia, mapa_3$total)

#Calculando intervalo de confiança dos nomeados do MinC:
minc_3 <- tabela_exp %>% filter(orgao_sup == "minc")
res_minc_3 <- prop.test(minc_3$com_experiencia, minc_3$total)

#Adicionando os valores dos intervalos de confiança na tabela:
tabela_exp$IC_inf <- c(res_mapa_3$conf.int[1], res_minc_3$conf.int[1])
tabela_exp$IC_sup <- c(res_mapa_3$conf.int[2], res_minc_3$conf.int[2])

#Calculando e adicionando valores das margens de erro na tabela:
tabela_exp$margem_erro <- (tabela_exp$IC_sup - tabela_exp$IC_inf) / 2

#4

#4.1
#H0: proporção de nomeações de indivíduos com experiência prévia em cargos públicos é igual entre os nomeados para MAPA e MinC.

#4.2
#H1: proporção de nomeações de indivíduos com experiência prévia em cargos públicos é diferente entre os nomeados para MAPA e MinC.

#4.3
prop.test(
  x = c(457, 284),
  n = c(498, 325)
)

#4.4
#p-value = 0.05325

#4.5
#Não se rejeita H0.

#4.6
#A diferença nas proporções de nomeações de indivíduos com experiência prévia em cargos públicos no MAPA e MinC não é estatisticamente significativa.

#5

#Criação da variável "alto_nivel":
dados <- dados %>%
  mutate(alto_nivel = case_when(
    nivel == 4 ~ 0,
    nivel %in% c(5, 6) ~ 1
  ))

#5.1

#Tabela com os valores e proporções de nomeados com cargos de alto nível no MAPA e MinC:
tabela_alto <- dados %>%
  group_by(orgao_sup) %>%
  summarise(
    total = sum(!is.na(alto_nivel)),
    alto = sum(alto_nivel == 1, na.rm = TRUE),
    proporcao = mean(alto_nivel, na.rm = TRUE)
  )

#5.2

#Calculando intervalo de confiança dos nomeados de alto nivel para o MAPA:
mapa_5 <- tabela_alto %>% filter(orgao_sup == "mapa")
prop.test(mapa_5$alto, mapa_5$total)
# Intervalo de confiança: "95 percent confidence interval:0.3154869 0.3990317"

#Calculando intervalo de confiança dos nomeados do MinC:
minc_5 <- tabela_alto %>% filter(orgao_sup == "minc")
prop.test(minc_5$alto, minc_5$total)
#Intervalo de confiança: "95 percent confidence interval:0.3736576 0.4772690"

#5.3

#H0: proporção de nomeações de indivíduos para cargos de alto nível é igual entre os nomeados para MAPA e MinC.
#H1: proporção de nomeações de indivíduos para cargos de alto nível é diferente entre os nomeados para MAPA e MinC.

prop.test(
  x = c(187, 155),
  n = c(525, 365)
)

#5.4

#p-value = 0.04601
#Hipótese H0 é rejeitada. Em outras palavras, a proporção de nomeados com cargos de alto nível no MAPA e MinC não é igual, pois a diferença entre as proporções é estatisticamente significativa.

#5.5
#Proporção de nomeados para cargos de alto nível é maior no MinC (42,47%) em relação ao MAPA (35,62%).

#P1

#Agrupar por id e,  nos casos em que há mais de uma nomeação por id, manter apenas a nomeação mais recente de cada indivíduo, excluindo as demais:
dados_id <- dados %>%
  group_by(id) %>%
  slice_max(order_by = entrada, n = 1, with_ties = FALSE) %>%
  ungroup()

#Repetindo as etapas do Bloco 5, agora com o banco "dados_id", em que a unidade de análise é o indivíduo.
   #Criar tabela com número de indivíduos por pasta, número de indivíduos em cargos de alto nível e proporção de indivíduos em cargos de alto nível.
tabela_id <- dados_id %>%
  group_by(orgao_sup) %>%
  summarise(
    total = sum(!is.na(alto_nivel)),
    alto = sum(alto_nivel == 1, na.rm = TRUE),
    proporcao = mean(alto_nivel, na.rm = TRUE)
  )
   #Teste de hipótese para verificar se as proporções de indivíduos em cargos de alto nível é igual.
prop.test(
  x = c(124, 123),
  n = c(314, 254)
)
      # p-value = 0.04031

#Conclusão: p-valor inferior ao p-valor do teste de hipótese conduzido com a base de dados na qual a unidade de análise é "nomeação".

#P2
#Exercício feito também com base nas informações do teste de hipótese do  Bloco 5.
#Intervalo de confiança para a diferença de proporções entre nomeações para cargos de alto nível no MAPA e MinC:
prop.test(
  x = c(155, 187),   
  n = c(365, 525)
)
   #Intervalo de confiança não inclui o 0 (valor inferior = 0.0009578958).