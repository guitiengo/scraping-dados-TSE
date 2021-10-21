#Scraping de programas dos candidatos aos governos dos estados e à presidente nas eleições de 2014 e 2018

#Etapas: 

  #Baixar informações de candidatos nos anos de 2014 e 2018
    #Cria base de dados RDS com informações dos candidatos a governador e a presidente.
  
  #Baixar propostas de governadores e de presidentes em 2018 - no site do TSE (Dados abertos)
  
  #Baixar propostas de governdores e de presidentes em 2014
    #Carrega base de governadores e presidentes
    #Para cada candidato:
      #Acessa a pgágina site do TSE (Divulgação de contas eleitorais)
      #Localiza o arquivo da proposta
      #Baixa o arquivo no padrão do TSE para 2018
      


#Site TSE: https://dadosabertos.tse.jus.br/
#Data da Criação: 2021-10-19 

#
options(scipen= 999)

#Carrega bibliotecas necessárias
library(dplyr)
library(RSelenium)
library(stringr)


#Baixar informação dos candidatos----------------

#Caso a base de dados não esteja disponível, faz o download e cria uma base R com os dados de 2014
if (!file.exists("base_RDS/candidatos_gov_pres_2014.RDS")) { 
  download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2014.zip",
                "arquivos/consulta_cand_2014.zip")
  diretorio_temporario <- tempdir()
  unzip("arquivos/consulta_cand_2014.zip", 
        exdir =  diretorio_temporario)
  
  #Carrega a base de 2014 e já seleciona governadores 
  consulta_candidatos_2014 <- read.csv2(file.path(diretorio_temporario,"consulta_cand_2014_BRASIL.csv"),
                                        sep=";",
                                        fileEncoding = "latin1") %>% 
    filter(CD_CARGO %in% c(1,3))
  
  #Salva dados dos governadores em RDS
  saveRDS(consulta_candidatos_2014, 
          "base_RDS/candidatos_gov_pres_2014.RDS")
  
  #remove o diretorio temporario e a base temporaria
  rm(list=c("diretorio_temporario", 
            "consulta_candidatos_2014"))
}

#Caso a base de dados não esteja disponível, faz o download e cria uma base R com os dados de 2018
if (!file.exists("base_RDS/candidatos_gov_pres_2018.RDS")) { 
  
  download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2018.zip",
                "arquivos/consulta_cand_2018.zip")
  diretorio_temporario <- tempdir()
  unzip("arquivos/consulta_cand_2018.zip", 
        exdir =  diretorio_temporario)
  
  #Carrega a base de 2014 e já seleciona governadores 
  consulta_candidatos_2018 <- read.csv2(file.path(diretorio_temporario,"consulta_cand_2018_BRASIL.csv"),
                                        sep=";",
                                        fileEncoding = "latin1") %>% 
    filter(CD_CARGO %in% c(1,3))
  
  #Salva dados dos governadores em RDS
  saveRDS(consulta_candidatos_2018, 
          "base_RDS/candidatos_gov_pres_2018.RDS")
  
  #remove o diretorio temporario e a base temporaria
  rm(list=c("diretorio_temporario", 
            "consulta_candidatos_2018"))
}



#Baixar arquivos com propostas de governo - 2018----

#Lista todos os estados disponíveis na base e adiciona BRASIL para baixa os dados de presidentes
estados <- readRDS("base_RDS/candidatos_gov_pres_2018.RDS") %>% 
  select(SG_UF) %>% 
  distinct(.) %>% 
  add_row(tibble(SG_UF = "BR"))
  
#Para cada estado, baixa os dados na pasta arquivos e extrai para pasta pdf

#Verifica se já existem dados na pasta antes de baixar os arquivos

if (!file.exists("pdf/LEIAME.pdf")) {
  for (x in 1:nrow(estados)) {
    print(paste0("Baixando informações para: ",estados[x,]))
    nome_arquivo <- paste0("proposta_governo_2018_",estados[x,],".zip") 
    caminho_completo <- file.path("arquivos",nome_arquivo)
    download.file(paste0("https://cdn.tse.jus.br/estatistica/sead/odsele/proposta_governo/",nome_arquivo),destfile = caminho_completo)
    unzip(caminho_completo, exdir = "pdf")
  }
}
rm("estados")  


#Baixar arquivos com propostas de governo 2014----

#o TSE não disponibiliza de as propostas de governo de uma forma simples para o ano de 2014, é necessário entrar no site Divulga Contas
#O site não é estático, sendo necessário utilizar alguma library que carregue de forma dinâmica, usei a RSelenium
#Depois de carregado, o link para a proposta é também dinâmico e só funciona ao clicar - o nome do arquivo tem um id que não está disponível ao público.
#Uma saída que encontrei foi encontrar o nome do arquivo da proposta em uma variável javascript e então fazer o download


#Carrega base das eleições de 2014
dados_candidatos_2014 <- readRDS("base_RDS/candidatos_gov_pres_2014.RDS") %>% 
  select(SG_UF,SQ_CANDIDATO,CD_ELEICAO,CD_CARGO,DS_CARGO,NM_CANDIDATO) %>% 
  arrange(SG_UF) %>% 
  distinct()

#Iniciar o navegador remotamente
driver <- rsDriver(browser=c("firefox"))
remote_driver <- driver[["client"]]


for (n in 1:nrow(dados_candidatos_2014)) {
  temp_estado <- dados_candidatos_2014$SG_UF[n]
  temp_numero_candidato <- dados_candidatos_2014$SQ_CANDIDATO[n]
  temp_cd_cargo <- dados_candidatos_2014$CD_CARGO[n]
  
  #essa é a página padrão do divulga contas do TSE
  temp_pagina <- paste0("https://divulgacandcontas.tse.jus.br/divulga/#/candidato/2014/680/",temp_estado,"/",temp_numero_candidato)
  
  remote_driver$navigate(temp_pagina)
  Sys.sleep(sample(10:20,1))
  temp_ng_storage <- remote_driver$executeScript("return window.sessionStorage.getItem('ngStorage-candidato');")
  temp_arquivo <- unlist(str_extract_all(temp_ng_storage,"proposta_governo.{5,15}pdf",simplify = F))
  
  if (length(temp_arquivo) == 0) { 
    print("Esperando mais 10s..")
    Sys.sleep(10)
    temp_ng_storage <- remote_driver$executeScript("return window.sessionStorage.getItem('ngStorage-candidato');")
    temp_arquivo <- unlist(str_extract_all(temp_ng_storage,"proposta_governo.{5,15}pdf",simplify = F))
  }
  
  
  temp_link_download <- paste0("https://divulgacandcontas.tse.jus.br/dados/2014/680/BR/",temp_estado,"/",temp_cd_cargo,"/",temp_numero_candidato,"/",temp_arquivo)
  temp_arquivo_pdf <- paste0("pdf/",temp_estado,"/","2014",temp_estado,temp_numero_candidato,".pdf")
  
  if ((length(temp_arquivo) != 0) & (!file.exists(temp_arquivo_pdf))) { 
    download.file(temp_link_download,
                  destfile = paste0("pdf/",temp_estado,"/","2014",temp_estado,temp_numero_candidato,".pdf"))
  }
    
  
  
  rm(list=ls()[grepl(ls(),pattern = "^temp")])
  Sys.sleep(sample(20:60,1))
  remote_driver$deleteAllCookies()
  
}


remote_driver$closeall()
driver$server$stop()

