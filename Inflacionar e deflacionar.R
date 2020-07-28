install.packages("devtools")
devtools::install_github("tbrugz/ribge")
library(ribge)
library(data.table)

# # Limpar o ambiente
# rm(list = ls())
# gc()

# atribuindo vetor vazio para sobrescrever
ipca <- c() 

#selecionando ano de 1991 a 2017
periodo <- 1991:2017 
for (i in periodo){ 
  x <- precos_ipca(i)
  ipca <<- rbind(ipca,x)
  rm(x)
}


# Função para criar inflator/deflator
deflator <- function(ano,mes,grupo){
  
  # Opção para não abrir debug de erro
  #options(error=NULL)
  
  # Atribuir ano ao objeto "a"  
  a <- ano
  # Criar regra para limitar o cálculo somente para os anos disponíveis na base
  a <- ifelse(a < min(ipca$ano) | a > max(ipca$ano), NA, a )
  if(any(is.na(a))) 
    # Interromper caso um ano fora do range for escolhido
    stop(paste0('Dados disponíveis apenas para os anos de ',min(ipca$ano),' a ',
                max(ipca$ano)))
  # Atribuir o mês ao objeto "m"
  m <- mes
  # Criar regra para limitar o cálculo somente para os anos disponíveis na base
  m <- ifelse(m < min(ipca$mes) | m > max(ipca$mes), NA, m )
  # Testar se o mês escolhido é válido
  if(any(is.na(m))) 
    # Interromper caso um mês fora do range for escolhido   
    stop(paste0('Dados disponíveis apenas para os meses de ',min(ipca$mes),' a ',
                max(ipca$mes)))
  
  # Criar uma variável de referência, indicando o mês e o ano considerado
  ipca <- transform(ipca,referencia= ano*100+mes)
  
  # Testar se a referência é válida
  if(a*100+m>max(ipca$referencia))
    stop(paste0('Dados disponíveis até o mês ',substr(max(ipca$referencia),5,6),' de ',
                substr(max(ipca$referencia),1,4)))
  
  # Atribuir o grupo ao objeto "g"
  g <- grupo
  # Criar regra para limitar o cálculo somente para os grupos disponíveis na base
  g <- ifelse(g < min(ipca$categoria) | g > max(ipca$categoria), NA, g )
  # Testar se a categoria escolhida é válida
  if(any(is.na(g))) 
    # Interromper caso uma categoria fora do range for escolhida  
    stop(paste0('Dados disponíveis apenas para as categorias de ',min(ipca$categoria),' a ',
                max(ipca$categoria)))
  
  # Recortar a base para o grupo selecionado
  ipca <- ipca[ipca$categoria==g,]
  
  # Criar o índice de cada mês de inflação
  ipca <- transform (ipca, indice = ipca$valor/100+1)
  
  
  # Criar uma lista com os valores acima da referência, a serem deflacionados
  list_defla <- ipca[ipca$referencia>a*100+m,]$referencia
  
  # Criar um objeto para receber os deflatores/inflatores
  infla_defla <- c()
  
  # Iniciar o loop para calcular o deflator de cada referência
  for (i in list_defla) {
    
    # Calcular o deflator da referência
    temp <- 1/prod(ipca[with(ipca,referencia<=i&referencia>=a*100+m),"indice"])
    # Criar um objeto para receber temporário o deflator com a referência
    l <- c()
    # Juntar a referência com o deflator
    l <- cbind(i,temp)
    # Adicionar o objeto criado ao objeto de deflatores/inflatores
    infla_defla <- rbind(infla_defla,l)
    # Limpar o objeto temporário para cálculo da próxima referência do loop
    l <- c()
  }
  
  # Criar uma lista com as referências a serem calculados os inflatores
  list_infla <- ipca[ipca$referencia<a*100+m,]$referencia
  
  # Iniciar o loop para calcular o inflator de cada referência
  for (i in list_infla) {
    # Calcular o inflator da referência
    temp <- prod(ipca[with(ipca,referencia>=i&referencia<=a*100+m),"indice"])
    # Criar um objeto para receber temporário o deflator com a referência
    l <- c()
    # Juntar a referência com o deflator
    l <- cbind(i,temp)
    # Adicionar o objeto criado ao objeto de deflatores/inflatores
    infla_defla <- rbind(infla_defla,l)
    # Limpar o objeto temporário para cálculo da próxima referência do loop
    l <- c()
  }
  # Criar o inflator/deflator do ano base = 1
  infla_defla <- rbind(infla_defla,cbind(a*100+m,1))
  # Ajustar o nome das variáveis para o merge com a base do ipca original
  colnames(infla_defla) <- c("referencia","infla_defla")
  # Criar o objeto para receber a base do ipca com os inflatores/deflatores
  ipca_id <- c()
  # Unir a base do ipca aos deflatores
  ipca_id <- merge(ipca,infla_defla,all.x = T) 
  # Excluir as colunas de referência e do indicador, criadas anteriormente
  ipca_id <- ipca_id[,c(-1,-7)]
  #print(ipca_id)
}

