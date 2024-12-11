library(cluster)
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)
install.packages("factoextra")
library(factoextra)
library(dplyr)
install.packages("writexl")
library(writexl)
install.packages("openxlsx")
library(openxlsx)

# Limpando a base de dados (data cleaning), utilizando apenas as informações:
# precipitação máxima e precipitação média e criando um novo data frame.
banco_de_dados <- read_excel("C:/Users/junny/OneDrive/Área de Trabalho/projetos Rstúdio/Prec_med_with_coordinates.xlsx")


# Limpando a base de dados, removendo valores NULL
any(is.na(banco_de_dados))
colSums(is.na(banco_de_dados))

banco_de_dados_s_null <- banco_de_dados %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

df <- banco_de_dados_s_null[,c(3,5)]

# Definindo o número ótimo de clusters (maior precisão considerando o custo 
  # marginal de aumento de 1 cluster)
k <- 5

# Criando o gráfico de cotovelo a partir do data.frame "prec_media_final"
k.max <- 5
  data <- df
    wss <- sapply(1:k.max,function(k){kmeans(data, k, nstart=10,
                                         iter.max = 5)$tot.withinss})
wss

fviz_nbclust(data, kmeans, method = "wss")+
  geom_vline(xintercept = 5, linetype = 2)


# Clusterizando os dados
kmeans_result <- kmeans(df, centers = k)
  plot(df, col = kmeans_result$cluster, pch = 3)
    points(kmeans_result$centers, col = 1:k, pch = 2)
      print(kmeans_result)

# Informações importantes da clusterização
centroids <- kmeans_result$centers
  print(centroids)

                          
# Criando um novo data frame a partir do vetor de cluster
df_vetor_cluster <- data.frame(cluster = kmeans_result$cluster)


# Reorganizando o vetor cluster na ordem crescente de risco (A clusterização, separa os cluster apenas por similaridade, não seguindo uma ordem crescente ou decrescente, apenas )
df_reorganizado <- df_vetor_cluster %>%
mutate(across(everything(), ~ ifelse(. == 1, 9, .)))
    df_reorganizado_1 <- df_reorganizado %>%
    mutate(across(everything(), ~ ifelse(. == 2, 8, .)))
      df_reorganizado_2 <- df_reorganizado_1 %>%
      mutate(across(everything(), ~ ifelse(. == 3, 10, .)))
        df_reorganizado_3 <- df_reorganizado_2 %>%
        mutate(across(everything(), ~ ifelse(. == 4, 6, .)))
          df_reorganizado_4 <- df_reorganizado_3 %>%
          mutate(across(everything(), ~ ifelse(. == 5, 7, .)))
            df_vetor_cluster <- df_reorganizado_4 %>%
            mutate(across(everything(), ~ . - 5))

# Criando um data frame para concatenar precipitação com o cluster especifico de cada ID
df_cluster <- data.frame(df = df, risco = df_vetor_cluster)



#criando um data frame para receber o ID de cada rio e em seguida concatenando o
# ID com os dados da clusterização
y <- Prec_med_with_coordinates[c(1,7,8)]
  df_con_y <- cbind(df_cluster, y)
    colnames(df_con_y)[1] <- "Precipitação média" 
      colnames(df_con_y)[2] <- "Precipitação máxima"
        colnames(df_con_y)[3] <- "cluster"
          colnames(df_con_y)[4] <- "ID"

# Alterando a variável quantidade cluster para uma variável qualitativa "Risco"
dados <- df_con_y %>%
            mutate(Risco = case_when(
                cluster == 1 ~ "Muito baixo",
                  cluster == 2 ~ "Baixo",
                    cluster == 3 ~ "Médio",
                      cluster == 4 ~ "Alto",
                         cluster == 5 ~ "Muito alto",
                            TRUE ~ NA_character_))
          
                    
# Exportando planilha para o excel no formato .xlsx
write.xlsx(dados, "C:/Users/junny/OneDrive/Área de Trabalho/Excel_vindo_do_R.xlsx")
