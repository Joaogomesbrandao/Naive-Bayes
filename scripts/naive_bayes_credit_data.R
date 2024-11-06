### Aprendizagem Bayesiana ###

#-------- Pacotes utilizados
library(caret)
library(caTools)
library(ggplot2)
library(e1071)

# Leitura da base de dados
base = read.csv('credit_data.csv')

# Apaga a coluna clientid
base$clientid = NULL

# Valores inconsistentes
base$age = ifelse(base$age < 0, 40.92, base$age)

# Valores faltantes
base$age = ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)

# Escalonamento
base[, 1:3] = scale(base[, 1:3])

# Encode da classe
base$default = factor(base$default, levels = c(0,1))

# Divisão entre treinamento e teste
set.seed(1)
divisao = sample.split(base$income, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

classificador = naiveBayes(x = base_treinamento[-4],
                           y = base_treinamento$default)
print(classificador)
previsoes = predict(classificador, newdata = base_teste[-4])


matriz_confusao = table(base_teste[, 4], previsoes)
print(matriz_confusao)
confusionMatrix(matriz_confusao)

acuracia = (confusionMatrix(matriz_confusao)$overall['Accuracy']) * 100
print(acuracia)

#---------- Gerando gráfico com GGPLOT
# Gerando a matriz de confusão
matriz_confusao <- confusionMatrix(table(base_teste$default, previsoes))

# Convertendo para data frame para usar com ggplot
matriz_df <- as.data.frame(matriz_confusao$table)
names(matriz_df) <- c("Verdadeiro", "Previsto", "Frequencia")

# Criando o heatmap
ggplot(matriz_df, aes(x = Verdadeiro, y = Previsto, fill = Frequencia)) +
  geom_tile() +
  scale_fill_gradient(low = "pink", high = "violet") +
  labs(title = "Heatmap da Matriz de Confusão", x = "Valor Real", y = "Valor Previsto") +
  theme_minimal() +
  geom_text(aes(label = Frequencia), color = "black", size = 5) 
