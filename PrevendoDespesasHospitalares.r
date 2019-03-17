############################################# Machine Learning - Regressão - Prevendo Despesas Hospitalares ############################################

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
#setwd("{SET_YOUR_HOME_DIRECTORY_HERE}")
#setwd("{SET_YOUR_HOME_DIRECTORY_HERE}")
getwd()

# Problema de Negócio: Previsão de Despesas Hospitalares

# Para esta análise, vamos usar um conjunto de dados simulando despesas médicas hipotéticas 
# para um conjunto de pacientes espalhados por 4 regiões do Brasil.
# Esse dataset possui 1.338 observações e 7 variáveis.

# Observação Importante: Nesse estudo de caso não estamos separando os dados em Dataset de Treino e Teste.

####################################################################################################################################################################################
                                                    # Etapa 1 - Coletando os dados
####################################################################################################################################################################################
despesas <- read.csv("despesas.csv")
View(despesas)

####################################################################################################################################################################################
                                                    # Etapa 2: Explorando e Preparando os Dados
####################################################################################################################################################################################
# Visualizando as variáveis
str(despesas)

# Medias de Tendência Central da variável gastos
summary(despesas$gastos)

# Construindo um histograma
hist(despesas$gastos, 
    main = 'Histograma', 
    xlab = 'Gastos')

# Tabela de contingência das regiões
table(despesas$regiao)

# Explorando relacionamento entre as variáveis: Matriz de Correlação
cor(despesas[c("idade", "bmi", "filhos", "gastos")])

# Nenhuma das correlações na matriz é considerada forte, mas existem algumas associações interessantes. 
# Por exemplo, a idade e o bmi (IMC) parecem ter uma correlação positiva fraca, o que significa que 
# com o aumento da idade, a massa corporal tende a aumentar. Há também uma correlação positiva 
# moderada entre a idade e os gastos, além do número de filhos e os gastos. Estas associações implicam 
# que, à media que idade, massa corporal e número de filhos aumenta, o custo esperado do seguro saúde sobe. 

# Visualizando relacionamento entre as variáveis: Scatterplot
# Perceba que não existe um claro relacionamento entre as variáveis
pairs(despesas[c("idade", "bmi", "filhos", "gastos")])

# Scatterplot Matrix {Pacote bastente util para visualizar uma mátrix de gráficos}
#install.packages("psych")
library(psych)

# Este gráfico fornece mais informações sobre o relacionamento entre as variáveis
pairs.panels(despesas[c("idade", "bmi", "filhos", "gastos")])


####################################################################################################################################################################################
                                              # Etapa 3: Treinando o Modelo (No caso de uso não estamos separando em dados de treino e teste)
####################################################################################################################################################################################
# Lado Esquerdo do ~ -> Target (variavel dependente: Y)
# Lado Direito do ~ -> Variáveis preditores (Variavel independente (X))
# Data -> DataSet a ser analisado
# Regressão Linear Multiplas (+ de 1 Variavel Preditora)
modelo <- lm(gastos ~ ., data = despesas) # . representa todas variaveis

# Visualizando os coeficientes
modelo

# Prevendo despesas médicas utilizando modelos lineares
# Aqui verificamos os gastos previstos pelo modelo que devem ser iguais aos dados de treino
previsao1 <- predict(modelo)
View(previsao1)

# Prevendo os gastos com Dados de teste
despesasteste <- read.csv("despesas-teste.csv") #Dados que o modelo não conhece
View(despesasteste)

previsao2 <- predict(modelo, despesasteste)
View(previsao2)


####################################################################################################################################################################################
                                                              # Etapa 4: Avaliando a Performance do Modelo
####################################################################################################################################################################################
# Mais detalhes sobre o modelo
summary(modelo)

# *************************************************************************************
# *** Informações abaixo são de suma importância para compreender o modelo treinado ***
# *************************************************************************************

# Equação de Regressão
# y = a + bx (simples)
# y = a + b0x0 + b1x1 (múltipla)

# Resíduos
# Diferença entre os valores observados de uma variável e seus valores previstos
# Seus resíduos devem se parecer com uma distribuição normal, o que indica
# que a média entre os valores previstos e os valores observados é próximo de 0 (o que é bom)

# Coeficiente - Intercept - a (alfa) Representa onde a reta de regressão corta o eixo Y.
# Valor de a na equação de regressão

# Coeficientes - Nomes das variáveis - b (beta) Slope Inclinação da Reta
# Valor de b na equação de regressão

# Obs: A questão é que lm() ou summary() têm diferentes convenções de 
# rotulagem para cada variável explicativa. 
# Em vez de escrever slope_1, slope_2, .... 
# Eles simplesmente usam o nome da variável em qualquer saída para 
# indicar quais coeficientes pertencem a qual variável.

# Erro Padrão Std. Error
# Medida de variabilidade na estimativa do coeficiente a (alfa). O ideal é que este valor 
# seja menor que o valor do coeficiente, mas nem sempre isso irá ocorrer.

# Asteriscos 
# Os asteriscos representam os níveis de significância de acordo com o p-value.
# Quanto mais estrelas, maior a significância.
# Atenção --> Muitos astericos indicam que é improvável que não exista 
# relacionamento entre as variáveis.

# Valor t
# Define se coeficiente da variável é significativo ou não para o modelo. 
# Ele é usado para calcular o p-value e os níveis de significância.

# p-value  (Pr(>|t|))
# O p-value representa a probabilidade que a variável não seja relevante. 
# Deve ser o menor valor possível. 
# Se este valor for realmente pequeno, o R irá mostrar o valor 
# como notação científica

# Significância
# São aquelas legendas próximas as suas variáveis (Legenda dos asteriscos)
# Espaço em branco - ruim (Sem Relevância, pode-se remover do modelo)
# Pontos - razoável 
# Asteriscos - bom
# Muitos asteriscos - muito bom

# Residual Standar Error
# Este valor representa o desvio padrão dos resíduos

# Degrees of Freedom
# É a diferença entre o número de observações na amostra de treinamento 
# e o número de variáveis no seu modelo

# R-squared (coeficiente de determinação - R^2)
# Ajuda a avaliar o nível de precisão do nosso modelo. 
# Quanto maior, melhor, sendo 1 o valor ideal.

# F-statistics
# É o teste F do modelo. Esse teste obtém os parâmetros do nosso modelo 
# e compara com um modelo que tenha menos parâmetros.
# Em teoria, um modelo com mais parâmetros tem um desempenho melhor. 

# Se o seu modelo com mais parâmetros NÃO tiver perfomance
# melhor que um modelo com menos parâmetros, o valor do p-value será bem alto. 

# Se o modelo com mais parâmetros tiver performance
# melhor que um modelo com menos parâmetros, o valor do p-value será mais baixo.

# Lembre-se que correlação não implica causalidade


####################################################################################################################################################################################
                                                                  # Etapa 5: Otimizando a Performance do Modelo
####################################################################################################################################################################################
# Adicionando uma variável com o dobro do valor das idades
despesas$idade2 <- despesas$idade ^ 2

# Adicionando um indicador para BMI >= 30
#?ifelse
despesas$bmi30 <- ifelse(despesas$bmi >= 30, 1, 0) #Se for >=30 coloca 1, senão 0

View(despesas)

# Criando o modelo final
modelo_v2 <- lm(gastos ~ idade + idade2 + filhos + bmi + sexo +
                   (bmi30 * fumante) + regiao, data = despesas)


# É possível verificar que com as alterações dos inputs o modelo_v2 obteve uma melhor taxa de acerto (87%)
# em comparação ao modelo (75%)
summary(modelo_v2)
summary(modelo)

# Os ajustes realizados no dataset "despesas" para treinar o modelo, não foram realizados no dataset "despesas-teste"
# Para executar a previsão de valores nos dados do dataset "despesas-teste" deve-se incluir as novas colunas no dataset
# igual foi feito ao dataset "despesas".
despesasteste <- read.csv("despesas-teste.csv")
despesasteste$idade2 <- despesasteste$idade ^ 2

despesasteste$bmi30 <- ifelse(despesasteste$bmi >= 30, 1, 0) #Se for >=30 coloca 1, senão 0

previsaofinal <- predict(modelo_v2, despesasteste)
View(previsaofinal)

# Combinando o dataset "despesasteste" com a previsão resolvida pelo modelo linear
DataSetFinal <- cbind(despesasteste,previsaofinal)
View(DataSetFinal)