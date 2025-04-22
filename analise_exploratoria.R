#LucasXavier_RM563436_fase2_cap7


# Leitura dos dados

if(!require(readxl)) install.packages("readxl")
library(readxl)

url = "C://Fase2_trabalho3//Base_de_Dados_Cana_de_Acucar.xlsx"
df = read_excel(url)


# Exame dos dados

df$Ano = as.factor(df$Ano)
df$`Produção (milhões de toneladas)` = as.numeric(df$`Produção (milhões de toneladas)`)
df$`Nível Tecnológico` = as.factor(df$`Nível Tecnológico`)


print("Análise geral da tabela: ")
head(df)          #mostra as primeiras linhas
dim(df)           #mostra as dimensões da tabela
summary(df)       #mostra um resumo dos dados da tabela

summary(df$`Produção (milhões de toneladas)`)




# Váriavel Quantitativa Contínua: PRODUÇÃO
# Medidas de Tendência Central
# Medidas Separatrizes
# Análise gráfica


if(!require(DescTools)) install.packages("DescTools")
library(DescTools)

producao = df$`Produção (milhões de toneladas)`

# Calculando a Mediana:
mediana_producao <-median(producao)

# Calculando a Moda: Baixar e habilitar o pacote DescTools
moda_producao <-Mode(producao)

# Máximo:
máximo_producao <-max(producao)

#Mínimo:
mínimo_producao <-min(producao)

# Calculando a Amplitude:
amplitude_producao <- diff(range(producao))
                        
# Calculando a Variância:
variancia_producao <-var(producao)

# Calculando o Desvio Padrão:
desvio_padrao_producao <-sd(producao)

# Calculando o Coeficiente de Variação:
cv_producao <-(sd(producao)/mean(producao))*100

# Calculando os Quartis:
quartis_producao <-quantile(producao, probs=c(0.25, 0.50, 0.75))

# Calculando os Decis:
decis_producao <-quantile(producao, probs = seq(0.1, 0.9, by = 0.1))

# Calculando os Centis:
centis_producao <-quantile(producao, probs = seq(0.01, 0.99, by = 0.01))

cat("Mediana:", mediana_producao, "\n")
cat("Moda:", moda_producao, "\n")
cat("Máximo:", máximo_producao, "\n")
cat("Mínimo:", mínimo_producao, "\n")
cat("Amplitude:", amplitude_producao, "\n")
cat("Variância:", variancia_producao, "\n")
cat("Desvio Padrão:", desvio_padrao_producao, "\n")
cat("Coeficiente de Variação (%):", cv_producao, "\n")
cat("Quartis:\n")
print(quartis_producao)
cat("Decis:\n")
print(decis_producao)
cat("Centis:\n")
print(centis_producao)

x1 = df$Ano
x2 = df$`Nível Tecnológico`
x3 = df$`Dias de Chuva (estimado)`
y = df$`Produção (milhões de toneladas)`

#plotagem de y versus x1, x2 e x3

plot(x1, y, pch=21, bg='red', cex=1,
     ylim = c(220,461),
     xlab = "x = Ano",
     ylab = "y = Produção (milhões de toneladas)")

plot(x2, y, pch=21, bg='blue', cex=1,
     ylim = c(220,461),
     xlab = "x = Nível Tecnológico",
     ylab = "y = Produção (milhões de toneladas)")

plot(x3, y, pch=21, bg='green', cex=1,
     ylim = c(220,461),
     xlab = "x = Dias de Chuva",
     ylab = "y = Produção (milhões de toneladas)")



# Variável Qualitativa Discreta: ANO

x = df$Ano
y1 = df$`Produção (milhões de toneladas)`
y2 = df$`Nível Tecnológico`
y3 = df$`Dias de Chuva (estimado)`

plot(x, y1, pch=21, bg='red', cex=1,
     xlab = "x = Ano",
     ylab = "y = Produção (milhões de toneladas)")

plot(x, y2, pch=21, bg='blue', cex=1,
     xlab = "x = Ano",
     ylab = "y = Nível Tecnológico")

plot(x, y3, pch=21, bg='green', cex=1,
     xlab = "x = Ano",
     ylab = "y = Dias de Chuva")