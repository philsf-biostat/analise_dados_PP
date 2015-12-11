## Leitura dos dados
dados<-read.csv("../dataset/dados_pp.csv")

## Conversao para fatores

# modelo de como criar os labels:
dados$SEXO<-factor(dados$SEXO, labels=c("M","F"))
dados$EST_CIV<-factor(dados$EST_CIV, labels = c("casado","solteiro","viúvo","não informa"))
dados$ESCOLAR<-factor(dados$ESCOLAR,labels = c("1o incompleto","1o completo","2o incompleto","2o completo","superior completo","superior incompleto","analfabeto","não informa"))
dados$MUNI_RES<-factor(dados$MUNI_RES)
dados$OCUPAC<-factor(dados$OCUPAC)
dados$RACA<-factor(dados$RACA)
dados$RENDA<-factor(dados$RENDA)
dados$OPC_SEX<-factor(dados$OPC_SEX,labels=c("hetero","homo","bi","ignorado"))
dados$HIV_AIDS<-factor(dados$HIV_AIDS, labels=c("HIV","AIDS"))

attach(dados)
## Preparacao dos graficos

## Frequencias
# http://www.statmethods.net/stats/frequencies.html
prop.table(table(SEXO))
prop.table(table(EST_CIV))
prop.table(table(ESCOLAR))
## prop.table(table(MUNI_RES))
## prop.table(table(OCUPAC))
## prop.table(table(RACA))
## prop.table(table(RENDA))
prop.table(table(OPC_SEX))
prop.table(table(HIV_AIDS))


# histograma das idades
png("idade_hist.png")
hist(IDADE, main="Histograma das idades", xlab="Idade", ylab="Quantidade")
dev.off()

# boxplot das idades
png("idade_boxplot.png")
boxplot(IDADE,main="Boxplot das idades",ylab="Idade")
dev.off()

# barplot do sexo
counts <- table(SEXO)
png("sexo-bp.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Sexo")
dev.off()

png("sexo-pizza.png")
pie(table(SEXO), col=c("darkblue","red"), main="Sexo")
dev.off()

slices<-table(SEXO)
lbls<-rownames(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels pie(slices,labels = lbs, col=rainbow(length(lbs)))
png("sexo-pizza-pct.png")
pie(slices,labels = lbls, col=c("darkblue","red"), main="Sexo")
dev.off()

library(plotrix)
png("sexo-pizza-3d.png")
pie3D(slices,labels=lbls,explode=0.1, main="Sexo")
dev.off()
detach("package:plotrix")

# barplot do estado civil x sexo
counts <- table(SEXO,EST_CIV)
png("est_civ-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Estado civil")
dev.off()
#pie(table(est_civ), main="Estado civil")

# barplot da escolaridade

counts<-table(SEXO,ESCOLAR)
png("escolaridade-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Escolaridade")
dev.off()

# pizza da escolaridade
slices<-table(ESCOLAR)
lbls<-rownames(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels pie(slices,labels = lbs, col=rainbow(length(lbs)))
png("escolaridade-pizza-pct.png")
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Escolaridade")
dev.off()
#pie3D(slices,labels=lbls,explode=0.1)

# barplot dos municípios
counts<-table(SEXO,muni_res)
png("muni_res-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Município de residência")
dev.off()

# barplot da ocupação
counts<-table(SEXO,ocupac)
png("ocupac-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Ocupação")
dev.off()

# barplot da raça
counts<-table(SEXO,raca)
png("raca-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Raça")
dev.off()

# barplot da renda
counts<-table(SEXO,renda)
png("renda-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Renda")
dev.off()

# barplot da opcao sexual x sexo
counts <- table(SEXO,OPC_SEX)
png("opc_sex-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Opção sexual")
dev.off()

# barplot do HIV x SEXO
counts <- table(SEXO,HIV_AIDS)
png("hiv_aids-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="HIV/AIDS")
dev.off()

## Sugesões: cruzando informações
# boxplot idade x SEXO
png("idade_sexo-boxplot.png")
boxplot(IDADE~SEXO,data=dados, main="Idade x Sexo")
dev.off()
png("idade_hiv-boxplot.png")
boxplot(IDADE~HIV_AIDS,data=dados, main="Idade x HIX/AIDS")
dev.off()
