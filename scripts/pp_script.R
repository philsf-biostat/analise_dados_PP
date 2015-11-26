## Leitura dos dados
dados<-read.csv("../dataset/dados_pp.csv")

## Conversao para fatores
idade<-dados$IDADE
#sexo<-factor(dados$SEXO)
# modelo de como criar os labels:
sexo<-factor(dados$SEXO, labels=c("M","F"))
est_civ<-factor(dados$EST_CIV, labels = c("casado","solteiro","viúvo","não informa"))
escolar<-factor(dados$ESCOLAR,labels = c("1o incompleto","1o completo","2o incompleto","2o completo","superior completo","superior incompleto","analfabeto","não informa"))
muni_res<-factor(dados$MUNI_RES)
ocupac<-factor(dados$OCUPAC)
raca<-factor(dados$RACA)
renda<-factor(dados$RENDA)
opc_sex<-factor(dados$OPC_SEX,labels=c("hetero","homo","bi","ignorado"))
hiv_aids<-factor(dados$HIV_AIDS, labels=c("HIV","AIDS"))

## Preparacao dos graficos

# histograma das idades
png("idade_hist.png")
hist(idade, main="Histograma das idades", xlab="Idade", ylab="Quantidade")
dev.off()

# boxplot das idades
png("idade_boxplot.png")
boxplot(idade,main="Boxplot das idades",ylab="Idade")
dev.off()

# barplot do sexo
counts <- table(sexo)
png("sexo-bp.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Sexo")
dev.off()

png("sexo-pizza.png")
pie(table(sexo), main="Sexo")
dev.off()

slices<-table(sexo)
lbls<-rownames(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels pie(slices,labels = lbs, col=rainbow(length(lbs)))
png("sexo-pizza-pct.png")
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Sexo")
dev.off()

library(plotrix)
png("sexo-pizza-3d.png")
pie3D(slices,labels=lbls,explode=0.1, main="Sexo")
dev.off()
detach("package:plotrix")

# barplot do estado civil x sexo
counts <- table(sexo,est_civ)
png("est_civ-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Estado civil")
dev.off()
#pie(table(est_civ), main="Estado civil")

# barplot da escolaridade
counts<-table(sexo,escolar)
png("escolaridade-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Escolaridade")
dev.off()

# pizza da escolaridade
slices<-table(escolar)
lbls<-rownames(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels pie(slices,labels = lbs, col=rainbow(length(lbs)))
png("escolaridade-pizza-pct.png")
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Escolaridade")
dev.off()
#pie3D(slices,labels=lbls,explode=0.1)

# barplot dos municípios
counts<-table(sexo,muni_res)
png("muni_res-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Município de residência")
dev.off()

# barplot da ocupação
counts<-table(sexo,ocupac)
png("ocupac-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Ocupação")
dev.off()

# barplot da raça
counts<-table(sexo,raca)
png("raca-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Raça")
dev.off()

# barplot da renda
counts<-table(sexo,renda)
png("renda-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Renda")
dev.off()

# barplot da opcao sexual x sexo
counts <- table(sexo,opc_sex)
png("opc_sex-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="Opção sexual")
dev.off()

# barplot do HIV x sexo
counts <- table(sexo,hiv_aids)
png("hiv_aids-barplot.png")
barplot(counts, col=c("darkblue","red"), legend = rownames(counts), main="HIV/AIDS")
dev.off()

## Sugesões: cruzando informações
# boxplot idade x sexo
png("idade_sexo-boxplot.png")
boxplot(idade~sexo,data=dados, main="Idade x Sexo")
dev.off()
png("idade_hiv-boxplot.png")
boxplot(idade~hiv_aids,data=dados, main="Idade x HIX/AIDS")
dev.off()
