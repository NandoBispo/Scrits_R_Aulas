# PACOTES ----

if(!require("pacman"))install.packages("pacman")

# O pacote qualityTools não está mais disponível no CRAN.

pacman::p_load(qcc, spc, SixSigma, IQCC, MSQC, MPCI, qicharts, Rspc, ggQC)

pacman::p_load(tidyverse, readxl, summarytools, kableExtra)


# VERSIONAMENTO ----
# https://curso-r.githud.io/zen-do-r/git-githud.html
# gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_github()
# _________________________________________________


# AULAS ----
## A5_SEG_03/04/23 ----
### EX1 ----
# x = vetor com as frequências totais de cada tipo categoria. 
# Use o comando names(x) para rotular as categorias.

# ____________________________________________________________________________
max(nchar(names(x)))

names(x)|>
  nchar()

names(x)|>
  stringr::str_length()|>
  max() #captura a quantidade mais longa de caracteres presentes em um dos nomes

sort(x, decreasing=T) # Organiza uma lista por ordem decrescente
# ____________________________________________________________________________

G.Pareto=function(x){
  m = max(nchar(names(x))) # Captura a quantidade mais longa de caracteres presentes em um dos nomes
  if(m<=2) aux = 0 else aux=3
  if(aux==0){
    a=4.5
    b=4.5
  }
  if(aux==3){
    a=9.5
    b=4
  }
  
  x=sort(x,decreasing=TRUE) # Organiza uma lista por ordem decrescente
  soma.acum=cumsum(x)
  porc.acum=seq(0,100,25)
  quantis=quantile(seq(0,max(soma.acum)),porc.acum/100)
  op=par(mar=c(a, 4.5, b, 4.5),las=aux) # Configura os parâmetros de margem do gráfico do RBase.
  gb=barplot(x,main="Gráfico de Pareto",ylim=c(0,max(soma.acum)*1.05),col="blue",border="blue")
  abline(h=quantis,col="lightgrey",lty=3)
  rect(gb-0.5,rep(0,length(x)),gb+0.5,x,col="blue",border="blue")
  lines(gb+0.5,soma.acum,type="b",col="red",cex=0.7,pch=16)
  axis(4,at=quantis,las=3,labels=paste(porc.acum,"%",sep=""))
  mtext(c("Frequência","Porcentagem Acumulada"),side=c(2,4),line=2.5)
  box()
  tabela=round(cbind(x, soma.acum, x/max(soma.acum)*100, soma.acum/max(soma.acum)*100), 2)
  colnames(tabela)=c("Frequência","Freq.Acum.","Porcentagem","Porcent.Acum.")
  return(as.table(tabela))
  par(op)
}

### Exemplo: tipos de defeitos encontrados em embalagens de barras de chocolate
x = c(322, 21, 145, 67, 10, 53, 10)
names(x) = c("Mal selada", "Rasgada","Pequenos furos","Data validade em falta","Cores esborratadas","Barra partida","Outros")

G.Pareto(x)

### EX2 ----
# x = lista com as causas principais e secund�rias (subcausas)
# y = variável string com o nome do efeito a ser analisado

Ishikawa=function(x,y){
  
  m=length(x)
  m1=m-round(m/2)
  m2=m-m1
  if(m1<m2){
    v1=m1; v2=m2
    m1=v2; m2=v1
  }
  nmax=max(sapply(x,length))
  op=par(mar=c(1,1,3,1),no.readonly=TRUE)
  on.exit(par(op))
  plot(0:50,0:50,type="n",xlab="",ylab="",axes=FALSE,main="Diagrama de causa e efeito")
  u=par("usr")
  le=strwidth(y,units="user")*1.1
  lc=max(unlist(sapply(x,strwidth,units="user")))
  ac=max(strheight(y,units="user"))
  arrows(0,25,u[2]-le-1,25,code=2,length=0.1,angle=25)
  text(u[2]-le,25,y,adj=c(0,0.35),cex=0.95,font=2)
  a=(u[2]-le-1)/(max(m1,m2)+1)
  a2=a*(0:(max(m1,m2)))
  media2=function(k){
    m2=rep(0,length(k)-1)
    for (i in 1:(length(k)-1)) m2[i]=mean(k[c(i,i+1)])
    return(m2)
  }
  for (i in 1:(length(a2)-1)){
    segments(media2(a2)[i],45,a2[i+1],25)
    text(media2(a2)[i],45.5,names(x)[i],pos=3,offset=0.5,cex=0.85,font=2,col="red")
    if (i<=m2){
      segments(media2(a2)[i],5,a2[i+1],25)
      text(media2(a2)[i],4.5,names(x)[[m1+i]],pos=1,offset=0.5,cex=0.85,font=2,col="red")
    }
  }
  for (i in 1:m1){
    beta=(25-45)/(a2[i+1]-media2(a2)[i])
    alpha=45-(beta*media2(a2)[i])
    y2=sort(25+(cumsum((45-25)/(nmax+1))*(1:(nmax))),decreasing=TRUE)
    x2=(y2-alpha)/beta
    for (j in 1:length(y2)){
      nomes=x[[i]][j]
      if (!is.na(nomes)) 
        text(x2[j],y2[j],nomes,pos=4,offset=0.2,cex=0.75,font=4,col="blue")
    }
  }
  for (i in 1:m1){
    beta=(25-5)/(a2[i+1]-media2(a2)[i])
    alpha=5-(beta*media2(a2)[i])
    y2=5+(cumsum((45-25)/(nmax+1))*(1:(nmax)))
    x2=(y2-alpha)/beta
    if (i<=m2) 
      for (j in 1:length(y2)) {
        nomes=x[[m1+i]][j]
        if (!is.na(nomes)) 
          text(x2[j],y2[j],nomes,pos=4,offset=0.2,cex=0.75,font=4,col="blue")
      }
  }
  invisible()
}



### Exemplo - Problema da má selagem das embalagens de barras de chocolate

x=list("Máquina"=c("Má regulagem da temperatura de selagem","Seladora pressiona a barra para fora da embalagem"),
       "Mão-de-obra"=c("Treinamento insuficiente","Funcionários não habituados a executar a função"),
       "Método"=c("Produção elevada","Excesso de peso nas embalagens"),
       "Matéria-prima"=c("Dimensionamento das embalagens"))

y="Mal selada"

Ishikawa(x,y)

## A6_QUA_05/04/23 ----

# m = número de amostras observadas.
# n = número de observações de cada amostra.
# y = vetor com as observações, ordenadas por amostra, da variável observada.
# mi = especificação para a média do processo.
# sigma = especificação para o desvio-padrão do processo.
xR1=function(m,n,y,mi,sigma){
  if (n>1) {
    amplit=numeric(m)
    mat=matrix(y,m,n,byrow=T)
    media=apply(mat,1,mean)
    for (i in 1:m) {
      amplit[i]=max(mat[i,])-min(mat[i,])
    }
  } else {
    amplit=numeric(m-1)
    media=y
    for (i in 2:m) amplit[i-1]=sqrt((y[i]-y[i-1])^2)
  }
  d2=c(1.1296,1.6918,2.0535,2.3248,2.5404,2.7074,
       2.8501,2.9677,3.0737,3.1696)
  d3=c(.8541,.8909,.8800,.8674,.8508,
       .8326,.8209,.8102,.7978,.7890)
  LIC1=mi-3*sigma/sqrt(n)
  LSC1=mi+3*sigma/sqrt(n)
  LAIC1=mi-2*sigma/sqrt(n)
  LASC1=mi+2*sigma/sqrt(n)
  LAIC1.C=mi-sigma/sqrt(n)
  LASC1.C=mi+sigma/sqrt(n)
  if(n>1) {
    LIC2=d2[n-1]*sigma-3*d3[n-1]*sigma
    LSC2=d2[n-1]*sigma+3*d3[n-1]*sigma
    if (LIC2 < 0) LIC2=0
    LAIC2=d2[n-1]*sigma-2*d3[n-1]*sigma #Limite de advertência inferior de controle
    LASC2=d2[n-1]*sigma+2*d3[n-1]*sigma
    if (LAIC2 < 0) LAIC2=0
    LAIC2.C=d2[n-1]*sigma-d3[n-1]*sigma
    LASC2.C=d2[n-1]*sigma+d3[n-1]*sigma
    if (LAIC2.C < 0) LAIC2.C=0
  } else {
    LIC2=d2[n]*sigma-3*d3[n]*sigma
    LSC2=d2[n]*sigma+3*d3[n]*sigma
    if (LIC2 < 0) LIC2=0
    LAIC2=d2[n]*sigma-2*d3[n]*sigma
    LASC2=d2[n]*sigma+2*d3[n]*sigma
    if (LAIC2 < 0) LAIC2=0
    LAIC2.C=d2[n]*sigma-d3[n]*sigma
    LASC2.C=d2[n]*sigma+d3[n]*sigma
    if (LAIC2.C < 0) LAIC2.C=0
  }
  par(mfrow=c(2,1))
  amostra1=matrix(rep(seq(1,m),4),m,4,byrow=F)
  if (n>1) amostra2=amostra1 else amostra2=amostra1[-1,]
  xbar=cbind(rep(mi,m),rep(LIC1,m),rep(LSC1,m),rep(LAIC1,m),rep(LASC1,m),rep(LAIC1.C,m),rep(LASC1.C,m),media)
  if (n>1) {
    rbar=cbind(rep(sigma,m),rep(LIC2,m),rep(LSC2,m),rep(LAIC2,m),rep(LASC2,m),rep(LAIC2.C,m),rep(LASC2.C,m),amplit)
  } else rbar=cbind(rep(sigma,m-1),rep(LIC2,m-1),
                    rep(LSC2,m-1),amplit)
  matplot(amostra2,rbar,type="o",ylab="Amplitude",
          col=c("black","red","red", "orange", "orange","gray","gray","blue"),
          ylim=c(min(rbar),max(rbar)),
          xlab="Amostras",main="Gráfico R",pch=20, lty=1, lwd=2)
  matplot(amostra1,xbar,type="o",ylab="Média",xlab="Amostras",
          main=expression(paste("Gráfico " *
                                  bar(x))),col=c("black","red","red","orange",
                                                 "orange","gray","gray","blue"),
          ylim=c(min(xbar),max(xbar)),pch=20, lty=1, lwd=2)
  
  # out=rbind(round(xbar[1,1:3],2),round(rbar[1,1:3],2))
  # colnames(out)=c("Referência","LIC","LSC")
  out <<-  rbind(round(rbar[1, 1:5], 2), round(xbar[1, 1:5], 2))
  colnames(out) <<- c("Referência", "LIC", "LSC", "LAIC", "LASC")
  rownames(out) <<- c("R", "Xbarra")

  # return(out)
}


# m = número de amostras observadas. 
# n = número de observações de cada amostra.
# y = vetor com as observações, ordenadas por amostra, da variável observada.

numeric(5) # Cria um vetor de zeros do tamanho especificado.

xR2=function(m,n,y){
  if (n>1) {
    amplit=numeric(m) 	
    mat=matrix(y,m,n,byrow=T)
    media=apply(mat,1,mean)
    for (i in 1:m) amplit[i]=max(mat[i,])-min(mat[i,])
    x2barras=mean(media)
    Rbarra=mean(amplit) 
    d2=c(1.1296,1.6918,2.0535,2.3248,2.5404,2.7074,2.8501,
         2.9677,3.0737,3.1696)
    d3=c(.8541,.8909,.8800,.8674,.8508,.8326,.8209,
         .8102,.7978,.7890)
    LIC1=x2barras-3*Rbarra/(d2[n-1]*sqrt(n))
    LSC1=x2barras+3*Rbarra/(d2[n-1]*sqrt(n))
    LIC2=Rbarra-3*d3[n-1]*Rbarra/d2[n-1]
    LSC2=Rbarra+3*d3[n-1]*Rbarra/d2[n-1]
    if (LIC2 < 0) LIC2=0
 
  } else {
    amplit=numeric(m-1)
    media=y
    for (i in 2:m) amplit[i-1]=sqrt((y[i]-y[i-1])^2)
    x2barras=mean(media)
    Rbarra=mean(amplit) 
    d2=1.1296
    d3=.8541
    LIC1=x2barras-3*Rbarra/(d2*sqrt(n))
    LSC1=x2barras+3*Rbarra/(d2*sqrt(n))
    LIC2=Rbarra-3*d3*Rbarra/d2
    LSC2=Rbarra+3*d3*Rbarra/d2
    if (LIC2 < 0) LIC2=0
  }
  par(mfrow=c(2,1))
  amostra1=matrix(rep(seq(1,m),4),m,4,byrow=F)
  if (n>1) amostra2=amostra1 else amostra2=amostra1[-1,]
  xbar=cbind(rep(x2barras,m),rep(LIC1,m),rep(LSC1,m),media)
  if (n>1) {
    rbar=cbind(rep(Rbarra,m),rep(LIC2,m),rep(LSC2,m),amplit) 
  } else rbar=cbind(rep(Rbarra,m-1),rep(LIC2,m-1),
                    rep(LSC2,m-1),amplit)
  matplot(amostra2,rbar,type="o",ylab="Amplitude",
          col=c("black","red","red", "blue"),
          ylim=c(min(rbar),max(rbar)),
          xlab="Amostras",main="Gráfico R",pch=20, lty=1, lwd=2)
  matplot(amostra1,xbar,type="o",ylab="Média",xlab="Amostras",
          main=expression(paste("Gráfico " * bar(x))),col=c("black","red","red",
                                                            "blue"),ylim=c(min(xbar),max(xbar)),pch=20, lty=1, lwd=2)
  
  # out=rbind(round(xbar[1,1:3],2),round(rbar[1,1:3],2))
  # colnames(out)=c("Referência","LIC","LSC")
  out <<- rbind(round(rbar[1,1:3],2), round(xbar[1,1:3],2))
  colnames(out) <<- c("Referência","LIC","LSC")
  rownames(out) <<- c("R", "Xbarra")
  # out
}


xR2.2=function(m,n,y){
  if (n>1) {
    amplit=numeric(m) 	
    mat=matrix(y,m,n,byrow=T)
    media=apply(mat,1,mean)
    for (i in 1:m) amplit[i]=max(mat[i,])-min(mat[i,])
    x2barras=mean(media)
    Rbarra=mean(amplit) 
    d2=c(1.1296,1.6918,2.0535,2.3248,2.5404,2.7074,2.8501,
         2.9677,3.0737,3.1696)
    d3=c(.8541,.8909,.8800,.8674,.8508,.8326,.8209,
         .8102,.7978,.7890)
    LIC1=x2barras-3*Rbarra/(d2[n-1]*sqrt(n))
    LSC1=x2barras+3*Rbarra/(d2[n-1]*sqrt(n))
    LIC2=Rbarra-3*d3[n-1]*Rbarra/d2[n-1]
    LSC2=Rbarra+3*d3[n-1]*Rbarra/d2[n-1]
    if (LIC2 < 0) LIC2=0
    
    LAIC1=x2barras-2*Rbarra/(d2[n-1]*sqrt(n))
    LASC1=x2barras+2*Rbarra/(d2[n-1]*sqrt(n))
    LAIC2=Rbarra-2*d3[n-1]*Rbarra/d2[n-1]
    LASC2=Rbarra+2*d3[n-1]*Rbarra/d2[n-1]
    if (LAIC2 < 0) LAIC2=0
    
  } else {
    amplit=numeric(m-1)
    media=y
    for (i in 2:m) amplit[i-1]=sqrt((y[i]-y[i-1])^2)
    x2barras=mean(media)
    Rbarra=mean(amplit) 
    d2=1.1296
    d3=.8541
    LIC1=x2barras-3*Rbarra/(d2*sqrt(n))
    LSC1=x2barras+3*Rbarra/(d2*sqrt(n))
    LIC2=Rbarra-3*d3*Rbarra/d2
    LSC2=Rbarra+3*d3*Rbarra/d2
    if (LIC2 < 0) LIC2=0
    
    LAIC1=x2barras-2*Rbarra/(d2*sqrt(n))
    LASC1=x2barras+2*Rbarra/(d2*sqrt(n))
    LAIC2=Rbarra-2*d3*Rbarra/d2
    LASC2=Rbarra+2*d3*Rbarra/d2
    if (LAIC2 < 0) LAIC2=0
  }
  par(mfrow=c(2,1))
  amostra1=matrix(rep(seq(1,m),4),m,4,byrow=F)
  if (n>1) amostra2=amostra1 else amostra2=amostra1[-1,]
  xbar=cbind(rep(x2barras,m),rep(LIC1,m),rep(LSC1,m),rep(LAIC1,m),rep(LASC1,m),media)
  if (n>1) {
    rbar=cbind(rep(Rbarra,m),rep(LIC2,m),rep(LSC2,m),rep(LAIC2,m),rep(LASC2,m),amplit) 
  } else rbar=cbind(rep(Rbarra,m-1),rep(LIC2,m-1),
                    rep(LSC2,m-1),amplit)
  matplot(amostra2,rbar,type="o",ylab="Amplitude",
          col=c("black","red","red","orange","orange","blue"),
          ylim=c(min(rbar),max(rbar)),
          xlab="Amostras",main="Gráfico R",pch=20, lty=1, lwd=2)
  matplot(amostra1,xbar,type="o",ylab="Média",xlab="Amostras",
          main=expression(paste("Gráfico " * bar(x))),col=c("black","red","red","orange","orange",
                                                            "blue"),ylim=c(min(xbar),max(xbar)),pch=20, lty=1, lwd=2)
  
  # out=rbind(round(xbar[1,1:3],2),round(rbar[1,1:3],2))
  # colnames(out)=c("Referência","LIC","LSC")
  out <<- rbind(round(rbar[1,1:5],2), round(xbar[1,1:5],2))
  colnames(out) <<- c("Referência","LIC","LSC", "LAIC", "LASC")
  rownames(out) <<- c("R", "Xbarra")
  # out
}

### Exemplo

m=25; n=4; mn=m*n; mi=100; sigma=10
set.seed(9962); x=round(rnorm(mn,mi,sigma),2); x[77:80]=x[77:80]+3*sigma 

xR2(m,n,x)



## A7_SEG_10/04/2023 ----

# m = número de amostras observadas.
# n = número de observações de cada amostra.
# y = vetor com as observações, ordenadas por amostra, da variável observada.
# mi = especificação para a média do processo.
# sigma = especificação para o desvio-padrão do processo.

xS1=function(m,n,y,mi,sigma){
  if (length(n)==1) n=rep(n,m)
  mat=matrix(NA,m,max(n)); k=1
  for (i in 1:m) {
    for (j in 1:n[i]) {
      mat[i,j]=y[k]
      k=k+1
    }
  }
  media=apply(mat,1,mean,na.rm=T)
  desviop=apply(mat,1,sd,na.rm=T)
  LIC1=numeric(m)
  LSC1=numeric(m)
  LIC2=numeric(m)
  LSC2=numeric(m)
  for (i in 1:m){
    LIC1[i]=mi-3*sigma/sqrt(n[i])
    LSC1[i]=mi+3*sigma/sqrt(n[i])
    c4=(gamma(n[i]/2)/gamma((n[i]-1)/2))*sqrt(2/(n[i]-1))
    LIC2[i]=c4*sigma-3*sigma*sqrt((1-c4^2))
    LSC2[i]=c4*sigma+3*sigma*sqrt((1-c4^2))
    if (LIC2[i] < 0) LIC2[i]=0
  }
  par(mfrow=c(2,1))
  amostra=matrix(rep(seq(1,m),4),m,4,byrow=F)
  xbar=cbind(rep(mi,m),LIC1,LSC1,media)
  rbar=cbind(rep(sigma,m), LIC2, LSC2, desviop)
  matplot(amostra, rbar, type="o", ylab="Desvio-Padrão",
          col = c("black","red","red","blue"),
          ylim = c(min(rbar),max(rbar)),
          xlab = "Amostras",main="Gráfico S", pch=20, lty=1, lwd=2)
  matplot(amostra,xbar,type="o",ylab="Média",xlab="Amostras",
          main = expression(paste("Gráfico " * bar(x))),
          col = c("black","red","red","blue"),
          ylim = c(min(xbar), max(xbar)), pch=20, lty=1, lwd=2)
}


# m = número de amostras observadas. 
# n = número de observações de cada amostra.
# y = vetor com as observações, ordenadas por amostra, da variável observada.

xS2=function(m,n,y){
  if (length(n)==1) n=rep(n,m)
  mat=matrix(NA,m,max(n)); k=1 
  for (i in 1:m) {
    for (j in 1:n[i]) {
      mat[i,j]=y[k]
      k=k+1
    }
  }
  media=apply(mat,1,mean,na.rm=T)
  desviop=apply(mat,1,sd,na.rm=T)
  x2barras=sum(n*media)/sum(n)
  Sbarra=sqrt(sum((n-1)*desviop^2)/(sum(n)-m))
  LIC1=numeric(m)
  LSC1=numeric(m)
  LIC2=numeric(m)
  LSC2=numeric(m)
  c4=0	
  for (i in 1:m){ 
    c4=(gamma(n[i]/2)/gamma((n[i]-1)/2))*sqrt(2/(n[i]-1))
    LIC1[i]=x2barras-3*Sbarra/(c4*sqrt(n[i]))
    LSC1[i]=x2barras+3*Sbarra/(c4*sqrt(n[i]))
    LIC2[i]=Sbarra-3*Sbarra*sqrt((1-c4^2))/c4
    LSC2[i]=Sbarra+3*Sbarra*sqrt((1-c4^2))/c4
    if (LIC2[i] < 0) LIC2[i]=0
  }
  amostra=matrix(rep(seq(1,m),4),m,4,byrow=F)
  xbar=cbind(rep(x2barras,m),LIC1,LSC1,media)
  rbar=cbind(rep(Sbarra,m),LIC2,LSC2,desviop)
  par(mfrow=c(2,1))
  matplot(amostra, rbar, type="o", ylab= "Desvio-Padrão",
          col=c("black","red","red","blue"),
          ylim=c(min(rbar),max(rbar)),
          xlab="Amostras", main="Gráfico S", pch=20, lty=1, lwd=2)
  matplot(amostra,xbar,type="o", ylab= "Média", xlab= "Amostras",
          main=expression(paste("Gráfico " * bar(x))), 
          col=c("black","red","red","blue"),
          ylim=c(min(xbar),max(xbar)),pch=20, lty=1, lwd=2)
}


### Exemplo 1

m=25; n=15; mn=m*n; mi=100; sigma=10
set.seed(4095); x=round(rnorm(mn,mi,sigma),2); x[286:300]=x[286:300]+2*sigma 

xS2(m,n,x)

### Exemplo 2

m=25; set.seed(35); n=sample(seq(14,16),m,replace=T); mn=sum(n)
mi=100; sigma=10; set.seed(3172); x=round(rnorm(mn,mi,sigma),2)
x[289:305]=x[289:305]+2*sigma 

xS2(m,n,x)


## Lista 2 ----
### Q1 ----
parafusos <- readxl::read_xls('parafusos.xls')

dplyr::glimpse(parafusos)

summary(parafusos)

# Sumarização _________________________________________________________________
summarytools::st_options(lang = "pt")
parafusos|>
  dplyr::select(-Amostra)|>
  summarytools::descr(
    style = "grid", justify = "c", transpose = T,
    stats = c("min", "Q1", "med", "mean", "Q3", "max", "sd", "cv"))
#______________________________________________________________________________

m=25; n=9; mn=m*n; mi=100; sigma=10
set.seed(9962); x=round(rnorm(mn,mi,sigma),2); x[77:80]=x[77:80]+3*sigma 



xR2(m, n, parafusos)



library("qcc")

parafusos|>
  qcc::qcc(type = "R", plot = F)|>
  plot(label.limits = c("LIC", "LSC"), 
       title = "Gráfico de Amplitudes dos parafusos",
       xlab = "Amostras", ylab = "Resumo das Estatísticas das Amostras")

parafusos|>
  qcc::qcc(type = "xbar", plot = F)|>
  plot(label.limits = c("LIC", "LSC"), 
       title = "Gráfico das Médias dos parafusos",
       xlab = "Amostras", ylab = "Resumo das Estatísticas das Amostras")

with()

### Q2 ----

m=20; n=5; mn=m*n; mi=500; sigma=1; set.seed(2453); x=round(rnorm(mn,mi,sigma),1)

# Com especificações
xR1(m, n, x, 500, 1)
out

result <- t(out)

# Sem especificações
xR2(m, n, x)
out

result <- cbind(result, t(out))

rownames(result) <- c("Linha Central", "LIC", "LSC")

# Tabela
tit = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(colspan = 1, ''),
      th(colspan = 2, 'Com Especificações'),
      th(colspan = 2, 'Sem Especificações')
      # th(colspan = 2, 'Petal')
    ),
    tr(
      lapply(rep(c('', 'R', "Xbarra", "R", "Xbarra"), 1), th)
    )
  )
))

result|>
  DT::datatable(
    caption = "Tabela 1: Informações dos gráficos R e Xbarra para Questão 2.",
    colnames = c("R", "Xbarra", "R", "Xbarra"),
    extensions = 'FixedHeader', container = tit,
    options = list(dom = 't', fixedHeader = T, autoWidth = F,
                   columnDefs = 
                     list(
                       list(className = 'dt-center', targets = c(1:4))))
  )|>
  DT::formatRound(
    columns = c(1:4), digits = 2, mark = ".", dec.mark = ",")

# Tentativa de utilização do pacote qcc
{
amostra <- c(1:20)

x1 <- c(500.5, 500.1, 498.9, 499.2, 500.1, 500.2, 500.3, 498.2, 501.1, 501.5, 499.2, 501, 500.2, 501.2, 500.5, 499.5, 500.3, 500.7, 500.2, 500.3)

x2 <- c(498.3, 500.4, 499.9, 498.4, 500.4, 502.7, 500.2, 500.3, 499.1, 500.3, 500.8, 501, 500.6, 499.7, 498.9, 499.9, 500, 501.5, 500.4, 500.7)

x3 <- c(500.9, 499.2, 500.6, 499.3, 498.5, 500.2, 500.3, 501.5, 498.3, 498.6, 498.4, 499.1, 500.2, 499.3, 499.8, 498.6, 500.1, 501.9, 501.3, 499.9)

x4 <- c(498.7, 501.7, 499.5, 498.9, 501.4, 500.5, 500, 497.8, 500.5, 500.1, 501.5, 500.9, 500.1, 499.5, 499.8, 500, 500, 498, 500, 497.7)

x5 <- c(498.3, 498.8, 499.2, 499.1, 498.5, 499.8, 499.1, 499.7, 499.9, 501.3, 500, 500.2, 499.5, 498, 499.1, 497.9, 499.9, 500.3, 499.3, 499.2)

sucos <- tibble::tibble(amostra, x1, x2, x3, x4, x5)
sucos2 <- cbind(amostra, x1, x2, x3, x4, x5)

# Verificação de NAs
sucos|>
  is.na()|>
  any()

mean(sucos[1,1])

sucos|>
  dplyr::summarise(mean())

dim(sucos2)[1]

for (i in 1:20) {
  x_barra[i] <- mean(sucos2[i, 2:5])
  sigma[i] = sd(sucos2[i, 2:5])
}

mean(x_barra)
mean(sigma)

sucos|>
  qcc::qcc(type = "R", center = 500, std.dev = 1, plot = F)|>
  plot()

sucos|>
  qcc::qcc(type = "xbar", center = 500, std.dev = 1, plot = F)|>plot()

}

### Q3 ----

m=25; n=14; mn=m*n; mi=150; sigma=10; set.seed(4125); x=round(rnorm(mn,mi,sigma),2); x[155:168]=x[155:168]-1.5*sigma; x=round(x,0)

xS2(m, n, x)

### Q4 ----
#### Item a ----
m=30; n=1; mn=m*n; mi=200; sigma=10; set.seed(1287); x=round(rnorm(mn,mi,sigma),2);
x[28:30]=x[28:30]+2.5*sigma; x=round(x,0)

test <- shapiro.test(x)

test$statistic
test$p.value

result <- cbind(test$statistic, test$p.value)
colnames(result) <- c("Estatística", "p-value")

par(mfrow=c(1,2))
qqnorm(x, main = "Histograma", xlab = "Quantis normais", ylab = "x")
qqline(x, col = "steelblue", lwd = 2)


hist(x, ylab = "Frequência")

# car::qqPlot(x,  envelope=list(style="lines"), xlab = "Quantis normais", main = "Normal Q-Q Plot")
par(mfrow=c(1,1))


x|>
  tibble::as_tibble()|>
  ggplot2::ggplot()+
  ggplot2::geom_histogram()

result|>
  DT::datatable(
    caption = "Tabela 2: Resultado do teste de normalidade de Shapiro-Wilk.",
    # colnames = c("R", "Xbarra", "R", "Xbarra"),
    extensions = 'FixedHeader', rownames = F,
    options = list(dom = 't', fixedHeader = F, autoWidth = F,
                   columnDefs = 
                     list(
                       list(className = 'dt-center', targets = c(0:1))))
  )|>
  DT::formatRound(columns = c(0:1), digits = 4, mark = ".", dec.mark = ",")


#### Item b ----

xR2(m, n, x)



### Q5 -----

m=25; set.seed(35); n=sample(seq(5,8),m,replace=T); mn=sum(n); mi=100; sigma=10; set.seed(3725); x=round(rnorm(mn,mi,sigma),1); x[89:100]=x[89:100]+2*sigma

xS2(m, n, x)


### Q6 -----










### Q7 -----



## A8_QUA_ ----
### CUSUM ----
# m = número de amostras observadas. 
# n = número de observações de cada amostra. 
# y = vetor com as observações, ordenadas por amostra, da variável observada.
# mi = especifição para a média do processo.
# sigma = especificação para o desvio-padrão do processo.

CUSUM=function(m,n,y,mi,sigma){
  k=0.5*sigma/sqrt(n)
  h=5*sigma/sqrt(n)
  hs=3.5*sigma/sqrt(n)
  cp=numeric(m)
  cn=numeric(m) 
  if (n>1) {
    dados=matrix(y,m,n,byrow=T)
    media=apply(dados,1,mean)
  } else media=y 
  cp[1]=max(c(0,media[1]-mi-k+h/2))
  cn[1]=max(c(0,mi+k-media[1]+h/2))
  for (i in 2:m) {
    cp[i]=max(c(0,media[i]-mi-k+cp[i-1]))
    cn[i]=max(c(0,mi-k-media[i]+cn[i-1]))
  }
  cn=-cn
  amostra=matrix(rep(seq(1,m),7),m,7,byrow=F)
  cusums=cbind(rep(0,m),rep(hs,m),rep(-hs,m),
               rep(h,m),rep(-h,m),cp,cn)
  par(mfrow=c(1,1))
  matplot(amostra,cusums,xlab="Amostras",ylab="Cusums",
          main="Gráfico de Somas Acumuladas",type="o",pch=20,
          col=c("black","red","red","orange","orange","blue",
                "blue"),ylim=c(min(cusums),max(cusums)) , lty=1, lwd=2)
}


#### Exemplo 4 ----

m=25; n=4; mn=m*n; mi=100; sigma=10
set.seed(9962); x=round(rnorm(mn,mi,sigma),2)
x[77:80]=x[77:80]+2*sigma

CUSUM(m,n,x,mi,sigma)

qcc::cusum(x, n, mi, sigma, 
           title = "Gráfico de Somas Acumuladas",
           xlab = "Amostras", ylab = "CUSUMS")

### MMEP ----
# m = número de amostras observadas. 
# n = número de observações de cada amostra, se as amostras forem balanceadas ou
# n = vetor com o nœmero de observações de cada amostra.
# mi = especificação para a média do processo.
# sigma = especificação para o desvio-padrão do processo.
# lambda = constante de suavização entre 0 e 1 (sugestão: use 0.05, 0.1 ou 0.2).
# gamma = constante de significância dos limites de controle (sugestão: use 3). 
# y = vetor com as observações, ordenadas por amostra, da variável observada.

MMEP=function(m,n,mi,sigma,lambda,gamma,y){
  if(length(n)==1){
    if (n > 1) {
      matdados=matrix(y,m,n,byrow=T)
      media=apply(matdados,1,mean,na.rm=T) 
    } else media=y 
  } else {
    matdados=matrix(NA,m,max(n))
    k=1
    for (i in 1:m) {
      for (j in 1:n[i]) {
        matdados[i,j]=y[k]
        k=k+1
      }
    }
    media=apply(matdados,1,mean,na.rm=T)
  }
  W=numeric(m); 
  W[1]=lambda*media[1]+(1-lambda)*mi
  for (i in 2:m) {
    W[i]=lambda*media[i]+(1-lambda)*W[i-1]
  }	
  LIC=numeric(m)
  LSC=numeric(m)	
  if(length(n)==1) {
    for (i in 1:m) {
      fator =(lambda*sigma^2)/((2-lambda)*n)
      ep=gamma*sqrt(fator*(1-(1-lambda)^(2*i)))
      LIC[i]=mi-ep
      LSC[i]=mi+ep
    }
  } else {
    for (i in 1:m) {
      fator =(lambda*sigma^2)/((2-lambda)*n[i])
      ep=gamma*sqrt(fator*(1-(1-lambda)^(2*i)))
      LIC[i]=mi-ep
      LSC[i]=mi+ep
    }
  }
  amostra=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
  LCMM=cbind(W,LIC,LSC,rep(mi,m))
  matplot(amostra,LCMM,type="o",ylab="Média",xlab="Amostras",
          main="Gráfico MMEP",col=c("blue","red","red","black"),
          ylim=c(min(LCMM),max(LCMM)),pch=20, lty=1, lwd=2)
}

#### Exemplo 4 ----

m=25; n=4; mn=m*n; mi=100; sigma=10
set.seed(9962); x=round(rnorm(mn,mi,sigma),2)
x[77:80]=x[77:80]+2*sigma

MMEP(m,n,mi,sigma,0.1,3,x)

### Códigos R - Gráfico np ----

# m = número de amostras disponíveis.
# n = número de unidades observadas em cada amostra.
# y = vetor contendo o total de itens defeituosos em cada amostra, ordenado de acordo com o tempo das amostras.

npse=function(m,n,y) {
  p=sum(y)/(n*m); LIC=n*p-3*sqrt(n*p*(1-p))
  if (LIC<0) LIC=0; LSC=n*p+3*sqrt(n*p*(1-p))	
  time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
  w=cbind(y,rep(n*p,m),rep(LIC,m),rep(LSC,m))
  par(mfrow=c(1,1))
  matplot(time,w,type="o",ylab="np",
          col=c("black","blue","red","red"),pch=20,lty=1,lwd=2,
          main="Gráfico np",xlab="Amostras",ylim=c(min(w),max(w)))
}


### Exemplo 1

m=25; n=100; p1=0.1; p2=0.2
set.seed(7256); x=rbinom(m,n,p1); set.seed(7865); x[21:25]=rbinom(5,n,p2)

npse(m,n,x)


# m = número de amostras dispon’veis.
# n = número de unidades observadas em cada amostra. Pode ser um vetor, caso as amostras tenham tamanhos diferentes.
# y = vetor contendo o total de itens defeituosos em cada amostra, ordenado de acordo com o tempo das amostras.

pse=function(m,n,y) {
  if (length(n)==1){
    p=sum(y)/(n*m);LIC=p-3*sqrt(p*(1-p)/n); if (LIC<0) LIC=0
    LSC=p+3*sqrt(p*(1-p)/n)	
    time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
    w=cbind(y/n,rep(p,m),rep(LIC,m),rep(LSC,m))
    par(mfrow=c(1,1))
    matplot(time,w,type="o",pch=20,main="Gráfico p",
            col=c("black","blue","red","red"),ylab="p",
            xlab="Amostras", ylim=c(min(w),max(w)),lty=1,lwd=2)
  } else {
    p=sum(y)/sum(n);LIC=numeric(m);LSC=numeric(m);z=numeric(m)
    for (i in 1:m){
      LIC[i]=p-3*sqrt(p*(1-p)/n[i]); if (LIC[i]<0) LIC[i]=0
      LSC[i]=p+3*sqrt(p*(1-p)/n[i])	
      z[i]=((y[i]/n[i]-p)/sqrt(p*(1-p)/n[i]))
    }
    time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
    w1=cbind(y/n,rep(p,m),LIC,LSC)
    w2=cbind(z,rep(0,m),rep(-3,m),rep(3,m))
    par(mfrow=c(1,2))
    matplot(time,w1,type="o",pch=20,ylim=c(min(w1),max(w1)),
            col=c("black","blue","red","red"),lty=1,lwd=2,
            main="Gráfico p",ylab="p",xlab="Amostras"	)
    matplot(time,w2,type="o",pch=20,ylim=c(min(w2),max(w2)),
            col=c("black","blue","red","red"),xlab="Amostras",
            main="Gráfico p Padronizado",ylab="z",lty=1,lwd=2)
  }
}

### Exemplo 1

m=25; n=100; p1=0.1; p2=0.2
set.seed(7256); x=rbinom(m,n,p1); set.seed(7865); x[21:25]=rbinom(5,n,p2)

pse(m,n,x)


### Exemplo 2

m=25; p1=0.1; p2=0.20
tamanho=seq(90,140); set.seed(1459); n=sample(tamanho,m,replace=T)
sementes=seq(0,9999); set.seed(8667); sementes=sample(sementes,m,replace=F)
prob=c(rep(p1,20),rep(p2,5)); x=numeric(25)
for (i in 1:m) {
  set.seed(sementes[i]); x[i]=rbinom(1,n[i],prob[i])
}	

pse(m,n,x)


### Código R - Gráfico c ----

# m = número de amostras disponíveis.
# y = vetor contendo o número total de defeitos em cada amostra, ordenado de acordo com o tempo de coleta das amostras.
# c = especificação para a taxa aceitável de defeitos para a amostra observada.

cce=function(m,y,c) {
  LIC=c-3*sqrt(c); if (LIC<0) LIC=0; LSC=c+3*sqrt(c)
  time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
  w=cbind(y,rep(c,m),rep(LIC,m),rep(LSC,m))
  par(mfrow=c(1,1))
  matplot(time,w,type="o",pch=20,ylim=c(min(w),max(w)),
          col=c("black","blue","red","red"), xlab="Amostras",
          main="Gráfico c",ylab="c", lty=1,lwd=2)
} 



# m = nœmero de amostras dispon’veis.
# y = vetor contendo o nœmero total de defeitos em cada amostra, ordenado de acordo com o tempo de coleta das amostras.

cse=function(m,y) {
  c=mean(y); LIC=c-3*sqrt(c); if (LIC<0) LIC=0; LSC=c+3*sqrt(c)
  time=cbind(seq(1,m),seq(1,m),seq(1,m),seq(1,m))
  w=cbind(y,rep(c,m),rep(LIC,m),rep(LSC,m))
  par(mfrow=c(1,1))
  matplot(time,w,type="o",pch=20, ylim=c(min(w),max(w)),
          col=c("black","blue","red","red"), xlab="Amostras",
          main="Gráfico c",ylab="c",lty=1,lwd=2)
}


### Exemplo 1:
set.seed(6847); x=rpois(25,10); set.seed(1975); x[21:25]=rpois(5,20)
cse(25,x)



## Lista 5 ----

### Q1a ----
x=c(9, 11, 14, 9, 16, 11, 10, 12, 5, 10, 9, 11, 7, 9, 22, 11, 8, 11, 9, 13, 9, 7, 9, 8, 12)
p = 0.05

# prob = c(rep(0.05,20))
# 
# pse(200,25,x)

qcc::qcc(x, type = "p", sizes = 200, plot = F, center = p)|>plot(label.limits = c("LIC", "LSC"))

qcc::qcc(x[-15], type = "p", sizes = 200, plot = F, center = p)|>plot(label.limits = c("LIC", "LSC"))




dia = c(1:25)

cbind(dia, x)|>
  DT::datatable(
    # caption = "Tabela 1: Informações dos gráficos R e Xbarra para Questão 2.",
    colnames = c("Dia", "N° pares defeituosos"),
    extensions = 'FixedHeader', 
    # container = tit,
    options = list(dom = 't', fixedHeader = F, autoWidth = T
                   # columnDefs =
                   #   list(
                   #     list(className = 'dt-center', targets = c(1:2)))
                   )
  )
  # DT::formatRound(
  #   columns = c(1:2),
  #   digits = 2, mark = ".",
  #   dec.mark = ",")

### Q1b ----



(LIC = p-3*sqrt((p*(1-p)/172)))

(p*(1-p))/((p-LIC)/3)^2


### Q2 ----

#### Item a ----

m = 20

n = c(200, rep(250, 3), 200, 200, rep(150, 4), rep(100, 3), rep(200, 5), rep(250, 2))

segunda_vis = c(6, 8, 9, 7, 3, 4, 2, 1, 0, 2, 1, 0, 1, 4, 5, 3, 10, 4, 7, 6)

(p_hat_i = segunda_vis/n)

(p_barra = sum(segunda_vis)/sum(n))

(lsc = p_barra + 3*sqrt((p_barra*(1-p_barra))/n))
(lic = p_barra - 3*sqrt((p_barra*(1-p_barra))/n))

# Média ponderada
# weighted.mean(n)
# 
# (8*200+5*250+4*150+3*100)/20
# 
# mean(n)

#### Item b ----

qcc::qcc(segunda_vis, type = "p", sizes = n, plot = T)
  plot(label.limits = c("LIC", "LSC"))


pse(m, n, segunda_vis)
mtext("Figura 5 Gráfico p com limites variáveis e gráfico padronizado", side = 3, line = 3, at = -12, cex = 1.1)

#### Item c ----

qcc::qcc(segunda_vis, type = "p", sizes = mean(n), plot = T)
  plot(label.limits = c("LIC", "LSC"), xlab = "Amostras",
       ylab = "Resumo das Estatísticas das Amostras",
       title = "Figura 3: Gráfico das proporções de manutenções \nque requerem segunda visita para conclusão")

pse(m, mean(n), segunda_vis)

#### Item d ----

mean(z_i)

(z_i = (p_hat_i - p_barra)/sqrt((p_barra*(1 - p_barra))/n))

plot(z_i, type = 'l', ylim=c(-3, 3), lty=1, lwd=2, col = "blue")
points(z_i, pch = 19, col = "blue")
abline(h = 0, col = "black", lwd = 2)
abline(h = 3, col = "red", lwd = 2)
abline(h = -3, col = "red", lwd = 2)
box()
grid()

qcc::qcc(z_i, data.name = "Padrão", type = "p", sizes = mean(n), limits = c(-3, 3), center = 0, plot = T)

qcc::qcc(segunda_vis, data.name = "Padrão", type = "p", sizes = mean(n), limits = c(-3, 3), center = 0, plot = T)
  # plot(label.limits = c("LIC", "LSC"), xlab = "Amostras",
  #      ylab = "Resumo das Estatísticas das Amostras",
  #      title = "Figura 3: Gráfico das proporções de manutenções \nque requerem segunda visita para conclusão")

segunda_vis*z_i

par(mfrow = c(1,1))

w2[,1]

### Q3 ----

hotel <- readxl::read_xls('hotel.xls')

dplyr::glimpse(hotel)

summary(hotel)

head(hotel)|>
  DT::datatable(
    caption = "Tabela 1: Hotel",
    # colnames = c("R", "Xbarra", "R", "Xbarra"),
    extensions = 'FixedHeader', 
    rownames = F,
    # container = tit,
    options = list(
      dom = 't',
      # fixedHeader = F, 
      autoWidth = F,
      columnDefs =
        list(
          list(className = 'dt-center', targets = c(0:1)))))

#### Item a ----

qcc::qcc(hotel$Erros, type = "np", sizes = 30)

(m = dim(hotel)[1])

n = hotel$Erros

np_barra = n*mean(n)

hotel$Erros/hotel$Dia

sum(hotel$Erros)/30

(p_barra = np_barra/n)

(lsc = np_barra + 3*sqrt(np_barra*(1-p_barra)))

# m = número de amostras disponíveis.
# n = número de unidades observadas em cada amostra.
# y = vetor contendo o total de itens defeituosos em cada amostra, ordenado de acordo com o tempo das amostras.

npse(m, n, hotel)


### Q4 ----

dia = c(1:25)

reg1 = c(8, 11, 1, 3, 3, 6, 8, 4, 1, 15, 1, 6, 7, 2, 6, 2, 11, 5, 6, 2, 7, 4, 4, 15, 2)

reg2 = c(7, 1, 1, 2, 2, 3, 8, 10, 6, 1, 7, 7, 6, 9, 14, 9, 1, 5, 15, 7, 5, 3, 1, 2, 15)

reg3 = c(1, 11, 8, 5, 13, 3, 2, 2, 1, 3, 13, 9, 3, 3, 7, 4, 1, 19, 5, 9, 6, 8, 4, 7, 3)

reg4 = c(11, 2, 2, 1, 6, 3, 1, 6, 3, 2, 5, 3, 3, 8, 1, 2, 3, 1, 6, 2, 14, 1, 20, 10, 11)

reg5 = c(17, 9, 5, 4, 5, 1, 5, 4, 2, 8, 1, 1, 1, 7, 8, 1, 2, 3, 6, 8, 10, 2, 5, 17, 2)


registros = cbind(dia, reg1, reg2, reg3, reg4, reg5)|> tibble::as_tibble()


#### Item a ----

registros = registros|>
  # dplyr::select(-dia) |>
  dplyr::mutate(
    total = rowSums(registros[,-1])
    # total = rowSums(reg1, reg2, reg3, reg4, reg5)
    # total = rowSums(., na.rm = F)
  )

rowSums(registros)


qcc::qcc(registros$total[-c(1, 21, 24)], type = "c", plot = F)|>
  plot(label.limits = c("LIC", "LSC"), xlab = "Amostras",
       ylab = "Resumo das Estatísticas das Amostras",
       title = "Figura 8: Gráfico ")


registros$total[c(-1, -c(17:20), -24)]
registros$total

cse(25, registros$total)


## Lista 6 ----





# FIM ----
