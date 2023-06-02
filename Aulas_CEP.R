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



## Lista 6 ----





# FIM ----
