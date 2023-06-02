

# Resolução da Lista 3 - CEP 

library("qcc")


## Lista 3 ----

  ### Q1 ----

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
  
  m=25; n=4; mn=m*n; mi=2; sigma=0.25; set.seed(2553); x=round(rnorm(mn,mi,sigma),1);
  x[81:100]=x[81:100]+1*sigma; x=round(x,1)
  
  
  CUSUM(m,n,x,mi,sigma)
  
  
  
  tab1 <- read.csv2("tab1-L3.csv")
  
  tab1[,-1] == x
  
  qcc::cusum(tab1[,-1], sizes = 25, center = 2, std.dev = 0.25)
  
  qcc::cusum(tab1[,-1], center = 2, std.dev = 0.25)
  
  qcc::cusum(tab1[,-1], center = 2, std.dev = 0.25, head.start = 1)
  # qcc::cusum(x, center = 2, std.dev = 0.25)
  
  qcc::cusum(tab1[,-1], center = 2, std.dev = 0.25, decision.interval = 1)
  
  
  qcc::qcc(parafusos[,-1], type = "R", plot = F)|>
    plot(label.limits = c("LIC", "LSC"), 
         title = "Gráfico de Amplitudes dos parafusos",
         xlab = "Amostras", ylab = "Resumo das Estatísticas das Amostras")
  
  qcc::qcc(parafusos[,-1], type = "xbar", plot = F)|>
    plot(label.limits = c("LIC", "LSC"), 
         title = "Gráfico dos diâmetros médios dos parafusos",
         xlab = "Amostras", ylab = "Resumo das Estatísticas das Amostras")
  
  # Remoção dos pontos discrepantes
  qcc::qcc(parafusos[c(-6, -19, -20), -1], type = "xbar", plot = F)|>
    plot(label.limits = c("LIC", "LSC"), 
         title = "Gráfico dos diâmetros médios dos parafusos",
         xlab = "Amostras", ylab = "Resumo das Estatísticas das Amostras")
  
  with()
  
### Q2 ----
  
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

MMEP(m, n, mi, sigma, 0.1, 2.7, x)



  
### Q3 ----
  
m=30; n=1; mn=m*n; mi=100; sigma=10; set.seed(2456); x=round(rnorm(mn,mi,sigma),1);
x[15:17]=x[15:17]+1*sigma
  

  
  # _________________________________
  amostra = c(1:25)
  
  x1 = c(158, 144, 158, 141, 135, 154, 147, 132, 140, 162, 156, 133, 144, 140, 149, 157, 147, 150, 153, 167, 153, 143, 157, 158, 152)
  x2 = c(160, 136, 154, 147, 149, 165, 157, 145, 149, 161, 146, 126, 138, 163, 139, 149, 147, 161, 125, 146, 140, 153, 150, 157, 139)
  x3 = c(155, 177, 154, 160, 142, 162, 148, 159, 141, 145, 155, 134, 144, 164, 139, 141, 161, 140, 157, 154, 160, 141, 147, 154, 161)
  x4 = c(164, 145, 170, 159, 156, 164, 147, 173, 147, 156, 154, 132, 137, 146, 136, 147, 152, 148, 156, 157, 160, 134, 156, 153, 163)
  x5 = c(143, 155, 150, 135, 164, 143, 170, 155, 147, 142, 163, 141, 135, 148, 132, 156, 135, 173, 159, 165, 130, 148, 141, 157, 161)
  x6 = c(134, 133, 147, 146, 159,	129,	148,	144,	154,	159,	151,	137,	147,	151,	164,	155,	160,	149,	143,	134,	159, 154, 124, 153, 134)
  
  
  
  garrafas <- read.csv2("garrafas.csv")
  
  # Gráficos sem especificação
  qcc::qcc(garrafas[,-1], type = "S", plot = F)|>
    plot(label.limits = c("LIC", "LSC"), xlab = "Amostras",
         ylab = "Resumo das Estatísticas das Amostras",
         title = "Figura 5: Gráfico R para medidas sem especificações")
  
  
  qcc::qcc(garrafas[-12 ,-1], type = "xbar", plot = F)|>
    plot(label.limits = c("LIC", "LSC"), xlab = "Amostras",
         ylab = "Resumo das Estatísticas das Amostras",
         title = "Figura 6: Gráfico R para medidas sem especificações")
  
  
### Q4 ----
  
  tempo <- read.csv2("fast-food.csv")
  
  
  #### Item a ----
  m=30; n=1; mn=m*n; mi=200; sigma=10; set.seed(1287); x=round(rnorm(mn,mi,sigma),2);
  x[28:30]=x[28:30]+2.5*sigma; x=round(x,0)
  
  test <- shapiro.test(x)
  test <- shapiro.test(tempo$tempo_entrega)
  
  test$statistic
  test$p.value
  
  result <- cbind(test$statistic, test$p.value)
  colnames(result) <- c("Estatística", "p-value")
  
  par(mfrow=c(1,2))
  qqnorm(x, main = "Histograma", xlab = "Quantis normais", ylab = "x")
  qqline(x, col = "steelblue", lwd = 2)
  
  
  hist(tempo$tempo_entrega, ylab = "Frequência")
  
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
  
  summary(tempo)
  
  # qcc
  qcc::qcc(tempo[,-1], type = "R", sizes = 30, center = 200, std.dev = 10, plot = T)
  qcc::qcc(tempo[,-1], type = "R", sizes = 30, limits = c(180:230), plot = T)
  plot(label.limits = c("LIC", "LSC"), 
       title = "Gráfico de Amplitudes dos parafusos",
       xlab = "Amostras", ylab = "Resumo das Estatísticas das Amostras")
  
  qcc::qcc(tempo[,-1], type = "xbar", sizes = 30, plot = T)
  
  qcc::qcc(tempo[,-1], type = "xbar", plot = F)|>
    plot(label.limits = c("LIC", "LSC"), 
         title = "Gráfico dos diâmetros médios dos parafusos",
         xlab = "Amostras", ylab = "Resumo das Estatísticas das Amostras")
  
  
