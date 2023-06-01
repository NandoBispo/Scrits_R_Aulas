

# Resolução da Lista 2 - CEP 

## Lista 2 ----
{
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
  
  
  library("qcc")
  
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
  
  #### Utilizando a fç criada ----
  
  m=20; n=5; mn=m*n; mi=500; sigma=1; set.seed(2453); x=round(rnorm(mn,mi,sigma),1)
  
  # Com especificações
  xR1(20, 5, x, 500, 1)
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
  
  #### Tentativa de utilização do pacote qcc ----
  {
    amostra <- c(1:20)
    
    x1 <- c(500.5, 500.1, 498.9, 499.2, 500.1, 500.2, 500.3, 498.2, 501.1, 501.5, 499.2, 501, 500.2, 501.2, 500.5, 499.5, 500.3, 500.7, 500.2, 500.3)
    
    x2 <- c(498.3, 500.4, 499.9, 498.4, 500.4, 502.7, 500.2, 500.3, 499.1, 500.3, 500.8, 501, 500.6, 499.7, 498.9, 499.9, 500, 501.5, 500.4, 500.7)
    
    x3 <- c(500.9, 499.2, 500.6, 499.3, 498.5, 500.2, 500.3, 501.5, 498.3, 498.6, 498.4, 499.1, 500.2, 499.3, 499.8, 498.6, 500.1, 501.9, 501.3, 499.9)
    
    x4 <- c(498.7, 501.7, 499.5, 498.9, 501.4, 500.5, 500, 497.8, 500.5, 500.1, 501.5, 500.9, 500.1, 499.5, 499.8, 500, 500, 498, 500, 497.7)
    
    x5 <- c(498.3, 498.8, 499.2, 499.1, 498.5, 499.8, 499.1, 499.7, 499.9, 501.3, 500, 500.2, 499.5, 498, 499.1, 497.9, 499.9, 500.3, 499.3, 499.2)
    
    sucos <- tibble::tibble(amostra, x1, x2, x3, x4, x5)
    
    # Verificação de NAs
    sucos|>
      is.na()|>
      any()
    
    # Gráficos sem especificação
    qcc::qcc(sucos[,-1], type = "R", plot = F)|>
      plot(label.limits = c("LIC", "LSC"), xlab = "Amostras",
           ylab = "Resumo das Estatísticas das Amostras",
           title = "Figura 5: Gráfico R para medidas sem especificações")
    
    
    qcc::qcc(sucos[,-1], type = "xbar", plot = F)|>
      plot(label.limits = c("LIC", "LSC"), xlab = "Amostras",
           ylab = "Resumo das Estatísticas das Amostras",
           title = "Figura 6: Gráfico R para medidas sem especificações")
    
    # Gráficos com especificação e sigma = 0,5
    qcc::qcc(sucos[,-1], type = "R", center = 2.5, std.dev = 0.5, plot = T)
    qcc::qcc(sucos[,-1], type = "xbar", center = 500, std.dev = 0.5, plot = T)
    
    
    # Gráficos com especificação e sigma = 1
    qcc::qcc(sucos[,-1], type = "R", center = 2.5, std.dev = 1, plot = T)
    qcc::qcc(sucos[,-1], type = "xbar", center = 500, std.dev = 1, plot = T)
    
    
  }
  
  par(mfrow = c(1,1))
  
  ### Q3 ----
  
  m=25; n=14; mn=m*n; mi=150; sigma=10; set.seed(4125); x=round(rnorm(mn,mi,sigma),2); x[155:168]=x[155:168]-1.5*sigma; x=round(x,0)
  
  xS2(m, n, x)
  
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
  
  
  
  ### Q5 -----
  
  m=25; set.seed(35); n=sample(seq(5,8),m,replace=T); mn=sum(n); mi=100; sigma=10; set.seed(3725); x=round(rnorm(mn,mi,sigma),1); x[89:100]=x[89:100]+2*sigma
  
  xS2(m, n, x)
  
amostra <- read.csv2("tab5-L2.csv")


# qcc
qcc::qcc(amostra[,-1], type = "R", plot = T)

qcc::qcc(amostra[,-1], type = "S", plot = T)
qcc::qcc(amostra[,-1], type = "xbar", plot = T)

qcc::qcc(amostra[-c(14, 15),-1], type = "xbar", plot = T)


qcc::qcc(tempo[,-1], type = "R", sizes = 30, limits = c(180:230), plot = T)
plot(label.limits = c("LIC", "LSC"), 
     title = "Gráfico de Amplitudes dos parafusos",
     xlab = "Amostras", ylab = "Resumo das Estatísticas das Amostras")

qcc::qcc(tempo[,-1], type = "xbar", sizes = 30, plot = T)

qcc::qcc(tempo[,-1], type = "xbar", plot = F)|>
  plot(label.limits = c("LIC", "LSC"), 
       title = "Gráfico dos diâmetros médios dos parafusos",
       xlab = "Amostras", ylab = "Resumo das Estatísticas das Amostras")

  
### Q6 -----
  
  
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
  
x=list("Pessoal"=c("Treinamento", "Inadequado"),
       "Clima"=c("Chuva", "Temperatura", "Vento"),
       "Fornecedores"=c("Materiais", "Atrasos", "Retrabalho"),
       "Planejamento"=c("Cliente", "Permissões", "Erros"))

y="Delay"

Ishikawa(x,y)

# Utilizando pacote qcc
  
qcc::cause.and.effect(cause = 
                        list("Pessoal"=c("Treinamento", "Inadequado"),
                             "Clima"=c("Chuva", "Temperatura", "Vento"),
                             "Fornecedores"=c("Materiais", "Atrasos", "Retrabalho"),
                             "Planejamento"=c("Cliente", "Permissões", "Erros")),
                      effect = "Delay", title = "Diagrama de Causa e Efeito")
  
  
### Q7 -----


Contagem = c(5, 1, 3, 1, 2, 18, 20, 4, 15, 2, 4)
Custo = c(50, 150, 50, 10, 20, 180, 200, 10, 5, 20, 150)

prazos = cbind(Contagem, Custo)|>tibble::as_tibble()
prazos = rbind(Contagem, Custo)


prazos |>
  DT::datatable(
    # caption = "Tabela 1: Informações dos gráficos R e Xbarra para Questão 2.",
    colnames = "",
    # extensions = 'FixedHeader', container = tit,
    options = list(dom = 't', fixedHeader = T, autoWidth = F,
                   columnDefs = 
                     list(
                       list(className = 'dt-center', targets = c(1:10))))
  )


names(Contagem) = c("Treinamento Inadequado", "Chuva", "Temperatura", "Vento", "Materiais", "Atrasos", "Retrabalho", "Planejamento", "Cliente", "Permissões", "Erros")

qcc::pareto.chart(Contagem, plot = F)|>plot()


}