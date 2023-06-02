

# Resolução da Lista 5 - CEP 

## Lista 5 ----
{
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
  
}