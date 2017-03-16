
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)
library(plotly)
library(rvest)
library(quantmod)
library(stargazer)
library(markdown)
library(tseries)
library(highcharter)
library(shinydashboard)
library(moments)
library(gridExtra)
library(ggplot2)
library(ggfortify)
library(forecast)
library(rugarch)
Nombre = c('Abertis Infraestructuras S.A.', 'Actividades de Construcción y Servicios S.A.', 
           'Acerinox, S.A.','Aena, S.A.','Amadeus IT Holding SA','Acciona, S.A.',
           'Banco Bilbao Vizcaya Argentaria, S.A.','Bankia, S.A.','Bankinter, S.A.',
           'CaixaBank, S.A.','Distribuidora Internacional de Alimentación, S.A.',
           'Endesa, Sociedad Anonima','Enagás, S.A.','Fomento de Construcciones y Contratas, S.A.',
           'Ferrovial, S.A.','Gamesa Corporación Tecnológica S.A.', 'Gas Natural SDG SA',
           'Grifols, S.A.','International Consolidated Airlines Group, S.A.','Iberdrola, S.A.',
           'Indra Sistemas, S.A.','Industria de Diseno Textil Inditex SA','Mapfre SA',
           'ArcelorMittal SA','Banco Popular Espanol S.A.',
           'Red Eléctrica Corporación S A.', 'Repsol, S.A.','Banco de Sabadell, S.A.',
           'Banco Santander, S.A.','Telefónica, S.A. ','Mediaset España Comunicación, SA',
           'Tecnicas Reunidas, S.A.','Viscofan SA')

yahoo = c('ABE.MC','ACS.MC','ACX.MC',
          'AENA.MC','AMS.MC','ANA.MC','BBVA.MC','BKIA.MC','BKT.MC',
          'CABK.MC','DIA.MC','ELE.MC',
          'ENG.MC','FCC.MC','FER.MC','GAM.MC','GAS.MC','GRF.MC','IAG.MC',
          'IBE.MC','IDR.MC','ITX.MC','MAP.MC',
          'MTS.MC','POP.MC','REE.MC',
          'REP.MC','SAB.MC','SAN.MC','TEF.MC',
          'TL5.MC','TRE.MC','VIS.MC')

wiki = c('Abertis', 'Grupo_ACS', 'Acerinox', 'Aena', 'Amadeus_IT_Group',
         'Acciona', 'Banco_Bilbao_Vizcaya_Argentaria', 'Bankia', 'Bankinter',
         'CaixaBank', 'Supermercados_Dia', 'Endesa', 'Enegas', 'Fomento_de_Construcciones_y_Contratas',
         'Ferrovial', 'Gamesa', 'Gas_natural', 'Grifols', 'International_Airlines_Group',
         'Iberdrola', 'Indra_Sistemas', 'Inditex', 'MAPFRE', 'ArcelorMittal', 
         'Banco_Popular_Español', 'Red_Eléctrica_de_España', 'Repsol',
         'Banco_Sabadell', 'Banco_Santander', 'Telefónica', 'Mediaset_España_Comunicación', 
         'Técnicas_Reunidas', 'Viscofan')

datos = data.frame(id = c(1:33), Nombre = Nombre, wiki=wiki, yahoo=yahoo)

#Función para contar palabras
CW = function(str2){
  length(strsplit(str2,' ')[[1]])} 
#Función para seleccionar el primer párrafo que contenga mas de 10 palabras  
TEXT = function(text){
  n=1
  while(n<6){
    if(CW(text[[n]]) > 10){
      return(text[[n]])
    }
    else{n = n+1}
  }}


function(input, output, session){
  
  output$noti <- renderText({
    paste('Ha seleccionado: ',datos$Nombre[as.numeric(input$accion)])
  })
  
  output$exp <- renderPlot({
    cot = get.hist.quote("^gspc", start= Sys.Date() - 1000,
                                  end= Sys.Date(), quote="AdjClose",
                                  provider="yahoo",
                                  compression="d", retclass="zoo")
    g1 <- ggplot(NULL) + geom_line(aes(x=time(cot)[-1],y=diff(log(cot$AdjClose))), col='darkorange') + 
      ggtitle('Log returns')+theme_minimal() + ylab('') + xlab('time')
    g1
  })
  

  cotizaciones <- function(){
    get.hist.quote(instrument=datos$yahoo[as.numeric(input$accion)], 
                   start= as.character(input$daterange)[1],
                   end= as.character(input$daterange)[2], quote="AdjClose",
                   provider="yahoo",
                   compression="d", retclass="zoo", quiet=TRUE)
  }
  
  observe({
    input$accion
    updateDateRangeInput(session, 'daterange', start= Sys.Date()-1600,
                         end=Sys.Date())
  })
  
  
  name <- reactive(
    {datos$wiki[as.numeric(input$accion)]}
  )

  serie = reactive(
    {data.frame(time = time(cotizaciones()), values = cotizaciones()$AdjClose)}
    )
  
  
  return = reactive(
    {diff(log(serie()$values))}
    )
  
  dataRet = reactive(
    {data.frame(time = serie()$time[-1], return = return())}
  )
  
  p = reactive({
    input$p
  })
  q = reactive({
    input$q})
  
  fit <- reactive({
    
    if(input$brexit){
      brex = which(dataRet()$time=='2016-06-23')
      n = input$dias[1]
      n1 = input$dias[2]
      leng = nrow(dataRet())
      xreg = c()
      for (i in n:n1){
        reg = rep(0,leng)
        reg[i+brex] = 1
        xreg = cbind(xreg,reg)
      }
      colnames(xreg) = paste(paste('Dia',n:n1, sep=''))
      xreg = as.matrix(xreg)
      
      if(input$xreg){
                brex = which(dataRet()$time=='2016-06-23')
                n1 = input$nxreg
                leng = nrow(dataRet())
                Vxreg = rep(0,leng)
                Vxreg[(brex):(brex+n1)] = 1
                Vxreg = as.matrix(Vxreg)
                spec1=ugarchspec(variance.model = list(model=input$modelo, submodel = input$submodel,
                                                       garchOrder=c(p(),q()),
                                                       external.regressors = Vxreg),
                                 mean.model = list(armaOrder=c(0,0), include.mean=as.logical(input$mean),
                                                   external.regressors = xreg),
                                 distribution.model = input$Gdist)
                ugarchfit(data=dataRet()$return,spec=spec1)        
              }else{
        
              spec1=ugarchspec(variance.model = list(model=input$modelo, submodel = input$submodel,
                                                     garchOrder=c(p(),q())),
                               mean.model = list(armaOrder=c(0,0), include.mean=as.logical(input$mean),
                                                 external.regressors = xreg),
                               distribution.model = input$Gdist)
              ugarchfit(data=dataRet()$return,spec=spec1)}

    }
    else{
      spec1=ugarchspec(variance.model = list(model=input$modelo, submodel = input$submodel, garchOrder=c(p(),q())),
                     mean.model = list(armaOrder=c(0,0), include.mean=as.logical(input$mean)),
                     distribution.model = input$Gdist)
    ugarchfit(data=dataRet()$return,spec=spec1)
    }

  })
  
  observe({
    input$accion
    updateSliderInput(session, 'p', value=1 )
    updateSliderInput(session, 'q', value=0 )
  })
  
  
  
  
  output$distPlot <- renderHighchart({
      highchart(type='stock') %>%
      hc_title(text = 'Evolución de la serie') %>%
      hc_add_series_times_values(dates = serie()$time,
                                 values = serie()$values)%>%
      hc_xAxis(text = 'empresa') %>%
      hc_add_theme(hc_theme_smpl())
    })


  output$des <- renderText({
    url <- 'https://es.wikipedia.org/wiki/'
    name <- name()
    urlname = paste0(url, name)
    imdb <- read_html(urlname)
    text <- imdb %>%html_nodes('p')%>%html_text()
    TEXT(text)
    
  })
  output$table <- renderTable({
    url <- 'https://es.wikipedia.org/wiki/'
    name <- name()
    urlname = paste0(url, name)
    imdb <- read_html(urlname)
    table <- imdb %>%html_nodes('.infobox')%>%html_table
    table = as.data.frame(table)
    table = table[,-3]
    colnames(table)[1] = 'Estadísticas'
    table

  })
  
  output$logs <- renderPlotly({
    
    
    data = serie()
    g <- ggplot(data) + xlab('Time') + ylab('Serie')+
      theme_minimal()
    
    if(input$enlog){
     g = g + geom_line(aes(x=time, y = log(values)), col='darkorange') + 
        ggtitle('Serie en logarítmos')}
    else{
      g = g + geom_line(aes(x=time, y = values), col='darkorange') + 
        ggtitle('Serie aritmética')
    }
    if(input$brexit1){
      x = as.numeric(data$time[data$time=="2016-06-23"])
      ggplotly(g + geom_vline(aes(xintercept = x), linetype=3, 
                              col='green')    )
    }
    else{ggplotly(g)}
    
  })
  output$return <- renderPlotly({
    
    
    dataRet = dataRet()
    g1 <- ggplot(dataRet) + xlab('Time') + ylab('Serie')+
      theme_minimal()
    if(input$var){
      g1 = g1 + geom_line(aes(x = time, y = (return^2)), col='darkorange') +
                 ggtitle('Stock daily returns square')}
    else{
      g1 = g1 + geom_line(aes(x = time, y = return), col='darkorange') + 
        ggtitle('Stock daily returns')}
    if(input$brexit1){
      x = as.numeric(dataRet$time[dataRet$time=="2016-06-23"])
      ggplotly(g1 + geom_vline(aes(xintercept = x), linetype=3, 
                               col='green'))
    }
    else{
      ggplotly(g1)
    }
    
    
  })
  
  output$stats <- renderTable({
    return = return()
    stats = data.frame(Statistics = c('N','Mean','Stdev','Min','Max','Skewness','Kurtosis'),
                       Valores = round(c(length(return), mean(return), sd(return),
                                         min(return), max(return),
                                         skewness(return), kurtosis(return)),3))
    stats
  }, digits=3)
  
  output$distribution <- renderPlot({
    dataRet = dataRet()
    gg <- ggplot(dataRet) + geom_density(aes(x=return), fill='darkorange', color = 'black', alpha=.6) + 
      theme_minimal() + labs(title = 'Densidad de daily returns', 
                             subtitle = paste('Estadístico del Jarque Bera Test:', 
                                              round(jarque.test(dataRet$return)$statistic,3), '. El p-value es:',
                                              round(jarque.test(dataRet$return)$p.value,3)),
                             x = 'returns')
    if(input$norm){
      gg + stat_function(fun = dnorm, colour = "blue", size=1.2, 
                         args = list(mean = mean(dataRet$return), 
                                     sd = sd(dataRet$return)))
    }else{gg}
    
  })
    output$ACF <- renderPlot({
      return = return()
      if(input$var1){
        acf <- ggAcf(return()^2) + ggtitle('ACF return^2') + theme_minimal()
        pacf <- ggPacf(return()^2) + ggtitle('PACF return^2') + theme_minimal()
        }
      else{
        acf <- ggAcf(return()) + ggtitle('ACF') + theme_minimal()
        pacf <- ggPacf(return()) + ggtitle('PACF') + theme_minimal()
        }
      
      
      
      grid.arrange(acf, pacf, nrow=2)
    })
      
    output$params <- renderTable({
      as.data.frame(fit()@fit$matcoef)
    }, rownames = T, width = '450px', digits=3, bordered = T, hover=T)
    
    output$params1 <- renderTable({
      data.frame(Persistence = persistence(fit()), Akaike = infocriteria(fit())[1], Likelihood = likelihood(fit()) )
      
    }, rownames = F, width = '450px', digits=3, bordered = T, hover=T)
    
    output$resid <- renderPlot({
      g <- ggplot(NULL) + geom_line(aes(x=dataRet()$time, y=fit()@fit$z), col='lightblue') +
        theme_minimal() + geom_hline(yintercept= 0 , col='red',size=1.1, alpha=.5)+
        labs(title = 'Residuos',
             subtitle= paste('El estadístico del Jarque Bera Test es:',
                             round(jarque.test(fit()@fit$z)$statistic,3),
                             ', y el p.value es de',
                             round(jarque.test(fit()@fit$z)$p.value,3)),
             x = '', y='')
      
      qqplot <- ggplot(NULL, aes(sample =fit()@fit$z )) + 
        labs(title='QQplot', x='', y='') + theme_minimal()
        
      
      
      dens <- ggplot(as.data.frame(fit()@fit$z)) + 
        geom_density(aes(x=fit()@fit$z), fill='lightblue',color = 'black', alpha=.6)+
        theme_minimal() + xlab('') + ylab('')
      
      if(input$Gdist=='norm'){
      dens <- dens + stat_function(fun = dnorm, colour = "blue", size=1.2, 
                      args = list(mean = mean(fit()@fit$z), 
                                  sd = sd(fit()@fit$z)))
      qqplot <- qqplot + stat_qq(distribution = stats::qnorm)
      }
      else if(input$Gdist=='std'){
        dens <- dens + stat_function(fun = dt, colour = "blue", size=1.2, 
                             args = list(df = 3))
        qqplot <- qqplot + stat_qq(distribution = stats::qt)
      }
        
        
      grid.arrange(g, arrangeGrob(qqplot, dens, ncol = 2))
      
      })
      
      
      output$pred <- renderPlot({
        if(input$abs){
          g <- ggplot(NULL) + geom_line(aes(x=dataRet()$time, y=abs(dataRet()$return)),
                                        col='darkorange') + xlab('') + ylab('')+
            theme_minimal() + geom_hline(yintercept= 0 , col='red',size=1.1, alpha=.5) +
            geom_line(aes(x=dataRet()$time, y= fit()@fit$sigma  ), col='green', size=1.1)
          g
          
        }else{
        g <- ggplot(NULL) + geom_line(aes(x=dataRet()$time, y=dataRet()$return),
                                      col='darkorange') + xlab('') + ylab('')+
          theme_minimal() + geom_hline(yintercept= 0 , col='red',size=1.1, alpha=.5) +
          geom_line(aes(x=dataRet()$time, y= fit()@fit$sigma  ), col='green') + 
          geom_line(aes(x=dataRet()$time, y=-fit()@fit$sigma  ), col='red')

       g}
      })
      
      output$pred1 <- renderPlot({
        n  = length(dataRet()$time)
        nh = as.numeric(input$nhead)
        forc =  ugarchforecast(fit(), n.ahead = nh)
        ggplot(NULL) + geom_line(aes(x = dataRet()$time[(n-40):n],
                                     y = dataRet()$return[(n-40):n]),
                                 col='darkorange')+
          geom_line(aes(x = seq(from=as.Date(dataRet()$time[n]+1),
                                length.out = nh, by='day'), 
                        y =forc@forecast$seriesFor ), col='blue') + 
          geom_line(aes (x = seq(from=as.Date(dataRet()$time[n]+1),
                                 length.out = nh, by='day'), 
                         y = forc@forecast$sigmaFor),col='green') + 
          geom_line(aes (x = seq(from=as.Date(dataRet()$time[n]+1),
                                 length.out = nh, by='day'), 
                         y = -forc@forecast$sigmaFor),col='red') + 
          xlab('time') + ylab('')+theme_minimal() + 
          ggtitle(paste('Predicciones a', nh,'periodos'))
          
        
      })
   ###########################################   
      xData = reactive({
        xts(cotizaciones()$AdjClose, 
                order.by = as.Date(time(cotizaciones()),"%Y-%m-%d"),
                frequency=365)
        })
      
      xRent = reactive({
        na.omit(monthlyReturn(xData(),type='log',leading=FALSE))
      })
      
      meses = reactive({
        .indexmon(xRent()) })
      

      #Select First day of the month
      xMonthFirstData=reactive({
        xData()[head(endpoints(xData(), "months") + 1, -1)]
      })
      
      #Generate indicator: 1 =[May to Nov]
      Indicador = reactive({
        (.indexmon(xMonthFirstData())== (as.numeric(input$mesC)-1) |
           .indexmon(xMonthFirstData())== (as.numeric(input$mesV)-1))
      })
      
      
      #Select data from mesC to mesV
      xCV=reactive({
        xMonthFirstData()[Indicador()]
      })
      
  
      
      #Generate accumulate return [May-Nov]
      xRentCV = reactive({
        na.omit(diff(log(xCV()))[.indexmon(xCV())==(as.numeric(input$mesV)-1)])
      })
        
      output$prueba <- renderPrint({

        xCV()
      })
      
      output$estac <- renderPlot({
        
        
        ggfreqplot(as.ts(return(),0),freq=12,nrow=2,
                   facet.labeller=month.name ,
                   conf.int.colour = 'blue',
                   col='darkorange') + 
          ggtitle("Rentabilidades Estacionales") + 
          geom_hline(yintercept = 0,color="red",size=1) + 
          theme_minimal()
      }) 
      
      output$estac1 <- renderTable({
        
        data.frame(estimate = as.numeric(t.test(xRentCV())$estimate),
                   statistic = as.numeric(t.test(xRentCV())$statistic),
                   Pvalue = as.numeric(t.test(xRentCV())$p.value),
                   Conf.IntervalNeg = t.test(xRentCV())$conf[1],
                   Conf.IntervalPos = t.test(xRentCV())$conf[2])
      }, digits=3, hover=T, bordered=T)
      
      



    


  
  
}
