
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(tseries)
library(rvest)
library(quantmod)
library(forecast)
library(shinydashboard)
library(stargazer)
library(highcharter)
library(gridExtra)
library(moments)
library(markdown)
library(ggplot2)
library(ggfortify)
library(rugarch)
library(knitr)
library(rmarkdown)
library(pander)

dashboardPage(
  
  # Application title
  dashboardHeader(title='Aplicación de Shiny',
                  dropdownMenu(type = 'notification', icon = icon(icon('users')),
                               notificationItem(text = textOutput('noti')))),

  # Sidebar with a slider input for number of bins
  dashboardSidebar(
    sidebarMenu(
      menuItem('Introducción', tabName='intro', icon = icon('database')),
      menuItem('Compañía', tabName='empresa', icon = icon('hand-pointer-o')),
      menuItem('Gráficos', tabName='grafs', icon = icon("line-chart")),
      menuItem('GARCH models', tabName='analisis', icon = icon("eur")),
      menuItem('Sell and Go', tabName = 'sell', icon = icon("calendar"))
    ) 
  ),
  
  
  # Show a plot of the generated distribution
  dashboardBody(
    
    tabItems(
      tabItem(tabName = 'intro',
              fluidRow(
                box(title='Explicación de la aplicación', width = 3, 'En esta aplicación el usuario podrá seleccionar 
                    las distintas empresas del IBEX35 y ver el comportamiento de su serie. Tanto la evolución del precio de la acción, así como las rentabilidades diarias.
                    Además el usuario podrá explorar distintos modelos de la familia de los GARCH models y también se podrá comprobar si el Brexit afectó al valor y a la volatilidad de la acción o no. Y finalmente, comprobar si para una acción determinada se puede seguir algun tipo de estrategia estacional. Es decir, comprar una acción en un determinado mes, mantenerla hasta otro mes. 
                    '),

# Explicación GARCH -------------------------------------------------------------


                box(width = 8, title = 'GARCH', 'Uno de los temas más actuales de las series temporales es la predicción de las volatilidades de los retornos de un determinado activo (normalmente acciones de empresas). Estos modelos se conoce como conditional heteroscedastic models. Utilizado por ejemplo cuando se intenta estimar el precio de una opción "call" utilizando el método de Black-Scholes, tambien para modelos de Value at risk para una determinada posición de un portfolio, etc.', 

                    'En la siguiente figura observamos como los logs return de S&P 500 siguen claramente un ruido blanco; por lo tante hacen que un modelo ARIMA o ETS no sea útil para predecir la serie. Sin embargo, se puede observar que la varianza no es constante a lo largo del tiempo: 
                    hay periodos que hay mayor variabilidad que otros',
                    plotOutput('exp', height = "200px"), 'El modelo general se basa en que los log returns:',

  withMathJax('$$r_{t} = \\mu + a_{t} \\quad   ; \\quad a_{t} = \\sigma_{t}\\epsilon_{t}$$'),
                    
  withMathJax('$$ \\mu \\text{es la media de la serie (casi siempre 0, aunque puede seguir un modelo ARIMA, etc}$$'),
  withMathJax('$$\\epsilon \\sim N(0,1)$$'),
                    
  withMathJax('$$\\sigma_{t}^{2}  \\text{es lo que los modelos GARCH buscan predecir.}$$'),
                    
                    'Los GARCH models tienen decenas de variaciones y modificaciones: En esta aplicación se pueden escojer varias de las modificaciones más importantes. 
  Una de las contribuciones más interesantes es la que incluye en los modelos la asimetría. Es decir, que cuando se producen retornos negativos grandes, la volatilidad aumenta más que si estos retornos fuesen positivos'
                                    , tabsetPanel(
                                      tabPanel('eGARCH','El GARCH exponencial derivado por Nelson en 1991 sigue el siguiente planteamiento:',
                                               withMathJax('$$ ln(\\sigma_{t}^{2}) = \\sum_{j=1}^{q}(\\alpha_{j}z_{t-j}+ \\gamma_{j}(|z_{t-j}|-E|z_{t-j}|)) + \\sum_{j=1}^{q}\\beta_{j}ln(\\sigma_{t-j}^{2})$$')),
                                      tabPanel('gjrGARCH', 'Desarrollado por Glosten en 1993 utilizando el indicativo I que toma valor 1 cuando la acción ha tenido una caída y 0 en el resto de los casos. Seguiría el siguiente planteamiento:',
                                               withMathJax('$$ \\sigma_{t} = \\sum_{j=1}^{q} (\\alpha_{j} \\epsilon_{t-j}^{2} + \\gamma_{j}I_{t-j}\\epsilon_{t-j}^{2}) + \\sum_{j=1}^{q}\\beta_{j} \\sigma_{t-j}^{2} $$')),
                                      tabPanel('apGARCH', 'Desarrollado por Ding en 1993 que permite el estudio tanto de la asimetría en los retornos, así como el efecto Taylor. Taylor (de allí el nombre Taylor effect), observó que la autocorrelación de los retornos absolutos era mayor que la autocorrelación con los retornos al cuadrado. 
                                               El modelo sigue la siguiente especificación:',
                                               withMathJax('$$ \\sigma_{t}^{\\delta} =\\sum_{j=1}^{q} \\alpha_{j}(|\\epsilon_{t-j}| - \\gamma_{j}\\epsilon_{t-j})^{\\delta} + \\sum_{j=1}^{q}\\beta_{j} \\sigma_{t-j}^{\\delta}$$'))
                                    )))
                ),

# Fin explicación ---------------------------------------------------------


      tabItem(tabName = 'empresa',
              fluidRow(
                box(h1('Empresa del IBEX35'),width=4,
                    dateRangeInput('daterange', label='Seleccione el rango de fechas', 
                                   start  = Sys.Date()-1600, end = Sys.Date()),
                    selectInput('accion', 'Por favor seleccione la empresa:',
                                choices = c('Abertis Infraestructuras S.A.'= '1', 'Actividades de Construcción y Servicios S.A.'='2', 
                                            'Acerinox, S.A.'='3','Aena, S.A.'='4','Amadeus IT Holding SA'='5','Acciona, S.A.'='6',
                                            'Banco Bilbao Vizcaya Argentaria, S.A.'='7','Bankia, S.A.'='8','Bankinter, S.A.'='9',
                                            'CaixaBank, S.A.'='10','Distribuidora Internacional de Alimentación, S.A.'='11',
                                            'Endesa, Sociedad Anonima'='12','Enagás, S.A.'='13','Fomento de Construcciones y Contratas, S.A.'='14',
                                            'Ferrovial, S.A.'='15','Gamesa Corporación Tecnológica S.A.'='16', 'Gas Natural SDG SA'='17',
                                            'Grifols, S.A.'='18','International Consolidated Airlines Group, S.A.'='19','Iberdrola, S.A.'='20',
                                            'Indra Sistemas, S.A.'='21','Industria de Diseno Textil Inditex SA'='22','Mapfre SA'='23',
                                            'ArcelorMittal SA'='24','Banco Popular Espanol S.A.'='25',
                                            'Red Eléctrica Corporación S A.'='26', 'Repsol, S.A.'='27','Banco de Sabadell, S.A.'='28',
                                            'Banco Santander, S.A.'='29','Telefónica, S.A.' = '30','Mediaset España Comunicación, SA'='31',
                                            'Tecnicas Reunidas, S.A.'='32','Viscofan SA'='33')
                )),
                box(h1('Gráfico'), width= 8, highchartOutput('distPlot'))
              ),
              fluidRow(
                box(title='Breve descripción', status = "warning", solidHeader = TRUE,
                    width=3, textOutput('des')),
                box(title='Estadísticas de la compañía', width=7,
                    status = "warning", solidHeader = TRUE, tableOutput('table'))
              )
      ),
      tabItem(tabName = 'grafs',
              fluidPage(
                box(title='Serie aritmetica o en logarítmo', width=6, 
                    checkboxInput('enlog','En logarítmo'),
                    checkboxInput('brexit1', 'Brexit'),
                    plotlyOutput('logs')),
                box(title='Log returns', width=6,
                    checkboxInput('var', 'Elevado al cuadrado'), plotlyOutput('return'))
              ),
              fluidPage(
                box(title = 'Principales estadísticas', width=3,
                    status = "warning", solidHeader = TRUE,
                    tableOutput('stats')),
                box(title='Correlogramas de los returns', width=4 , status = "warning",
                    solidHeader = TRUE,
                    checkboxInput('var1','Elevado al cuadrado'), plotOutput('ACF')),
                box(title='Distribución de los returns', width=5 , 
                    status = "warning", solidHeader = TRUE, 
                    checkboxInput('norm', 'Distribución normal'), plotOutput('distribution'))
              )),
      tabItem(tabName = 'analisis',
              fluidRow(
                box(h1('Modelo GARCH'), width = 3,status = "primary", 
                    solidHeader = F,
                    collapsible = TRUE,
                    selectInput('modelo', 'Escoja el modelo', choices = c('GARCH' = 'sGARCH',
                                                                          'eGARCH' = 'eGARCH', 
                                                                          'gjrGARCH' = 'gjrGARCH',
                                                                          'csGARCH' = 'csGARCH',
                                                                          'apARCH' = 'apARCH',
                                                                          'fGARCH' = 'fGARCH')),
                    conditionalPanel(condition =  "input.modelo == 'fGARCH'",
                                     selectInput('submodel', 'Escoja el submodelo de fGARCH',
                                                 choices = c('GARCH', 'TGARCH', 'AVGARCH',
                                                             'NGARCH', 'APARCH','ALLGARCH'))),
                    checkboxInput('mean','Include mean'),
                    sliderInput('p', 'Seleccione la p', min=0, max=4, value=1),
                    sliderInput('q', 'Seleccione la q', min=0, max=4, value=0),
                    selectInput('Gdist', 'Seleccione la distribución de las innovaciones', 
                                choices = c('Normal' = 'norm',
                                            'Student' = 'std')),
                    checkboxInput('brexit', 'Brexit'),
                      conditionalPanel(condition = "input.brexit",
                            sliderInput('dias', 'Seleccione los días',
                                min=-1, max=10, value = c(0,1)),
                            checkboxInput('xreg', 'Varianza Brexit'),
                              conditionalPanel(condition = "input.xreg",
                                               sliderInput('nxreg', 'Seleccione los días', 
                                                           min=0, max=5, value=1)))
                      ),
                box(h1('Parámetros estimados'), width = 7, tableOutput('params'), tableOutput('params1'))
               
              ),
              fluidRow(
                box(title='Análisis de los residuos',status='danger', 
                          plotOutput('resid')
                    ),
                tabBox(
                  tabPanel('Fitting the model',checkboxInput('abs', 'Absolut values'),
                           plotOutput('pred')),
                  tabPanel('Predicciones', 
                           sliderInput('nhead','Seleccione el número de periodos de predección', 
                                       min=1,max=20,value=5), 
                    plotOutput('pred1'))
                )
                
              )
              
      ),
      tabItem(tabName = 'sell',
              fluidRow(
                box(title='Meses', width=3, 
                    selectInput('mesC', 'Seleccione el mes de Compra del activo',
                                choices = c('Enero' = 1, 'Febrero' = 2, 'Marzo'=3,
                                            'Abril' = 4, 'Mayo' = 5, 'Junio'= 6,
                                            'Julio' = 7, 'Agosto' = 8, 'Septiembre'=9,
                                            'Octubre' = 10, 'Noviembre' = 11, 'Diciembre'=  12)),
                    selectInput('mesV', 'Seleccione el mes de Venta del activo',
                                choices = c('Enero' = 1, 'Febrero' = 2, 'Marzo'=3,
                                            'Abril' = 4, 'Mayo' = 5, 'Junio'= 6,
                                            'Julio' = 7, 'Agosto' = 8, 'Septiembre'=9,
                                            'Octubre' = 10, 'Noviembre' = 11, 'Diciembre'=  12), 
                                'Febrero')),
                box(title = 'Estacionalidad', width=7, plotOutput('estac'),
                    'A continuación se hace el t.test para la media: Si el p.value es menor que 0 significaría 
                      que la estrategia es buena',
                    tableOutput('estac1'))
              )

)
      )
))
                
      
  

