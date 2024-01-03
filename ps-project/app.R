library(shiny)
library(shinydashboard)
library("ggplot2")
library(reshape2)
library(rsconnect)
library(plotly)
library(readxl)
library(Rlab)


ui <- dashboardPage(
  dashboardHeader(title = "Proiect Probabilitati"),
  dashboardSidebar(
    sidebarMenu(id='menus',
                menuItem(text = "Repartitii" ,tabName = "density")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("density",
              fluidRow(column(width = 11,offset = 3,box(uiOutput('densitate'), title=strong('Functie De Densitate :'), collapsible = F,collapsed = T,solidHeader = T))),
              fluidRow(
                tabsetPanel(id = 'tabs',
                            tabPanel(title='Uniforma',value='unif',fluidRow(
                              column(8, plotlyOutput('grafic_uniforma')),
                              column(3,wellPanel(
                                sliderInput(inputId = "inf",label = "a",min = -10,max = 10,value = 0,step = 0.2),br(),
                                sliderInput(inputId = "sup",label = "b",min = -10,max = 10,value = 1,step = 0.2),br(),
                                actionButton(inputId = 'resetare_parametrii_uniforma',label = "Resetare"))
                              ))),
                            tabPanel(title='Normala',value='norm',fluidRow(
                              column(8, plotlyOutput('grafic_normala')),
                              column(3,wellPanel(
                                sliderInput(inputId = "mu",label = "Medie (miu)",min = -10,max = 10,value = 0,step = 0.2),br(),
                                sliderInput(inputId = "var",label = "Varianta (sigma^2)",min = 0,max = 10,value = 1,step = 0.2),br(),
                                actionButton(inputId = 'resetare_parametrii_normala',label = "Resetare"))
                              ))),
                            tabPanel(title = "Exponentiala", value='exp', fluidRow(
                              column(8, plotlyOutput("grafic_exponentiala")),
                              column(3, wellPanel(
                                sliderInput(inputId = "lamb",label = "lambda", min = 0.1, max = 3, step = 0.1, value = 1),br(),
                                actionButton(inputId = "resetare_parametrii_exponentiala", label='reset'))
                              ))),
                            tabPanel('title'='Cauchy',value='cauchy', fluidRow(
                              column(8, plotlyOutput("grafic_cauchy")),
                              column(3,wellPanel(
                                sliderInput(inputId = "xcauchy",label = "x0",min = -5,max = 5,value = 0,step = 0.1),br(),
                                sliderInput(inputId = "acauchy",label = "a",min = 0.1,max = 10,value = 1,step = 0.1),br(),
                                actionButton(inputId = "resetcauchy",label = "Resetare"))
                              ))),
                            tabPanel('title'='Logistica',value='logi',fluidRow(
                              column(8, plotlyOutput("grafic_logi")),
                              column(3,wellPanel(
                                sliderInput(inputId = "mulogi",label = "miu",min = -10,max = 10,value = 0,step = .1),br(),
                                sliderInput(inputId = "slogi",label = "s",min = .1 ,max = 10,value = 1 ,step = 1),br(),
                                actionButton(inputId = "resetlogi",label = "Resetare"))
                              ))),
                            tabPanel('title'='Poisson',value='poisson',fluidRow(
                              column(8, plotlyOutput("plotpoisson")),
                              column(3,wellPanel(
                                sliderInput(inputId = "poisson_lambda",label = "lambda", min = 0, max = 100, step = 0.1, value = 1),br(),
                                actionButton(inputId = "resetpoisson",label = "Resetare"))
                              ))),
                            tabPanel('title'='Bernoulli',value='bernoulli',fluidRow(
                              column(8, plotlyOutput("plotbernoulli")),
                              column(3,wellPanel(
                                sliderInput(inputId = "bernoulli_p",label = "p", min = 0, max = 1, step = 0.02, value = 1),br(),
                                actionButton(inputId = "resetbernoulli",label = "Resetare"))
                              ))),
                            tabPanel('title'='Geometrica',value='geometric',fluidRow(
                              column(8, plotlyOutput("plotgeometric")),
                              column(3,wellPanel(
                                sliderInput(inputId = "geometric_p",label = "p", min = 0.02, max = 1, step = 0.02, value = 1),br(),
                                actionButton(inputId = "resetgeometric",label = "Resetare"))
                              ))),
                            tabPanel('title'='Hipergeometrica',value='hipergeometric',fluidRow(
                              column(8, plotlyOutput("plothipergeometric")),
                              column(3,wellPanel(
                                sliderInput(inputId = "hipergeometric_N",label = "N", min = 1, max = 100, step = 1, value = 1),br(),
                                sliderInput(inputId = "hipergeometric_M",label = "M", min = 1, max = 100, step = 1, value = 1),br(),
                                sliderInput(inputId = "hipergeometric_n",label = "n", min = 1, max = 100, step = 1, value = 1),br(),
                                actionButton(inputId = "resethipergeometric",label = "Resetare"))
                              ))),
                            tabPanel('title'='Binomiala',value='binomial',fluidRow(
                              column(8, plotlyOutput("plotbinomial")),
                              column(3,wellPanel(
                                sliderInput(inputId = "binomial_n",label = "n", min = 1, max = 100, step = 1, value = 1),br(),
                                sliderInput(inputId = "binomial_p",label = "p", min = 0, max = 1, step = 0.02, value = 1),br(),
                                actionButton(inputId = "resetbinomial",label = "Resetare"))
                              ))),
                            tabPanel('title'='Lognormala',value='logn', fluidRow(
                              column(8, plotlyOutput("grafic_lognormala")),
                              column(3,wellPanel(
                                sliderInput(inputId = "mulogn",label = "miu",min = -10,max = 10,value = 0,step = 0.1),br(),
                                sliderInput(inputId = "varlogn",label = "varianta",min = 0.01 ,max = 2,value = 1 ,step = 0.01),br(),
                                actionButton(inputId = "resetare_parametrii_lognormala",label = "Resetare"))
                              ))),
                            tabPanel('title'='Chi^2',value='khi', fluidRow(
                              column(8, plotlyOutput("grafic_khi2")),
                              column(3,wellPanel(
                                sliderInput(inputId = "kkhi",label = "k",min = 1 ,max = 20,value = 1 ,step = 1),br(),
                                actionButton(inputId = "resetkhi",label = "Resetare"))
                              ))),
                            tabPanel('title'='Gamma',value='gamma',fluidRow(
                              column(8, plotlyOutput("grafic_gamma")),
                              column(3,wellPanel(
                                sliderInput(inputId = "kgamma",label = "k",min = 0.1,max = 10,value = 1,step = 0.1),br(),
                                sliderInput(inputId = "thetagamma",label = "theta",min = 0.1,max = 10,value = 1,step = 0.1),br(),
                                actionButton(inputId = "resetgamma",label = "Resetare"))
                              ))),
                            tabPanel('title'='Beta',value='beta',fluidRow(
                              column(8, plotlyOutput("grafic_beta")),
                              column(3,wellPanel(
                                sliderInput(inputId = "alphabeta",label = "alpha",min = 0.1,max = 10,value = 0.5,step = 0.1),br(),
                                sliderInput(inputId = "betabeta",label = "beta",min = 0.1,max = 10,value = .5,step = 0.1),br(),
                                actionButton(inputId = "resetbeta",label = "Resetare"))
                              )))
                            
                )
              ),
              fluidRow(br()),
              fluidRow(box(uiOutput('medie'),title = strong('Medie') ,collapsible=F, collapsed=F, solidHeader=T),
                       box(uiOutput('varianta'),title = strong('Varianta'), collapsible=F, collapsed=F, solidHeader=T)),
              fluidRow(infoBoxOutput(outputId = "val_medie",width = 6),
                       infoBoxOutput(outputId = "var",width = 6))
      )
    )
  )
)

server <- function(input, output,session) {
  
  x <- reactive({switch (input$tabs,
                         'unif' = seq(-10,10,0.1),
                         'norm' = seq(-10,10,0.1),
                         'exp' = seq(0,20,0.1),
                         'gamma' = seq(0,20,0.1),
                         'beta' = seq(0,1,0.01),
                         'cauchy' = seq(-5,5,.1),
                         'logn' = seq(0,5,.01),
                         'khi' = seq(0,20,0.1),
                         'logi' = seq(-5,30,.1),
                         'poisson' = seq(0, 100, 1),
                         'geometric' = seq(0, 1, 0.02),
                         'bernoulli' = seq(0, 1, 0.02),
                         'hipergeometric' = seq(1, 100, 1),
                         'binomial' = seq(0, 100, 1)
  )})
  
  dlogi <- function(x,mu,s){ e <- exp(-(x-mu)/s)
  return(e/(s*(1+e)**2))}
  
  functii_densitate_repartitii <- reactive({switch(input$tabs,
                                                   'unif' = dunif(x(),min(input$inf, input$sup),max(input$inf,input$sup)),
                                                   'norm' = dnorm(x(), input$mu, sqrt(input$var)) ,
                                                   'exp' = dexp(x(),input$lamb),
                                                   'gamma' = dgamma(x(),shape = input$kgamma, scale = input$thetagamma),
                                                   'beta' = dbeta(x(),input$alphabeta, input$betabeta),
                                                   'cauchy' = dcauchy(x(),location = input$xcauchy, scale = input$acauchy),
                                                   'logn' = dlnorm(x(),input$mulogn, sqrt(input$varlogn)),
                                                   'khi' = dchisq(x(),input$kkhi),
                                                   'logi' = dlogi(x(),input$mulogi,input$slogi),
                                                   'hipergeometric' = dhyper(x(), input$hipergeometric_M, input$hipergeometric_N, input$hipergeometric_n),
                                                   'binomial' = dbinom(x(), input$binomial_n, input$binomial_p),
                                                   'poission' = dpois(x(), input$poisson_lambda),
                                                   'geometric' = dgeom(x(), input$gemoetric_p),
                                                   'bernoulli' = dbern(x(), input$bernoulli_p)
  )})
  
  minimumx <- reactive({switch(input$tabs,
                               'unif' = -10,
                               'norm' = -10,
                               'exp' = 0,
                               'gamma' = 0,
                               'beta' = 0,
                               'cauchy' = -5,
                               'logn' = 0,
                               'khi' = 0,
                               'logi' = -5,
                               'poisson' = -10,
                               'geometric' = 0,
                               'hipergeometric' = 1,
                               'binomial' = 0,
                               'bernoulli' = 0
  )})
  
  maximumx <- reactive({switch (input$tabs,
                                'unif' = 10,
                                'norm' = 10,
                                'exp' = 20,
                                'gamma' = 20,
                                'beta' = 1,
                                'cauchy' = 5,
                                'logn' = 3,
                                'khi' = 20,
                                'logi' = 30,
                                'poisson' = 100,
                                'geometric' = 1,
                                'hipergeometric' = 100,
                                'binomial' = 100,
                                'bernoulli' = 1
  )})
  
  minimumy <- reactive({switch(input$tabs,
                               'unif' = 0,
                               'norm' = 0,
                               'exp' = 0,
                               'gamma' = 0,
                               'beta' = 0,
                               'cauchy' = 0,
                               'logn' = 0,
                               'khi' = 0,
                               'logi' = 0,
                               'poisson' = 0,
                               'geometric' = 0,
                               'hipergeometric' = 0,
                               'binomial' = 0,
                               'bernoulli' = 0
                               
  )})
  
  maximumy <- reactive({switch (input$tabs,
                                'unif' = max(1/(max(input$sup,input$inf)-min(input$sup,input$inf))+0.1, 1),
                                'norm' = .45,
                                'exp' = input$lamb+1,
                                'gamma' = 0.5,
                                'beta' = 2.6,
                                'cauchy' = .7,
                                'logn' = 2.5,
                                'khi' = .5,
                                'logi' = .3,
                                'poisson' = 10,
                                'geometric' = 1,
                                'hipergeometric' = 1,
                                'binomial' = 1,
                                'bernoulli' = 1
  )})
  
  valoare_medie <- reactive({switch (input$tabs,
                                     'unif' = (input$inf+input$sup)/2,
                                     'norm' = input$mu,
                                     'exp' = 1/input$lamb,
                                     'gamma'= input$kgamma*input$thetagamma,
                                     'beta' = (input$alphabeta)/(input$alphabeta+input$betabeta),
                                     'cauchy' = 'Non defined',
                                     'logn' = exp(input$mulogn+input$varlogn/2),
                                     'khi' = input$kkhi,
                                     'logi' = input$mulogi,
                                     'poisson' = input$poisson_lambda,
                                     'geometric' = 1/input$geometric_p,
                                     'hipergeometric' = input$hipergeometric_n * (input$hipergeometric_M * input$hipergeometric_N),
                                     'binomial' = input$binomial_n * input$binomial_p ,
                                     'bernoulli' = input$bernoulli_p
  )})
  
  valoare_varianta <- reactive({switch (input$tabs,
                                        'unif' = (input$sup-input$inf)**2/12,
                                        'norm' = input$var,
                                        'exp' = 1/input$lamb**2,
                                        'gamma' = (input$kgamma*input$thetagamma**2),
                                        'beta' = input$alphabeta*input$betabeta/(((input$alphabeta+input$betabeta)**2)*(input$alphabeta+1+input$betabeta)), 
                                        'cauchy' = 'Non defined',
                                        'logn' = exp(input$varlogn-1)*exp(2*input$mulogn+input$varlogn),
                                        'khi' = 2*input$kkhi,
                                        'logi' = input$slogi**2*pi/3,
                                        'poisson' = input$poisson_lambda,
                                        'geometric' = (1-input$geometric_p)/input$geometric_p^2,
                                        'hipergeometric' = input$hipergeometric_n * (input$hipergeometric_M / input$hipergeometric_N) * ((input$hipergeometric_N-input$hipergeometric_M) / input$hipergeometric_N) * ((input$hipergeometric_N-input$hipergeometric_n) / (input$hipergeometric_N - 1)),
                                        'binomial' = input$binomial_n * input$binomial_p* (1-input$binomial_p),
                                        'bernoulli' = input$bernoulli_p-input$bernoulli_p^2
  )})
  
  output$val_medie <- renderInfoBox({infoBox(title='Valoarea Mediei', subtitle=' ', color='red', value=paste('medie =', as.character(valoare_medie()),collapse = ' '), fill = T)})
  output$var <- renderInfoBox({infoBox(title='Valoarea Variantei ',subtitle='     ',color='red', value={paste('varianta =', as.character(valoare_varianta()),collapse=' ')},fill=T)})
  
  observeEvent(input$resetare_parametrii_uniforma, {updateSliderInput(session, 'inf',value=0); updateSliderInput(session, 'sup',value=1)})
  observeEvent(input$resetare_parametrii_normala, {updateSliderInput(session, 'mu',value=0); updateSliderInput(session, 'var',value=1)})
  observeEvent(input$resetare_parametrii_exponentiala, {updateSliderInput(session, 'lamb',value=1)})
  observeEvent(input$resetgamma, {updateSliderInput(session, 'kgamma',value=1);updateSliderInput(session, 'thetagamma',value=1)})
  observeEvent(input$resetbeta, {updateSliderInput(session, 'alphabeta',value=0.5);updateSliderInput(session, 'betabeta',value=0.5)})
  observeEvent(input$resetgum, {updateSliderInput(session, 'mugum', value=0);updateSliderInput(session, 'betagum', value=1)})
  observeEvent(input$resetare_parametrii_lognormala, {updateSliderInput(session, 'mulogn', value=0);updateSliderInput(session, 'varlogn', value=1)})
  observeEvent(input$resetkhi, {updateSliderInput(session = session, 'kkhi', value=1)})
  observeEvent(input$resetlogi, {updateSliderInput(session, 'mulogi', value=0);updateSliderInput(session, 'slogi', value=1)})  
  observeEvent(input$resetpoisson, {updateSliderInput(session, 'poisson_lambda', value=1)})  
  observeEvent(input$resetgeometric, {updateSliderInput(session, 'geometric_p', value=1)}) 
  observeEvent(input$resetbernoulli, {updateSliderInput(session, 'bernoulli_p', value=1)})
  observeEvent(input$resethipergeometric, {updateSliderInput(session, 'hipergeometric_N', value=1);updateSliderInput(session, 'hipergeometric_M', value=1);updateSliderInput(session, 'hipergeometric_n', value=1)})
  observeEvent(input$resetbinomial, {updateSliderInput(session, 'binomial_n', value=1);updateSliderInput(session, 'binomial_p', value=1)})
  
  output$grafic_uniforma <- output$grafic_normala <- output$grafic_exponentiala <- output$grafic_gamma <- output$grafic_beta <- output$grafic_cauchy <- output$grafic_lognormala <-
    output$grafic_khi2 <- output$grafic_logi <- renderPlotly({
      df <- melt(data.frame(x(), functii_densitate_repartitii()), id='x..')
      p <- ggplot(data=df, aes(x=x.., y=value, colour=variable)) + geom_line() + xlim(minimumx(),maximumx()) + ylim(minimumy(),maximumy()) + theme(legend.position = 'none')
      p <- p + labs(x = 'x', y = 'f(x)') + scale_fill_discrete(name = "Values", labels = c("Reference", "New"))
      print(ggplotly(p,tooltip = c('y')))
    })
  
  output$plotpoisson <- renderPlotly(print(ggplot(transform(data.frame(x=c(0:100)), y=dpois(x, input$poisson_lambda)), aes(x, y)) + 
                                             geom_bar(stat="identity")));
  output$plotgeometric <- renderPlotly(print(ggplot(transform(data.frame(x=c(0:250)), y=dgeom(x, input$geometric_p)), aes(x, y)) +
                                               geom_bar(stat="identity")));
  output$plotbernoulli <- renderPlotly(print(ggplot(transform(data.frame(x=c(0:1)), y=dbern(x, input$bernoulli_p)), aes(x, y)) +
                                               geom_bar(stat="identity")));
  
  output$plothipergeometric <- renderPlotly(print(ggplot(transform(data.frame(x=c(0:100)), y=dhyper(x, input$hipergeometric_M, input$hipergeometric_N, input$hipergeometric_n)), aes(x, y)) +
                                                    geom_bar(stat="identity")));
  output$plotbinomial <- renderPlotly(print(ggplot(transform(data.frame(x=c(0:100)), y=dbinom(x, input$binomial_n, input$binomial_p,)), aes(x, y)) +
                                              geom_bar(stat="identity")));
  
  # Adugare date pt box-ul de medie
  output$medie <- renderUI({p({switch(input$tabs,'unif' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{a+b}{2}$$'),
                                      'norm' = withMathJax('$$\\mathbb{E}[X]=\\mu$$'),
                                      'exp' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{1}{\\lambda}$$'),
                                      'gamma' = withMathJax('$$\\mathbb{E}[X]=k\\theta$$'),
                                      'beta' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{\\alpha}{\\alpha+\\beta}$$'),
                                      'cauchy' = withMathJax('$$\\mathbb{E}[X]=\\varnothing$$'),
                                      'gum' = withMathJax('$$\\mathbb{E}[X]=\\mu+\\beta\\gamma,\\textrm{ with }\\gamma=-\\Gamma\'(1)$$'),
                                      'logn' = withMathJax('$$\\mathbb{E}[X]=e^{\\mu+\\frac{\\sigma^2}{2}}$$'),
                                      'khi' = withMathJax('$$\\mathbb{E}[X]=k$$'),
                                      'logi' = withMathJax('$$\\mathbb{E}[X]=\\mu$$'),
                                      'poisson' = withMathJax('$$\\mathbb{E}[X]={\\lambda}$$'),
                                      'geometric' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{1}{p}$$'),
                                      'bernoulli' = withMathJax('$$\\mathbb{E}[X]={p}$$'),
                                      'hipergeometric' = withMathJax('$$n\\dfrac{M}{N}$$'),
                                      'binomial' = withMathJax('$$\\mathbb {E}[X]= {x}{p}$$')
  )})})
  
  
  # Adugare date pt box-ul de densitate
  output$densitate <- renderUI({p({switch(input$tabs,'unif' = withMathJax('$$f(x)=\\dfrac{1}{b-a}\\textbf{1}\\{a\\leq x \\leq b\\}$$'),
                                          'norm' = withMathJax('$$f(x)=\\dfrac{1}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$$'),
                                          'exp' = withMathJax('$$f(x)=\\lambda e^{-\\lambda x}\\textbf{1}_{\\mathbb{R}^+}$$'),
                                          'gamma' = withMathJax('$$f(x)=\\dfrac{x^{k-1}e^{-\\frac{x}{\\theta}}}{\\Gamma(k)e^k}\\textbf{1}_{\\mathbb{R}^+},\\textrm{ with } \\Gamma(z)=\\int_{0}^{+\\infty}t^{z-1}e^{-t}\\mathrm{d}t,\\quad \\Gamma(x+1)=x\\Gamma(x)$$'),
                                          'beta' = withMathJax('$$f(x)=\\dfrac{x^{\\alpha-1}(1-x)^{\\beta-1}}{\\textbf{B}(\\alpha,\\beta)} \\textbf{1}_{[0,1]},\\textrm{ with } \\textbf{B}(x,y)=\\dfrac{\\Gamma(x)\\Gamma(y)}{\\Gamma(x+y)}$$'),
                                          'cauchy' = withMathJax('$$f(x)=\\dfrac{1}{\\pi a\\left[1+\\left(\\dfrac{x-x_0}{a}\\right)^2\\right]}$$'),
                                          'gum' = withMathJax('$$f(x)=\\dfrac{e^{-z}z}{\\beta}\\textrm{ with }z=e^{-\\frac{x-\\mu}{\\beta}}$$'),
                                          'logn' = withMathJax('$$f(x)=\\dfrac{1}{x\\sigma\\sqrt{2\\pi}}\\mathrm{exp}\\left(-\\dfrac{(\\ln x-\\mu)^2}{2\\sigma^2}\\right)\\textbf{1}_{\\mathbb{R}^+_*}$$'),
                                          'khi' = withMathJax('$$f(x)=\\dfrac{\\left(\\frac{1}{2}\\right)^{\\frac{k}{2}}}{\\Gamma\\left(\\frac{k}{2}\\right)}x^{\\frac{k}{2}-1}e^{-\\frac{x}{2}}\\mathrm{1}_{\\mathbb{R}^+}$$'),
                                          'logi' = withMathJax('$$f(x)=\\dfrac{e^{-\\frac{(x-\\mu)}{s}}}{s\\left(1+e^{-\\frac{(x-\\mu)}{s}}\\right)^2}$$'),
                                          'poisson' = withMathJax('$$\\mathbb f(x)=\\dfrac{{\\lambda}^xe^{-x}}{x!}$$'),
                                          'geometric' = withMathJax('$$\\mathbb f(x)={(1-p)^{x-1}}{p}$$'),
                                          'hipergeometric' = withMathJax('$$f(x)=\\dfrac{{M \\choose x}{N-M \\choose n-x}}{{N \\choose n}}$$'),
                                          'bernoulli' = withMathJax('$$\\mathbb f(x)={p^x}{(1-p)^{1-x}}$$'),
                                          'binomial'= withMathJax('$$\\mathbb f(x)= {n \\choose x}{p^x}{(1-p)^{(n-x)}} $$')
  )})})
  
  # Adaugare date pt box-ul de varianta 
  output$varianta <- renderUI({p({switch(input$tabs,'unif' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{(b-a)^2}{12}$$'),
                                         'norm' = withMathJax('$$\\mathbb{V}[X]=\\sigma^2$$'),
                                         'exp' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{1}{\\lambda^2}$$'),
                                         'gamma' = withMathJax('$$\\mathbb{V}[X]=k\\theta^2$$'),
                                         'beta' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{\\alpha\\beta}{(\\alpha+\\beta)^2(\\alpha+\\beta+1)}$$'),
                                         'cauchy' = withMathJax('$$\\mathbb{V}[X]=\\varnothing$$'),
                                         'gum' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{\\pi^2}{6}\\beta^2$$'),
                                         'logn' = withMathJax('$$\\mathbb{V}[X]=\\left(e^{\\sigma^2}-1\\right)e^{2\\mu+\\sigma^2}$$'),
                                         'khi' = withMathJax('$$\\mathbb{V}[X]=2k$$'),
                                         'logi' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{\\pi^2}{3}s^2$$'),
                                         'poisson' = withMathJax('$$\\mathbb{V}[X]={\\lambda}$$'),
                                         'geometric' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{1-p}{p^2}$$'),
                                         'hipergeometric' = withMathJax('$$n\\dfrac{M}{N}\\dfrac{N-M}{N}\\dfrac{N-n}{N-1}$$'),
                                         'bernoulli' = withMathJax('$$\\mathbb{V}[X]={p-p^2}$$'),
                                         'binomial' = withMathJax('$$\\mathbb{V}[X]={x}{(p-p^2)}$$')
  )})})
  
}

shinyApp(ui = ui, server = server)