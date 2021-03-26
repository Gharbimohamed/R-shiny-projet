library(shiny)
library(Hmisc)
library(UsingR)
library(reshape2)
library(ggpubr)
library(shinydashboard)
library(DT)
library(plyr)
library(plotly)
library(reader)
library("ElemStatLearn")
library("class")
library("plotrix")
library(CatEncoders)
library(MLmetrics)
library(ROSE)
library(smotefamily)
library(ggplot2)
library(GGally)
ui <- dashboardPage(
  dashboardHeader(
    title="FERRAH & GHARBI"
  ),
  
  dashboardSidebar(
    
    
    #chargement du fichier
    fileInput(inputId = "file1", label = "Veuillez choisir votre fichier CSV",
              accept = c("text/plain", ".csv")
    ),
    
    
    sidebarMenu(
      
      #Donnees quantitatives discrÃ¨tes:
      
      menuItem("Accueil", tabName = "accueill", icon = icon("home")),
      menuItem("Le dataset", tabName = "readData", icon = icon("table")),
      menuItem("Analyse des Donnees", tabName="datanalys", icon=icon("chart-line"),
               menuSubItem("Univariee", tabName = "quant", icon=icon("dice-one")),
               menuSubItem("Bivariee", tabName = "qual", icon=icon("dice-two")),
               menuSubItem("Qualitative VS Quantitative", tabName = "vs", icon=icon("chart-pie")),
               menuSubItem("Qualitative VS Qualitative", tabName = "vs2", icon=icon("chart-pie"))),
      
      menuItem("Churn", tabName="churn", icon=icon("recycle"),
               menuSubItem("Churn Variable", tabName = "ChurnIn", icon=icon("dice-one")),
               menuSubItem("Churn Vs Qualitatives", tabName = "ChurnQual", icon=icon("dice-one")),
               menuSubItem("Churn Vs Quantitatives", tabName = "ChurnQuant", icon=icon("dice-one")) ),
      menuItem("Modeles d'apprentissages", tabName = "app", icon=icon("recycle")),
      menuItem("Balances des Donnees", tabName="databalance", icon=icon("recycle"))
      
      
    )
  ),
  dashboardBody(
    
    tags$head(tags$style(HTML('
      .content-wrapper {
        
        background-color: #fcfffff5;
      }
      .main-sidebar{
        background-color: #595f5e !important;
      } 
      .navbar  {
        background-color: #595f5e !important;
      }
      .col-sm-8{
      width :100% !important;
      }
                              .sidebar-toggle:hover{
                              background-color: #595f5e !important;
                              }
                              .logo{
                              
                              background-color: #595f5e !important;
                              }
                              '
    ))),
    
    tabItems(
      
      tabItem(tabName = "readData",
              h3("Les données du dataset IRIS",align = "center"),
              #tableOutput(outputId = "contents")
              DT::dataTableOutput("contents")
              
      ),
      tabItem(tabName = "analydata",
              tabItem(tabName = "Something"),
              
      ),
      tabItem(tabName = "stat",
              h3("Statistiques  ",align = "center"),
              tableOutput(outputId = "statistique")
              
      ),
      tabItem(tabName = "qual",
              
              
              mainPanel(
                fluidRow(
                  column(6,uiOutput('quantlistbi1'),
                         #varSelectInput("choix", "Le choix de la variable", data(), multiple = FALSE),
                         #selectInput("choix", "Le choix de la variable",
                         #tableOutput(outputId = "input_list"),selected = 1),
                         
                  ),
                  column(6,uiOutput('quantlistbi2'),
                         #selectInput("choixx", "Le choix de la variable",
                         #           tableOutput(outputId = "input_list"),selected = 1),
                         
                  )
                  
                  
                ),
                
                tabsetPanel(
                  tabPanel("Nuage de points", 
                           fluidRow(
                             h3("Nuage de point avec la regression linéaire", align="center"),
                             column(6, plotOutput("nuagePoints")),
                             
                             column(6, textOutput("correlation"))
                             
                           )
                  ),
                  tabPanel("Histogrammes dos à dos", 
                           fluidRow(
                             column(8, offset = 1, plotOutput("histbackback"))
                           ), style="padding-left: 150px; margin-top: 10px; padding-right: 350px;"
                           
                  ),
                  tabPanel("Nuage de points et Histogrammes", 
                           fluidRow(
                             column(8, offset = 1, plotOutput("nuagePointshist"))
                           ), style="padding-left: 150px; margin-top: 10px; padding-right: 350px;"
                           
                  ),
                  tabPanel("Caractéristiques", tableOutput("caract")),
                  
                  tabPanel("Boîtes parallèles", 
                           fluidRow(
                             column(6, plotOutput("boxplotBasic")),
                             column(6, plotOutput("boxplotGgplot"))
                           ),
                           fluidRow(
                             column(4, offset = 4, textOutput("cor"))
                           )
                           
                  )
                  
                  
                )
              )),
      
      tabItem(tabName = "quant",
              tabsetPanel(
                tabPanel("Variables Quantitatives", 
                         mainPanel(uiOutput('quantlist'),
                                   #selectInput("radio", "Le choix de la variable",
                                   #            tableOutput(outputId = "input_quant"),selected = 1),
                                   
                                   
                                   tabsetPanel(
                                     tabPanel("Le SUMMARY", 
                                              fluidRow(
                                                h3("Le summary de la variable choisie", align = "center"),
                                                
                                                # Affichage d'un summary
                                                verbatimTextOutput(outputId = "summary")
                                              )
                                              
                                              
                                              
                                              
                                     ),
                                     
                                     tabPanel("Graphes + boxplot", 
                                              fluidRow(
                                                column(4, 
                                                       # Zone d'affichage du diagramme en bÃÂÃÂ¢tons des effectifs
                                                       plotOutput(outputId = "effectifsDiag")),
                                                column(4, 
                                                       # Zone d'affichage du diagramme en bÃÂÃÂ¢tons des effectifs cumulÃÂÃÂ©s
                                                       plotOutput(outputId = "effectifsCumDiag")),
                                                column(4, 
                                                       # Zone d'affichage de la boÃÂÃÂ®te ÃÂÃÂ  moustaches
                                                       plotOutput(outputId = "boiteMoustaches"))
                                              )
                                              
                                              
                                              
                                     ),
                                     
                                     tabPanel("Histogrammes et courbes ", 
                                              h3("Visualisation des graphes ", align = "center"),
                                              fluidRow(
                                                
                                                
                                                column(4,
                                                       h5("Histogramme des effectifs", align = "center"),
                                                       plotOutput(outputId = "effectifsHist")),
                                                column(4,
                                                       h5("Histogramme des densitÃ©s de frequences", align = "center"),
                                                       plotOutput(outputId = "effectifsHistFreqDens")),
                                                column(4,
                                                       h5("Courbe cumulative", align ="center"),
                                                       plotOutput(outputId = "effectifsCumCurve"))
                                                
                                              ),
                                              
                                              
                                     )
                                     
                                     
                                     
                                     
                                   )
                         )
                         
                ), 
                tabPanel("Variables Qualitatives", 
                         fluidRow(uiOutput('qualist'),
                                  tabsetPanel(
                                    tabPanel("Histogramme des effectifs",fluidPage(column(6,
                                                                                          h5("Histogramme des effectifs", align = "center"),
                                                                                          tableOutput(outputId = "statqq")),
                                                                                   column(6,
                                                                                          h5("Courbe cumulative", align ="center"),
                                                                                          plotOutput(outputId = "effectifsDiagq")))),
                                    tabPanel("Diagrammes",fluidPage(column(6,
                                                                           h5("Diagramme en colonnes", align = "center"),
                                                                           plotOutput("colonnes")),
                                                                    column(6,
                                                                           h5("Diagramme en secteurs", align ="center"),
                                                                           plotOutput("secteurs"))))
                                  ),
                                  
                                  
                                  
                         ))
              ),
              
      ),
      
      tabItem(tabName = "vs",
              h3("Quantitative VS Qualitative  ",align = "center"),
              mainPanel( 
                fluidRow(
                  column(6,uiOutput('quantlistvs'),
                         #varSelectInput("choix", "Le choix de la variable", data(), multiple = FALSE),
                         #selectInput("choix", "Le choix de la variable",
                         #tableOutput(outputId = "input_list"),selected = 1),
                         
                  ),
                  column(6,uiOutput('qualistvs'),
                         #selectInput("choixx", "Le choix de la variable",
                         #           tableOutput(outputId = "input_list"),selected = 1),
                         
                  )
                  
                  
                ),
                tabsetPanel(
                  tabPanel("Diag. Barres (1 var.)", 
                           fluidRow(
                             column(6, plotOutput("barplotUni")),
                             column(6, plotOutput("barplotOrderedUni"))
                           )
                  ),
                  tabPanel("Diag. Barres (2 var.)", 
                           fluidRow(
                             column(6, plotOutput("barplotBi")),
                             column(6, plotOutput("barplotDodgeBi"))
                           )
                  )
                  
                  
                )
                , style = "font-size: 75%")),
      tabItem(tabName = "vs2",
              h3("Qualitative VS Qualitative  ",align = "center"),
              mainPanel( 
                fluidRow(
                  column(6,uiOutput('qualistvs1'),
                         #varSelectInput("choix", "Le choix de la variable", data(), multiple = FALSE),
                         #selectInput("choix", "Le choix de la variable",
                         #tableOutput(outputId = "input_list"),selected = 1),
                         
                  ),
                  column(6,uiOutput('qualistvs2'),
                         #selectInput("choixx", "Le choix de la variable",
                         #           tableOutput(outputId = "input_list"),selected = 1),
                         
                  )
                  
                  
                ),
                tabsetPanel(
                  
                  
                  tabPanel("Diag. Profils", 
                           fluidRow(
                             column(6, plotOutput("barplotProfils")),
                             column(6, tableOutput("contingency"))
                           )
                  ),
                  tabPanel("Indices", 
                           fluidRow(
                             column(6, offset = 2, tableOutput("force"))
                           )
                  )
                  
                )
                , style = "font-size: 75%")),
      
      tabItem(tabName = "accueill",
              h3(" A propos du tp ",align = "center"),
              br(),
              br(),
              br(),
              strong("Problèmatique", style="font-size :15pt"),
              br(),
              br(),
              p("Le travail présenté dans cette application, rentre dans le cadre d’un Projet du module Data Science 2 pour les étudiants MLDS de l’université de Paris pour l’année universitaire 2020/2021.
Le travail consiste de faire une analyse uni-variée, bi-variée, le traitement des donnees, la comparaison des methodes de classification et le churn sur l’ensemble du dataset choisi .
L’application interagit d’une manière réactive à la demande de l’utilisateur.
L’utilisateur à la possibilité de l’explorer.
", style=" font-size :13pt"),
              br(),
              br(),
              strong("Données utilisées :", style="font-size :15pt"),
              p("Le Data-set utilisé dans ce travail est un data-set classique intitulé « Bank additional » qui regroupe 41189 lignes d’observations de données des clients de la banque qui sont identifiées par 21 variables (9 quantitatives et 12 qualitative).
", style=" font-size :13pt"),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              h3("Réalisé Par : FERRAH Hichem & GHARBI Mohamed",align = "center")
      ),
      
      
      
      
      #-------------------- CHURN -------------
      tabItem(tabName = "ChurnIn", 
              uiOutput('churnchoice'), plotlyOutput(outputId = "churnIntro1")),
      tabItem(tabName = "ChurnQual", 
              uiOutput('churnqual'),plotlyOutput(outputId = "churnqualplot"),
      ),
      tabItem(tabName = "ChurnQuant", 
              uiOutput('churnquant'),
              tabsetPanel(
                
                
                tabPanel("Pie Charts", 
                         sliderInput('sliderquant', "Ajuster la distribution pour ameliorer le Rendu ", 1, 5, 3,1),plotlyOutput(outputId = "churnquantplot")
                         
                         
                ),
                tabPanel("Bar plots", 
                         plotlyOutput(outputId = "churnquantplot2"))
                ,
                tabPanel("pairwise Scatter", 
                         uiOutput('pairwise1'),uiOutput('pairwise2'),
                         plotOutput(outputId = "churnquantplot3")
                         
                )
                
              ) 
              
      ),      
      
      
      
      #---------------------- Apprentissage Unbalanced ------------
      tabItem(tabName = "app",
              
              
              mainPanel(
                
                
                tabsetPanel(
                  tabPanel("KNN", 
                           fluidRow(
                             
                             h3("Apprentissage Supervise knn", align="center"),
                             column(6, plotOutput("plot1"))
                             
                           )
                  ),
                  tabPanel("LR", 
                           fluidRow(
                             h3("Apprentissage Supervise LR", align="center"),
                             column(8, offset = 1, plotOutput("plot2"), style="width: 850px !important")
                           )
                           
                  )))),
      #---------------------- data Balancing ---------------
      tabItem(tabName = "databalance",
              
              
              mainPanel(
                
                
                tabsetPanel(
                  tabPanel("Equilibre", 
                           fluidRow(
                             
                             column(12,uiOutput("varbalance"),uiOutput("sampling"),uiOutput("go"),plotlyOutput("compare") )
                             
                           )
                  ),
                  tabPanel("KNN", 
                           fluidRow(
                             
                             h3("Apprentissage Supervise knn", align="center"),
                             column(6, plotOutput("plot4"))
                             
                           )
                  ),
                  tabPanel("LR", 
                           fluidRow(
                             h3("Apprentissage Supervise LR", align="center"),
                             column(8, offset = 1, plotOutput("plot5"), style="width: 850px !important")
                           )
                           
                  ))))
      
      
    )
  )
)

server <- function(input, output) {
  options(shiny.maxRequestSize=200*1024^2)
  data <- reactive({
    # Initialement, class(input$file1) = NULL
    # AprÃÂÃÂ¨s chargement, class(input$file1) = data.frame
    # avec les colonnes 'size', 'type', and 'datapath' columns.
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    delim<-get.delim(inFile$datapath, n = 10, comment = "#", skip = 0, delims = c("\t", "\t| +", " ", ";", ","), large = 10, one.byte = TRUE)
    print(delim)
    read.csv(inFile$datapath, header = TRUE, sep=delim)
    
    # data <- readLines(con <- file)
    # records <- sapply(data, strsplit, split=separators)
    # dataFrame <- data.frame(t(sapply(records,c)))
    # rownames(dataFrame) <- 1: nrow(dataFrame)
    # return(as.data.frame(dataFrame,stringsAsFactors = FALSE))
  })
  #data <- read.csv(url("https://raw.githubusercontent.com/Ferrah-hichem/datasets/master/bank-full.csv"), header = TRUE, sep=';')
  #input_list <- reactive({)})
  # output$input_list <- renderTable(colnames(data))
  # # input_list <<- colnames(data)
  # # input_quant <<- reactive({colnames(data)[!grepl('factor|logical|character',sapply(data,class))]})
  # # input_qual <<- reactive({colnames(data)[grepl('factor|logical|character',sapply(data,class))]})
  # output$input_quant <- renderTable(colnames(data)[!grepl('factor|logical|character',sapply(data,class))])
  # output$input_qual <- renderTable(colnames(data)[grepl('factor|logical|character',sapply(data,class))])
  # 
  quant<- reactive({
    names(data())[!grepl('factor|logical|character',sapply(data(),class))]
  })
  output$list = renderUI({
    selectInput('choix', 'Le choix de la variable', names(data()))
  })
  output$list2 = renderUI({
    selectInput('choix', 'Le choix de la variable', names(data()))
  })
  output$quantlist = renderUI({
    selectInput('radio', 'Le choix de la variable', names(data())[!grepl('factor|logical|character',sapply(data(),class))])
  })
  output$quantlistbi1 = renderUI({
    selectInput('quantlistbi1', 'Le choix de la variable', names(data())[!grepl('factor|logical|character',sapply(data(),class))])
  })
  output$quantlistbi2 = renderUI({
    selectInput('quantlistbi2', 'Le choix de la variable', names(data())[!grepl('factor|logical|character',sapply(data(),class))])
  })
  output$quantlistvs = renderUI({
    selectInput('quantlistvs', 'Le choix de la variable', names(data())[!grepl('factor|logical|character',sapply(data(),class))])
  })
  
  output$qualist = renderUI({
    selectInput('choixx', 'Le choix de la variable', names(data())[grepl('factor|logical|character',sapply(data(),class))])
  })
  
  output$qualistvs = renderUI({
    selectInput('qualistvs', 'Le choix de la variable', names(data())[grepl('factor|logical|character',sapply(data(),class))])
  })
  output$qualistvs1 = renderUI({
    selectInput('qualistvs1', 'Le choix de la variable', names(data())[grepl('factor|logical|character',sapply(data(),class))])
  })
  output$qualistvs2 = renderUI({
    selectInput('qualistvs2', 'Le choix de la variable', names(data())[grepl('factor|logical|character',sapply(data(),class))])
  })
  output$churnchoice = renderUI({
    selectInput('churnchoice', 'Le choix de la variable', names(data()))
  })
  output$churnqual = renderUI({
    selectInput('churnqual', 'Choisissez une Variable pour la comparer avec la variable Churn', names(data())[grepl('factor|logical|character',sapply(data(),class))])
  })
  output$churnquant = renderUI({
    selectInput('churnquant', 'Choisissez une Variable pour la comparer avec la variable Churn', names(data())[!grepl('factor|logical|character',sapply(data(),class))])
  })
  output$varbalance = renderUI({
    selectInput("varbalance", 'Choisissez une Variable Binaire "Desiquilibre" ', names(data()))
  })
  output$pairwise1 = renderUI({
    selectInput('pairwise1', 'Choisissez une 2eme Variable', names(data())[!grepl('factor|logical|character',sapply(data(),class))])
  })
  output$pairwise2 = renderUI({
    selectInput('pairwise2', 'Choisissez une 3eme Variable', names(data())[!grepl('factor|logical|character',sapply(data(),class))])
  })
  output$go =renderUI({
    validate(
      need(nrow(unique(data()[input$varbalance])) == 2, "... En attente de la variable ...")
    )
    actionButton("go",label="valider",style="margin-left:51%; margin-top:-20%;")
  })
  output$sampling = renderUI({
    validate(
      need(nrow(unique(data()[input$varbalance])) == 2, "Cette Methode marche uniquement avec les variables binaires, veuillez choisir une autre")
    )
    
    selectInput("sampling","Choisissez La methode du Data balancing",c("Random Oversampling","Random UnderSampling","Both"))
  })
  tabStats <- reactive({
    dt = data()
    dt2 =dt[,input$radio]
    # Calculer les effectifs et les effectifs cumulÃÂ©s
    table.tmp <- as.data.frame(table(dt2))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les frÃÂ©quences et les frÃÂ©quences cumulÃÂ©s
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(data)*100,
                       table.tmp[[3]]/nrow(data)*100)
    # Ajouter des noms de colonnes
    colnames(table.tmp) <- c(dt[1,input$radio], "Effectifs", "Effectifs Cum.",
                             "Frequences", "Frequences Cum.")
    # Renvoyer le tableau statistique
    return(table.tmp)
  })
  tabStat <- reactive({
    dt = data()
    dt2 =dt[,input$choixx]
    # Calculer les effectifs et les effectifs cumulÃÂ©s
    table.tmp <- as.data.frame(table(dt2))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les frÃÂ©quences et les frÃÂ©quences cumulÃÂ©s
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(data())*100,
                       table.tmp[[3]]/nrow(data())*100)
    # Ajouter des noms de colonnes
    colnames(table.tmp) <- c(input$choixx, "Effectifs", "Effectifs Cum.",
                             "Frequences", "Frequences Cum.")
    # Renvoyer le tableau statistique
    print(dim(table.tmp[,1]))
    return(table.tmp)
  })
  
  
  # Commande pour le calcul du summary
  
  output$contents <- DT::renderDataTable({ 
    #data()
    DT::datatable(
      data(),
      filter = 'top', extensions = c('Buttons'),
      options = list(scrollY = 650,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     buttons = list('excel',
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE), 
      rownames = FALSE)
    
  })
  
  
  
  # Boîtes parallèles
  # ----
  
  output$boxplotBasic <- renderPlot({
    
    d <- data()
    d.stack <- melt(d, measure.vars = quant())
    # Boxplot basique
    d.stack$value <- as.numeric(d.stack$value)
    boxplot(d.stack$value ~ d.stack$variable , col="grey",
            xlab = "Modalités", ylab = "Mesures")
  })
  
  output$boxplotGgplot <- renderPlot({
    d <- data()
    d.stack <- melt(d, measure.vars = quant())
    d.stack$value <- as.numeric(d.stack$value)
    # Boxplot élaborée
    qplot(x = d.stack[,2], y = d.stack[,1], 
          xlab = "Modalités", ylab = "Mesures",
          geom=c("boxplot", "jitter"), fill=d.stack[,2]) +
      theme(legend.title=element_blank())
  })
  
  
  
  
  output$histbackback <- renderPlot({
    options(digits=1)
    x.var = input$quantlistbi1 ; y.var = input$quantlistbi2;
    dt = data()
    dt2 =dt[,input$quantlistbi1]
    dt2 = as.numeric(dt2)
    dt = data()
    dt3 =dt[,input$quantlistbi2]
    dt3 = as.numeric(dt3)
    histbackback(x = dt2, y = dt3,
                 xlab = c(x.var, y.var), main = paste(x.var, "and", y.var), 
                 las = 2)
  })
  output$nuagePoints <- renderPlot({
    # Simple nuage de point 
    options(scipen=999)
    x.var = input$quantlistbi1 ; y.var = input$quantlistbi2;
    plot(x = data()[, x.var], y = data()[, y.var], col = "blue",
         las = 2, cex.axis = 0.7,
         main = paste(y.var, "en fonction de", x.var),
         xlab = x.var, ylab = y.var, cex.lab = 1.2
    )
    # Droite de régression linéaire (y~x) 
    abline(lm(data()[, y.var]~data()[, x.var]), col="red", lwd = 2)
    options(scipen=0)
    
    
  })
  output$correlation <- renderText({
    dt = data()
    dt2 =dt[,input$quantlistbi1]
    dt2 = as.numeric(dt2)
    dt = data()
    dt3 =dt[,input$quantlistbi2]
    dt3 = as.numeric(dt3)
    #x.var = input$choix ; y.var = input$choixx;
    coeff.tmp <- cov(dt2, dt3)/(sqrt(var(dt2)*var(dt3)))
    paste("Coefficient de corrélation linéaire =", round(coeff.tmp,digits = 2))
  })
  output$summary <- renderPrint({
    dt = data()
    dt2 =dt[,input$radio]
    #print(dt2)
    t(summary.default(as.numeric(as.character(dt2))))
    t(summary(dt2))})
  
  output$statistique <- renderTable({ 
    tabStats() })
  output$statqq <- renderTable({ 
    tabStat() })
  # Nuage de points et histogrammes
  # ----
  output$nuagePointshist <- renderPlot({
    options(digits=1)
    dt = data()
    dt2 =dt[,input$quantlistbi1]
    dt2 = as.numeric(dt2)
    dt = data()
    dt3 =dt[,input$quantlistbi2]
    dt3 = as.numeric(dt3)
    EF = dt2; 
    CA = dt3;
    scatter.with.hist( EF, CA)
  })
  
  output$caract <- renderTable({
    # Définition des colonnes choisies 
    var.names <- quant()
    # Initialisation de la table
    caract.df <- data.frame()
    
    # Pour chaque colonne, calcul de min, max, mean et ecart-type
    for(strCol in var.names){
      caract.vect <- c(min(data()[, strCol]), max(data()[,strCol]), 
                       mean(var(data()[,strCol])), sqrt(var(data()[,strCol])))
      caract.df <- rbind.data.frame(caract.df, caract.vect)
    }
    
    # Définition des row/colnames
    rownames(caract.df) <- var.names
    colnames(caract.df) <- c("Minimum", "Maximum", "Moyenne", "Ecart-type")
    # Renvoyer la table
    caract.df
  }, rownames = TRUE, digits = 0)
  # Commande pour l'affichage du plot des effectifs
  output$effectifsDiag <- renderPlot({ 
    dt = data()
    plot(table(data.frame(dt[,input$radio])), col ="blue", xlab =dt[1,input$radio], ylab ="Effectifs", 
         main ="Distribution des effectifs")
  })
  output$effectifsDiagq <- renderPlot({ 
    dt = data()
    plot(table(data.frame(dt[,input$choixx])), col ="blue", xlab =dt[,input$choixx], ylab ="Effectifs", 
         main ="Distribution des effectifs")
  })
  effectifs <- reactive({
    
    dt = data()
    return(table(dt[,input$choixx]))
  })
  
  # Diagramme en colonnes
  output$colonnes <- renderPlot({
    barplot(effectifs(), main = " ", 
            ylab="Effectifs", las = 2,
            names.arg = substr(names(effectifs()), 1, 4))
    
  })
  
  # Diagramme en secteurs
  output$secteurs <- renderPlot({
    pie(effectifs(), labels = substr(names(effectifs()), 1, 4), 
        main = " ", col=c())
  })
  
  # Commande pour l'affichage du plot des frÃÂ©quences cumulÃÂ©es
  output$effectifsCumDiag <- renderPlot({
    dt= table(data()[,input$radio])
    dt= cumsum(dt)/nrow(data())*100
    plot(ecdf(as.numeric(as.character(dt))),
         col ="blue", xlab = "La variable" , ylab ="Frequences cumulees",
         main ="Frequences cumulees ")
  })
  # output$effectifsCumDiag <- renderPlot({ 
  #   # plot(ecdf(as.numeric(as.character(data.frame(tabStats()[,5])))), 
  #   #      col ="blue", xlab = "La variable" , ylab ="Frequences cumulees", 
  #   #      main ="Frequences cumulees ")
  #   dt = data()
  #   dt2= table(data.frame(dt[,input$radio]))/nrow(dt)*100
  #   
  #   plot(dt2, 
  #        col ="blue", xlab = "La variable" , ylab ="Frequences cumulees", 
  #        main ="Frequences cumulees ")
  # })
  
  # Commande pour l'affichage de la boÃÂ®te ÃÂ  moustaches
  output$boiteMoustaches <- renderPlot({
    # BoÃÂ®te ÃÂ  moustaches
    dt = data()
    boxplot( data.frame(as.numeric(as.character(dt[,input$radio]))), col = grey(0.8), 
             main = " ",
             ylab = "", las = 1)
    # Affichage complÃÂ©mentaires en Y des diffÃÂ©rents ÃÂ¢ges
    rug(data()[,input$radio], side = 2)
  })
  # RÃ©cupÃ©ration des valeurs fecondite
  fecondite <- reactive({
    if(!"Sepal.Length" %in% colnames(data())) return(NULL)
    data()$Sepal.Length
    
  })
  # Histogrammes
  # ----
  output$effectifsHist <- renderPlot({
    dt = data()
    
    # Histogramme des effectifs
    hist(as.numeric(as.character(dt[,input$radio])) , freq = TRUE, cex.axis = 1.5, cex.main = 1.5,
         main = "Histogramme", col = "blue",
         xlab = dt[1,input$radio], ylab = "Effectifs", las = 1,
         right = FALSE, cex.lab = 1.5)
  })
  
  output$effectifsCumCurve <- renderPlot({
    dt = data()
    # RÃ©cupÃ©ration des infos Ã  partir de l'histogramme
    tmp.hist <- hist(as.numeric(as.character(dt[,input$radio])) , plot = FALSE,
                     
                     right = FALSE)
    # Courbe cumulative (effectifs)
    plot(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts),
         xlab = dt[1,input$radio],
         ylab = "Effectifs cumulés", cex.axis = 1.5, cex.lab = 1.5,
         main = "Courbe cumulative ",
         type = "o", col = "blue", lwd = 2, cex.main = 1.5)
    
  })
  
  
  
  output$effectifsHistFreqDens <- renderPlot({
    dt = data()
    # Histogramme des densitÃ©s de frÃ©quences
    hist( as.numeric(as.character(dt[,input$radio])), freq = FALSE, cex.axis = 1.5, cex.main = 1.5,
          main = "Histogramme de la variable", col = "green",
          xlab = dt[1,input$radio] , ylab = "Densité de fréquences", las = 1,
          right = FALSE, cex.lab = 1.5)
  })
  
  
  # Force de la liaison entre 'especes' et 'couleur'
  # ----
  output$force <- renderTable({
    force.df <- as.data.frame(matrix(NA, nrow = 3, ncol = 1))
    rownames(force.df) = c("X2", "Phi2", "Cramer")
    
    # La table de contingence des profils observés
    #tab = with(data(), table(input$quantlistvs, input$qualistvs))
    var1<-input$qualistvs1
    var2<-input$qualistvs2
    data<-cbind(data()[var1],data()[var2])
    data<-as.data.frame(data)
    colnames(data)<-c("var1","var2")
    
    tab =with(data,table(var1,var2))
    print(tab)
    # La table de contigence s'il y a indépendence
    tab.indep = tab
    n = sum(tab)
    tab.rowSum = apply(tab, 2, sum)
    tab.colSum = apply(tab, 1, sum)
    print(tab.colSum)
    print(tab.rowSum)
    print(c)
    for(i in c(1:length(tab.colSum))){
      for(j in c(1:length(tab.rowSum))){
        tab.indep[i,j] = tab.colSum[i]*tab.rowSum[j]/n
      }
    }
    
    # Calcul du X²
    force.df[1,1] = sum((tab-tab.indep)^2/tab.indep)
    # Calcul du Phi²
    force.df[2,1] = force.df[1,1]/n
    # Calcul du Cramer
    force.df[3,1] = sqrt(force.df[2,1]/(min(nrow(tab), ncol(tab))-1))
    
    force.df
    
  }, rownames=TRUE, colnames=FALSE)
  
  # Unidimensionnel
  output$barplotUni <- renderPlot({
    # Diagramme en barres de la variable 'Level' avec ggplot
    
    # list<-as.list(data()[,input$quantlistvs])
    # print(list)
    # ggplot(data(), aes(x = paste(data))) + geom_bar(stat="identity")
    var<-sym(input$quantlistvs)
    ggplot(data(),aes(x = !!var) )+ geom_bar()
  })
  
  
  
  output$barplotProfils <- renderPlot({
    # Diagramme de profils entre les variables 'V2' et 'V6'
    var1<-sym(input$qualistvs1)
    var2<-sym(input$qualistvs2)
    ggplot(data(), aes(x = !!var2, fill = !!var1)) + geom_bar(position = "fill")
  })
  
  # Bidimensionnel
  output$barplotBi <- renderPlot({
    # Diagramme en barres entre les variables 'V2' et 'V6'
    var1<-sym(input$quantlistvs)
    var2<-sym(input$qualistvs)
    ggplot(data(), aes(x = !!var1, fill = !!var2)) + geom_bar()
  })
  
  
  # output$barplotProfils <- renderPlot({
  #   var1<-sym(input$quantlistvs)
  #   var2<-sym(input$qualistvs)
  #   ggplot(data(), aes(x = !!var2, fill = !!var1)) + geom_bar(position = "fill")
  # })
  # 
  # 
  # Table de contingence entre 'Sex' et 'Level'
  # ----
  output$contingency <- renderTable({
    var1<-input$qualistvs1
    var2<-input$qualistvs2
    data<-cbind(data()[var1],data()[var2])
    data<-as.data.frame(data)
    colnames(data)<-c("var1","var2")
    tab = with(data,table(var1, var2))
    # print(tab)
    # tab2 = with(data(), table(y, default))
    # print(tab2)
    # 
    print(tab)
    round(tab/sum(tab), 3)
    tab
    print(tab)
  })
  #------------------ CHURN PART : ---------------
  churnvar <- reactive({
    validate(
      need(nrow(unique(data()[input$churnchoice])) == 2, "Cette Variable ne peut pas etre une Varibale Churn Veuillez choisir une autre (Variable Binaire).")
    )
    return(data()[input$churnchoice])
  })
  output$churnIntro1 <- renderPlotly({
    var<-as.data.frame(churnvar())
    # # pie(var$freq, labels = unique(data()$y), main = "Churn Variable", col=c("#ffa600","#bc5090"))
    # bp<- ggplot(df, aes(x="", y=value, fill=group))+
    #   geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
    freq<- count(var) 
    print(freq)
    freq<- cbind(freq,round(freq$freq/ nrow(var)*100,2))
    colnames(freq) <- c("value","occu","percent")
    print(freq)
    
    # Compute the cumulative percentages (top of each rectangle)
    freq$ymax <- cumsum(freq$percent)
    
    # Compute the bottom of each rectangle
    freq$ymin <- c(0, head(freq$ymax, n=-1))
    
    # Compute label position
    freq$labelPosition <- (freq$ymax + freq$ymin) / 2
    
    # Compute a good label
    freq$label <- paste0(freq$percent,"%")
    print(freq)
    # Make the plot
    
    plot<-plot_ly(freq,
                  labels = ~value,
                  values = ~percent, type = 'pie',
                  text="",
                  textinfo = "",hoverinfo="text",hovertext=paste(freq$value," ",freq$occu)) %>%
      layout(title = paste("Variable Churn"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             showlegend = TRUE)
    return(plot)
  })
  
  slidervalues<- reactive({
    values<-data()[input$churnchoice]
    max<-max(values)*0.5
    min<-min(values)*0.05
    
    
  })
  output$churnqualplot  <- renderPlotly({
    print(nrow(count(names(data())[grepl('factor|logical|character',sapply(data(),class))])))
    validate(
      need(nrow(unique(data()[input$churnchoice])) == 2, "Veuillez Choisir une variable Churn dans l'onglet 'Churn Variable' "),
      need(nrow(count(names(data())[grepl('factor|logical|character',sapply(data(),class))])) != 0, "Aucune Variable qualitative Detecte")
    )
    filtervar1 <- data()
    filtervar2 <- data()
    var <- input$churnchoice
    values<-unique(data()[var])
    print(values)
    print(values[1,1])
    print(values[2,1])
    valeurchurn1 <- values[1,1]
    valeurchurn2 <- values[2,1]
    
    filtervar1 <- data()[data()[var] == valeurchurn1, input$churnqual]
    filtervar2 <- data()[data()[var] == valeurchurn2, input$churnqual]
    
    
    
    var1<-as.data.frame(filtervar1)
    freq<- count(var1) 
    print(freq)
    freq<- cbind(freq,round(freq$freq/ nrow(var1)*100,2))
    colnames(freq) <- c("value","occu","percent")
    print(freq)
    
    # Compute the cumulative percentages (top of each rectangle)
    freq$ymax <- cumsum(freq$percent)
    
    # Compute the bottom of each rectangle
    freq$ymin <- c(0, head(freq$ymax, n=-1))
    
    # Compute label position
    freq$labelPosition <- (freq$ymax + freq$ymin) / 2
    
    # Compute a good label
    freq$label <- paste0(freq$percent,"%")
    print(freq)
    # Make the plot
    var2<-as.data.frame(filtervar2)
    freq2<- count(var2) 
    print("freq2")
    print(freq2)
    freq2<- cbind(freq2,round(freq2$freq/ nrow(var2)*100,2))
    colnames(freq2) <- c("value","occu","percent")
    print(freq2)
    
    # Compute the cumulative percentages (top of each rectangle)
    freq2$ymax <- cumsum(freq2$percent)
    
    # Compute the bottom of each rectangle
    freq2$ymin <- c(0, head(freq2$ymax, n=-1))
    
    # Compute label position
    freq2$labelPosition <- (freq2$ymax + freq2$ymin) / 2
    
    # Compute a good label
    freq2$label <- paste0(freq2$percent,"%")
    print(freq2)
    # Make the plot
    
    print(values[1,1])
    print(values[2,1])
    plot <- plot_ly(labels = ~value, values = ~percent,textinfo = 'value+percent') %>%
      add_pie(data = freq, name = values[1,1], domain = list(row = 0, column = 0))%>%
      add_pie(data = freq2, name = values[2,1], domain = list(row = 0, column = 1))%>%
      layout(title = paste("Distribution de",input$churnqual,"par rapport a ",input$churnchoice), showlegend = T,
             grid=list(rows=1, columns=2),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             annotations = list(x = c(0, .5),
                                y = c(0, 0),text = c(paste("Distribution \n % Churn = ", values[1,1]),paste("Distribution \n % Churn = ", values[2,1])),
                                xref = "papper",
                                yref = "papper",
                                showarrow = F
             ))
    
    return(plot)
  })
  output$plot1 <- renderPlot({
    
    
    df<- data.frame(lapply(data(), function(x) {
      if(!is.numeric(x)) as.numeric(factor(x)) else x
    }))
    df$y <- ifelse(df$y==1,0,1)
    samples <- sample(1:nrow(df), 0.7*nrow(df))
    trainclass <- df[samples, ]$y
    testclass <- df[-samples, ]$y
    train = df[samples, 1:(ncol(df)-1)]
    test = df[-samples, 1:(ncol(df)-1)]
    kmax <- 10
    err_valid <- rep(NA,kmax)
    for (k in 1:kmax) 
    { 
      print(k)
      samples <- sample(1:nrow(df), 0.7*nrow(df))
      pred <- knn(train,test,trainclass,k=18) 
      err_valid[k] <- F1_Score(y_pred=pred,y_true=testclass)
    } 
    #plot(x=seq(1,kmax,by=1),y=err_test,type="o",col="blue")
    boxplot(err_valid)
    
    
  })
  output$plot2 <- renderPlot({
    df <- data()
    df$y <- ifelse(df$y=="no",0,1)
    kmax <- 10
    err_valid <- rep(NA,kmax) 
    for (k in 1:kmax) 
    { 
      samples <- sample(1:nrow(df), 0.7*nrow(df))
      test_y <- df[-samples, ncol(df)]
      test_X <- df[-samples, 1:(ncol(df)-1)]
      print(k)
      train <- df[samples, ]
      model <- glm(y ~ .,data = train)
      pred <- predict(model,test_X,type="response")
      pred <- ifelse(pred>0.5,1,0)
      err_valid[k] <- F1_Score(y_pred=pred,y_true=test_y)
      
    } 
    
    boxplot(err_valid)
    
  })
  output$churnquantplot  <- renderPlotly({
    print(nrow(count(names(data())[!grepl('factor|logical|character',sapply(data(),class))])))
    validate(
      need(nrow(unique(data()[input$churnchoice])) == 2, "Veuillez Choisir une variable Churn dans l'onglet 'Churn Variable' "),
      need(nrow(count(names(data())[!grepl('factor|logical|character',sapply(data(),class))])) != 0, "Aucune Variable qualitative Detecte")
    )
    filtervar1 <- data()
    filtervar2 <- data()
    var <- input$churnchoice
    values<-unique(data()[var])
    print(values)
    print(values[1,1])
    print(values[2,1])
    valeurchurn1 <- values[1,1]
    valeurchurn2 <- values[2,1]
    
    filtervar1 <- data()[data()[var] == valeurchurn1, input$churnquant]
    filtervar2 <- data()[data()[var] == valeurchurn2, input$churnquant]
    maxvar1<-max(filtervar1)
    maxvar2<-max(filtervar2)
    minvar1<-min(filtervar1)
    minvar2<-min(filtervar2)
    
    stepvar1<-abs((1/(2**(input$sliderquant-1)))*maxvar1)
    stepvar2<-abs((1/(2**(input$sliderquant-1)))*maxvar2)
    length<-2**(input$sliderquant)
    print(input$sliderquant)
    print(stepvar1)
    print(stepvar2)
    filtervar1<-cut(filtervar1, seq(minvar1,maxvar1,length.out = length))
    filtervar2<-cut(filtervar2, seq(minvar2,maxvar2,length.out = length))
    
    
    var1<-as.data.frame(filtervar1)
    freq<- count(var1) 
    print(freq)
    freq<- cbind(freq,round(freq$freq/ nrow(var1)*100,2))
    colnames(freq) <- c("value","occu","percent")
    print(freq)
    
    # Compute the cumulative percentages (top of each rectangle)
    freq$ymax <- cumsum(freq$percent)
    
    # Compute the bottom of each rectangle
    freq$ymin <- c(0, head(freq$ymax, n=-1))
    
    # Compute label position
    freq$labelPosition <- (freq$ymax + freq$ymin) / 2
    
    # Compute a good label
    freq$label <- paste0(freq$percent,"%")
    print(freq)
    # Make the plot
    var2<-as.data.frame(filtervar2)
    freq2<- count(var2) 
    print("freq2")
    print(freq2)
    freq2<- cbind(freq2,round(freq2$freq/ nrow(var2)*100,2))
    colnames(freq2) <- c("value","occu","percent")
    print(freq2)
    
    # Compute the cumulative percentages (top of each rectangle)
    freq2$ymax <- cumsum(freq2$percent)
    
    # Compute the bottom of each rectangle
    freq2$ymin <- c(0, head(freq2$ymax, n=-1))
    
    # Compute label position
    freq2$labelPosition <- (freq2$ymax + freq2$ymin) / 2
    
    # Compute a good label
    freq2$label <- paste0(freq2$percent,"%")
    print(freq2)
    # Make the plot
    
    print(values[1,1])
    print(values[2,1])
    plot <- plot_ly(labels = ~value, values = ~percent,textinfo = 'value+percent') %>%
      add_pie(data = freq, name = values[1,1], domain = list(row = 0, column = 0))%>%
      add_pie(data = freq2, name = values[2,1], domain = list(row = 0, column = 1))%>%
      layout(title = paste("Distribution de",input$churnquant,"par rapport a ",input$churnchoice), showlegend = T,
             grid=list(rows=1, columns=2),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             annotations = list(x = c(0, .5),
                                y = c(0, 0),text = c(paste("Distribution \n % Churn = ", values[1,1]),paste("Distribution \n % Churn = ", values[2,1])),
                                xref = "papper",
                                yref = "papper",
                                showarrow = F
             ))
    
    return(plot)
  })
  
  
  output$churnquantplot2  <- renderPlotly({
    print(nrow(count(names(data())[!grepl('factor|logical|character',sapply(data(),class))])))
    validate(
      need(nrow(unique(data()[input$churnchoice])) == 2, "Veuillez Choisir une variable Churn dans l'onglet 'Churn Variable' "),
      need(nrow(count(names(data())[!grepl('factor|logical|character',sapply(data(),class))])) != 0, "Aucune Variable qualitative Detecte")
    )
    filtervar1 <- data()
    filtervar2 <- data()
    var <- input$churnchoice
    values<-unique(data()[var])
    print(values)
    print(values[1,1])
    print(values[2,1])
    valeurchurn1 <- values[1,1]
    valeurchurn2 <- values[2,1]
    
    filtervar1 <- data()[data()[var] == valeurchurn1, input$churnquant]
    filtervar2 <- data()[data()[var] == valeurchurn2, input$churnquant]
    
    
    var1<-as.data.frame(filtervar1)
    freq<- count(var1) 
    print(freq)
    freq<- cbind(freq,round(freq$freq/ nrow(var1)*100,2))
    colnames(freq) <- c("value","occu","percent")
    print(freq)
    
    # Compute the cumulative percentages (top of each rectangle)
    freq$ymax <- cumsum(freq$percent)
    
    # Compute the bottom of each rectangle
    freq$ymin <- c(0, head(freq$ymax, n=-1))
    
    # Compute label position
    freq$labelPosition <- (freq$ymax + freq$ymin) / 2
    
    # Compute a good label
    freq$label <- paste0(freq$percent,"%")
    print(freq)
    # Make the plot
    var2<-as.data.frame(filtervar2)
    freq2<- count(var2) 
    print("freq2")
    print(freq2)
    freq2<- cbind(freq2,round(freq2$freq/ nrow(var2)*100,2))
    colnames(freq2) <- c("value","occu","percent")
    print(freq2)
    
    # Compute the cumulative percentages (top of each rectangle)
    freq2$ymax <- cumsum(freq2$percent)
    
    # Compute the bottom of each rectangle
    freq2$ymin <- c(0, head(freq2$ymax, n=-1))
    
    # Compute label position
    freq2$labelPosition <- (freq2$ymax + freq2$ymin) / 2
    
    # Compute a good label
    freq2$label <- paste0(freq2$percent,"%")
    print(freq2)
    # Make the plot
    
    print(values[1,1])
    print(values[2,1])
    tab<-cbind(freq$value,freq$occu,freq2$occu)
    tab<- as.data.frame(tab)
    colnames(tab)<-c("value","occu1","occu2")
    colnames(tab)
    fig <- plot_ly(tab, x = ~value, y = ~occu1, type = 'bar', name = valeurchurn1)
    fig <- fig %>% add_trace(y = ~occu2, name = valeurchurn2)
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    return(fig)
  })
  
  
  output$churnquantplot3 <- renderPlot({
    print(nrow(count(names(data())[!grepl('factor|logical|character',sapply(data(),class))])))
    validate(
      need(nrow(unique(data()[input$churnchoice])) == 2, "Veuillez Choisir une variable Churn dans l'onglet 'Churn Variable' "),
      need(nrow(count(names(data())[!grepl('factor|logical|character',sapply(data(),class))])) != 0, "Aucune Variable qualitative Detecte")
    )
    
    tab<-cbind(data()[input$churnchoice],data()[input$churnquant],data()[input$pairwise1],data()[input$pairwise2])
    tab<-as.data.frame(tab)
    colnames(tab)<-c("var0","var1","var2","var3")
    colnames(tab)
    
    print(tab)
    my_cols <- c("#000FF", "#FF0000")  
    print(tab[,2:4])
    fig <- ggpairs(tab, columns = 2:4,olumnLabels = c(input$churnquant,input$pairwise1,input$pairwise2),ggplot2::aes(colour=var0))
    return(fig)
  })
  
  #------------------ Balanced Data -----------------
  samplevalue<- reactive({
    validate(
      need(nrow(unique(data()[input$varbalance])) == 2, "... En attente de la variable ...")
    )
    method<-input$sampling
    choice<-input$varbalance
    df<- data.frame(lapply(data(), function(x) {
      if(!is.numeric(x)) as.numeric(factor(x)) else x
    }))
    df[,input$varbalance] <- ifelse(df[,input$varbalance]==1,0,1)
    samples <- sample(1:nrow(df), 0.7*nrow(df))
    train = df[samples, 1:(ncol(df))]
    
    data<-train
    
    values<-unique(data[choice])
    
    print(values)
    print(values[1,1])
    print(values[2,1])
    valeurchoice1 <- values[1,1]
    valeurchoice2 <- values[2,1]
    print(valeurchoice1)
    print("----------------------------------")
    print(method)
    var1<-as.data.frame(data[data[choice] == valeurchoice1,choice])
    var2<-as.data.frame(data[data[choice] == valeurchoice2,choice])
    print(count(var1))
    if(method=="Random Oversampling"){
      
      n_samp<-max(count(var1)$freq,count(var2)$freq)
      print("n_samp")
      print(n_samp)
      return(n_samp)
    }
    if(method=="Random UnderSampling"){
      
      n_samp<-min(count(var1)$freq,count(var2)$freq)
      print("n_samp")
      print(n_samp)
      return(n_samp)
    }
    if(method=="Both"){
      n_samp<-max(count(var1)$freq,count(var2)$freq)
      print("n_samp")
      print(n_samp)
      return(n_samp)
    }
    # if(method=="Smote"){
    #   n_samp<-max(count(var1)$freq,count(var2)$freq)
    #   n_samp<-cbind(n_samp,min(count(var1)$freq,count(var2)$freq))
    #   print(method)
    #   return(n_samp)
    # }
    
  })
  
  balanceddata<- eventReactive(input$go,{
    print("worked!")
    df<- data.frame(lapply(data(), function(x) {
      if(!is.numeric(x)) as.numeric(factor(x)) else x
    }))
    df[,input$varbalance] <- ifelse(df[,input$varbalance]==1,0,1)
    samples <- sample(1:nrow(df), 0.7*nrow(df))
    train = df[samples, 1:(ncol(df))]
    
    data<-train
    print(data)
    method<-input$sampling
    choice<-input$varbalance
    n_samp<-samplevalue()*2
    f<-paste(choice," ~ ",paste(names(data()[names(data())!=choice]),collapse=" + "))
    print(paste("f = ",f))
    print(paste("f as formula = ",f))
    f<-as.formula(f)
    curr_frame <<- sys.nframe()
    if(method=="Random Oversampling"){
      over_res<-ovun.sample(get("f", sys.frame(curr_frame)), data=get("data", sys.frame(curr_frame)), method="over", N=get("n_samp", sys.frame(curr_frame)), seed=2021)$data
      print(over_res)
      return(over_res)
    }
    if(method=="Random UnderSampling"){
      under_res<-ovun.sample(get("f", sys.frame(curr_frame)), data=get("data", sys.frame(curr_frame)), method="under", N=get("n_samp", sys.frame(curr_frame)), seed=2021)$data
      print(under_res)
      return(under_res)
    }
    if(method=="Both"){
      both_res<-ovun.sample(get("f", sys.frame(curr_frame)), data=get("data", sys.frame(curr_frame)), method="both", N=get("n_samp", sys.frame(curr_frame)),p=0.5, seed=2021)$data
      print(both_res)
      return(both_res)
    }
    # if(method=="Smote"){
    #   n1<-n_samp[0]
    #   n0<-n_samp[1]
    #   r0<-0.6
    #   ntimes<-(( 1 - r0) / r0 ) * ( n0 / n1 ) - 1
    #   data[input$varbalance]<-factor(data[input$varbalance])
    #   print(names(data[names(data)!=input$varbalance]))
    #   smote_res<-SMOTE(X = data[names(data)!=input$varbalance], target = data[input$varbalance], K = 5, dup_size = ntimes)$data
    #   print(smote_res)
    #   return(smote_res)}
  })
  output$compare<-renderPlotly({
    print("I'm here -1 ")
    
    print("I'm here 0 ")
    var <- input$varbalance
    filtervar1 <- data()[var] 
    filtervar2 <- balanceddata()[var] 
    print("I'm here 2 ")
    
    var1<-as.data.frame(filtervar1)
    freq<- count(var1) 
    print(freq)
    freq<- cbind(freq,round(freq$freq/ nrow(var1)*100,2))
    colnames(freq) <- c("value","occu","percent")
    print(freq)
    print("I'm here 3 ")
    # Compute the cumulative percentages (top of each rectangle)
    freq$ymax <- cumsum(freq$percent)
    
    # Compute the bottom of each rectangle
    freq$ymin <- c(0, head(freq$ymax, n=-1))
    
    # Compute label position
    freq$labelPosition <- (freq$ymax + freq$ymin) / 2
    
    # Compute a good label
    freq$label <- paste0(freq$percent,"%")
    print(freq)
    # Make the plot
    var2<-as.data.frame(filtervar2)
    freq2<- count(var2) 
    print("freq2")
    print(freq2)
    freq2<- cbind(freq2,round(freq2$freq/ nrow(var2)*100,2))
    colnames(freq2) <- c("value","occu","percent")
    print(freq2)
    
    # Compute the cumulative percentages (top of each rectangle)
    freq2$ymax <- cumsum(freq2$percent)
    
    # Compute the bottom of each rectangle
    freq2$ymin <- c(0, head(freq2$ymax, n=-1))
    
    # Compute label position
    freq2$labelPosition <- (freq2$ymax + freq2$ymin) / 2
    
    # Compute a good label
    freq2$label <- paste0(freq2$percent,"%")
    print(freq2)
    # Make the plot
    plot <- plot_ly(labels = ~value, values = ~percent,textinfo = 'occu',hovertext=~occu) %>%
      add_pie(data = freq, name = 'Avant', domain = list(row = 0, column = 0))%>%
      add_pie(data = freq2, name = 'Apres', domain = list(row = 0, column = 1))%>%
      layout(title = paste("Comparaisons des donnees avant et apres le sampling "), showlegend = T,
             grid=list(rows=1, columns=2),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             annotations = list(x = c(0, .5),
                                y = c(0, 0),text = c("Avant","Apres"),
                                xref = "papper",
                                yref = "papper",
                                showarrow = F
             ))
    
    return(plot)
    
  })
  output$balancedplot <- renderPlotly({
    
    plot_ly(data = balanceddata(), x = balanceddata()[,1], y = balanceddata()[,3],color = balanceddata()[input$varbalance])
    
  })
  # KNN ET LR APRES L'EQUILIBRE 
  output$plot4 <- renderPlot({
    
    
    df<- data.frame(lapply(balanceddata(), function(x) {
      if(!is.numeric(x)) as.numeric(factor(x)) else x
    }))
    print(df)
    df$y <- ifelse(df$y==1,0,1)
    samples <- sample(1:nrow(df), 0.7*nrow(df))
    trainclass <- df[samples, ]$y
    testclass <- df[-samples, ]$y
    train = df[samples, 1:(ncol(df)-1)]
    test = df[-samples, 1:(ncol(df)-1)]
    kmax <- 10
    err_valid <- rep(NA,kmax)
    for (k in 1:kmax) 
    { 
      print(k)
      samples <- sample(1:nrow(df), 0.7*nrow(df))
      pred <- knn(train,test,trainclass,k=18) 
      err_valid[k] <- F1_Score(y_pred=pred,y_true=testclass)
    } 
    #plot(x=seq(1,kmax,by=1),y=err_test,type="o",col="blue")
    boxplot(err_valid)
    
    
  })
  output$plot5 <- renderPlot({
    df <- data()
    df$y <- ifelse(df$y=="no",0,1)
    kmax <- 10
    err_valid <- rep(NA,kmax) 
    for (k in 1:kmax) 
    { 
      samples <- sample(1:nrow(df), 0.7*nrow(df))
      test_y <- df[-samples, ncol(df)]
      test_X <- df[-samples, 1:(ncol(df)-1)]
      print(k)
      train <- df[samples, ]
      model <- glm(y ~ .,data = train)
      pred <- predict(model,test_X,type="response")
      pred <- ifelse(pred>0.5,1,0)
      err_valid[k] <- F1_Score(y_pred=pred,y_true=test_y)
      
    } 
    
    boxplot(err_valid)
    
  })
  
}
shinyApp(ui, server)