options(scipen=999)

ui <- dashboardPage(
  dashboardHeader(
    # Ligne d'entête
    title = "Taxe d'habitation",
    tags$li(
      class = "dropdown",
      actionLink(inputId = "pourquoi", label = "Pourquoi cette application ?"))
  ),
  
  dashboardSidebar(
    collapsed = FALSE,
    # Menu de gauche
    sidebarMenu(
      id = "tabs",
      menuItem("Vos informations", tabName = "home", icon = icon("user-circle")),
      menuItem("Résultat", tabName = "resultat", icon = icon("tasks")),
      menuItem("Abattements", tabName = "abattements", icon = icon("sort-amount-desc")),
      menuItem("Détail", tabName = "detail", icon = icon("table")),
      menuItem("Réforme 2018", tabName = "reforme2018", icon = icon("table"))),
    # Liens en bas à gauche
    div(
      class = "credits",
      tags$i("Module développé par Marion"),
      tags$br(),
      tags$a(href = "https://github.com/marion-paclot/TH/", "Voir le code source"),
      tags$br(),
      tags$a(href = "https://github.com/marion-paclot/TH/issues", "Signaler une erreur ou proposer une amélioration"))
  ),
  
  dashboardBody(
    
    tags$head(
      # Lien vers le fichier CSS de l'application
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  
    tabItems(
    
      # Onglet principal de saisie des informations ============================
      tabItem(
        tabName = "home",
        mainPanel(
          width = 12,
          div(
            class = "title",
            uiOutput('introduction')
            ),
          hr(),
          h4('MES INFORMATIONS'),
          h5("Vous pourrez modifier les paramètres plus tard"),
          wellPanel(
            fluidRow(
              column(
                width = 6,
                h4('Ma situation'),
                checkboxGroupInput(
                  "alloc",
                  "Bénéficiaire de ces allocations",
                  choices = c("ASPA", "ASI", "AAH"),
                  inline = FALSE
                ),
                groupTooltip(id = "alloc", choice = "AAH", title = tooltipAah),
                groupTooltip(id = "alloc", choice = "ASPA", title = tooltipAspa),
                groupTooltip(id = "alloc", choice = "ASI", title = tooltipAsi),
                
                checkboxGroupInput(
                  "situation",
                  "Situation personnelle au 1er janvier 2017",
                  choices = c("Veuf", "Senior", "Handicapé", "Indigent"),
                  inline = FALSE
                ),
                groupTooltip(id = "situation", choice = "Veuf", title = tooltipVeuf),
                groupTooltip(id = "situation", choice = "Senior", title = tooltipSenior),
                groupTooltip(id = "situation", choice = "Handicapé", title = tooltipHandicape),
                groupTooltip(id = "situation", choice = "Indigent", title = tooltipIndigent),
                
                fluidRow(
                  column(
                    width = 6,
                    numericInput(
                      "rfr",
                      label = "Revenu fiscal de référence",
                      value = 0,
                      min = 0,
                      step = 1
                    ),
                    myTooltip(id = "rfr", title = tooltipRfr),
                    
                    div(
                      style = "margin-top: -12px; margin-bottom: 15px;",
                      actionLink(inputId = "rfrAbsent", label = "RFR à blanc sur mon avis")
                    )
                  ),
                  column(
                    width = 6,
                    div(
                      style = "margin-top: 30px;",
                      checkboxInput("isf", "ISF", value = FALSE),
                      myTooltip(id = "isf", title = tooltipIsf)
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    numericInput(
                      "nbParts",
                      "Nombre de parts fiscales",
                      1,
                      min = 1,
                      max = 99,
                      step = 0.25)
                    ),
                  myTooltip(id = "nbParts", title = "Nombre de parts fiscales du ménage"),
                  
                  column(
                    width = 6,
                    numericInput(
                      "nbPAC",
                      "Nombre de personnes à charge",
                      0,
                      min = 0,
                      max = 99,
                      step = 0.5),
                    myTooltip(id = "nbPAC", title = tooltipPac)
                    )
                )
              ),
              
              column(
                width = 6,
                h4('Mon habitation'),
                fluidRow(
                  column(
                    width = 5,
                    selectizeInput(
                      "nomDepartement",
                      "Département",
                      choices = unique(rei$LIBDEP),
                      selected = rei$LIBDEP[1],
                      multiple = FALSE,
                      options = NULL
                    )),
                  column(
                    width = 7,
                    selectizeInput(
                      "nomCommune",
                      "Commune",
                      choices = "",
                      selected = "",
                      multiple = FALSE,
                      options = NULL
                    ))
                ),
                numericInput(
                  "vlBrute",
                  "Valeur locative brute",
                  value = 0,
                  min = 1,
                  step = 1
                ),
                myTooltip(id = "vlBrute", title = tooltipVlb),
                
                radioButtons(
                  "residence",
                  "Résidence",
                  c(
                    "Principale" = "principale",
                    "Secondaire" = "secondaire",
                    "Dépendance résidence principale" = "dépendance princ",
                    "Logement vacant" = "vacant"
                    ),
                  inline = FALSE
                  ),
                # groupTooltip(id = "residence", choice = "vacant", title = tooltipVacant),

                useShinyjs(),
                numericInput(
                  "tauxMajRsCommune",
                  "Majoration résidence secondaire (en %)",
                  0,
                  min = 0,
                  step = 1
                ),
                myTooltip(id = "tauxMajRsCommune", title = tooltipMajRs)
              )
            ),
            br(),
            div(
              style = "text-align: center",
              actionButton("lancement", "Lancer le simulateur")
            )
          )
        ),
        # Permet de prolonger la toile de fond jusqu'en bas
        div(style = "clear: both")
      ),
    
      # Résultat majeur ========================================================
      tabItem(
        tabName = "resultat",
        mainPanel(
          width = 12,
          
          fluidRow(
            column(width = 6,
                   uiOutput("totalBox")
                   ),
            column(width = 6,
              tags$div(class="alert alert-info", uiOutput("explicationAssujettissement")),
              conditionalPanel('output.montantRameneA0',
                               tags$div(class="alert alert-info", uiOutput('montantAnnule'))
                       )
              )
            ),
          fluidRow(
            conditionalPanel(
              condition = "output.beneficiairePlafond && input.vlBrute > 0",
              tags$div(class="alert alert-info", uiOutput('warningPlafond'))
            )
          ),  
            
          fluidRow(
            conditionalPanel(
                   condition = "output.assujetti && input.vlBrute == 0",
                   uiOutput("vlBruteNulle_resultat")
                   )
            ),
          fluidRow(
            conditionalPanel(
                   condition = "input.vlBrute > 0",
                   tags$div(style = "text-align: center", actionButton("detail", "Accéder au détail du calcul"))
                 )
          )
        ),
        div(style = "clear: both")
      ),
      
      # Abattements ============================================================
      tabItem(
        tabName = "abattements",
        mainPanel(
          width = 12,
          # Explication des abattements
          conditionalPanel(
            condition = "input.residence == 'principale'", 
            tags$div(class="alert alert-info", uiOutput("vlNette")),
            tags$div(class="alert alert-info", uiOutput("exoPartielle"))
            ),
          # Cas des habitations hors résidences principales
          conditionalPanel(
            condition = "input.residence != 'principale'", 
            tags$div(class="alert alert-info", textOutput("pas_d_abattement"))),
          # Si la valeur locative n'est pas renseignée
          conditionalPanel(
            condition = "input.vlBrute == 0", 
            fluidRow(uiOutput("vlBruteNulle_abattements"))),
          # Dans le cas où la valeur locative est renseignée
          conditionalPanel(
            condition = "input.vlBrute > 0",
            div(
              h4("Base locatives nettes retenues"),
              dataTableOutput("calcul_baseNette"),
              hr())),
          conditionalPanel(
            condition = "input.residence == 'principale' && input.vlBrute > 0",
            div(
              radioButtons("ab_gph", "Calcul de la base locative nette", choices = "", inline = TRUE),
              dataTableOutput("abattements"),
              br(),
              plotlyOutput("cascadeAbattements"))),
          groupTooltip(id = "ab_gph", choice = "GEMAPI", title = tooltipGemapi),
          groupTooltip(id = "ab_gph", choice = "TSE", title = tooltipTse)
        ),
        # Permet de prolonger la toile de fond jusqu'en bas
        div(style = "clear: both")
      ),
    

      # Boites dynamiques détaillant le calcul =================================
      tabItem(
        tabName = "detail",
        mainPanel(
          width = 12,
          # Boites de décomposition
          fluidRow(
            div(class = "col-container",
                     div(class="col-md-5ths col-xs-6", uiOutput("cotisationsBox")),
                     div(class="col-md-5ths col-xs-6", uiOutput("fraisBox")),
                     div(class="col-md-5ths col-xs-6", uiOutput("baseEleveeBox")),
                     div(class="col-md-5ths col-xs-6", uiOutput("residenceSecondaireBox")),
                     div(class="col-md-5ths col-xs-6", uiOutput("plafonnementBox"))
                )
            ),
          
          fluidRow( 
                   # Titre
                   uiOutput("titreCalcul"),
                   # Tableau et explications affichés selon la boite sélectionnée
                   
                   tags$div(class = "alert alert-info", uiOutput("calculExplication")),
                   conditionalPanel(condition = "output.beneficiairePlafond & output.boxActiveBaseElevee & output.degrevementBaseElevee", 
                                    tags$div(class="alert alert-info", uiOutput("exonerationBaseElevee"))
                                    ),
                   # Tableau avec le détail
                   conditionalPanel(condition = "output.boxInactivePlafonnement",
                                    dataTableOutput("calculDetaille")
                                    ),
                   conditionalPanel(condition = "output.boxActivePlafonnement & output.eligiblePlafond",
                                    column(width = 6, plotOutput("graphPlafond")),
                                    column(width = 6, tags$div(class="alert alert-info", uiOutput("explicationPlafonnement")))
                                    )
                   ),
          fluidRow(
            conditionalPanel(condition = "output.boxActivePlafonnement & output.eligiblePlafond",
                             tags$div(class="alert alert-info", uiOutput("calculPlafonnement")),
                             tags$div(class="alert alert-info", uiOutput("resultatPlafonnement"))
                             )
                  
                  )
            
        ),
        div(style = "clear: both")
      ),
      
      # Réforme 2018 ===========================================================
      tabItem(
        tabName = "reforme2018",
        mainPanel(
          width = 12,
          tags$div(
            class = "alert alert-info", 
            uiOutput("explicationReforme2018")),
          conditionalPanel(condition = "input.residence == 'principale'",
                           fluidRow(
                            column(width = 4, uiOutput("degrevement2018")),
                            column(width = 4, uiOutput("tauxDegrevement2018")),
                            column(width = 4, uiOutput("montant2018"))
                            ),
                          h3("Taux de dégrèvement accordé en fonction du revenu fiscal de référence"),
                          h5("Cette courbe dépend du nombre de parts fiscales."),
                          plotlyOutput("courbeDegrevement2018")
                        )
          ),
        div(style = "clear: both")
      )
    )
  )
)
