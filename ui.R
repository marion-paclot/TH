navbarPage(
  title = "Taxe d'habitation",
  tabPanel(
    "Simulation",
    sidebarPanel(
      width = 4,
      id = "form",
      div(
        style = "display:inline-block;",
        numericInput("rfr", 
                     label = "Revenu fiscal de référence",
                     value = 0,
                     min = 0,
                     step = 1)
      ),
      div(
        style = "display:inline-block ; margin-left: 10px;",
        checkboxInput("rfrAbsent", "Pas de RFR", value = FALSE),
        bsTooltip(
          id = "rfrAbsent",
          title = "Le RFR est \\à blanc sur l\\'avis d\\'imposition",
          trigger = "hover"
        )
      ),
      
      checkboxInput("isf", "ISF", value = FALSE),
      bsTooltip(id = "isf", title = tooltipIsf, trigger = "hover"),
      checkboxGroupInput(
        "alloc",
        "Bénéficiaire de ces allocations",
        choices = c("ASPA", "ASI", "AAH"),
        inline = TRUE
      ),
      groupTooltip(id = "alloc", choice = "AAH", title = tooltipAah, trigger = "hover"),
      
      checkboxGroupInput(
        "situation",
        "Situation personnelle au 1er janvier 2017",
        choices = c("Veuf", "Senior", "Handicapé", "Indigent"),
        inline = TRUE
      ),
      groupTooltip(id = "situation", 
                   choice = "Veuf", 
                   title = tooltipVeuf, 
                   trigger = "hover"),
      groupTooltip(id = "situation", 
                   choice = "Senior", 
                   title = tooltipSenior, 
                   trigger = "hover"),
      groupTooltip(id = "situation", 
                   choice = "Handicapé", 
                   title = tooltipHandicape, 
                   trigger = "hover"),
      groupTooltip(id = "situation", 
                   choice = "Indigent", 
                   title = tooltipIndigent, 
                   trigger = "hover"),

      
      div(
        style = "display:inline-block",
        numericInput(
          "nbParts",
          "Nombre de parts fiscales",
          1,
          min = 1,
          max = 99,
          step = 0.25
        )
      ),
      bsTooltip(
        id = "nbParts",
        title = "Nombre de parts fiscales du ménage",
        trigger = "hover"
      ),
      div(
        style = "display:inline-block ; margin-left: 20px;",
        numericInput(
          "nbPAC",
          "Nombre de personnes à charge",
          0,
          min = 0,
          max = 99,
          step = 0.5
        )
      ),
      bsTooltip(
        id = "nbPAC",
        title = tooltipPac,
        placement = "right",
        trigger = "hover"
      ),
      div(
        style = "display:inline-block ; margin-left: 20px;",
        selectizeInput(
          "nomDepartement",
          "Département",
          choices = unique(rei$LIBDEP),
          selected = rei$LIBDEP[1],
          multiple = FALSE,
          options = NULL
        )),
      div(
        style = "display:inline-block ; margin-left: 20px;",
        selectizeInput(
          "nomCommune",
          "Commune",
          choices = "",
          selected = "",
          multiple = FALSE,
          options = NULL
        )),
      
      div(
        style = "display:inline-block",
        numericInput(
          "vlBrute",
          "Valeur locative brute",
          0,
          min = 1,
          step = 1
        )
      ),
      div(
        style = "display:inline-block; margin-left: 20px;",
        radioButtons("residence", "Résidence",
                     c("Principale" = "principale",
                       "Secondaire" = "secondaire", 
                       "Dépendance résidence principale" = "dépendance princ",
                       "Logement vacant" = "vacant"),inline = T)
      ),
      numericInput(
        "tauxMajRsCommune",
        "Taux de majoration résidence secondaire appliqué par la commune (en %)",
        0,
        min = 0,
        step = 1
      ),
      bsTooltip(id = "tauxMajRsCommune", title = tooltipMajRs, trigger = "hover")
    ),
    mainPanel(width = 8,
              tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
              
    tabsetPanel(
      id = "tabs",
      type = "tabs",
      tabPanel("Taxe",
               br(),
               tags$div(class="alert alert-info", textOutput("exoneration")),
               conditionalPanel(condition = "output.assujetti", tags$div(class="alert alert-info", textOutput("valeurFinale"))),
               conditionalPanel(condition = "output.plafondActif", tags$div(class="alert alert-warning", textOutput("warningPlafonnement"))),
               conditionalPanel(condition = "output.assujetti", hr()),
               conditionalPanel(condition = "output.assujetti", dataTableOutput("totaux"))
              ),
      
      tabPanel("Abattements",
               br(),
               conditionalPanel(condition = "output.assujetti && input.residence == 'principale'", 
                                tags$div(class="alert alert-info", textOutput("vlNette"))),
               conditionalPanel(condition = "input.residence != 'principale'", 
                                tags$div(class="alert alert-info", textOutput("pas_d_abattement"))),
               
               conditionalPanel(condition = "output.assujetti", h4("Base locatives nettes retenues")),
               conditionalPanel(condition = "output.assujetti", dataTableOutput("calcul_baseNette")),
               conditionalPanel(condition = "output.assujetti", hr()),
               
               conditionalPanel(condition = "output.assujetti && input.residence == 'principale'",
                                radioButtons("ab_gph", "Calcul de la base locative nette",
                                             choices = "", inline = TRUE),
                                groupTooltip(id = "ab_gph", choice = "GEMAPI", 
                                             title = "Gestion des milieux aquatiques et pr\\évention des inondations", trigger = "hover"),
                                groupTooltip(id = "ab_gph", choice = "TSE", 
                                             title = "Taxe sp\\éciale d\\'\\équipement", trigger = "hover")
                                ),

      
               conditionalPanel(condition = "output.assujetti && input.residence == 'principale'", 
                                dataTableOutput("abattements")),
               br(),
               conditionalPanel(condition = "output.assujetti && input.residence == 'principale'", 
                                plotlyOutput("cascadeAbattements"))

               ),
      
      ### Onglet cotisations, frais de gestion, 
      tabPanel("Cotisations et frais de gestion",
               
               # Cotisations
               br(),
               conditionalPanel(condition = "output.assujetti", h4("Cotisations")),
               conditionalPanel(condition = "output.assujetti", tags$div(class="alert alert-info", textOutput("expl_cotisations"))),
               conditionalPanel(condition = "output.assujetti", dataTableOutput("detail_cotisations")),
               
               # Frais de gestion
               conditionalPanel(condition = "output.assujetti", hr()),
               conditionalPanel(condition = "output.assujetti", h4("Frais de gestion")),
               conditionalPanel(condition = "output.assujetti", tags$div(class="alert alert-info", textOutput("expl_fraisGestion"))),
               conditionalPanel(condition = "output.assujetti", dataTableOutput("detail_fraisGestion")),
               
               # Majoration pour base locative élevée
               conditionalPanel(condition = "output.baseElevee", hr()),
               conditionalPanel(condition = "output.baseElevee", h4("Majoration sur base locative élevée au profit de l'Etat")),
               conditionalPanel(condition = "output.baseElevee", tags$div(class="alert alert-info", textOutput("expl_majorationBaseElevee"))),
               conditionalPanel(condition = "output.baseElevee", dataTableOutput("detail_majorationBaseElevee")),
               
               # Majoration RS au profit de l'Etat
               conditionalPanel(condition = "input.residence == 'secondaire'", hr()),
               conditionalPanel(condition = "input.residence == 'secondaire'", h4("Majoration résidence secondaire au profit de l'Etat")),
               conditionalPanel(condition = "input.residence == 'secondaire'", tags$div(class="alert alert-info", textOutput("expl_majorationRsEtat"))),
               conditionalPanel(condition = "input.residence == 'secondaire'", dataTableOutput("detail_majorationRsEtat"))
               ),
      
      # Onglet Plafonnement
      tabPanel("Plafonnement",
               br(),
               tags$div(class="alert alert-info", textOutput("explicationPlafonnement")),
               conditionalPanel(condition = "output.plafondActif",
                                tags$div(class="alert alert-info", textOutput("applicationPlafonnement"))),
               conditionalPanel(condition = "output.plafondActif", dataTableOutput("plafonnement"))
      ),
      
      # Onglet Réforme 2018
      tabPanel("Réforme 2018",
               br(),
               tags$div(class="alert alert-info", textOutput("reforme2018")),
               tags$div(class="alert alert-info", textOutput("calculReforme2018"))
      )
     )
    )
  ),
  tabPanel("Démarche",
          tags$div("Cette application a pour objectif de permettre aux contribuables de
                    comprendre comment a été calculée leur taxe d'habitation."),
          tags$br(),
          tags$div("L'onglet Simulation > Taxe vous donnera un rapide aperçu de la décomposition de votre taxe, 
                   tandis que les autres onglets vous permettront de comprendre le calcul des différentes
                   composantes de la taxe."),
          tags$br(),
          tags$div("L'application n'a été développée qu'à partir de documents et 
                   données librement accessibles, si bien que le calcul de la 
                   taxe n'est pas possible pour tous les cas et que des erreurs peuvent subsister.
                   Par ailleurs, des cas très spécifiques (foyers, ambassadeurs) n'ont pas été traités,
                   l'objectif étant de simuler la taxe du plus grand nombre sans refaire intégralement le travail de la DgFiP."),
          tags$div("Quelques exemples :"),
          tags$div(tags$ul(
            tags$li(tags$span("Lorsqu'une commune réunit trop peu d'habitations, 
                              la valeur locative moyenne de la commune n'est pas renseignée
                              afin de préserver le secret statistique. Cette valeur est pourtant nécessaire au calcul de
                              l'abattement spécial à la base.")),
            tags$li(tags$span("Les habitations principales des DOM sont exonérées de taxe d'habitation
                              si leur valeur locative est inférieure à 40% de la valeur locative moyenne de la commune.
                              Ce taux peut être porté à 50% par décision de la commune, mais cette information est absente du REI.
                              Aussi, certaines habitations sont-elles effectivement exonérées sans qu'on puisse le savoir.")),
            tags$li(tags$span("Le dégrèvement lié au plafonnement peut être réduit lorsque les collectivités ont 
                              modifié leur taux d'imposition ou leurs abattements. Cependant, 
                              calculer exactement le plafond nécessite de disposer de valeurs
                              pour les années 2000 (taux global 2000 corrigé) et 2003 (abattements de référence en 2003),
                              qui ne sont pas disponibles en open data."))),
            tags$br()
            
          )
          ),

  footer = column(
    12,
    tags$i("Module développé par Marion"),
    tags$br(),
    tags$a(href = "https://github.com/marion-paclot/TH/", "Voir le code source"),
    tags$br(),
    tags$a(href = "https://github.com/marion-paclot/TH/issues", "Signaler une erreur ou proposer une amélioration")
    # tags$a(href = "https://www.etalab.gouv.fr/fr/", "Etalab")
  )
)
