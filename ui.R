navbarPage(
  title = "Taxe d'habitation",
  tabPanel(
    "Simulation",
    sidebarPanel(
      width = 4,
      id = "form",
      div(
        style = "display:inline-block;",
        numericInput('rfr', 
                     label = 'Revenu fiscal de référence',
                     value = 0,
                     min = 0,
                     step = 1)
      ),
      checkboxInput('isf', "ISF", value = FALSE),
      bsTooltip(
        id = "isf",
        title = tooltipIsf,
        trigger = "hover",
        placement = 'right'
      ),
      checkboxGroupInput(
        'alloc',
        'Bénéficiaire de ces allocations',
        choices = c('ASPA', 'ASI', 'AAH'),
        inline = TRUE
      ),
      groupTooltip(id = "alloc", choice = "AAH", title = tooltipAah, trigger = "hover"),
      
      checkboxGroupInput(
        'situation',
        'Situation personnelle au 1er janvier 2017',
        choices = c('Veuf', 'Senior', 'Handicapé', 'Indigent'),
        inline = TRUE
      ),
      groupTooltip(id = "situation", choice = "Veuf", title = tooltipVeuf, trigger = "hover"),
      groupTooltip(id = "situation", choice = "Senior", title = tooltipSenior, trigger = "hover"),
      groupTooltip(id = "situation", choice = "Handicapé", title = tooltipHandicape, trigger = "hover"),
      groupTooltip(id = "situation", choice = "Indigent", title = tooltipIndigent, trigger = "hover"),

      
      div(
        style = "display:inline-block",
        numericInput(
          'nbParts',
          'Nombre de parts fiscales',
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
          'nbPAC',
          'Nombre de personnes à charge',
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
      selectizeInput(
        "nomDepartement",
        "Département",
        choices = unique(rei$LIBDEP),
        selected = rei$LIBDEP[1],
        multiple = FALSE,
        options = NULL
      ),
      selectizeInput(
        "nomCommune",
        "Commune",
        choices = '',
        selected = '',
        multiple = FALSE,
        options = NULL
      ),
      
      div(
        style = "display:inline-block",
        numericInput(
          'vlBrute',
          'Valeur locative brute',
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
                       "Logement vacant" = 'vacant'),inline = T)
      )
      
     
    ),
    mainPanel(width = 8,
              tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
              
    tabsetPanel(
      id = 'tabs',
      type = "tabs",
      tabPanel("Taxe",
               br(),
               verbatimTextOutput("exoneration"),
               br(),
               conditionalPanel(condition = "output.assujeti", dataTableOutput("calcul")),
               br(),
               conditionalPanel(condition = "output.assujeti", dataTableOutput("totaux")),
               br(),
               conditionalPanel(condition = "output.assujeti", dataTableOutput("plafonnement"))
              ),
      tabPanel('Abattements',
               br(),
               conditionalPanel(condition = "output.assujeti && input.residence == 'principale'", verbatimTextOutput("vlNette")),
               conditionalPanel(condition = "input.residence == 'secondaire'", verbatimTextOutput("vlNette_secondaire")),
               br(),
               # uiOutput("myList"),
               # br(),
               conditionalPanel(condition = "output.assujeti && input.residence == 'principale'",
                                radioButtons('ab_gph', "Element de calcul de la base d'imposition, détail des abattements",
                                             choices = c('Commune' = 'commune',
                                                         'Syndicat' = 'syndicat',
                                                         'Intercommunalité' = 'interco',
                                                         'TSE' = 'TSE', 'GEMAPI' = 'GEMAPI'), inline = TRUE)),
               conditionalPanel(condition = "output.assujeti && input.residence == 'principale'", 
                                dataTableOutput("abattements")),
               br(),
               conditionalPanel(condition = "output.assujeti && input.residence == 'principale'", 
                                plotlyOutput("cascadeAbattements"))

               ),
      tabPanel('Cotisations et frais de gestion',
               br(),
               h4('Cotisations'),
               conditionalPanel(condition = "output.assujeti", verbatimTextOutput("cotisations")),
               conditionalPanel(condition = "output.assujeti && input.residence == 'secondaire'", 
                                verbatimTextOutput("cotisationsMajResSecondaire")),
               h4('Frais de gestion'),
               conditionalPanel(condition = "output.assujeti", verbatimTextOutput("fraisGestion")),
               h4("Majoration résidence secondaire au profit de l'Etat")
      ),
      tabPanel('Plafonnement')
      )
    )
  ),
  tabPanel("Aide",
          tags$span("Trouver les textes de loi ...")),
  footer = column(
    12,
    tags$i("Module développé par "),
    tags$a(href = 'https://www.etalab.gouv.fr/fr/', 'Etalab')
  )
)