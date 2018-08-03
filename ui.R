navbarPage(
  title = "Taxe d'habitation",
  tabPanel(
    "Simulation",
    sidebarPanel(
      width = 4,
      id = "form",
      
      numericInput('rfr', 
                   label = 'Revenu fiscal de référence',
                   value = 0,
                   min = 0,
                   step = 1),
      
      checkboxInput('isf', "ISF", value = FALSE),
      checkboxGroupInput(
        'alloc',
        'Bénéficiaire de ces allocations',
        choices = c('ASPA', 'ASI', 'AAH'),
        inline = TRUE
      ),
      checkboxGroupInput(
        'situation',
        'Situation personnelle',
        choices = c('Veuf', 'Senior', 'Handicapé'),
        inline = TRUE
      ),

      div(
        style = "display:inline-block",
        numericInput(
          'nbParts',
          '# parts fiscales',
          1,
          min = 1,
          max = 99,
          step = 0.25
        )
      ),
      bsTooltip(
        id = "nbParts",
        title = "Nombre de parts fiscales sur la déclaration de revenu",
        placement = "right",
        trigger = "hover"
      ),
      div(
        style = "display:inline-block",
        numericInput(
          'nbPAC',
          '# PAC',
          0,
          min = 0,
          max = 99,
          step = 0.5
        )
      ),
      bsTooltip(
        id = "nbPAC",
        title = tooltip_PAC,
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
        style = "display:inline-block",
        radioButtons("residence", "Résidence",
                     c("Principale" = "principale",
                       "Secondaire" = "secondaire"),inline = F)
      )
      
     
    ),
    mainPanel(width = 8,
    tabsetPanel(
      type = "tabs",
      tabPanel("Taxe",
               br(),
               textOutput("exoneration_totale"),
               br(),
               conditionalPanel(condition = "output.assujeti", dataTableOutput("calcul")),
               br(),
               conditionalPanel(condition = "output.assujeti", textOutput("plafond"))
              ),
      tabPanel('Valeur loc nette',
               conditionalPanel(condition = "output.assujeti", textOutput("vlNette")),
               br(),
               # uiOutput("myList"),
               # br(),
               conditionalPanel(condition = "output.assujeti", 
                                radioButtons('ab_gph', "Element de calcul de la base d'imposition, détail des abattements",
                                             choices = c('Commune' = 'commune', 
                                                         'Syndicat' = 'syndicat', 
                                                         'Intercommunalité' = 'interco', 
                                                         'TSE' = 'TSE', 'GEMAPI' = 'GEMAPI'), inline = TRUE)),
               conditionalPanel(condition = "output.assujeti", dataTableOutput("abattements")),
               br(),
               conditionalPanel(condition = "output.assujeti", plotlyOutput("cascadeAbattements"))
               
               )
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