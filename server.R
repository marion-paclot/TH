server <- function(input, output, session) {
  
  # Pop up quand clic sur Pourquoi ?
  observeEvent(input$pourquoi, { showModalPourquoi() })
  
  ##############################################################################
  #### Onglet de saisie des informations personnelles
  
  # Pop up quand clic sur rfr absent
  observeEvent(input$rfrAbsent, { showModalRFR() })
  
  # Basculer d'un onglet à l'autre quand on clique sur le bouton de lancement
  observeEvent(input$lancement, {
    updateTabItems(session, "tabs", "resultat")
  })
  
  observeEvent(input$detail, {
    updateTabItems(session, "tabs", "detail")
  })
  
  # Introduction
  output$introduction = renderUI({
    phrase = "<h1>COMPRENEZ VOTRE TAXE D'HABITATION !</h1>
            <h4>Cet outil a pour objectif de vous permettre de comprendre
            comment a été calculée votre taxe d'habitation 2017. 
            <br>Des erreurs peuvent subsister dans ce simulateur. 
            La valeur sur votre avis d'imposition fait foi.
            <br>Munissez-vous de votre avis d'imposition, 
            qui contient des informations personnelles nécessaires au calcul.</h4>"
    phrase = HTML(phrase)
  })
  
  # Validation des saisies
  entree <- reactive({
    
    nomDep = input$nomDepartement
    print(nomDep)
    nomCom = input$nomCommune
    nbParts = as.numeric(gsub(',', '.', input$nbParts))
    nbPAC = as.numeric(gsub(',', '.', input$nbPAC))
    
    # Protection contre les valeurs vides
    vlBrute = input$vlBrute
    vlBrute[is.na(vlBrute)] = 0

    rfr = input$rfr
    rfr[is.na(rfr)] = 0
    
    nbPAC[is.na(nbPAC)] = 1
    nbParts[is.na(nbParts)] = 0
    tauxMajRsCommune = input$tauxMajRsCommune
    tauxMajRsCommune[is.na(tauxMajRsCommune)] = 0
    
    # Figer le nom du département jusqu'à ce qu'un département existant soit choisi
    # Création d'une variable qui prend la précédente valeur correcte. Initialisation à l'ouverture de l'app
    stockageDep <<- ifelse(nomDep %in% dep, nomDep, stockageDep)
    nomDep = ifelse(nomDep %in% dep, nomDep, stockageDep)
    
    # Figer le nom de la commune tant qu'elle n'est pas choisie
    if (nomCom == '') {
      nomCom = rei$LIBCOM[rei$LIBDEP == nomDep][1]
    }
    
    cat('Lancement des calculs pour', nomDep, '>', nomCom, '\n')
    
    # Code INSEE de la commune
    codeCom = subset(rei, LIBDEP == nomDep & LIBCOM == nomCom)$IDCOM
    reiCom = subset(rei, IDCOM == codeCom)
    # print(reiCom)
    
    # Taxe d'habitation sur les logements vacants. Si TRUE alors la th n'est pas due
    # Par simplicité on ramène la vlBrute à 0
    tlv = reiCom$COMTLV == 1
    if (input$residence == 'vacant' & tlv){
      vlBrute = 0
    }
      
    ## Zone géographique
    zoneGeo = "Métropole"
    zoneGeo = ifelse(nomDep == 'GUYANE', 'Guyane', zoneGeo)
    zoneGeo = ifelse(nomDep == 'MAYOTTE', 'Mayotte', zoneGeo)
    zoneGeo = ifelse(nomDep %in% c('GUADELOUPE', 'MARTINIQUE', 'LA REUNION'), 'Guadeloupe Martinique Réunion', zoneGeo)
    
    return(list(nbParts = nbParts,
                nbPAC = nbPAC,
                zoneGeo = zoneGeo,
                codeCom = codeCom,
                reiCom = reiCom,
                vlBrute = vlBrute,
                rfr = rfr,
                tlv = tlv,
                tauxMajRsCommune = tauxMajRsCommune))
    
  }) 
  
  choix_commune = observe({
    departement = input$nomDepartement
    choix = subset(rei, LIBDEP == departement)
    # Pour éviter qu'un calcul soit lancé avec un couple (département, commune) incohérent
    freezeReactiveValue(input, 'nomCommune')
    updateSelectizeInput(session, 'nomCommune', 
                         choices = choix$LIBCOM,
                         selected = choix$LIBCOM[1],
                         server = FALSE)
  })
  
  choix_collectivite = observe({
    nomsCollectivite = list("Commune" = "commune",
                            "Syndicat" = "syndicat",
                            "Intercommunalité" = "interco",
                            "TSE" = "TSE", "GEMAPI" = "GEMAPI")
    nomsCollectivite = nomsCollectivite[which(calculTH()$tauxCotisation>0)]
    updateRadioButtons(session, 'ab_gph', choices = nomsCollectivite, inline = TRUE)
  })
  
  ## Bascule d'un onglet à l'autre quand clic sur une ligne spécifique du tableau calcul
  
  # observeEvent(input$calcul_cell_clicked, {
  #   cellule = data.frame(input$calcul_cell_clicked)
  #   if (nrow(cellule) >0){
  #     if (cellule$value == "Abattements"){
  #       updateTabsetPanel(session,inputId = 'tabs', selected = 'Abattements')
  #     }
  #   }
  # })
  
  ### Seuils divers d'exonération et d'abattement
  seuils <- reactive({
    return(list(
      art1417_1 = calculer_seuil(grille_1417_1_CGI, entree()$zoneGeo, "2017", entree()$nbParts),
      art1417_2 = calculer_seuil(grille_1417_2_CGI, entree()$zoneGeo, "2017", entree()$nbParts),
      art1417_1bis = calculer_seuil(grille_1417_1bis_CGI, entree()$zoneGeo, "2017", entree()$nbParts),
      art1414_A1 = calculer_seuil(grille_1414_A1_CGI, entree()$zoneGeo, "2017", entree()$nbParts),
      art1417_2bisa = calculer_seuil(grille_1417_2bisa_CGI, "France", "2018", entree()$nbParts),
      art1417_2bisb = calculer_seuil(grille_1417_2bisb_CGI, "France", "2018", entree()$nbParts)
    ))
  })
  
  #### EXONERATION
  exonerations <- reactive({
    # Calcul du seuil d'exonération
    exoneration_1417_1 = entree()$rfr <= seuils()$art1417_1
    
    # Situation d'exonération totale
    exoDOM = exo_logement_modeste_DOM(entree()$zoneGeo, entree()$vlBrute, entree()$reiCom)
    exoIndigent = 'Indigent' %in% input$situation
    exoAspaAsi = any(c('ASPA', 'ASI') %in% input$alloc)
    exoAAH = 'AAH' %in% input$alloc & exoneration_1417_1
    exoAahRejet = 'AAH' %in% input$alloc & ! exoneration_1417_1
    exoVeufSenior = any(c('Veuf', 'Senior') %in% input$situation) & ! input$isf & exoneration_1417_1 
    exoVeufSeniorRejetIsf = any(c('Veuf', 'Senior') %in% input$situation) & input$isf
    exoVeufSeniorRejetSeuil = any(c('Veuf', 'Senior') %in% input$situation) & !exoneration_1417_1 
    
    exoLogementVacant = entree()$tlv & input$residence == 'vacant'
    exoCriteresSociaux = any(exoDOM | exoIndigent| exoAspaAsi | exoAAH | exoVeufSenior) & 
      input$residence %in% c('principale', 'dépendance principale')
    assujetti = ! exoCriteresSociaux & ! exoLogementVacant
    
    return(list(exoDOM = exoDOM, 
                exoIndigent = exoIndigent,
                exoAspaAsi = exoAspaAsi, 
                exoAAH = exoAAH,
                exoVeufSenior = exoVeufSenior,
                exoAahRejet = exoAahRejet,
                exoVeufSeniorRejetIsf = exoVeufSeniorRejetIsf,
                exoVeufSeniorRejetSeuil = exoVeufSeniorRejetSeuil,
                exoLogementVacant = exoLogementVacant,
                assujetti = assujetti))
  })

  output$assujetti <- reactive({
    exonerations()$assujetti
  })
  outputOptions(output, "assujetti", suspendWhenHidden = FALSE)
  
  calculTH  <- reactive({
    calculComplet(entree()$nbPAC, entree()$rfr, seuils()$art1417_1, entree()$vlBrute, 
                  input$situation, input$alloc, entree()$reiCom,
                  colAbattements, input$residence, 
                  entree()$zoneGeo, input$isf, entree()$nbParts, entree()$tauxMajRsCommune)
    
  })
   
  output$baseElevee <- reactive({
    calculTH()$basesNettes[1] > 4573
  })
  outputOptions(output, "baseElevee", suspendWhenHidden = FALSE)
  
  output$eligiblePlafond <- reactive({
    calculTH()$eligibilite
  })
  
  output$beneficiairePlafond <- reactive({
    calculTH()$degrevementCalcule > 0
  })
  outputOptions(output, "beneficiairePlafond", suspendWhenHidden = FALSE)
  
  outputOptions(output, "eligiblePlafond", suspendWhenHidden = FALSE)
  
  output$montantRameneA0 <- reactive({
    calculTH()$montantThFinal != calculTH()$montantDu
  })
  outputOptions(output, "montantRameneA0", suspendWhenHidden = FALSE)
  
  
  output$degrevementBaseElevee <- reactive({
    calculTH()$degrevementBaseElevee
  })
  outputOptions(output, "degrevementBaseElevee", suspendWhenHidden = FALSE)
  
  ##############################################################################
  #### Onglet Résultat
  
  output$totalBox <- renderUI ({
    valeur = ifelse(exonerations()$assujetti, calculTH()$montantDu, "0 €")
    valueBox(
      valeur,
      "Montant final de la taxe",
      color = "navy",
      width = 12,
      href = "#"
    )
  })

  # exoneration <- reactive({showTextExoneration(entree()$nbParts, seuils()$art1417_1, exonerations(), input$residence, entree()$tlv) }) 
  output$explicationAssujettissement <- renderUI ({
    # exoneration()
    showTextExoneration(entree()$nbParts, seuils()$art1417_1, exonerations(), input$residence, entree()$tlv)
  })
  
  # Boite warning dans le cas où le plafonnement est touché
  output$warningPlafond <- renderUI ({
    phrase = "<p>Vous bénéficiez du plafonnement. Certaines données nécessaires 
    à son calcul n'étant pas disponibles en open data, le calcul n'a pas été 
    mené à son terme. Cela peut expliquer un écart entre le montant calculé ici 
    et le montant de votre impôt.</p>"
    return(HTML(phrase))
  })
  
  output$montantAnnule <- renderUI({
    phrase = ''
    if(calculTH()$montantThFinal != calculTH()$montantDu){
      phrase = sprintf("<p>Votre taxe d'habitation finale est de %s. Ce montant étant inférieur à 12 €, 
      il n'est pas dû.</p>", calculTH()$montantThFinal)
    }
    return(HTML(phrase))
  })
  
  ##############################################################################
  #### Onglet Abattements
  
  output$vlNette <- renderUI({
    HTML(text_abattements)
    })
  output$exoPartielle <- renderUI({
    HTML(pExoPartielle)
  })
  
  vlBruteNonRenseignee = reactive({
    valueBox(
      'Donnée manquante',
      "Vous n'avez pas encore renseigné de valeur locative brute pour votre bien.",
      color = "orange",
      width = 12,
      href = "#"
    )
  })
  
  output$vlBruteNulle_resultat <- renderUI ({
    vlBruteNonRenseignee()
  })
  
  output$vlBruteNulle_abattements <- renderUI ({
    vlBruteNonRenseignee()
  })
  
  output$vlBruteNulle_plafonnement <- renderUI ({
    vlBruteNonRenseignee()
  })
  
  output$pas_d_abattement <- renderText({
    typeRes = input$residence
    typeRes = ifelse(typeRes == 'secondaire', 'une résidence secondaire', typeRes)
    typeRes = ifelse(typeRes == 'vacant', 'un logement vacant', typeRes)
    typeRes = ifelse(typeRes == 'dépendance princ', 'une dépendance de votre résidence principale', typeRes)
    
    phrase = sprintf("Cette habitation est %s. Vous ne bénéficiez donc pas d'abattement.
                     <br>La valeur locative nette ou base nette d'imposition est alors égale à la valeur 
                     locative brute du bien.", typeRes)
    return(formatter_phrase(phrase))
  })
  
  output$calcul_baseNette = DT::renderDataTable({
    datatable(calculTH()$detail[1:3,], 
              options = list(dom = 't', "pageLength" = 40))
  })
  
  output$abattements <- DT::renderDataTable({
    ct = input$ab_gph
    
    vlMoyenne = entree()$reiCom[,as.character(colValeurLoc[ct])]
    vlMax = round2((1.3 + entree()$nbPAC*0.1)*vlMoyenne,0)
    
    multiplicateur = unlist(calculTH()$multiplicateurs[ct])
    abattements = unlist(calculTH()$abattements[ct])
    explications = rep(NA, 5)
    
    
    # Adaptation des explications :
    ### Général à la base
    explications[1] = ifelse(abattements[1] > 0,
                             "La collectivité a voté un abattement qui s'applique à l'ensemble des foyers.",
                             "La collectivité n'a pas voté d'abattement général à la base.")
    ### Personne à charge de rang 1 ou 2
    explications[2] = ifelse(abattements[2] > 0,
                             "Abattement pour chacune des deux premières personnes à charge.",
                             "La collectivité n'a pas voté d'abattement pour les deux premières personnes à charge.")
    if (abattements[2] > 0 & entree()$nbPAC == 0){
      explications[2] = paste(explications[2], "Vous n'avez pas de personne à charge.")
    }
    if (abattements[2] > 0 & entree()$nbPAC >0){
      explications[2] = paste(explications[2], "Vous avez des personnes à charge.")
    }
    ### Personne à charge de rang 3 et plus
    explications[3] = ifelse(abattements[3] > 0,
                             "Abattement pour chaque personne à charge, au delà de 2.",
                             "La collectivité n'a pas voté d'abattement pour les 
                             personnes à charge au delà de 2.")
    if (abattements[3] > 0 & entree()$nbPAC < 2.5){
      explications[3] = paste(explications[3], "Vous n'avez pas de personne à charge de rang 3 ou plus.")
    }
    if (abattements[3] > 0 & entree()$nbPAC >0){
      explications[3] = paste(explications[3], "Vous avez des personnes à charge de rang 3.")
    }
    
    ### Abattement special à la base
    explications[4] = ifelse(abattements[4] > 0,
                             sprintf("Abattement sous conditions : votre rfr ne doit pas dépasser %s et
                                     la valeur locative de votre bien ne doit pas excéder %s.", 
                                     euro(seuils()$art1417_2, F), euro(vlMax, F)),
                             "La collectivité n'a pas voté d'abattement en faveur des personnes de condition modeste.")
    ### Abattement handicapé
    explications[5] = ifelse(abattements[5] > 0,
                             "Abattement applicable aux foyers dans lesquels une personne est en situation de handicap, bénéficiaire de l'ASI ou de l'AAH.",
                             "La collectivité n'a pas voté d'abattement en faveur des personnes handicapées ou invalides.")
    
    detailAbattements = data.frame(Abattements = euro(abattements, F),
                                   Multiplicateur = paste('x', multiplicateur),
                                   Explication = explications)
    rownames(detailAbattements) = c("Général à la base", "PAC1-2", "PAC3", "Spécial", "Handicapé")
    detailAbattements = datatable(detailAbattements, options = list(dom = 't'))
    
    return(detailAbattements)
  })

  output$cascadeAbattements = renderPlotly({
    ct = input$ab_gph
    
    multiplicateur = unlist(calculTH()$multiplicateurs[ct])
    abattements = unlist(calculTH()$abattements[ct])
    
    g = cascade_abattements(entree()$vlBrute, abattements, multiplicateur)
    return(g)
  })
  
  ##############################################################################
  ### Onglet Détails
  
  RV = reactiveValues(TAB_BOX = "cotisations")

  output$boxActive <- reactive({
    return(RV[['TAB_BOX']])
  })
  outputOptions(output, "boxActive", suspendWhenHidden = FALSE)
  
  output$boxActivePlafonnement <- reactive({
    RV[['TAB_BOX']] == "plafonnement"
  })
  outputOptions(output, "boxActivePlafonnement", suspendWhenHidden = FALSE)
  
  output$boxInactivePlafonnement <- reactive({
    RV[['TAB_BOX']] != "plafonnement"
  })
  outputOptions(output, "boxInactivePlafonnement", suspendWhenHidden = FALSE)
  
  output$boxActiveBaseElevee <- reactive({
    RV[['TAB_BOX']] == "baseElevee"
  })
  outputOptions(output, "boxActiveBaseElevee", suspendWhenHidden = FALSE)
  
  # Titre de la page
  output$titreCalcul = renderUI({
    if (RV[['TAB_BOX']] == 'cotisations') {
      phrase = "<h3>Calcul des cotisations</h3>"
    }
    if (RV[['TAB_BOX']] == 'frais') {
      phrase = "<h3>Calcul des frais de gestion</h3>"
    }
    if (RV[['TAB_BOX']] == 'baseElevee') {
      phrase = "<h3>Calcul du prélèvement pour base élevée</h3>"
    }
    if (RV[['TAB_BOX']] == 'residenceSecondaire') {
      phrase = "<h3>Calcul du prélèvement pour résidence secondaire</h3>"
    }
    if(RV[['TAB_BOX']] == 'plafonnement'){
      phrase = "<h3>Calcul du dégrèvement lié au plafonnement</h3>"
    }
    return(HTML(phrase))
  })
  
  # Tableau à afficher quand on clique sur les boites
  output$calculDetaille = DT::renderDataTable({
    if (RV[['TAB_BOX']] == 'cotisations') {
      tableau = calculTH()$detail[3:5, ]
    }
    if (RV[['TAB_BOX']] == 'frais') {
      tableau = calculTH()$detail[5:7, ]
    }
    if (RV[['TAB_BOX']] == 'baseElevee') {
      tableau = calculTH()$detail[c(5, 8, 9), ]
    }
    if (RV[['TAB_BOX']] == 'residenceSecondaire') {
      tableau = calculTH()$detail[c(5, 10, 11), ]
    }
    if(RV[['TAB_BOX']] == 'plafonnement'){
      tableau = data.frame(c(1,1))
    }
    datatable(tableau, options = list(dom = 't', "pageLength" = 40))
  })
  
  
  # Explication à afficher quand on clique sur les boites
  output$calculExplication = renderUI({
    if (RV[['TAB_BOX']] == 'cotisations') {
      phrase = "<p>Base nette d'imposition x taux de cotisation voté par la collectivité.</p>"
      if (input$residence == 'secondaire'){
        phrase2 = "<p>Dans le cas d'une résidence secondaire, les communes peuvent décider
        d'une majoration du taux de cotisation allant de 5% à 60%. 
        Si votre commune a voté une majoration, son taux est indiqué sur votre feuille d'imposition. 
        Vous pouvez entrer cette valeur dans le menu de paramètres.</p>"
        phrase = paste0(phrase, phrase2)
      }
    }
    
    if (RV[['TAB_BOX']] == 'frais') {
      phrase = "<p>Cotisations x taux dépendant de la collectivité.</p> 
      <p>L'Etat est chargé de la collecte de la taxe d'habitation et perçoit les
      frais de gestion. 
      <br>Les frais de gestion pour la commune et l'intercommunalité sont calculés en 
      sommant les cotisations avant application du taux.</p>"
    }
    
    if (RV[['TAB_BOX']] == 'baseElevee') {
      phrase = "<p>L'Etat perçoit une majoration de cotisation pour les résidences dont la
     locative nette communale dépasse 4 573 €. Pour les résidences principales, ce taux est de 2%.
      Pour les résidences secondaires dont la valeur locative nette est inférieure ou égale à 7 622 €, 
      il est 1,2%, 1,7% au delà.</p>"
    }
    
    if (RV[['TAB_BOX']] == 'residenceSecondaire') {
      phrase = "<p>L'Etat perçoit une majoration de cotisation dans le cas des résidences secondaires,
      correspondant à 1,5% des cotisations dues à la commune et aux EPCI à fiscalité propre.</p>"
      }
    
    if (RV[['TAB_BOX']] == 'plafonnement'){
      phrase = sprintf("<p>Compte tenu de votre situation (nombre de parts fiscales et département), 
                    votre revenu fiscal de référence ne doit pas dépasser %s pour 
                       que le plafonnement de la taxe d'habitation vous soit accordé. 
                       Ce plafonnement n'est pas valable pour les résidences secondaires, ni
                       pour les foyers ayant acquitté l'ISF.</p>", euro(seuils()$art1417_2bisb, F))
      if (! calculTH()$eligibilite){
        if (entree()$rfr > seuils()$art1417_2) {
          phrase2 = sprintf("<p>Votre revenu fiscal de référence étant de %s, vous ne pouvez pas bénéficier
                           du plafonnement de votre taxe d'habitation.</p>",euro(entree()$rfr, F))
        }
        if (input$isf) {
          phrase2 = sprintf("<p>Vous ne bénéficiez pas du plafonnement car vous avez payé
                            l'ISF l'an passé.</p>",euro(entree()$rfr, F))
        }
        if (! input$residence %in% c('principale', 'dépendance princ')) {
          phrase2 = sprintf("<p>Le plafonnement n'est applicable que pour les résidences
                            principales.</p>",euro(entree()$rfr, F))
        }
        phrase = paste0(phrase, phrase2)
      }
      if (calculTH()$eligibilite){
        phrase = paste0(phrase, "<p>Vous bénéficiez du plafonnement de votre taxe d'habitation.</p>")
      }
      
    }
    return(HTML(phrase))
  })

  output$exonerationBaseElevee = renderUI({
    phrase = "<p>Vous bénéficiez du plafonnement de la taxe d'habitation. Ce prélèvement n'est 
    donc pas dû.</p>"
    return(HTML(phrase))
  })
  
  ### Cotisations
  output$cotisationsBox <- renderUI ({
    makeButton("cotisations", "Cotisations principales", calculTH()$totalCotisations, RV[["TAB_BOX"]])
  })
  observeEvent(input$button_cotisations, {
    RV[["TAB_BOX"]] = "cotisations"
  })
  
  ### Frais
  output$fraisBox <- renderUI ({
    makeButton("frais", "Frais de gestion\n ", calculTH()$totalFraisGestion, RV[["TAB_BOX"]])
  })
  observeEvent(input$button_frais, {
    RV[["TAB_BOX"]] = "frais"
  })
  
  ### Base Elevée
  output$baseEleveeBox <- renderUI ({
    valeur = calculTH()$totalPrelevementBaseElevee
    if (calculTH()$degrevementBaseElevee >0){
      valeur = "0 €*"
    }
    makeButton("baseElevee", "Prélèvement base élevée ", valeur, RV[["TAB_BOX"]])
  })
  observeEvent(input$button_baseElevee, {
    RV[["TAB_BOX"]] = "baseElevee"
  })
  
  # Résidence secondaire
  output$residenceSecondaireBox <- renderUI ({
    makeButton("residenceSecondaire", "Prélèvement résidence secondaire", calculTH()$totalPrelevementResSecondaire, RV[["TAB_BOX"]])
  })
  observeEvent(input$button_residenceSecondaire, {
    RV[["TAB_BOX"]] = "residenceSecondaire"
  })
  
  
  # Plafonnement
  output$plafonnementBox <- renderUI({
    valeur = paste('-', euro(calculTH()$degrevementApplique, F))
    makeButton('plafonnement', "Plafonnement", valeur, RV[["TAB_BOX"]])
  })
  observeEvent(input$button_plafonnement, {
    RV[["TAB_BOX"]] = "plafonnement"
  })
  
  ##############################################################################
  ### Onglet Plafonnement 2017
  
  output$explicationPlafonnement <- renderUI({
    phrase = "<p>Le calcul du plafond se fait en trois temps :
    <ol>
      <li>Calcul d'un revenu fiscal de référence abattu. Le montant de l'abattement
    dépend du nombre de parts du foyer fiscal ainsi que du département de résidence</li>
      <li>Calcul d'un seuil à ne pas exécéder : 3,44% du revenu fiscal de référence abattu,
      calculé à l'étape précédente.<br>Le dégrèvement correspond à la différence entre le montant
      (cotisations + frais) et le seuil que l'on vient de calculer.</li>
      <li>Calcul d'une éventuelle réduction du dégrèvement : si les taux de cotisations applicables
      à une habitation de votre commune ont augmenté
      par rapport à 2000, ou que des abattements ont été réduits par rapport à 2003, 
      le dégrèvement est réduit. La baisse du dégrèvement n'est appliquée que si elle
      est supérieure à 15 €.</li></ol>
    Un dégrèvement final de moins de 8 € n'est pas appliqué.</p>
    <p>Les données nécessaires à cette troisième étape étant indisponibles
    en open data, l'éventuelle réduction du dégrèvement n'a pas été modélisée ici. Il s'en suit
    que le montant final du dégrèvement pourrait être inexact."
    return(HTML(phrase))
  })
  
  output$graphPlafond = renderPlot({
    g = dessiner_plafonnement(entree()$rfr, calculTH()$abattementPlafond)
    return(g)
  })
  
  # Cas où le contribuable est éligible au plafonnement
  output$calculPlafonnement <- renderUI({
    
    if (!calculTH()$eligibilite){
      return(HTML(""))
    }
    
    rfrAbattu = calculTH()$rfrAbattu
    abattement = euro(calculTH()$abattementPlafond, F)
    plafond = calculTH()$plafond
    phrase = sprintf("<p>Votre revenu fiscal de référence est de %s. Un abattement de %s est calculé.
                      Le montant de votre rfr abattu est donc de %s. 
                     <br>Votre taxe d'habitation (cotisations + frais de gestion) ne doit pas excéder 3,44%% de 
                     ce rfr abattu, soit %s.</p>",
                     euro(entree()$rfr, F), abattement, rfrAbattu, plafond)
    return(HTML(phrase))
  })

  output$resultatPlafonnement = renderUI({
    degrevementCalcule = calculTH()$degrevementCalcule
    phrase = "Le montant de votre taxe étant en dessous du plafond, vous ne 
    bénéficiez pas de dégrèvement."
    if (degrevementCalcule>0){
      phrase = sprintf("L'application du plafonnement vous permet de bénéficier
                       d'un dégrèvement de cotisation de %s.", euro(degrevementCalcule,F))
      if (degrevementCalcule <8){
        phrase = paste(phrase, "<br>Ce dégrèvement étant de moins de 8 €, il n'est pas appliqué.")
      }
      phrase = sprintf("<p>%s</p>", phrase)
    }
    return(HTML(phrase))
})
  
  ##############################################################################
  ### Onglet Réforme 2018
  
  calculReforme2018 = reactive({
    calculer_reforme2018(entree()$rfr, entree()$nbParts, calculTH()$montantThFinal)
  })
  
  output$explicationReforme2018 <- renderUI({
    HTML("<p>Pour l'année 2018, les ménages dont le revenu fiscal de référence est 
         inférieur à un certain seuil bénéficient d'un dégrèvement pouvant 
         atteindre 30% du montant total de leur taxe d'habitation payée en 2017.
         <br>Le montant estimé en 2018 est calculé en considérant seulement l'éventuelle
         baisse liée à la réforme de la taxe d'habitation : si votre situation a changé,
         ou si un taux ou un abattement a été modifié, la valeur affichée sera fausse.</p>
         <p>Cette réforme ne concerne que les résidences principales.</p>")
  })
  
  output$degrevement2018 <- renderUI({
    valueBox(
      calculReforme2018()$degrevement,
      "Montant du dégrèvement attendu pour 2018",
      color = "navy",
      width = 12,
      href = "#"
    )
  })
  
  output$tauxDegrevement2018 <- renderUI({
    valueBox(
      calculReforme2018()$taux,
      "Taux de dégrèvement",
      color = "navy",
      width = 12,
      href = "#"
    )
  })
  
  output$montant2018 <- renderUI({
    valueBox(
      calculReforme2018()$montantTaxe,
      "Montant final attendu pour 2018 (indicatif)",
      color = "navy",
      width = 12,
      href = "#"
    )
  })
  
  output$courbeDegrevement2018 = renderPlotly({
    g = calculReforme2018()$graph
    return(g)
  })


}
