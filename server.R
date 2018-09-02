server <- function(input, output, session) {

  observeEvent(TRUE, {
    showModal(modalDialog(
      title = "Bienvenue",
      "Cette application permet aux contribuables de comprendre le calcul de leur propre taxe d'habitation.
      Des erreurs peuvent subsister, notamment dans le calcul du plafonnement ou de la taxe pour les logements vacants. 
      Les cas particuliers (hébergements collectifs par exemple) ne sont pas gérés par cette application.
      La valeur figurant sur votre avis d'imposition est celle qui fait foi.",
      easyClose = TRUE
    ))})
    
    
  entree <- reactive({
    nomDep = input$nomDepartement
    nomCom = input$nomCommune
    nbParts = as.numeric(gsub(',', '.', input$nbParts))
    nbPAC = as.numeric(gsub(',', '.', input$nbPAC))

    # Protection contre les valeurs vides
    vlBrute = input$vlBrute
    vlBrute[is.na(vlBrute)] = 0

    rfr = input$rfr
    rfr[is.na(rfr)] = 0
    rfr = ifelse(input$rfrAbsent, 100000000, rfr) # Cas d'un rfr à blanc sur l'avis de TH.

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

    # Taxe d'habitation sur les logements vacants. Si TRUE alors la th n'est pas due
    tlv = reiCom$COMTLV == 1

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
    art1417_1 = calculer_seuil(grille_1417_1_CGI, entree()$zoneGeo, "2017", entree()$nbParts)
    art1417_2 = calculer_seuil(grille_1417_2_CGI, entree()$zoneGeo, "2017", entree()$nbParts)
    art1417_1bis = calculer_seuil(grille_1417_1bis_CGI, entree()$zoneGeo, "2017", entree()$nbParts)
    art1414_A1 = calculer_seuil(grille_1414_A1_CGI, entree()$zoneGeo, "2017", entree()$nbParts)
    art1417_2bisa = calculer_seuil(grille_1417_2bisa_CGI, "France", "2018", entree()$nbParts)
    art1417_2bisb = calculer_seuil(grille_1417_2bisb_CGI, "France", "2018", entree()$nbParts)
    
    return(list(art1417_1 = art1417_1,
                art1417_1bis = art1417_1bis,
                art1417_2 = art1417_2,
                art1414_A1 = art1414_A1,
                art1417_2bisa = art1417_2bisa,
                art1417_2bisb = art1417_2bisb))
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
  
  ########################################################################
  #################################### CALCUL PRINCIPAUX
  
  calculTH  <- reactive({
    detailler_calcul(entree()$nbPAC, entree()$rfr, seuils()$art1417_1, entree()$vlBrute, 
                    input$situation, input$alloc, entree()$reiCom,
                    colAbattements, input$residence, 
                    entree()$zoneGeo, input$isf, entree()$nbParts, entree()$tauxMajRsCommune)
  })
  
  calculPlafond <- reactive({
    calcul = calculer_plafonnement(entree()$rfr, entree()$zoneGeo, 
                          input$isf, entree()$nbParts, input$residence,
                          calculTH()$totalCotisations, 
                          calculTH()$totalFraisGestion,
                          calculTH()$totalPrelevementBaseElevee, 
                          calculTH()$totalPrelevementResSecondaire)
    print(calcul)
    return(calcul)
  })

  output$baseElevee <- reactive({
    calculTH()$basesNettes[1] > 4573
  })
  outputOptions(output, "baseElevee", suspendWhenHidden = FALSE)


  ##############################################################################    
  ##############################################################################  
  ########## Onglet taxe
  
  output$exoneration <- renderText({
    
    phraseExoPartielle = "<br>Dans le cas où vous bénéficiez d'une exonération partielle 
    au titre de l'article 1414-I bis 2° du CGI, la valeur locative brute de votre 
    bien tient compte de l'abattement auquel vous avez droit.
    <br>Dans le cas où votre habitation se situe en zone de revitalisation rurale (ZRR) et que
    la commune a décidé d'une exonération de taxe d'habitation pour les parties louées
    à titre de meublé, la valeur locative brute de votre bien tient compte de l'abattement
    auquel vous avez droit."
    
    phraseSeuil = sprintf("<br>L'exonération est soumise à condition de revenu : 
                          Le RFR d'un foyer de %s part%s ne doit pas dépasser %s.", 
                          entree()$nbParts, ifelse(entree()$nbParts>1, 's', ''), 
                          euro(seuils()$art1417_1, F))
    
    phraseExoDom = "Vous habitez dans un département d'Outre-Mer et la valeur locative brute
    de votre logement est inférieure à 40% de la valeur locative moyenne de la commune.
    <br>Vous êtes exonéré de TH au titre de l'article 332 de l'annexe II du CGI."
    
    phraseExoIndigent = "La commission communale des impôts directs vous a reconnu indigent.
    <br>Vous êtes exonéré de TH au titre de l'article 1408-II-3° du CGI."
    
    phraseExoAspaAsi = "Vous percevez l'ASPA ou l'ASI. Aucune condition de revenu n'est requise.
    <br>Vous êtes exonéré de TH au titre de l'article 1417-I du CGI."
    
    phraseExoAah = sprintf("Vous percevez l'AAH. %s <br>Vous êtes exonéré de TH au 
                           titre de l'article 1417-I du CGI.", phraseSeuil)
    
    phraseExoAahRejet = sprintf("Vous percevez l'AAH. %s Vous n'êtes pas exonéré de TH. %s", 
                                phraseSeuil, phraseExoPartielle)
    
    phraseExoSeniorRejetIsf = sprintf("Vous êtes veuf/veuve ou avez plus de 60 ans 
                                      mais vous avez payé l'ISF l'an passé. <br>Vous n'êtes pas exonéré de TH. %s", phraseExoPartielle)
    
    phraseExoSeniorRejet = sprintf("Vous êtes veuf/veuve ou avez plus de 60 ans et 
                                   vous n'avez pas payé l'ISF l'an passé. %s 
                                   <br>Vous n'êtes pas exonéré de TH. %s", phraseSeuil, phraseExoPartielle)
    
    phraseExoSenior = sprintf("Vous êtes veuf/veuve ou avez plus de 60 ans et vous n'avez pas payé l'ISF l'an passé.
                              %s <br>Vous êtes exonéré de TH au titre de l'article 1417-I du CGI", phraseSeuil)
    
    phrase = ifelse(exonerations()$exoDOM, phraseExoDom, NA)
    phrase = ifelse(exonerations()$exoAspaAsi, phraseExoAspaAsi, phrase)
    phrase = ifelse(exonerations()$exoAAH, phraseExoAah, phrase)
    phrase = ifelse(exonerations()$exoIndigent, phraseExoIndigent, phrase)
    
    phrase = ifelse(exonerations()$exoAahRejet  & is.na(phrase), phraseExoAahRejet, phrase)
    phrase = ifelse(exonerations()$exoVeufSeniorRejetIsf  & is.na(phrase), phraseExoSeniorRejetIsf, phrase)
    phrase = ifelse(exonerations()$exoVeufSeniorRejetSeuil  & is.na(phrase), phraseExoSeniorRejet, phrase)
    phrase = ifelse(exonerations()$exoVeufSenior & is.na(phrase), phraseExoSenior, phrase)
    
    if (is.na(phrase)){
      phrase = "Votre situation personnelle ne vous permet pas d'être exonéré 
      de TH au titre de l'article 1417-I du CGI."
    }
    
    # Cas des logements vacants
    if (input$residence == 'vacant'){
      phrase = ifelse(entree()$tlv, "Pour les logements vacants, votre commune a opté pour une taxe spécifique
                      qui remplace la taxe d'habitation.",
                      "Votre logement est vacant, mais la taxe d'habitation est due.")
    }
    
    return(formatter_phrase(phrase))
    })
  
  output$valeurFinale = renderText({
    montantThFinal = calculPlafond()$montantThFinal
    montantThReclamme = ifelse(montantThFinal <= 12, 0, montantThFinal)
    phrase = sprintf("Le montant final de votre taxe d'habitation est de %s.", 
                     euro(calculPlafond()$montantThFinal, F))
    if (montantThFinal<=12 & montantThFinal >0){
      phrase = paste0(phrase, "<br>", "Ce montant étant inférieur à 12 €, il ne vous est pas réclammé.
                      Vous ne devez rien au titre de la taxe d'habitation.")
    }
    formatter_phrase(phrase)
  })
  
  output$warningPlafonnement = renderText({
    phrase = "Vous avez bénéficié du plafonnement de la taxe d'habitation. Cependant, en l'absence
    de la publication en open data de certaines données, il n'est pas possible de calculer une éventuelle
    réduction du dégrèvement accordé au titre du plafonnement. Le montant final de votre taxe d'habitation
    pourrait donc être supérieur à celui qui est ici affiché."
    formatter_phrase(phrase)
  })
  
  ########################
  # Tableau des totaux
  
  # Pour l'onglet taxe
  output$totaux = DT::renderDataTable({
    totaux = data.frame("Montant" = c(calculTH()$totalCotisations,
                                      calculTH()$totalFraisGestion,
                                     calculTH()$totalPrelevementBaseElevee,
                                     calculTH()$totalPrelevementResSecondaire,
                                    - calculPlafond()$degrevementApplique,
                                    - calculTH()$totalPrelevementBaseElevee*calculPlafond()$degrevementBaseElevee,
                                    calculPlafond()$montantThFinal))
    rownames(totaux) = c("Cotisations",
                         "Frais de gestion",
                         "Prélèvement pour base élevée",
                         "Prélèvement sur résidence secondaire",
                         "Dégrèvement lié au plafonnement",
                         "Dégrèvement prélèvement pour base élevée",
                         "Total")
    totaux$Montant[is.na(totaux$Montant)] = 0
    totaux$Montant = euro(totaux$Montant, F)
    datatable(totaux, options = list(dom = 't', "pageLength" = 40))
  })
  
  # Pour l'onglet plafonnement
  output$plafonnement = DT::renderDataTable({
    calcul = data.frame("Montant" = c(calculPlafond()$montantThPourPlafond,
                                      calculPlafond()$plafond,
                                      - calculPlafond()$degrevementCalcule,
                                      - calculTH()$totalPrelevementBaseElevee,
                                      calculPlafond()$montantThFinal))
    calcul$Montant = euro(calcul$Montant, F)
    rownames(calcul) = c("Taxe prise en compte pour le plafonnement", 
                         "Plafond, si éligible au plafonnement",
                         "Dégrèvement lié au plafonnement",
                         "Dégrèvement du prélèvement pour base élevée",
                         "Taxe après application du plafonnement")
    datatable(calcul, options = list(dom = 't', "pageLength" = 40))
  })
  

  ##############################################################################    
  ##############################################################################  
  ########## Onglet abattements
  
  output$vlNette <- renderText({
    phrase = "La base nette d'imposition (valeur locative nette) d'un bien immobilier 
    est calculée en soustrayant la somme des abattements à la valeur locative brute du bien.
    <br>Si ces abattements excèdent la valeur locative brute, la valeur locative nette est ramenée à 0 €.
    <br>Les collectivités à fiscalité propre peuvent fixer des abattements différents des abattements 
    communaux. Un abattement est soit un montant forfaitaire, soit un pourcentage de la valeur locative moyenne de la collectivité."
    return(formatter_phrase(phrase))
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
    explications[1] = ifelse(abattements[1] > 0,
      "La collectivité a voté un abattement qui s'applique à l'ensemble des foyers.",
      "La collectivité n'a pas voté d'abattement général à la base.")
    explications[2] = ifelse(abattements[2] > 0,
                             "Abattement pour chacune des deux premières personnes à charge.",
                             "La collectivité n'a pas voté d'abattement pour les deux premières personnes à charge.")
    explications[3] = ifelse(abattements[3] > 0,
                             "Abattement pour chaque personne à charge, au delà de 2.",
                             "La collectivité n'a pas voté d'abattement pour les 
                             personnes à charge au delà de 2.")
    explications[4] = ifelse(abattements[4] > 0,
                             sprintf("Abattement sous conditions : votre rfr ne doit pas dépasser %s et
                              la valeur locative de votre bien ne doit pas excéder %s.", 
                                     euro(seuils()$art1417_2, F), euro(vlMax, F)),
                             "La collectivité n'a pas voté d'abattement en faveur des personnes de condition modeste.")

    explications[5] = ifelse(abattements[5] > 0,
                             "Abattement applicable aux foyers dans lesquels une personne est en situation de handicap, bénéficiaire de l'ASI ou de l'AAH.",
                             "La collectivité n'a pas voté d'abattement en faveur des personnes handicapées ou invalides.")

    detailAbattements = data.frame(Abattements = euro(abattements, T),
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
  ##############################################################################  
  ########################### Onglet cotisation et frais de gestion
    
  output$expl_cotisations = renderText({
    phrase = "Base nette d'imposition x taux de cotisation voté par la collectivité."
    if (input$residence == 'secondaire'){
      phrase2 = "Dans le cas d'une résidence secondaire, les communes peuvent décider
      d'une majoration du taux de cotisation allant de 5% à 60%. 
      Si votre commune a voté une majoration, son taux est indiqué sur votre feuille d'imposition. 
      Entrez ce taux dans le menu de gauche pour adapter le taux communal de cotisation."
    phrase = paste0(phrase, '<br>', phrase2)
    }
    return(formatter_phrase(phrase))
  })
  output$detail_cotisations = DT::renderDataTable({
    datatable(calculTH()$detail[3:5,], 
              options = list(dom = 't', "pageLength" = 40))
  })
    
  output$expl_fraisGestion = renderText({
    phrase = "Cotisations x taux dépendant de la collectivité. 
    <br>L'Etat est chargé de la collecte de la taxe d'habitation et perçoit les
    frais de gestion. 
    <br>Les frais de gestion pour la commune et l'intercommunalité sont calculés en 
    sommant les cotisations avant application du taux."
    return(formatter_phrase(phrase))
  })   
  output$detail_fraisGestion = DT::renderDataTable({
    datatable(calculTH()$detail[5:7,], 
              options = list(dom = 't', "pageLength" = 40))
  })
  
  output$expl_majorationRsEtat = renderText({
    phrase = "L'Etat perçoit une majoration de cotisation dans le cas des résidences secondaires,
    correspondant à 1,5% des cotisations dues à la commune et aux EPCI à fiscalité propre."
    return(formatter_phrase(phrase))
  })   
  output$detail_majorationRsEtat = DT::renderDataTable({
    datatable(calculTH()$detail[c(5,10,11),], 
              options = list(dom = 't', "pageLength" = 40))
  })
  
  output$expl_majorationBaseElevee = renderText({
    phrase = "L'Etat perçoit une majoration de cotisation pour les résidences dont la
     locative nette communale dépasse 4 573 €. Pour les résidences principales, ce taux est de 2%.
    Pour les résidences secondaires dont la valeur locative nette est inférieure ou égale à 7 622 €, 
    il est 1,2%, 1,7% au dela.
    <br>Si vous bénéficiez du plafonnement de votre taxe, cette majoration n'est pas due."
    return(formatter_phrase(phrase))
  })   
  output$detail_majorationBaseElevee = DT::renderDataTable({
    datatable(calculTH()$detail[c(5,8,9),], 
              options = list(dom = 't', "pageLength" = 40))
  })
    
  ##############################################################################  
  ##############################################################################
  ########################### Onglet plafonnement
  output$plafondActif <- reactive({
    calculPlafond()$degrevementCalcule >0
  })
  outputOptions(output, "plafondActif", suspendWhenHidden = FALSE)


  output$explicationPlafonnement = renderText({
    
    # Cas où il n'y a pas de plafonnement
    if (entree()$rfr > seuils()$art1417_2) {
      phrase = sprintf("Votre revenu excèdant le seuil de %s (défini en fonction
                       de votre zone géographique et de la composition de votre foyer), vous ne bénéficiez
                       pas du plafonnement de votre taxe d'habitation", 
                       euro(seuils()$art1417_2, F))
    }
    
    if (input$isf) {
      phrase = "Il n'y a pas de plafonnement pour les ménages ayant payé l'ISF"
    }
    
    if (input$residence == 'secondaire') {
      phrase = "Il n'y a pas de plafonnement pour les résidences secondaires"
    }

    # Cas où le contribuable est éligible au plafonnement
    if (calculPlafond()$eligibilite){
      rfrAbattu = calculPlafond()$rfrAbattu
      plafond = calculPlafond()$plafond
      phrase = sprintf("Votre revenu fiscal de référence est de %s.
                       Le plafond est obtenu en calculant un revenu fiscal de référence, abattu d'un certain
                       montant tenant compte de votre département et du nombre de parts fiscales de votre foyer.
                       <br>Dans votre cas (département : %s, nombre de parts fiscales : %s), cet abattement est de %s, 
                       et le rfr abattu est alors de %s.
                       <br>Votre taxe d'habitation (cotisations + frais de gestion) ne doit pas excéder 3,44%% de 
                       ce rfr abattu, soit %s.",
                       euro(entree()$rfr, F), str_to_title(input$nomDepartement),  
                       entree()$nbParts,
                       euro(seuils()$art1414_A1, F), euro(rfrAbattu, F), euro(plafond, F))
    }

    return(formatter_phrase(phrase))

  })
  
  output$applicationPlafonnement = renderText({
    degrevementCalcule = calculPlafond()$degrevementCalcule
    degrevementBaseElevee = calculPlafond()$degrevementBaseElevee
    phrase = ''
    if (degrevementCalcule>0){
      phrase = sprintf("L'application du plafonnement vous permet de bénéficier
        d'un dégrèvement de cotisation de %s.", euro(degrevementCalcule, F))
      if (degrevementCalcule <8){
        phrase = paste(phrase, "<br>Ce dégrèvement étant de moins de 8 €, il n'est pas appliqué.")
      }
      if (degrevementBaseElevee){
        phrase = paste(phrase, "<br>De plus, vous êtes exonéré du prélèvement pour base élevée.")
        }
      }
    return(formatter_phrase(phrase))
  })
  
  
  
  ##############################################################################
  ########################### Onglet reforme 2018
  output$reforme2018 = renderText({
    phrase = "Pour l'année 2018, les ménages dont le revenu fiscal de référence est inférieur à un certain
    seuil bénéficient d'un dégrèvement pouvant atteindre 30% du montant total de leur taxe d'habitation."
    return(formatter_phrase(phrase))
  })
  
  output$calculReforme2018 = renderText({
    reforme2018 = calculer_reforme2018(entree()$rfr, entree()$nbParts, calculPlafond()$montantThFinal)
    print(reforme2018)
    if (reforme2018$taux == 0){
      phrase = sprintf("Votre revenu fiscal de référence est trop élevé pour bénéficier d'un dégrèvement.
      <br>Compte tenu du nombre de parts de votre foyer, un dégrèvement n'est accordé que pour
      un rfr inférieur à %s.", euro(seuils()$art1417_2bisb, F))
    }
    if (reforme2018$taux > 0){
      phrase = sprintf("Votre revenu fiscal de référence vous permet de bénéficier 
                       d'un dégrèvement de votre taxe d'habitation de %s%%, soit 
                       %s (taux et montant indicatifs).", 
                       round(100*reforme2018$taux), euro(reforme2018$degrevement, F))
    }
    return(formatter_phrase(phrase))
  })
} 



