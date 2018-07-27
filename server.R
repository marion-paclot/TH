server <- function(input, output, session) {
  
  entree = reactive({
    
    alloc = input$alloc
    situation = input$situation
    rfr = input$rfr
    isf = input$isf
    nbParts = as.numeric(gsub(',', '.', input$nbParts))
    nbPAC = as.numeric(gsub(',', '.', input$nbPAC))
    nomDep = input$nomDepartement
    nomCom = input$nomCommune
    vlBrute = input$VLBrute
    residence = input$residence
    ab_gph = input$ab_gph
    
    # Cela peut encore arriver quand l'utilisateur est en train de saisir une
    # valeur dans la liste déroulante
    if (nomCom == '') {
      nomCom = rei$LIBCOM[rei$LIBDEP == nomDep][1]
    }
    
    cat('Lancement des calculs pour', nomDep, '>', nomCom, '\n')
    
    # Code INSEE de la commune
    codeCom = subset(rei, LIBDEP == nomDep & LIBCOM == nomCom)$IDCOM
    reiCom = subset(rei, IDCOM == codeCom)
    vlCommune = reiCom$J21
    
    ## Zone géographique
    zoneGeo = "Métropole"
    zoneGeo = ifelse(nomDep == 'GUYANE', 'Guyane', zoneGeo)
    zoneGeo = ifelse(nomDep == 'MAYOTTE', 'Mayotte', zoneGeo)
    zoneGeo = ifelse(nomDep %in% c('GUADELOUPE', 'MARTINIQUE', 'LA REUNION'), 'Guadeloupe Martinique Réunion', zoneGeo)
    
    return(list(alloc = alloc, 
                situation = situation,
                isf = isf, 
                rfr = rfr, 
                nbParts = nbParts,
                nbPAC = nbPAC,
                zoneGeo = zoneGeo,
                residence = residence,
                codeCom = codeCom,
                vlBrute = vlBrute,
                vlCommune = vlCommune,
                reiCom = reiCom,
                ab_gph = ab_gph))

  }) 
  
  choix_commune = observe({
    departement = input$nomDepartement
    choix = subset(rei, LIBDEP == departement)
    # Pour éviter qu'un calcul soit lancé avec un couple (département, commune)
    # qui serait incohérent (nouveau département mais commune de l'ancien).
    freezeReactiveValue(input, 'nomCommune')
    updateSelectizeInput(session, 'nomCommune', 
                         choices = choix$LIBCOM,
                         selected = choix$LIBCOM[1],
                         server = FALSE)
  })
  
  ### Seuils divers d'exonération et d'abattement
  seuils = reactive({
    art1417_1 = calculer_seuil(grille_1417_1_CGI, entree()$zoneGeo, "2017", entree()$nbParts)
    art1417_2 = calculer_seuil(grille_1417_2_CGI, entree()$zoneGeo, "2017", entree()$nbParts)
    art1417_1bis = calculer_seuil(grille_1417_1bis_CGI, entree()$zoneGeo, "2017", entree()$nbParts)
    art1414_A1 = calculer_seuil(grille_1414_A1_CGI, entree()$zoneGeo, "2017", entree()$nbParts)
    
    return(list(art1417_1 = art1417_1,
                art1417_1bis = art1417_1bis,
                art1417_2 = art1417_2,
                art1414_A1 = art1414_A1))
  })
  
  ### Multiplicateur pour les abattements
  multiplicateur = reactive({
    coefficients = calculer_multiplicateur(entree()$nbPAC, entree()$rfr, 
                                             seuils()$art1417_1, 
                                             entree()$vlBrute, entree()$vlCommune, 
                                             entree()$situation, entree()$alloc)
    return(list(coefficients = coefficients))
  })
  
  abattements = reactive({
    commune = extraire_abattements(entree()$reiCom, nomColAbattementsCommune)
    syndicat = extraire_abattements(entree()$reiCom, nomColAbattementsSyndicat)
    interco = extraire_abattements(entree()$reiCom, nomColAbattementsInterco)
    TSE = extraire_abattements(entree()$reiCom, nomColAbattementsTSE)
    return(list(commune = commune, 
                syndicat = syndicat,
                interco = interco, 
                TSE = TSE))
  })
  
  cotisations = reactive({
    reiCom = entree()$reiCom
    vlBrute = entree()$vlBrute
    coefficient = multiplicateur()$coefficient
    
    commune = calculer_cotisations(reiCom, abattements()$commune, vlBrute, 
                                   coefficient, tauxCotisations$commune)
    syndicat = calculer_cotisations(reiCom, abattements()$syndicat, vlBrute,
                                    coefficient, tauxCotisations$syndicat)
    interco = calculer_cotisations(reiCom, abattements()$interco, vlBrute, 
                                   coefficient, tauxCotisations$interco)
    TSE = calculer_cotisations(reiCom, abattements()$TSE, vlBrute, coefficient, tauxCotisations$TSE)
    
    return(list(commune = commune, 
                syndicat = syndicat,
                interco = interco, 
                TSE = TSE))
  })
  
  frais = reactive({
    residence = entree()$residence
    commune = calculer_frais(cotisations()$commune, residence, tauxGestionCommune)
    syndicat = calculer_frais(cotisations()$syndicat, residence, tauxGestionSyndicat)
    interco = calculer_frais(cotisations()$interco, residence, tauxGestionInterco)
    TSE = calculer_frais(cotisations()$TSE, residence, tauxGestionTSE)
    
    return(list(commune = commune, 
                syndicat = syndicat,
                interco = interco, 
                TSE = TSE))
  })
  
  #### EXONERATION
  exonerations <- reactive({
    # Calcul du seuil d'exonération
    exoneration_1417_1 = entree()$rfr <= seuils()$art1417_1
    
    # Situation d'exonération totale
    exoDOM = exo_logement_modeste_DOM(entree()$zoneGeo, entree()$reiCom$J21, entree()$vlBrute)
    exoASPA_ASI = any(c('ASPA', 'ASI') %in% entree()$alloc)
    exoAAH = 'AAH' %in% entree()$alloc & exoneration_1417_1
    exoAAH_rejet = 'AAH' %in% entree()$alloc & ! exoneration_1417_1
    exoVeufSenior = any(c('Veuf', 'Senior') %in% entree()$situation) & ! entree()$isf & exoneration_1417_1 
    exoVeufSenior_rejetISF = any(c('Veuf', 'Senior') %in% entree()$situation) & entree()$isf
    exoVeufSenior_rejetSeuil = any(c('Veuf', 'Senior') %in% entree()$situation) & !exoneration_1417_1 
    
    assujeti = !any(exoDOM | exoASPA_ASI | exoAAH | exoVeufSenior)

    return(list(exoDOM = exoDOM, 
                exoASPA_ASI = exoASPA_ASI, 
                exoAAH = exoAAH,
                exoVeufSenior = exoVeufSenior,
                exoAAH_rejet = exoAAH_rejet,
                exoVeufSenior_rejetISF = exoVeufSenior_rejetISF,
                exoVeufSenior_rejetSeuil = exoVeufSenior_rejetSeuil,
                assujeti = assujeti))
  })
  
  output$assujeti <- reactive({
    exonerations()$assujeti
  })
  outputOptions(output, "assujeti", suspendWhenHidden = FALSE)
    
  
  
  output$exoneration_totale <- renderText({
    
    phraseExoPartielle =  "Dans le cas où vous bénéficiez d'une exonération partielle 
    au titre de l'article 1414-I bis 2° du CGI, la valeur locative brute de votre 
    bien tient compte de l'abattement auquel vous avez droit."
    
    phraseSeuil = sprintf("L'exonération est soumise à condition de revenu : 
                          Le RFR d'un foyer de %s part%s ne doit pas dépasser %s€.", 
                          entree()$nbParts, ifelse(entree()$nbParts>1, 's', ''), seuils()$art1417_1)
    
    if (exonerations()$exoDOM){
      phrase = "Vous habitez dans un département d'Outre-Mer et la valeur locative brute
      de votre logement est inférieure à 40% de la valeur locative moyenne de la commune.
      Vous êtes exonéré de TH au titre de l'article 332 de l'annexe II du CGI."
      phrase = gsub('\n', '', phrase)
      return(phrase)
    }

    if (exonerations()$exoASPA_ASI){
      phrase = "Vous percevez l'ASPA ou l'ASI. Aucune condition de revenu n'est requise.
      Vous êtes exonéré de TH au titre de l'article 1417-I du CGI."
      phrase = gsub('\n', '', phrase)
      return(phrase)
    }

    if (exonerations()$exoAAH){
      phrase = paste("Vous percevez l'AAH.", phraseSeuil, "Vous êtes exonéré de TH au titre de l'article 1417-I du CGI.")
      phrase = gsub('\n', '', phrase)
      return(phrase)
    }
    
    if (exonerations()$exoAAH_rejet){
      phrase = paste("Vous percevez l'AAH.", phraseSeuil, 
                     "Vous n'êtes pas exonéré de TH", phraseExoPartielle)
      phrase = gsub('\n', '', phrase)
      return(phrase)
    }
    
    if (exonerations()$exoVeufSenior_rejetISF){
      phrase = "Vous êtes veuf/veuve ou avez plus de 60 ans mais vous avez payé l'ISF l'an passé.
      Vous n'êtes pas exonéré de TH."
      phrase = paste(phrase, phraseExoPartielle)
      phrase = gsub('\n', '', phrase)
      return(phrase)
    }
    
    if (exonerations()$exoVeufSenior_rejetSeuil){
      phrase = paste("Vous êtes veuf/veuve ou avez plus de 60 ans et vous n'avez pas payé l'ISF l'an passé.", 
                     phraseSeuil, "Vous n'êtes pas exonéré de TH", phraseExoPartielle)
      phrase = gsub('\n', '', phrase)
      return(phrase)
    }
    
    if (exonerations()$exoVeufSenior){
      phrase = paste("Vous êtes veuf/veuve ou avez plus de 60 ans et vous n'avez pas payé l'ISF l'an passé.", 
                     phraseSeuil, "Vous êtes exonéré de TH au titre de l'article 1417-I du CGI")
      phrase = gsub('\n', '', phrase)
      return(phrase)
    }
    
    # Tous les autres cas
    phrase_autre = "Votre situation personnelle ne vous permet pas d'être exonéré 
    de TH au titre de l'article 1417-I du CGI."
    phrase_autre = paste(phrase_autre, phraseExoPartielle)
    phrase_autre = gsub('\n', '', phrase_autre)
    return(phrase_autre)
  })

  
  output$vlCommune <- renderText({
    vlCommune = entree()$reiCom$J21
    return(sprintf('Valeur locative brute moyenne de la commune : %s€', vlCommune))
  })
  
  output$vlBrute <- renderText({
    vlBrute = entree()$vlBrute
    return(sprintf('Valeur locative brute de votre bien : %s€', vlBrute))
  })
  
  calculTH  <- reactive({
    reiCom = entree()$reiCom
    vlBrute = entree()$vlBrute
    residence = entree()$residence
    coefficients = multiplicateur()$coefficients
    
    commune = detailler_calcul(reiCom, vlBrute, residence, 
                              abattements()$commune, coefficients, 
                              colTauxCotisations$commune, tauxGestion$commune, 'commune')
    syndicat = detailler_calcul(reiCom, vlBrute, residence, 
                               abattements()$syndicat, coefficients, 
                               colTauxCotisations$syndicat, tauxGestion$syndicat, 'syndicat')
    interco = detailler_calcul(reiCom, vlBrute, residence, 
                               abattements()$interco, coefficients, 
                               colTauxCotisations$interco, tauxGestion$interco, 'interco')
    TSE = detailler_calcul(reiCom, vlBrute, residence, 
                               abattements()$TSE, coefficients, 
                               colTauxCotisations$TSE, tauxGestion$TSE, 'TSE')
    
    
    detail = data.frame(Commune = commune, 
                        Syndicat = syndicat, 
                        Intercommunalité = interco,
                        TSE = TSE)
    detail[] <- lapply(detail, function(x) if(is.factor(x)) factor(x) else x)
    
    rownames(detail) = c('Valeur locative brute', 'Somme des abattements',
                         'Valeur locative nette', "Taux d'imposition",
                         "Cotisations",
                         "Taux de gestion", "Frais de gestion",
                         "Taux base élevée", "Cotisation base élevée",
                         "Taux résidence secondaire", "Cotisation résidence secondaire")
    
    totalCotisation = sum(as.numeric(detail["Cotisations",]))
    totalFrais = sum(as.numeric(detail["Frais de gestion",]))
    totalBaseElevee = sum(as.numeric(detail['Cotisation base élevée',]))
    totalResidenceSecondaire = sum(as.numeric(detail['Cotisation résidence secondaire',]))
    
    detail$Total = ''
    detail$Total[rownames(detail) == 'Cotisations'] = totalCotisation
    detail$Total[rownames(detail) == 'Frais de gestion'] = totalFrais
    detail$Total[rownames(detail) == 'Cotisation base élevée'] = totalBaseElevee
    detail$Total[rownames(detail) == 'Cotisation résidence secondaire'] = totalResidenceSecondaire
    
    # Calcul d'un éventuel dégrèvement
    totalTH = totalCotisation + totalFrais + totalBaseElevee + totalResidenceSecondaire
    degrevement = degrevement(entree()$rfr, entree()$zoneGeo, entree()$isf, entree()$nbParts)
    rfrDegreve = max(0,entree()$rfr - degrevement)
    plafondTH = round(0.0344*rfrDegreve,0)
    degrevement = ifelse(totalTH >plafondTH, totalTH - plafondTH, 0)
    
    # Application du degrevement
    totalTH_postDegrevement = max(0, totalTH-ifelse(degrevement<8,0, degrevement))
    totalTH_due = ifelse(totalTH_postDegrevement <= 12, 0, totalTH_postDegrevement)
    
    return(list(detail = detail, totalTH = totalTH, degrevement = degrevement,
                totalTH_postDegrevement = totalTH_postDegrevement, 
                totalTH_due = totalTH_due))
  })
  
  output$calcul <-  DT::renderDataTable({
    detail = calculTH()$detail
    detail = datatable(detail, options = list(dom = 't', "pageLength" = 40))
    return(detail)
  })
  
  output$plafond <- renderText({
    totalTH = calculTH()$totalTH
    degrevement = ifelse(calculTH()$degrevement<0, 0, calculTH()$degrevement)
    totalTH_postDegrevement = calculTH()$totalTH_postDegrevement
    if (degrevement == 0){
      phrase = sprintf("Vous ne bénéficiez pas d'un dégrèvement du montant final de la TH.
      Le montant final du au titre de la TH s'élève à %s€.", totalTH_postDegrevement)
    }
    if (degrevement > 0 & degrevement <8){
      phrase = sprintf("Vous bénéficiez d'un dégrèvement de %s€, mais celui-ci n'est pas
      appliqué lorsqu'il est inférieurà 8€. Le montant final du au titre de la TH
      s'élève à %s€.", degrevement, totalTH_postDegrevement)
    }

    if (degrevement >=8){
      phrase = sprintf("Vous bénéficiez d'un dégrèvement de %s€. Le montant final du au titre de la TH
                       s'élève à %s€.", degrevement, totalTH_postDegrevement)
    }
    if(totalTH_postDegrevement >0 & totalTH_postDegrevement <=12 ){
      phrase = paste(phrase, "Ce montant n'est pas réclammé lorsqu'il est inférieur à 12€.")
    }
    
    phrase = gsub('\n', '', phrase)
    return(phrase)
  })

  
  output$abattements <- DT::renderDataTable({
    coefficients = multiplicateur()$coefficients
    # Pour éviter l'erreur
    if (length(coefficients) != 5){
      coefficients = rep(0,5)
    }
    abattements = data.frame(Commune = euro(abattements()$commune),
                             Syndicat = euro(abattements()$syndicat),
                             Intercommunalité = euro(abattements()$interco),
                             TSE = euro(abattements()$TSE),
                             Multiplicateurs = paste('x', coefficients))

    rownames(abattements) = c("Général à la base", "PAC1-2", "PAC3", "Spécial", "Handicapé")
    abattements = datatable(abattements, options = list(dom = 't'))

    return(abattements)
  })

  
  output$cotisations <- DT::renderDataTable({
    cotisations = data.frame(Commune = cotisations()$commune,
                             Syndicat = cotisations()$syndicat,
                             Intercommunalité = cotisations()$interco,
                             TSE = cotisations()$TSE)

    rownames(cotisations) = c('Cotisations')
    cotisations = datatable(cotisations, options = list(dom = 't'))

    return(cotisations)
  })
  
  output$frais <- DT::renderDataTable({
    frais = data.frame(Commune = frais()$commune,
                             Syndicat = frais()$syndicat,
                             Intercommunalité = frais()$interco,
                             TSE = frais()$TSE)
    rownames(frais) = c('Frais de gestion')
    frais = datatable(frais, options = list(dom = 't'))
    
    return(frais)
  })
  
  output$cascadeAbattements = renderPlotly({
    print(entree()$ab_gph)
    ab_gph = entree()$ab_gph 
    coefficients = multiplicateur()$coefficients

    if (ab_gph == 'commune'){
      g = cascade_abattements(entree()$vlBrute, abattements()$commune, coefficients)
    }
    if (ab_gph == 'syndicat'){
      g = cascade_abattements(entree()$vlBrute, abattements()$syndicat, coefficients)
    }
    if (ab_gph == 'interco'){
      g = cascade_abattements(entree()$vlBrute, abattements()$interco, coefficients)
    }
    if (ab_gph == 'TSE'){
      g = cascade_abattements(entree()$vlBrute, abattements()$TSE, coefficients)
    }
    return(g)
  })
  
}