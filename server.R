server <- function(input, output, session) {
  
  # observeEvent(input$calcul_cell_clicked, {
  #   cellule = data.frame(input$calcul_cell_clicked)
  #   print(cellule)
  #   if (nrow(cellule) >0){
  #     if (cellule$value == "Abattements"){
  #       print("ok")
  #       updateTabsetPanel(session, "Simulation", selected = 'Valeur loc nette')
  #     }
  #   }
  # }) 
  
  entree = reactive({
    nomDep = input$nomDepartement
    nomCom = input$nomCommune
    nbParts = as.numeric(gsub(',', '.', input$nbParts))
    nbPAC = as.numeric(gsub(',', '.', input$nbPAC))

    # Figer le nom de la commune tant qu'elle n'est pas choisie
    if (nomCom == '') {
      nomCom = rei$LIBCOM[rei$LIBDEP == nomDep][1]
    }
    
    cat('Lancement des calculs pour', nomDep, '>', nomCom, '\n')
    
    # Code INSEE de la commune
    codeCom = subset(rei, LIBDEP == nomDep & LIBCOM == nomCom)$IDCOM
    reiCom = subset(rei, IDCOM == codeCom)

    ## Zone géographique
    zoneGeo = "Métropole"
    zoneGeo = ifelse(nomDep == 'GUYANE', 'Guyane', zoneGeo)
    zoneGeo = ifelse(nomDep == 'MAYOTTE', 'Mayotte', zoneGeo)
    zoneGeo = ifelse(nomDep %in% c('GUADELOUPE', 'MARTINIQUE', 'LA REUNION'), 'Guadeloupe Martinique Réunion', zoneGeo)
    
    return(list(nbParts = nbParts,
                nbPAC = nbPAC,
                zoneGeo = zoneGeo,
                codeCom = codeCom,
                # vlCommune = vlCommune,
                reiCom = reiCom))

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
  multiplicateurs = reactive({
    
    commune = calculer_multiplicateur(entree()$nbPAC, input$rfr, seuils()$art1417_1, 
                                      input$vlBrute, input$situation, input$alloc, 
                                      entree()$reiCom, 'commune')
    interco = calculer_multiplicateur(entree()$nbPAC, input$rfr, seuils()$art1417_1, 
                                      input$vlBrute, input$situation, input$alloc, 
                                      entree()$reiCom, 'interco')
    syndicat = calculer_multiplicateur(entree()$nbPAC, input$rfr, seuils()$art1417_1, 
                                      input$vlBrute, input$situation, input$alloc, 
                                      entree()$reiCom, 'syndicat')
    TSE = calculer_multiplicateur(entree()$nbPAC, input$rfr, seuils()$art1417_1, 
                                      input$vlBrute, input$situation, input$alloc, 
                                      entree()$reiCom, 'TSE')
    GEMAPI = calculer_multiplicateur(entree()$nbPAC, input$rfr, seuils()$art1417_1, 
                                  input$vlBrute, input$situation, input$alloc, 
                                  entree()$reiCom, 'GEMAPI')
    

    return(list(commune = commune, 
                syndicat = syndicat, 
                interco = interco,
                TSE = TSE,
                GEMAPI = GEMAPI))
  })
  
  abattements = reactive({
    commune = extraire_abattements(entree()$reiCom, colAbattements$commune)
    syndicat = extraire_abattements(entree()$reiCom, colAbattements$syndicat)
    interco = extraire_abattements(entree()$reiCom, colAbattements$interco)
    TSE = extraire_abattements(entree()$reiCom, colAbattements$TSE)
    GEMAPI = extraire_abattements(entree()$reiCom, colAbattements$GEMAPI)
    
    return(list(commune = commune, 
                syndicat = syndicat,
                interco = interco, 
                TSE = TSE, 
                GEMAPI = GEMAPI))
  })
  
  cotisations = reactive({
    reiCom = entree()$reiCom
    vlBrute = input$vlBrute

    commune = calculer_cotisations(reiCom, abattements()$commune, vlBrute, 
                                   multiplicateurs()$commune, tauxCotisations$commune)
    syndicat = calculer_cotisations(reiCom, abattements()$syndicat, vlBrute,
                                    multiplicateurs()$syndicat, tauxCotisations$syndicat)
    interco = calculer_cotisations(reiCom, abattements()$interco, vlBrute, 
                                   multiplicateurs()$interco, tauxCotisations$interco)
    TSE = calculer_cotisations(reiCom, abattements()$TSE, vlBrute, 
                               multiplicateurs()$TSE, tauxCotisations$TSE)
    GEMAPI = calculer_cotisations(reiCom, abattements()$GEMAPI, vlBrute, 
                              multiplicateurs()$GEMAPI, tauxCotisations$GEMAPI)
    
    return(list(commune = commune, 
                syndicat = syndicat,
                interco = interco, 
                TSE = TSE,
                GEMAPI = GEMAPI))
  })
  
  frais = reactive({
    residence = input$residence
    commune = calculer_frais(cotisations()$commune, residence, tauxGestion$commune)
    syndicat = calculer_frais(cotisations()$syndicat, residence, tauxGestion$syndicat)
    interco = calculer_frais(cotisations()$interco, residence, tauxGestion$interco)
    TSE = calculer_frais(cotisations()$TSE, residence, tauxGestion$TSE)
    GEMAPI = calculer_frais(cotisations()$GEMAPI, residence, tauxGestion$GEMAPI)
    
    return(list(commune = commune, 
                syndicat = syndicat,
                interco = interco, 
                TSE = TSE,
                GEMAPI = GEMAPI))
  })
  
  #### EXONERATION
  exonerations <- reactive({
    # Calcul du seuil d'exonération
    exoneration_1417_1 = input$rfr <= seuils()$art1417_1
    
    # Situation d'exonération totale
    exoDOM = exo_logement_modeste_DOM(entree()$zoneGeo, input$vlBrute, entree()$reiCom)
    exoASPA_ASI = any(c('ASPA', 'ASI') %in% input$alloc)
    exoAAH = 'AAH' %in% input$alloc & exoneration_1417_1
    exoAAH_rejet = 'AAH' %in% input$alloc & ! exoneration_1417_1
    exoVeufSenior = any(c('Veuf', 'Senior') %in% input$situation) & ! input$isf & exoneration_1417_1 
    exoVeufSenior_rejetISF = any(c('Veuf', 'Senior') %in% input$situation) & input$isf
    exoVeufSenior_rejetSeuil = any(c('Veuf', 'Senior') %in% input$situation) & !exoneration_1417_1 
    
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
    bien tient compte de l'abattement auquel vous avez droit.
    Dans le cas où votre habitation se situe en zone de revitalisation rurale (ZRR) et que
    la commune a décidé d'une exonération de taxe d'habitation pour les parties louées
    à titre de meublé, la valeur locative brute de votre bien tient compte de l'abattement
    auquel vous avez droit."
    
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

  # output$myList <- renderUI(HTML("<ul><li>...text...</li><li>...more text...</li></ul>"))
  
  
  output$vlNette <- renderText({
    phrase = "La valeur locative nette d'un bien immobilier correspond à la valeur 
        locative brute à laquelle on soustrait la somme des abattements. 
        Si ces abattements excèdent la valeur locative brute, la valeur locative nette est ramenée à 0.
    Chaque collectivité territoriale fixe le montant des 5 abattements."
  return(phrase)
    })
  
  calculTH  <- reactive({
    reiCom = entree()$reiCom
    vlBrute = input$vlBrute
    residence = input$residence

    commune = detailler_calcul(reiCom, vlBrute, residence, 
                              abattements()$commune, multiplicateurs()$commune, 
                              colTauxCotisations$commune, tauxGestion$commune, 'commune')
    syndicat = detailler_calcul(reiCom, vlBrute, residence, 
                               abattements()$syndicat, multiplicateurs()$syndicat, 
                               colTauxCotisations$syndicat, tauxGestion$syndicat, 'syndicat')
    interco = detailler_calcul(reiCom, vlBrute, residence, 
                               abattements()$interco, multiplicateurs()$interco, 
                               colTauxCotisations$interco, tauxGestion$interco, 'interco')
    TSE = detailler_calcul(reiCom, vlBrute, residence, 
                               abattements()$TSE, multiplicateurs()$TSE, 
                               colTauxCotisations$TSE, tauxGestion$TSE, 'TSE')
    GEMAPI = detailler_calcul(reiCom, vlBrute, residence, 
                           abattements()$GEMAPI, multiplicateurs()$GEMAPI, 
                           colTauxCotisations$GEMAPI, tauxGestion$GEMAPI, 'GEMAPI')
    
    
    detail = data.frame(Commune = commune, 
                        Syndicat = syndicat, 
                        Intercommunalité = interco,
                        TSE = TSE, 
                        GEMAPI = GEMAPI)
    detail[] <- lapply(detail, function(x) if(is.factor(x)) factor(x) else x)
    
    rownames(detail) = c('Valeur locative brute', 'Abattements',
                         "Base nette d'imposition", "Taux d'imposition",
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
    degrevement = degrevement(input$rfr, entree()$zoneGeo, input$isf, entree()$nbParts)
    rfrDegreve = max(0,input$rfr - degrevement)
    plafondTH = round(0.0344*rfrDegreve,0)
    
    
    degrevement = ifelse(totalTH >plafondTH, totalTH - plafondTH, 0)
    
    # Application du degrevement
    totalTH_postDegrevement = max(0, totalTH-ifelse(degrevement<8,0, degrevement))
    totalTH_due = ifelse(totalTH_postDegrevement <= 12, 0, totalTH_postDegrevement)
    
    # Réduction du dégrèvement
    # Cas de mayotte a traiter ultérieurement
    tauxGlobalEnCours = sum(entree()$reiCom[,unlist(colTauxCotisations)])
    taux2000corrige = 0.22
    baseNetteMin = as.numeric(min(commune[3], interco[3], syndicat[3], TSE[3]))
    reductionDegrevement = baseNetteMin*(tauxGlobalEnCours - taux2000corrige*1.034)
    reductionDegrevementApplique = ifelse(reductionDegrevement>15, reductionDegrevement, 0)
    
    degrevementPostReduction = min(0, degrevement - reductionDegrevementApplique)
    
    
    return(list(detail = detail, 
                totalTH = totalTH, 
                degrevement = degrevement,
                totalTH_postDegrevement = totalTH_postDegrevement, 
                totalTH_due = totalTH_due))
  })
  

  output$calcul = DT::renderDataTable(datatable(calculTH()$detail, options = list(dom = 't', "pageLength" = 40)), 
                                      selection = "single", server = FALSE)
  
  
  # # Basculer sur un autre onglet
  # observeEvent(input$datatable_cell_clicked, {
  #   cellule = data.frame(input$datatable_cell_clicked)
  #   print(cellule)
  #   if (nrow(cellule) >0){
  #     if (cellule$value == "Valeur locative nette"){
  #       updateTabsetPanel(session, "mainPanel", selected = "Valeur loc nette")
  #     }
  #   }
  # }) 
  
  
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
    ct = input$ab_gph
    
    multiplicateur = unlist(multiplicateurs()[ct])
    abattements = unlist(abattements()[ct])
    explications = rep(NA, 5)
    
    # Adaptation des explications : 
    explications[1] = ifelse(abattements[1] > 0,
      "La collectivité territoriale a voté un abattement qui s'applique à l'ensemble des foyers",
      "La collectivité n'a pas voté d'abattement général")
    explications[2] = "Abattement applicable pour chacune des deux premières personnes à charge."
    explications[3] = "Abattement applicable pour chaque personne à charge, au delà de 2"
    explications[4] = "Applicable aux foyers dont le rfr ne dépasse pas un certain seuil, et dont la valeur locative du logement 
    ne dépasse pas une certaine proportion de la valeur locative moyenne de la collectivité territoriale."
    explications[5] = "Applicable aux foyers dans lesquels une personne est en situation de handicap, bénéficiaire de l'ASI ou de l'AAH"
    
    
    detailAbattements = data.frame(Abattements = euro(abattements),
                             Multiplicateur = paste('x', multiplicateur),
                             Explication = explications)
    rownames(detailAbattements) = c("Général à la base", "PAC1-2", "PAC3", "Spécial", "Handicapé")
    detailAbattements = datatable(detailAbattements, options = list(dom = 't'))

    return(detailAbattements)
  })


  output$cotisations <- DT::renderDataTable({
    cotisations = data.frame(Commune = cotisations()$commune,
                             Syndicat = cotisations()$syndicat,
                             Intercommunalité = cotisations()$interco,
                             TSE = cotisations()$TSE,
                             GEMAPI = cotisations()$GEMAPI)

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
    ct = input$ab_gph
    
    multiplicateur = unlist(multiplicateurs()[ct])
    abattements = unlist(abattements()[ct])
    
    g = cascade_abattements(input$vlBrute, abattements, multiplicateur)
    return(g)
  })
  
}