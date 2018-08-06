library(shiny)
library(shinyBS) # Additional Bootstrap Controls
library(DT)
library(ggplot2)
library(plotly)
library(Cairo)
library(plyr)

options(shiny.reactlog = T, stringsAsFactors = FALSE)

# Fichier de recensement des éléments d'imposition à la fiscalité directe locale
# Seule les colonnes relatives à la TH ont été conservées
rei = read.csv2('REI_TH.csv')
url_rei = "https://www.data.gouv.fr/fr/datasets/impots-locaux-fichier-de-recensement-des-elements-dimposition-a-la-fiscalite-directe-locale-rei-3/"

# Seuil d'éligibilité du CGI
grille_1417_1_CGI = read.csv2('seuils_1417-1-CGI.csv')
grille_1417_1bis_CGI = read.csv2('seuils_1417-1bis-CGI.csv')
grille_1417_2_CGI = read.csv2('seuils_1417-2-CGI.csv')
grille_1414_A1_CGI = read.csv2('seuils_1414-A-1-CGI.csv')

# Liens vers les articles du CGI
url_1417 = "https://www.legifrance.gouv.fr/affichCodeArticle.do?idArticle=LEGIARTI000027517723&cidTexte=LEGITEXT000006069577"
url_1414 = "https://www.legifrance.gouv.fr/affichCodeArticle.do?cidTexte=LEGITEXT000006069577&idArticle=LEGIARTI000037100266&dateTexte=&categorieLien=id"


################################################### 
# Valeurs d'abattements pour une collectivité territoriale.
colAbattements = list(commune = c('J51A', 'J31A', 'J41A', 'J61A', 'J61HA'), 
                      syndicat = c('J52A', 'J32A', 'J42A', 'J62A', 'J62HA'), 
                      interco = c('J53A', 'J33A', 'J43A', 'J63A', 'J63HA'), 
                      TSE = c('J51A', 'J31A', 'J41A', 'J61A', 'J61HA'),
                      GEMAPI = c('J51A', 'J31A', 'J41A', 'J61A', 'J61HA'))

colValeurLoc = list(commune = 'J21', 
                    syndicat = 'J22', 
                    interco = 'J23', 
                    TSE = 'J21',
                    GEMAPI = 'J21')

colTauxCotisations = list(commune = 'H12', 
                          syndicat = 'H22', 
                          interco = 'H32', 
                          TSE = c('H52', 'H52A'),
                          GEMAPI = c('H52cGEMAPI', 'H52gGEMAPI'))

tauxGestion = list(commune = c(0.01, 0.03),
                   syndicat = c(0.08, 0.08),
                   interco = c(0.01, 0.03),
                   TSE = c(0.09,0.09),
                   GEMAPI = c(0.03, 0.03))

tauxResidenceSecondaire = list(commune = 0.015,
                               syndicat = 0.015,
                               interco = 0,
                               TSE = 0,
                               GEMAPI = 0)

# Fixé arbitrairement en attendant les vraies valeurs
tauxMajorationResidenceSecondaire = 0.015


############ TOOLTIP
tooltip_PAC = "les enfants du contribuable ou ceux qu'il a recueillis, 
c'est-à-dire qui sont pris en compte dans le foyer fiscal pour le calcul 
de l'impôt sur le revenu au titre de l'année N-1 ;
les ascendants du contribuable âgés de plus de 70 ans ou infirmes 
quel que soit leur âge, résidant avec le contribuable et remplissant 
la condition de revenus (montant de leur revenu fiscal de référence (RFR) 
de l’année précédente n’excédant pas la limite prévue à l’article 1417-I du CGI)"




######## FONCTIONS

# Arrondi spécifique 0.5 --> 1
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Afficher € après un montant
euro = function(montant){
  montant = paste(montant, '€')
  montant = gsub('^0 €$', '-', montant)
  return(montant)
}

euro2 = function(montant){
  montant = paste(montant, '€')
  return(montant)
}

# Seuils au titre de l'article 1417 et 1414 du CGI
calculer_seuil = function (grille, zone, annee, nombreParts) {
  referentiel = subset(grille, Zone == zone & Annee == annee)
  
  partJusqua3 = min(3, nombreParts)
  valeurPartsJusqua3 = subset(referentiel, Parts == partJusqua3)$Seuil
  demiPartsAuDela3 = 2 * max(nombreParts - 3, 0)
  valeurDemiPartMarginale = subset(referentiel, Parts == 0.5)$Seuil
  
  seuil = valeurPartsJusqua3 + demiPartsAuDela3 * valeurDemiPartMarginale
  return(seuil)
}

afficher_seuil = function(tableSeuils, zone, annee, nombreParts){
  seuil_exo = calculer_seuil(tableSeuils, zone, annee, nombreParts)
  phraseSeuil = sprintf("Dans votre département, le seuil d'exonération pour un foyer de %s parts est de %s€.", nombreParts, seuil_exo)
}

# Logement modeste dans les DOM 
exo_logement_modeste_DOM = function(zone, vlBrute, reiCom){
  return(zone != 'Métropole' & vlBrute <= reiCom$L03)
}

################################################################################
# Fonctions pour le calcul étape par étape
calculer_multiplicateur = function(nombrePAC, rfr, seuil, vlBrute, situation, 
                                   alloc, reiCommune){

  vlMoyenne = reiCommune[,as.character(colValeurLoc)]
  general = 1
  PAC12 = min(2, nombrePAC)
  PAC3 = max(0, nombrePAC -2)
  special = as.numeric(rfr <= seuil &  vlBrute <= (1.3 + nombrePAC*0.1)*vlMoyenne) # Condition modeste + logement raisonnable
  handicape = as.numeric('Handicapé' %in% situation | 'ASI' %in% alloc |'AAH' %in% alloc)
  
  multiplicateur = rbind.data.frame(general, PAC12, PAC3, special, handicape)
  colnames(multiplicateur) = c('commune', 'syndicat', 'interco', 'TSE', 'GEMAPI')
  rownames(multiplicateur) = c('general', 'pac12', 'pac3', 'special', 'handicape')
  
  return(multiplicateur)
}

extraire_abattements = function(reiCommune, nomCol, typeRes){
  nomCol = colAbattements
  nomCol = as.character(unlist(nomCol))
  
  # Création des colonnes manquantes
  reiCommune[, setdiff(nomCol, colnames(reiCommune))] = 0
  abattements = data.frame(matrix(as.numeric(reiCommune[, nomCol]), nrow = 5))
  abattements = round2(abattements, 0)
  
  # Résidence secondaire --> Abattements non applicables
  if (typeRes == 'secondaire'){
    abattements = data.frame(ifelse(abattements>0, 0, 0))
  }
  colnames(abattements) = c('commune', 'syndicat', 'interco', 'TSE', 'GEMAPI')
  rownames(abattements) = c('general', 'pac12', 'pac3', 'special', 'handicape')
  
  return(abattements)
}

calculer_prelevement = function(assiette, taux){
  return(round2(assiette*taux,0))
}

calculer_taux_cotisation = function(colTaux, reiCom){
  valeur = c()
  for (i in 1:length(colTaux)){
    tx = sum(reiCom[, as.character(unlist(colTaux[i]))])
    valeur = c(valeur, tx/100)
  }
  return(valeur)
}

calculer_taux_gestion = function(typeResidence, taux){
  residencePS = c('principale', 'secondaire')
  valeur = sapply(taux,function(x) x[match(typeResidence, residencePS)])
  valeur = as.numeric(valeur)
  return(valeur)
}

calculer_taux_base_elevee = function(typeResidence, vlNette){
  tauxCommune = 0
  if (typeResidence == 'principale' & vlNette[1] > 4573){
    tauxCommune = 0.002
  }
  if (typeResidence == 'secondaire' & vlNette[1] > 4573 & vlNette[1] <= 7622){
    tauxCommune = 0.012
  }
  if (typeResidence == 'secondaire' & vlNette[1] > 7622){
    tauxCommune = 0.017
  }
  return(c(tauxCommune, 0, 0, 0, 0))
}

calculer_taux_residence_secondaire = function(typeResidence){
  return(ifelse(typeResidence == "secondaire", c(0.015, 0.015, 0, 0, 0), rep(0,5)))
}



calculer_plafonnement = function(rfr, zoneGeo, isf, nbParts, typeRes){
  if (isf |typeRes == 'secondaire'){
    return(NA)
  }
  seuil = calculer_seuil(grille_1417_2_CGI, zoneGeo, 2017, nbParts)
  abattement = ifelse(isf | rfr > seuil, 0, calculer_seuil(grille_1414_A1_CGI, zoneGeo, 2017, nbParts))
  
  th_max = round2(max(0,0.0344*(rfr - abattement)),0)
  return(th_max)
}

################################
detailler_calcul = function(nbPAC, rfr, seuil, vlBrute, situation, alloc, reiCommune,
                            nomColAbattements, typeRes, zoneGeo, isf, nbParts){
  
  # Abattements 
  multiplicateurs = calculer_multiplicateur(nbPAC, rfr, seuil, vlBrute, 
                                            situation, alloc, reiCommune)
  abattements = extraire_abattements(reiCommune, nomColAbattements, typeRes)
  totauxAbattements = colSums(abattements*multiplicateurs)
  basesNettes = pmax(0, vlBrute - totauxAbattements)
  
  # Cotisations par collectivité
  tauxCotisation = calculer_taux_cotisation(colTauxCotisations, reiCommune)
  cotisations = calculer_prelevement(basesNettes, tauxCotisation)


  # Frais de gestion
  # On doit calculer des v modifiée car le taux de 1% ou 3% s'applique au cumul 
  # des cotisation communes et interco
  tauxFraisGestion = calculer_taux_gestion(typeRes, tauxGestion)
  
  # Attention les frais sont calculés différemment
  cotisations_modif = cotisations
  cotisations_modif[1] = cotisations_modif[1] + cotisations_modif[3]
  tauxFraisGestion_modif = tauxFraisGestion
  tauxFraisGestion_modif[3] = 0
  fraisGestion = calculer_prelevement(cotisations_modif, tauxFraisGestion_modif)
  
  fraisGestion_affichage = fraisGestion
  fraisGestion_affichage[1] = paste("Com. + interco. =\n", fraisGestion_affichage[1])
  
  #####################
  
  # Prélèvement pour base elevée - applicable uniquement aux communes
  tauxBaseElevee = calculer_taux_base_elevee(typeRes, basesNettes)
  cotisationsBaseElevee = calculer_prelevement(basesNettes, tauxBaseElevee)
  
  # Prélèvement résidense secondaire
  tauxResSecondaire = calculer_taux_residence_secondaire(typeRes)
  cotisationsResSecondaire = calculer_prelevement(basesNettes, tauxResSecondaire)
  
  # Plafonnement
  plafond = calculer_plafonnement(rfr, zoneGeo, isf, nbParts, typeRes)

  detailCalcul = data.frame(
    'Valeur locative brute' = euro2(vlBrute), 
    'Abattements' = euro(totauxAbattements), 
    "Base nette d'imposition" = euro2(basesNettes), 
    "Taux d'imposition" = paste0(100*tauxCotisation, '%'), 
    "Cotisations" = euro(cotisations), 
    "Taux de gestion" = paste0(100*tauxFraisGestion, '%'), 
    "Frais de gestion" = euro(fraisGestion_affichage),
    "Taux de cotisation pour base élevée" = paste0(100*tauxBaseElevee, '%'), 
    "Cotisations pour base élevée" = euro(cotisationsBaseElevee),
    "Taux de cotisation pour résidence secondaire" = paste0(100*tauxResSecondaire, '%'), 
    "Frais de cotisation pour résidence secondaire" = euro(cotisationsResSecondaire)
    )
  detailCalcul = t(detailCalcul)
  rownames(detailCalcul) = gsub('\\.', ' ', rownames(detailCalcul))

  return(list(detailCalcul = detailCalcul,
              vlBrute = vlBrute, 
              multiplicateurs = multiplicateurs,
              abattements = abattements,
              totauxAbattements = totauxAbattements,
              basesNettes = basesNettes,
              tauxCotisation = tauxCotisation,
              cotisations = cotisations, 
              tauxFraisGestion = tauxFraisGestion,
              fraisGestion = fraisGestion,
              fraisGestion_affichage = fraisGestion_affichage,
              tauxBaseElevee = tauxBaseElevee,
              cotisationsBaseElevee = cotisationsBaseElevee,
              tauxResSecondaire = tauxResSecondaire,
              cotisationsResSecondaire = cotisationsResSecondaire,
              plafond = plafond))
}

# detailler_calcul(2, 20000, 13000, 3000, c(), c(), rei[120,], colAbattements, 
#                  "secondaire", "MAYOTTE", FALSE, 3)

cascade_abattements = function(vlBrute, abattements, multiplicateur){
  nom_abattements = c('GAB', 'PAC1-2', 'PAC3+', 'Special', 'Handicape')

  valeur = abattements*multiplicateur
  calcul = data.frame(
    etape = nom_abattements,
    abattements = abattements,
    multiplicateur = multiplicateur,
    valeur = valeur,
    start = c(vlBrute, vlBrute -cumsum(valeur[-length(valeur)])),
    end = c(vlBrute -cumsum(valeur)),
    type = 'abattement')

  calcul_net = calcul
  calcul_net$type = 'net'
  calcul_net$end = pmax(calcul_net$end, 0)
  calcul_net$start = 0

  calcul_vlb = data.frame(etape = 'Valeur locative brute', start = 0, end = vlBrute, type = 'total')
  calcul_vln = data.frame(etape = 'Base nette', start = 0, end = max(vlBrute-sum(valeur),0), type = 'total')

  calcul = rbind.fill(calcul_vlb, calcul, calcul_net, calcul_vln)
  calcul$etape = factor(calcul$etape,levels = unique(calcul$etape))
  calcul$id = as.numeric(calcul$etape)
  calcul$type = factor(calcul$type, levels = c('abattement','net'))

  calcul$tooltip = ifelse(calcul$type == "abattement",
                          paste(calcul$multiplicateur, 'x',calcul$abattements, '=', calcul$valeur, "€"),
                          paste('Valeur locative transitoire :', calcul$end, '€'))
  calcul$tooltip = ifelse(calcul$etape == "Valeur locative brute", paste('Valeur locative brute :', calcul$end, '€'), calcul$tooltip)
  calcul$tooltip = ifelse(calcul$etape == "Base nette", paste('Base nette :', calcul$end, '€'), calcul$tooltip)

  # Graphique
  g = ggplot(calcul, aes(etape, fill = type)) +
    geom_rect(aes(x = etape, text = tooltip,
                  xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start)) +
    geom_segment(aes(x = id - 0.45, xend = id + 0.45, y = 0, yend = 0, colour=type)) +
    xlab('') + ylab('Montant') 
  
  # Cas où la vl est nulle
  if (max(calcul$end) == 0){
    g = g + scale_y_continuous(breaks = c(0,100))
  }

  # Si abattements, affichage des valeurs
  if (any(valeur>0)){
    g = g + geom_text(data=subset(calcul, type == 'abattement' & valeur >0),
                      aes(x=id,y=(start+end)/2,label=paste('-',euro(start-end))))
  }

  g = ggplotly(g, tooltip = c('text'))
  g = hide_legend(g)
  return(g)
}