# Explication détaillée

# Cas où la valeur indiquée est déjà abattue
pExoPartielle = "<p>Dans le cas où vous bénéficiez d'une exonération partielle
au titre de l'article 1414-I bis 2° du CGI, la valeur locative brute de votre
bien tient compte de l'abattement auquel vous avez droit.
<br>Dans le cas où votre habitation se situe en zone de revitalisation rurale (ZRR) et que
la commune a décidé d'une exonération de taxe d'habitation pour les parties louées
à titre de meublé, la valeur locative brute de votre bien tient compte de l'abattement
auquel vous avez droit.</p>"


showTextExoneration = function (nbParts, seuil, exonerations, residence, tlv) {
  
  ## En tête
  enteteExplication = ifelse(exonerations$assujetti, 
                             "Vous êtes assujetti à la taxe d'habitation.",
                             "Vous êtes exonéré de taxe d'habitation.")
  
  pSeuil = sprintf("L'exonération est soumise à condition de revenu : 
  Le RFR d'un foyer de %s part%s ne doit pas dépasser %s.", 
  nbParts, ifelse(nbParts > 1, 's', ''), euro(seuil, F))
  
  pExoDom = "Vous habitez dans un département d'Outre-Mer et la valeur locative brute
  de votre logement est inférieure à 40% de la valeur locative moyenne de la commune.
  <br>Vous êtes exonéré de TH au titre de l'article 332 de l'annexe II du CGI."
  
  pExoIndigent = "<p>La commission communale des impôts directs vous a reconnu indigent.
  <br>Vous êtes exonéré de TH au titre de l'article 1408-II-3° du CGI."
  
  pExoAspaAsi = "Vous percevez l'ASPA ou l'ASI. Aucune condition de revenu n'est requise.
  <br>Vous êtes exonéré de TH au titre de l'article 1417-I du CGI."
  
  pExoAah = sprintf("Vous percevez l'AAH. %s <br>Vous êtes exonéré de TH au 
  titre de l'article 1417-I du CGI.</p>", pSeuil)
  
  pExoAahRejet = sprintf("Vous percevez l'AAH. %s Vous n'êtes pas exonéré de 
  TH.", pSeuil)
  
  pExoSeniorRejetIsf = sprintf("Vous êtes veuf/veuve ou avez plus de 60 ans 
  mais vous avez payé l'ISF l'an passé. <br>Vous n'êtes pas exonéré de TH.")
  
  pExoSeniorRejet = sprintf("Vous êtes veuf/veuve ou avez plus de 60 ans et 
  vous n'avez pas payé l'ISF l'an passé. %s <br>Vous n'êtes pas exonéré de TH.", 
  pSeuil)
  
  pExoSenior = sprintf("Vous êtes veuf/veuve ou avez plus de 60 ans et vous n'avez 
  pas payé l'ISF l'an passé.%s <br>Vous êtes exonéré de TH au titre de l'article 
  1417-I du CGI", pSeuil)
  
  
  phrase = ifelse(exonerations$exoDOM, pExoDom, NA)
  phrase = ifelse(exonerations$exoAspaAsi, pExoAspaAsi, phrase)
  phrase = ifelse(exonerations$exoAAH, pExoAah, phrase)
  phrase = ifelse(exonerations$exoIndigent, pExoIndigent, phrase)
  
  phrase = ifelse(exonerations$exoAahRejet  & is.na(phrase), pExoAahRejet, phrase)
  phrase = ifelse(exonerations$exoVeufSeniorRejetIsf  & is.na(phrase), pExoSeniorRejetIsf, phrase)
  phrase = ifelse(exonerations$exoVeufSeniorRejetSeuil  & is.na(phrase), pExoSeniorRejet, phrase)
  phrase = ifelse(exonerations$exoVeufSenior & is.na(phrase), pExoSenior, phrase)
  
  if (is.na(phrase)){
    phrase = "Votre situation personnelle ne vous permet pas d'être exonéré 
    de TH au titre de l'article 1417-I du CGI."
  }
  
  # Cas des logements vacants
  if (residence == 'vacant'){
    phrase = ifelse(tlv, "Pour les logements vacants, votre commune a opté pour une taxe spécifique
                    qui remplace la taxe d'habitation.",
                    "Votre logement est vacant, mais la taxe d'habitation est dûe.")
  }
  
  ## Conclusion
  pPedagogie = "A titre pédagogique, l'application détaille le calcul de la taxe
  d'habitation si vous n'en aviez pas été exonéré."
  
  
  explication = sprintf("<h4>%s</h4><p>%s</p>", enteteExplication, phrase)
  if (!exonerations$assujetti & residence != 'vacant'){
    explication = sprintf("%s<p>%s</p>", explication, pPedagogie)
  }

  return(HTML(explication))
}
