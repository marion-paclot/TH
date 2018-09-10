showModalRFR = function() {
  showModal(modalDialog(
    h4(
      "Il n'y a pas de revenu fiscal de référence sur votre avis d'imposition."
    ),
    h5(
      "C'est le cas lorsque vous habitez en colocation. Dans ce cas, afin de ne 
      pas révéler le revenu fiscal de vos colocataires, cette valeur n'est pas 
      remplie."
    ),
    h5("C'est aussi le cas du nombre de parts fiscales."),
    h5(
      "Pour effectuer une simulation et comprendre le calcul de la taxe 
      d'habitation, entrez des valeurs vraisemblables pour ces deux champs."
    ),
    easyClose = TRUE,
    footer = NULL
    ))
}
