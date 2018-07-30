# TH

Cette application R-Shiny a pour objectif de détailler le calcul de la taxe d'habitation d'un foyer, à partir de ses informations personnelles (valeur locative brute, commune d'habitation, revenu fiscal de référence, nombre de parts fiscales, minima sociaux éventuellement perçus ...) et des éléments d'impositions relatifs à sa commune (abattements, majoration ...).

L'intégralité des éléments de fiscalité relatifs au lieu de l'habitation se trouvent dans le fichier REI-2017.csv, disponible à l'adresse https://www.data.gouv.fr/fr/datasets/r/249a166e-6a4e-4d2a-b3f4-00ef0ac40d2d, à l'exception du taux de majoration "résidence secondaire" voté par la commune (de 5% à 60%, taux voté par les communes ayant choisi d'appliquer une majoration) et du "taux consolidé 2000 global". Une personne souhaitant obtenir l'explication complète de sa propre taxe devra entrer ces deux valeurs, qu'elle trouvera sur son avis d'imposition. Lorsque ces données seront disponibles dans le REI, l'application sera modifiée pour en tenir compte.

L'application a été codée en ne tenant compte que de la documentation disponible à cette adresse :
https://www.impots.gouv.fr/portail/www2/fichiers/documentation/brochure/idl_2017/files/assets/common/downloads/publication.pdf, page 87-114

Des erreurs, notamment sur des cas particuliers peuvent subsister. 
N'hésitez pas à me contacter pour me les indiquer.
