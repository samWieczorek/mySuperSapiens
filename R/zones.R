#' @title zones
#' @export
Zones <- function() {
  zones <- data.frame(
  Min = c(0, 70, 90, 140, 250),
  Max = c(70, 90, 140, 250, 300),
  Info = c('Hypoglycemie',
    'Etat adaptatif', 
    'Recharge glucidique optimale', 
    'Etat d’inflammation latent', 
    'Hyperglycemie'),
  Color = c("#900C3F", "#FF5733", "#28b463", "#2980b9", "#1a5276")
)
rownames(zones) <- LETTERS[seq(5)]

#! @export
zones
}

#' @title Get zone
#' @export
GetZone <- function(value){
  zone <- NULL
  for (i in rownames(Zones()))
    if (value >= Zones()[i, 'Min'] && value < Zones()[i, 'Max'])
      zone <- i
  
  zone
}



#' @export
zones_HR <- function(){
  zones <- data.frame(
    Min = c(0, 137, 151, 166, 181),
    Max = c(137, 151, 166, 181, 196),
    Info = c('Récupération',
      'Endurance fondamentale', 
      'Endurance active', 
      'Seuil anaérobie', 
      'Puissance maximale anaérobie'),
    Color = c("#99a3a4", "#5dade2", "#2ecc71", "#f39c12", "#e74c3c")
  )
  rownames(zones) <- LETTERS[seq(5)]
  
  zones
}
  

#' @title Get zone
#' @export
GetZoneHR <- function(value){
  zone <- NULL
  for (i in rownames(zones_HR()))
    if (value >= zones_HR()[i, 'Min'] && value < zones_HR()[i, 'Max'])
      zone <- i
  
  zone
}
