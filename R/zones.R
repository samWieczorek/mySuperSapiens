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
