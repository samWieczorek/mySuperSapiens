zones <- data.frame(
  Min = c(0, 70, 90, 140, 250),
  Max = c(70, 90, 140, 250, 300),
  Info = c('Hypoglycemie',
    'Etat adaptatif', 
    'Recharge glucidique optimale', 
    'Etat d’inflammation latent', 
    'Hyperglycemie'),
  Color = c("#273b76", "#6832c0", "#567df2", "#c281c8", "#fd3a4c")
)
rownames(zones) <- LETTERS[seq(5)]

#! @export
zones


#' @export
GetZone <- function(value){
  zone <- NULL
  for (i in rownames(zones))
    if (value >= zones[i, 'Min'] && value < zones[i, 'Max'])
      zone <- i
  
  zone
}
