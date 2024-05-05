classify_dataset <- function(dataset){
  taxonomy_columns <- c('type', 'subtype', 'class', 'group')
  
  structured_data <- dataset |>
    mutate(type='', subtype='', class='',  group='')
  
  leaves <- c('^C[^a-zA-Z]*', 'CM','CO','CI','CV','CK','CR','CH','CB', '^H[^a-z]*', '^L[^a-zA-Z]*', 'LL', '^E[^a-zA-Z]*', 'EH', 'EL', '^R[^a-zA-Z]*',
              '^K', 'Acapulcoite', '[Ll]odranite', 'Winonaite', 'Howardite', 'Eucrite', 'Diogenite',
              'Angrite', 'Aubrite', 'Ureilite', 'Brachinite', 'Lunar', 'Martian', 'Pallasite',
              'Mesosiderite', '[^I]IC', '[^I]IIAB', 'IIC', 'IID', '[^I]IIF', 'IIG', 'IIIAB', 'IIIE', 'IIIF',
              'IVA', 'IVB', '[^I]IAB', '[^I]IIE')
  
  group_names <- c('C', 'CM (Mighei-like)','CO (Ornans-like)','CI (Ivuna-like)','CV (Vigarano-like)','CK (Karoonda-like)',
                   'CR (Renazzo-like)','CH (Allan Hills-like)','CB (Bencubbin-like)', 'H', 'L', 'LL', 'E', 'EH', 'EL', 'R (Rumuruti-like)',
                   'K (Kakangari-like)', 'Acapulcoite', 'Lodranite', 'Winonaite', 'Howardite', 'Eucrite', 'Diogenite',
                   'Angrite', 'Aubrite', 'Ureilite', 'Brachinite', 'Lunar', 'Martian', 'Pallasite',
                   'Mesosiderite', 'IC', 'IIAB', 'IIC', 'IID', 'IIF', 'IIG', 'IIIAB', 'IIIE', 'IIIF',
                   'IVA', 'IVB', 'IAB', 'IIE')
  
  class_names <- c('Carbonaceous chondrite', 'Ordinary chondrite', 'Enstatite chondrite', 'Other chondrite',
                   'Primitive achondrites', 'Asteroidal achondrites', 'Lunar', 'Martian',
                   'Pallasite', 'Mesosiderite',
                   'Magmatic iron', "Non-magmatic (primitive) iron")
  
  class_ranges <- list(c(1,9), c(10,12), c(13,15), c(16,17),
                       c(18,20), c(21,27), c(28,28), c(29,29),
                       c(30,30), c(31,31),
                       c(32,42), c(43,44))
  
  subtype_names <- c('Chondrites', 'Achondrites', 'Pallasite', 'Mesosiderite',
                     'Magmatic', 'Non-magmatic')
  subtype_ranges <- list(c(1,17), c(18,29), c(30,30), c(31,31),
                         c(32,42), c(43,44))
  
  type_names <- c('Stony', 'Stony-iron', 'Iron')
  type_ranges <- list(c(1,29), c(30,31), c(32,44))
  
  get_label <- function(idx, labels, ranges) {
    for (r_i in 1:length(ranges)) {
      if (idx >= ranges[[r_i]][1] & idx <= ranges[[r_i]][2]) {
        return(labels[r_i])
      }
    }
  }
  
  get_taxonomy <- function(idx) {
    column_values <- c(
      get_label(idx, type_names, type_ranges),
      get_label(idx, subtype_names, subtype_ranges),
      get_label(idx, class_names, class_ranges),
      group_names[idx]
    )
    
    return(column_values)
  }
  
  get_parent_body <- function(group) {
    case_when(
      group %in% c('Howardite', 'Eucrite', 'Diogenite') ~ '4vesta',
      group == 'L' ~ '8flora',
      group == 'LL' ~ 'itokawa',
      group == 'H' ~ '6hebe',
      TRUE ~ NA
    ) 
  }
  
  for (i in 1:length(leaves)){
    leaf_mask <- grepl(leaves[i], structured_data$recclass)
    update_size <- sum(leaf_mask, na.rm=TRUE)
    taxonomic_labels <- get_taxonomy(i)
    
    update_df <- data.frame(type=rep(taxonomic_labels[1], update_size),
                            subtype=rep(taxonomic_labels[2], update_size),
                            class=rep(taxonomic_labels[3], update_size),
                            group=rep(taxonomic_labels[4], update_size))
    
    structured_data[leaf_mask, taxonomy_columns] <- update_df
  }
  
  structured_data <- structured_data |>
    mutate(
      parent_body = get_parent_body(group)
    )
  
  return(structured_data)
}
