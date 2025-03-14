
tableau <- function(data, ..., var_quali, pond=IPONDI, nom_var_quali) {
  
  tab <- data %>% 
    filter(...) %>% 
    count({{var_quali}}, wt={{pond}}) %>% 
    mutate(Pourcentage=round(prop.table(n)*100, 1)) %>% 
    adorn_totals("row") %>% 
    rename(Effectif=n, {{nom_var_quali}} := {{var_quali}})
  
  return(tab)
  
}


somme <- function(data, var_gpe, nom_var) {
  
  data %>% 
    group_by({{var_gpe}}) %>%
    count({{nom_var}}, wt=IPONDI) %>% 
    mutate(n=round(n)) %>% 
    pivot_wider(names_from = {{nom_var}}, values_from = n)
  
}


libelles_var <- function(data, cod_var, new_var){
  
  levels_var <- meta[meta$COD_VAR=={{ cod_var }}, ]$COD_MOD
  labels_var <- meta[meta$COD_VAR=={{ cod_var }}, ]$LIB_MOD
  data %>% mutate({{ new_var }} := factor(eval(parse(text={{ cod_var }})), 
                                          levels = levels_var, labels = labels_var))
  
}

