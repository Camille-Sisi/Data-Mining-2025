
library(shiny)
library(gt)

# Data mining


  
# App Shiny
ui <- fluidPage(

    # Titre application
    titlePanel("Caractéristiques des offres d'emplois en CDD et intérim en 2020 sur Paris et sa petite couronne"),

    # Définition du ou des wigdets
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "mois",
                      label = "Choix du mois de publication de l'offre",
                      choices = levels(OffresPE_2020$mois_publication),
                      selected = "janvier"),
          
          radioButtons(inputId = "dept",
                       label = "Choix du département :",
                       choices = levels(OffresPE_2020$location_departement), 
                       selected = "75")
        ),

        # Tableau et graphe montré à l'utilisateur
        mainPanel(
           
          gtOutput("tab"),

          br(),
          br(),
          
          plotOutput("barPlot")
          
        )
    )
)


server <- function(input, output) {

  output$tab <- render_gt({
    OffresPE_2020 %>% 
      filter(contractType %in% c("CDD", "MIS") & !is.na(contractDuration_value) & 
               !entrepriseSecteur_NAF21=="U" & location_departement == input$dept) %>% 
      count(contractType) %>% 
      pivot_wider(names_from = contractType, values_from = n) %>% 
      rename("Nombre de missions d'intérim"="MIS",
             "Nombre de CDD"="CDD") %>% 
      add_column("Durée moyenne des contrats, en jours" = round(mean(OffresPE_2020[OffresPE_2020$contractType %in% c("CDD", "MIS") & 
                                                                                     !is.na(OffresPE_2020$contractDuration_value), ]$contractDuration_value)),
                 "Durée médiane des contrats, en jours" = round(median(OffresPE_2020[OffresPE_2020$contractType %in% c("CDD", "MIS") & 
                                                                                       !is.na(OffresPE_2020$contractDuration_value), ]$contractDuration_value))) %>% 
      gt() %>%  
      fmt_number(columns= 1:2, sep_mark = " ", decimals = 0) %>% 
      tab_header(title=md("Nombre et durée des contrats de travail (CDD ou intérim) proposés dans les offres d'emploi de Pôle emploi en 2020 sur Paris et sa petite couronne"))
  })
  
  
  output$barplot <- renderPlot({
    OffresPE_2020 %>% 
      filter(contractType %in% c("CDD", "MIS") & !is.na(contractDuration_value) & 
               !entrepriseSecteur_NAF21=="U" & 
               mois_publication == input$mois & location_departement==input$dept) %>% 
      group_by(secteurs) %>% 
      summarise(moy_duree = round(mean(contractDuration_value))) %>% 
      arrange(desc(moy_duree)) %>% slice(1:5) %>% 
      ggplot() + aes(x = moy_duree, y = fct_reorder(secteurs, moy_duree))  + 
      geom_bar(stat="identity", fill="darkgoldenrod1")  +
      geom_text(aes(label=moy_duree), position = position_stack(vjust=0.5), size=4.5) +
      labs(title=paste("Top 5 des secteurs d'activité ayant des offres d'emploi en CDD \net missions d'intérim avec les durées moyennes en jours les \nplus élevées",
                       "en", input$mois, "dans le", input$dept), 
           x = "", y = " ", caption="Source : Base 'JOCAS', offres d'emploi émanant uniquement de Pôle emploi (auj. France Travail), 2020.") +
      theme_classic() +
      theme(legend.position = "none", plot.title=element_text(size=16),
            plot.caption=element_text(size=11),
            axis.text.y = element_text(size=12.5),
            axis.text.x = element_text(size=11))
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
