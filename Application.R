#Manipuler 
df <- read.csv2("RestaurantPaysdelaLoire.csv")
str(df)
df$Type.de.restaurant<-as.factor(df$Type.de.restaurant)
df$Commune<-as.factor(df$Commune)
df$Animal_accepté<-as.factor(df$Animal_accepté)
any(is.na(df$Commune))
str(df)
unique(df$Langue.s..parlée.s..à.l.accueil)


(langue<-c(unique(df$Langue.s..parlée.s..à.l.accueil)))
languages <- unlist(strsplit(langue, ","))  
languages <- trimws(languages)  
(unique_languages <- unique(languages))
df$longitude<-as.numeric(df$longitude)
df$latitude<-as.numeric(df$latitude)


#Application
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(stringr)
library(DT)
library(leaflet)
library(shinythemes)
library(bslib)


ui <- fluidPage(
  
  #lấy cách chọn từ người để chọn palette màu
  theme = bs_theme(
    version = 5,
    # Contrôle de la palette de gris par défaut
    bg = "#032820",  # Couleur de fond sombre et apaisante, idéale pour une interface moderne
    fg = "#C5C764",   # Couleur du texte en vert clair pour un bon contraste avec le fond sombre
    
    # Contrôle des couleurs d'accentuation (par exemple, les liens, les boutons, etc.)
    primary = "#80A416",  # Couleur principale en vert clair pour les éléments importants comme les boutons et liens
    secondary = "#08852C",  # Couleur secondaire en vert foncé pour les éléments secondaires
    
    # Police de caractères de base pour le texte
    base_font = c("Grandstander", "sans-serif"),  # Police moderne et lisible pour le texte principal
    
    # Police de caractères pour les blocs de code
    code_font = c("Courier", "monospace"),  # Police à espacement fixe pour afficher le code
    
    # Police de caractères pour les titres
    heading_font = "'Helvetica Neue', Helvetica, sans-serif",  # Police propre et professionnelle pour les titres
    
    # Personnalisation supplémentaire des éléments de l'interface
    "input-border-color" = "#AD9F3C",  # Couleur des bordures des champs de saisie, élégante mais visible
    "btn-border-color" = "#5E7343"  # Couleur de bordure des boutons pour un effet visuel agréable et cohérent
  ),
  
  
  # Titre de l'application
  titlePanel(
    div(
      style = "
      background-color: #032820;  /* Dark green background */
      color: #C5C764;            /* Light green text color */
      text-align: left;        /* Center-align the title */
      padding: 20px;             /* Add padding for better spacing */
      font-family: 'Grandstander', sans-serif;  /* Custom font for a modern look */
      font-size: 36px;           /* Increase font size for visibility */
      border-radius: 10px;       /* Rounded corners for the panel */
      border: 2px solid #80A416; /* Border with the primary color */
    ",
      "LOIREAT"  # Title text
    ),
    windowTitle = "LOIREAT.fr"  # Title for the browser tab
  ),
  p(HTML("Un clic, un choix : trouvez votre resto en Pays de la Loire")),
  
  # Tabset panel principal
  tabsetPanel(
    
    # Onglet "À propos de nous"
    tabPanel(
      title = "À propos de nous",
      
      # Main layout
      fluidRow(
        column(
          12,
          h3("Bienvenue sur notre application!", class = "text-center")  # Centered title
        )
      ),
      
      # Row for application description
      fluidRow(
        column(
          12,
          p("Cette application vous permet de rechercher des restaurants dans la région Pays de la Loire en fonction de divers critères tels que le département, la commune, la catégorie, et les langues parlées à l'accueil.",
            style = "margin-top: 20px; text-align: justify;"  # Add spacing and justify text
          )
        )
      ),
      
      # Row for video
      fluidRow(
        column(
          12,
          tags$video(
            src = "VidLogo.mp4",
            type = "video/mp4",
            controls = TRUE,
            style = "display: block; margin: 0 auto; max-width: 70%; height: auto;"  # Center the video
          )
        )
      ),
      
      
      
      # Row for image and credits
      fluidRow(
        column(
          6,
          img(src = "Logo_Loi (1).png", height = "300px", width = "300px", style = "display: block; margin: 0 auto;")  # Center the image in its column
        ),
        column(
          6,
          p(HTML("<strong>Créé par: Lou-Ann, Thanh Thao, Theturus et Binh Minh</strong>"),
            style = "margin-top: 100px; text-align: left; font-size: 18px;"  # Align text with the image
          )
        )
      )
    ),
    
    
    # Onglet "Recherche"
    tabPanel(
      title = "Recherche",
      sidebarLayout(
        sidebarPanel(
          h4("Filtres de recherche", class = "text-primary"),
          
          # Bouton guide d'utilisation
          actionButton("button2", "Guide d'utilisation"),
          
          # Sélection du département
          selectInput(
            inputId = "departement",
            label = "Choisissez le département",  # Labeled "Choisissez le département" (Chọn khu vực)
            choices = c("", unique(df$Département)),  # Ajouter une option vide pour ne pas avoir de valeur par défaut
            selected = NULL,  # Pas de valeur par défaut sélectionnée
            multiple = FALSE,  # Permet de choisir une seule valeur
            width = "100%"  # Élément qui occupe toute la largeur du panneau
          ),
          
          # Sélection de la commune (dynamique)
          uiOutput("commune_ui"),
          
          # Sélection de la catégorie de restaurant
          selectInput(
            "cate_restaurant", 
            "Quelle catégorie de restaurant ?", 
            choices = unique(df$Catégorie.du.restaurant),
            selected = unique(df$Catégorie.du.restaurant)[1],
            width = "100%"
          ),
          
          # Sélection des langues parlées
          awesomeCheckboxGroup(
            inputId = "Id001",
            label = "Choisissez les langues parlées à l'accueil", 
            choices = unique_languages,  # Assurez-vous que unique_languages est défini dans le serveur
            inline = FALSE,  # Mettre à FALSE pour que les cases à cocher soient affichées sur plusieurs lignes
            width = "100%"
          ),
          
          # Bouton pour lancer la recherche
          actionButton("button1", "Lancer la recherche")
        ),
        
        # Main panel qui affichera les résultats
        mainPanel(
          fluidRow(
            tabsetPanel(
              tabPanel("Liste des restaurants", DT::DTOutput("table")),
              tabPanel("Carte géographique", leafletOutput("mymap"))
            )
          )
        )
      )
    ),
    tabPanel("Connexion",
                textInput("prenom","Prenom: "),
                textInput("nom","Nom:"),
                textInput("password","Mot de passe:"),#valide()
                actionButton("inscire","S'inscire")
                )
  )
)





server <- function(input, output, session) {
  
  
  # Mettre à jour la liste des communes en fonction du département sélectionné
  observe({
    # Vérifier si input$departement est défini et non vide
    if (length(input$departement) > 0) {
      # Filtrer les communes qui appartiennent au département sélectionné
      communes_filtered <- df %>% filter(Département %in% input$departement) %>% pull(Commune)
      
      # Mettre à jour la liste des communes dynamiquement
      updateSelectInput(session, "commune", choices = unique(communes_filtered))
    }
  })
  
  # Rendre la liste des communes dynamique selon le département sélectionné
  output$commune_ui <- renderUI({
    # Assurez-vous que le département est sélectionné avant d'afficher les communes
    selectInput("commune", "Choisissez la commune",choices = NULL)  # Initialiser la liste des communes avec une valeur NULL
  })
  
  
  
  # Réactive pour filtrer les données en fonction des choix de l'utilisateur
  df1 <- eventReactive(input$button1,{
    filtered_df <- df  # Commencez avec les données complètes
    
    # Filtrer par les langues si des langues sont sélectionnées
    if (!is.null(input$Id001)) {
      # Si l'utilisateur a sélectionné "Tout", on ne filtre pas par langue
      if ("Tout" %in% input$Id001) {
        # Si "Tout" est sélectionné, ne pas appliquer de filtre sur les langues
        filtered_df <- filtered_df
      } else {
        # Si l'utilisateur n'a pas sélectionné "Tout", on filtre par les langues sélectionnées
        filtered_df <- filtered_df %>%
          filter(
            sapply(Langue.s..parlée.s..à.l.accueil, function(x) {
              any(str_detect(x, input$Id001))  # Vérifie si l'input correspond à l'une des langues sélectionnées
            })
          )
      }
    }
    
    
    
    
    
    # Filtrer par commune si sélectionnée
    if (!is.null(input$commune)) {
      filtered_df <- filtered_df %>%
        filter(Commune == input$commune)
    }
    
    # Filtrer par catégorie de restaurant si sélectionnée
    if (!is.null(input$cate_restaurant)) {
      filtered_df <- filtered_df %>%
        filter(Catégorie.du.restaurant == input$cate_restaurant)
    }
    
    return(filtered_df)  # Retourner les données filtrées
  })  # fin df1() reactive
  
  # Rendre le tableau avec les données filtrées
  output$table <- DT::renderDT({
    DT::datatable(
      df1(),
      options = list(
        
        columnDefs = list(
          list(visible = FALSE, targets = c(2,4,5,6,7,8,14,15,16,17,19,20))  # Cacher certaines colonnes si nécessaire
        )
      ),
      rownames = FALSE,  # Supprimer les numéros de ligne
      class = "display compact"  # Rendre le tableau plus compact
    )
  })
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -0.5, lat = 47.0, zoom = 7)
  })
  
  observe({
    # Récupérer les données filtrées depuis reactive
    filtered_data <- df1()
    
    # Vérifie s'il y a des données dans le dataframe filtré
    if (nrow(filtered_data) > 0) {
      # Ajoute les marqueurs sur la carte pour les données disponibles
      leafletProxy("mymap", data = filtered_data) %>%
        clearMarkers() %>%  # Supprime les anciens marqueurs de la carte
        addMarkers(
          lng = ~longitude, lat = ~latitude,  # Coordonnées des marqueurs
          popup = ~paste("<b>", Restaurant, "</b><br>", "Adresse: ", Adresse),  # Contenu du popup
          clusterOptions = markerClusterOptions()  # Active l'option de regroupement des marqueurs
          
        )
    } else {
      # Si aucune donnée n'est disponible, on nettoie les marqueurs de la carte
      leafletProxy("mymap") %>%
        clearMarkers()  # Supprime tous les marqueurs existants
      showNotification("No data available for the selected filters.")
    }
  })
  Guide<-"Veuillez d'abord choisir un département afin de pouvoir sélectionner une commune. Vous pouvez ensuite utiliser la carte pour voir la position géographique des restaurants. N'hésitez pas à zoomer ou dézoomer pour mieux explorer. Si vous souhaitez voir les résultats sur la carte, cliquez sur 'Lancer la recherche' pour mettre à jour les données."
  observeEvent(input$button2,{showModal(modalDialog(Guide))})
  
  
}


shinyApp(ui, server)


