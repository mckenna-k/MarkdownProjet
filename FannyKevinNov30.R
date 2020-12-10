library(shiny)
library(devtools)
library(tidyverse)
#devtools::install_github("mckenna-k/FinalProject")
library(KBFpackage)
#library(shinyWidgets)

#nb max de compétences
nbMax<-5

#niveau max pour slider input
M<-10


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
          radioButtons(
            "Langue",
            label = h3("Choisi une Langue"),
            choices = list("France" = "France", "USA" = "USA"),
            selected = "France"
          ),
          conditionalPanel(condition = "input.Langue == 'France'",
                           fileInput(
                             "photo", label = h3("Ajoute un photo de profile (.jpeg, .png, .pdf)")
                           )),


          uiOutput("Yphotos"),
          uiOutput("Ynom"),
          uiOutput("Yprenom"),
          uiOutput("Yadresse"),
          uiOutput("Ymail"),
          uiOutput("YCountryCode"),
          uiOutput("Ynumero"),
      ### PAYS
          #choix du pays
            # selectInput("Langue", label="Type" , choices=country),

      ### SKILLS
            # titre et boutons + -
            h3(textOutput("skillsTitle")),
            actionButton("plusS", label = "+"),
            actionButton("moinsS", label = "-"),
            hr(),

            # apparition des champs compétences en fonction du nb choisi
            lapply(1:nbMax, function(i) {
                conditionalPanel(
                    condition = paste0('output.nbSkills',i),
                    selectizeInput(paste0("sType",i), label=NULL, choices=NULL, selected=NULL),
                    textInput(paste0("sTxt",i),label = NULL),

                    # # choix du niveau de langue, si compétence choisi=Langue
                    # conditionalPanel(
                    #     condition=paste0('output.Langue',i),
                    #     shinyWidgets::sliderTextInput(paste0("levelLangue",i),"level",choices=c('A1','A2','B1','B2','C1','C2'))
                    # ),

                    # choix du niveau, si compétence choisi<> Permis
                    conditionalPanel(
                      condition=paste0('output.Level',i),
                      sliderInput(paste0("levelSkill",i),label=NULL,min=1,max=M,value=1)
                    )
                )
            }),

            #Alerte si le nb de compétence a dépassé le max autorisé
            conditionalPanel(
                condition = 'output.SkillsWarningCondition',
                verbatimTextOutput("SkillsWarning")),

      ### ACTIVITIES
            # titre et boutons + -
            h3(textOutput("actTitle")),
            actionButton("plusA", label = "+"),
            actionButton("moinsA", label = "-"),
            hr(),

            # apparition des champs activities en fonction du nb choisi
            lapply(1:nbMax, function(i) {
              conditionalPanel(
                condition = paste0('output.nbAct',i),
                textInput(paste0("aTxt1",i),label = "Type"),
                textInput(paste0("aTxt2",i),label = NULL),


                # choix du niveau si pays choisi=France
                conditionalPanel(
                  condition=paste0('output.Activities',i),
                  sliderInput(paste0("levelAct",i),"A quel point cette information vous caractérise ?",min=1,max=M,value=5)
                )
              )
            }),


      ### OTHER
            # titre et boutons + -
            h3(textOutput("otherTitle")),
            actionButton("plusO", label = "+"),
            actionButton("moinsO", label = "-"),
            hr(),

            # apparition des champs other en fonction du nb choisi
            lapply(1:nbMax, function(i) {
              conditionalPanel(
                condition = paste0('output.nbOther',i),
                textInput(paste0("oTxt1",i),label = "Type"),
                textInput(paste0("oTxt2",i),label = NULL)
              )
            })

        ), # fin side panel

        mainPanel(
          textOutput("nomComplet"),
          textOutput("adresseComplet"),
          textOutput("mailComplet"),
          textOutput("numeroComplet"),
      ### SKILLS
          # affichage skills
          conditionalPanel(
            condition = paste0('output.nbSkills',1),
            tableOutput("skillsTxt")
          ),

      ### ACTIVITIES
          # affichage other
          conditionalPanel(
            condition = paste0('output.nbAct',1),
            tableOutput("actTxt")
          ),

      ### OTHER
          # affichage other
          conditionalPanel(
            condition = paste0('output.nbOther',1),
            tableOutput("otherTxt")
          ),


          plotOutput("plot", width = "5%", height = "2px", inline=F),



      ### DOWNLOAD
          downloadButton("downloadData", "Download")

        )
    )
) # fin UI

server <- function(input, output, session) {
  output$Langue <- renderPrint({
    input$Langue
  })
  observe({
    if (input$Langue == "France" && !is.null(input$photo)) {
      output$plot <- renderImage({
        infile <- input$photo
        filename <- normalizePath(file.path(infile$datapath))
        list(src = filename, width="300", height="300")
      },
      deleteFile = F)
    }
  })

  language <- reactive({
    switch(input$Langue,
           "France" = "France",
           "USA" = "USA")
  })
  output$Ynom <- renderUI({
    textInput("nom", label = paysdf[[language()]][2], value = paysdf[[language()]][9])
  })
  output$Yprenom <- renderUI({
    textInput("prenom", label = paysdf[[language()]][3], value = paysdf[[language()]][10])
  })
  output$Yadresse <- renderUI({
    textInput("adresse", label = paysdf[[language()]][4], value = paysdf[[language()]][11])
  })
  output$Ymail <- renderUI({
    textInput("mail", label = paysdf[[language()]][5], value = paysdf[[language()]][12])
  })
  output$YCountryCode <- renderUI({
    textInput("countryCode", label = paysdf[[language()]][6], value = paysdf[[language()]][13])
  })
  output$Ynumero <- renderUI({
    textInput("tonNumero", label = paysdf[[language()]][7], value = paysdf[[language()]][14])
  })
#####
# Header
#####
  output$nomComplet <- renderText(
    KBFpackage::catNomComplet(input$Langue, input$nom, input$prenom)
  )
  output$adresseComplet <- renderText(
    input$adresse
  )
  output$mailComplet <- renderText(
    catMailComplet(input$mail, language())
  )
  output$numeroComplet <- renderText(
    catNumeroComplet(input$countryCode, input$tonNumero, language())
  )
### SKILLS
  # MAJ en fonction de la langue
    observeEvent(input$Langue,{

        # mise a jour du titre de la catégorie "skills" en fonction du pays choisi
        output$skillsTitle<-renderText({paste(cat["skills",input$Langue])})

        # mise a jour du label et de la liste de choix des champs "skills" en fonction du pays choisi
        lapply(1:nbMax, function(i) {
          updateSelectizeInput(session, paste0('sType',i),
                               choices = skills[,input$Langue],
                               label= paste0(cat["skills",input$Langue],i),
                               server = TRUE)
        })
        # mise a jour du titre niveau
        lapply(1:nbMax, function(i) {
          updateSliderInput(session, paste0('levelSkill',i),
                               label= paste0(message["level",input$Langue]))
        })
    }) # fin observeEvent

  # Création du texte à afficher (output$skillsTxt) et du texte à downloader (skillsTxt) pour la section compétence
   output$skillsTxt<-renderTable({skillsTxt()[-2]})
     skillsTxt<-function() {
        skillsPrintLength<-min(nbMax,input$plusS-input$moinsS)
            # data frame compétence : 1ligne=1competence
        if (skillsPrintLength>0) {
          skillsTxt<-data.frame()

          for (i in 1:skillsPrintLength) {
            skillsTxt[i,1]<-ifelse(input[[paste0("sTxt",i)]]!="",
                                        # catégorie de compétence
                                   paste0(input[[paste0("sType",i)]],
                                         " : ",
                                         # texte compétence
                                         input[[paste0("sTxt",i)]],
                                         # level compétence
                                         # ifelse(input[[paste0("sType",i)]] %in% skills["Langue",],
                                         #        paste(", ",input[[paste0("levelLangue",i)]]),""),
                                         ifelse(input[[paste0("sType",i)]] %in% as.matrix(skills[c("Prog","soft","other","Langue"),]),
                                                paste0(", ", message["level",input$Langue],input[[paste0("levelSkill",i)]]),"")
                                         )
                                   ,"")
            skillsTxt[i,2]<-paste0(cat["skills",input$Langue])
          }
          names(skillsTxt)<-c(paste0(cat["skills",input$Langue]),"type")
          skillsTxt

        }
     } # fin de creation du texte skills


    # condition d'appparition des champs compétences supplémentaires
    lapply(1:nbMax, function(i) {
        output[[paste0("nbSkills",i)]] <- reactive(input$plusS-input$moinsS>=i)
        outputOptions(output, paste0("nbSkills",i), suspendWhenHidden = FALSE)
    })

    # message max compétences & condition d'apparition du message
    output$SkillsWarning <- renderText({message["skills",input$Langue]})
    output$SkillsWarningCondition <- reactive(input$plusS-input$moinsS>=nbMax)
    outputOptions(output, "SkillsWarningCondition", suspendWhenHidden = FALSE)

    # # condition d'apparition du niveau en Langue
    # lapply(1:nbMax, function(i) {
    #     output[[paste0("Langue",i)]] <- reactive(input[[paste0("sType",i)]] %in% skills["Langue",] && input$plusS-input$moinsS>=i)
    #     outputOptions(output, paste0("Langue",i), suspendWhenHidden = FALSE)
    # })

    # condition d'apparition du niveau
    lapply(1:nbMax, function(i) {
      output[[paste0("Level",i)]] <- reactive(input[[paste0("sType",i)]] %in% as.matrix(skills[c("Prog","soft","other","Langue"),]) && input$plusS-input$moinsS>=i)
      outputOptions(output, paste0("Level",i), suspendWhenHidden = FALSE)
    })

### ACTIVITIES
    # MAJ langue
    observeEvent(input$Langue,{
      # mise a jour du titre de la catégorie "activities" en fonction du pays choisi
      output$actTitle<-renderText({paste(cat["act",input$Langue])})
    }) # fin observeEvent

    # Création du texte à afficher (output$actTxt) et du texte à downloader (actTxt)
    output$actTxt<-renderTable({actTxt()[-2]})
      actTxt<-function(){
      actPrintLength<-min(nbMax,input$plusA-input$moinsA)
      # data frame activities : 1ligne=1activities
      if (actPrintLength>0) {
        actTxt<-data.frame()

        for (i in 1:actPrintLength) {
          actTxt[i,1]<-ifelse(input[[paste0("aTxt1",i)]]!="",
                                # catégorie de activities
                                paste0(input[[paste0("aTxt1",i)]],
                                      " : ",
                                      input[[paste0("aTxt2",i)]],
                                      ifelse(input$Langue=="France",
                                             paste0(", ", message["level",input$Langue],input[[paste0("levelAct",i)]])
                                             ,"")
                                )
                              ,"")
          actTxt[i,2]<-paste0(cat["act",input$Langue])
        }
        names(actTxt)<-c(paste0(cat["act",input$Langue]),"type")
        actTxt

      }
    }

    # condition d'appparition des champs activities supplémentaires
    lapply(1:nbMax, function(i) {
      output[[paste0("nbAct",i)]] <- reactive(input$plusA-input$moinsA>=i)
      outputOptions(output, paste0("nbAct",i), suspendWhenHidden = FALSE)
    })

    # condition d'apparition du niveau
    lapply(1:nbMax, function(i) {
      output[[paste0("Activities",i)]] <- reactive(input$Langue=="France" && input$plusA-input$moinsA>=i)
      outputOptions(output, paste0("Activities",i), suspendWhenHidden = FALSE)
    })


### OTHER
  # MAJ langue
    observeEvent(input$Langue,{
        # mise a jour du titre de la catégorie "other" en fonction du pays choisi
        output$otherTitle<-renderText({paste(cat["other",input$Langue])})
    }) # fin observeEvent

    # Création du texte à afficher (output$otherTxt) et du texte à downloader (otherTxt)
    output$otherTxt<-renderTable({otherTxt()[-2]})
    otherTxt<-function(){
      otherPrintLength<-min(nbMax,input$plusO-input$moinsO)
      # data frame other : 1ligne=1other
      if (otherPrintLength>0) {
        otherTxt<-data.frame()

        for (i in 1:otherPrintLength) {
          otherTxt[i,1]<-ifelse(input[[paste0("oTxt1",i)]]!="",
                                 # catégorie de other
                                 paste(input[[paste0("oTxt1",i)]],
                                       " : ",
                                       input[[paste0("oTxt2",i)]])
                                 ,"")
          otherTxt[i,2]<-paste0(cat["other",input$Langue])
        }
        names(otherTxt)<-c(paste0(cat["other",input$Langue]),"type")
        otherTxt

      }
    }

    # condition d'appparition des champs other supplémentaires
    lapply(1:nbMax, function(i) {
      output[[paste0("nbOther",i)]] <- reactive(input$plusO-input$moinsO>=i)
      outputOptions(output, paste0("nbOther",i), suspendWhenHidden = FALSE)
    })

### DOWNLOAD
    wd<-getwd()
    titre<-paste("CV-",Sys.info()[["user"]],"-", Sys.Date())
    # download
    output$downloadData <- downloadHandler(
      # filename = function() {
      #   paste(wd,"/CV-",Sys.info()[["user"]],"-", Sys.Date(), '.csv', sep='')
      # },
      # content = function(file) {
      #   write.csv2(data.frame("type"=c(skillsTxt()[,2],actTxt()[,2],otherTxt()[,2]),"data"=c(skillsTxt()[,1],actTxt()[,1],otherTxt()[,1])), file, row.names = FALSE)
      # }
      filename = function() {
        paste(wd,"/CV-",Sys.info()[["user"]],"-", Sys.Date(), '.html', sep='')
      },
      content = function(file) {
        src <- normalizePath('CV.Rmd')

        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'CV.Rmd', overwrite = TRUE)

        library(rmarkdown)
        out <- render('CV.Rmd', html_document())
        file.rename(out, file)
      }
    )


} # fin server

shinyApp(ui, server)

