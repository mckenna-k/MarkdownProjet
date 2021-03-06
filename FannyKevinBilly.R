library(shiny)
library(devtools)
library(tidyverse)
library(KBFpackage)
#devtools::install_github("mckenna-k/FinalProject")
#library(shinyWidgets)

#nb max de compétences
nbMax<-5

#niveau max pour slider input
M<-10


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
          #Creation du widget Radio Button pour savoir quel format de CV.  La reponse va etre utiliser dans les autre widgets pour la langue
          radioButtons(
            "Langue",
            label = h3("Choisi une Langue"),
            choices = list("France" = "France", "USA" = "USA"),
            selected = "France"
          ),
          #Creation du widget fileInput pour ajouter un photo dans le cas ou le format sera celle de la France
          conditionalPanel(condition = "input.Langue == 'France'",
                           fileInput(
                             "photo", label = h3("Ajoute un photo de profile (.jpeg, .png, .pdf)")
                           )),

          #Creation des markeurs pour ajouter les inputs qui sont sur le cote serveur pour que c'est moins lourde cote UI
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

      ### OBJECTIF
      textInput(
        "user_text_Obj",
        label = h3("Objectif"),
        value = " "
      ),
      hr(),

      ### EDUCATION
      # titre et boutons + -
      h3(textOutput("eduTitle")),
      actionButton("plusEdu", label = "+"),
      actionButton("moinsEdu", label = "-"),
      hr(),

      # apparition des champs activities en fonction du nb choisi
      lapply(1:nbMax, function(i) {
        conditionalPanel(
          condition = paste0('output.nbEdu',i),
          textInput(paste0("eduTxt1",i),label = "School"),
          textInput(paste0("eduTxt2",i),label = "Degree"),
          textInput(paste0("eduTxt3",i),label = "Periode (format mm/yyyy)"),


          # choix du niveau si pays choisi=France
          conditionalPanel(
            condition=paste0('output.Education',i),
            sliderInput(paste0("levelEdu",i),"Estimer le résultat:(1-Mauvais->10-Excellent)",min=1,max=10,value=5)
          )
        )
      }),

      ### WORK EXPERIENCE
      # titre et boutons + -
      h3(textOutput("weTitle")),
      actionButton("plusWE", label = "+"),
      actionButton("moinsWE", label = "-"),
      hr(),

      # apparition des champs activities en fonction du nb choisi
      lapply(1:nbMax, function(i) {
        conditionalPanel(
          condition = paste0('output.nbWE',i),
          textInput(paste0("weTxt1",i),label = "Company"),
          textInput(paste0("weTxt2",i),label = "Task "),
          textInput(paste0("weTxt3",i),label = "Periode (format mm/yyyy)")


          # choix du niveau si pays choisi=France
          # conditionalPanel(
          #   condition=paste0('output.WorkExperience',i),
          #   sliderInput(paste0("levelWE",i),"Estimer le résultat:(1-Mauvais->10-Excellent)",min=1,max=10,value=5)
          # )
        )
      }),

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
      ### HEADER
          #affichage des champs pour le header
          textOutput("nomComplet"),
          textOutput("adresseComplet"),
          textOutput("mailComplet"),
          textOutput("numeroComplet"),

          ### OBJECTIF
          #uiOutput("user_text_obj"),
          textOutput("user_text"),

          ### EDUCATION
          # affichage education
          conditionalPanel(
            condition = paste0('output.nbEdu',1),
            tableOutput("eduTxt")
          ),



          ### WORK EXPERIENCE
          # affichage work exp.
          conditionalPanel(
            condition = paste0('output.nbWE',1),
            tableOutput("weTxt")
          ),

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

      ###Affichage du photo.  En bas pour qu'il ne cache pas les autres champs
          plotOutput("plot", width = "5%", height = "2px", inline=F),



      ### DOWNLOAD
          downloadButton("downloadData", "Download")

        )
    )
) # fin UI

server <- function(input, output, session) {
  #Rend la langue.  Finalement on ne l'affiche pas
  output$Langue <- renderPrint({
    input$Langue
  })
  #observe le input du Langue.  Si c'est "France", on ajoute le photo, sinon on fait rien
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
  #Prend en compte la langue pour que tout les textInputs sont dans la bonne langue
  language <- reactive({
    switch(input$Langue,
           "France" = "France",
           "USA" = "USA")
  })
  #Affichage des champs a remplire pour le header. Label donne la question et value donne un exemple.
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
  #concaténe les champs du header pour les afficher.
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

  ### OBJECTIF
  output$user_text <- renderText(input$user_text_Obj)


  ### EDUCATION
  # MAJ langue
  observeEvent(input$Langue,{
    # mise a jour du titre de la catégorie "education" en fonction du pays choisi
    output$eduTitle<-renderText({paste(cat["edu",input$Langue])})
  }) # fin observeEvent

  # Création du texte à afficher (output$eduTxt) et du texte à downloader (eduTxt)
  output$eduTxt<-renderTable({eduTxt()[-2]})
  eduTxt<-function(){
    eduPrintLength<-min(nbMax,input$plusEdu-input$moinsEdu)
    # data frame education : 1ligne=1education
    if (eduPrintLength>0) {
      eduTxt<-data.frame()

      for (i in 1:eduPrintLength) {
        eduTxt[i,1]<-ifelse(input[[paste0("eduTxt1",i)]]!="",
                            # formation
                            paste(input[[paste0("eduTxt1",i)]],
                                  " : ",
                                  input[[paste0("eduTxt2",i)]],
                                  input[[paste0("eduTxt3",i)]],
                                  ifelse(input$Langue=="France",
                                         paste(", ",input[[paste0("levelEdu",i)]])
                                         ,"")
                            )
                            ,"")
        eduTxt[i,2]<-paste0(cat["edu",input$Langue])
      }
      names(eduTxt)<-c(paste0(cat["edu",input$Langue]),"type")
      eduTxt

    }
  }

  # condition d'appparition des champs education supplémentaires
  lapply(1:nbMax, function(i) {
    output[[paste0("nbEdu",i)]] <- reactive(input$plusEdu-input$moinsEdu>=i)
    outputOptions(output, paste0("nbEdu",i), suspendWhenHidden = FALSE)
  })

  # condition d'apparition du niveau
  lapply(1:nbMax, function(i) {
    output[[paste0("Education",i)]] <- reactive(input$Langue=="France" && input$plusEdu-input$moinsEdu>=i)
    outputOptions(output, paste0("Education",i), suspendWhenHidden = FALSE)
  })





  ### WORK EXPERIENCE
  # MAJ langue
  observeEvent(input$Langue,{
    # mise a jour du titre de la catégorie "education" en fonction du pays choisi
    output$weTitle<-renderText({paste(cat["work",input$Langue])})
  }) # fin observeEvent

  # Création du texte à afficher (output$eduTxt) et du texte à downloader (eduTxt)
  output$weTxt<-renderTable({weTxt()[-2]})
  weTxt<-function(){
    wePrintLength<-min(nbMax,input$plusWE-input$moinsWE)
    # data frame education : 1ligne=1education
    if (wePrintLength>0) {
      weTxt<-data.frame()

      for (i in 1:wePrintLength) {
        weTxt[i,1]<-ifelse(input[[paste0("weTxt1",i)]]!="",
                           # exp. pro.
                           paste(input[[paste0("weTxt1",i)]],
                                 " : ",
                                 input[[paste0("weTxt2",i)]],
                                 input[[paste0("weTxt3",i)]]
                                 # ifelse(input$Langue=="France",
                                 #        paste(", ",input[[paste0("levelWE",i)]])
                                 #        ,"")
                           )
                           ,"")
        weTxt[i,2]<-paste0(cat["work",input$Langue])
      }
      names(weTxt)<-c(paste0(cat["work",input$Langue]),"weTxt1")
      weTxt

    }
  }

  # condition d'appparition des champs education supplémentaires
  lapply(1:nbMax, function(i) {
    output[[paste0("nbWE",i)]] <- reactive(input$plusWE-input$moinsWE>=i)
    outputOptions(output, paste0("nbWE",i), suspendWhenHidden = FALSE)
  })

  # condition d'apparition du niveau
  lapply(1:nbMax, function(i) {
    output[[paste0("Work Experience")]] <- reactive(input$Langue=="France" && input$plusWE-input$moinsWE>=i)
    outputOptions(output, paste0("Work Experience"), suspendWhenHidden = FALSE)
  })

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

