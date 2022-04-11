#--------------------------#
# server part of MTG-App #
#--------------------------#


shinyServer(function(input, output,session) {

  output$collectionContent <-  DT::renderDataTable({
    file <- input$collection_file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    
    read.delim(file$datapath, header = FALSE,sep=',')
  })
   
  output$deckContent <-  DT::renderDataTable({
    file <- input$decklist_file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    
    read.delim(file$datapath, header = FALSE,sep='\t')
  })
  
  
  missingCards <- reactive({
    #only works if both collection and decklist are provided
    if(is.null(input$collection_file) | is.null(input$decklist_file)) return()
    
    # read collection
    file <- input$collection_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    collection = read.delim(file$datapath, header = FALSE,sep=',')
    
    # read decklist
    file <- input$decklist_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    decklist = read.delim(file$datapath, header = FALSE,sep=',')
    
    # formatting/collapsing collection
    ## some rows have only one entry due to weird coma formatting
    collection = collection[-which(collection$V3 == ''),]
    
    ## some cards have a coma in their name...
    collection = collection[,1:3]
    collection$V1 = as.numeric(as.character(collection$V1))
    collection$V2 = as.character(collection$V2)
    collection$V3 = as.character(collection$V3)
    idx.split = which(sapply(collection$V3,function(x) toupper(x) != x)) # assuming no card has all capital name
    collection$V2[idx.split] = paste(collection$V2[idx.split],collection$V3[idx.split],sep=',')
    
    ## some cards with same name can be in different collections/foil/languages
    ## but they all count as one coccurence of the same card
    collection_collapse = table(rep(collection$V2,times=collection$V1))
    names(collection_collapse) = trimws(names(collection_collapse))
    
    # formatting/collapsing decklist
    ## formatting is different here as it is a space separated file
    decklist_collapse_count = as.numeric(sapply(strsplit(as.character(decklist$V1),split = ' '),function(x) x[1]))
    decklist_collapse_names = sapply(strsplit(as.character(decklist$V1),split = ' '),function(x) paste(x[2:length(x)],collapse = ' '))
    # remove NAs
    idx.na = which(is.na(decklist_collapse_count))
    if(length(idx.na) > 0){
      decklist_collapse_count = decklist_collapse_count[-idx.na]
      decklist_collapse_names = decklist_collapse_names[-idx.na]
    }
    decklist_collapse = table(rep(decklist_collapse_names,times=decklist_collapse_count))
    
    # missing names
    ## fully missing cards
    missing_names = names(decklist_collapse)[which(!(names(decklist_collapse) %in% names(collection_collapse)))]
    missing_cards = data.frame('name' = missing_names,
                               'qty' = as.numeric(decklist_collapse[missing_names]))
    # partially missing cards
    detected_names = names(decklist_collapse)[which((names(decklist_collapse) %in% names(collection_collapse)))]
    ## check if quantity is enough
    not_enough_cards = names(which(collection_collapse[detected_names] < decklist_collapse[detected_names]))
    not_enough_qty = decklist_collapse[not_enough_cards] - collection_collapse[not_enough_cards]
    missing_cards = rbind(missing_cards,
                          data.frame('name' = not_enough_cards,
                                     'qty' = as.numeric(not_enough_qty)))
    missing_cards
  })
  
  
  # print missing cards
  output$tableMissingCards <-  DT::renderDataTable({
    missingCards()
  })
  
  output$downloadMissingCards <- downloadHandler(

    filename = "missing_cards.csv",
    content = function(file) {
      missingCardsTab <- missingCards()
      write.csv(missingCardsTab, file, row.names = FALSE)
    }
  )
  
})
