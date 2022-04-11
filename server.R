#--------------------------#
# server part of CarboLogR #
#--------------------------#


shinyServer(function(input, output,session) {

  output$collectionContent <- renderTable({
    file <- input$collection_file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    
    read.delim(file$datapath, header = FALSE,sep=',')
  })
   
  path_collection <- reactive({
    if(!is.null(input$collection_file)){
      return(parseFilePaths(roots=c('home'=home_dir,getVolumes()()), input$collection_file))
    }else{
      return(NULL)
    }
  })
  # 
  # plate data location
  # shinyDirChoose(input, 'collection_file', session=session, roots=c('library'=paste(.libPaths()[1],'/CarboLogR/extdata',sep=''),'home'=home_dir,getVolumes()()),defaultRoot = 'library')
  # 
  # path_plate <- reactive({
  #   return(print(parseDirPath(roots=c('library'=paste(.libPaths()[1],'/CarboLogR/extdata',sep=''),'home'=home_dir,getVolumes()()), input$plate_directory)))
  # })
  # 
  # #number of organism function
  # n_organism <- reactive({
  #   if(is.null(input$plate_directory))
  #     return()
  #   
  #   plate.files <- list.files(path = path_plate(), pattern = '*.csv',full.names = TRUE)
  #   # look for the different organisms
  #   organisms = unique(gsub(pattern = '_.*',replacement = '',x=basename(plate.files)))
  #   
  #   
  #   return(length(organisms))
  #   
  # })
  # 
  # # print number of organisms
  # output$text_organism <- renderText({
  #   data = n_organism()
  #   paste(data, " different organisms were detected.")
  # })
  # 
  # 
  # # quality control function
  # QC <- reactive({
  #   if(is.null(input$plate_directory))
  #     return()
  #   
  #   plate.files <- list.files(path = path_plate(), pattern = '*.csv',full.names = TRUE)
  #   # look for the different organisms
  #   organisms <- NULL
  #   
  #   for(f in plate.files){
  #     tmp = read.csv(f,header=TRUE)
  #     # store microbe name
  #     organisms = c(organisms,gsub(pattern = '_.*',replacement = '',x=basename(f)))
  #   }
  #   
  #   organisms <- unique(organisms)
  #   
  #   # create a list to store the kinetics features of all organisms
  #   DB = list()
  #   # create a list to store outlier replicates
  #   flag_replicates = list()
  #   # create a list to store outlier wells
  #   flag_wells = list()
  #   
  #   withProgress(message = 'QC', value = 0, {
  #     n <- length(organisms)
  #     
  #     for (organism in organisms){
  #       incProgress(1/n, detail = paste("Processing organism ", organism))
  #       # retrieve all files for this organism
  #       orga.f <- list.files(path=path_plate(),pattern = organism,full.names = TRUE)
  #       
  #       # loop over the replicates
  #       reps <- list()
  #       for(i in 1:length(orga.f)){
  #         reps[[i]] <- read.csv(orga.f[i],header=TRUE,skip=10)[,1:97] # remove potential NA last colum
  #       }
  #       names(reps) = orga.f
  #       #check if all records have the same length
  #       if(length(unique(sapply(reps,nrow))) > 1){
  #         
  #         min.row = min(sapply(reps,nrow))
  #         for(i in 1:length(reps)){
  #           reps[[i]] <- reps[[i]][1:min.row,]
  #         }
  #       }else{
  #         min.row = nrow(reps[[1]])
  #       }
  #       
  #       # the Growthcurver pakcage requires a column named 'blank' to make background correction
  #       for(i in 1:length(orga.f)){
  #         colnames(reps[[i]])[1] <- 'time'
  #         colnames(reps[[i]])[2] <- 'blank'
  #       }
  #       
  #       # fit growth model for each record
  #       suppressWarnings(gc_out <- lapply(1:length(reps),function(i) SummarizeGrowthByPlate(plate=reps[[i]],record.name=names(reps)[i],bg_correct = 'blank',plot_fit = FALSE)[1:95,]))
  #       # note: remove the final row as it is empty
  #       
  #       # Filter low-quality replicates
  #       # get number of low quality fit wells for each replicate
  #       
  #       l.lq = sapply(gc_out,function(x) length(which(x$note == '')))
  #       
  #       # Define threshold as lower than average quality across replicates and 1 standard deviation
  #       thresh = mean(l.lq) - sd(l.lq)
  #       idx = which(l.lq < thresh)
  #       # filter wells with note (bad fit)
  #       if(length(idx) >0){
  #         reps[idx] <- NULL
  #         flag_replicates[[organism]] = orga.f[idx]
  #       }
  #       
  #       
  #       suppressWarnings(gc_out <- lapply(1:length(reps),function(i)SummarizeGrowthByPlate(plate=reps[[i]],record.name=names(reps)[i],bg_correct = 'blank',plot_fit = FALSE)[1:95,]))
  #       
  #       # Filter low-quality wells
  #       
  #       # get wells with good fit in each replicate
  #       gc_out_filt <- lapply(gc_out,function(x) which(x$note ==''))
  #       # get wells with decent fits in at least half of the replicates
  #       gc_out_filt_idx <- as.numeric(names(which(table(unlist(gc_out_filt)) >= floor(0.5*length(reps)) )))
  #       
  #       if(length(gc_out_filt_idx) == 0){
  #         flag_wells[[organism]] = NULL
  #       }else{
  #         flag_wells[[organism]] = colnames(reps[[1]])[gc_out_filt_idx+2]
  #       }
  #       
  #       #rerun only with the wells with a proper fit
  #       filt.reps <- lapply(1:length(reps),function(i) reps[[i]][,c(1,2,gc_out_filt_idx+2)] )
  #       names(filt.reps) <- names(gc_out_filt)
  #       
  #       suppressWarnings(gc_out_filt_well <- lapply(1:length(filt.reps),function(i)SummarizeGrowthByPlate(plate=filt.reps[[i]],record.name=names(filt.reps)[i],bg_correct = 'blank',plot_fit = FALSE)))
  #       
  #       
  #       DB[[organism]] = gc_out_filt_well
  #     }
  #   })
  #   return(list(data=DB,fr=flag_replicates,fw=flag_wells))
  # })
  # 
  # 
  # # quality control function returning all the data
  # QC_return_all <- reactive({
  #   if(is.null(input$plate_directory))
  #     return()
  #   
  #   pp = path_plate()
  #   plate.files <- list.files(path = pp, pattern = '*.csv',full.names = TRUE)
  #   
  #   # store microbe name
  #   organisms = unique(gsub(pattern = '_.*',replacement = '',x=basename(plate.files)))
  #   
  #   # create a list to store the kinetics features of all organisms
  #   DB.all = list()
  #   reps.all = list()
  #   
  #   # create a list to store outlier replicates
  #   flag_replicates = list()
  #   # create a list to store outlier wells
  #   flag_wells = list()
  #   
  #   withProgress(message = 'QC', value = 0, {
  #     n <- length(organisms)
  #     
  #     for (organism in organisms){
  #       incProgress(1/n, detail = paste("Processing organism ", organism))
  #       # retrieve all files for this organism
  #       orga.f <- list.files(path=pp,pattern = paste0(organism,'_'),full.names = TRUE)
  #       
  #       # loop over the replicates
  #       reps <- list()
  #       for(i in 1:length(orga.f)){
  #         reps[[i]] <- read.csv(orga.f[i],header=TRUE,skip=10)[,1:97] # remove potential NA last colum
  #       }
  #       names(reps) = orga.f
  #       #check if all records have the same length
  #       if(length(unique(sapply(reps,nrow))) > 1){
  #         
  #         min.row = min(sapply(reps,nrow))
  #         for(i in 1:length(reps)){
  #           reps[[i]] <- reps[[i]][1:min.row,]
  #         }
  #       }else{
  #         min.row = nrow(reps[[1]])
  #       }
  #       
  #       # the Growthcurver package requires a column named 'blank' to make background correction
  #       for(i in 1:length(orga.f)){
  #         colnames(reps[[i]])[1] <- 'time'
  #         colnames(reps[[i]])[2] <- 'blank'
  #       }
  #       #store all the data
  #       reps.all[[organism]] = reps
  #       
  #       # fit growth model for each record
  #       suppressWarnings(gc_out <- lapply(1:length(reps),function(i) SummarizeGrowthByPlate(plate=reps[[i]],record.name=names(reps)[i],bg_correct = 'blank',plot_fit = FALSE)[1:95,]))
  #       # note: remove the final row as it is empty
  #       
  #       # Filter low-quality replicates
  #       # get number of low quality fit wells for each replicate
  #       
  #       l.lq = sapply(gc_out,function(x) length(which(x$note == '')))
  #       
  #       # Define threshold as lower than average quality across replicates and 1 standard deviation
  #       if(any(l.lq == 0)){
  #         tmp.l.lq = l.lq[-which(l.lq==0)] # remove replicates with 0 good quality wells, as it indicates clear technical outlier
  #         thresh = mean(tmp.l.lq) - sd(tmp.l.lq)
  #       }else{
  #         thresh = mean(l.lq) - sd(l.lq)
  #       }
  #       
  #       idx = which(l.lq < thresh)
  #       # filter wells with note (bad fit)
  #       if(length(idx) >0){
  #         reps[idx] <- NULL
  #         flag_replicates[[organism]] = orga.f[idx]
  #       }
  #       
  #       
  #       suppressWarnings(gc_out_filt_rep <- lapply(1:length(reps),function(i)SummarizeGrowthByPlate(plate=reps[[i]],record.name=names(reps)[i],bg_correct = 'blank',plot_fit = FALSE)[1:95,]))
  #       
  #       # Filter low-quality wells
  #       
  #       # get wells with good fit in each replicate
  #       gc_out_filt <- lapply(gc_out_filt_rep,function(x) which(x$note ==''))
  #       # get wells with decent fits in at least half of the replicates
  #       gc_out_filt_idx <- as.numeric(names(which(table(unlist(gc_out_filt)) >= floor(0.5*length(reps)) )))
  #       
  #       if(length(gc_out_filt_idx) == 0){
  #         flag_wells[[organism]] = NULL
  #       }else{
  #         flag_wells[[organism]] = colnames(reps[[1]])[gc_out_filt_idx+2]
  #       }
  #       
  #       #rerun only with the wells with a proper fit
  #       filt.reps <- lapply(1:length(reps),function(i) reps[[i]][,c(1,2,gc_out_filt_idx+2)] )
  #       names(filt.reps) <- names(gc_out_filt)
  #       
  #       suppressWarnings(gc_out_filt_well <- lapply(1:length(filt.reps),function(i)SummarizeGrowthByPlate(plate=filt.reps[[i]],record.name=names(filt.reps)[i],bg_correct = 'blank',plot_fit = FALSE)))
  #       
  #       DB.all[[organism]] = gc_out_filt_well
  #     }
  #   })
  #   return(list(reps= reps.all ,fits=DB.all,fr=flag_replicates,fw=flag_wells))
  # })
  # 
  # rv <- reactiveValues()
  # rv$data <- NULL
  # rv$anno <- NULL
  # rv$matchTable <- NULL
  # rv$kine <- NULL
  # 
  # observe({    ## will 'observe' the button press
  #   
  #   if(input$evReactiveButton){
  #     rv$data <- QC_return_all()   ## store the data in the reactive value
  #     rv$data
  #   }
  # })
  # 
  # observe({    ## will 'observe' the radiobutton press
  #   if(input$plateTypeSelect != 'other'){
  #     rv$anno <- ANNO[[input$plateTypeSelect]]   ## store the annotation in the reactive value
  #   }else{
  #     rv$anno <- NULL
  #   }
  #   rv$anno
  # })
  # 
  # observe({    ## will 'observe' the radiobutton press
  #   if(input$plateTypeSelect != 'other'){
  #     rv$matchTable <- match_table[which(match_table$plateID == input$plateTypeSelect),]   ## store the annotation in the reactive value
  #   }else{
  #     rv$matchTable <- NULL
  #   }
  #   rv$matchTable
  # })
  # 
  # observe({    ## will 'observe' the radiobutton press
  #   rv$kine <- input$kinefeature   ## store the annotation in the reactive value
  #   rv$kine
  # })
  # 
  # # print number of filtered organims and wells for each organism
  # output$text_qc <- renderText({
  #   
  #   dat = rv$data
  #   if(!is.null(dat)){
  #     print(sapply(names(dat$fw),function(org) if(length(dat$reps[[org]])-length(dat$fr[[org]]) > 2){
  #       paste('For organism ',org,':\n---- We found ', length(dat$fr[[org]]),' outlier(s): ',
  #             paste(dat$fr[[org]],collapse=' ,'),'\n',
  #             '---- We detected growth in ', length(dat$fw[[org]]),' wells(s): ',
  #             paste(dat$fw[[org]],collapse=' ,'),'\n','\n','\n',sep='')
  #     }else{
  #       paste('For organism ',org,':\n---- We found ', length(dat$fr[[org]]),' outlier(s): ',
  #             paste(dat$fr[[org]],collapse=' ,'),'\n',
  #             'WARNING: it brings the number of replicates below 2 !!!! You probably need more replicates.',
  #             '---- We detected growth in ', length(dat$fw[[org]]),' wells(s): ',
  #             paste(dat$fw[[org]],collapse=' ,'),'\n','\n','\n',sep='')
  #     }))
  #     
  #   }else{
  #     print('User is required to press the button to start the analysis.')
  #   }
  # })
  # 
  # output$downloadDataQC <- downloadHandler(
  #   
  #   filename = "results_qc.csv",
  #   content = function(file) {
  #     #test if QC data are available
  #     if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #     # get all unique wells
  #     all_wells <- sort(unique(unlist(rv$data$fw)))
  #     
  #     merge_profiles = do.call(what = 'c',rv$data$fits)
  #     
  #     profiles.fill = lapply(1:length(merge_profiles),function(i){
  #       fits = merge_profiles[[i]]
  #       rownames(fits) = merge_profiles[[i]][,1]
  #       
  #       # remove column with well names and put them as rownames
  #       fits =fits[,-1]
  #       # remove last row which is empty
  #       fits = fits[-nrow(fits),]
  #       # get all the missing wells
  #       idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #       if(length(idx) < length(all_wells)){
  #         add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #         rownames(add) = idx
  #         colnames(add) = colnames(fits)
  #         tmp = rbind(fits,add)
  #         #rownames(tmp) = c(rownames(fits),idx)
  #         tmp = tmp[order(row.names(tmp)),]
  #         return(tmp)
  #       }else{ # create a zero matrix if all the wells are missing
  #         tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #         rownames(tmp) = all_wells
  #         colnames(tmp) = colnames(fits)
  #         return(tmp)
  #       }
  #     })
  #     
  #     # presence/absence for each well
  #     tmp = lapply(profiles.fill,function(x) as.numeric(x[,1] != 0))
  #     tmp = do.call('rbind',tmp)
  #     rownames(tmp) = unlist(lapply(rv$data$reps,names))[-which(unlist(lapply(rv$data$reps,names)) %in% unlist(rv$data$fr))]#rep(names(rv$data$fits),times=lapply(rv$data$fits,length))
  #     colnames(tmp) = row.names(profiles.fill[[1]])
  #     
  #     write.csv(tmp, file)
  #   }
  # )
  # 
  # 
  # output$selectedOrganism  <- renderUI({
  #   if(is.null(input$plate_directory))
  #     return()
  #   if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #   
  #   plate.files <- list.files(path = path_plate(), pattern = '*.csv',full.names = TRUE)
  #   # look for the different organisms
  #   organisms = unique(gsub(pattern = '_.*',replacement = '',x=basename(plate.files)))
  #   
  #   radioButtons("radio_selectOrganism", "Choose organism",
  #                choices  =  organisms,
  #                selected =  character(0))
  # })
  # 
  # output$plot_growth <- renderPlot({
  #   if(is.null(input$radio_selectOrganism)) return()
  #   
  #   # number of columns of the output grid
  #   NCOL = 16
  #   
  #   dat = rv$data$reps[[input$radio_selectOrganism]]
  #   
  #   if(!input$showOutRep){
  #     dat[rv$data$fr[[input$radio_selectOrganism]]] <- NULL
  #   }
  #   
  #   #store the blank columns before filtering the outlier wells
  #   blank = lapply(dat, function(x) matrix(x[,2],ncol=1))
  #   
  #   #save time vector as it is normalized across replicates during QC step
  #   time = dat[[1]][,1]
  #   
  #   #if(!input$showOutWell){
  #   if(length(rv$data$fw[[input$radio_selectOrganism]]) > 1){
  #     dat = lapply(dat, function(x) x[,rv$data$fw[[input$radio_selectOrganism]]])
  #   }else{
  #     if(length(rv$data$fw[[input$radio_selectOrganism]]) == 1){
  #       dat = lapply(dat, function(x) matrix(x[,rv$data$fw[[input$radio_selectOrganism]]],ncol=1))
  #     }else{
  #       dat = 0
  #     }
  #   }
  #   #}
  #   
  #   #get all plot Y-axis ranges --> same for all
  #   y.axis.range = c(1000,-1000)
  #   for(repl in 1:length(dat)){
  #     x = data.frame(dat[[repl]])
  #     for(i in 1:ncol(x)){
  #       tmp = range(x[,i]-blank[[repl]])
  #       if(tmp[1] < y.axis.range[1]) y.axis.range[1] = tmp[1]
  #       if(tmp[2] > y.axis.range[2]) y.axis.range[2] = tmp[2]
  #     }
  #   }
  #   y.axis.range = 1.1*y.axis.range
  #   
  #   plot_output_list <- list()
  #   for(repl in 1:length(dat)){
  #     
  #     x = data.frame(dat[[repl]])
  #     #get the modulo for empty plot to keep the grid aligned from one replicate to another
  #     rest = 16 - (ncol(x) %% NCOL)
  #     
  #     # create a empty plot with juste the file name:
  #     df <- data.frame(
  #       x = 1.5,
  #       y = 1.5,
  #       text = sub("^([^.]*).*", "\\1",basename(names(dat)[repl]))
  #     )
  #     
  #     p <- ggplot(df, aes(x, y)) +
  #       geom_text(aes(label = text),size=3.5,fontface=2) + xlim(0,4) + ylim(1,2) +
  #       theme(axis.title.x=element_blank(),
  #             axis.text.x=element_blank(),
  #             axis.ticks.x=element_blank(),
  #             axis.title.y=element_blank(),
  #             axis.text.y=element_blank(),
  #             axis.ticks.y=element_blank(),
  #             panel.border = element_blank(),
  #             panel.background = element_blank(),
  #             panel.grid.major = element_blank(),
  #             panel.grid.minor = element_blank()
  #       )
  #     
  #     #add red text to show filtered replicates
  #     if(input$showOutRep & names(dat)[repl] %in% rv$data$fr[[input$radio_selectOrganism]]){
  #       p <- ggplot(df, aes(x, y)) +
  #         geom_text(aes(label = text,fontface=2),size=3.5) + xlim(0,4) + ylim(1,2) +
  #         theme(axis.title.x=element_blank(),
  #               axis.text.x=element_blank(),
  #               axis.ticks.x=element_blank(),
  #               axis.title.y=element_blank(),
  #               axis.text.y=element_blank(),
  #               axis.ticks.y=element_blank(),
  #               panel.border = element_blank(),
  #               panel.background = element_rect(fill = "red",
  #                                               colour = "red",
  #                                               size = 0.5, linetype = "solid"),
  #               panel.grid.major = element_blank(),
  #               panel.grid.minor = element_blank()
  #         ) + theme(legend.position="none")
  #     }
  #     plot_output_list[[length(plot_output_list) + 1]] = p
  #     
  #     # get all the plots for one replicate
  #     plot_output_list <- c(plot_output_list,lapply(2:ncol(x), function(i) {
  #       tbl <- data.frame('time'=time,'intensity'=x[,i]-blank[[repl]]) # substract blank signal
  #       #DEBUG
  #       #tbl <- data.frame('time'=x[,1],'intensity'=x[,i]-x[,2]) # remove blank signal
  #       return(ggplot(tbl,aes(x=time,y=intensity)) + ylab(colnames(x)[i]) + geom_point(size=0.1) + ylim(y.axis.range[1],y.axis.range[2]) +
  #                theme(axis.title.x=element_blank(),
  #                      axis.text.x=element_blank(),
  #                      axis.ticks.x=element_blank(),
  #                      axis.text.y=element_blank(),
  #                      axis.ticks.y=element_blank()
  #                ))
  #     }))
  #     
  #     # add empty plot for grid alignment across replicates
  #     if(rest > 0){
  #       plot_output_list <- c(plot_output_list,lapply(1:rest, function(i) {
  #         df <- data.frame(x = 1.5,y = 1.5)
  #         return(ggplot(df, aes(x = x, y = y)) + geom_blank() + theme_bw() +
  #                  theme(axis.title.x=element_blank(),
  #                        axis.text.x=element_blank(),
  #                        axis.ticks.x=element_blank(),
  #                        axis.text.y=element_blank(),
  #                        axis.ticks.y=element_blank(),
  #                        axis.title.y=element_blank(),
  #                        panel.border = element_blank(),
  #                        panel.background = element_blank(),
  #                        panel.grid.major = element_blank(),
  #                        panel.grid.minor = element_blank()
  #                  ))
  #       }))
  #     }
  #   }
  #   
  #   do.call(grid.arrange, c(plot_output_list, list(ncol = NCOL)))
  #   
  # },width=800,height=600)
  # 
  # 
  # shinyFileChoose(input, 'metadataFile', roots=c('library'=paste(.libPaths()[1],'/CarboLogR/extdata',sep=''),
  #                                                'home'=home_dir,getVolumes()()),  filetypes=c('txt'),defaultRoot = 'library')
  # 
  # 
  # path_metadata <- reactive({
  #   if(!is.null(input$metadataFile)){
  #     return(parseFilePaths(roots=c('library'=paste(.libPaths()[1],'/CarboLogR/extdata',sep=''),
  #                                   'home'=home_dir,getVolumes()()), input$metadataFile))
  #   }else{
  #     return(NULL)
  #   }
  # })
  # 
  # #metadata print
  # output$metadataTable <- renderTable({
  #   inFile <- path_metadata()
  #   #if (is.null(inFile) | n_organism() < 2)
  #   if (nrow(inFile) == 0 | n_organism() < 2)
  #     return(NULL)
  #   head(read.table(inFile$datapath, header = FALSE),5)
  # })
  # 
  # # metadata single-well comparison
  # output$singleWellPlot <- renderPlotly({
  #   
  #   #test if QC data are available
  #   if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #   #inFile <- input$metadataFile
  #   inFile <- path_metadata()
  #   
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   hoverName = all_wells
  #   if(input$showSourceName) hoverName = rv$matchTable$sourceName[match(all_wells,rv$matchTable$wellID)]
  #   
  #   # fill profiles
  #   merge_profiles = do.call(what = 'c',rv$data$fits)
  #   
  #   profiles.fill = lapply(1:length(merge_profiles),function(i){
  #     fits = merge_profiles[[i]]
  #     rownames(fits) = merge_profiles[[i]][,1]
  #     
  #     # remove column with well names and put them as rownames
  #     fits =fits[,-1]
  #     # remove last row which is empty
  #     fits = fits[-nrow(fits),]
  #     # get all the missing wells
  #     idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #     if(length(idx) < length(all_wells)){
  #       add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #       rownames(add) = idx
  #       colnames(add) = colnames(fits)
  #       tmp = rbind(fits,add)
  #       #rownames(tmp) = c(rownames(fits),idx)
  #       tmp = tmp[order(row.names(tmp)),]
  #       return(tmp)
  #     }else{ # create a zero matrix if all the wells are missing
  #       tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #       rownames(tmp) = all_wells
  #       colnames(tmp) = colnames(fits)
  #       return(tmp)
  #     }
  #   })
  #   
  #   # presence/absence statistical test for each well
  #   tmp = lapply(profiles.fill,function(x) as.numeric(x[,1] != 0))
  #   tmp = do.call('rbind',tmp)
  #   
  #   # #row.names(tmp) = names(av.profiles) #TODO: could be needed for the match with phenotype
  #   
  #   
  #   if(n_organism() > 1){
  #     if (nrow(inFile)==0)
  #       return(NULL)
  #     
  #     #read metadata
  #     metadata = read.table(inFile$datapath, header = FALSE)
  #     # reorder metadata to fit the Growth profile order
  #     metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #     #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #     #TODO: implement a match/check
  #     pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #     
  #     #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #     
  #     pvals <- NULL
  #     signs <- NULL
  #     or <- NULL
  #     # Fisher test per well
  #     for(j in 1:ncol(tmp)){
  #       if(any(tmp[,j] == 1) & any(tmp[,j] == 0)){
  #         tab = table(tmp[,j],pheno)
  #         if(length(idx<-which(tab == 0)) > 0){ # fix potential infinite odd ratio
  #           tab = tab + 0.5
  #         }
  #         #stats = fisher.test(x = tmp[,j],y=pheno)
  #         stats = fisher.test(x=tab)
  #         tmp2 = stats$p.val
  #         or <- c(or,log2((tab[1,1] * tab[2,2]) / (tab[1,2] * tab[2,1])))
  #         # get pvalue sign (direction of enrichment)
  #         tab = table(tmp[,j],pheno)
  #         if((tab[2,1]/(tab[1,1]+tab[2,1])) < (tab[2,2]/(tab[1,2]+tab[2,2]))){
  #           signs = c(signs,-1)
  #         }else{
  #           
  #           signs = c(signs,1)
  #         }
  #         
  #         pvals = c(pvals,-log10(tmp2))
  #       }else{
  #         or <- c(or,0)
  #         pvals = c(pvals,0) # pvalue is -log10(1) if no growth was detected in the two groups
  #         # TODO: does it happen as we supposedly filter the wells with no growth beforehand?
  #         signs = c(signs,0)
  #       }
  #     }
  #     
  #     #fix division by 0
  #     if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #     
  #     # define color for each bar based on significance and direction of effect
  #     myPalette = rep('grey',length(all_wells))
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs > 0)] = 'darkgreen'
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs < 0)] = 'darkviolet'
  #     
  #     names.legend = rep("non significant",length(myPalette))
  #     names.legend[which(myPalette == 'darkgreen')] = paste0("enriched in ",levels(pheno)[1])
  #     names.legend[which(myPalette == 'darkviolet')] = paste0("enriched in ",levels(pheno)[2])
  #     # names.legend = factor(names.legend,levels=c("non significant",
  #     #                                             paste0("enriched in ",levels(pheno)[1]),
  #     #                                             paste0("enriched in ",levels(pheno)[2])))
  #     #
  #     # change these names by sugar name if checkbox is ticked
  #     
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     # if OR is infinite, we should draw it (replace it by 10)
  #     if((idx <- length(which(is.infinite(or) & pvals > -log10(0.05)))) > 0) or[which(is.infinite(or) & pvals > -log10(0.05) )] = 10*sign(or)[which(is.infinite(or) & pvals > -log10(0.05))]
  #     
  #     #plot each well signed pvalue
  #     plot_ly(y = all_wells,
  #             x = or,
  #             name = names.legend,
  #             type = "bar",
  #             #marker = list(color = myPalette),
  #             color = I(myPalette),
  #             text = hoverName,
  #             hoverinfo = 'text',
  #             orientation = 'h'
  #     )%>%
  #       layout(title=list(text=paste0(c(rep("&nbsp;", 3),
  #                                       levels(pheno)[1],
  #                                       rep("&nbsp;", 10),
  #                                       '|',
  #                                       rep("&nbsp;", 10),
  #                                       levels(pheno)[2],
  #                                       rep("\n&nbsp;", 3)),
  #                                     collapse = ""),xanchor = "right"),
  #              yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),
  #                                                 "well",
  #                                                 rep("&nbsp;", 20),
  #                                                 rep("\n&nbsp;", 3)),
  #                                               collapse = "")), xaxis=list(title='log2 Odd ratio'),showlegend = TRUE,font=t)
  #     
  #     #data <- data.frame(x=1:length(all_wells), y=profiles.fill[[1]][,1])
  #     
  #     #p <- plot_ly(data, x = ~x, y = ~y, type = 'scatter',mode= 'markers')
  #     
  #   }else{
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     #plot each well proportion of growth
  #     plot_ly(y = all_wells,
  #             x = apply(tmp,2,sum)/nrow(tmp),
  #             type = "bar",
  #             text = hoverName,
  #             hoverinfo = 'text',
  #             orientation = 'h'
  #     )%>%
  #       layout(yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),
  #                                                 "well",
  #                                                 rep("&nbsp;", 20),
  #                                                 rep("\n&nbsp;", 3)),
  #                                               collapse = "")), xaxis=list(title='proportion of growth'),font=t)
  #     
  #   }
  # })
  # 
  # 
  # output$downloadDataSingleWell <- downloadHandler(
  #   
  #   filename = "results_singleWell.csv",
  #   content = function(file) {
  #     #test if QC data are available
  #     if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #     
  #     inFile <- path_metadata()
  #     if(n_organism() > 1){
  #       if (nrow(inFile)==0) return(NULL)
  #       
  #       #read metadata
  #       metadata = read.table(inFile$datapath, header = FALSE)
  #       # reorder metadata to fit the Growth profile order
  #       metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #       #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #       #TODO: implement a match/check
  #       pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #     }
  #     # get all unique wells
  #     all_wells <- sort(unique(unlist(rv$data$fw)))
  #     
  #     # fill profiles
  #     merge_profiles = do.call(what = 'c',rv$data$fits)
  #     
  #     profiles.fill = lapply(1:length(merge_profiles),function(i){
  #       fits = merge_profiles[[i]]
  #       rownames(fits) = merge_profiles[[i]][,1]
  #       
  #       # remove column with well names and put them as rownames
  #       fits =fits[,-1]
  #       # remove last row which is empty
  #       fits = fits[-nrow(fits),]
  #       # get all the missing wells
  #       idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #       if(length(idx) < length(all_wells)){
  #         add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #         rownames(add) = idx
  #         colnames(add) = colnames(fits)
  #         tmp = rbind(fits,add)
  #         #rownames(tmp) = c(rownames(fits),idx)
  #         tmp = tmp[order(row.names(tmp)),]
  #         return(tmp)
  #       }else{ # create a zero matrix if all the wells are missing
  #         tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #         rownames(tmp) = all_wells
  #         colnames(tmp) = colnames(fits)
  #         return(tmp)
  #       }
  #     })
  #     
  #     # presence/absence statistical test for each well
  #     tmp = lapply(profiles.fill,function(x) as.numeric(x[,1] != 0))
  #     tmp = do.call('rbind',tmp)
  #     
  #     # #row.names(tmp) = names(av.profiles) #TODO: could be needed for the match with phenotype
  #     
  #     if(n_organism() > 1){
  #       #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #       
  #       pvals <- NULL
  #       signs <- NULL
  #       or <- NULL
  #       # Fisher test per well
  #       for(j in 1:ncol(tmp)){
  #         if(any(tmp[,j] == 1) & any(tmp[,j] == 0)){
  #           tab = table(tmp[,j],pheno)
  #           if(length(idx<-which(tab == 0)) > 0){ # fix potential infinite odd ratio
  #             tab = tab + 0.5
  #           }
  #           #stats = fisher.test(x = tmp[,j],y=pheno)
  #           stats = fisher.test(x=tab)
  #           tmp2 = stats$p.val
  #           or <- c(or,log2((tab[1,1] * tab[2,2]) / (tab[1,2] * tab[2,1])))
  #           # get pvalue sign (direction of enrichment)
  #           tab = table(tmp[,j],pheno)
  #           if((tab[2,1]/(tab[1,1]+tab[2,1])) < (tab[2,2]/(tab[1,2]+tab[2,2]))){
  #             signs = c(signs,-1)
  #           }else{
  #             
  #             signs = c(signs,1)
  #           }
  #           
  #           pvals = c(pvals,tmp2)
  #         }else{
  #           or <- c(or,0)
  #           pvals = c(pvals,1)
  #           # TODO: does it happen as we supposedly filter the wells with no growth beforehand?
  #           signs = c(signs,0)
  #         }
  #       }
  #       if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #       db = cbind(all_wells,or,pvals)
  #       colnames(db) = c('wellID','oddRatio','pval')
  #     }else{
  #       
  #       db = cbind(all_wells,apply(tmp,2,sum)/nrow(tmp))
  #       colnames(db) = c('wellID','growthProp')
  #     }
  #     write.csv(db, file, row.names = FALSE)
  #   }
  # )
  # 
  # 
  # # manual annotated categories comparison
  # output$manualAnnoPlot <- renderPlotly({
  #   
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   #test if QC data are available
  #   if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #   inFile <- path_metadata()
  #   if(n_organism() > 1){
  #     if (nrow(inFile)==0)
  #       return(NULL)
  #     #read metadata
  #     metadata = read.table(inFile$datapath, header = FALSE)
  #     # reorder metadata to fit the Growth profile order
  #     metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #     #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #     #TODO: implement a match/check
  #     pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #   }
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   
  #   # fill profiles
  #   merge_profiles = do.call(what = 'c',rv$data$fits)
  #   
  #   profiles.fill = lapply(1:length(merge_profiles),function(i){
  #     fits = merge_profiles[[i]]
  #     rownames(fits) = merge_profiles[[i]][,1]
  #     
  #     # remove column with well names and put them as rownames
  #     fits =fits[,-1]
  #     # remove last row which is empty
  #     fits = fits[-nrow(fits),]
  #     # get all the missing wells
  #     idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #     if(length(idx) < length(all_wells)){
  #       add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #       rownames(add) = idx
  #       colnames(add) = colnames(fits)
  #       tmp = rbind(fits,add)
  #       #rownames(tmp) = c(rownames(fits),idx)
  #       tmp = tmp[order(row.names(tmp)),]
  #       return(tmp)
  #     }else{ # create a zero matrix if all the wells are missing
  #       tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #       rownames(tmp) = all_wells
  #       colnames(tmp) = colnames(fits)
  #       return(tmp)
  #     }
  #   })
  #   
  #   # presence/absence statistical test for each well
  #   tmp = lapply(profiles.fill,function(x) as.numeric(x[,1] != 0))
  #   tmp = do.call('rbind',tmp)
  #   # #row.names(tmp) = names(av.profiles) #TODO: could be needed for the match with phenotype
  #   
  #   
  #   
  #   #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #   
  #   pvals <- NULL
  #   signs <- NULL
  #   or <- NULL
  #   
  #   all_categories = factor(rv$matchTable$manualAnno)[match(all_wells,rv$matchTable$wellID)]
  #   
  #   if(n_organism() > 1){
  #     # Fisher test per category, but only using the filtered wells
  #     
  #     for(category in levels(all_categories)){
  #       idx = which(all_categories == category)
  #       pheno.vec = rep(pheno,length(idx))
  #       tmp.vec = as.vector(tmp[,idx])
  #       
  #       if(any( tmp.vec == 1) & any( tmp.vec == 0)){
  #         tab = table(tmp.vec,pheno.vec)
  #         if(length(idx<-which(tab == 0)) > 0){ # fix potential infinite odd ratio
  #           tab = tab + 0.5
  #         }
  #         #stats = fisher.test(x = tmp[,j],y=pheno)
  #         stats = fisher.test(x=tab)
  #         tmp2 = stats$p.val
  #         or <- c(or,log2((tab[1,1] * tab[2,2]) / (tab[1,2] * tab[2,1])))
  #         # get pvalue sign (direction of enrichment)
  #         tab = table( tmp.vec, pheno.vec)
  #         if((tab[2,1]/(tab[1,1]+tab[2,1])) < (tab[2,2]/(tab[1,2]+tab[2,2]))){
  #           signs = c(signs,-1)
  #         }else{
  #           
  #           signs = c(signs,1)
  #         }
  #         
  #         pvals = c(pvals,-log10(tmp2))
  #       }else{
  #         or <- c(or,0)
  #         pvals = c(pvals,0) # pvalue is -log10(1) if no growth was detected in the two groups
  #         # TODO: does it happen as we supposedly filter the wells with no growth beforehand?
  #         signs = c(signs,0)
  #       }
  #     }
  #     if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #     # if OR is infinite, we should draw it (replace it by 10)
  #     #if((idx <- length(which(is.infinite(or) & pvals > -log10(0.05)))) > 0) or[which(is.infinite(or) & pvals > -log10(0.05) )] = 10*sign(or)[which(is.infinite(or) & pvals > -log10(0.05))]
  #     
  #     
  #     # define color for each bar based on significance and direction of effect
  #     myPalette = rep('grey',nlevels(all_categories))
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs > 0)] = 'darkgreen'
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs < 0)] = 'darkviolet'
  #     
  #     names.legend = rep("non significant",length(myPalette))
  #     names.legend[which(myPalette == 'darkgreen')] = paste0("enriched in ",levels(pheno)[1])
  #     names.legend[which(myPalette == 'darkviolet')] = paste0("enriched in ",levels(pheno)[2])
  #     
  #     
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     
  #     #plot each well signed pvalue
  #     plot_ly(source = 'source.cat',
  #             y = levels(all_categories),
  #             x = or,
  #             name = names.legend,
  #             type = "bar",
  #             orientation = 'h',
  #             #marker = list(color = myPalette)
  #             color = I(myPalette)#factor(names.legend,levels = c(paste0("enriched in ",levels(pheno)[1]),paste0("enriched in ",levels(pheno)[2]),"non significant"))
  #     )%>%
  #       #layout(yaxis = list(size=8,title='category'), xaxis=list(title='log2 Odd ratio'),showlegend = TRUE,font=t)
  #       layout(title=list(text=paste0(c(rep("&nbsp;", 3),
  #                                       levels(pheno)[1],
  #                                       rep("&nbsp;", 10),
  #                                       '|',
  #                                       rep("&nbsp;", 10),
  #                                       levels(pheno)[2],
  #                                       rep("\n&nbsp;", 3)),
  #                                     collapse = ""),xanchor = "right"),
  #              yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),
  #                                                 "category",
  #                                                 rep("&nbsp;", 20),
  #                                                 rep("\n&nbsp;", 3)),
  #                                               collapse = "")), xaxis=list(title='log2 Odd ratio'),showlegend = TRUE,font=t)
  #   }else{
  #     for(category in levels(all_categories)){
  #       idx = which(all_categories == category)
  #       tmp.vec = as.vector(tmp[,idx])
  #       
  #       if(any( tmp.vec == 0)){
  #         pvals = c(pvals,sum(tmp.vec)/length(tmp.vec))
  #       }else{
  #         pvals = c(pvals,1)
  #       }
  #     }
  #     
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     
  #     #plot each well signed pvalue
  #     plot_ly(source = 'source.cat',
  #             y = levels(all_categories),
  #             x = pvals,
  #             type = "bar",
  #             orientation = 'h'
  #     )%>%
  #       layout(yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),
  #                                                 "category",
  #                                                 rep("&nbsp;", 20),
  #                                                 rep("\n&nbsp;", 3)),
  #                                               collapse = "")), xaxis=list(title='proportion of growth'),font=t)
  #   }
  # })
  # 
  # 
  # output$downloadDatamanualAnno <- downloadHandler(
  #   
  #   filename = "results_categories.csv",
  #   content = function(file) {
  #     #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #     if(is.null(rv$anno)) return(NULL)
  #     #test if QC data are available
  #     if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #     inFile <- path_metadata()
  #     if(n_organism() > 1){
  #       if (nrow(inFile)==0)
  #         return(NULL)
  #       #read metadata
  #       metadata = read.table(inFile$datapath, header = FALSE)
  #       # reorder metadata to fit the Growth profile order
  #       metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #       #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #       #TODO: implement a match/check
  #       pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #     }
  #     # get all unique wells
  #     all_wells <- sort(unique(unlist(rv$data$fw)))
  #     
  #     # fill profiles
  #     merge_profiles = do.call(what = 'c',rv$data$fits)
  #     
  #     profiles.fill = lapply(1:length(merge_profiles),function(i){
  #       fits = merge_profiles[[i]]
  #       rownames(fits) = merge_profiles[[i]][,1]
  #       
  #       # remove column with well names and put them as rownames
  #       fits =fits[,-1]
  #       # remove last row which is empty
  #       fits = fits[-nrow(fits),]
  #       # get all the missing wells
  #       idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #       if(length(idx) < length(all_wells)){
  #         add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #         rownames(add) = idx
  #         colnames(add) = colnames(fits)
  #         tmp = rbind(fits,add)
  #         #rownames(tmp) = c(rownames(fits),idx)
  #         tmp = tmp[order(row.names(tmp)),]
  #         return(tmp)
  #       }else{ # create a zero matrix if all the wells are missing
  #         tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #         rownames(tmp) = all_wells
  #         colnames(tmp) = colnames(fits)
  #         return(tmp)
  #       }
  #     })
  #     
  #     # presence/absence statistical test for each well
  #     tmp = lapply(profiles.fill,function(x) as.numeric(x[,1] != 0))
  #     tmp = do.call('rbind',tmp)
  #     # #row.names(tmp) = names(av.profiles) #TODO: could be needed for the match with phenotype
  #     
  #     
  #     
  #     #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #     
  #     pvals <- NULL
  #     signs <- NULL
  #     or <- NULL
  #     # Fisher test per category, but only using the filtered wells
  #     #all_categories = factor(match_table$manualAnno)[match(all_wells,match_table$wellID)]
  #     all_categories = factor(rv$matchTable$manualAnno)[match(all_wells,rv$matchTable$wellID)]
  #     if(n_organism() > 1){
  #       for(category in levels(all_categories)){
  #         idx = which(all_categories == category)
  #         pheno.vec = rep(pheno,length(idx))
  #         tmp.vec = as.vector(tmp[,idx])
  #         
  #         if(any( tmp.vec == 1) & any( tmp.vec == 0)){
  #           tab = table(tmp.vec,pheno.vec)
  #           if(length(idx<-which(tab == 0)) > 0){ # fix potential infinite odd ratio
  #             tab = tab + 0.5
  #           }
  #           #stats = fisher.test(x = tmp[,j],y=pheno)
  #           stats = fisher.test(x=tab)
  #           tmp2 = stats$p.val
  #           or <- c(or,log2((tab[1,1] * tab[2,2]) / (tab[1,2] * tab[2,1])))
  #           # get pvalue sign (direction of enrichment)
  #           tab = table( tmp.vec, pheno.vec)
  #           if((tab[2,1]/(tab[1,1]+tab[2,1])) < (tab[2,2]/(tab[1,2]+tab[2,2]))){
  #             signs = c(signs,-1)
  #           }else{
  #             
  #             signs = c(signs,1)
  #           }
  #           
  #           pvals = c(pvals,-log10(tmp2))
  #         }else{
  #           or <- c(or,0)
  #           pvals = c(pvals,0) # pvalue is -log10(1) if no growth was detected in the two groups
  #           # TODO: does it happen as we supposedly filter the wells with no growth beforehand?
  #           signs = c(signs,0)
  #         }
  #       }
  #       
  #       if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #       db = cbind(levels(all_categories),or,10^-pvals)
  #       
  #       colnames(db) = c('Category','oddRatio','pval')
  #     }else{
  #       for(category in levels(all_categories)){
  #         idx = which(all_categories == category)
  #         tmp.vec = as.vector(tmp[,idx])
  #         
  #         if(any( tmp.vec == 0)){
  #           pvals = c(pvals,sum(tmp.vec)/length(tmp.vec))
  #         }else{
  #           pvals = c(pvals,1)
  #         }
  #       }
  #       
  #       db = cbind(levels(all_categories),pvals)
  #       colnames(db) = c('Category','growthProp')
  #     }
  #     write.csv(db, file, row.names = FALSE)
  #   }
  # )
  # 
  # 
  # # Coupled hover event
  # output$selectedCatInfoWell <- renderText({
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   # Read in hover data
  #   eventdata <- event_data("plotly_click", source = "source.cat")
  #   validate(need(!is.null(eventdata), "Click on one bar to show corresponding category information"))
  #   # Get cluster number (hovered)
  #   catNumber <- eventdata$y
  #   
  #   
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   all_categories = factor(rv$matchTable$manualAnno)[match(all_wells,rv$matchTable$wellID)]
  #   idx = which(all_categories == catNumber)
  #   
  #   print(paste0('Selected cluster: ',catNumber,'\n',
  #                '\t\t\tIt contains the following wells: ',list(all_wells[idx]),'\n'))
  #   
  # })
  # 
  # # Coupled hover event
  # output$selectedCatInfoName <- renderText({
  #   
  #   # Read in hover data
  #   eventdata <- event_data("plotly_click", source = "source.cat")
  #   validate(need(!is.null(eventdata), "Click on one bar to show corresponding cluster information"))
  #   # Get cluster number (hovered)
  #   catNumber <- eventdata$y
  #   
  #   
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   all_categories = factor(rv$matchTable$manualAnno)[match(all_wells,rv$matchTable$wellID)]
  #   idx = which(all_categories == catNumber)
  #   
  #   
  #   print(paste0('\t\t\tIt contains the following carbon sources: ',list(as.character(rv$matchTable$sourceName[match(all_wells[idx],rv$matchTable$wellID)])),'\n'))
  #   
  # })
  # 
  # 
  # # chemoinformatic cluster comparison
  # output$chemoinfoPlot <- renderPlotly({
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   #test if QC data are available
  #   if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #   inFile <- path_metadata()
  #   
  #   if(n_organism() > 1){
  #     if (nrow(inFile)==0)
  #       return(NULL)
  #     #read metadata
  #     metadata = read.table(inFile$datapath, header = FALSE)
  #     # reorder metadata to fit the Growth profile order
  #     metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #     #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #     #TODO: implement a match/check
  #     pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #   }
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   
  #   # fill profiles
  #   merge_profiles = do.call(what = 'c',rv$data$fits)
  #   
  #   profiles.fill = lapply(1:length(merge_profiles),function(i){
  #     fits = merge_profiles[[i]]
  #     rownames(fits) = merge_profiles[[i]][,1]
  #     
  #     # remove column with well names and put them as rownames
  #     fits =fits[,-1]
  #     # remove last row which is empty
  #     fits = fits[-nrow(fits),]
  #     # get all the missing wells
  #     idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #     if(length(idx) < length(all_wells)){
  #       add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #       rownames(add) = idx
  #       colnames(add) = colnames(fits)
  #       tmp = rbind(fits,add)
  #       #rownames(tmp) = c(rownames(fits),idx)
  #       tmp = tmp[order(row.names(tmp)),]
  #       return(tmp)
  #     }else{ # create a zero matrix if all the wells are missing
  #       tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #       rownames(tmp) = all_wells
  #       colnames(tmp) = colnames(fits)
  #       return(tmp)
  #     }
  #   })
  #   
  #   # presence/absence statistical test for each well
  #   tmp = lapply(profiles.fill,function(x) as.numeric(x[,1] != 0))
  #   tmp = do.call('rbind',tmp)
  #   # #row.names(tmp) = names(av.profiles) #TODO: could be needed for the match with phenotype
  #   
  #   
  #   
  #   #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #   
  #   pvals <- NULL
  #   signs <- NULL
  #   or <- NULL
  #   # Fisher test per category, but only using the filtered wells
  #   all_categories = factor(paste0('cluster',factor(rv$matchTable$chemoCluster)[match(all_wells,rv$matchTable$wellID)]))
  #   if(n_organism() >1){
  #     for(category in levels(all_categories)){
  #       idx = which(all_categories == category)
  #       pheno.vec = rep(pheno,length(idx))
  #       tmp.vec = as.vector(tmp[,idx])
  #       
  #       if(any( tmp.vec == 1) & any( tmp.vec == 0)){
  #         tab = table(tmp.vec,pheno.vec)
  #         if(length(idx<-which(tab == 0)) > 0){ # fix potential infinite odd ratio
  #           tab = tab + 0.5
  #         }
  #         #stats = fisher.test(x = tmp[,j],y=pheno)
  #         stats = fisher.test(x=tab)
  #         tmp2 = stats$p.val
  #         or <- c(or,log2((tab[1,1] * tab[2,2]) / (tab[1,2] * tab[2,1])))
  #         # get pvalue sign (direction of enrichment)
  #         tab = table( tmp.vec, pheno.vec)
  #         if((tab[2,1]/(tab[1,1]+tab[2,1])) < (tab[2,2]/(tab[1,2]+tab[2,2]))){
  #           signs = c(signs,-1)
  #         }else{
  #           
  #           signs = c(signs,1)
  #         }
  #         
  #         pvals = c(pvals,-log10(tmp2))
  #       }else{
  #         or = c(or,0)
  #         pvals = c(pvals,0) # pvalue is -log10(1) if no growth was detected in the two groups
  #         # TODO: does it happen as we supposedly filter the wells with no growth beforehand?
  #         signs = c(signs,0)
  #       }
  #     }
  #     
  #     if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #     # if OR is infinite, we should draw it (replace it by 10)
  #     #if((idx <- length(which(is.infinite(or) & pvals > -log10(0.05)))) > 0) or[which(is.infinite(or) & pvals > -log10(0.05) )] = 10*sign(or)[which(is.infinite(or) & pvals > -log10(0.05))]
  #     
  #     
  #     # define color for each bar based on significance and direction of effect
  #     myPalette = rep('grey',nlevels(all_categories))
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs > 0)] = 'darkgreen'
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs < 0)] = 'darkviolet'
  #     
  #     names.legend = rep("non significant",length(myPalette))
  #     names.legend[which(myPalette == 'darkgreen')] = paste0("enriched in ",levels(pheno)[1])
  #     names.legend[which(myPalette == 'darkviolet')] = paste0("enriched in ",levels(pheno)[2])
  #     
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     
  #     #plot each well signed pvalue
  #     plot_ly(source = 'source',
  #             y = levels(all_categories),
  #             x = or,
  #             name = names.legend,
  #             type = "bar",
  #             orientation = 'h',
  #             #marker = list(color = myPalette),
  #             color = I(myPalette)#factor(names.legend,levels = c(paste0("enriched in ",levels(pheno)[1]),paste0("enriched in ",levels(pheno)[2]),"non significant"))
  #     )%>%
  #       layout(title=list(text=paste0(c(rep("&nbsp;", 3),
  #                                       levels(pheno)[1],
  #                                       rep("&nbsp;", 10),
  #                                       '|',
  #                                       rep("&nbsp;", 10),
  #                                       levels(pheno)[2],
  #                                       rep("\n&nbsp;", 3)),
  #                                     collapse = ""),xanchor = "right"),yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),
  #                                                                                                          "cluster",
  #                                                                                                          rep("&nbsp;", 20),
  #                                                                                                          rep("\n&nbsp;", 3)),
  #                                                                                                        collapse = "")), xaxis=list(title='log2 Odd ratio'),showlegend = TRUE,font=t)
  #   }else{
  #     
  #     for(category in levels(all_categories)){
  #       idx = which(all_categories == category)
  #       tmp.vec = as.vector(tmp[,idx])
  #       
  #       if(any( tmp.vec == 0)){
  #         pvals = c(pvals,sum(tmp.vec)/length(tmp.vec))
  #       }else{
  #         pvals = c(pvals,1)
  #       }
  #     }
  #     
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     
  #     #plot each well signed pvalue
  #     plot_ly(source = 'source',
  #             y = levels(all_categories),
  #             x = pvals,
  #             type = "bar",
  #             orientation = 'h'
  #     )%>%
  #       layout(yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),
  #                                                 "cluster",
  #                                                 rep("&nbsp;", 20),
  #                                                 rep("\n&nbsp;", 3)),
  #                                               collapse = "")), xaxis=list(title='proportion of growth'),font=t)
  #   }
  # })
  # 
  # 
  # output$downloadDatachemoinfo <- downloadHandler(
  #   
  #   filename = "results_chemocluster.csv",
  #   content = function(file) {
  #     #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #     if(is.null(rv$anno)) return(NULL)
  #     #test if QC data are available
  #     if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #     inFile <- path_metadata()
  #     if(n_organism()>1){
  #       if (nrow(inFile)==0)
  #         return(NULL)
  #       #read metadata
  #       metadata = read.table(inFile$datapath, header = FALSE)
  #       # reorder metadata to fit the Growth profile order
  #       metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #       #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #       #TODO: implement a match/check
  #       pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #     }
  #     # get all unique wells
  #     all_wells <- sort(unique(unlist(rv$data$fw)))
  #     
  #     # fill profiles
  #     merge_profiles = do.call(what = 'c',rv$data$fits)
  #     
  #     profiles.fill = lapply(1:length(merge_profiles),function(i){
  #       fits = merge_profiles[[i]]
  #       rownames(fits) = merge_profiles[[i]][,1]
  #       
  #       # remove column with well names and put them as rownames
  #       fits =fits[,-1]
  #       # remove last row which is empty
  #       fits = fits[-nrow(fits),]
  #       # get all the missing wells
  #       idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #       if(length(idx) < length(all_wells)){
  #         add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #         rownames(add) = idx
  #         colnames(add) = colnames(fits)
  #         tmp = rbind(fits,add)
  #         #rownames(tmp) = c(rownames(fits),idx)
  #         tmp = tmp[order(row.names(tmp)),]
  #         return(tmp)
  #       }else{ # create a zero matrix if all the wells are missing
  #         tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #         rownames(tmp) = all_wells
  #         colnames(tmp) = colnames(fits)
  #         return(tmp)
  #       }
  #     })
  #     
  #     # presence/absence statistical test for each well
  #     tmp = lapply(profiles.fill,function(x) as.numeric(x[,1] != 0))
  #     tmp = do.call('rbind',tmp)
  #     # #row.names(tmp) = names(av.profiles) #TODO: could be needed for the match with phenotype
  #     
  #     
  #     
  #     #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #     
  #     pvals <- NULL
  #     signs <- NULL
  #     or <- NULL
  #     # Fisher test per category, but only using the filtered wells
  #     all_categories = factor(paste0('cluster',factor(rv$matchTable$chemoCluster)[match(all_wells,rv$matchTable$wellID)]))
  #     if(n_organism() > 1){
  #       for(category in levels(all_categories)){
  #         idx = which(all_categories == category)
  #         pheno.vec = rep(pheno,length(idx))
  #         tmp.vec = as.vector(tmp[,idx])
  #         
  #         if(any( tmp.vec == 1) & any( tmp.vec == 0)){
  #           tab = table(tmp.vec,pheno.vec)
  #           if(length(idx<-which(tab == 0)) > 0){ # fix potential infinite odd ratio
  #             tab = tab + 0.5
  #           }
  #           #stats = fisher.test(x = tmp[,j],y=pheno)
  #           stats = fisher.test(x=tab)
  #           tmp2 = stats$p.val
  #           or <- c(or,log2((tab[1,1] * tab[2,2]) / (tab[1,2] * tab[2,1])))
  #           # get pvalue sign (direction of enrichment)
  #           tab = table( tmp.vec, pheno.vec)
  #           if((tab[2,1]/(tab[1,1]+tab[2,1])) < (tab[2,2]/(tab[1,2]+tab[2,2]))){
  #             signs = c(signs,-1)
  #           }else{
  #             
  #             signs = c(signs,1)
  #           }
  #           
  #           pvals = c(pvals,-log10(tmp2))
  #         }else{
  #           or = c(or,0)
  #           pvals = c(pvals,0) # pvalue is -log10(1) if no growth was detected in the two groups
  #           # TODO: does it happen as we supposedly filter the wells with no growth beforehand?
  #           signs = c(signs,0)
  #         }
  #       }
  #       if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #       db = cbind(levels(all_categories),or,10^-pvals)
  #       colnames(db) = c('Chemocluster','oddRatio','pval')
  #     }else{
  #       for(category in levels(all_categories)){
  #         idx = which(all_categories == category)
  #         tmp.vec = as.vector(tmp[,idx])
  #         
  #         if(any( tmp.vec == 0)){
  #           pvals = c(pvals,sum(tmp.vec)/length(tmp.vec))
  #         }else{
  #           pvals = c(pvals,1)
  #         }
  #       }
  #       db = cbind(levels(all_categories),pvals)
  #       colnames(db) = c('Chemocluster','growthProp')
  #     }
  #     write.csv(db, file, row.names = FALSE)
  #   }
  # )
  # 
  # 
  # # Coupled hover event
  # output$selectedClusterInfoWell <- renderText({
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   # Read in hover data
  #   eventdata <- event_data("plotly_click", source = "source")
  #   validate(need(!is.null(eventdata), "Click on one bar to show corresponding cluster information"))
  #   # Get cluster number (hovered)
  #   clusterNumber <- eventdata$y
  #   
  #   
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   all_categories = factor(paste0('cluster',factor(rv$matchTable$chemoCluster)[match(all_wells,rv$matchTable$wellID)]))
  #   idx = which(all_categories == clusterNumber)
  #   
  #   print(paste0('Selected cluster: ',clusterNumber,'\n',
  #                '\t\t\tIt contains the following wells: ',list(all_wells[idx]),'\n'))
  #   
  # })
  # 
  # # Coupled hover event
  # output$selectedClusterInfoName <- renderText({
  #   
  #   # Read in hover data
  #   eventdata <- event_data("plotly_click", source = "source")
  #   validate(need(!is.null(eventdata), "Click on one bar to show corresponding cluster information"))
  #   # Get cluster number (hovered)
  #   clusterNumber <- eventdata$y
  #   
  #   
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   all_categories = factor(paste0('cluster',factor(rv$matchTable$chemoCluster)[match(all_wells,rv$matchTable$wellID)]))
  #   idx = which(all_categories == clusterNumber)
  #   
  #   print(paste0('\t\t\tIt contains the following carbon sources: ',list(as.character(rv$matchTable$sourceName[match(all_wells[idx],rv$matchTable$wellID)])),'\n'))
  #   
  # })
  # 
  # # Coupled hover event
  # output$selectedClusterInfoKEGG <- renderText({
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   # Read in hover data
  #   eventdata <- event_data("plotly_click", source = "source")
  #   validate(need(!is.null(eventdata), "Click on one bar to show corresponding cluster information"))
  #   # Get cluster number (hovered)
  #   clusterNumber <- eventdata$y
  #   
  #   anno = rv$anno$ENR
  #   names(anno) = paste0('cluster',1:length(anno))
  #   
  #   if(length(anno[[clusterNumber]]) > 0){
  #     
  #     # get all unique wells
  #     all_wells <- sort(unique(unlist(rv$data$fw)))
  #     all_categories = factor(paste0('cluster',factor(rv$matchTable$chemoCluster)[match(all_wells,rv$matchTable$wellID)]))
  #     idx = which(all_categories == clusterNumber)
  #     
  #     tmp.anno = matrix(anno[[clusterNumber]],ncol=2)
  #     
  #     if(length(idx.neg <- which(tmp.anno[,2] < 0)) > 0){
  #       enriched_path = tmp.anno[-idx.neg,1]
  #     }else{
  #       enriched_path = tmp.anno[,1]
  #     }
  #     if(length(enriched_path) > 0){
  #       
  #       print(paste0('\t\t\tIt is enriched in the following KEGG pathways: ','\n',
  #                    list(enriched_path))) # remove negative enrichment as it is confusing the interpretation
  #     }else{
  #       print('No KEGG enrichment was found for this cluster\n')
  #     }
  #   }else{
  #     print('No KEGG enrichment was found for this cluster\n')
  #   }
  #   
  # })
  # 
  # output$heatmapPlot <- renderPlotly({
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   if(input$showSourceName2){
  #     labrow = rv$matchTable$sourceName[match(rownames(rv$anno$simMA),rv$matchTable$wellID)]
  #     labcol = rv$matchTable$sourceName[match(rownames(rv$anno$simMA),rv$matchTable$wellID)]
  #   }else{
  #     labrow = rownames(rv$anno$simMA)
  #     labcol = colnames(rv$anno$simMA)
  #   }
  #   
  #   heatmaply(rv$anno$simMA, k_col = NA, k_row = NA, label_names = c("product1", "product2", "similarity"),
  #             labRow = labrow,labCol = labcol,fontsize_row = 6,fontsize_col = 6) %>% layout(margin = list(t=0, b=0, l=0, r=0,autosize=F))
  #   
  # })
  # 
  # #####################################################
  # # KINETIC PANEL
  # 
  # 
  # shinyFileChoose(input, 'metadataFileKine', roots=c('library'=paste(.libPaths()[1],'/CarboLogR/extdata',sep=''),
  #                                                    'home'=home_dir,getVolumes()()),  filetypes=c('txt'),defaultRoot = 'library')
  # 
  # path_metadata_kine <- reactive({
  #   if(!is.null(input$metadataFileKine)){
  #     return(parseFilePaths(roots=c('library'=paste(.libPaths()[1],'/CarboLogR/extdata',sep=''),
  #                                   'home'=home_dir,getVolumes()()), input$metadataFileKine))
  #   }else{
  #     return(NULL)
  #   }
  # })
  # 
  # #metadata print
  # output$metadataTableKine <- renderTable({
  #   inFile <- path_metadata_kine()
  #   
  #   if (nrow(inFile)==0 | n_organism() < 2)
  #     return(NULL)
  #   head(read.table(inFile$datapath, header = FALSE),5)
  # })
  # 
  # 
  # # metadata single-well comparison
  # output$singleWellPlotKine <- renderPlotly({
  #   
  #   #test if QC data are available
  #   if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #   inFile <- path_metadata_kine()#input$metadataFileKine
  #   
  #   if(n_organism() > 1){
  #     if (nrow(inFile)==0)
  #       return(NULL)
  #     #read metadata
  #     metadata = read.table(inFile$datapath, header = FALSE)
  #     # reorder metadata to fit the Growth profile order
  #     metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #     #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #     #TODO: implement a match/check
  #     pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #   }
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   
  #   # fill profiles
  #   merge_profiles = do.call(what = 'c',rv$data$fits)
  #   
  #   profiles.fill = lapply(1:length(merge_profiles),function(i){
  #     fits = merge_profiles[[i]]
  #     rownames(fits) = merge_profiles[[i]][,1]
  #     
  #     # remove column with well names and put them as rownames
  #     fits =fits[,-1]
  #     # remove last row which is empty
  #     fits = fits[-nrow(fits),]
  #     # get all the missing wells
  #     idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #     if(length(idx) < length(all_wells)){
  #       add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #       rownames(add) = idx
  #       colnames(add) = colnames(fits)
  #       tmp = rbind(fits,add)
  #       tmp = tmp[order(row.names(tmp)),]
  #       return(tmp)
  #     }else{ # create a zero matrix if all the wells are missing
  #       tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #       rownames(tmp) = all_wells
  #       colnames(tmp) = colnames(fits)
  #       return(tmp)
  #     }
  #   })
  #   
  #   # get the corresponding kinetic feature statistical test for each well
  #   tmp = lapply(profiles.fill,function(x) as.numeric(x[,rv$kine]))
  #   tmp = do.call('rbind',tmp)
  #   
  #   #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #   
  #   pvals <- NULL
  #   signs <- NULL
  #   or <- NULL
  #   
  #   if(n_organism()>1){
  #     # T-test per well
  #     for(j in 1:ncol(tmp)){
  #       stats = t.test(tmp[which(pheno == levels(pheno)[1]),j],tmp[which(pheno == levels(pheno)[2]),j])
  #       tmp2 = stats$p.val
  #       tmp.or = cohen.d(f=tmp[which(pheno == levels(pheno)[1]),j],d=tmp[which(pheno == levels(pheno)[2]),j])$estimate
  #       or <- c(or,tmp.or)
  #       # get pvalue sign (direction of enrichment)
  #       signs = c(signs,sign(tmp.or))
  #       pvals = c(pvals,-log10(tmp2))
  #     }
  #     if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #     # define color for each bar based on significance and direction of effect
  #     myPalette = rep('grey',length(all_wells))
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs < 0)] = 'darkgreen'
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs > 0)] = 'darkviolet'
  #     
  #     names.legend = rep("non significant",length(myPalette))
  #     names.legend[which(myPalette == 'darkgreen')] = paste0("enriched in ",levels(pheno)[1])
  #     names.legend[which(myPalette == 'darkviolet')] = paste0("enriched in ",levels(pheno)[2])
  #     
  #     # change these names by sugar name if checkbox is ticked
  #     hoverName = all_wells
  #     if(input$showSourceNameKine) hoverName = rv$matchTable$sourceName[match(all_wells,rv$matchTable$wellID)]
  #     
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     
  #     #plot each well signed pvalue
  #     plot_ly(y = all_wells,
  #             x = or,
  #             name = names.legend,
  #             type = "bar",
  #             color = I(myPalette),
  #             text = hoverName,
  #             hoverinfo = 'text',
  #             orientation = 'h'
  #     )%>%
  #       layout(title=list(text=paste0(c(rep("&nbsp;", 3),
  #                                       levels(pheno)[1],
  #                                       rep("&nbsp;", 10),
  #                                       '|',
  #                                       rep("&nbsp;", 10),
  #                                       levels(pheno)[2],
  #                                       rep("\n&nbsp;", 3)),
  #                                     collapse = ""),xanchor = "right"),yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),
  #                                                                                                          "well",
  #                                                                                                          rep("&nbsp;", 20),
  #                                                                                                          rep("\n&nbsp;", 3)),
  #                                                                                                        collapse = "")), xaxis=list(title="Cohen's d"),showlegend = TRUE,font=t)
  #   }else{
  #     for(j in 1:ncol(tmp)){
  #       pvals = c(pvals,mean(tmp[,j]))
  #     }
  #     
  #     # change these names by sugar name if checkbox is ticked
  #     hoverName = all_wells
  #     if(input$showSourceNameKine) hoverName = rv$matchTable$sourceName[match(all_wells,rv$matchTable$wellID)]
  #     
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     #plot each well signed pvalue
  #     plot_ly(y = all_wells,
  #             x = pvals,
  #             type = "bar",
  #             text = hoverName,
  #             hoverinfo = 'text',
  #             orientation = 'h'
  #     )%>%
  #       layout(yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),"well",rep("&nbsp;", 20),
  #                                                 rep("\n&nbsp;", 3)),
  #                                               collapse = "")), xaxis=list(title=paste("Mean",rv$kine,"value")),font=t)
  #   }
  # })
  # 
  # 
  # output$downloadDataSingleWellKine <- downloadHandler(
  #   
  #   filename = "results_singleWellKine.csv",
  #   content = function(file) {
  #     
  #     #test if QC data are available
  #     if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #     inFile <- path_metadata_kine()#input$metadataFileKine
  #     if(n_organism()>1){
  #       if (nrow(inFile)==0)
  #         return(NULL)
  #       #read metadata
  #       metadata = read.table(inFile$datapath, header = FALSE)
  #       # reorder metadata to fit the Growth profile order
  #       metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #       #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #       #TODO: implement a match/check
  #       pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #     }
  #     # get all unique wells
  #     all_wells <- sort(unique(unlist(rv$data$fw)))
  #     
  #     # fill profiles
  #     merge_profiles = do.call(what = 'c',rv$data$fits)
  #     
  #     profiles.fill = lapply(1:length(merge_profiles),function(i){
  #       fits = merge_profiles[[i]]
  #       rownames(fits) = merge_profiles[[i]][,1]
  #       
  #       # remove column with well names and put them as rownames
  #       fits =fits[,-1]
  #       # remove last row which is empty
  #       fits = fits[-nrow(fits),]
  #       # get all the missing wells
  #       idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #       if(length(idx) < length(all_wells)){
  #         add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #         rownames(add) = idx
  #         colnames(add) = colnames(fits)
  #         tmp = rbind(fits,add)
  #         #rownames(tmp) = c(rownames(fits),idx)
  #         tmp = tmp[order(row.names(tmp)),]
  #         return(tmp)
  #       }else{ # create a zero matrix if all the wells are missing
  #         tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #         rownames(tmp) = all_wells
  #         colnames(tmp) = colnames(fits)
  #         return(tmp)
  #       }
  #     })
  #     
  #     # get the corresponding kinetic feature statistical test for each well
  #     tmp = lapply(profiles.fill,function(x) as.numeric(x[,input$kinefeature]))
  #     tmp = do.call('rbind',tmp)
  #     
  #     #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #     
  #     pvals <- NULL
  #     signs <- NULL
  #     or <- NULL
  #     
  #     if(n_organism()>1){
  #       # T-test per well
  #       for(j in 1:ncol(tmp)){
  #         stats = t.test(tmp[which(pheno == levels(pheno)[1]),j],tmp[which(pheno == levels(pheno)[2]),j])
  #         tmp2 = stats$p.val
  #         tmp.or = cohen.d(f=tmp[which(pheno == levels(pheno)[1]),j],d=tmp[which(pheno == levels(pheno)[2]),j])$estimate
  #         or <- c(or,tmp.or)
  #         # get pvalue sign (direction of enrichment)
  #         signs = c(signs,sign(tmp.or))
  #         pvals = c(pvals,-log10(tmp2))
  #       }
  #       if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #       db = cbind(all_wells,or,10^-pvals)
  #       colnames(db) = c('wellID','CohenD','pval')
  #     }else{
  #       for(j in 1:ncol(tmp)){
  #         pvals = c(pvals,mean(tmp[,j]))
  #       }
  #       db = cbind(all_wells,pvals)
  #       colnames(db) = c('wellID','average')
  #     }
  #     write.csv(db, file, row.names = FALSE)
  #   }
  # )
  # 
  # 
  # # manual annotated categories comparison
  # output$manualAnnoPlotKine <- renderPlotly({
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   #test if QC data are available
  #   if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #   inFile <- path_metadata_kine()#input$metadataFileKine
  #   if(n_organism()>1){
  #     if (nrow(inFile)==0)
  #       return(NULL)
  #     #read metadata
  #     metadata = read.table(inFile$datapath, header = FALSE)
  #     # reorder metadata to fit the Growth profile order
  #     metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #     #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #     #TODO: implement a match/check
  #     pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #   }
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   
  #   # fill profiles
  #   merge_profiles = do.call(what = 'c',rv$data$fits)
  #   
  #   profiles.fill = lapply(1:length(merge_profiles),function(i){
  #     fits = merge_profiles[[i]]
  #     rownames(fits) = merge_profiles[[i]][,1]
  #     
  #     # remove column with well names and put them as rownames
  #     fits =fits[,-1]
  #     # remove last row which is empty
  #     fits = fits[-nrow(fits),]
  #     # get all the missing wells
  #     idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #     if(length(idx) < length(all_wells)){
  #       add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #       rownames(add) = idx
  #       colnames(add) = colnames(fits)
  #       tmp = rbind(fits,add)
  #       #rownames(tmp) = c(rownames(fits),idx)
  #       tmp = tmp[order(row.names(tmp)),]
  #       return(tmp)
  #     }else{ # create a zero matrix if all the wells are missing
  #       tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #       rownames(tmp) = all_wells
  #       colnames(tmp) = colnames(fits)
  #       return(tmp)
  #     }
  #   })
  #   
  #   # presence/absence statistical test for each well
  #   tmp = lapply(profiles.fill,function(x) as.numeric(x[,input$kinefeature]))
  #   tmp = do.call('rbind',tmp)
  #   
  #   #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #   
  #   pvals <- NULL
  #   signs <- NULL
  #   or <- NULL
  #   # Fisher test per category, but only using the filtered wells
  #   #all_categories = factor(match_table$manualAnno)[match(all_wells,match_table$wellID)]
  #   all_categories = factor(rv$matchTable$manualAnno)[match(all_wells,rv$matchTable$wellID)]
  #   
  #   
  #   pvals <- NULL
  #   signs <- NULL
  #   or <- NULL
  #   if(n_organism()>1){
  #     for(category in levels(as.factor(as.character(all_categories)))){
  #       idx = which(all_categories == category)
  #       pheno.vec = rep(pheno,length(idx))
  #       tmp.vec = as.vector(tmp[,idx])
  #       
  #       stats = t.test(tmp.vec[which(pheno.vec == levels(pheno.vec)[1])],tmp.vec[which(pheno.vec == levels(pheno.vec)[2])])
  #       tmp2 = stats$p.val
  #       tmp.or = cohen.d(f=tmp.vec[which(pheno.vec == levels(pheno.vec)[1])],d=tmp.vec[which(pheno.vec == levels(pheno.vec)[2])])$estimate
  #       or <- c(or,tmp.or)
  #       # get pvalue sign (direction of enrichment)
  #       signs = c(signs,sign(tmp.or))
  #       pvals = c(pvals,-log10(tmp2))
  #     }
  #     if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #     # define color for each bar based on significance and direction of effect
  #     myPalette = rep('grey',nlevels(as.factor(as.character(all_categories))))
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs < 0)] = 'darkgreen'
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs > 0)] = 'darkviolet'
  #     
  #     names.legend = rep("non significant",length(myPalette))
  #     names.legend[which(myPalette == 'darkgreen')] = paste0("enriched in ",levels(pheno)[1])
  #     names.legend[which(myPalette == 'darkviolet')] = paste0("enriched in ",levels(pheno)[2])
  #     
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     
  #     #plot each well signed pvalue
  #     plot_ly(source = 'source.cat.kine',
  #             y = levels(as.factor(as.character(all_categories))),
  #             x = or,
  #             name = names.legend,
  #             type = "bar",
  #             orientation = 'h',
  #             #marker = list(color = myPalette)
  #             color = I(myPalette)#factor(names.legend,levels = c(paste0("enriched in ",levels(pheno)[1]),paste0("enriched in ",levels(pheno)[2]),"non significant"))
  #     )%>%
  #       layout(title=list(text=paste0(c(rep("&nbsp;", 3),
  #                                       levels(pheno)[1],
  #                                       rep("&nbsp;", 10),
  #                                       '|',
  #                                       rep("&nbsp;", 10),
  #                                       levels(pheno)[2],
  #                                       rep("\n&nbsp;", 3)),
  #                                     collapse = ""),xanchor = "right"),yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),
  #                                                                                                          "category",
  #                                                                                                          rep("&nbsp;", 20),
  #                                                                                                          rep("\n&nbsp;", 3)),
  #                                                                                                        collapse = "")), xaxis=list(title="Cohen's d"),showlegend = TRUE,font=t)
  #   }else{
  #     for(category in levels(as.factor(as.character(all_categories)))){
  #       idx = which(all_categories == category)
  #       tmp.vec = as.vector(tmp[,idx])
  #       pvals = c(pvals,mean(tmp.vec))
  #     }
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     
  #     #plot each well signed pvalue
  #     plot_ly(source = 'source.cat.kine',
  #             y = levels(as.factor(as.character(all_categories))),
  #             x = pvals,
  #             type = "bar",
  #             orientation = 'h'
  #     )%>%
  #       layout(yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),
  #                                                 "category",
  #                                                 rep("&nbsp;", 20),
  #                                                 rep("\n&nbsp;", 3)),
  #                                               collapse = "")), xaxis=list(title=paste("Mean",rv$kine,"value")),font=t)
  #     
  #   }
  # })
  # 
  # 
  # output$downloadDatamanualAnnoKine <- downloadHandler(
  #   
  #   filename = "results_categoriesKine.csv",
  #   content = function(file) {
  #     #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #     if(is.null(rv$anno)) return(NULL)
  #     #test if QC data are available
  #     if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #     inFile <- path_metadata_kine()#input$metadataFileKine
  #     if(n_organism()>1){
  #       
  #       if (nrow(inFile)==0)
  #         return(NULL)
  #       #read metadata
  #       metadata = read.table(inFile$datapath, header = FALSE)
  #       # reorder metadata to fit the Growth profile order
  #       metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #       #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #       #TODO: implement a match/check
  #       pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #     }
  #     # get all unique wells
  #     all_wells <- sort(unique(unlist(rv$data$fw)))
  #     
  #     # fill profiles
  #     merge_profiles = do.call(what = 'c',rv$data$fits)
  #     
  #     profiles.fill = lapply(1:length(merge_profiles),function(i){
  #       fits = merge_profiles[[i]]
  #       rownames(fits) = merge_profiles[[i]][,1]
  #       
  #       # remove column with well names and put them as rownames
  #       fits =fits[,-1]
  #       # remove last row which is empty
  #       fits = fits[-nrow(fits),]
  #       # get all the missing wells
  #       idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #       if(length(idx) < length(all_wells)){
  #         add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #         rownames(add) = idx
  #         colnames(add) = colnames(fits)
  #         tmp = rbind(fits,add)
  #         #rownames(tmp) = c(rownames(fits),idx)
  #         tmp = tmp[order(row.names(tmp)),]
  #         return(tmp)
  #       }else{ # create a zero matrix if all the wells are missing
  #         tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #         rownames(tmp) = all_wells
  #         colnames(tmp) = colnames(fits)
  #         return(tmp)
  #       }
  #     })
  #     
  #     # presence/absence statistical test for each well
  #     tmp = lapply(profiles.fill,function(x) as.numeric(x[,input$kinefeature]))
  #     tmp = do.call('rbind',tmp)
  #     
  #     #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #     
  #     
  #     all_categories = factor(rv$matchTable$manualAnno)[match(all_wells,rv$matchTable$wellID)]
  #     
  #     pvals <- NULL
  #     signs <- NULL
  #     or <- NULL
  #     if(n_organism()>1){
  #       # Fisher test per category, but only using the filtered wells
  #       for(category in levels(as.factor(as.character(all_categories)))){
  #         idx = which(all_categories == category)
  #         pheno.vec = rep(pheno,length(idx))
  #         tmp.vec = as.vector(tmp[,idx])
  #         
  #         stats = t.test(tmp.vec[which(pheno.vec == levels(pheno.vec)[1])],tmp.vec[which(pheno.vec == levels(pheno.vec)[2])])
  #         tmp2 = stats$p.val
  #         tmp.or = cohen.d(f=tmp.vec[which(pheno.vec == levels(pheno.vec)[1])],d=tmp.vec[which(pheno.vec == levels(pheno.vec)[2])])$estimate
  #         or <- c(or,tmp.or)
  #         # get pvalue sign (direction of enrichment)
  #         signs = c(signs,sign(tmp.or))
  #         pvals = c(pvals,-log10(tmp2))
  #       }
  #       if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #       db = cbind(levels(all_categories),or,10^-pvals)
  #       colnames(db) = c('Category','CohenD','pval')
  #     }else{
  #       
  #       for(category in levels(as.factor(as.character(all_categories)))){
  #         idx = which(all_categories == category)
  #         tmp.vec = as.vector(tmp[,idx])
  #         pvals = c(pvals,mean(tmp.vec))
  #       }
  #       
  #       db = cbind(levels(all_categories),pvals)
  #       colnames(db) = c('Category','average')
  #     }
  #     write.csv(db, file, row.names = FALSE)
  #   }
  # )
  # 
  # 
  # # Coupled hover event
  # output$selectedCatInfoWellKine <- renderText({
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   # Read in hover data
  #   eventdata <- event_data("plotly_click", source = "source.cat.kine")
  #   validate(need(!is.null(eventdata), "Click on one bar to show corresponding category information"))
  #   # Get cluster number (hovered)
  #   catNumber <- eventdata$y
  #   
  #   
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   all_categories = factor(rv$matchTable$manualAnno)[match(all_wells,rv$matchTable$wellID)]
  #   idx = which(all_categories == catNumber)
  #   
  #   print(paste0('Selected cluster: ',catNumber,'\n',
  #                '\t\t\tIt contains the following wells: ',list(all_wells[idx]),'\n'))
  #   
  # })
  # 
  # # Coupled hover event
  # output$selectedCatInfoNameKine <- renderText({
  #   
  #   # Read in hover data
  #   eventdata <- event_data("plotly_click", source = "source.cat.kine")
  #   validate(need(!is.null(eventdata), "Click on one bar to show corresponding cluster information"))
  #   # Get cluster number (hovered)
  #   catNumber <- eventdata$y
  #   
  #   
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   all_categories = factor(rv$matchTable$manualAnno)[match(all_wells,rv$matchTable$wellID)]
  #   idx = which(all_categories == catNumber)
  #   
  #   
  #   print(paste0('\t\t\tIt contains the following carbon sources: ',list(as.character(rv$matchTable$sourceName[match(all_wells[idx],rv$matchTable$wellID)])),'\n'))
  #   
  # })
  # 
  # 
  # # chemoinformatic cluster comparison
  # output$chemoinfoPlotKine <- renderPlotly({
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   #test if QC data are available
  #   if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #   inFile <- path_metadata_kine()#input$metadataFileKine
  #   if(n_organism()>1){
  #     if (nrow(inFile)==0)
  #       return(NULL)
  #     #read metadata
  #     metadata = read.table(inFile$datapath, header = FALSE)
  #     # reorder metadata to fit the Growth profile order
  #     metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #     #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #     #TODO: implement a match/check
  #     pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #   }
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   
  #   # fill profiles
  #   merge_profiles = do.call(what = 'c',rv$data$fits)
  #   
  #   profiles.fill = lapply(1:length(merge_profiles),function(i){
  #     fits = merge_profiles[[i]]
  #     rownames(fits) = merge_profiles[[i]][,1]
  #     
  #     # remove column with well names and put them as rownames
  #     fits =fits[,-1]
  #     # remove last row which is empty
  #     fits = fits[-nrow(fits),]
  #     # get all the missing wells
  #     idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #     if(length(idx) < length(all_wells)){
  #       add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #       rownames(add) = idx
  #       colnames(add) = colnames(fits)
  #       tmp = rbind(fits,add)
  #       #rownames(tmp) = c(rownames(fits),idx)
  #       tmp = tmp[order(row.names(tmp)),]
  #       return(tmp)
  #     }else{ # create a zero matrix if all the wells are missing
  #       tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #       rownames(tmp) = all_wells
  #       colnames(tmp) = colnames(fits)
  #       return(tmp)
  #     }
  #   })
  #   
  #   # presence/absence statistical test for each well
  #   tmp = lapply(profiles.fill,function(x) as.numeric(x[,input$kinefeature]))
  #   tmp = do.call('rbind',tmp)
  #   
  #   #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #   
  #   pvals <- NULL
  #   signs <- NULL
  #   or <- NULL
  #   
  #   all_categories = factor(paste0('cluster',factor(rv$matchTable$chemoCluster)[match(all_wells,rv$matchTable$wellID)]))
  #   if(n_organism()>1){
  #     # Fisher test per category, but only using the filtered wells
  #     for(category in levels(all_categories)){
  #       idx = which(all_categories == category)
  #       pheno.vec = rep(pheno,length(idx))
  #       tmp.vec = as.vector(tmp[,idx])
  #       
  #       stats = t.test(tmp.vec[which(pheno.vec == levels(pheno.vec)[1])],tmp.vec[which(pheno.vec == levels(pheno.vec)[2])])
  #       tmp2 = stats$p.val
  #       tmp.or = cohen.d(f=tmp.vec[which(pheno.vec == levels(pheno.vec)[1])],d=tmp.vec[which(pheno.vec == levels(pheno.vec)[2])])$estimate
  #       or <- c(or,tmp.or)
  #       # get pvalue sign (direction of enrichment)
  #       signs = c(signs,sign(tmp.or))
  #       pvals = c(pvals,-log10(tmp2))
  #     }
  #     if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #     # define color for each bar based on significance and direction of effect
  #     myPalette = rep('grey',nlevels(all_categories))
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs < 0)] = 'darkgreen'
  #     myPalette[which(abs(signs*pvals) > -log10(0.05) & signs > 0)] = 'darkviolet'
  #     
  #     names.legend = rep("non significant",length(myPalette))
  #     names.legend[which(myPalette == 'darkgreen')] = paste0("enriched in ",levels(pheno)[1])
  #     names.legend[which(myPalette == 'darkviolet')] = paste0("enriched in ",levels(pheno)[2])
  #     
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     
  #     #plot each well signed pvalue
  #     plot_ly(source = 'source.kine',
  #             y = levels(all_categories),
  #             x = or,
  #             name = names.legend,
  #             type = "bar",
  #             orientation = 'h',
  #             #marker = list(color = myPalette),
  #             color = I(myPalette)#factor(names.legend,levels = c(paste0("enriched in ",levels(pheno)[1]),paste0("enriched in ",levels(pheno)[2]),"non significant"))
  #     )%>%
  #       layout(title=list(text=paste0(c(rep("&nbsp;", 3),
  #                                       levels(pheno)[1],
  #                                       rep("&nbsp;", 20),
  #                                       levels(pheno)[2],
  #                                       rep("\n&nbsp;", 3)),
  #                                     collapse = ""),xanchor = "right"),yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),
  #                                                                                                          "cluster",
  #                                                                                                          rep("&nbsp;", 20),
  #                                                                                                          rep("\n&nbsp;", 3)),
  #                                                                                                        collapse = "")), xaxis=list(title="Cohen's d"),showlegend = TRUE,font=t)
  #   }else{
  #     for(category in levels(as.factor(as.character(all_categories)))){
  #       idx = which(all_categories == category)
  #       tmp.vec = as.vector(tmp[,idx])
  #       pvals = c(pvals,mean(tmp.vec))
  #     }
  #     
  #     t <- list(
  #       family = "sans serif",
  #       size = 14,
  #       color = 'black')
  #     
  #     
  #     #plot each well signed pvalue
  #     plot_ly(source = 'source.kine',
  #             y = levels(all_categories),
  #             x = pvals,
  #             type = "bar",
  #             orientation = 'h'
  #     )%>%
  #       layout(yaxis = list(size=8,title=paste0(c(rep("&nbsp;", 20),
  #                                                 "cluster",
  #                                                 rep("&nbsp;", 20),
  #                                                 rep("\n&nbsp;", 3)),
  #                                               collapse = "")), xaxis=list(title=paste("Mean",rv$kine,"value")),font=t)
  #   }
  #   
  # })
  # 
  # 
  # output$downloadDatachemoinfoKine <- downloadHandler(
  #   
  #   filename = "results_chemoclusterKine.csv",
  #   content = function(file) {
  #     #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #     if(is.null(rv$anno)) return(NULL)
  #     #test if QC data are available
  #     if(is.null(rv$data)) print('User is required to run the quality control analysis first.')
  #     inFile <- path_metadata_kine()#input$metadataFileKine
  #     if(n_organism()>1){
  #       if (nrow(inFile)==0)
  #         return(NULL)
  #       #read metadata
  #       metadata = read.table(inFile$datapath, header = FALSE)
  #       # reorder metadata to fit the Growth profile order
  #       metadata = metadata[match(names(rv$data$fits),metadata$V1),]
  #       #define phenotype vector: for now it assumes that the metadata file is sorted in the same order than the organisms files
  #       #TODO: implement a match/check
  #       pheno = rep(metadata[,2],times=sapply(rv$data$fits,length))
  #     }
  #     # get all unique wells
  #     all_wells <- sort(unique(unlist(rv$data$fw)))
  #     
  #     # fill profiles
  #     merge_profiles = do.call(what = 'c',rv$data$fits)
  #     
  #     profiles.fill = lapply(1:length(merge_profiles),function(i){
  #       fits = merge_profiles[[i]]
  #       rownames(fits) = merge_profiles[[i]][,1]
  #       
  #       # remove column with well names and put them as rownames
  #       fits =fits[,-1]
  #       # remove last row which is empty
  #       fits = fits[-nrow(fits),]
  #       # get all the missing wells
  #       idx = all_wells[which(!(all_wells%in%row.names(fits)))]
  #       if(length(idx) < length(all_wells)){
  #         add = matrix(0,nrow=length(idx),ncol=ncol(fits))
  #         rownames(add) = idx
  #         colnames(add) = colnames(fits)
  #         tmp = rbind(fits,add)
  #         #rownames(tmp) = c(rownames(fits),idx)
  #         tmp = tmp[order(row.names(tmp)),]
  #         return(tmp)
  #       }else{ # create a zero matrix if all the wells are missing
  #         tmp = matrix(0,nrow=length(all_wells),ncol=ncol(fits))
  #         rownames(tmp) = all_wells
  #         colnames(tmp) = colnames(fits)
  #         return(tmp)
  #       }
  #     })
  #     
  #     # presence/absence statistical test for each well
  #     tmp = lapply(profiles.fill,function(x) as.numeric(x[,input$kinefeature]))
  #     tmp = do.call('rbind',tmp)
  #     
  #     #Each well close from signifance is represented by the contingency table (row: presence/absence of growth, column: group1 or 2).
  #     
  #     pvals <- NULL
  #     signs <- NULL
  #     or <- NULL
  #     
  #     all_categories = factor(paste0('cluster',factor(rv$matchTable$chemoCluster)[match(all_wells,rv$matchTable$wellID)]))
  #     if(n_organism()>1){
  #       # Fisher test per category, but only using the filtered wells
  #       for(category in levels(all_categories)){
  #         idx = which(all_categories == category)
  #         pheno.vec = rep(pheno,length(idx))
  #         tmp.vec = as.vector(tmp[,idx])
  #         
  #         stats = t.test(tmp.vec[which(pheno.vec == levels(pheno.vec)[1])],tmp.vec[which(pheno.vec == levels(pheno.vec)[2])])
  #         tmp2 = stats$p.val
  #         tmp.or = cohen.d(f=tmp.vec[which(pheno.vec == levels(pheno.vec)[1])],d=tmp.vec[which(pheno.vec == levels(pheno.vec)[2])])$estimate
  #         or <- c(or,tmp.or)
  #         # get pvalue sign (direction of enrichment)
  #         signs = c(signs,sign(tmp.or))
  #         pvals = c(pvals,-log10(tmp2))
  #       }
  #       if(length(which(is.infinite(or))) > 0) or[which(is.infinite(or))] <- 0
  #       db = cbind(levels(all_categories),or,10^-pvals)
  #       colnames(db) = c('Chemocluster','CohenD','pval')
  #     }else{
  #       for(category in levels(as.factor(as.character(all_categories)))){
  #         idx = which(all_categories == category)
  #         tmp.vec = as.vector(tmp[,idx])
  #         pvals = c(pvals,mean(tmp.vec))
  #       }
  #       db = cbind(levels(all_categories),pvals)
  #       colnames(db) = c('Chemocluster','average')
  #     }
  #     write.csv(db, file, row.names = FALSE)
  #   }
  # )
  # 
  # 
  # # Coupled hover event
  # output$selectedClusterInfoWellKine <- renderText({
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   # Read in hover data
  #   eventdata <- event_data("plotly_click", source = "source.kine")
  #   validate(need(!is.null(eventdata), "Click on one bar to show corresponding cluster information"))
  #   # Get cluster number (hovered)
  #   clusterNumber <- eventdata$y
  #   
  #   
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   all_categories = factor(paste0('cluster',factor(rv$matchTable$chemoCluster)[match(all_wells,rv$matchTable$wellID)]))
  #   idx = which(all_categories == clusterNumber)
  #   
  #   print(paste0('Selected cluster: ',clusterNumber,'\n',
  #                '\t\t\tIt contains the following wells: ',list(all_wells[idx]),'\n'))
  #   
  # })
  # 
  # # Coupled hover event
  # output$selectedClusterInfoNameKine <- renderText({
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   # Read in hover data
  #   eventdata <- event_data("plotly_click", source = "source.kine")
  #   validate(need(!is.null(eventdata), "Click on one bar to show corresponding cluster information"))
  #   # Get cluster number (hovered)
  #   clusterNumber <- eventdata$y
  #   
  #   
  #   # get all unique wells
  #   all_wells <- sort(unique(unlist(rv$data$fw)))
  #   all_categories = factor(paste0('cluster',factor(rv$matchTable$chemoCluster)[match(all_wells,rv$matchTable$wellID)]))
  #   idx = which(all_categories == clusterNumber)
  #   
  #   print(paste0('\t\t\tIt contains the following carbon sources: ',list(as.character(rv$matchTable$sourceName[match(all_wells[idx],rv$matchTable$wellID)])),'\n'))
  #   
  # })
  # 
  # # Coupled hover event
  # output$selectedClusterInfoKEGGKine <- renderText({
  #   #this option is only available for Biolog plates already annotated (AN, PM1, PM2A)
  #   if(is.null(rv$anno)) return(NULL)
  #   # Read in hover data
  #   eventdata <- event_data("plotly_click", source = "source.kine")
  #   validate(need(!is.null(eventdata), "Click on one bar to show corresponding cluster information"))
  #   # Get cluster number (hovered)
  #   clusterNumber <- eventdata$y
  #   
  #   anno = rv$anno$ENR
  #   names(anno) = paste0('cluster',1:length(anno))
  #   
  #   if(length(anno[[clusterNumber]]) > 0){
  #     
  #     # get all unique wells
  #     all_wells <- sort(unique(unlist(rv$data$fw)))
  #     all_categories = factor(paste0('cluster',factor(rv$matchTable$chemoCluster)[match(all_wells,rv$matchTable$wellID)]))
  #     idx = which(all_categories == clusterNumber)
  #     
  #     tmp.anno = matrix(anno[[clusterNumber]],ncol=2)
  #     
  #     if(length(idx.neg <- which(tmp.anno[,2] < 0)) > 0){
  #       enriched_path = tmp.anno[-idx.neg,1]
  #     }else{
  #       enriched_path = tmp.anno[,1]
  #     }
  #     if(length(enriched_path) > 0){
  #       
  #       print(paste0('\t\t\tIt is enriched in the following KEGG pathways: ','\n',
  #                    list(enriched_path))) # remove negative enrichment as it is confusing the interpretation
  #     }else{
  #       print('No KEGG enrichment was found for this cluster\n')
  #     }
  #   }else{
  #     print('No KEGG enrichment was found for this cluster\n')
  #   }
  #   
  # })
  # 
  # url <- a("GrowthCurver manual", href="https://cran.r-project.org/web/packages/growthcurver/vignettes/Growthcurver-vignette.html")
  # output$tab <- renderUI({
  #   tagList(url)
  # })
  
})