#' line_figure
#'
#' @param data_table_list
#' @param merge_variable
#' @param value_unit
#' @param title_name
#' @param character_list
#' @param x_label
#' @param lower_limit
#' @param upper_limit
#' @param return_static
#' @param source_citation
#' @param modifications
#' @param subtitle_description
#' @param future_date
#'
#' @return figure
#' @export
line_figure <- function(data_table_list,merge_variable,value_unit,title_name,character_list=NULL,x_label="Year",lower_limit=0,upper_limit=NA,return_static=TRUE,source_citation=NULL,modifications=NULL,subtitle_description=NULL,future_date=NULL){
  #data_table_list is a list of data tables which should be ready to be merged into one table
  #       *if only one table is included in input list (note that it still must be in list form), this table should be ready to be plotted i.e it should include a variable and value column and an x-value (usually date or year) column
  #merge_variable is a character description of which variable the merge should be performed on (ex:"date","year) if applicable; it should also be the x-axis being graphed
  #value_unit = character description of units of value being plotted
  #title_name = character description of what title of figure should be
  #character_list defaults to NULL, but if applicable should be a list of the relevant database data table names as strings, so that their source info can be extracted from metadata table
  #x_label defaults to "Year" but can be substituted with another character if Year is not appropriate xlabel
  #lower_limit defaults to 0, but can be changed to another numeric value appropriate for the data
  #upper_limit defaults to NA, but can be adjusted if needed
  #return_static defaults to TRUE, in which case a ggplot opbject will be returned
  #       *if FALSE, a list containing the ggplot figure, the x axis label name, and the citation will be returned
  #source_citation defaults to NULL, however if a source can't be pulled from the metadata table, the source should be set to be the citation description
  #       *if needed to be set, should be of form: "Source: U.S. Energy Information Administration" for example
  #modifications defaults to NULL, in which case nothing would be added to the figure, but can be set if additional modifications are needed
  #       *examples of different modifications which may be necessary are scaling the y-axis or removing the legend
  #subtitle_description defaults to NULL, but can be added if desired
  #future_date defaults to NULL and should only be given a value if the plot is showing future and past values, and it is desired that the future values be dashed
  #       *the format of future_date must match the format of whatever the x unit variable is
  #       *ex: if year is on x-axis, future_date = 2021 or if date (in y/m/d format) is on x-axis, future_date = '2021-01-01'
  #       *note also that this only works when there is both historic and future data

  library(ggplot2)
  if(!("Hmisc" %in% installed.packages())) install.packages("Hmisc")
  library("Hmisc") #Hmisc package includes a capitilization function which is utilized to get legend labels

  working_table <- NULL

  for(table in data_table_list){
    if (is.null(working_table))
    {working_table <- table}
    else
    {working_table <- merge(working_table, table[], by = merge_variable, all=TRUE)}
  }

  if(length(data_table_list)==1) #accounts for possibility that it is necessary for data table to be constructed outside function, in which case only one data table will be listed as input
  {lf_working_table <- working_table}
  else #if multiple tables are listed as input, will melt the merged tables into their longform by merge variable
  {lf_working_table <- melt(working_table,id=merge_variable)}

  lf_working_table[,variable:=as.character(variable)]
  lf_working_table <- lf_working_table[order(variable)] #alphabetizes variable elements
  lf_working_table[,variable:=gsub("solar_utility","Solar, utility",variable)]
  lf_working_table[,variable:=gsub("solar_distributed","Solar, distributed",variable)]
  lf_working_table[,variable:=gsub("_"," ",variable)] #subtitutes "_" from variable name with a space to create legend labels
  lf_working_table[,variable:=gsub("apco","APCO",variable)] #deals with specific case if "apco" is included in a variable name, APCO will be used in the legend label
  lf_working_table[,variable:=gsub("dom","Dominion",variable)]
  lf_working_table[,variable:=gsub("^ros","Rest of State",variable)]
  lf_working_table[,variable:=gsub("co2","CO2",variable)] #specific CO2 case
  lf_working_table[,variable:=gsub("gdp","GDP",variable)] #specific GDP case
  lf_working_table[,variable:=gsub("gwh","GWh",variable)]
  lf_working_table[,variable:=capitalize(variable)] #capitalizes first word of legend labels

  setnames(lf_working_table,merge_variable,"x_unit")

  if(is.null(source_citation)){
    source_list <- NULL

    for(table in character_list){
      source <- metadata[db_table_name==table,data_source_full_name]

      if(is.null(source_list))
      {source_list <- source}
      else
      {source_list <- c(source_list,source)}
    }
    source_list <-as.vector(unique(source_list))

    source_description <- NULL

    for (source in source_list){
      if(is.null(source_description))
      {source_description <- paste("Source:",source)}
      else
      {source_description <- paste0(source_description,", ",source)}
    }
  }
  else
  {source_description <- source_citation}

  category_count <- length(unique(lf_working_table$variable))
  ceps_pal <- c("#00A087B2", "#3C5488B2", "#CEA5AC", "#BE7E8A", "#4DBBD5B2", "#91D1C2B2","#D9C6C9","#8491B4B2","#5868AC","#6FB3D9","#56BD96","#99A9E2","#A94F64","#B0DEFA","#99EEBB","#8FD3FE")

  if(is.null(future_date)){
    figure <- ggplot(lf_working_table, aes(x=x_unit,y=value,color=variable)) +
      geom_line(aes(group=variable,text=paste0(x_label,": ",x_unit,"\n","Value: ",round(value,4),"\n","Variable: ",variable))) +
      ylab(value_unit) + xlab(x_label) + ylim(lower_limit,upper_limit) +
      labs(title=title_name,subtitle=subtitle_description,caption=source_description) +
      scale_color_manual(name=NULL,values=ceps_pal[1:category_count])+
      theme_ceps()+
      modifications
    figure
  }
  else{
    figure <- ggplot() +
      geom_line(data=lf_working_table[x_unit<future_date],mapping=aes(x=x_unit,y=value,color=variable,group=variable,text=paste0(x_label,": ",x_unit,"\n","Value: ",round(value,4),"\n","Variable: ",variable))) +
      geom_line(data=lf_working_table[x_unit>=future_date],mapping=aes(x=x_unit,y=value,color=variable,group=variable,text=paste0(x_label,": ",x_unit,"\n","Value: ",round(value,4),"\n","Variable: ",variable)),linetype="dashed") +
      ylab(value_unit) + xlab(x_label) + ylim(lower_limit,upper_limit) +
      labs(title=title_name,subtitle=subtitle_description,caption=source_description) +
      scale_color_manual(name=NULL,values=ceps_pal[1:category_count])+
      theme_ceps()+
      modifications
    figure
  }

  return_list <- list(figure=figure,x_label=x_label,source_description=source_description,title_name=title_name,subtitle_description=subtitle_description,y_label=value_unit)

  if(return_static==TRUE)
  {return(figure)}
  else
  {return(return_list)}
}
