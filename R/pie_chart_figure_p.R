#' pie_chart_figure_p
#'
#' @param data_table_list
#' @param merge_variable
#' @param title_name
#' @param character_list
#' @param legend_shown
#' @param source_citation
#'
#' @return figure
#' @export
pie_chart_figure_p <- function(data_table_list,merge_variable=NULL,title_name=NULL,character_list=NULL,legend_shown=FALSE,source_citation=NULL){
  #data_table_list is a list of data tables which should be ready to be merged into one table
  #       *if only one table is included in input list (note that it still must be in list form), this table should be ready to be plotted i.e it should include a variable and value column and an x-value (usually date or year) column
  #       *value may be in GWh or whatever is the unit of what is being plotted, the values need not add to 100% or 1 they can be actual values
  #merge_variable is a character description of which variable the merge should be performed on (ex:"date","year) if applicable
  #title_name defaults to NULL but can be set as a character if a title is desired
  #character_list defaults to NULL, but if applicable should be a list of the relevant database data table names as strings, so that their source info can be extracted from metadata table
  #legend_shown defaults to FALSE
  #       *if FALSE, no legend is shown and the name of each category and associated percent is displayed on the pie slice
  #       *if TRUE, legend is shown and only the percent is displaye on the pie slice, this may be a better optio if some slices are very small
  #source_citation defaults to NULL, however if a source can't be pulled from the metadata table, the source should be set to be the citation description
  #       *if needed to be set, should be of form: "Source: U.S. Energy Information Administration" for example
  #eventually when a custom theme is set, we can store the colors from that theme in a character vector called "theme_colors" then include "marker=list(colors=theme_colors)" as argument in plotly function

  library(plotly)
  if(!("Hmisc" %in% installed.packages())) install.packages("Hmisc")
  library("Hmisc") #Hmisc package includes a capitilization function which is utilized to get legend labels
  library(scales) #contains ggplot default palette function

  working_table <- NULL

  #creating one working table by merging tables in input list
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
  lf_working_table[,variable:=gsub("^ros","Rest of state",variable)]
  lf_working_table[,variable:=gsub("co2","CO2",variable)] #specific CO2 case
  lf_working_table[,variable:=gsub("gdp","GDP",variable)] #specific GDP case
  lf_working_table[,variable:=gsub("gwh","GWh",variable)]
  lf_working_table[,variable:=capitalize(variable)] #capitalizes first word of legend labels

  #building source citation if no source citation is given as an input
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
  {source_description <- source_citation} #using input source citation if given

  category_count <- length(lf_working_table$variable) #counts number of categories being graphed
  theme_colors <- c("#00A087B2", "#3C5488B2", "#CEA5AC", "#BE7E8A", "#4DBBD5B2", "#91D1C2B2","#D9C6C9","#8491B4B2","#5868AC","#6FB3D9","#56BD96","#99A9E2","#A94F64","#B0DEFA","#99EEBB","#8FD3FE")[1:category_count] #generates a list of that many colors to be assigned to each color (for now from ggplot default color palette)

  # Create sector labels
  total_sum <- lf_working_table[value>=0,sum(value)]
  pct <- lf_working_table[,round(value/total_sum,3)]
  pct[pct<0.1] <- 0  # Anything less than 10% should be blank
  pct <- paste0(pct*100, "%")
  pct <- gsub("^0%","",pct)

  if (legend_shown==FALSE){
    figure <- plot_ly(lf_working_table,labels=~variable,values=~value,type='pie',hoverinfo="percent+label",marker=list(colors=theme_colors),sort=F,textinfo = "label+percent") %>%
      layout(title=list(text=title_name,x=0,xref='paper',yref='paper',font=list(size=15,family = "Helvetica",color="dimgrey")),
             showlegend=F,
             annotations=list(x=0.5,y=-0.1,text=paste0("<i>","<sub>",source_description,"<sub>","</i>"),showarrow=F,xref='paper',yref='paper',font=list(size=14,family = "Helvetica",color="dimgrey")),
             font = list(family="Helvetica",color="dimgrey"),
             paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")%>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","select2d","lasso2d","hoverCompareCartesian","zoom2d","autoScale2d","resetScale2d"))
  }
  else{
    figure <- plot_ly(lf_working_table,labels=~variable,values=~value,type='pie',hoverinfo="percent+label",marker=list(colors=theme_colors),sort=F,text=pct,textposition = "inside",textinfo = "text") %>%
      layout(title=list(text=title_name,x=0.5,xref='paper',yref='paper',font=list(size=15,family = "Helvetica",color="dimgrey")),
             annotations=list(x=0.5,y=-0.1,text=paste0("<i>","<sub>",source_description,"<sub>","</i>"),showarrow=F,xref='paper',yref='paper',font=list(size=14,family = "Helvetica",color="dimgrey")),
             font = list(family="Helvetica",color="dimgrey"),
             paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0",
             legend = list(x = 100, y = 0.5))%>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","select2d","lasso2d","hoverCompareCartesian","zoom2d","autoScale2d","resetScale2d"))
  }
  return(figure)
}
