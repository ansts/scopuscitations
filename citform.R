citform=function(cc, fl=T){
 if (fl) Authors=cc$authorships$display_name else Authors=cc$authorships[[1]]$display_name
 Title=cc$display_name
 Year=cc$publication_year
 Journal=cc$source_display_name
 Volume=cc$volume
 Issue=cc$issue 
 Page_f=cc$first_page
 Page_l=cc$last_page
 DOI=cc$doi
 Link=cc$landing_page_url
 ISSN=paste("ISSN:",cc$issn_l, collapse="")
 str1=paste(c(paste(Authors, collapse=", "),Title, Journal), collapse=". ")
 str2=paste(c(Year,", ", Volume,"(",Issue,"), ", Page_f,"-",Page_l,", "), collapse="")
 str3=paste(c(DOI,ISSN),collapse=", ")
 strout=paste(c(str1,str2,str3,"."), collapse="")
 return(strout)
}