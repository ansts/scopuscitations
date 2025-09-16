require(openalexR)
require(parallel)
require(pbapply)
require(future.apply)
require(reshape2)
require(dplyr)
require(rmarkdown)
require(knitr)

# opeanalexR native ------------------------------------------------------------
options(openalexR.mailto = "a_pashov@microbio.bas.bg")
Q="Anastas Pashov"
my_profile <- oa_fetch(
  entity = "authors",
  display_name.search = Q
)
my_works <- oa_fetch(
  entity = "works",
  author.orcid = "0000-0002-6033-3566", 
  verbose = TRUE
)
cits=lapply(my_works$id,\(id) {
  citing_works <- oa_fetch(
    entity = "works",
    cites = id,  
    publication_year = "1993-2025",
    count_only = FALSE
  )
})

noncits=which(lengths(cits)==0)
my_works=my_works[-noncits,]
cits=cits[-noncits]

theList0=pblapply(my_works$id,\(id){
    j=my_works$id==id
    # print(id)
    auth=my_works$authorships[j][[1]]$display_name
    dn=lapply(cits[j][[1]]$authorships,\(cja) cja$display_name)
    authocit=unique(unlist(lapply(auth, \(ai) grep(ai,dn))))
    if (nrow(cits[j][[1]])==0) return(NA)
    cj=cits[j][[1]][-authocit,]
    if (nrow(cj)==0) return(NA)
    citlist=apply(cj,1, citform)
    myref=my_works[my_works$id==id,]
    list(citform(myref, fl=F),"цитирана в: ",citlist)
})
  
theList0=theList0[!is.na(theList0)]
n=sapply(theList0,\(l) length(l[[3]])) 


sapply(theList0,\(l){
  print(l[[1]])
  print("")
  print(l[[2]])
  print("")
  for (i in seq_along(l[[3]])){
    print(l[[3]][[i]])
    print("")
  }
})


# based on Scopus download -----------------------------------------------------
rawdata=read.csv("export_cit.csv")
dois=rawdata$DOI;idoi=seq_along(dois)
j0=which(nchar(dois)==0)
dois=dois[-j0];idoi=idoi[-j0]
x=pblapply(dois,\(di) oa_fetch(entity="works", doi=di, output="list"))
j=which(lengths(x)==0)
x=x[-j]

refs=pblapply(x,\(xi){
  refi=xi[[1]]$referenced_works
  if (length(refi)==0) {
    return("NA")
  }
  rai=oa_fetch(entity = "works", identifier = refi)
  raia=rai$authorships
  auq=grep(Q,lapply(raia,\(ri) {
    ri$display_name
  }))
  y=lapply(auq,\(ai){
    z=rai$id[[ai]]
    refi0=oa_fetch(entity = "works", identifier = z)
    y=paste(paste0(unlist(refi0$authorships[[1]][2]),collapse=","),
                 refi0$title,
                 refi0$source_display_name, 
                 refi0$publication_year, 
                 paste0(refi0$volume,"(",refi0$issue,")",collapse=""),
                 paste(refi0$first_page, refi0$last_page, sep="-"), sep=", ")
  })
  return(y)
})
save(refs, file="refs")
names(refs)=dois[-j]
z=lapply(refs,\(l) {
  if (length(l)>0) {
    n=paste("|",seq_along(l))
    names(l)=n
    return(l)
  } else return("NA")
})
refsflat=unlist(z)
nrf=names(refsflat)
nrf=sapply(nrf, \(ni) substr(ni,1,nchar(ni)-4))
refagg=aggregate(nrf, by=list(refsflat), list)

trimmed=rawdata[-j0,][-j,]
namesToUse=c("Authors","Title","Year","Source.title","Volume","Issue","Page.start","Page.end","DOI","Link")
trimmed=trimmed[,namesToUse]
theList=apply(refagg,1,\(ci){
  ri=trimmed[trimmed$DOI %in% ci[[2]][[1]],]
  ri=apply(ri,1,\(rii){
    rii1=paste(rii$Authors,rii$Title, collapse=". ")
    # .... (unfinished)
  })
  list(ci[[1]],ri)
})
#-------------------------------------