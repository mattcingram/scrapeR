# scrape for github and container

# packages
library(pacman)
p_load(RSelenium,
       rvest, 
       stringr,
       data.table,
       devtools,
       ggplot2, forcats,
       stargazer,
       stringr)

# directories

# create subdirectories
dir.create('./code', showWarnings = TRUE)
dir.create('./data', showWarnings = TRUE)
dir.create('./data/original', showWarnings = TRUE)
dir.create('./data/working', showWarnings = TRUE)
dir.create('./figures', showWarnings = TRUE)
dir.create('./tables', showWarnings = TRUE)

#

urls <- c(
  "https://scholar.google.com/citations?mauthors=label%3Apublic_law&hl=en&view_op=search_authors",
  "https://scholar.google.com/citations?mauthors=label%3Ajudicial_politics&hl=en&view_op=search_authors",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:american_politics",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:comparative_politics",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:international_relations",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:political_theory",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:political_methodology",
  "https://scholar.google.com/citations?view_op=search_authors&hl=en&mauthors=label:public_policy",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:political_science",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:politics",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:government",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:sociology",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:anthropology",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:history",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:economics",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:philosophy",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:mathematics",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:psychology",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:law",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:physics",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:chemistry",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:biology",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:public_health",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:medicine",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:engineering",
  "https://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=label:computer_science"
)

fields <- c(
  "publaw",
  "judpol",
  "american",
  "comparative",
  "ir",
  "theory",
  "polmeth",
  "policy",
  "polsci",
  "politics",
  "government",
  "sociology",
  "anthro",
  "history",
  "econ",
  "phil",
  "math",
  "psych",
  "law",
  "physics",
  "chemistry",
  "biology",
  "pubhealth",
  "medicine",
  "engineering",
  "compsci"
)

# psychology
#

a=18

rD <- rsDriver(browser=c("firefox"), version="latest")
#rD <- rsDriver(browser=c("chrome"), chromever="79.0.3945.36")
#rD <- rsDriver(browser=c("internet explorer"))
#rD <- rsDriver(browser=c("phantomjs"))

mybrowser <- rD$client

#print(fields[a])
#print(urls[a])

# navigate to page
mybrowser$navigate(urls[a])

# swtiching to rvest and stringr packages
# get current url
url <- as.character(mybrowser$getCurrentUrl())
html <- read_html(url)
# identify nodes at url where data are located
namehtml <- html_nodes(html, ".gs_ai_name")    # collect name data
affhtml <- html_nodes(html, ".gs_ai_aff")      # collect affiliation data
emailhtml <- html_nodes(html, ".gs_ai_eml")    # collect email data
citationhtml <- html_nodes(html, ".gs_ai_cby") # collect citation data
fieldbyentryhtml <- html_nodes(html, ".gs_ai_int")      # collect field data by each entry (person can report up to 5)
#fieldbyentryhtml <- html_nodes(html, ".gs_ai_one_int")      # collect field data by each entry (person can report up to 5)
fieldnotbyentryhtml <- html_nodes(html, ".gsc_1usr")        # collect all field data on page


# notes:
#fieldbyentry grabs all field labels in a single string for each individual entry; each individual field would have to then be identified within this string
#fieldnotbyentry grabs all field labels on the page, and does not associate them with any individual entry
#the discrete fields associated with each person could be distilled by comparing fieldbyentry to fieldnotbyentry
#i.e., could iterate through fieldnotbyentry, deleting each field as it is found within string fieldbyentry


# grab data
name <- html_text(namehtml)
aff <- html_text(affhtml)
email <- html_text(emailhtml)
citation <- html_text(citationhtml)
fieldentry <- html_text(fieldbyentryhtml)
fieldpage <- html_text(fieldnotbyentryhtml)


# note: wrote field data to field to check structure if written to csv
#write.table(field, file = "fieldbyentry.csv", append = FALSE, quote = FALSE, sep = "\t",
#            row.names = FALSE,
#            col.names = TRUE)


# strip/clean data to get data of interest
# note: from here forward, I have only worked with fieldentry text for fields
lastword <- word(citation, -1)
email2 <- word(email, -1) # note: many email incomplete; contain only home site, not recipient's address at site
# still, can use to verify id or to match individual entries to each other

# reformat
namelist <- name
afflist <- aff
emaillist <- email2
citationnum <- as.numeric(lastword)
fieldlist <- tolower(fieldentry)
fieldpagelist <- tolower(fieldpage)
# print
#namelist
#afflist
#emaillist
#citationnum
#fieldlist
#fieldpagelist

#for (i in 1:8){
i <- 10
while (i > 9){                    # this keep below loop going until last page with less than 10 entries
  # now swtich to rSelenium to click button to advance to next 10 entries
  # first, create button object
  #button <- mybrowser$findElements(using = 'css selector', ".gsc_btnPR .gs_in_ib .gs_btn_half .gs_btn_lsb .gs_btn_slt .gsc_pgn_ppr")
  button <- mybrowser$findElements("button", using = 'css selector')
  # click on button using clickElement for button object: 
  button[[3]]$clickElement()
  # get new url
  url <- as.character(mybrowser$getCurrentUrl())
  html <- read_html(url)
  namehtml <- html_nodes(html, ".gs_ai_name")    
  affhtml <- html_nodes(html, ".gs_ai_aff")
  emailhtml <- html_nodes(html, ".gs_ai_eml")
  citationhtml <- html_nodes(html, ".gs_ai_cby") 
  fieldbyentryhtml <- html_nodes(html, ".gs_ai_int") 
  fieldnotbyentryhtml <- html_nodes(html, ".gs_1usr")        
  name <- html_text(namehtml)
  aff <- html_text(affhtml)
  email <- html_text(emailhtml)
  citation <- html_text(citationhtml)
  fieldentry <- html_text(fieldbyentryhtml)
  fieldpage <- html_text(fieldnotbyentryhtml)
  lastword <- word(citation, -1)
  email2 <- word(email, -1)
  
  namelist <- append(namelist, name)
  afflist <- append(afflist, aff)
  emaillist <- append(emaillist, email2)
  citationnum <- append(citationnum, as.numeric(lastword))
  fieldlist <- append(fieldlist, tolower(fieldentry))
  fieldpagelist <- append(fieldpagelist, tolower(fieldpage))
  #print(namelist)
  #print(afflist)
  #print(emaillist)
  #print(citationnum)
  #print(fieldlist)
  #print(fieldpagelist)
  i <- length(name)               # here, if pages has less than 10 entries, then it is last page, and loop will not repeat (i.e., i <= 9)
}

mybrowser$close()

#print(namelist[1:5])
#print(citationnum[1:5])

# CAUTION
# affiliations not always present (e.g., Seth Greenfest in public law), and does not register as blank or NA
# rather, skips to next entry, which messes with order of entries across html nodes
# in any one field, field will of course always be present, along with name; citations may not be present, 
# but this does not matter since entries are always in descending order according to citations, so entries
# with no citations will always appear last.
# However, entries with no affiliation need correction.

data1 <- as.data.table(namelist)
data1 <- as.data.table(cbind(data1, as.numeric(row.names(data1))))

data2 <- as.data.table(citationnum)
data2 <- as.data.table(cbind(data2, as.numeric(row.names(data2))))

data3 <- as.data.table(afflist)
data3 <- as.data.table(cbind(data3, as.numeric(row.names(data3))))

data4 <- as.data.table(fieldlist)
data4 <- as.data.table(cbind(data4, as.numeric(row.names(data4))))


names(data1)[names(data1)=="V2"] <- "id"
names(data2)[names(data2)=="V2"] <- "id"
names(data3)[names(data3)=="V2"] <- "id"
names(data4)[names(data4)=="V2"] <- "id"

#data1
#data2

df <- merge(data1, data2, by="id", all=TRUE)
df <- merge(df, data3, by="id", all=TRUE)
df <- merge(df, data4, by="id", all=TRUE)
df[is.na(df)] <- 0
#data

df$field <- fields[a]

#df[1:5,c(1:3,6)]

#save data

write.table(df, file = paste0("./data/working/citations", "_", fields[a], ".csv", sep=""), 
            append = FALSE, 
            quote = FALSE, 
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)

#end
