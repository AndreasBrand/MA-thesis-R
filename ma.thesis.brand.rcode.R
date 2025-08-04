###############################################################
###############################################################
# RCode for MA Thesis
# Fall Semester 2017
# How Lobbying shapes the Political Agenda
# September 15, 2018, Andreas Brand, 12-756-672                         
###############################################################
###############################################################

rm(list=ls())
path <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Paper"

###############################################################
# 0. Content
###############################################################

# 1. load data
# 2. data prep: eu meetings
# 3. data prep: eu speeches
# 4. data viz for litreview
# 5. descriptive statistics (independent variables)
# 6. analysis
# 7. diagnostics
# 8. word cloud for titlepage

###############################################################
# 1. load data
###############################################################

# set working directory
setwd("C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data")

# eu: data on lobbyists yearly expenditure in the EU
eu_register <- read.csv("transparency_register.csv", header = T, sep = ",", na.strings = "NA")

# eu: data on lobbyists eu_meetings with eu commission
eu_meet <- read.csv("eu_meetings.csv", header = T, sep = ",", na.strings = "NA")

# us: data on contributions over time (per institution) and type of contribution (individual vs pac)
cc <- read.csv("cc.us.csv", header = T, sep = ",", na.strings = "NA")

# us: data on contributions per interest group type
cc_bli <- read.csv("cc.bli.csv", header = T, sep = ",", na.strings = "NA")

###############################################################
# 2. data prep: eu meetings
###############################################################

# -------------------------------------------------------------
# eu main dataframe: empty date dataframe (weekly)
# -------------------------------------------------------------

# get dataframe for week-observational level
dates <- seq(as.Date("2014-11-01"), as.Date("2018-06-30"), by = "1 month") 
d     <- data.frame(dates = dates, month = format(dates, format = "%M"))
rm(dates)

# -------------------------------------------------------------
# eu: transparency register (for lit review)
# -------------------------------------------------------------

library(dplyr)

# only consider active interest groups
eu_register$status <- as.character(eu_register$status)
eu_register <- filter(eu_register, status == "active")

# add number of group meetings to d dataset
eu_register$avgminmax <- (eu_register$cost_min + eu_register$cost_max) / 2

# calculate average costs
eu_register$cost_absolute <- ifelse(!is.na(eu_register$cost_absolute), eu_register$cost_absolute,
              ifelse(!is.na(eu_register$cost_min & eu_register$cost_max), eu_register$avgminmax,
                     ifelse(!is.na(eu_register$cost_min) & is.na(eu_register$cost_max), eu_register$cost_min,
                            ifelse(is.na(eu_register$cost_min) & !is.na(eu_register$cost_max), eu_register$cost_max, NA
                            ))))

# calculate sum of how much money is spent on lobbying
eu_register <- filter(eu_register, !is.na(cost_absolute))

# spent on lobbying in bn Euros
sum(eu_register$cost_absolute) / 1000000000
rm(eu_register)

# -------------------------------------------------------------
# eu meetings prep
# -------------------------------------------------------------

# create variable for eu commission-member type
eu_meet$type_person <- ifelse(grepl("Commissioner", eu_meet$host), "commissioner",
                              ifelse(grepl("Director-General", eu_meet$host), "director-general",
                                     "cabinet member"))

# make data ready
eu_meet$host        <- as.character(eu_meet$host)
eu_meet$portfolio   <- as.character(eu_meet$portfolio)
eu_meet$date        <- as.Date(eu_meet$date, format = "%d-%m-%y")
eu_meet$subject     <- as.character(eu_meet$subject)
eu_meet$lobbyorg    <- as.character(eu_meet$lobbyorg)
eu_meet$type_person <- as.character(eu_meet$type_person)

# clean names
library(tm)
eu_meet$host      <- tolower(eu_meet$host)
eu_meet$portfolio <- tolower(eu_meet$portfolio)
eu_meet$subject   <- tolower(eu_meet$subject)
eu_meet$lobbyorg  <- tolower(eu_meet$lobbyorg)
eu_meet$host      <- removePunctuation(eu_meet$host)
eu_meet$portfolio <- removePunctuation(eu_meet$portfolio)
eu_meet$subject   <- removePunctuation(eu_meet$subject)
eu_meet$lobbyorg  <- removePunctuation(eu_meet$lobbyorg)
eu_meet$host      <- stripWhitespace(eu_meet$host)
eu_meet$portfolio <- stripWhitespace(eu_meet$portfolio)
eu_meet$subject   <- stripWhitespace(eu_meet$subject)
eu_meet$lobbyorg  <- stripWhitespace(eu_meet$lobbyorg)

# get rid off meetings about brexit
eu_meet <- filter(eu_meet, portfolio != "taskforce on article 50 negotiations with the united kingdom")

# -------------------------------------------------------------
# eu merge meetings data with timeframe data
# -------------------------------------------------------------

# add number of group meetings to d dataset
for (i in 1:nrow(d)){
  
  # filter eu_meet data to month in d-dataset
  date_filter <- filter(eu_meet, date >= d$dates[i] & date < d$dates[i + 1])
  
  # filter date_filter to type of group
  corporate_filter <- filter(date_filter, group == "corporate")
  labor_filter     <- filter(date_filter, group == "labor")
  issue_filter     <- filter(date_filter, group == "issue-ideology")
  
  # count how many there are
  n_corporate <- nrow(corporate_filter)
  n_labor     <- nrow(labor_filter)
  n_issue     <- nrow(issue_filter)
  
  # add variable counting group-mettings to d dataset
  d$corp_n[[i]]  <- n_corporate
  d$labor_n[[i]] <- n_labor
  d$issue_n[[i]] <- n_issue
}

# define as numeric variables
d$corp_n <- as.numeric(d$corp_n)
d$labor_n <- as.numeric(d$labor_n)
d$issue_n <- as.numeric(d$issue_n)

# remove unnecessary stuff
rm(i, n_corporate, n_issue, n_labor, corporate_filter, issue_filter, labor_filter, date_filter)

###############################################################
# 3. data prep: eu speeches
###############################################################

# -------------------------------------------------------------
# preparation
# -------------------------------------------------------------

library(tm)

# make data analysis easier
options(stringsAsFactors = FALSE)

# The purpose of the readPDF()-function is to create a function that reads in PDF files
Rpdf <- readPDF(control = list(text = "-layout"))

# -------------------------------------------------------------
# get files paths
# -------------------------------------------------------------

and <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/andriukaitas-health"
ans <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/ansip-digital single market"
avr <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/avramopolous-home affairs"
bie <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/bienkowska-internal market"
bul <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/bulc-transport"
can <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/canete-climate action & energy"
cre <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/cretu-regional policy"
dom <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/dombrovskis&hill-euro & financial markets"
gab <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/gabriel&oettinger-digital economy"
hah <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/hahn-neighbourhood"
hog <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/hogan-agriculture"
jou <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/jourova-justice"
jun <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/juncker-presidency"
kat <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/katainen-jobs & growth"
mal <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/malmström-trade"
mim <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/mimica-development"
moe <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/moedas-research"
mog <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/mogherini-external action"
mos <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/moscovici-economics & tax"
nav <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/navracsics-education"
oet <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/oettinger&georgieva-budget"
sef <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/sefcovic-energy union"
sty <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/stylianides-humanitarian aid"
thy <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/thyssen-employment"
tim <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/timmermanns-better regulation"
vel <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/vella-environment"
ves <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/EU Speeches/vestager-competition"

# -------------------------------------------------------------
# load texts
# -------------------------------------------------------------

and <- VCorpus(DirSource(and), readerControl = list(reader = Rpdf))
ans <- VCorpus(DirSource(ans), readerControl = list(reader = Rpdf))
avr <- VCorpus(DirSource(avr), readerControl = list(reader = Rpdf))
bie <- VCorpus(DirSource(bie), readerControl = list(reader = Rpdf))
bul <- VCorpus(DirSource(bul), readerControl = list(reader = Rpdf))
can <- VCorpus(DirSource(can), readerControl = list(reader = Rpdf))
cre <- VCorpus(DirSource(cre), readerControl = list(reader = Rpdf))
dom <- VCorpus(DirSource(dom), readerControl = list(reader = Rpdf))
gab <- VCorpus(DirSource(gab), readerControl = list(reader = Rpdf))
hah <- VCorpus(DirSource(hah), readerControl = list(reader = Rpdf))
hog <- VCorpus(DirSource(hog), readerControl = list(reader = Rpdf))
jou <- VCorpus(DirSource(jou), readerControl = list(reader = Rpdf))
jun <- VCorpus(DirSource(jun), readerControl = list(reader = Rpdf))
kat <- VCorpus(DirSource(kat), readerControl = list(reader = Rpdf))
mal <- VCorpus(DirSource(mal), readerControl = list(reader = Rpdf))
mim <- VCorpus(DirSource(mim), readerControl = list(reader = Rpdf))
moe <- VCorpus(DirSource(moe), readerControl = list(reader = Rpdf))
mog <- VCorpus(DirSource(mog), readerControl = list(reader = Rpdf))
mos <- VCorpus(DirSource(mos), readerControl = list(reader = Rpdf))
nav <- VCorpus(DirSource(nav), readerControl = list(reader = Rpdf))
oet <- VCorpus(DirSource(oet), readerControl = list(reader = Rpdf))
sef <- VCorpus(DirSource(sef), readerControl = list(reader = Rpdf))
sty <- VCorpus(DirSource(sty), readerControl = list(reader = Rpdf))
thy <- VCorpus(DirSource(thy), readerControl = list(reader = Rpdf))
tim <- VCorpus(DirSource(tim), readerControl = list(reader = Rpdf))
vel <- VCorpus(DirSource(vel), readerControl = list(reader = Rpdf))
ves <- VCorpus(DirSource(ves), readerControl = list(reader = Rpdf))

# -------------------------------------------------------------
# add portfolio to the corpus
# -------------------------------------------------------------

i <- 0
and <- tm_map(and, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "health"
  x
})
ans <- tm_map(ans, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "digital single market"
  x
})
avr <- tm_map(avr, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "home affairs"
  x
})
bie <- tm_map(bie, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "internal market"
  x
})
bul <- tm_map(bul, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "transport"
  x
})
can <- tm_map(can, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "climate energy"
  x
})
cre <- tm_map(cre, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "regional policy"
  x
})
dom <- tm_map(dom, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "euro financial markets"
  x
})
gab <- tm_map(gab, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "digital economy"
  x
})
hah <- tm_map(hah, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "neighbourhood"
  x
})
hog <- tm_map(hog, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "agriculture"
  x
})
jou <- tm_map(jou, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "justice"
  x
})
jun <- tm_map(jun, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "presidency"
  x
})
kat <- tm_map(kat, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "jobs growth"
  x
})
mal <- tm_map(mal, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "trade"
  x
})
mim <- tm_map(mim, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "development"
  x
})
moe <- tm_map(moe, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "research"
  x
})
mog <- tm_map(mog, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "external action"
  x
})
mos <- tm_map(mos, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "economics tax"
  x
})
nav <- tm_map(nav, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "education"
  x
})
oet <- tm_map(oet, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "budget"
  x
})
sef <- tm_map(sef, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "energy union"
  x
})
sty <- tm_map(sty, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "humanitarian aid"
  x
})
thy <- tm_map(thy, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "employment"
  x
})
tim <- tm_map(tim, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "better regulation"
  x
})
vel <- tm_map(vel, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "environment"
  x
})
ves <- tm_map(ves, function(x) {
  i <<- i +1
  meta(x, "portfolio") <- "competition"
  x
})

# add all vcorpus and remove other
tmspee <- c(and, ans)
tmspee <- c(and, ans, avr, bie, bul, can, cre, dom, gab, hah, hog, jou, jun, kat, mal, mim, moe, mog, mos, nav, oet, sef, sty, thy, tim, vel, ves)
rm(and, ans, avr, bie, bul, can, cre, dom, gab, hah, hog, jou, jun, kat, mal, mim, moe, mog, mos, nav, oet, sef, sty, thy, tim, vel, ves)

# -------------------------------------------------------------
# clean
# -------------------------------------------------------------


  # clean automatically (part 1/2)
  tmspee <- tm_map(tmspee, content_transformer(tolower))
  tmspee <- tm_map(tmspee, content_transformer(function(x) iconv(x, "latin1", "ASCII", "")))
  tmspee <- tm_map(tmspee, removePunctuation)
  tmspee <- tm_map(tmspee, removeNumbers)
  
  # clean manually (part 1/2)
  manualclean <- c("[\r\n]", "european commission", "speech", "europe direct by phone", "general public inquiries",
                   "statement", "Press contacts", "Check Against Delivery", "or by email", " le ", " la ", " der ",
                   " die ", " das ", "european", "europe", "commission", "president", "union", " de ", " und ",
                   "prime minister", " g ", "jeanclaude", "juncker", " one ", " two ", " three ", "margaritis",
                   "andreeva", " wir ", "nicht", " zu ", " dass ", " es ", " ein ", " nous ", " des ", " den ",
                   " et ", " les ", " que ", " des ", " qui ", " je ", " en ", " pas ", " s ", " er ",
                   " le ", " i ", " pour ", "timmermanns", "katainen", "digital single market", "climate action energy",
                   "miguel arias", "jonathan hill")
  
  for (j in 1:length(manualclean)) {
    for (i in 1:length(tmspee)){
      tmspee[[i]][["content"]] <- gsub(manualclean[j], " ", tmspee[[i]][["content"]], ignore.case = TRUE)
    }
  }

  # # clean automatically (part 2/2)
  tmspee <- tm_map(tmspee, removeWords, stopwords("english"))
  tmspee <- tm_map(tmspee, stemDocument, language = "english")
  tmspee <- tm_map(tmspee, stripWhitespace)

  # clean manually (part 2/2)
  manualclean <- c(" vice ", "jyrki", "januari", "februari", "march", "april", "june", "juli", "septemb",
                   "octob", "novemb", "decemb", "dimitri", "avramopoulo", "christo", "stylianid", "ansip",
                   "king", "mariann", "thyssen", "migrat home affair", " caet ", "tibor", "navracs",
                   "violeta", "bulc", " ladi ", "oetting", "gentlemen", "dombrovski", "jean claud",
                   "federica", "mogherini", "johann", "hahn", "neighbourhood polici", "moscovici",
                   "health food safeti", "vyteni", "andriuka", "kristalina", "georgieva", "jourov",
                   "malmstrm", "moeda", "vestag", "representativevic", "high repres", "timmerman", "valdi",
                   "karmenu", "vella", "climat action energi", " aria ")
  
  
  for (j in 1:length(manualclean)) {
    for (i in 1:length(tmspee)){
      tmspee[[i]][["content"]] <- gsub(manualclean[j], " ", tmspee[[i]][["content"]], ignore.case = TRUE)
    }
  }
  
  # lastly, get rid of white space again
  tmspee <- tm_map(tmspee, stripWhitespace)
  
# remove objects
rm(i, j, Rpdf, manualclean)

# -------------------------------------------------------------
# Ingest: Reading and processing text data
# -------------------------------------------------------------

library(quanteda)
library(stm)
library(lubridate) # for date()-function (or year(), month(), week(), day())

# transform data into quanteda corpus
eucorp <- corpus(tmspee)

# add date variable
docvars(eucorp, "date") <- date(docvars(eucorp, 'datetimestamp'))
docvars(eucorp, "year") <- year(docvars(eucorp, 'datetimestamp'))

# add empty lobby meetings variable (normal, logged, squared, logged-squared)
docvars(eucorp, "corpmeet")        <- NA
docvars(eucorp, "labormeet")       <- NA
docvars(eucorp, "issuemeet")       <- NA
docvars(eucorp, "corpmeetlog")     <- NA
docvars(eucorp, "labormeetlog")    <- NA
docvars(eucorp, "issuemeetlog")    <- NA
docvars(eucorp, "corpmeetsq")      <- NA
docvars(eucorp, "labormeetsq")     <- NA
docvars(eucorp, "issuemeetsq")     <- NA
docvars(eucorp, "corpmeetsqlog")   <- NA
docvars(eucorp, "labormeetsqlog")  <- NA
docvars(eucorp, "issuemeetsqlog")  <- NA

# add number of group meetings to corpus
for (i in 1:ndoc(eucorp)){
  
  # filter eu_meet data to week in corpus
  port_filter <- filter(eu_meet, portfolio == as.character(eucorp$documents$portfolio[i]))
  date_filter <- filter(eu_meet, date <= as.Date(eucorp$documents$date[i]) & date >= as.Date(eucorp$documents$date[i]-6))
  
  # filter date_filter to type of group
  corporate_filter <- filter(date_filter, group == "corporate")
  labor_filter     <- filter(date_filter, group == "labor")
  issue_filter     <- filter(date_filter, group == "issue-ideology")
  
  # count how many there are
  n_corporate <- nrow(corporate_filter)
  n_labor     <- nrow(labor_filter)
  n_issue     <- nrow(issue_filter)
  
  # add variable counting group-mettings to d dataset
  eucorp$documents$corpmeet[[i]]  <- n_corporate
  eucorp$documents$labormeet[[i]] <- n_labor
  eucorp$documents$issuemeet[[i]] <- n_issue
}
rm(corporate_filter, labor_filter, issue_filter, port_filter, date_filter, n_corporate, n_labor, n_issue, i)

# add variables to show whether corporate or issue-ideology groups dominate (competition)
eucorp$documents$freqissueofcorp <- eucorp$documents$issuemeet / eucorp$documents$corpmeet
eucorp$documents$competition <- ifelse(eucorp$documents$freqissueofcorp < 0.5,
                                       "Corporate Groups Dominate", "Issue-ideology Groups Dominate")

# add salience variable
corptop <- quantile(eucorp$documents$corpmeet)[[3]]
issuetop <- quantile(eucorp$documents$issuemeet)[[3]]
eucorp$documents$salience <- ifelse(eucorp$documents$corpmeet >= corptop &
                                      eucorp$documents$issuemeet >= issuetop,
                                    1, 0)

# add ideology variable
eucorp$documents$ideology <- ifelse(eucorp$documents$portfolio == "presidency", "EPP",
                                    ifelse(eucorp$documents$portfolio == "agriculture", "EPP",
                                    ifelse(eucorp$documents$portfolio == "better regulation", "PES",
                                    ifelse(eucorp$documents$portfolio == "budget", "EPP",
                                    ifelse(eucorp$documents$portfolio == "climate energy", "EPP",
                                    ifelse(eucorp$documents$portfolio == "competition", "ALDE",
                                    ifelse(eucorp$documents$portfolio == "development", "EPP",
                                    ifelse(eucorp$documents$portfolio == "digital economy", "EPP",
                                    ifelse(eucorp$documents$portfolio == "digital single market", "ALDE",
                                    ifelse(eucorp$documents$portfolio == "economics tax", "PES",
                                    ifelse(eucorp$documents$portfolio == "education", "EPP",
                                    ifelse(eucorp$documents$portfolio == "employment", "EPP",
                                    ifelse(eucorp$documents$portfolio == "energy union", "EPP",
                                    ifelse(eucorp$documents$portfolio == "environment", "PES",
                                    ifelse(eucorp$documents$portfolio == "euro financial markets", "EPP",
                                    ifelse(eucorp$documents$portfolio == "external action", "PES",
                                    ifelse(eucorp$documents$portfolio == "health", "PES",
                                    ifelse(eucorp$documents$portfolio == "home affairs", "EPP",
                                    ifelse(eucorp$documents$portfolio == "humanitarian aid", "EPP",
                                    ifelse(eucorp$documents$portfolio == "internal market", "EPP",
                                    ifelse(eucorp$documents$portfolio == "jobs growth", "EPP",
                                    ifelse(eucorp$documents$portfolio == "justice", "ALDE",
                                    ifelse(eucorp$documents$portfolio == "neighbourhood", "EPP",
                                    ifelse(eucorp$documents$portfolio == "regional policy", "EPP",
                                    ifelse(eucorp$documents$portfolio == "research", "PES",
                                    ifelse(eucorp$documents$portfolio == "trade", "ALDE",
                                    ifelse(eucorp$documents$portfolio == "transport", "ALDE",
                                    NA)))))))))))))))))))))))))))

# group ideology into direction
eucorp$documents$ideology <- ifelse(eucorp$documents$ideology == "ALDE", "Right",
                             ifelse(eucorp$documents$ideology == "EPP", "Center",
                             ifelse(eucorp$documents$ideology == "PES", "Left", NA)))

# put in right structure
eucorp$documents$date        <- as.Date(eucorp$documents$date)
eucorp$documents$year        <- as.factor(eucorp$documents$year)
eucorp$documents$portfolio   <- as.factor(eucorp$documents$portfolio)
eucorp$documents$competition <- as.factor(eucorp$documents$competition)
eucorp$documents$salience    <- as.factor(eucorp$documents$salience)
eucorp$documents$ideology    <- as.factor(eucorp$documents$ideology)
eucorp$documents$corpmeet    <- as.numeric(eucorp$documents$corpmeet)
eucorp$documents$labormeet   <- as.numeric(eucorp$documents$labormeet)
eucorp$documents$issuemeet   <- as.numeric(eucorp$documents$issuemeet)
eucorp$documents$ideology    <- as.factor(eucorp$documents$ideology)

# add logged values
docvars(eucorp, "corpmeetlog")     <- log(1 + eucorp$documents$corpmeet)
docvars(eucorp, "labormeetlog")    <- log(1 + eucorp$documents$labormeet)
docvars(eucorp, "issuemeetlog")    <- log(1 + eucorp$documents$issuemeet)
docvars(eucorp, "corpmeetsq")      <- eucorp$documents$corpmeet^2
docvars(eucorp, "labormeetsq")     <- eucorp$documents$labormeet^2
docvars(eucorp, "issuemeetsq")     <- eucorp$documents$issuemeet^2
docvars(eucorp, "corpmeetsqlog")   <- log(1 + (eucorp$documents$corpmeet^2))
docvars(eucorp, "labormeetsqlog")  <- log(1 + (eucorp$documents$labormeet^2))
docvars(eucorp, "issuemeetsqlog")  <- log(1 + (eucorp$documents$issuemeet^2))

# take a loot at corpus
summary(eucorp, n = 5, showmeta = TRUE)

# create a token file
#eutoks <- tokens(eucorp)
#head(eu_toks[[1]], 50)

# document-frequency matrix
eudfm <- dfm(eucorp)
ndoc(eudfm)
head(eudfm)

# political ideology
#lg_dict <- dictionary(file = "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Data/LaverGarry.cat")
#lg_dfm  <- dfm(eucorp, dictionary = lg_dict)
#lg_dfm  <- dfm_lookup(lg_dfm, lg_dict)
#head(lg_dfm, n = 20)

# inspect data
head(featnames(eudfm), 20)

# convert to data type the stm-package can work with
eustm <- convert(eudfm, to = "stm")

# create out file
#out   <- prepDocuments(eustm$documents, eustm$vocab, eustm$meta)
#docs  <- out$documents
#vocab <- out$vocab
#meta  <- out$meta
eucorp2 <- textProcessor(eucorp$documents$texts, metadata = eucorp$documents)
out   <- prepDocuments(eucorp2$documents, eucorp2$vocab, eucorp2$meta)
docs  <- out$documents
vocab <- out$vocab
meta  <- out$meta

rm(tmspee, corptop, issuetop)

# -------------------------------------------------------------
# Prepare: Associating text with metadata
# -------------------------------------------------------------

plotRemoved(eustm$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(eustm$documents, eustm$vocab, eustm$meta, lower.thresh = 40)

###############################################################
# 4. data viz for litreview
###############################################################

# -------------------------------------------------------------
# eu_meetings and registration in the eu, by group type
# -------------------------------------------------------------

# eu_meetings data
type <- c("corp", "labor", "issue")

# corporate: prof. consultancies + companies and groups + other in house + trade & bus. orgs
corp <- table(eu_meet$group == "corporate")[[2]]

# labor
labor <- table(eu_meet$group == "labor")[[2]]

# issue-ideology groups: NGOs + academic + think-thanks + churches
issue <- table(eu_meet$group == "issue-ideology")[[2]]

# row bind
meetings <- rbind(corp, labor, issue)

# registration data
corp <- 767 + 2227 + 2382 + 336
labor <- 880
issue <- 3121 + 333 + 582 + 52
registrations <- rbind(corp, labor, issue)

# create dataset
eu_lobby <- matrix(c(meetings, registrations), nrow = 3, ncol = 2)
colnames(eu_lobby) <- c("meetings", "registrations")
eu_lobby <- as.data.frame(eu_lobby)
eu_lobby$type <- c("Corporate", "Labor", "Issue-ideology")
rm(meetings, registrations, corp, issue, labor, type)

# share variables
eu_lobby$meetings_share <- eu_lobby$meetings / sum(eu_lobby$meetings)
eu_lobby$registrations_share <- eu_lobby$registrations / sum(eu_lobby$registrations)

library(ggplot2)
library(gridExtra)

# eu_meetings by type graph
eu_meet_type <- ggplot(data = eu_lobby, aes(y = meetings_share, x = type)) +
  geom_col() +
  theme_minimal() +
  labs(title = "(a) Meetings", y = "", x = "") +
  theme(plot.title       = element_text(),
        axis.text.x      = element_text(angle = 0, vjust = 0),
        axis.text.y      = element_text(),
        axis.text        = element_text(),
        axis.title       = element_text(),
        panel.grid.minor = element_blank(),
        text             = element_text(family = "serif", size = 12)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.8)) +
  geom_text(aes(label = paste(100*round(meetings_share, 3), "%", sep = " "),
                family = "serif"), vjust = -0.5)

eu_meet_type

# registrations by type graph
eu_regi_type <- ggplot(data = eu_lobby, aes(y = registrations_share, x = type)) +
  geom_col() +
  theme_minimal() +
  labs(title = "(b) Registrations", y = "", x = "") +
  theme(plot.title       = element_text(),
        axis.text.x      = element_text(angle = 0, vjust = 0),
        axis.text.y      = element_text(),
        axis.text        = element_text(),
        axis.title       = element_text(),
        panel.grid.minor = element_blank(),
        text             = element_text(family = "serif", size = 12)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.8)) +
  geom_text(aes(label = paste(100*round(registrations_share, 3), "%", sep = " "),
                family = "serif"), vjust = -0.5)

eu_regi_type

# save
ggsave("eu_type.png", width = 8, height = 4,
       arrangeGrob(eu_meet_type, eu_regi_type, nrow = 1, ncol = 2),
       path = "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Paper")

rm(eu_meet_type, eu_regi_type, eu_lobby)

# -------------------------------------------------------------
# campaign contributions over time, for each institution
# -------------------------------------------------------------


# amount of money raised, for each institution
cc_inst <- ggplot(cc, aes(year)) +
  geom_line(aes(y = raised_inf / 1000000000, linetype = type), size = 1) +
  theme_minimal() +
  labs(title = "(a) Amount of contributions raised by candidates",
       y = "$bn", x = "") +
  scale_x_continuous(breaks = cc$year) +
  scale_y_continuous(limits = c(0, 2)) +
  theme(plot.title       = element_text(size = 16),
        axis.line        = element_line(size = 0, colour = "Black"),
        axis.text.x      = element_text(angle = 90, vjust=0.5, size = 16),
        axis.text.y      = element_text(size = 16),
        axis.text        = element_text(size = 16),
        axis.title       = element_text(size = 16),
        panel.grid.minor = element_blank(),
        text             = element_text(family = "serif")) +
  scale_linetype_discrete(name = "Institution", labels = c("House", "Presidency", "Senate")) +
  geom_vline(aes(xintercept = 2002), size = 1, color = "grey35") +
  geom_vline(aes(xintercept = 2014), size = 1, color = "grey35") +
  annotate(geom = "label", x = 2002 - 9, y = 1.7, label = "BCR-Act, \nincreased contribution limits",
           angle = 0, hjust = 0, size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = 2014 - 9, y = 0.2, label = "McCutcheon v. FEC, \nincreased contribution limits",
           angle = 0, hjust = 0, size = 4, family = "serif", color = "grey35")
  
# show plot
cc_inst

# -------------------------------------------------------------
# source of money: individuals vs pacs (and other)
# -------------------------------------------------------------

# create variable
cc$indv_inf_share <- NA
cc$pacs_inf_share <- NA
cc$indv_inf_share <- cc$indv_inf / cc$raised_inf
cc$pacs_inf_share <- cc$pacs_inf / cc$raised_inf

# dataset for each institution
library(dplyr)
cc_pres <- filter(cc, type == "pres")
cc_hous <- filter(cc, type == "house")
cc_sena <- filter(cc, type == "senate")

# create plot
pres_source <- ggplot(cc_pres, aes(year)) +
  geom_area(aes(y = indv_inf_share + pacs_inf_share + (1 - indv_inf_share - pacs_inf_share)), fill = "grey") +
  geom_area(aes(y = indv_inf_share + pacs_inf_share), fill = "grey35") +
  geom_area(aes(y = pacs_inf_share), fill = "grey20") +
  theme_minimal() +
  labs(title = "(a) Presidential candidates",
       y = "", x = "") +
  scale_x_continuous(breaks = cc$year) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title       = element_text(size = 16),
        axis.line        = element_line(size = 0, colour = "Black"),
        axis.text.x      = element_text(angle = 90, vjust=0.5, size = 16),
        axis.text.y      = element_text(size = 16),
        axis.text        = element_text(size = 16),
        axis.title       = element_text(size = 16),
        panel.grid.minor = element_blank(),
        text             = element_text(family = "serif")) +
  annotate(geom = "text", x = summary(cc_pres$year)[[2]], y = 0.05,
           label = paste("PAC contributions, mean = ",
                         round(mean(cc_pres$pacs_inf_share)*100, 1), "%", sep = " "),
           angle = 0, hjust = 0, size = 5, family = "serif") +
  annotate(geom = "text", x = summary(cc_pres$year)[[2]], y = 0.60,
           label = paste("Individual contributions, mean = ",
                         round(mean(cc_pres$indv_inf_share)*100, 1), "%", sep = " "),
           angle = 0, hjust = 0, size = 5, family = "serif") +
  annotate(geom = "text", x = summary(cc_pres$year)[[2]], y = 0.90,
           label = paste("Other contributions, mean = ",
                         round(((1 - mean(cc_pres$indv_inf_share) - mean(cc_pres$pacs_inf_share)) * 100), 1),
                         "%", sep = " "),
           angle = 0, hjust = 0, size = 5, family = "serif")

pres_source
  
hous_source <- ggplot(cc_hous, aes(year)) +
  geom_area(aes(y = indv_inf_share + pacs_inf_share + (1 - indv_inf_share - pacs_inf_share)), fill = "grey") +
  geom_area(aes(y = indv_inf_share + pacs_inf_share), fill = "grey35") +
  geom_area(aes(y = pacs_inf_share), fill = "grey20") +
  theme_minimal() +
  labs(title = "(b) House candidates",
       y = "", x = "") +
  scale_x_continuous(breaks = cc$year, limits = c(1992, 2016)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title       = element_text(size = 16),
        axis.line        = element_line(size = 0, colour = "Black"),
        axis.text.x      = element_text(angle = 90, vjust=0.5, size = 16),
        axis.text.y      = element_text(size = 16),
        axis.text        = element_text(size = 16),
        axis.title       = element_text(size = 16),
        panel.grid.minor = element_blank(),
        text             = element_text(family = "serif")) +
  annotate(geom = "text", x = summary(cc_hous$year)[[2]], y = 0.15,
           label = paste("PAC contributions, mean = ",
                         round(mean(cc_hous$pacs_inf_share)*100, 1), "%", sep = " "),
           angle = 0, hjust = 0, size = 5, family = "serif") +
  annotate(geom = "text", x = summary(cc_hous$year)[[2]], y = 0.60,
           label = paste("Individual contributions, mean = ",
                         round(mean(cc_hous$indv_inf_share)*100, 1), "%", sep = " "),
           angle = 0, hjust = 0, size = 5, family = "serif") +
  annotate(geom = "text", x = summary(cc_hous$year)[[2]], y = 0.95,
           label = paste("Other contributions, mean = ",
                         round(((1 - mean(cc_hous$indv_inf_share) - mean(cc_hous$pacs_inf_share)) * 100), 1),
                         "%", sep = " "),
           angle = 0, hjust = 0, size = 5, family = "serif")

hous_source

sena_source <- ggplot(cc_sena, aes(year)) +
  geom_area(aes(y = indv_inf_share + pacs_inf_share + (1 - indv_inf_share - pacs_inf_share)), fill = "grey") +
  geom_area(aes(y = indv_inf_share + pacs_inf_share), fill = "grey35") +
  geom_area(aes(y = pacs_inf_share), fill = "grey20") +
  theme_minimal() +
  labs(title = "(c) Senate candidates", x = "", y = "") +
  scale_x_continuous(breaks = cc$year, limits = c(1992, 2016)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title       = element_text(size = 16),
        axis.line        = element_line(size = 0, colour = "Black"),
        axis.text.x      = element_text(angle = 90, vjust=0.5, size = 16),
        axis.text.y      = element_text(size = 16),
        axis.text        = element_text(size = 16),
        axis.title       = element_text(size = 16),
        panel.grid.minor = element_blank(),
        text             = element_text(family = "serif")) +
  annotate(geom = "text", x = summary(cc_sena$year)[[2]], y = 0.10,
           label = paste("PAC contributions, mean = ",
                         round(mean(cc_sena$pacs_inf_share)*100, 1), "%", sep = " "),
           angle = 0, hjust = 0, size = 5, family = "serif") +
  annotate(geom = "text", x = summary(cc_sena$year)[[2]], y = 0.60,
           label = paste("Individual contributions, mean = ",
                         round(mean(cc_sena$indv_inf_share)*100, 1), "%", sep = " "),
           angle = 0, hjust = 0, size = 5, family = "serif") +
  annotate(geom = "text", x = summary(cc_sena$year)[[2]], y = 0.95,
           label = paste("Other contributions, mean = ",
                         round(((1 - mean(cc_sena$indv_inf_share) - mean(cc_sena$pacs_inf_share)) * 100), 1),
                         "%", sep = " "),
           angle = 0, hjust = 0, size = 5, family = "serif")

sena_source

# -------------------------------------------------------------
# CC by trade, business and ideological groups
# -------------------------------------------------------------

# function to make 1 decimal figure in plot
scaleFUN <- function(x) sprintf("%.1f", x)

# function
graph_bli <- ggplot(cc_bli, aes(year)) +
  geom_area(aes(y = tot_inf / 1000000000, fill = type)) +
  theme_minimal() +
  labs(title = "(b) Source of contributions by interest group type (all types of contributions)",
       y = "$bn", x = "") +
  scale_x_continuous(breaks = cc$year) +
  scale_y_continuous(labels = scaleFUN) +
  scale_fill_grey(name = "Interest group type", start = 0.8, end = 0.2) +
  theme(plot.title       = element_text(size = 16),
        axis.line        = element_line(size = 0, colour = "Black"),
        axis.text.x      = element_text(angle = 90, vjust=0.5, size = 16),
        axis.text.y      = element_text(size = 16),
        axis.text        = element_text(size = 16),
        axis.title       = element_text(size = 16),
        legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 14),
        panel.grid.minor = element_blank(),
        text             = element_text(family = "serif"))

graph_bli

# save all in one
library(gridExtra)

ggsave("cc_tot_bli.png", width = 8, height = 8,
       arrangeGrob(cc_inst, graph_bli, nrow = 2, ncol = 1),
       path = "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Paper")

ggsave("cc_source.png", width = 8, height = 10,
       arrangeGrob(pres_source, hous_source, sena_source, nrow = 3, ncol = 1),
       path = "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Paper")

rm(cc_inst, graph_bli, pres_source, hous_source, sena_source, cc_bli, cc_hous, cc_pres, cc_sena, scaleFUN)

# -------------------------------------------------------------
# meetings over time by group
# -------------------------------------------------------------

library(xts)
library(ggfortify)
library(lubridate)

# plot
lbls <- paste0(month.abb[month(d$dates)], " ", lubridate::year(d$dates))
lbls <- lbls[c(TRUE, FALSE)]
brks <- d$dates
brks <- brks[c(TRUE, FALSE)]
eu_meet_time_type <- ggplot(d, aes(x = dates)) + 
  geom_line(aes(y = corp_n, col = "corp_n"), linetype = "solid", size = 1) + 
  geom_line(aes(y = issue_n, col = "issue_n"), linetype = "solid", size = 1) + 
  geom_line(aes(y = labor_n, col ="labor_n"), size = 1) +
  labs(title = "",y = "Amount of meetings each month", x = "") +
  theme_minimal() +
  scale_color_manual(name = "Interest group type", labels = c("Corporate", "Issue-ideology", "Labor"),
                     values = c("corp_n" = "grey80", "issue_n" = "grey50", "labor_n" = "grey1")) +
  scale_x_date(labels = lbls, breaks = brks) + 
  theme(plot.title       = element_text(size = 16),
        axis.line        = element_line(size = 0, colour = "Black"),
        axis.text.x      = element_text(angle = 90, vjust=0.5, size = 16),
        axis.text.y      = element_text(size = 16),
        axis.text        = element_text(size = 16),
        axis.title       = element_text(size = 16),
        legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 14),
        #panel.grid.minor = element_blank(),
        text             = element_text(family = "serif"))

eu_meet_time_type

ggsave("eu_meet_time_type.png", plot = eu_meet_time_type,
       path = "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Paper",
       width = 10, height = 6)

rm(eu_meet_time_type, d, cc, brks, lbls)

###############################################################
# 5. descriptive statistics (independent variables)
###############################################################

library(gridExtra)
library(stargazer)

# -------------------------------------------------------------
# eu: summary statistics
# -------------------------------------------------------------

# google how to include factor variables in sum stats!

# summary statistics of all variables included
sumstats <- select(eucorp$documents, corpmeet, corpmeetlog, issuemeet, issuemeetlog, labormeet, labormeetlog)
stargazer(sumstats, median = TRUE, digits = 2, covariate.labels =
            c("Corporate group meetings", "Corporate group meetings (log)",
              "Issue-ideology group meetings", "Issue-ideology group meetings (log)",
              "Labor group meetings", "Labor group meetings (log)",
              "Salience (Dummy)", "Competition (Dummy)",
              "Party Ideology", "Year", "Portfolio/Issue Area"))

# -------------------------------------------------------------
# salience and domination
# -------------------------------------------------------------

table(eucorp$documents$competition)

table(eucorp$documents$salience)

# -------------------------------------------------------------
# eu: meetings, portfolios and speeches
# -------------------------------------------------------------

# create dataset for overview of meetings, portfolios and speeches
eudes <- as.data.frame(table(eucorp$documents$portfolio))
colnames(eudes)[colnames(eudes) == "Var1"] <- "portfolio"
colnames(eudes)[colnames(eudes) == "Freq"] <- "speeches"
eudes$portfolio <- as.character(eudes$portfolio)
eudes$speeches  <- as.numeric(eudes$speeches)

# add number of meetings to dataset
for (i in 1:nrow(eudes)) {
  b <- filter(eu_meet, portfolio == eudes$portfolio[i])
  
  c <- filter(b, group == "corporate")
  ii <- filter(b, group == "issue-ideology")
  l <- filter (b, group == "labor")
  
  c <- nrow(c)
  ii <- nrow(ii)
  l <- nrow(l)
  
  eudes$corpmeet[[i]] <- as.numeric(c)
  eudes$issuemeet[[i]] <- as.numeric(ii)
  eudes$labormeet[[i]] <- as.numeric(l)  
}
rm(c, i, ii, l, b)

# order
eudes$portfolio <- factor(eudes$portfolio, levels = eudes$portfolio[order(eudes$speeches, decreasing = T)])
eudes <- eudes[order(eudes$speeches, decreasing = TRUE),]
eudes$id <- 1:nrow(eudes)

# convert to long format
library(reshape2)
eudeslong <- melt(eudes, id.vars = "portfolio")
eudeslong <- filter(eudeslong, variable != "speeches")
eudeslong <- filter(eudeslong, variable != "id")

# labels
eulab <- c("Euro & Financial Markets", "Presidency", "Home Affairs", "Energy Union",
  "Climate & Energy", "Digital Single Market", "Better Regulation", "Employment",
  "Justice", "Jobs & Growth", "Neighbourhood", "Transport", "Research",
  "Internal Market", "Environment", "Health", "Trade", "Digital Economy",
  "Economics & Tax", "Education", "Budget", "Agriculture", "Humanitarian Aid",
  "Regional Policy", "External Action", "Competition", "Development")

# create plot
eu_meetspee <- ggplot(eudeslong, aes(x = portfolio, y = value, fill = variable)) +
  geom_bar(position = "fill", stat = 'identity') +
  annotate(geom = "label", x = 14, y = 0.90, label = "# speeches\n(right axis)",
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[1], y = (eudes$speeches[1]/200), label = eudes$speeches[1],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[2], y = (eudes$speeches[2]/200), label = eudes$speeches[2],
           angle = 0, size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[3], y = (eudes$speeches[3]/200), label = eudes$speeches[3],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[4], y = (eudes$speeches[4]/200), label = eudes$speeches[4],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[5], y = (eudes$speeches[5]/200), label = eudes$speeches[5],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[6], y = (eudes$speeches[6]/200), label = eudes$speeches[6],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[7], y = (eudes$speeches[7]/200), label = eudes$speeches[7],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[8], y = (eudes$speeches[8]/200), label = eudes$speeches[8],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[9], y = (eudes$speeches[9]/200), label = eudes$speeches[9],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[10], y = (eudes$speeches[10]/200), label = eudes$speeches[10],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[11], y = (eudes$speeches[11]/200), label = eudes$speeches[11],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[12], y = (eudes$speeches[12]/200), label = eudes$speeches[12],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[13], y = (eudes$speeches[13]/200), label = eudes$speeches[13],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[14], y = (eudes$speeches[14]/200), label = eudes$speeches[14],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[15], y = (eudes$speeches[15]/200), label = eudes$speeches[15],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[16], y = (eudes$speeches[16]/200), label = eudes$speeches[16],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[17], y = (eudes$speeches[17]/200), label = eudes$speeches[17],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[18], y = (eudes$speeches[18]/200), label = eudes$speeches[18],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[19], y = (eudes$speeches[19]/200), label = eudes$speeches[19],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[20], y = (eudes$speeches[20]/200), label = eudes$speeches[20],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[21], y = (eudes$speeches[21]/200), label = eudes$speeches[21],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[22], y = (eudes$speeches[22]/200), label = eudes$speeches[22],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[23], y = (eudes$speeches[23]/200), label = eudes$speeches[23],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[24], y = (eudes$speeches[24]/200), label = eudes$speeches[24],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[25], y = (eudes$speeches[25]/200), label = eudes$speeches[25],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[26], y = (eudes$speeches[26]/200), label = eudes$speeches[26],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
  annotate(geom = "label", x = eudes$id[27], y = (eudes$speeches[27]/200), label = eudes$speeches[27],
           angle = 0, hjust = "center", size = 4, family = "serif", color = "grey35") +
    scale_fill_grey(start = 0.8, end = 0.2, name = "Interest group type (left axis): ",
                  label = c(" Corporate ", " Issue-ideology ", " Labor ")) +
  scale_y_continuous(labels = scales::percent,
                     sec.axis = sec_axis(~.*200, name = "Amount of speeches")) +
  scale_x_discrete(labels = eulab) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x      = element_text(size = 16, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y      = element_text(size = 16),
        axis.text        = element_text(size = 16),
        axis.title       = element_text(size = 16),
        legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 14),
        legend.position  = "top",
        text             = element_text(family = "serif")) +
  labs(y = "Share of total meetings", x = "")

# show graph
eu_meetspee

# save graph
ggsave("eu_meetspee.png", plot = eu_meetspee, path = path,
       width = 10, height = 6)

rm(eu_meetspee, eulab, eudes, eudeslong)

# -------------------------------------------------------------
# eu: density plots distribution meetings
# -------------------------------------------------------------

# create dataset
eud <- eucorp$documents
eud <- select(eud, id, corpmeet, issuemeet, labormeet)
eudlong <- melt(eud, id.vars = "id")

# create density plot

group_by(eudlong, variable)
eumeetdens <- ggplot(eudlong, aes(x = value)) +
  geom_density(aes(fill = variable), kernel = "gaussian", size = 0.8, alpha = 0.8, linetype = "blank") +
  scale_fill_grey(start = 0.8, end = 0.2, name = "Interest group type: ",
                  label = c(" Corporate ", " Issue-ideology ", " Labor ")) +
  theme_minimal() + 
  theme(plot.title       = element_text(size = 16),
        axis.text.x      = element_text(size = 16),
        axis.text.y      = element_text(size = 16),
        axis.text        = element_text(size = 16),
        axis.title       = element_text(size = 16),
        legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 14),
        legend.position  = "top",
        text             = element_text(family = "serif")) +
  labs(title = "(a) Non-logarithmic values", x = "Amount of meetings",
       y = "Density") +
    scale_y_continuous(labels = scales::percent)

eumeetdens

eumeetdenslog <- ggplot(eudlong, aes(x = log(value))) +
  geom_density(aes(fill = variable), kernel = "gaussian", size = 0.8, alpha = 0.8, linetype = "blank") +
  scale_fill_grey(start = 0.8, end = 0.2, name = "Interest group type: ",
                  label = c(" Corporate ", " Issue-ideology ", " Labor ")) +
  theme_minimal() + 
  theme(plot.title       = element_text(size = 16),
        axis.text.x      = element_text(size = 16),
        axis.text.y      = element_text(size = 16),
        axis.text        = element_text(size = 16),
        axis.title       = element_text(size = 16),
        legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 14),
        legend.position  = "top",
        text             = element_text(family = "serif")) +
  labs(title = "(b) Logarithmic values", x = "Amount of meetings (log)",
       y = "Density") +
  scale_y_continuous(labels = scales::percent)

eumeetdenslog

ggsave("eu_meetdens.png", width = 12, height = 6, arrangeGrob(eumeetdens, eumeetdenslog, nrow = 1, ncol = 2),
       path = path)

rm(eumeetdens, eumeetdenslog, eud, eudlong, eu_meet)

###############################################################
# 6. analysis and descriptive statistics (dependent variable)
###############################################################

# -------------------------------------------------------------
# Model selection and search
# -------------------------------------------------------------

#zerok <- stm(documents = out$documents, vocab = out$vocab,
 #                K = 0, prevalence =~ corpmeet*salience + issuemeet*salience + labormeet +
  #                 year + portfolio,
   #              max.em.its = 75, data = out$meta, init.type = "Spectral")

#zerok <- 50

# Model search across numbers of topics
#storage <- searchK(out$documents, out$vocab, K = c(30, 50, 70), prevalence =~ corpmeet*salience + issuemeet*salience + labormeet +year + portfolio, data = meta)
#round(storage$results, 2)

k <- 50

# -------------------------------------------------------------
# Estimating the structural topic model
# -------------------------------------------------------------

set.seed(1234)

# topical prevalence (not-logged)
#euprevfit <- stm(documents = out$documents, vocab = out$vocab,
#                 K = k, prevalence =~ corpmeet*salience + issuemeet*salience + labormeet +
 #                  year + portfolio,
  #               max.em.its = 75, data = out$meta, init.type = "Spectral")

# topical prevalence (logged)
euprevfitlog <- stm(documents = out$documents, vocab = out$vocab,
                   K = k, prevalence =~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog +
                    year + portfolio,
                   max.em.its = 75, data = out$meta,
                   init.type = "Spectral", set.seed(1234))

#euprevfitlogwofe <- stm(documents = out$documents, vocab = out$vocab,
 #                   K = k, prevalence =~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog,
  #                  max.em.its = 75, data = out$meta,
   #                 init.type = "Spectral", set.seed(1234))

# topical content
euprevfitcont <- stm(documents = out$documents, vocab = out$vocab,
                     K = k, prevalence =~ competition + salience + labormeetlog +
                       year + portfolio,
                     content =~ competition, max.em.its = 75, data = out$meta,
                     init.type = "Spectral", set.seed(1234))

# -------------------------------------------------------------
# topic labelling
# -------------------------------------------------------------

# explore words and docs associated with each topic
seq <- seq(1, k, 1)
labelTopics(euprevfitlog, 34)
findThoughts(euprevfitlog, texts = eucorp$documents$texts, topics = 50, n = 1) # around 2 lines on laptop

# topic vector
topics <- c("Non-English Words", "Renewables", "Capital Markets Union", "Poland", "Fiscal Policy (unclear)", "Research", "EU Crisis/Investment (unclear)", "Trade", "Ukraine", "Food Waste",
            "Energy supply", "Borders", "Youth employment", "Data Protection", "Climate Change", "Banking", "Security", "Budget", "Martime Environment", "Pensions",
            "Statehood", "Technology", "Development", "Intra-EU Labor Movement (unclear)", "Public Procurement", "Greece", "Radicalization/Internet", "Spain", "Economic Reforms (unclear)", "Greetings",
            "China", "Digitization", "Refugee Crisis", "Financial Regulation", "European Unity (unclear)", "Media Freedom", "Threats of Globalization", "Tax evasion", "Hungary", "Foreign Affairs (unclear)",
            "Germany", "Green Finance", "Visa Liberalization", "Social Institutions (unclear)", "Terrorism", "Sports", "Infrastructure", "Agriculture", "Migration", "Gas Prices")

# topic prevalence: all topics graph (save as "topplot" pdf 7x10)
setwd(path)
set.seed(1234)
par(family = "serif", bty = "n", col = "grey40", lwd = 5, mfrow = c(1, 1), ps = 12)
plot.STM(euprevfitlog, type = "summary", family = "serif", xlim = c(0, 0.06), main = "Top topics"
         #,topic.names = topics
         ,custom.labels = topics
         )

# wordcloud visualization for specific topics (save using pdf 5x5)
#library(RColorBrewer)
#pal <- brewer.pal(9,"Greys")
#pal <- pal[-(1:4)]
#par(family = "serif", mfrow = c(1,1))
#cloud(euprevfitlog, topic = 32, random.order = F, random.color = T, colors = pal)
#cloud(euprevfitlog, topic = 2, random.order = F, random.color = T, colors = pal)
#cloud(euprevfitlog, topic = 38, random.order = F, random.color = T, colors = pal)

rm(seq)

# -------------------------------------------------------------
# topical prevalence: regression results
# -------------------------------------------------------------

# corporate topic: 34 = financial regulation, 33 = refugee crisis, 2 = renewables
reg <- estimateEffect(c(34, 33, 2) ~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog +
                          year + portfolio,
                        euprevfitlog, metadata = out$meta, uncertainty = "Global",
                      set.seed(1234))

regwofe <- estimateEffect(c(34, 33, 2) ~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog,
                      euprevfitlogwofe, metadata = out$meta, uncertainty = "Global",
                      set.seed(1234))

# regression output
summary(reg, set.seed(1230484))
summary(regwofe, set.seed(1230484))

# -------------------------------------------------------------
# topical prevalence: visualization
# -------------------------------------------------------------

# visualization settings (save as prevplot 8 x 11)
highsal <- "dodgerblue"
lowsal  <- "red3"
linesize <- 1
ylim <- c(-0.2, 0.2)
xlimcorp <- c(quantile(x = out$meta$corpmeetlog,
                       probs = seq(0, 1, 0.05))[2], max(out$meta$corpmeetlog))
xlimissue <- c(quantile(x = out$meta$issuemeetlog,
                       probs = seq(0, 1, 0.05))[2], max(out$meta$issuemeetlog))
par(family ="serif", bty = "n", col = "black", lwd = linesize, mfrow = c(3, 2),
    ps = 16, oma = c(0, 0, 2, 0))

# topic 1: Corp
plot(reg, covariate = "corpmeetlog", model = euprevfitlog, topics = 34,
     method = "continuous", xlab = "Amount of corporate group meeting (log)", moderator = "salience",
     moderator.value = 1, linecol = highsal, xlim = xlimcorp, main = "Financial Regulation",
     printlegend = F, ylim = ylim)
plot(reg, covariate = "corpmeetlog", model = euprevfitlog, topics = 34,
     method = "continuous", xlab = "corp. meetings", moderator = "salience",
     moderator.value = 0, linecol = lowsal, add = TRUE,
     printlegend = F, ylim = ylim)
abline(a = 0, b = 0, lwd = 1)

#¸ topic 1: issue
plot(reg, covariate = "issuemeetlog", model = euprevfitlog, topics = 33,
     method = "continuous", xlab = "Amount of issue-ideology group meeting (log)", moderator = "salience",
     moderator.value = 1, linecol = highsal, xlim = xlimissue, main = "Financial Regulation",
     printlegend = F, ylim = ylim)
plot(reg, covariate = "issuemeetlog", model = euprevfitlog, topics = 33,
     method = "continuous", xlab = "corp. meetings", moderator = "salience",
     moderator.value = 0, linecol = lowsal, add = TRUE,
     printlegend = F, ylim = ylim)
abline(a = 0, b = 0, lwd = 1)

# topic 2: corp
plot(reg, covariate = "corpmeetlog", model = euprevfitlog, topics = 2,
     method = "continuous", xlab = "Amount of corporate group meeting (log)", moderator = "salience",
     moderator.value = 1, linecol = highsal, xlim = xlimcorp, main = "Refugee Crisis",
     printlegend = F, ylim = ylim)
plot(reg, covariate = "corpmeetlog", model = euprevfitlog, topics = 2,
     method = "continuous", xlab = "corp. meetings", moderator = "salience",
     moderator.value = 0, linecol = lowsal, add = TRUE,
     printlegend = F, ylim = ylim)
abline(a = 0, b = 0, lwd = 1)

# topic 2: issue
plot(reg, covariate = "issuemeetlog", model = euprevfitlog,
     method = "continuous", xlab = "Amount of issue-ideology group meetings (log)", moderator = "salience",
     moderator.value = 1, linecol = highsal, xlim = xlimissue, main = "Refugee Crisis",
     printlegend = F, ylim = ylim)
plot(reg, covariate = "issuemeetlog", model = euprevfitlog,
     method = "continuous", xlab = "corp. meetings", moderator = "salience",
     moderator.value = 0, linecol = lowsal, add = TRUE,
     printlegend = F, ylim = ylim)
abline(a = 0, b = 0, lwd = 1)

# topic 3: corp
plot(reg, covariate = "corpmeetlog", model = euprevfitlog,
     method = "continuous", xlab = "Amount of corporate group meetings (log)", moderator = "salience",
     moderator.value = 1, linecol = highsal, xlim = xlimcorp, main = "Renewables",
     printlegend = F, ylim = ylim)
plot(reg, covariate = "corpmeetlog", model = euprevfitlog,
     method = "continuous", xlab = "corp. meetings", moderator = "salience",
     moderator.value = 0, linecol = lowsal, add = TRUE,
     printlegend = F, ylim = ylim)
abline(a = 0, b = 0, lwd = 1)

# topic 3: issue
plot(reg, covariate = "issuemeetlog", model = euprevfitlog,
     method = "continuous", xlab = "Amount of issue-ideology group meetings (log)", moderator = "salience",
     moderator.value = 1, linecol = highsal, xlim = xlimissue, main = "Renewables",
     printlegend = F, ylim = ylim)
plot(reg, covariate = "issuemeetlog", model = euprevfitlog,
     method = "continuous", xlab = "corp. meetings", moderator = "salience",
     moderator.value = 0, linecol = lowsal, add = TRUE,
     printlegend = F, ylim = ylim)
abline(a = 0, b = 0, lwd = 1)
title("Effects of corporate group (left) and issue-ideology group (right) lobbying", outer = TRUE)
legend(1.1, 1.62, bty = "n", c("High salience", "Low salience"),
       lwd = linesize, col = c(highsal, lowsal), horiz = TRUE, xpd = "NA")

# -------------------------------------------------------------
# topical content: visualization
# -------------------------------------------------------------

# visualize (save as contplot 8 x 12)
setwd(path)
set.seed(1234)
par(family = "serif", mfrow = c(3, 1)
    , ps = 16
    )

# corporate topic
plot.STM(euprevfitcont, type = "perspectives", topics = 34, family = "serif"
         #,text.cex = 2
         ,main = "(a) Financial Regulation",
         plabels = c("Corporate", "Issue-ideology"))

# issue topic
plot.STM(euprevfitcont, type = "perspectives", topics = 33, family = "serif"
         #,text.cex = 0
         , main = "(b) Refugee Crisis",
         plabels = c("Corporate", "Issue-ideology"))

# both topic
plot.STM(euprevfitcont, type = "perspectives", topics = 2, family = "serif"
         ,text.cex = 2
         , main = "(c) Renewables",
         plabels = c("Corporate", "Issue-ideology"))

###############################################################
# 7. evaluation
###############################################################
# -------------------------------------------------------------
# other topics
# -------------------------------------------------------------

# topical prevalence

# corporate topic: 49 = Migration, 33 = capital markets union, 17 = security and defence
set.seed(1234)
regcontrol <- estimateEffect(c(49, 3, 17) ~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog +
                        year + portfolio,
                      euprevfitlog, metadata = out$meta, uncertainty = "Global")

# regression output
set.seed(1230484)
summary(regcontrol)

rm(regcontrol)

# topical content

# visualize (save as contplotcontrol 8 x 12)
setwd(path)
set.seed(1234)
par(family = "serif", mfrow = c(3, 1)
    , ps = 16
)

# corporate topic
plot.STM(euprevfitcont, type = "perspectives", topics = 49, family = "serif"
         #,text.cex = 2
         ,main = "(a) Migration",
         plabels = c("Corporate", "Issue-ideology"))

# issue topic
plot.STM(euprevfitcont, type = "perspectives", topics = 3, family = "serif"
         #,text.cex = 0
         , main = "(b) Capital Markets Union",
         plabels = c("Corporate", "Issue-ideology"))

# both topic
plot.STM(euprevfitcont, type = "perspectives", topics = 17, family = "serif"
         ,text.cex = 1
         , main = "(c) Security and Defense",
         plabels = c("Corporate", "Issue-ideology"))

# -------------------------------------------------------------
# Other initializations, more iterations and no uncertainty
# -------------------------------------------------------------

# lda initialization
euprevfitloglda <- stm(documents = out$documents, vocab = out$vocab,
                    K = k, prevalence =~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog +
                      year + portfolio,
                    max.em.its = 500, data = out$meta,
                    init.type = "LDA", set.seed(1234))

euprevfitlogldawofe <- stm(documents = out$documents, vocab = out$vocab,
                       K = k, prevalence =~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog,
                       max.em.its = 500, data = out$meta,
                       init.type = "LDA", set.seed(1234))

# new number for the topics
labelTopics(euprevfitloglda, c(45, 30, 2))

# covariate estimation
reglda <- estimateEffect(c(45, 30, 2) ~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog +
                         year + portfolio,
                          euprevfitloglda, metadata = out$meta, uncertainty = "None",
                          set.seed(1234))

regldawofe <- estimateEffect(c(45, 30, 2) ~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog,
                         euprevfitloglda, metadata = out$meta, uncertainty = "None",
                         set.seed(1234))


# regression output
summary(reglda, set.seed(1230484))
summary(regldawofe, set.seed(1230484))

# random initialization
euprevfitlogran <- stm(documents = out$documents, vocab = out$vocab,
                       K = k, prevalence =~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog +
                         year + portfolio,
                       max.em.its = 500, data = out$meta,
                       init.type = "Random", set.seed(1234))

euprevfitlogranwofe <- stm(documents = out$documents, vocab = out$vocab,
                       K = k, prevalence =~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog,
                       max.em.its = 500, data = out$meta,
                       init.type = "Random", set.seed(1234))

# new number for the topics
labelTopics(euprevfitlogran, c(27, 32, 22))

# covariate estimation
regran <- estimateEffect(c(27, 32, 22) ~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog +
                           year + portfolio,
                         euprevfitlogran, metadata = out$meta, uncertainty = "None",
                         set.seed(1234))

regranwofe <- estimateEffect(c(27, 32, 22) ~ corpmeetlog*salience + issuemeetlog*salience + labormeetlog,
                         euprevfitlogran, metadata = out$meta, uncertainty = "None",
                         set.seed(1234))

# regression output
summary(regran, set.seed(1230484))
summary(regranwofe, set.seed(1230484))

# convergence criteria
par(mfrow = c(1,1))
plot(euprevfitlog$convergence$bound, type = "l")

# topical content
euprevfitconteval <- stm(documents = out$documents, vocab = out$vocab,
                     K = k, prevalence =~ competition + salience + labormeetlog +
                       year + portfolio,
                     content =~ competition, max.em.its = 500, data = out$meta,
                     init.type = "LDA", set.seed(1234))

# visualize (save as contplot 8 x 12)
setwd(path)
set.seed(1234)
par(family = "serif", mfrow = c(3, 1)
    , ps = 16
)

# corporate topic
plot.STM(euprevfitcont, type = "perspectives", topics = 34, family = "serif"
         #,text.cex = 2
         ,main = "(a) Financial Regulation",
         plabels = c("Corporate", "Issue-ideology"))

# issue topic
plot.STM(euprevfitcont, type = "perspectives", topics = 33, family = "serif"
         #,text.cex = 0
         , main = "(b) Refugee Crisis",
         plabels = c("Corporate", "Issue-ideology"))

# both topic
plot.STM(euprevfitcont, type = "perspectives", topics = 2, family = "serif"
         ,text.cex = 2
         , main = "(c) Renewables",
         plabels = c("Corporate", "Issue-ideology"))

###############################################################
# 8. word cloud for titlepage
###############################################################

library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(tm)

Rpdf <- readPDF(control = list(text = "-layout"))

# define path
thesis_path <- "C:/Users/Andreas Brand/polybox/05 4th Semester/Thesis/Paper"

# get PDF names
thesis_list <- list.files(pattern = "pdf$", path = thesis_path)  # PDF file names

# use readPDF to create function to read in PDF files (setting working directory to EU Speeches folder)
setwd(path)
thesis <- Corpus(URISource(thesis_list), readerControl = list(reader = Rpdf))

# clean text manually: get rid off \r\n\, name and ETH Zurich
thesis[["ma.thesis.brand.pdf"]][["content"]] <- gsub("[\r\n]", " ",
                                                     thesis[["ma.thesis.brand.pdf"]][["content"]])
thesis[["ma.thesis.brand.pdf"]][["content"]] <- gsub("Brand", " ",
                                                     thesis[["ma.thesis.brand.pdf"]][["content"]])
thesis[["ma.thesis.brand.pdf"]][["content"]] <- gsub("ETH", " ",
                                                     thesis[["ma.thesis.brand.pdf"]][["content"]])
thesis[["ma.thesis.brand.pdf"]][["content"]] <- gsub("Literature Review", " ",
                                                     thesis[["ma.thesis.brand.pdf"]][["content"]])
thesis[["ma.thesis.brand.pdf"]][["content"]] <- gsub("Zurich", " ",
                                                     thesis[["ma.thesis.brand.pdf"]][["content"]])
thesis[["ma.thesis.brand.pdf"]][["content"]] <- gsub("Figueiredo", " ",
                                                     thesis[["ma.thesis.brand.pdf"]][["content"]])
thesis[["ma.thesis.brand.pdf"]][["content"]] <- gsub("Richter", " ",
                                                     thesis[["ma.thesis.brand.pdf"]][["content"]])
thesis[["ma.thesis.brand.pdf"]][["content"]] <- gsub("Andreas", " ",
                                                     thesis[["ma.thesis.brand.pdf"]][["content"]])

# clean PDF texts systematically
thesis_tdm <- TermDocumentMatrix(thesis, control = list(removePunctuation = TRUE,
                                                        stopwords = TRUE,
                                                        tolower = TRUE,
                                                        stemming = FALSE,
                                                        removeNumbers = TRUE)) 

# look at it
inspect(thesis_tdm[1:10,]) 

# prepare data (first change to matrix format, then sort so that highest appearing words is on top,
# and finally make a data frame)
m <- as.matrix(thesis_tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d$word <- tolower(d$word)
head(d, 10)

# generate word cloud
pal <- brewer.pal(9,"Greys")
pal <- pal[-(1:4)]
par(family ="serif")
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 100, random.order = FALSE, rot.per = 0.35, 
          colors = pal, use.r.layout = TRUE, family = "serif")

rm(d, m, thesis, thesis_tdm, thesis_list, thesis_path, v, Rpdf, pal)

#######
# END #
#######