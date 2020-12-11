###############################################################
#
#      Motivations, qualifications, and strategies of science 
#               communicators on YouTube: 
#         a case study of the french ecosystem
#                 Part 0: survey data preparation
#
###############################################################

library(forcats)

#--------------------------
#     Data reading
#--------------------------

# Loading survey replies
dataread <- read.csv("Data/Survey_replies.csv", header = T, sep = ",", encoding = "UTF-8")

# Removing duplicates
# dataread <- dataread[-c(37, 114),]

# Number of answers
nrep <- nrow(dataread)

#--------------------------
#   Prepare the data
#--------------------------

# Initialize the data.frame
datatab <- data.frame(time_stamp = dataread[1])

#----- Gender -----
# I attribute NA to missing values (which are replied from institutions) and "je préfère ne pas dire"
datatab$gender <- fct_collapse(dataread[,17], Male = "Masculin", Female = "Féminin",
  Other = "Autre", other_level = "NA")

#----- Number of people behind the channel -----
# Is the creator working alone
npeople_str <- dataread[,2]

# For those not working alone, add the number of people involved
several <- npeople_str == "Nous sommes plusieurs à travailler sur les vidéos de façon régulière" 
npeople_str[several] <- dataread[several,3]
datatab$npeople <- factor(npeople_str, c("Je travaille seul(e)",
    "Je travaille majoritairement seul(e) mais fais parfois appel à d'autres personnes",
    as.character(2:10), "10 et plus"),
  c("1", "~ 1", as.character(2:10), "> 10"),
  ordered = T
)

#----- Institutions -----
datatab$institution <- factor(dataread[,4], 
  labels = c("Individual", "Institution")
)

#----- Time spent on the channel -----
# /!\ Field concerns only institutions

# Obtain the yes and add the percentage for the others
tc <- dataread[,5]
isno <- dataread[,5] != "Oui"
tc[isno] <- dataread[isno,6]

datatab$time_spent <- factor(tc, c("1", "5", as.character(seq(10,90,10)), "Oui", 
    "Un temps plein ou plus, mais réparti sur différentes personnes"),
  c("1", "5", as.character(seq(10,90,10)), "100", "~100"),
  ordered = T  
)

#----- Education in video making -----
datatab$EducationVideo <- factor(dataread[,19],
  levels = c("Non, j'apprends sur le tas", 
    "Oui et je l'avais avant de lancer ma chaîne",
    "Oui mais elle date d'après le lancement de ma chaîne"), 
  labels = c("None", "Prior", "Posterior"))
  
#----- Highest degree -----
datatab$degree <- factor(dataread[,18], c("Brevet des collèges", "Baccalauréat", 
  "Bac +2", "Bac +3", "Bac +5", "Bac +8 et au-delà"),
  labels = c("General", "High-School", "High-School +2", "Bachelor", "Master", "PhD"),
  ordered = T)

#----- Expertise of the subject -----
datatab$expertise <- factor(dataread[,48], 
  c("Oui (de façon directe)", "A peu près", "Non", 
    "javais commence ses études en cette voie, mais j'ai basculé vers une formation audiovisuelle",
    "Mes études ne sont pas encore spécialisées",
    "Oui pour les cours que je donne moi-même (biologie). Les autres cours sont données par des spécialistes des domaines concernés.",
    "oui pour la chaine environnement, non pour la chaine histoire",
    "Mon parcours ayant été chaotique il faudrait que je puisse répondre à la fois \"oui, non, oui et non\" "),
  c("Yes, directly", "Somewhat", "No", "Somewhat", "No", "Yes, directly", 
    "Somewhat", "Somewhat"),
  exclude = "Non applicable"
)
  
#----- Professional situation -----
# type of employment
sep_field <- strsplit(dataread[,20], ";")
list_fact <- sapply(sep_field, factor, 
  c("Etudiant(e)", "Doctorant(e) rémunéré(e)", "Doctorant(e) sans rémunération", 
    "Salarié(e) dans l'Enseignement ou la Recherche", 
    "Salarié(e) dans la CSTI", "Salarié(e) dans un autre domaine", 
    "Indépendant(e) (autoentreprise, profession libérale...)", 
    "En recherche d'emploi dans l'Enseignement ou la Recherche", 
    "En recherche d'emploi dans la CSTI", 
    "En recherche d'emploi dans un autre domaine", "Néant.",
# Below are the weird cases
    "Mon contrat de doctorant vient de se finir... un peu flou pour l'instant",    
    "\"Chercheur bénévole\" dans un certains sens", 
    "Artiste-auteur", 
    "salarié de la societe de production qui porte lactivitz de videaste que jai développé ",
    "Freelance post-covid. Autrement dit freelance au chômage !",
    "Salarié par portage salarial + auteur (payé en note d'auteur via la société de portage)",
    "Ancien directeur de recherche au Cnrs",
    "Salarié dans ma propre association", 
    "Intermittent", 
    "Pendant le plus gros de la production, et une bonne partie de la diffusion, le cour de l'équipe était en doctorat rémunéré. Mais d'autres étaient, non scientifiques, étaient en autoentreprise. Aujourd'hui, certains sont en post-docs. Moi personnellement, je suis en R&D privée. ",
    "Interne en médecine",
    "futur doctorant rémunéré",
    "Chef d'entreprise - Innovaxiom",
    "Autoentrepreneur",
    "cadre",
    "Chef d'entreprise",
    "Professeur certifié"),
  c("Student", "Employee", "Employee", "Employee", "Employee", "Employee", 
    "Self-employed", "Unemployed", "Unemployed", "Unemployed", "Unemployed", 
# And here I classify the weird cases (not sure about what I did)
    "Unemployed", "Self-employed", "Self-employed", "Employee", "Unemployed",
    "Employee", "Employee", "Employee", "Self-employed", "Student", "Student",
    "Student", "Self-employed", "Self-employed", "Employee", "Self-employed",
    "Employee")
)
datatab$proCat <- list_fact

# Is in academia
academ_list <- sapply(sep_field, fct_collapse, 
  Yes = c("Doctorant(e) rémunéré(e)", 
    "Doctorant(e) sans rémunération", 
    "Salarié(e) dans l'Enseignement ou la Recherche",
    "En recherche d'emploi dans l'Enseignement ou la Recherche",
    "Mon contrat de doctorant vient de se finir... un peu flou pour l'instant",
    "Ancien directeur de recherche au Cnrs", "futur doctorant rémunéré",
    "Professeur certifié"),
  Na = "",
  other_level = "No" 
)
datatab$Academia <- academ_list

#----- Type of funding -----
sep_field <- strsplit(dataread[,43], ";")
sep_field[sapply(sep_field, length) == 0] <- ""

datatab$funding <- sapply(sep_field, factor,
  c("Apports personnels / de l'institution", "Publicités via la plateforme de diffusion", 
    "Placement de produits", "Sponsoring et partenariats autres que du placement de produits", 
    "Autres prestations vidéos pour des entreprises / institutions",
    "Prestations auprès d'institutions / associations culturelles et scientifiques",
    "Appels à projets", "Dons (via Tipeee, uTip ou Patreon, par exemple)", 
    "Mécénat", "Vente de produits dérivés", "Droits d'auteur",
    "droits d'auteurs (SCAM, édition livre), participation à d'autres chaines",
    "La production de mes vidéos ne m'a jamais rien coûté",
    "Je fais tout moi-même, gratuitement", "Aides et subventions (type CNC, régions...)",
    "Other"),
  c("Personal", "Advertisement", "Advertisement", "Sponsoring", "Sponsoring", 
    "Sponsoring", "Sponsoring", "Donations", "Donations", "Sales", "Copyright", 
    "Sales", "None", "None", "Subsidies", "Other"),
  exclude = ""
) 

#----- Balance -----
# NB: these fields have been set to NA in the shared data for confidentiality reasons
datatab$balance <- factor(dataread[,45], c("Négatif : vous perdez de l'argent",
  "Neutre", "Positif : vous gagnez de l'argent"),
  c("Negative", "Neutral", "Positive"), exclude = "", ordered = T  
)

datatab$mainSource <- factor(dataread[,46], 
  c("Oui et elle est suffisante pour vivre", "Oui mais j'ai besoin d'autres revenus pour vivre",
    "Cela pourrait être ma source principale de revenu mais je préfère garder un travail stable",
    "Non mais ça représente une bonne part de mon revenu", "Non, ce revenu est faible"),
  c("Yes, and it is enough", "Yes, but I need another source of income",
    "It could be but I prefer keeping a stable job", "No but it is a large part",
    "No, it is not enough"),
  exclude = "Non concerné(e)") 

#----- Pro wish -----
datatab$prowish <- factor(dataread[,47], 
  c("J'aimerais que cela devienne mon revenu principal",
    "J'aimerais que cela devienne un revenu d'appoint",
    "Amortir les frais me suffi(rai)t",
    "Ce n'est pas l'objectif, peu m'importe"),
  c("Main income", "Secondary income", "Neutral balance", "Not interested"),
  exclude = c("Non concerné(e)", ""), ordered = T
)

#----- Institutions: channel objective -----
datatab$institutionGoal <- factor(dataread[,9], 
  c("Promotion de l'institution", "Promotion de la science", "Les deux", 
    "Nous avons plusieurs chaînes pour remplir ces différents objectifs",
    "Aucun des deux"),
  c("Institution", "Science", "Both", "Sep. channels", "None"),
  exclude = ""
)

#----- Income -----
# NB: this field has been set to NA in the shared data for confidentiality reasons
# datatab$income <- dataread[,44]
datatab$income <- NA

#----- Institutions: channel priority -----
datatab$priority <- dataread[,10]

#----- Institutions: channel satisfaction -----
datatab$satisfaction <- dataread[,11]

#----- Institutions: initial reluctance -----
datatab$reluctance <- dataread[,12]

#----- Institutions: positive feedback -----
datatab$feedback <- dataread[,13]

#----- Wide subject -----
fields <- c("Agronomie", "Anthropologie", "Archéologie", "Astronomie", "Biologie", "Chimie", "Droit", "Économie", "Éducation", "Environnement", "Géographie", "Géologie", "Histoire", "Informatique", "Ingéniérie", "Linguistique", "Mathématiques", "Médecine", "Neurosciences", "Océanographie", "Paléontologie", "Philosophie", "Physique", "Psychologie", "Scepticisme / Zététique", "Sciences politiques", "Sociologie", "Technologie", "Autre")
eng_fields <- c("Agronomy", "Anthropology", "Archaeology", "Astronomy", "Biology", "Chemistry", "Law", "Economics", "Education", "Environment", "Geography", "Geology", "History", "Computer Science", "Engineering", "Linguistics", "Mathematics", "Medicine", "Neurosciences", "Oceanography", "Paleontology", "Philosophy", "Physics", "Psychology", "Skepticism", "Political Science", "Sociology", "Technology", "Other")

fields_spl <- strsplit(dataread[,27], ";")
fields_list <- sapply(fields_spl, factor, fields, eng_fields)
fields_list <- sapply(fields_list, function(x) {
  x[is.na(x)] <- "Other"
  x
})

datatab$field <- sapply(fields_spl, factor, fields, eng_fields)

#----- Number of channels owned -----
datatab$nchannel <- factor(dataread[,23], c(as.character(1:5), "5 et plus"),
  c(as.character(1:5), "> 5"),
  ordered = T)

#----- Publication frequency -----
datatab$pubFrequency <- dataread[,38]

#----- Production type -----
datatab$faceCam <- factor(dataread[,39], 
  c("Oui, et c'est le format principal", "Oui", "Non"),
  c("Mainly", "Sometimes", "No"), exclude = "")
datatab$animation <- factor(dataread[,41], 
  c("Oui, et c'est le format principal", "Oui", "Non"),
  c("Mainly", "Sometimes", "No"), exclude = "")
  
#----- Number of subscribers -----
datatab$subs <- dataread[,26]

#----- Target audience -----
target_spl <- strsplit(dataread[,29], ";")
target_list <- sapply(target_spl, factor, 
  c("Enfants", "Adolescents", "Adultes", "Spécialistes du domaine traité"),
  c("Children", "Teenagers", "Adults", "Experts"))
datatab$target <- target_list

#----- Actual audience -----
audience <- dataread[,grep("Proportion", names(dataread))]
names(audience) <- sprintf("Prop%s", c("Female", "13-17", "18-24", "25-34", "35-44", 
  "45-54", "55-64", "65+"))
datatab <- cbind(datatab, audience)

#---- Communicator age ----
datatab$Age <- dataread[,15]

#---- Channel age ----
cage <- dataread[,24]
cage[cage > 2000] <- 2020 - cage[cage > 2000]
datatab$Channel_age <- cage

#--------------------------
#   Export
#--------------------------

saveRDS(datatab, file = "Data/Survey_data.RDS")