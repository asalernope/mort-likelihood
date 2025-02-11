###########################################################
#  Balkan disturbance - survival
#  Add species codes to tree data
#  Dec 2024
###########################################################

rm(list=ls(all=TRUE))

# User computer
computer <- "~"

# Directores
if(computer == "~"){
  Dir.Base <- "~/Desktop/Prague/Code"
}else{
  Dir.Base <- "insert path"
}

Dir.Data <- file.path(Dir.Base, "Data")
Dir.Code <- file.path(Dir.Base, "Code")
dirs <- sapply(c(Dir.Base, Dir.Data, Dir.Code), function(x) if(!dir.exists(x)) dir.create(x) )


# get tree tree data
targets <- load(file=file.path(Dir.Data,"A1_Balkan_trees_no_cores.Rdata"))
targets <- get(targets)


# read species table
setwd(Dir.Data)
spp_table <- read.table("Species_table.txt", header=T)
spp_table <- spp_table[,!names(spp_table) %in% c("Genus_nam","Species_nam","functional.grp")]

# attach
targets <- merge(targets,spp_table,by="species",all.x=T)
unique(targets$SPCD)
unique(targets$species) # 19 species


#####
# remove unnecessary columns
#names <- c("id","date_re","plotsize_re","tree_id_re","dbh_mm_re","plot_id_re")
#targets <- targets[,!names(targets) %in% names]
str(targets)   # 11811 trees

#####
# save
save(targets,file=file.path(Dir.Data,"A2_Balkan_trees_no_cores_code.Rdata"))





