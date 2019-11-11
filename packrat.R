# DO THIS 1 TIME
install.packages("packrat")

#packrat::init()

# ADD NEW PACKAGES TO REPO, DO THIS IF YOU NEED TO INSTALL ANY PACKAGES 
packrat::snapshot()

# LOAD SNAPSHOT DO THIS WHEVER YOU SIT DOWN TO WORK TO MAKE SURE YOU ARE UP TO DATE 
packrat::status()
