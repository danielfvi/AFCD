
#===============================================================================
# Clean AFCD 
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@uw.edu
# For: BFA Nutrition paper
# Date started: 02/20/2018
# Revised: 02/28/2018
#===============================================================================


# # set global options 
# options(stringsAsFactors=FALSE)
# Sys.setenv(LANG = "en",stringsAsFactors=FALSE)


#_____________________________________________________________________________________________
# read data and load libraries directory defaults
# ________________________________________________________________________________________________
# 
# library(tidyverse)
# 
# # filenames
# afcd_file <- "AFCD_live.csv"
# 
# # work directory, set yours using wk_dir here!
# wk_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Nutrition/Separate/aquatic_foods_nutrient_database"
# 
# 
# afcd_dat <- read.csv(
#   file.path(wk_dir,
#             "data",
#             "OutputsFromR",
#             "aquatic_food_composition_database",
#             afcd_file),
#   header = TRUE
# )

# preparation - main categories
# use these categories, and then anything that is left uncategorized goes into the 'few observations' bin
frozen <- c("frozen","Frozen")
raw <- c("raw","Raw","r","wet","crudo","cruda","cru","Fresh","flesh","flesh","sa","raw \\(Alaska Native\\)","Raw","crudo","fridge","minced")
freezedried <- c("freeze-dried", "freeze dried","Freeze dried ",
                 "freezedried","Freeze-dried","freeze-dried, dried",
                 "freeze fried", #clearly spelling error!
                 "Freezedried")
rehydrated <- c("rehydrated")
unknown_preparation <- c("unknown","NA","b","sl","not specified",
	"conserva","0","Control","wet","fz","fe","e","Experiment")
dried <- c("dried","dry","d","Dry","dried and homogenized",
           "air-dried","Dried","seco")
baked <- c("baked","baked","baked / broiled","al horno","broiled","bake","broiled/baked")
boiled_steamed <- c("boil","boiled","boiled ","boiled/steamed","steamed","st","hervido","poached","precocido","cozido")
fried <- c("fried","f","frito","frita")
canned <- c("canned","c","enlatado","canned, drained","enlatada")
smoked <- c("smoked","sm")
microwaved <- c("microwave","microwaved")
oil <- c("oil")
cooked <- c("cooked","cooked, dry heat","cooked, moist heat",
            "sancochado","cocida","cocido","roasted")
brined <- c("brined")
grilled <- c("grilled","assado/40 min")
acid_digestion <- c("acid digestion")
salted <- c("salted","s")
aged <- c("aged")
curried <- c("curried")
combination <- c("freeze-dried, aged","boiled, frozen",
                 "freeze-dried, boiled","m","ds",
                 "salted \\+ dried","boild, dried",
                 "kippered, canned","d; sm","kippered","kippered",
                 "dried \\+ smoked","smoked/dried","b;d",
                 "salted \\+ dried","freeze-dried, aged","freeze-dried, boiled",
                 "boiled, frozen","canned in oil","m",
                 "canned in oil, drained","boild, dried","kippered, canned",
                 "cooked in oil","cooked in rice","dried + smoked","smoked/dried",
                 "smoked, canned","salted \\+ rehydrated","smoked/baked","d; sm","canned with salt",
                 "salted \\+ fermented","b;d","dried, salted","salted \\+ smoked"
                 )


# problematic variables


muscle_tissue <- c(
	"f","meat","muscle","fillet","Muscle ","carne","muscle ","dorsal muscle","epaxial muscle","Muscle","soft tissue",
	"filete","flesh","Meat","Fillet and skinned","pulpa","meat ","fillet ","muscle, dark meat",
    "flesh and skin","fs","Muscle and skin", #flesh with skin
    "filé","músculo dorsal ","d","adductor muscle","foot","foot muscle","Adductor muscle","Foot","adductor","abdominal muscle",
    "middle cut","muscle + skin","muscles","juvenile muscle","breast","f, skin","Fillet ","Fillet with skin",
    "light meat","meat + subcutaneous fat","Muscles","v","músculo ventral","tail muscle"
    )
whole <- c("w","whole","fb","whole body","Whole","Whole fish","juvenile whole",
	"cuerpo","fingerlings","entero","entera","fhb","Whole body","animal entero","fbhe")
reproductive_tissue <- c("gonads","gonad","Ovaries","Gonad","ovary","Gonads")
body_wall <- c("thalli","body wall")
gills <- c("gills","gill","Gills")
roots <- c("Meristematic tip","Meristem")
unknown_part <- c("unknown","not specified","tunic coat","m","l","soma","","solids + liquid","s","Mid",
	"solids + liquids","drained solids","fhe","smoked",
	"meat, bone + liquid","solids, liquid, bones","trunk","carne con aparato digestivo","no holdings",
	"con espinas","contenido total","composite sample for each species"
	)
mantle <- c("mantle","Mantle")
cephalothorax <- c("cephalothorax")
blade <- c("blade","Blade")
stipe <- c("stipe","Stipe","stipes")
holdfast <- c("Holdfast","holdfast")
eyes <- c("eyes")
edible <- c("e","edible")
raw <- c("r","raw")
scale <- c("scale","scales")
seeds <- c("seeds")
skin <- c("skin","skein","Skin")
liver <- c("liver","juvenile liver","Liver")
viscera <- c(
	"viscera","hepatopancreas","intestine","stomach","visceral mass",
	"digestive gland","offal","Midgut gland","digestive tract","inteiro","g","head and viscera"
	)
leg <- c("leg","legs")
tail <- c("tail","tail end")
shell <- c("Shell","shell")
roe <- c("eggs","roe","egg","eggs ","Fertalized eggs")
blubber <- c("blubber","mattak","muktuk","mattack")
frond <- c("frond","leaf")
bone <- c("bone","vertebrate","bones","vertebrae","frame")
claw <- c("claw")
flippers <- c("flippers","fin","flipper (w/o skin + bone)")
heart <- c("heart")
gutted <- c("gutted","excluding viscera","head, rear and the middle","cleaned")
fats <- c("oil","grease","fat","liver oil")
larvae <- c("Larvae","larvae")
head <- c("head","brain","head end","head, eyes, cheeks \\+ soft bones")
kidney <- c("kidney","kidneys")
combination <- c("muscle and liver","solids \\+ bones","eggs on kelp","whole, no liver")
gelatin <- c("gelatin")


