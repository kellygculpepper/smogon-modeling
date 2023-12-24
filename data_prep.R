# Full code for generating the post & pre datasets

library(tidyverse)
library(data.table)

df = data.frame(name = character(0), usage = numeric(0), month = numeric(0),
                           year = numeric(0))

# Read usage rates & counts for all of gen 9 OU
for (month in c(11,12)) {
  month_str = ifelse(month < 10, paste0("0", month), toString(month))
  link = paste0("https://smogon.com/stats/2022-", month_str, "/gen9ou-0.txt")
  monthly_data = fread(link, skip = 5, header = FALSE, strip.white = TRUE, sep = "|",
                       fill = TRUE, select = c(3, 4, 5), na.string = "") %>%
    na.omit()
  colnames(monthly_data) = c("name", "usage", "count")
  monthly_data$year = 2022
  monthly_data$month = month
  monthly_data$usage = as.numeric(gsub("%", "", monthly_data$usage))
  monthly_data$count = as.numeric(monthly_data$count)
  df = rbind(df, monthly_data)
}

for (month in seq(1:11)) {
  month_str = ifelse(month < 10, paste0("0", month), toString(month))
  link = paste0("https://smogon.com/stats/2023-", month_str, "/gen9ou-0.txt")
  monthly_data = fread(link, skip = 5, header = FALSE, strip.white = TRUE, sep = "|",
                       fill = TRUE, select = c(3, 4, 5), na.string = "") %>%
    na.omit()
  colnames(monthly_data) = c("name", "usage", "count")
  monthly_data$year = 2023
  monthly_data$month = month
  monthly_data$usage = as.numeric(gsub("%", "", monthly_data$usage))
  monthly_data$count = as.numeric(monthly_data$count)
  df = rbind(df, monthly_data)
}


# Create df_overall w/ overall mean usage rate per mon (used for EDA stat plots)
df_overall = df %>%
  group_by(name) %>%
  summarise(usage = mean(usage))

# Give the mean usage rate & total count of each mon for pre-HOME (period = 0)
# and post-HOME (period = 1)
df = df %>%
  mutate(period = if_else(year == 2023 & month > 5, 1, 0)) %>%
  group_by(name, period) %>%
  summarise(usage = mean(usage), count = sum(count))

# Load data from pokemonData repo
mons = read.csv("data/Pokemon.csv")

# Add info on DLC mons
dlc_mons = data.frame(ID = c(1011, 1012, 1013, 1014, 1015, 1016, 1017, 1017, 1017, 
                             1017),
                      Name = c("Dipplin", "Poltchageist", "Sinistcha", "Okidogi",
                               "Munkidori", "Fezandipiti", "Ogerpon", "Ogerpon",
                               "Ogerpon", "Ogerpon"),
                      Form = c("", "", "", "", "", "", "", "Cornerstone",
                               "Hearthflame", "Wellspring"),
                      Type1 = c("Grass", "Grass", "Grass", "Poison", "Poison",
                                "Poison", "Grass", "Grass", "Grass", "Grass"),
                      Type2 = c("Dragon", "Ghost", "Ghost", "Fighting", "Psychic",
                                "Fairy", "", "Rock", "Fire", "Water"),
                      Total = c(485, 308, 508, 555, 555, 555, 550, 550, 550, 550),
                      HP = c(80, 40, 71, 88, 88, 88, 80, 80, 80, 80),
                      Attack = c(80, 45, 60, 128, 75, 91, 120, 120, 120, 120),
                      Defense = c(110, 45, 106, 115, 66, 82, 84, 84, 84, 84),
                      Sp..Atk = c(95, 74, 121, 58, 130, 70, 60, 60, 60, 60),
                      Sp..Def = c(80, 54, 80, 86, 90, 125, 96, 96, 96, 96),
                      Speed = c(40, 50, 70, 80, 106, 99, 110, 110, 110, 110),
                      Generation = rep(9, 10)
)

mons = rbind(mons, dlc_mons)

# Pokémon with weird names, need to be matched manually to correct row
manual_names = c("NidoranF", "NidoranM", "Wo-Chien", "Chien-Pao", "Ting-Lu",
                 "Chi-Yu", "Tauros-Paldea", "Tauros-Paldea-Combat", "Tauros-Paldea-Fire",
                 "Tauros-Paldea-Blaze", "Tauros-Paldea-Water", "Tauros-Paldea-Aqua",
                 "Flabebe")
manual_rows = c(29, 32, 1185, 1186, 1187, 1188, 1076, 1076, 1077, 1077, 1078,
                1078, 744)

# Function that returns row of pokemonData for the Pokemon with (Smogon) name x
get_mon_row = function(x) {

  # handle manual matches
  row_num = manual_rows[which(manual_names == x)]
  if (length(row_num) == 1) {
    return(mons[row_num,])
  }
  
  # handle mons with no form/variant specified
  if (!str_detect(x, "-")) {
    no_form_match = mons %>% filter(Name == x & str_trim(Form) == "") %>% slice_head()
    if (nrow(no_form_match) > 0) return(no_form_match)
    return(mons %>% filter(Name == x) %>% slice_head())
  }
  
  # handle regionals
  if (str_detect(x, "Galar|Paldea|Alola")) {
    region = str_extract(x, "Galar|Paldea|Alola")
    name = str_extract(x, "^[^-]*")
    return(mons %>% filter(str_detect(Form, region) & str_detect(Name, name)) %>% slice_head())
  }
  
  # split by hyphen
  parts = str_split(x, "-", simplify = TRUE)
  
  # attempt to match form
  form_match = mons %>% filter(sapply(Form, function(f) all(str_detect(tolower(f), tolower(parts[2:length(parts)]))))) %>% slice_head()
  if (nrow(form_match) > 0) return(form_match)
  
  # if matching form fails use name
  return(mons %>% filter(str_detect(tolower(Name), tolower(parts[1]))) %>% slice_head())
}

# Combine usage data & Pokemon data into one df
new_rows = data.frame()
for (i in seq(nrow(df))) {
  name = df[i,]$name
  row = get_mon_row(name)
  new_rows = rbind(new_rows, row)
}
df = cbind(df, new_rows) %>%
  mutate(Type2 = str_trim(Type2))

# Do same for overall df
new_rows_overall = data.frame()
for (i in seq(nrow(df_overall))) {
  name = df_overall[i,]$name
  row = get_mon_row(name)
  new_rows_overall = rbind(new_rows_overall, row)
}
df_overall = cbind(df_overall, new_rows_overall)
write.csv(df_overall, "data/overall.csv")

# Add type multiplier columns
type_chart = read.csv("data/typechart.csv")

get_multiplier = function(atk_type, def_type) {
  mult = ifelse(def_type == "", 1.0, type_chart[type_chart$def == def_type, atk_type])
  return(mult)
}

types = unique(df$Type1)
for (type in types) {
  df = df %>%
    mutate(
      "mult_{type}" := get_multiplier(type, Type1) * get_multiplier(type, Type2)
    )
}

# IDs of mons with pivot moves: u-turn, volt switch, flip turn
u_turn = c(187, 188, 189, 193, 207, 456, 457, 469, 472, 571, 619, 620, 648,
           724, 816, 817, 818, 886, 887, 893, 906, 907, 908, 944, 945, 967,
           49, 52, 53, 56, 57, 123, 144, 145, 146, 151, 161, 162, 190, 198,
           212, 278, 279, 284, 313, 314, 357, 390, 391, 392, 396, 397, 398,
           416, 417, 424, 430, 480, 481, 482, 489, 490, 570, 603, 604, 627, 
           628, 629, 630, 635, 636, 637, 641, 642, 645, 648, 656, 657, 658,
           661, 662, 663, 666, 692, 693, 701, 702, 714, 715, 724, 734, 735,
           741, 742, 743, 763, 766, 775, 810, 811, 812, 813, 814, 815, 821,
           822, 823, 841, 863, 873, 886, 887, 891, 892, 900, 903, 914, 918,
           919, 920, 924, 925, 931, 940, 941, 955, 956, 962, 973, 979, 988,
           991, 993, 994, 1005, 1007, 1008, 1015, 1016, 1017)
flip_turn = c(964, 991, 54, 55, 134, 151, 211, 350, 370, 393, 394, 395, 418, 
              419, 456, 457, 489, 490, 501, 502, 503, 550, 581, 594, 690,
              691, 692, 693, 779, 818, 846, 847, 875, 902, 913, 914, 976,
              1009, 7, 8, 9, 116, 117, 118, 119, 120, 121, 134, 141, 230,
              260, 318, 319, 647, 728, 729, 730, 746)
volt_switch = c(403, 404, 405, 642, 702, 940, 941, 25, 26, 74, 75, 76, 81, 
                82, 100, 101, 135, 145, 151, 172, 179, 180, 181, 205, 299,
                417, 462, 476, 479, 603, 604, 736, 737, 738, 801, 849, 877,
                894, 921, 922, 923, 938, 939, 989, 990, 992, 995, 1008)
pivot_ids = c(u_turn, flip_turn, volt_switch)

# IDs of mons with hazard control: defog, rapid spin, court change, tidy up,
# mortal spin, magic bounce
defog = c(110, 163, 164, 487, 549, 580, 581, 627, 628, 629, 630, 873, 123, 212, 900,
          273, 274, 275, 333, 334, 425, 426, 532, 533, 534, 661, 662, 663, 701, 
          714, 715, 722, 723, 724, 741, 753, 754, 821, 822, 823, 845)
rapid_spin = c(27, 28, 204, 205, 232, 324, 615, 712, 713, 761, 762, 763, 775,
               837, 838, 839, 894, 946, 947, 967, 984, 990, 225, 877, 912, 913, 
               914, 948, 949, 978)
other_hazard_control = c(970, 815, 925, 161, 162, 177, 178, 196, 856, 857, 858)
hazard_control_ids = c(defog, rapid_spin, other_hazard_control)

# IDs of mons with hazard-setting moves: stealth rock, spikes (incl. ceaseless
# edge), toxic spikes
rocks = c(74, 75, 76, 703, 719, 744, 745, 837, 838, 839, 874, 932, 933, 934, 
          969, 970, 995, 27, 28, 35, 36, 39, 40, 50, 51, 52, 57, 58, 59, 113,
          151, 185, 194, 195, 204, 205, 206, 207, 472, 219, 220, 221, 222, 473,
          231, 232, 242, 246, 247, 248, 299, 322, 323, 324, 339, 340, 383, 384,
          385, 386, 387, 388, 389, 390, 391, 392, 395, 422, 423, 436, 437, 
          438, 443, 444, 445, 449, 450, 476, 480, 481, 482, 483, 485, 493, 
          551, 552, 553, 624, 625, 635, 645, 713, 749, 750, 769, 770, 784, 834,
          843, 844, 863, 878, 879, 900, 932, 933, 934, 950, 957, 958, 959, 962,
          968, 969, 970, 979, 980, 982, 983, 984, 985, 989, 990, 995, 1003, 503)
spikes = c(91, 204, 205, 211, 331, 332, 658, 904, 1003, 27, 28, 90, 151, 185,
           194, 195, 207, 214, 225, 340, 361, 362, 383, 416, 423, 438, 445, 
           472, 478, 650, 651, 652, 656, 657, 703, 707, 719, 801, 838, 839, 
           871, 908, 917, 918, 946, 947, 948, 949, 968, 969, 970, 980, 989,
           995, 1003, 1017, 724)
tspikes = c(91, 194, 205, 211, 747, 748, 904, 970, 980, 23, 24, 48, 49, 89, 
            90, 93, 94, 109, 110, 151, 167, 168, 195, 204, 207, 215, 275, 316,
            317, 331, 332, 416, 434, 435, 472, 656, 657, 658, 690, 691, 849, 
            871, 890, 903, 908, 917, 918, 948, 949, 965, 966, 969, 994)
hazard_ids = c(rocks, spikes, tspikes)

# Add indicator variables for move categories
df = df %>%
  mutate(pivot = ifelse(ID %in% pivot_ids, 1, 0),
         hazard_control = ifelse(ID %in% hazard_control_ids, 1, 0),
         hazard = ifelse(ID %in% hazard_ids, 1, 0)) %>%
  mutate(pivot = as.factor(pivot), 
         hazard_control = as.factor(hazard_control), 
         hazard = as.factor(hazard))

# Rename variables & remove extraneous columns
# note: added Type1 and Type2 back in for EDA boxplot
df = df %>%
  select(!c(ID, Name, Form, Generation)) %>%
  rename(atk = Attack,
         def = Defense,
         spatk = Sp..Atk,
         spdef = Sp..Def,
         spd = Speed,
         hp = HP,
         bst = Total)

# Create pre- and post-HOME dfs
df_pre = df %>%
  filter(period == 0) %>%
  select(!period)

df_post = df %>%
  filter(period == 1) %>%
  select(!period)

# total numbers of teams (= 2 * total number of battles, since 2 teams/battle)
# in each period. retrieved manually from top of each usage stats page
total_pre = (2531335+3385255+2188410+1466284+1511656+1194529+1282631)*2
total_post = (1999363+1336974+1250214+1912075+1709112+1339482)*2

# Add column for total no. teams to each df (need this for regression offset)
df_pre = df_pre %>%
  mutate(total = total_pre)
df_post = df_post %>%
  mutate(total = total_post)

write.csv(df_pre, "data/pre.csv")
write.csv(df_post, "data/post.csv")