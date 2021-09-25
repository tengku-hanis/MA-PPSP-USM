# Data for ivermectin studies
# https://www.medrxiv.org/content/10.1101/2021.04.30.21256415v2

iver <- 
  read.table(header = T, text = "
             even.e n.e event.c n.c
             3 82 4 82
             17 110 17 55
             0 42 0 20
             0 34 103 942
             0 110 2 137
             0 12 0 12
             0 110 0 62
             0 62 0 51
             12 53 25 115
             3 15 77 1020
             5 36 8 70
             0 16 2 71
             48 122 39 74
             2 70 6 70
             1 115 9 133
             0 55 4 57
             13 434 52 287
             0 200 1 198
             1 10 5 15
             0 183 3 180
             0 100 0 52
             0 50 0 50
             4 120 11 60
             6 30 9 30
             26 173 27 107
             1 35 0 34
             132 561 940 5122
             4 250 3 251
             1 50 0 50")
iver$studyID <- paste0(rep("study"), 1:29)
iver$study_type <- "clinical" 
iver$study_type[c(2,4,5,10,13,15,19,22,25,27)] <- "nonClinical"

