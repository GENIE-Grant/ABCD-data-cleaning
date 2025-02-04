
Raw <- haven::read_sav('data/Schaumberg_22June22.sav')
Scoresheet <- readxl::read_excel('scoresheets/to_check/NEW_ED_cognitions.xlsx')

scorekeeper::scorekeep(Raw,Scoresheet)
