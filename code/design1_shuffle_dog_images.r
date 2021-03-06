library(data.table)

breed_select = function(){
  DT = data.table()
  breednames = c("Shih_Tzu", "Yorkshire_Terrier", "Boston_Bull", "Cocker_Spaniel", 
                 "Golden_Retriever", "Bloodhound", "Saluki", "Irish_Wolfhound")
  for (i in 1:8) {
    easy = sample(x = c(4,6,8,10,12), 3)
    hard = sample(x = c(5,7,9,11,13), 3)
    combined = sample(x = c(easy,hard), 6, replace = F)
    breed = rep(breednames[i], 6)
    DT = rbind(DT, data.table(breed = breed, id = combined))
  }
  shuffle_id = sample(x = 1:48, 48, replace = F)
  DT[shuffle_id,]
  }

breed_select()


# DESIGN 1
question number, breed, image id on google drive 
------------------------------------------------
1:       Boston_Bull 12
2:          Shih_Tzu  9
3:          Shih_Tzu  5
4:    Cocker_Spaniel  7
5:   Irish_Wolfhound 12
6:  Golden_Retriever  7
7:    Cocker_Spaniel  8
8: Yorkshire_Terrier  6
9:            Saluki  8
10:          Shih_Tzu  4
11: Yorkshire_Terrier  8
12:        Bloodhound  5
13:          Shih_Tzu 11
14:   Irish_Wolfhound  5
15:            Saluki 10
16: Yorkshire_Terrier  9
(Please insert question for cat_1 image here)
17:    Cocker_Spaniel 13
18:  Golden_Retriever  9
19:  Golden_Retriever  4
20:   Irish_Wolfhound  9
21: Yorkshire_Terrier 13
22:        Bloodhound 10
23:    Cocker_Spaniel  9
24:            Saluki  4
25:       Boston_Bull 11
26:            Saluki 11
27:    Cocker_Spaniel  6
28:   Irish_Wolfhound  6
29:  Golden_Retriever  8
30: Yorkshire_Terrier  4
31:            Saluki  5
32:    Cocker_Spaniel 12
33:       Boston_Bull  6
(Please insert question for cat_2 image here)
34: Yorkshire_Terrier  7
35:       Boston_Bull  4
36:  Golden_Retriever 12
37:        Bloodhound  6
38:  Golden_Retriever 11
39:        Bloodhound 11
40:            Saluki  9
41:       Boston_Bull  9
42:          Shih_Tzu 12
43:        Bloodhound  4
44:       Boston_Bull  7
45:   Irish_Wolfhound  7
46:          Shih_Tzu 10
47:   Irish_Wolfhound 10
48:        Bloodhound 13


# DESIGN 2
question number, breed, image id on google drive 
------------------------------------------------
  1:       Boston_Bull 12
2:          Shih_Tzu  9
3:          Shih_Tzu  5
4:    Cocker_Spaniel  7
5:   Irish_Wolfhound 12
6:  Golden_Retriever  7
7:    Cocker_Spaniel  8
8: Yorkshire_Terrier  6
9:            Saluki  8
10:          Shih_Tzu  4
11: Yorkshire_Terrier  8
12:        Bloodhound  5
13:          Shih_Tzu 11
14:   Irish_Wolfhound  5
15:            Saluki 10
16: Yorkshire_Terrier  9
# break
(Please insert question for cat_1 image here)
17:    Cocker_Spaniel 13
18:  Golden_Retriever  9
19:  Golden_Retriever  4
20:   Irish_Wolfhound  9
21: Yorkshire_Terrier 13
22:        Bloodhound 10
23:    Cocker_Spaniel  9
24:            Saluki  4
25:       Boston_Bull 11
26:            Saluki 11
27:    Cocker_Spaniel  6
28:   Irish_Wolfhound  6
29:  Golden_Retriever  8
30: Yorkshire_Terrier  4
31:            Saluki  5
32:    Cocker_Spaniel 12
# break
33:       Boston_Bull  6
34: Yorkshire_Terrier  7
35:       Boston_Bull  4
36:  Golden_Retriever 12
37:        Bloodhound  6
38:  Golden_Retriever 11
39:        Bloodhound 11
40:            Saluki  9
41:       Boston_Bull  9
42:          Shih_Tzu 12
43:        Bloodhound  4
44:       Boston_Bull  7
45:   Irish_Wolfhound  7
46:          Shih_Tzu 10
47:   Irish_Wolfhound 10
48:        Bloodhound 13