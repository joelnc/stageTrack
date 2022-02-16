## App errors after resetting st <- 2019 to st <- 2022
## Error is both table an map panes
##### Error: arguments imply differing number of rows: 53, 51

## Ok, it is the discontiuned briar tribs
36 0214645022           Briar Creek @ Colony Road
37 0214645075             Tr Briar Creek @ Colony

## Need to drop them from siteCoor table, and then replace all the hard
## coded 53s in server.r with 51s, or nrow()

## So dropped at front of pullFormatHistorical.
## then ui.r loads another version, dropped again there
## server.r used a 53 site hardcode, swapped to 51



## via below hist daily has extra
> dim(siteCoor)
[1] 53  8
> dim(histDaily)
[1] 365  55
> dim(histFlowTab)
[1]  53 367
>

## but per error, what whould have 51??
## from server browser() , fdata() line 61
Browse[2]> r3$site_no
 [1] "0212393300" "02124080"   "0212414900" "02124269"   "0212427947"
 [6] "0212430293" "0212430653" "0212466000" "0212467451" "0212467595"
[11] "02142654"   "0214265808" "0214266000" "0214266080" "02142900"  
[16] "02142914"   "0214291555" "0214295600" "0214297160" "02146211"  
[21] "0214627970" "02146285"   "02146300"   "02146315"   "02146330"  
[26] "02146348"   "02146381"   "0214640410" "02146409"   "02146420"  
[31] "0214642825" "0214643770" "0214643820" "0214643860" "02146449"  
[36] "0214645022" "02146470"   "02146507"   "02146530"   "0214655255"
[41] "02146562"   "0214657975" "02146600"   "02146614"   "02146670"  
[46] "0214668150" "02146700"   "02146750"   "0214676115" "0214678175"
[51] "0214685800"

 [1]       
 [6] "0212430293" "0212430653" "0212466000" "0212467451" "0212467595"
[11] "02142654"   "0214265808" "0214266000" "0214266080" "02142900"  
[16] "02142914"   "0214291555" "0214295600" "0214297160" "02146211"  
[21] "0214627970" "02146285"   "02146300"   "02146315"   "02146330"  
[26] "02146348"   "02146381"   "0214640410" "02146409"   "02146420"  
[31] "0214642825" "0214643770" "0214643820" "0214643860" "02146449"  
[36] "0214645022" "02146470"   "02146507"   "02146530"   "0214655255"
[41] "02146562"   "0214657975" "02146600"   "02146614"   "02146670"  
[46] "0214668150" "02146700"   "02146750"   "0214676115" "0214678175"
[51] "0214685800"

## gets cbind with this at 53 sites
Browse[2]> str(SiteNamesAndFlooding)
'data.frame':	53 obs. of  2 variables:
 $ SiteName   : chr  "W Br Rocky River bl Mth" "ClarkeCreek nr Harrisburg" "Mallard Creek @ Harrisburg" "Back Creek @ SR1173" ...
 $ floodHeight: num  27 19 12 NA NA ...

## Note below, 53 vs 51, and 5 don't have flood height


                              SiteName floodHeight
1              W Br Rocky River bl Mth       26.98 "0212393300"
2            ClarkeCreek nr Harrisburg       19.03 "02124080"
3           Mallard Creek @ Harrisburg       12.00 "0212414900"
4                  Back Creek @ SR1173          NA "02124269"
5                 Reedy Creek @ SR2803          NA "0212427947"
6                 Reedy Creek bl I-485       12.97
7                 McKee Creek @ SR2804       14.01
8                 Clear Creek @ SR3181       15.01
9  Goose Creek at 1524 nr Indian Trail          NA
10                Goose Creek @ SR1525          NA
11      McDowell Creek nr Huntersville       30.03
12           Torrence Creek @ Bradford       15.27
13           McDowell Creek @ Beatties       16.03
14                  Gar Creek @ SR2074       21.99
15              Long Creek @ Paw Creek       13.51
16             Gum Branch nr Thrift NC       10.03
17               Long Creek @ Rhyne NC       14.95
18          Paw Creek @ Wilkinson Blvd       12.53
19             Beaverdam Creek Shopton        9.99
20       Irwin Creek @ Statesville Ave        8.96
21            Stewart Creek @ State St       17.96
22            Stewart Creek @ Morehead       13.97
23                  Irwin Creek @ WWTP       25.03
24           Taggart Creek @ West Blvd       26.03
25              Sugar Creek @ Arrowood       47.26
26                 Coffey Creek @ NC49       12.98
27        Sugar Creek @ NC51 Pineville       18.05
28                Little Sugar @ 36 St       10.98
29       Little Sugar @ Medical Center       13.48
30             Little Sugar @ Hillside       11.99
31                 Briar @ Shamrock Dr        5.50
32          Briar Creek @ Independence        9.98
33          Edwards Branch @ Sheffield       12.02
34            Briar Creek bl Edwards B       16.98
35            Briar Creek @ Providence       14.98
36           Briar Creek @ Colony Road       14.98
37             Tr Briar Creek @ Colony       19.58
38                 Trib to Briar Creek       10.98
39          Little Hope @ Seneca Place        9.48
40       Little Sugar @ Archdale Drive       12.01
41       Little Sugar @ NC51 Pineville       17.96
42           McAlpine Creek @ Idlewild       16.51
43           Campbell Creek @ Idlewild        9.58
44            Irvins @ Sam Newell Road       12.94
45                   McAlpine @ Sardis       21.94
46          McAlpine Creek @ Colony Rd       31.74
47           Fourmile Creek @ Elm Lane       16.51
48           McMullen Creek @ Lincrest        6.96
49        McMullen Creek @ Sharon View       17.99
50                     McAlpine @ WWTP       17.99
51             McAlpine Creek @ SR2964          NA
52       Steele Creek @ Carowinds Blvd        9.47
53          SixMile Creek nr Pineville       29.87
Browse[2]>

    ## from siteCoor
    Browse[2]> siteCoor[,c("site_no","SiteName")]
      site_no                            SiteName
1  0212393300             W Br Rocky River bl Mth
2    02124080           ClarkeCreek nr Harrisburg
3  0212414900          Mallard Creek @ Harrisburg
4    02124269                 Back Creek @ SR1173
5  0212427947                Reedy Creek @ SR2803
6  0212430293                Reedy Creek bl I-485
7  0212430653                McKee Creek @ SR2804
8  0212466000                Clear Creek @ SR3181
9  0212467451 Goose Creek at 1524 nr Indian Trail
10 0212467595                Goose Creek @ SR1525
11   02142654      McDowell Creek nr Huntersville
12 0214265808           Torrence Creek @ Bradford
13 0214266000           McDowell Creek @ Beatties
14 0214266080                  Gar Creek @ SR2074
15   02142900              Long Creek @ Paw Creek
16   02142914             Gum Branch nr Thrift NC
17 0214291555               Long Creek @ Rhyne NC
18 0214295600          Paw Creek @ Wilkinson Blvd
19 0214297160             Beaverdam Creek Shopton
20   02146211       Irwin Creek @ Statesville Ave
21 0214627970            Stewart Creek @ State St
22   02146285            Stewart Creek @ Morehead
23   02146300                  Irwin Creek @ WWTP
24   02146315           Taggart Creek @ West Blvd
25   02146330              Sugar Creek @ Arrowood
26   02146348                 Coffey Creek @ NC49
27   02146381        Sugar Creek @ NC51 Pineville
28 0214640410                Little Sugar @ 36 St
29   02146409       Little Sugar @ Medical Center
30   02146420             Little Sugar @ Hillside
31 0214642825                 Briar @ Shamrock Dr
32 0214643770          Briar Creek @ Independence
33 0214643820          Edwards Branch @ Sheffield
34 0214643860            Briar Creek bl Edwards B
35   02146449            Briar Creek @ Providence
36 0214645022           Briar Creek @ Colony Road
37 0214645075             Tr Briar Creek @ Colony
38 0214645080                 Trib to Briar Creek
39   02146470          Little Hope @ Seneca Place
40   02146507       Little Sugar @ Archdale Drive
41   02146530       Little Sugar @ NC51 Pineville
42 0214655255           McAlpine Creek @ Idlewild
43   02146562           Campbell Creek @ Idlewild
44 0214657975            Irvins @ Sam Newell Road
45   02146600                   McAlpine @ Sardis
46   02146614          McAlpine Creek @ Colony Rd
47   02146670           Fourmile Creek @ Elm Lane
48 0214668150           McMullen Creek @ Lincrest
49   02146700        McMullen Creek @ Sharon View
50   02146750                     McAlpine @ WWTP
51 0214676115             McAlpine Creek @ SR2964
52 0214678175       Steele Creek @ Carowinds Blvd
53 0214685800          SixMile Creek nr Pineville
