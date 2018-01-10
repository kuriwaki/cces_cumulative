Codebook
================
Shiro Kuriwaki
2018-01-10

<<<<<<< HEAD
Administration
==============

Year
----

=======
>>>>>>> 1b3b89ed015ee86d12da978157e14a3da72efef9
    year 
     year     n
     2006 36421
     2007 10000
     2008 32800
     2009 13800
     2010 55400
     2011 20150
     2012 54535
     2013 16400
     2014 56200
     2015 14250
     2016 64600

Approval
========

<<<<<<< HEAD
### President

    approval_pres 
                      approval_pres      n
                   Strongly Approve  74158
                   Somewhat Approve  92205
                Somewhat Disapprove  40113
                Strongly Disapprove 156820
                           Not Sure  10117
     Neither Approve Nor Disapprove    443
                            Skipped     72
                               <NA>    628

### Governor

=======
>>>>>>> 1b3b89ed015ee86d12da978157e14a3da72efef9
    approval_gov 
                       approval_gov      n
                   Strongly Approve  54544
                   Somewhat Approve 116578
                Somewhat Disapprove  71062
                Strongly Disapprove  97473
                           Not Sure  31575
     Neither Approve Nor Disapprove   1414
                            Skipped     83
                          Not Asked    230
                               <NA>   1597

<<<<<<< HEAD
### House Representative

=======
>>>>>>> 1b3b89ed015ee86d12da978157e14a3da72efef9
    approval_rep 
                       approval_rep     n
                   Strongly Approve 55069
                            Approve 44742
                   Somewhat Approve 71715
                         Disapprove 25041
                Somewhat Disapprove 40026
                Strongly Disapprove 58680
         Never Heard Of This Person 21315
                           Not Sure 49953
     Neither Approve Nor Disapprove  1798
                            Skipped    96
                          Not Asked   487
                               <NA>  5634

Demographics
============

    pid3 
            pid3      n
        Democrat  16876
      Republican  14503
     Independent  13630
           Other   1675
            <NA> 327872

    pid7 
                           pid7     n
                Strong Democrat 89117
       Not Very Strong Democrat 45242
                  Lean Democrat 37432
                    Independent 49221
                Lean Republican 40464
     Not Very Strong Republican 36225
              Strong Republican 63147
                       Not Sure 10783
                        Skipped    40
                      Not Asked    24
                           <NA>  2861

Voting
======

    intent_rep 
                               intent_rep      n
                 [Democrat / Candidate 1] 103873
               [Republican / Candidate 2]  97039
                    [Other / Candidate 3]   4071
       $HouseCand4Name ($HouseCand4Party)     18
                                    Other   1720
                             I'm Not Sure  60579
                                   No One  15860
       $HouseCand5Name ($HouseCand5Party)     20
            I Won't Vote In This Election   2269
       $HouseCand6Name ($HouseCand6Party)     19
       $HouseCand7Name ($HouseCand7Party)     15
       $HouseCand8Name ($HouseCand8Party)     14
       $HouseCand9Name ($HouseCand9Party)      1
     $HouseCand10Name ($HouseCand10Party)      1
     $HouseCand11Name ($HouseCand11Party)      3
                                  Skipped    217
                                Not Asked   1521
                                     <NA>  87316

Validated Vote
==============

    vv_regstatus 
                  vv_regstatus      n
                        Active 176387
     No Record Of Registration  64474
                  Unregistered  13397
                       Dropped   5168
                      Inactive   2983
          Multiple Appearances   1126
                          <NA> 111021
<<<<<<< HEAD

Text
====

Incumbents
----------

    Observations: 374,556
    Variables: 4
    $ hou_inc  <chr> "Patrick T. McHenry", "Michael R. Turner", "Robert E....
    $ sen1_inc <chr> "Elizabeth Dole", "Mike DeWine", "Robert Menendez", "...
    $ sen2_inc <chr> "Richard Burr", "George V. Voinovich", "Frank R. Laut...
    $ gov_inc  <chr> "Michael Easley", "Bob Taft", "Jon Corzine", "Rod Bla...

Incumbent Identifiers
---------------------

    Observations: 374,556
    Variables: 4
    $ hou_icpsr  <int> 20522, 20342, 29132, 29911, 29380, 20531, 29126, 29...
    $ sen1_icpsr <int> 40303, 15020, 29373, 15021, 14858, 49306, 40101, 15...
    $ sen2_icpsr <int> 29548, 49903, 14914, 40502, 40105, 40305, 40302, 29...
    $ gov_fec    <chr> "NC5998", NA, "NJ6395", "IL7", NA, "TX3156", "MN472...

Chosen
------

    Observations: 374,556
    Variables: 6
    $ intent_rep_chosen <chr> "Richard C. Carsner (D)", "Stephanie Studeba...
    $ intent_sen_chosen <chr> NA, "Sherrod C. Brown (D)", "Robert Menendez...
    $ intent_gov_chosen <chr> NA, "Ted Strickland (D)", NA, "Rod Blagojevi...
    $ voted_rep_chosen  <chr> "Richard C. Carsner (D)", "Stephanie Studeba...
    $ voted_sen_chosen  <chr> NA, "Sherrod C. Brown (D)", "Robert Menendez...
    $ voted_gov_chosen  <chr> NA, "Ted Strickland (D)", NA, "Rod Blagojevi...

Candidate Identifiers
---------------------

    Observations: 374,556
    Variables: 6
    $ intent_rep_fec <chr> "H6NC10141", "H6OH03142", "H0NJ01066", "H8IL090...
    $ intent_sen_fec <chr> NA, "S6OH00163", "S6NJ00289", NA, NA, NA, "S6MN...
    $ intent_gov_fec <chr> NA, "OH19691", NA, "IL7", "NY19490", NA, "MN472...
    $ voted_rep_fec  <chr> "H6NC10141", "H6OH03142", "H0NJ01066", "H8IL090...
    $ voted_sen_fec  <chr> NA, "S6OH00163", "S6NJ00289", NA, "S0NY00188", ...
    $ voted_gov_fec  <chr> NA, "OH19691", NA, "IL7", "NY19490", NA, "MN472...
=======
>>>>>>> 1b3b89ed015ee86d12da978157e14a3da72efef9
