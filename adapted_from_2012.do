#delimit;
clear;
set more off;

cd "c:/users/stephen/research/cces";

use "2006/cces_2006_common.dta";

gen pid7_post = .;
replace pid7_post = 4 if v4036 == 4;
replace pid7_post = 3 if v4036 == 3;
replace pid7_post = 5 if v4036 == 5;
replace pid7_post = 8 if v4036 == 8;
replace pid7_post = 1 if v4035 == 1;
replace pid7_post = 2 if v4035 == 2;
replace pid7_post = 7 if v4035 == 7;
replace pid7_post = 6 if v4035 == 6;


drop 
v2001
v2002
v2003
v2007
v2008
v2009
v2010
v2011
v2012
v2013
v2014
v2015
v2016
v2017
v2028
v2035
v2036
v2037
v2043
v2044
v2045
v2046
v2047
v2048
v2049
v2050
v2051
v2052
v2053
v2054
v2055
v2056
v2057
v2058
v2059
v2060
v2061
v2062
v2063
v2064
v2065
v2066
v2067
v2068
v2069
v2071
v2072
v2073
v2074
v2075
v2076
v2077
v2078
v2079
v2080
v2081
v2083
v2085
v2087
v2088
v2089
v2090
v2091
v2093
v2094
v2095
v2096
v2097
v2098
v2099
v2100
v2101
v2104
v2105
v2108
v2109
v2110
v2111
v2112
v2113
v2114
v2115
v2116
v2117
v2118
v2119
v2120
v2121
v2122
v2123
v2124
v2125
v2127
v2128
v2130
v2131
v2132
v2133
v2134
v2135
v2136
v3001
v3002
v3009
v3020
v3021
v3023
v3025
v3026
v3028
v3036
v3037
v3038
v3039
v3040
v3041
v4002
v4003
v4005
v4016
v4019
v4022
v4023
v4024
v4025
v4026
v4027
v4028
v4029
v4030
v4031
v4032
v4033
v4034
v4035
v4036
v4037
v4038
v4039
v4041
v4043
v4045
v4047
v4048
v4049
v4050
v4051
v4052
v4053
v4054
v4055
v4056
v4057
v4058
v4059
v4060
v4061
v4062
v4064
v4067
v4068
v4070
v4071;

drop 
v5001
v5002
v5003
v5004
v5005
v5006
v5007
v5008
v5009
v5010
v5011
v5012
v5013
v5014
v5015
v5016
v5017
v5018
v5019
v5020
v5021
v5022
v5023
v5024
v5025
v5026
v5027
v5028
v5029
v5030
v5031
v5032
v5033
v5034
v5035
v5036
v5037
v5038
v5039
v5040
v5041
v5042
v5043
v5044
v5045
v5046;

rename v5047 zip_pre;
rename v3019 abortion;
rename v3027 affirm_action_06;
rename v3011 approval_gov;
rename v3003 approval_pres;
rename v3017 approval_rep;
rename v3013 approval_sen1;
rename v3015 approval_sen2;
rename v2020 birthyr;
rename v2070 born_again;
rename v4065 campaign_contact;
rename v1000 caseid;
rename v1006 census_region;
rename v2129 children_number;
rename v2026 church_attendence_06_07;
rename v1003 congdist_pre_06;
rename v1004 county_fips_pre_06;
drop v4063;
rename v3008 economy_retrospective;
rename v2018 educ;
rename v2030 employ;
rename v2031 employ_text;
rename v2032 family_income_06;
rename v4044 fiscal_pref;
rename v4046 fiscal_pref_least;
rename v2103 gay_marriage_amendment_06;
rename v2004 gender;
rename v2092 global_warming_06;
rename v2033 home_owner;
rename v2034 home_owner_text;
rename v3042 ideo100;
rename v3043 ideo100_dems;
rename v3044 ideo100_gop;
rename v3053 ideo100_gov;
rename v3051 ideo100_govcand1;
rename v3052 ideo100_govcand2;
rename v3045 ideo100_housecand1;
rename v3046 ideo100_housecand2;
rename v3049 ideo100_sen1;
rename v3050 ideo100_sen2;
rename v3047 ideo100_sencand1;
rename v3048 ideo100_sencand2;
rename v2021 ideo5;
rename v3086 immigration_status;
rename v2106 internet_home;
rename v2107 internet_work;
rename v3022 jobs_environment;
rename v3012 knowledge_gov_party_06_07;
rename v4066 knowledge_house_maj;
rename v3018 knowledge_rep_party_06_07;
rename v4069 knowledge_sen_maj;
rename v3061 knowledge_sen1_abortion;
rename v3079 knowledge_sen1_cafta;
rename v3076 knowledge_sen1_capgains;
rename v3070 knowledge_sen1_immig;
rename v3067 knowledge_sen1_iraqwithdraw;
rename v3073 knowledge_sen1_minwage;
rename v3014 knowledge_sen1_party_06_07;
rename v3064 knowledge_sen1_stemcell;
rename v3062 knowledge_sen2_abortion;
rename v3080 knowledge_sen2_cafta;
rename v3077 knowledge_sen2_capgains;
rename v3071 knowledge_sen2_immig;
rename v3068 knowledge_sen2_iraqwithdraw;
rename v3074 knowledge_sen2_minwage;
rename v3016 knowledge_sen2_party_06_07;
rename v3065 knowledge_sen2_stemcell;
rename v4009 line_length;
rename v4010 line_length_text;
rename v2019 marriage_status;
drop matched;
drop matchState;
drop G2006status;
rename vote_gen06 gen_validated;
rename v1005 media_market_area_pre_06;
rename v3033 mil_use_allies;
rename v3032 mil_use_democracy;
rename v3031 mil_use_genocide;
rename v3034 mil_use_intl_law;
rename v3035 mil_use_none;
rename v3029 mil_use_oil;
rename v3030 mil_use_terrorist;
rename v3084 military_family_served;
rename v3082 military_family;
rename v3085 military_none;
rename v3083 military_served;
rename v3081 military_serving;
rename v3010 mistake_iraq;
rename v3078 opinion_cafta;
rename v3075 opinion_extend_capgains;
rename v3069 opinion_immig_citizenship;
rename v3066 opinion_iraqwithdraw;
rename v3072 opinion_minwage;
rename v3060 opinion_partial_birth;
rename v3063 opinion_stemcell;
rename v2126 phone;
rename v3005 pid3;
rename v3006 pid3_other;
rename v3007 pid7;
rename v2042 politics_interest;
rename v2027 prayer_freq_06;
rename v2005 race;
rename v2006 race_text;
rename v3004 registered_pre;
rename v2029 relig_importance_06;
rename v2023 relig_protestant_06;
rename v2022 religion_06;
rename v2024 relig_christian_06;
rename v2025 relig_other_06;
rename v2041 residence_address_months_06;
rename v2040 residence_address_years_06;
rename v2039 residence_city_months_06;
rename v2038 residence_city_years_06;
rename v4042 sales_vs_incometaxes;
rename v4007 show_voter_id;
rename v3024 soc_sec_private;
drop starttime;
rename v1002 state_pre_06;
rename v2102 stem_cell_research;
rename v2086 stock_invest;
rename v4001 survey_complete;
rename v4040 tax_vs_spendingcuts;
rename v4004 turnout_06;
rename v2082 union;
rename v2084 union_household;
rename v4017 vote_attygen_06;
rename v4013 vote_gov;
rename v4015 vote_house;
rename vote06turn vote_intent_general;
rename v3058 vote_intent_gov;
rename v3059 vote_intent_gov_text;
rename v3054 vote_intent_house;
rename v3055 vote_intent_house_text;
rename v3056 vote_intent_senate;
rename v3057 vote_intent_senate_text;
rename v4006 vote_method;
drop v4011;
rename v4012 vote_problem_allowed;
rename v4018 vote_secstate;
rename v4014 vote_sen;
rename v4020 vote_staterep;
rename v4021 vote_statesen;
rename v4008 voter_id_allowed_vote;
rename v1001 weight;

tostring county_fips_pre, replace;

replace knowledge_house_maj = 4 if knowledge_house_maj == 3;

replace voter_id_allowed_vote = 3 if voter_id_allowed_vote == 1;
replace voter_id_allowed_vote = 1 if voter_id_allowed_vote == 2;

gen year = 2006;

save "2006/cces_06_for_cumulative.dta", replace;

clear;

















use "2007/cces_2007_common.dta";

replace CC14 = 1 if CC51a == 3;
replace CC14 = 3 if CC51a == 3;
replace CC14 = 2 if CC51a == 1;

drop govname
repname
sen1name
sen2name
CC06_V1000
CC06_V1002
CC06_V1003
CC06_V1004
CC06_V1005
CC06_V1006
CC06_V2001
CC06_V2002
CC06_V2003
CC06_V2004
CC06_V2005
CC06_V2006
CC06_V2007
CC06_V2008
CC06_V2009
CC06_V2010
CC06_V2011
CC06_V2012
CC06_V2013
CC06_V2014
CC06_V2015
CC06_V2016
CC06_V2017
CC06_V2018
CC06_V2019
CC06_V2020
CC06_V2021
CC06_V2022
CC06_V2023
CC06_V2024
CC06_V2025
CC06_V2026
CC06_V2027
CC06_V2028
CC06_V2029
CC06_V2030
CC06_V2031
CC06_V2032
CC06_V2033
CC06_V2034
CC06_V2035
CC06_V2036
CC06_V2037
CC06_V2038
CC06_V2039
CC06_V2040
CC06_V2041
CC06_V2042
CC06_V2043
CC06_V2044
CC06_V2045
CC06_V2046
CC06_V2047
CC06_V2048
CC06_V2049
CC06_V2050
CC06_V2051
CC06_V2052
CC06_V2053
CC06_V2054
CC06_V2055
CC06_V2056
CC06_V2057
CC06_V2058
CC06_V2059
CC06_V2060
CC06_V2061
CC06_V2062
CC06_V2063
CC06_V2064
CC06_V2065
CC06_V2066
CC06_V2067
CC06_V2068
CC06_V2069
CC06_V2070
CC06_V2071
CC06_V2072
CC06_V2073
CC06_V2074
CC06_V2075
CC06_V2076
CC06_V2077
CC06_V2078
CC06_V2079
CC06_V2080
CC06_V2081
CC06_V2082
CC06_V2083
CC06_V2084
CC06_V2085
CC06_V2086
CC06_V2087
CC06_V2088
CC06_V2089
CC06_V2090
CC06_V2091
CC06_V2092
CC06_V2093
CC06_V2094
CC06_V2095
CC06_V2096
CC06_V2097
CC06_V2098
CC06_V2099
CC06_V2100
CC06_V2101
CC06_V2102
CC06_V2103
CC06_V2104
CC06_V2105
CC06_V2106
CC06_V2107
CC06_V2108
CC06_V2109
CC06_V2110
CC06_V2111
CC06_V2112
CC06_V2113
CC06_V2114
CC06_V2115
CC06_V2116
CC06_V2117
CC06_V2118
CC06_V2119
CC06_V2120
CC06_V2121
CC06_V2122
CC06_V2123
CC06_V2124
CC06_V2125
CC06_V2126
CC06_V2127
CC06_V2128
CC06_V2129
CC06_V2130
CC06_V2131
CC06_V2132
CC06_V2133
CC06_V2134
CC06_V2135
CC06_V2136
CC06_V3001
CC06_V3002
CC06_V3003
CC06_V3004
CC06_V3005
CC06_V3006
CC06_V3007
CC06_V3008
CC06_V3009
CC06_V3010
CC06_V3011
CC06_V3012
CC06_V3013
CC06_V3014
CC06_V3015
CC06_V3016
CC06_V3017
CC06_V3018
CC06_V3019
CC06_V3020
CC06_V3021
CC06_V3022
CC06_V3023
CC06_V3024
CC06_V3025
CC06_V3026
CC06_V3027
CC06_V3028
CC06_V3029
CC06_V3030
CC06_V3031
CC06_V3032
CC06_V3033
CC06_V3034
CC06_V3035
CC06_V3036
CC06_V3037
CC06_V3038
CC06_V3039
CC06_V3040
CC06_V3041
CC06_V3042
CC06_V3043
CC06_V3044
CC06_V3045
CC06_V3046
CC06_V3047
CC06_V3048
CC06_V3049
CC06_V3050
CC06_V3051
CC06_V3052
CC06_V3053
CC06_V3054
CC06_V3055
CC06_V3056
CC06_V3057
CC06_V3058
CC06_V3059
CC06_V3060
CC06_V3061
CC06_V3062
CC06_V3063
CC06_V3064
CC06_V3065
CC06_V3066
CC06_V3067
CC06_V3068
CC06_V3069
CC06_V3070
CC06_V3071
CC06_V3072
CC06_V3073
CC06_V3074
CC06_V3075
CC06_V3076
CC06_V3077
CC06_V3078
CC06_V3079
CC06_V3080
CC06_V3081
CC06_V3082
CC06_V3083
CC06_V3084
CC06_V3085
CC06_V3086
CC06_V4001
CC06_V4002
CC06_V4003
CC06_V4004
CC06_V4005
CC06_V4006
CC06_V4007
CC06_V4008
CC06_V4009
CC06_V4010
CC06_V4011
CC06_V4012
CC06_V4013
CC06_V4014
CC06_V4015
CC06_V4016
CC06_V4017
CC06_V4018
CC06_V4019
CC06_V4020
CC06_V4021
CC06_V4022
CC06_V4023
CC06_V4024
CC06_V4025
CC06_V4026
CC06_V4027
CC06_V4028
CC06_V4029
CC06_V4030
CC06_V4031
CC06_V4032
CC06_V4033
CC06_V4034
CC06_V4035
CC06_V4036
CC06_V4037
CC06_V4038
CC06_V4039
CC06_V4040
CC06_V4041
CC06_V4042
CC06_V4043
CC06_V4044
CC06_V4045
CC06_V4046
CC06_V4047
CC06_V4048
CC06_V4049
CC06_V4050
CC06_V4051
CC06_V4052
CC06_V4053
CC06_V4054
CC06_V4055
CC06_V4056
CC06_V4057
CC06_V4058
CC06_V4059
CC06_V4060
CC06_V4061
CC06_V4062
CC06_V4063
CC06_V4064
CC06_V4065
CC06_V4066
CC06_V4067
CC06_V4068
CC06_V4069
CC06_V4070
CC06_V4071
CC06_V5001
CC06_V5002
CC06_V5003
CC06_V5004
CC06_V5005
CC06_V5006
CC06_V5007
CC06_V5008
CC06_V5009
CC06_V5010
CC06_V5011
CC06_V5012
CC06_V5013
CC06_V5014
CC06_V5015
CC06_V5016
CC06_V5017
CC06_V5018
CC06_V5019
CC06_V5020
CC06_V5021
CC06_V5022
CC06_V5023
CC06_V5024
CC06_V5025
CC06_V5026
CC06_V5027
CC06_V5028
CC06_V5029
CC06_V5030
CC06_V5031
CC06_V5032
CC06_V5033
CC06_V5034
CC06_V5035
CC06_V5036
CC06_V5037
CC06_V5038
CC06_V5039
CC06_V5040
CC06_V5041
CC06_V5042
CC06_V5043
CC06_V5044
CC06_V5045
CC06_V5046
CC10
cc11a_t
CC16
CC17a
CC17a_t
CC17b
CC17b_t
CC20a
CC20a_t
CC31
CC34lean
CC38lean
CC46lean
CC9;

rename CC11a abortion;
rename CC3 approval_cong;
rename CC23a approval_gov;
rename CC2 approval_pres;
rename CC21 approval_rep;
rename CC19 approval_sen1;
rename CC20 approval_sen2;
rename CC24a approval_stateleg;
rename churatd church_attendence_06_07;
rename cdid_num congdist_pre;
rename CC23 contact_house;
rename CC23sat contact_house_satisfied;
rename CC4 economy_retrospective;
rename endtime end_pre;
rename income family_income_06;
rename CC27 ideo100;
rename CC28 ideo100_dems;
rename CC29 ideo100_gop;
rename CC30 ideo100_gwbush;
rename CC32 ideo100_sen1;
rename CC33 ideo100_sen2;
rename CC12x_4 immig_border_patrol;
rename CC12x_5 immig_border_wall;
rename CC12x_1 immig_fine_businesses;
rename CC12x_2 immig_legal_status;
rename CC12x_6 immig_none;
rename CC22a_gov knowledge_gov_party_06_07;
rename CC49 knowledge_house_iraqwithdraw;
rename CC25_house knowledge_house_maj;
rename CC37 knowledge_house_schip;
rename CC41 knowledge_house_surveillance;
rename CC22a_rep knowledge_rep_party_06_07;
rename CC21a knowledge_rep_race;
rename CC21a_t knowledge_rep_race_text;
rename CC25_senate knowledge_sen_maj;
rename CC47 knowledge_sen1_iraqwithdraw;
rename CC22a_sen1 knowledge_sen1_party_06_07;
rename CC35 knowledge_sen1_schip;
rename CC39 knowledge_sen1_surveillance;
rename CC48 knowledge_sen2_iraqwithdraw;
rename CC22a_sen2 knowledge_sen2_party_06_07;
rename CC36 knowledge_sen2_schip;
rename CC40 knowledge_sen2_surveillance;
rename CC25_lower knowledge_state_house_maj;
rename CC25_upper knowledge_state_sen_maj;
rename marstat marriage_status;
rename CC6 mistake_iraq;
rename CC12x_3 immig_guest_worker;
rename newsint news_interest;
rename CC46 opinion_iraqwithdraw;
rename CC34 opinion_schip;
rename CC38 opinion_surveillance;
rename pid3_t pid3_other;
rename polinterest politics_interest;
rename CC24_t project_description;
rename CC24 projects_to_dist;
rename race_other race_text;
rename votereg registered_pre;
rename CCr12a relig_advent;
rename CCr12a_t relig_advent_text;
rename CCr1a relig_baptist;
rename CCr1a_t relig_baptist_text;
rename CCr7 relig_buddhist;
rename CCr7_t relig_buddhist_text;
rename CCr2 relig_catholic;
rename CCr2_t relig_catholic_text;
rename CCr8a relig_christian;
rename CCr8a_t relig_christian_text;
rename CCr9a relig_congreg;
rename CCr9a_t relig_congreg_text;
rename CCr7a relig_episcopal;
rename CCr7a_t relig_episcopal_text;
rename CCr8 relig_hindu;
rename CCr8_t relig_hindu_text;
rename CCr10a relig_holiness;
rename CCr10a_t relig_holiness_text;
rename CCr5 relig_jewish;
rename CCr5_t relig_jewish_text;
rename CCr4a relig_lutheran;
rename CCr4a_t relig_lutheran_text;
rename CCr2a relig_methodist;
rename CCr2a_t relig_methodist_text;
rename CCr3 relig_mormon;
rename CCr3_t relig_mormon_text;
rename CCr6 relig_muslim;
rename CCr6_t relig_muslim_text;
rename CCr3a relig_nondenom;
rename CCr3a_t relig_nondenom_text;
rename CCr4 relig_orthodox;
rename CCr4_t relig_orthodox_text;
rename CCr6a relig_pentecost;
rename CCr6a_t relig_pentecost_text;
rename CCr5a relig_presbyterian;
rename CCr5a_t relig_presbyterian_text;
rename CCr1 relig_protestant;
rename CCr1_t relig_protestant_text;
rename CCr11a relig_reformed;
rename CCr11a_t relig_reformed_text;
rename CCrinit religion;
rename CCrinit_t religion_text;
rename starttime start_pre;
rename inputstate state_pre;
rename CC14 turnout_06;
drop CC51a;
rename CC15 turnout_intent_primary;
rename inputzip zip_pre;

gen year = 2007;


label def approval 1 "strongly approve"
2 "somewhat approve"
3 "somewhat disapprove"
4 "strongly disapprove"
5 "not sure"
6 "neither approve nor disapprove";
replace approval_cong = 8 if approval_cong==3;
replace approval_cong = 3 if approval_cong==4;
replace approval_cong = 4 if approval_cong==5;
replace approval_cong = 5 if approval_cong==6;
replace approval_cong = 6 if approval_cong==8;
label values approval_cong approval;
replace approval_gov = 8 if approval_gov==3;
replace approval_gov = 3 if approval_gov==4;
replace approval_gov = 4 if approval_gov==5;
replace approval_gov = 5 if approval_gov==6;
replace approval_gov = 6 if approval_gov==8;
label values approval_gov approval;
replace approval_pres = 8 if approval_pres==3;
replace approval_pres = 3 if approval_pres==4;
replace approval_pres = 4 if approval_pres==5;
replace approval_pres = 5 if approval_pres==6;
replace approval_pres = 6 if approval_pres==8;
label values approval_pres approval;
replace approval_rep = 8 if approval_rep==3;
replace approval_rep = 3 if approval_rep==4;
replace approval_rep = 4 if approval_rep==5;
replace approval_rep = 5 if approval_rep==6;
replace approval_rep = 6 if approval_rep==8;
label values approval_rep approval;
replace approval_sen1 = 8 if approval_sen1==3;
replace approval_sen1 = 3 if approval_sen1==4;
replace approval_sen1 = 4 if approval_sen1==5;
replace approval_sen1 = 5 if approval_sen1==6;
replace approval_sen1 = 6 if approval_sen1==8;
label values approval_sen1 approval;
replace approval_sen2 = 8 if approval_sen2==3;
replace approval_sen2 = 3 if approval_sen2==4;
replace approval_sen2 = 4 if approval_sen2==5;
replace approval_sen2 = 5 if approval_sen2==6;
replace approval_sen2 = 6 if approval_sen2==8;
label values approval_sen2 approval;
replace approval_stateleg = 8 if approval_stateleg==3;
replace approval_stateleg = 3 if approval_stateleg==4;
replace approval_stateleg = 4 if approval_stateleg==5;
replace approval_stateleg = 5 if approval_stateleg==6;
replace approval_stateleg = 6 if approval_stateleg==8;
label values approval_stateleg approval;

replace contact_house_satisfied = contact_house_satisfied + 4;
label define cont 5 "yes" 6 "no";
label values contact_house_satisfied cont;

tostring congdist_pre, replace;

replace knowledge_rep_race = knowledge_rep_race - 1 if knowledge_rep_race == 6 | knowledge_rep_race == 7;

replace turnout_06 = turnout_06 + 1;
replace turnout_06 = 1 if turnout_06 == 4;
label define to 1 "yes" 2 "no" 3 "not sure";
label values turnout_06 to;

rename zip_pre zip_pre_06;

save "2007/cces_07_for_cumulative.dta", replace;

clear;










use "2008/cces_2008_common.dta";

drop
V501
V502
V503
V504
V505
V506
V507
V508
V509
V510
V5100
V5101
V5102
V5103
V5104
V5105
V511
V512
V513
V514
V515
V516
V518
V519
V520
V521
V522
V524
V525
V526
V527
V528
V529
V530
V531
V532
V533
V534
V535
V536
V537
V538
V540
V541
V542
V543
V544
V545
V546
V547
V548
V549
V550
V551
V552
V553
V554
V555
V556
V557
V558
V559
V560
V561
V562
V563
V564
V565
V569
V570
V571
V572
V573
V574
V575
V576
V577
V578
V579
V580
V581
V582
V583
V584
V585
V586
V587
V588
V589
V590
V591
V592
V593
V594
V595
V596
V597
V598
V599
V601
V602
V603
V604
V605
V606
V607
V608
V609
V610
V611
V612
V613
V614
V615
V616
V618
V619
V620
V621
V622
V624
V625
V626
V627
V628
V629
V630
V631
V632
V633
V634
V635
V636
V637
V638
V640
V641
V642
V643
V644
V645
V646
V647
V648
V649
V650
V651
V652
V653
V654
V655
V656
V657
V658
V659
V660
V661
V662
V663
V664
V665
V669
V670
V671
V672
V673
V674
V675
V676
V677
V678
V679
V680
V681
V682
V683
V684
V685
V686
V687
V688
V689
V690
V691
V692
V693
V694
V695
CC301
CC301_t
CC303a
CC303b
CC304a
CC304b
CC304c
CC311
CC316f
CC316g
CC317E_BO
CC317E_HC1
CC317E_HC2
CC317E_JM
CC317E_SC1
CC317E_SC1S2
CC317E_SC2
CC317E_SC2S2
CC317H_BO
CC317H_HC1
CC317H_HC2
CC317H_JM
CC317H_SC1
CC317H_SC1S2
CC317H_SC2
CC317H_SC2S2
CC317I_BO
CC317I_HC1
CC317I_HC2
CC317I_JM
CC317I_SC1
CC317I_SC1S2
CC317I_SC2
CC317I_SC2S2
CC317iS2
CC317jS2
CC317m
CC323_1
CC323_2
CC323_3
CC323_4
CC325a
CC325b
CC326
CC326b
CC326b_t
CC335S2
CC335S2_t
CC337S2_t
CC340
CC340_t
CC415_2
CC415_5
CC417
CC419_1
CC419_2
CC419_3
CC419_4
CC419_5
CC419_6
CC422
CC338
CC338_t
CC337S2;

rename CC310 abortion;
rename CC313 affirm_action;
rename CC327 vote_intent_pres_08;
rename V247 age_group;
rename CC335cong approval_cong;
rename CC335gov approval_gov;
rename CC335bush approval_pres;
rename CC335rep approval_rep;
rename CC335sup approval_scotus;
rename CC335sen1 approval_sen1;
rename CC335sen2 approval_sen2;
rename CC335leg approval_stateleg;
rename V207 birthyr;
rename V215 born_again;
rename V100 caseid;
rename V252 census_region;
drop V263;
rename V242 children;
rename V241 children_number;
rename V217 church_attendence;
rename V264 congdist_post;
rename V250 congdist_pre;
rename CC320 contact_house;
rename CC321 contact_house_satisfied;
rename V270 county_fips_post;
rename V269 county_fips_pre;
rename V268 county_name_post;
rename V267 county_name_pre;
rename CC416b donation_amount;
rename CC416a_4 donation_house_mine;
rename CC416a_5 donation_house_other;
rename CC416a_8 donation_pac;
rename CC416a_7 donation_party;
rename CC416a_9 donation_politicalgroup;
rename CC416a_1 donation_pres;
rename CC416a_2 donation_sen_mine;
rename CC416a_3 donation_sen_other;
rename CC416a_6 donation_stateoffice;
rename CC302 economy_retrospective;
rename V213 educ;
rename V209 employ;
rename V210 employ_text;
rename V402 end_post;
rename V301 end_pre;
rename V246 family_income_06;
rename CC309 fiscal_pref;
rename CC310a fiscal_pref_least;
rename vote_gen08 gen_validated;
rename vote_pri08 prim_validated;
rename V208 gender;
rename CC333 home_owner_08;
rename CC317a ideo100;
rename CC317b ideo100_dems;
rename CC317c ideo100_gop;
rename CC317d ideo100_gwbush;
rename CC317k ideo100_housecand1;
rename CC317l ideo100_housecand2;
rename CC317g ideo100_mccain;
rename CC317h ideo100_obama;
rename CC317e ideo100_sen1;
rename CC317f ideo100_sen2;
rename CC317i ideo100_sencand1;
rename CC317j ideo100_sencand2;
rename V243 ideo5;
rename CC332 immigration_status;
rename CC309a knowledge_governor_party_name;
rename CC308a knowledge_house_maj;
rename CC309d knowledge_rep_party_name;
rename CC319 knowledge_rep_race;
rename CC319_t knowledge_rep_race_text;
rename CC308b knowledge_sen_maj;
rename CC309b knowledge_sen1_party_name;
rename CC309c knowledge_sen2_party_name;
rename CC308d knowledge_state_house_maj;
rename CC308c knowledge_state_sen_maj;
rename CC406 line_length;
rename CC406_t line_length_text;
rename V214 marriage_status;
rename voter_status reg_validation;
rename CC301_1 media_24hrs_blog;
rename CC301_3 media_24hrs_newspaper;
rename CC301_f2 media_24hrs_newspaper_type;
rename CC301_5 media_24hrs_none;
rename CC301_4 media_24hrs_radio;
rename CC301_2 media_24hrs_tv_news;
rename CC301_f1 media_24hrs_tv_type;
rename V273 media_market_name_post;
rename V271 media_market_name_pre;
rename V274 media_market_area_post;
rename V272 media_market_area_pre;
rename CC418_5 mil_use_allies;
rename CC418_4 mil_use_democracy;
rename CC418_3 mil_use_genocide;
rename CC418_6 mil_use_intl_law;
rename CC418_7 mil_use_none;
rename CC418_1 mil_use_oil;
rename CC418_2 mil_use_terrorist;
rename CC328_4 military_family_served;
rename CC328_2 military_family;
rename CC328_5 military_none;
rename CC328_3 military_served;
rename CC328_1 military_serving;
rename CC304 mistake_iraq_08;
rename V244 news_interest;
rename CC404 no_vote_reason1;
rename CC404b no_vote_reason2;
rename CC316h opinion_cafta;
rename CC316a opinion_iraqwithdraw;
rename CC316b opinion_minwage;
rename CC316e opinion_schip;
rename CC316c opinion_stemcell;
rename CC316d opinion_surveillance;
rename CC316i opinion_tarp;
drop CC402;
drop CC402_t;
rename CC415_6 past_yr_donate_money;
rename CC415_1 past_yr_local_pol_meetings;
rename CC415_3 past_yr_polit_sign;
rename CC415_4 past_yr_work_for_campaign;
rename V255 phone;
rename CC307 pid3;
rename CC307_t pid3_other;
rename CC423 pid3_post;
rename CC423_t pid3_post_other;
rename CC307a pid7;
rename CC424 pid7_post;
rename V245 politics_interest;
rename V218 prayer_freq;
rename CC322a project_description;
rename CC322 projects_to_dist;
rename V211 race;
rename V212 race_text;
rename V257 reg_zip_post;
rename V205 reg_zip_same_pre;
rename V258 reg_zip_same_post;
rename V204 reg_zip_pre;
rename V256 registered_post;
rename V203 registered_pre;
rename V233 relig_advent;
rename V233_t relig_advent_text;
rename V222 relig_baptist;
rename V222_t relig_baptist_text;
rename V239 relig_buddhist;
rename V239_t relig_buddhist_text;
rename V234 relig_catholic;
rename V234_t relig_catholic_text;
rename V229 relig_christian;
rename V229_t relig_christian_text;
rename V230 relig_congreg;
rename V230_t relig_congreg_text;
rename V228 relig_episcopal;
rename V228_t relig_episcopal_text;
rename V240 relig_hindu;
rename V240_t relig_hindu_text;
rename V231 relig_holiness;
rename V231_t relig_holiness_text;
rename V216 relig_importance;
rename V237 relig_jewish;
rename V237_t relig_jewish_text;
rename V225 relig_lutheran;
rename V225_t relig_lutheran_text;
rename V223 relig_methodist;
rename V223_t relig_methodist_text;
rename V235 relig_mormon;
rename V235_t relig_mormon_text;
rename V238 relig_muslim;
rename V238_t relig_muslim_text;
rename V224 relig_nondenom;
rename V224_t relig_nondenom_text;
rename V236 relig_orthodox;
rename V236_t relig_orthodox_text;
rename V227 relig_pentecost;
rename V227_t relig_pentecost_text;
rename V226 relig_presbyterian;
rename V226_t relig_presbyterian_text;
rename V220 relig_protestant;
drop V221;
drop V220_t;
rename V232 relig_reformed;
rename V232_t relig_reformed_text;
rename V219 religion;
rename V219_t religion_text;
drop CC334;
rename CC421 sales_vs_incometaxes;
rename CC407 show_voter_id;
rename CC312 soc_sec_private;
rename V401 start_post;
rename V300 start_pre;
rename V265 state_fips_post;
rename V251 state_fips_pre;
rename V259 state_post;
rename V206 state_pre;
rename V400 survey_complete;
rename CC420 tax_vs_spendingcuts;
rename CC403 turnout_08;

gen turnout_primary_08 = .;
replace turnout_primary_08 = 1 if CC324_1 == 1;
replace turnout_primary_08 = 2 if CC324_2 == 1;
replace turnout_primary_08 = 3 if CC324_3 == 1;

label define primary 1 "yes primary" 2 "yes caucus" 3 "no";
label values turnout_primary_08 primary;

drop CC324_1 CC324_2 CC324_3;
rename CC329 union_08;
rename CC413 vote_gov;
rename CC412 vote_house;
rename CC412a vote_house_hypothetical1;
rename CC412b vote_house_hypothetical2;
rename CC336 vote_intent_gov;
rename CC336_t vote_intent_gov_text;
rename CC339 vote_intent_house;
rename CC339_t vote_intent_house_text;
rename CC327_t vote_intent_pres_08_text;
drop CC337;
rename CC335 vote_intent_senate;
rename CC335_t vote_intent_senate_text;
rename CC405 vote_method;
rename CC410 vote_pres_08;
rename CC410_t vote_pres_08_text;
rename CC408 vote_problem;
rename CC408a vote_problem_allowed;
rename CC411 vote_sen;
rename CC337_t vote_sen_text;
rename CC411b vote_sen2;
rename CC414_1 vote_staterep;
rename CC414_2 vote_statesen;
rename CC407a voter_id_allowed_vote;
rename V201 weight;
rename V253 zip_post;
rename V202 zip_pre;

tostring county_fips_post, replace;
tostring county_fips_pre, replace;

replace knowledge_rep_race = knowledge_rep_race + 1 if knowledge_rep_race ==4 | knowledge_rep_race == 5;

replace soc_sec_private = soc_sec_private + 1 if soc_sec_private == 3 | soc_sec_private == 4;
label def ssr 1 "strongly support"
2 "somewhat support"
3 "neither support nor oppose"
4 "somewhat oppose"
5 "strongly oppose";
label values soc_sec_private srr;

rename zip_pre zip_pre_06;
rename zip_post zip_post_06;

gen year = 2008;

save "2008/cces_08_for_cumulative.dta", replace;

clear;













use "2009/cces09_cmn_output-first.dta";

drop
v608
v618
v619
v627
v651
v652
v653
v654
v655
v656
cc09_01
cc09_04
cc09_07a
cc09_07b
cc09_07c
cc09_07d
cc09_07e
cc09_07f
cc09_07g
cc09_07h
cc09_07i
cc09_07j
cc09_21a1
cc09_21a2
cc09_21b1
cc09_21b2
cc09_21b3
cc09_24
cc09_50a
cc09_50b
cc09_50c
cc09_50d
cc09_50e
cc09_50f
cc09_52
cc09_54
cc09_56b
cc09_57
cc09_58a
cc09_58b
cc09_58c
cc09_58d
cc09_58e
cc09_59a
cc09_59b
cc09_59f
cc09_59g
v201
v291;

rename cc09_53 abortion;
rename cc09_55 affirm_action;
rename v288 age;
rename v247 age_group;
rename cc09_43d approval_cong;
rename cc09_43f approval_gov;
rename cc09_43e approval_pres;
rename cc09_43a approval_rep;
rename cc09_43b approval_sen1;
rename cc09_43c approval_sen2;
rename v207 birthyr;
rename v215 born_again;
rename v100 caseid;
rename v242 children;
rename v241 children_number;
rename v217 church_attendence;
rename v264 congdist_pre;
rename v269 county_fips_pre;
rename v267 county_name_post;
rename cc09_20 economy_retrospective_09;
rename v213 educ;
rename v209 employ;
rename v210 employ_text;
rename v402 end_pre;
rename v246 family_income_06;
rename v208 gender;
rename cc09_51 global_warming_06;
rename cc09_06 health_ins_needscleaned;
rename cc09_05 home_owner;
rename cc09_05t home_owner_text;
rename cc09_41a house_inc_competence;
rename cc09_41b house_inc_integrity;
rename v261 ideo5;
rename cc09_42a ideo7;
rename cc09_42d ideo7_dems;
rename cc09_42e ideo7_gop;
rename cc09_42b ideo7_gov;
rename cc09_42c ideo7_obama;
rename cc09_42f ideo7_rep;
rename cc09_42g ideo7_sen1;
rename cc09_42h ideo7_sen2;
rename v283 immigration_status;
rename cc09_40b knowledge_gov_party_06_07;
rename cc09_40e knowledge_house_maj;
rename cc09_40a knowledge_rep_party_06_07;
rename cc09_40f knowledge_sen_maj;
rename cc09_40c knowledge_sen1_party_06_07;
rename cc09_40d knowledge_sen2_party_06_07;
rename v214 marriage_status;
rename v272 media_market_area_pre;
rename v271 media_market_name_pre;
rename v279 military_family;
rename v281 military_family_served;
rename v282 military_none;
rename v280 military_served;
rename v278 military_serving;
rename cc09_23 mistake_afghanistan;
rename cc09_22 mistake_iraq;
rename v244 news_interest;
rename cc09_59h opinion_affordablecareact;
rename cc09_59e opinion_captrade;
rename cc09_59d opinion_schip;
rename cc09_59c opinion_stimulus;
rename cc09_02 phone;
rename cc423 pid3;
rename cc423_t pid3_other;
rename cc424 pid7;
rename v218 prayer_freq;
rename v211 race;
rename v212 race_text;
rename v257 reg_zip_pre;
rename v258 reg_zip_same_pre;
rename v256 registered_pre;
rename v233 relig_advent;
rename v233_t relig_advent_text;
rename v222 relig_baptist;
rename v222_t relig_baptist_text;
rename v239 relig_buddhist;
rename v239_t relig_buddhist_text;
rename v234 relig_catholic;
rename v234_t relig_catholic_text;
rename v229 relig_christian;
rename v229_t relig_christian_text;
rename v230 relig_congreg;
rename v230_t relig_congreg_text;
rename v228 relig_episcopal;
rename v228_t relig_episcopal_text;
rename v240 relig_hindu;
rename v240_t relig_hindu_text;
rename v231 relig_holiness;
rename v231_t relig_holiness_text;
rename v216 relig_importance;
rename v237 relig_jewish;
rename v237_t relig_jewish_text;
rename v225 relig_lutheran;
rename v225_t relig_lutheran_text;
rename v223 relig_methodist;
rename v223_t relig_methodist_text;
rename v235 relig_mormon;
rename v235_t relig_mormon_text;
rename v238 relig_muslim;
rename v238_t relig_muslim_text;
rename v224 relig_nondenom;
rename v224_t relig_nondenom_text;
rename v236 relig_orthodox;
rename v236_t relig_orthodox_text;
rename v227 relig_pentecost;
rename v227_t relig_pentecost_text;
rename v226 relig_presbyterian;
rename v226_t relig_presbyterian_text;
rename v220 relig_protestant;
rename v220_t relig_protestant_text;
rename v232 relig_reformed;
rename v232_t relig_reformed_text;
rename v219 religion;
rename v219_t religion_text;
drop cc09_03;
rename v295 residence_address_months;
rename v294 residence_address_years;
rename v293 residence_city_months;
rename v292 residence_city_years;
rename v401 start_pre;
rename v265 state_fips_post;
rename v259 state_pre;
rename v277 stock_invest;
rename cc09_56 tax_vs_spendingcuts;
rename cc09_30 turnout_08;
rename v284 union;
rename v273 union_household;
rename cc09_32 vote_house;
rename cc09_31 vote_pres_08;
rename cc09_33 vote_sen;
rename cc09_33x2 vote_sen2;
rename v200 weight;
rename v253 zip_pre;

tostring county_fips_pre, replace;

replace economy_retrospective = economy_retrospective - 5;
replace economy_retrospective = -economy_retrospective;

gen health_ins_govt = 1 if health_ins_needscleaned == 3;
replace health_ins_govt = 2 if health_ins_needscleaned != 3 & health_ins_needscleaned != .;
gen health_ins_none = 1 if health_ins_needscleaned == 1;
replace health_ins_none = 2 if health_ins_needscleaned != 1 & health_ins_needscleaned != .;
gen health_ins_notsure = 1 if health_ins_needscleaned == 5;
replace health_ins_notsure = 2 if health_ins_needscleaned != 5 & health_ins_needscleaned != .;
gen health_ins_self = 1 if health_ins_needscleaned == 4;
replace health_ins_self = 2 if health_ins_needscleaned != 4 & health_ins_needscleaned != .;
gen health_ins_work = 1 if health_ins_needscleaned == 2;
replace health_ins_work = 2 if health_ins_needscleaned != 2 & health_ins_needscleaned != .;
drop health_ins_needscleaned;

replace knowledge_gov_party_06_07 = 4 if knowledge_gov_party_06_07 == 3;
replace knowledge_rep_party_06_07 = 4 if knowledge_rep_party_06_07 == 3;
replace knowledge_sen1_party_06_07 = 4 if knowledge_sen1_party_06_07 == 3;
replace knowledge_sen2_party_06_07 = 4 if knowledge_sen2_party_06_07 == 3;

replace knowledge_house_maj = 4 if knowledge_house_maj == 3;
replace knowledge_sen_maj = 4 if knowledge_sen_maj == 3;

replace mistake_afghanistan = mistake_afghanistan - 1 if mistake_afghanistan == 1 | mistake_afghanistan == 2;
replace mistake_afghanistan = 2 if mistake_afghanistan == 0;
replace mistake_iraq = mistake_iraq - 1 if mistake_iraq == 1 | mistake_iraq == 2;
replace mistake_iraq = 2 if mistake_iraq == 0;

replace phone = 4 if phone == 1;
replace phone = 1 if phone == 3;
replace phone = 3 if phone == 4;

replace turnout_08 = turnout_08 + 1 if turnout_08 == 2 | turnout_08 == 3;

rename zip_pre zip_pre_06;

gen year = 2009;

save "2009/cces_09_for_cumulative.dta", replace;

clear;









use "2010/cces_2010_common_validated.dta";

gen pid7_post = .;
replace pid7_post = 4 if CC421b == 3;
replace pid7_post = 3 if CC421b == 1;
replace pid7_post = 5 if CC421b == 2;
replace pid7_post = 8 if CC421b == 8;
replace pid7_post = 1 if CC421_dem == 1;
replace pid7_post = 2 if CC421_dem == 2;
replace pid7_post = 7 if CC421_rep == 1;
replace pid7_post = 6 if CC421_rep == 2;

drop
V501
V501_post
V502
V502_post
V503
V503_post
V504
V504_post
V505
V505_post
V506
V506_post
V507
V507_post
V508
V508_post
V509
V509_post
V510
V510_post
V511
V511_post
V512
V512_post
V513
V513_post
V514
V514_post
V515
V515_post
V516
V516_post
V517
V517_post
V518
V518_post
V519
V519_post
V520
V520_post
V521
V521_post
V522
V522_post
V523
V523_post
V524
V524_post
V525
V525_post
V526
V526_post
V527
V527_post
V528
V528_post
V529
V529_post
V530
V530_post
V531
V531_post
V532
V532_post
V533
V533_post
V534
V534_post
V535
V535_post
V536
V536_post
V537
V537_post
V538
V538_post
V539
V539_post
V540
V540_post
V541
V541_post
V542
V542_post
V543
V543_post
V544
V544_post
V545
V545_post
V546
V546_post
V547
V548
V548_post
V549
V549_post
V550
V550_post
V551
V551_post
V552
V552_post
V553
V553_post
V554
V554_post
V555
V555_post
V556
V556_post
V557
V557_post
V558
V558_post
V559
V559_post
V560
V560_post
V561
V561_post
V562
V562_post
V563
V563_post
V564
V564_post
V565
V565_post
V566
V566_post
V567
V567_post
V568
V568_post
V569
V569_post
V570
V570_post
V571
V571_post
V572
V572_post
V573
V573_post
V574
V574_post
V575
V575_post
V576
V576_post
V577
V577_post
V578
V578_post
V579
V579_post
V580
V580_post
V581
V581_post
V582
V582_post
V583
V583_post
V584
V584_post
V585
V585_post
V586
V586_post
V587
V587_post
V588
V588_post
V589
V589_post
V590
V590_post
V591
V591_post
V592
V592_post
V593
V593_post
V594
V594_post
V595
V595_post
V596
V596_post
V597
V597_post
V598
V598_post
V599
V599_post
V600
V600_post
V601
V601_post
V602
V602_post
V603
V603_post
V604
V604_post
V605
V605_post
V606
V606_post
V607
V607_post
CC307
CC332E
CC332F
CC334Hb
CC334Ib
CC335a1
CC335a2
CC335b1
CC335b2
CC352a
CC352b
CC355a
CC355a_t
CC356a
CC356a_t
CC390a
CC390a_t
CC410a_nv
CC410a_nv_t
CC410b_nv
CC410b_nv_t
CC410b_t
CC411_nv
CC411_nv_t
CC412_nv
CC412_nv_t
CC417bx_9
CC417bx_t
CC421_dem
CC421_rep
CC421_t
CC421a
CC421b
V202
V202_post
V212a
V212c
V279
V280
V292_post
V294
V295;

rename CC324 abortion;
rename CC327 affirm_action;
rename CC308b approval_cong;
rename CC308d approval_gov;
rename CC308a approval_pres;
rename CC315a approval_rep;
rename CC308c approval_scotus;
rename CC315b approval_sen1;
rename CC315c approval_sen2;
rename CC308e approval_stateleg;
rename V207 birthyr;
rename V215 born_again;
rename CC425a campaign_contact;
rename CC425b_3 campaign_contact_email;
rename CC425b_1 campaign_contact_inperson;
rename CC425b_4 campaign_contact_mail;
rename CC425b_2 campaign_contact_phone;
rename V100 caseid;
rename V242 children;
rename V241 children_number;
rename V217 church_attendence;
rename V276_post congdist_post;
rename V276 congdist_pre;
rename V277_post county_fips_post;
rename V277 county_fips_pre;
rename V278_post county_name_post;
rename V278 county_name_pre;
rename CC417c donation_amount;
rename CC417bx_3 donation_house_mine;
rename CC417bx_4 donation_house_other;
rename CC417bx_7 donation_pac;
rename CC417bx_6 donation_party;
rename CC417bx_8 donation_politicalgroup;
rename CC417bx_1 donation_sen_mine;
rename CC417bx_2 donation_sen_other;
rename CC417bx_5 donation_stateoffice;
rename CC302 economy_retrospective;
rename V213 educ;
rename V209 employ;
rename V209_t employ_text;
rename V405 end_post;
rename V402 end_pre;
rename V246 family_income_06;
rename CC328 fiscal_pref;
rename CC329 fiscal_pref_least;
drop CC355b;
rename CC326 gay_marriage_amendment;
rename vote_gen10 gen_validated;
rename V208 gender;
rename CC321 global_warming;
rename CC320 gun_control;
rename V268 health_ins_govt;
rename V272 health_ins_none;
rename V271 health_ins_notsure;
rename V269 health_ins_school;
rename V270 health_ins_self;
rename V267 health_ins_work;
rename V290 hispanic;
rename V250 home_owner;
rename V251 home_owner_text;
rename CC335c1 house_inc_competence;
rename CC335c2 house_inc_integrity;
rename V243 ideo5;
rename CC334A ideo7;
rename CC334D ideo7_dems;
rename CC334E ideo7_gop;
rename CC334B ideo7_gov;
rename CC334J ideo7_housedemcand;
rename CC334K ideo7_houserepcand;
rename CC334C ideo7_obama;
rename CC334L ideo7_rep;
rename CC334F ideo7_sen1;
rename CC334G ideo7_sen2;
rename CC334H ideo7_sencand1;
rename CC334I ideo7_sencand2;
rename CC334M ideo7_teaparty;
rename CC322_4 immig_border_patrol;
rename CC322_1 immig_fine_businesses;
rename CC322_3 immig_guest_worker;
rename CC322_2 immig_legal_status;
rename CC322_6 immig_none;
rename CC322_5 immig_police_question;
rename V263 immigration_status;
rename V274 internet_home;
rename V275 internet_work;
rename CC325 jobs_environment;
rename CC310a knowledge_governor_party_name;
rename CC423a knowledge_house_demcand_race;
rename CC423a_other knowledge_house_demcand_race_t;
rename CC309a knowledge_house_maj;
rename CC423b knowledge_house_repcand_race;
rename CC423b_other knowledge_house_repcand_race_t;
rename CC310d knowledge_rep_party_name;
rename CC423c knowledge_rep_race;
rename CC423c_other knowledge_rep_race_text;
rename CC309b knowledge_sen_maj;
rename CC310b knowledge_sen1_party_name;
rename CC310c knowledge_sen2_party_name;
rename CC309d knowledge_state_house_maj;
rename CC309c knowledge_state_sen_maj;
rename V214 marriage_status;
rename CC301_1 media_24hrs_blog;
rename CC301_3 media_24hrs_newspaper;
rename CC301c media_24hrs_newspaper_type;
rename CC301_5 media_24hrs_none;
rename CC301_4 media_24hrs_radio;
rename CC301_2 media_24hrs_tv_news;
rename CC301b media_24hrs_tv_type;
rename CC414_5 mil_use_allies;
rename CC414_4 mil_use_democracy;
rename CC414_3 mil_use_genocide;
rename CC414_6 mil_use_intl_law;
rename CC414_7 mil_use_none;
rename CC414_1 mil_use_oil;
rename CC414_2 mil_use_terrorist;
rename V253 military_family;
rename V255 military_family_served;
rename V256 military_none;
rename V254 military_served;
rename V252 military_serving;
rename CC306 mistake_afghanistan;
rename CC305 mistake_iraq;
drop V210;
rename V244 news_interest;
rename CC402a no_vote_reason1;
rename CC402a_t no_vote_reason1_text;
rename CC402b no_vote_reason2;
rename CC402b_t no_vote_reason2_text;
rename CC332D opinion_affordablecareact;
rename CC332C opinion_captrade;
rename CC332G opinion_dadt_repeal;
rename CC332B opinion_schip;
rename CC332I stem_cell_research;
rename CC332A opinion_stimulus;
rename CC332H opinion_surveillance;
rename CC332J opinion_tarp;
rename CC424 opinion_tea_party;
drop CC350;
rename CC417a_5 past_yr_donate_blood;
rename CC417a_4 past_yr_donate_money;
rename CC417a_1 past_yr_local_pol_meetings;
rename CC417a_6 past_yr_none;
rename CC417a_2 past_yr_polit_sign;
rename CC417a_3 past_yr_work_for_campaign;
rename V273 phone;
rename V212b pid3_other;
rename V212d pid7;
rename V218 prayer_freq;
rename vote_pri10 prim_validated;
rename V211 race;
rename V211_t race_text;
rename CC422b racial_resent_slavery;
rename CC422a racial_resent_special_favors;
rename V205_post reg_zip_post;
rename V205 reg_zip_pre;
rename V204_post reg_zip_same_post;
rename V204 reg_zip_same_pre;
rename V203 registered_pre;
rename V203_post registered_post;
rename V233 relig_advent;
rename V233_t relig_advent_text;
rename V222 relig_baptist;
rename V222_t relig_baptist_text;
rename V239 relig_buddhist;
rename V239_t relig_buddhist_text;
rename V234 relig_catholic;
rename V234_t relig_catholic_text;
rename V229 relig_christian;
rename V229_t relig_christian_text;
rename V230 relig_congreg;
rename V230_t relig_congreg_text;
rename V228 relig_episcopal;
rename V228_t relig_episcopal_text;
rename V240 relig_hindu;
rename V240_t relig_hindu_text;
rename V231 relig_holiness;
rename V231_t relig_holiness_text;
rename V216 relig_importance;
rename V237 relig_jewish;
rename V237_t relig_jewish_text;
rename V225 relig_lutheran;
rename V225_t relig_lutheran_text;
rename V223 relig_methodist;
rename V223_t relig_methodist_text;
rename V235 relig_mormon;
rename V235_t relig_mormon_text;
rename V238 relig_muslim;
rename V238_t relig_muslim_text;
rename V224 relig_nondenom;
rename V224_t relig_nondenom_text;
rename V236 relig_orthodox;
rename V236_t relig_orthodox_text;
rename V227 relig_pentecost;
rename V227_t relig_pentecost_text;
rename V226 relig_presbyterian;
rename V226_t relig_presbyterian_text;
rename V220 relig_protestant;
rename V220_t relig_protestant_text;
rename V232 relig_reformed;
rename V232_t relig_reformed_text;
rename V219 religion;
rename V219_t religion_text;
drop CC351;
rename V262 residence_address_months;
rename V261 residence_address_years;
rename V259 residence_city_months;
rename V258 residence_city_years;
rename CC418bx_3 run_for_citycoun;
rename CC418bx_6 run_for_countywide;
rename CC418bx_5 run_for_distatty;
rename CC418bx_9 run_for_federal;
rename CC418bx_10 run_for_judge;
rename CC418bx_2 run_for_localboard;
rename CC418bx_4 run_for_mayor;
rename CC418a run_for_office;
rename CC418bx_11 run_for_other;
rename CC418b_t run_for_other_text;
rename CC418bx_1 run_for_schoolboard;
rename CC418bx_7 run_for_stateleg;
rename CC418bx_8 run_for_statewide;
rename CC416r sales_vs_incometaxes;
rename CC404 same_day_reg;
rename V401 start_pre;
rename V404 start_post;
rename V302_post state_fips_post;
rename V302 state_fips_pre;
rename V206_post state_post;
rename V206 state_pre;
rename V266 stock_invest;
rename V403 survey_complete;
rename CC415r tax_vs_spendingcuts;
rename CC316 turnout_08;
rename CC401 turnout_10;
rename V264 union;
rename V265 union_household;
rename CC413a vote_attygen;
rename CC411 vote_gov;
rename CC411_t vote_gov_text;
rename CC412 vote_house;
rename CC412a vote_house_hypothetical1;
rename CC412b vote_house_hypothetical2;
rename CC412_t vote_house_text;
rename CC354 vote_intent_general;
rename CC356 vote_intent_gov;
rename CC356_t vote_intent_gov_text;
rename CC390 vote_intent_house;
rename CC390_t vote_intent_house_text;
rename CC355 vote_intent_senate;
rename CC355_t vote_intent_senate_text;
drop CC355b_t;
rename CC403 vote_method;
rename CC317 vote_pres_08;
rename CC317_t vote_pres_08_text;
rename CC405a vote_problem;
rename CC405c vote_problem_allowed;
rename CC405b_1 vote_problem_id;
rename CC405b_2 vote_problem_registration;
rename CC405b_3 vote_problem_wrong_location;
rename CC413b vote_secstate;
rename CC410a vote_sen;
rename CC410a_t vote_sen_text;
rename CC410b vote_sen2;
rename CC413d vote_staterep;
rename CC413c vote_statesen;
rename V101 weight;
rename V201_post zip_post;
rename V201 zip_pre;
rename voter_status reg_validation;

gen year = 2010;

save "2010/cces_10_for_cumulative.dta", replace;

clear;






use "2011/CCES11_Common_OUTPUT.dta";

drop
V501
V513
V521
V529
V533
V536
V579
CC301_treat
CC301b
CC301c
CC301d
CC320c
CC336
CC337
CC340a
CC340a_force
CC340b_force
CC340c_force
CC340d_force
CC341_treat
CC341D
CC355b
CC361
CC363
CC370_1
CC370_10
CC370_2
CC370_3
CC370_4
CC370_5
CC370_6
CC370_7
CC370_8
CC370_9
CC434_treat
randrev_CC343
V201
V202
V204_nd
V212c
V247
V280
V281_t
V282;

rename CC352 abortion;
rename CC354 affirm_action;
rename V278 age;
rename CC320b approval_cong;
rename CC320f approval_gov;
rename CC320a approval_pres;
rename CC323a approval_rep;
rename CC323b approval_sen1;
rename CC323c approval_sen2;
rename CC320g approval_stateleg;
rename V207 birthyr;
rename V215 born_again;
rename V100 caseid;
rename V242 children;
rename V241 children_number;
rename V217 church_attendence;
rename V281 computer_type;
rename V276 congdist_pre;
rename V303 county_fips_pre;
rename CC310 economy_retrospective;
rename V213 educ;
rename V209 employ;
rename CC300 employ_past_five_yrs;
rename V209_t employ_text;
rename CC301a employer_category;
rename V402 end_pre;
rename V246 family_income;
rename CC355a fiscal_pref;
rename CC353 gay_marriage_amendment;
rename V208 gender;
rename CC350 global_warming;
rename V268 health_ins_govt;
rename V272 health_ins_none;
rename V271 health_ins_notsure;
rename V269 health_ins_school;
rename V270 health_ins_self;
rename V267 health_ins_work;
rename V290 hispanic;
rename CC306 home_owner;
rename V243 ideo5;
rename CC342A ideo7;
rename CC342D ideo7_dems;
rename CC342E ideo7_gop;
rename CC342B ideo7_gov;
rename CC342C ideo7_obama;
rename CC342H ideo7_rep;
rename CC342I ideo7_scotus;
rename CC331F ideo7_sen1;
rename CC342G ideo7_sen2;
rename CC342M ideo7_teaparty;
rename CC351_2 immig_border_patrol;
rename CC351_1 immig_legal_status;
rename CC351_4 immig_none;
rename CC351_3 immig_police_question;
rename V263 immigration_status;
rename CC302 industry_class;
rename V274 internet_home;
rename V275 internet_work;
drop CC311;
rename CC322a knowledge_governor_party_name;
rename CC340b knowledge_house_debtceiling;
rename CC321a knowledge_house_maj;
rename CC322d knowledge_rep_party_name;
rename CC334 knowledge_rep_race;
rename CC321b knowledge_sen_maj;
rename CC340c knowledge_sen1_debtceiling;
rename CC322b knowledge_sen1_party_name;
rename CC340d knowledge_sen2_debtceiling;
rename CC322c knowledge_sen2_party_name;
rename CC321d knowledge_state_house_maj;
rename CC321c knowledge_state_sen_maj;
rename V214 marriage_status;
rename CC309_1 media_24hrs_blog;
rename CC309_3 media_24hrs_newspaper;
rename CC309c media_24hrs_newspaper_type;
rename CC309_5 media_24hrs_none;
rename CC309_4 media_24hrs_radio;
rename CC309_2 media_24hrs_tv_news;
rename CC309b media_24hrs_tv_type;
rename CC356_5 mil_use_allies;
rename CC356_4 mil_use_democracy;
rename CC356_3 mil_use_genocide;
rename CC356_6 mil_use_intl_law;
rename CC356_7 mil_use_none;
rename CC356_1 mil_use_oil;
rename CC356_2 mil_use_terrorist;
rename V253 military_family;
rename V255 military_family_served;
rename V256 military_none;
rename V254 military_served;
rename V252 military_serving;
rename CC314 mistake_afghanistan;
rename CC313 mistake_iraq;
drop V210;
rename V244 news_interest;
rename CC341C opinion_captrade;
rename CC341E opinion_dadt_repeal;
rename CC341B opinion_schip;
rename CC341G opinion_stemcell;
rename CC341A opinion_stimulus;
rename CC341F opinion_surveillance;
rename CC341H opinion_tarp;
rename CC343 opinion_tea_party_11;
drop CC308;
drop CC308_t;
rename V273 phone;
rename V212a pid3;
rename V245 pid3_other;
rename V212d pid7;
rename V218 prayer_freq;
rename V211 race;
rename V211_t race_text;
rename CC359 racial_resent_special_favors;
rename V205 reg_zip_pre;
rename V204 reg_zip_same_pre;
rename V203 registered_pre;
rename V233 relig_advent;
rename V233_t relig_advent_text;
rename V222 relig_baptist;
rename V222_t relig_baptist_text;
rename V239 relig_buddhist;
rename V239_t relig_buddhist_text;
rename V234 relig_catholic;
rename V234_t relig_catholic_text;
rename V229 relig_christian;
rename V229_t relig_christian_text;
rename V230 relig_congreg;
rename V230_t relig_congreg_text;
rename V228 relig_episcopal;
rename V228_t relig_episcopal_text;
rename V240 relig_hindu;
rename V240_t relig_hindu_text;
rename V231 relig_holiness;
rename V231_t relig_holiness_text;
rename V216 relig_importance;
rename V237 relig_jewish;
rename V237_t relig_jewish_text;
rename V225 relig_lutheran;
rename V225_t relig_lutheran_text;
rename V223 relig_methodist;
rename V223_t relig_methodist_text;
rename V235 relig_mormon;
rename V235_t relig_mormon_text;
rename V238 relig_muslim;
rename V238_t relig_muslim_text;
rename V224 relig_nondenom;
rename V224_t relig_nondenom_text;
rename V236 relig_orthodox;
rename V236_t relig_orthodox_text;
rename V227 relig_pentecost;
rename V227_t relig_pentecost_text;
rename V226 relig_presbyterian;
rename V226_t relig_presbyterian_text;
rename V220 relig_protestant;
rename V220_t relig_protestant_text;
rename V232 relig_reformed;
rename V232_t relig_reformed_text;
rename V219 religion;
rename V219_t religion_text;
drop CC307;
rename V262 residence_address_months;
rename V261 residence_address_years;
rename V259 residence_city_months;
rename V258 residence_city_years;
rename CC335 run_for_office;
rename CC335b run_for_office_needscleaned;
rename CC335b_t run_for_other_text;
rename CC358 sales_vs_incometaxes;
rename V401 start_pre;
rename V302 state_fips_pre;
rename V206 state_pre;
rename V266 stock_invest;
rename CC357 tax_vs_spendingcuts;
rename CC330 turnout_08;
rename CC332 turnout_10;
rename V264 union;
rename V265 union_household;
rename CC333 vote_house;
rename CC333_t vote_house_text;
rename CC331 vote_pres_08;
rename CC331_t vote_pres_08_text;
rename V101 weight;
rename V279 zip_pre;

gen year = 2011;

tostring county_fips_pre, replace;

replace employer_category = employer_category-1 if employer_category == 1 | employer_category == 2;
replace employer_category = 2 if employer_category == 0;
replace employer_category = 3 if employer_category == 4 | employer_category == 5;
replace employer_category = 5 if employer_category == 6;

gen run_for_schoolboard = 1 if run_for_office_n == 1;
gen run_for_localboard = 1 if run_for_office_n == 2;
gen run_for_citycoun = 1 if run_for_office_n == 3;
gen run_for_mayor = 1 if run_for_office_n == 4;
gen run_for_distatty = 1 if run_for_office_n == 5;
gen run_for_countywide = 1 if run_for_office_n == 6;
gen run_for_stateleg = 1 if run_for_office_n == 7;
gen run_for_statewide = 1 if run_for_office_n == 8;
gen run_for_federal = 1 if run_for_office_n == 9;
gen run_for_judge = 1 if run_for_office_n == 10;
gen run_for_other = 1 if run_for_office_n == 11;

replace run_for_schoolboard = 0 if run_for_office_n != 1 & run_for_office_n != .;
replace run_for_localboard = 0 if run_for_office_n != 2 & run_for_office_n != .;
replace run_for_citycoun = 0 if run_for_office_n != 3 & run_for_office_n != .;
replace run_for_mayor = 0 if run_for_office_n != 4 & run_for_office_n != .;
replace run_for_distatty = 0 if run_for_office_n != 5 & run_for_office_n != .;
replace run_for_countywide = 0 if run_for_office_n != 6 & run_for_office_n != .;
replace run_for_stateleg = 0 if run_for_office_n != 7 & run_for_office_n != .;
replace run_for_statewide = 0 if run_for_office_n != 8 & run_for_office_n != .;
replace run_for_federal = 0 if run_for_office_n != 9 & run_for_office_n != .;
replace run_for_judge = 0 if run_for_office_n != 10 & run_for_office_n != .;
replace run_for_other = 0 if run_for_office_n != 11 & run_for_office_n != .;

replace turnout_10 = 3 if turnout_10 == 2;
replace turnout_10 = 6 if turnout_10 == 3;
replace turnout_10 = 5 if turnout_10 == 4;
label define to10 1 "didn't vote"
2 "thought about voting but didn't" 
3 "usually vote but didn't"
4 "attempted but couldn't"
5 "definitely voted"
6 "not sure";
label values turnout_10 to10;


save "2011/cces_11_for_cumulative.dta", replace;





#delimit;
clear;
use "2012/CCES12_Common_v10_20130311.dta";

drop
currentgovname
currentgovname_post
currentgovparty
currentgovparty_post
currenthousefreshman
currenthousefreshman_post
currenthousegender
currenthousegender_post
currenthousename
currenthousename_post
currenthouseparty
currenthouseparty_post
currenthouseretiring_post
currentsen1name
currentsen1name_post
currentsen1party
currentsen1party_post
currentsen2name
currentsen2name_post
currentsen2party
currentsen2party_post
govcand1incumbent
govcand1incumbent_post
govcand1name
govcand1name_post
govcand1party
govcand1party_post
govcand2incumbent
govcand2incumbent_post
govcand2name
govcand2name_post
govcand2party
govcand2party_post
housecand1gender
housecand1gender_post
housecand1incumbent
housecand1incumbent_post
housecand1incumbentcdid112
housecand1incumbentcdid112_post
housecand1name
housecand1name_post
housecand1party
housecand1party_post
housecand2gender
housecand2gender_post
housecand2incumbent
housecand2incumbent_post
housecand2incumbentcdid112
housecand2incumbentcdid112_post
housecand2name
housecand2name_post
housecand2party
housecand2party_post
housecand3gender
housecand3gender_post
housecand3incumbent
housecand3incumbent_post
housecand3incumbentcdid112
housecand3incumbentcdid112_post
housecand3name
housecand3name_post
housecand3party
housecand3party_post
judgecand1name_seat1_post
judgecand1name_seat2_post
judgecand1name_seat3_post
judgecand1nameselected_post
judgecand1party_seat1_post
judgecand1party_seat2_post
judgecand1partyselected_post
judgecand2name_seat1_post
judgecand2name_seat2_post
judgecand2name_seat3_post
judgecand2nameselected_post
judgecand2party_seat1_post
judgecand2party_seat2_post
judgecand2partyselected_post
judgecand3name_seat1_post
judgecand3name_seat2_post
judgecand3name_seat3_post
judgecand3nameselected_post
judgecand3party_seat1_post
judgecand3party_seat2_post
judgecand3partyselected_post
judgecand4name_seat1_post
judgecand4nameselected_post
judgecand4party_seat1_post
judgecand4partyselected_post
judgenameretent1_post
judgenameretent2_post
judgenameretent3_post
judgenameretent4_post
judgenameretent5_post
judgenameretent6_post
judgenameretent7_post
judgenameretentselected_post
legname
legname_post
lowerchambername
lowerchambername_post
pickjudgecontest
sencand1gender
sencand1gender_post
sencand1incumbent
sencand1incumbent_post
sencand1name
sencand1name_post
sencand1party
sencand1party_post
sencand2gender
sencand2gender_post
sencand2incumbent
sencand2incumbent_post
sencand2name
sencand2name_post
sencand2party
sencand2party_post
sencand3gender
sencand3gender_post
sencand3name
sencand3name_post
sencand3party
sencand3party_post
statesupremecourtname_post
askvote
cc302b
cc303_1
cc303_2
cc303_3
cc303_4
cc303_5
cc303_6
cc303_7
cc304
cc325_1
cc325_2
cc325_3
cc325_4
cc326
cc326_1
cc326_2
cc326_3
cc326_4
cc326_5
cc326_6
cc326_7
cc332a
cc332b
cc332c
cc332d
cc332e
cc332f
cc332g
cc332h
cc334d
cc334grid
cc334j
cc334k
cc354b
cc354b_t
cc355
cc355_t
cc356
cc356_t
cc390
cc390_t
cc403b
cc410a_nv
cc410a_nv_t
cc410b_nv
cc410b_nv_t
cc411_nv
cc411_nv_t
cc412_nv
cc412_nv_t
cc417bx_10
cc417bx_t
cc421_dem
cc421_rep
cc421_t
cc421a
cc421b
ccj413_mi1_1
ccj413_mi1_2
ccj413_mi1_3
ccj413_mi1_4
ccj413_mi1_5
ccj413_mi1_6
ccj413_mi1_7
ccj413_mi1_97
ccj413_mi1_98
ccj413_mi1_nv_1
ccj413_mi1_nv_2
ccj413_mi1_nv_3
ccj413_mi1_nv_4
ccj413_mi1_nv_5
ccj413_mi1_nv_6
ccj413_mi1_nv_7
ccj413_mi1_nv_98
ccj413_mi2
ccj413_mi2_nv
ccj413_wv_1
ccj413_wv_2
ccj413_wv_3
ccj413_wv_4
ccj413_wv_97
ccj413_wv_98
ccj413_wv_nv_1
ccj413_wv_nv_2
ccj413_wv_nv_3
ccj413_wv_nv_4
ccj413_wv_nv_98
ccj413a
ccj413a_nv
ccj413a_nv_t
ccj413a_t
ccj413d
ccj413d_nv
employertext
employtext
genhealth
healthcost
numemployees
pid7others
stateabbr;

rename cc327 affirm_action;
rename cc324 abortion;
rename cc308b approval_cong;
rename cc308d approval_gov;
rename cc308a approval_pres;
rename cc315a approval_rep;
rename cc308c approval_scotus;
rename cc315b approval_sen1;
rename cc315c approval_sen2;
rename cc308e approval_stateleg;
drop govtleveltext;
rename pew_bornagain born_again;
rename cc425a campaign_contact;
rename cc425b_3 campaign_contact_email;
rename cc425b_1 campaign_contact_inperson;
rename cc425b_4 campaign_contact_mail;
rename cc425b_2 campaign_contact_phone;
rename v101 caseid;
rename child18 children;
rename child18numx children_number;
rename pew_churatd church_attendence;
rename comptype computer_type;
rename cdid_post congdist_post;
rename cdid congdist_pre;
rename cdid113_post congdist_redist_post;
rename cdid113 congdist_redist_pre;
rename countyfips_post county_fips_post;
rename countyfips county_fips_pre;
rename countyname_post county_name_post;
rename countyname county_name_pre;
rename cc417c donation_amount;
rename cc417bx_4 donation_house_mine;
rename cc417bx_5 donation_house_other;
rename cc417bx_8 donation_pac;
rename cc417bx_7 donation_party;
rename cc417bx_9 donation_politicalgroup;
rename cc417bx_1 donation_pres;
rename cc417bx_2 donation_sen_mine;
rename cc417bx_3 donation_sen_other;
rename cc417bx_6 donation_stateoffice;
rename cc302 economy_retrospective;
rename cc302a economy_prospective;
rename hadjob employ_past_five_yrs;
rename employ_t employ_text;
rename employercat employer_category;
drop govtlevel;
rename endtime_post end_post;
rename endtime end_pre;
rename faminc family_income;
rename cc328 fiscal_pref;
rename cc329 fiscal_pref_least;
rename cc321 global_warming;
rename cc320 gun_control;
rename healthins_2 health_ins_govt;
rename healthins_6 health_ins_none;
rename healthins_5 health_ins_notsure;
rename healthins_3 health_ins_school;
rename healthins_4 health_ins_self;
rename healthins_1 health_ins_work;
rename ownhome home_owner;
rename ownhome_t home_owner_text;
rename cc334a ideo7;
rename cc334e ideo7_dems;
rename cc334f ideo7_gop;
rename cc334b ideo7_gov;
rename cc334l ideo7_housedemcand;
rename cc334m ideo7_houserepcand;
rename cc334c ideo7_obama;
rename cc334n ideo7_rep;
rename cc334p ideo7_scotus;
rename cc334h ideo7_sen1;
rename cc334i ideo7_sen2;
rename cc334g ideo7_teaparty;
rename cc322_6 immig_auto_citizenship;
rename cc322_2 immig_border_patrol;
rename cc322_4 immig_fine_businesses;
rename cc322_5 immig_hosp_school;
rename cc322_1 immig_legal_status;
rename cc322_3 immig_police_question;
rename immstat immigration_status;
rename industryclass industry_class;
rename internethome internet_home;
rename internetwork internet_work;
rename cc325 jobs_environment;
rename cc310a knowledge_governor_party_name;
rename cc423a knowledge_house_demcand_race;
rename cc423a_other knowledge_house_demcand_race_t;
rename cc309a knowledge_house_maj;
rename cc423b knowledge_house_repcand_race;
rename cc423b_other knowledge_house_repcand_race_t;
rename cc310d knowledge_rep_party_name;
rename cc423c knowledge_rep_race;
rename cc423c_other knowledge_rep_race_text;
rename cc309b knowledge_sen_maj;
rename cc310b knowledge_sen1_party_name;
rename cc310c knowledge_sen2_party_name;
rename cc309d knowledge_state_house_maj;
rename cc309c knowledge_state_sen_maj;
rename cc404 line_length;
rename cc404_t line_length_text;
rename marstat marriage_status;
rename cc414_5 mil_use_allies;
rename cc414_4 mil_use_democracy;
rename cc414_3 mil_use_genocide;
rename cc414_6 mil_use_intl_law;
rename cc414_7 mil_use_none;
rename cc414_1 mil_use_oil;
rename cc414_2 mil_use_terrorist;
rename milstat_4 military_family_served;
rename milstat_2 military_family;
rename milstat_5 military_none;
rename milstat_3 military_served;
rename milstat_1 military_serving;
rename cc306 mistake_afghanistan;
rename cc305 mistake_iraq;
drop ccesmodule;
rename newsint news_interest;
rename cc402a no_vote_reason1;
rename cc402a_t no_vote_reason1_text;
rename cc402b no_vote_reason2;
rename cc402b_t no_vote_reason2_text;
rename occupationcat occupation_category;
rename cc332i opinion_affordablecareact;
rename cc332j opinion_dadt_repeal;
rename cc424 opinion_tea_party;
drop cc350;
rename cc417a_5 past_yr_donate_blood;
rename cc417a_4 past_yr_donate_money;
rename cc417a_1 past_yr_local_pol_meetings;
rename cc417a_6 past_yr_none;
rename cc417a_2 past_yr_polit_sign;
rename cc417a_3 past_yr_work_for_campaign;
rename pid3_t pid3_other;
rename pew_prayer prayer_freq;
rename cc422b racial_resent_slavery;
rename cc422a racial_resent_special_favors;
rename regzip reg_zip_pre;
rename votereg_f reg_zip_same_pre;
rename votereg_post registered_post;
rename votereg registered_pre;
rename religpew_buddhist relig_buddhist;
rename religpew_advent relig_advent;
rename religpew_advent_t relig_advent_text;
rename religpew_baptist relig_baptist;
rename religpew_baptist_t relig_baptist_text;
rename religpew_buddhist_t relig_buddhist_text;
rename religpew_catholic relig_catholic;
rename religpew_catholic_t relig_catholic_text;
rename religpew_christian relig_christian;
rename religpew_christian_t relig_christian_text;
rename religpew_congreg relig_congreg;
rename religpew_congreg_t relig_congreg_text;
rename religpew_episcop relig_episcopal;
rename religpew_episcop_t relig_episcopal_text;
rename religpew_hindu relig_hindu;
rename religpew_hindu_t relig_hindu_text;
rename religpew_holiness relig_holiness;
rename religpew_holiness_t relig_holiness_text;
rename pew_religimp relig_importance;
rename religpew_jewish relig_jewish;
rename religpew_jewish_t relig_jewish_text;
rename religpew_lutheran relig_lutheran;
rename religpew_lutheran_t relig_lutheran_text;
rename religpew_methodist relig_methodist;
rename religpew_methodist_t relig_methodist_text;
rename religpew_mormon relig_mormon;
rename religpew_mormon_t relig_mormon_text;
rename religpew_muslim relig_muslim;
rename religpew_muslim_t relig_muslim_text;
rename religpew_nondenom relig_nondenom;
rename religpew_nondenom_t relig_nondenom_text;
rename religpew_orthodox relig_orthodox;
rename religpew_orthodox_t relig_orthodox_text;
rename religpew_pentecost relig_pentecost;
rename religpew_pentecost_t relig_pentecost_text;
rename religpew_presby relig_presbyterian;
rename religpew_presby_t relig_presbyterian_text;
rename religpew_protestant relig_protestant;
rename religpew_protestant_t relig_protestant_text;
rename religpew_reformed relig_reformed;
rename religpew_reformed_t relig_reformed_text;
rename religpew religion;
rename religpew_t religion_text;
drop cc351;
rename cc418bx_3 run_for_citycoun;
rename cc418bx_6 run_for_countywide;
rename cc418bx_5 run_for_distatty;
rename cc418bx_9 run_for_federal;
rename cc418bx_10 run_for_judge;
rename cc418bx_2 run_for_localboard;
rename cc418bx_4 run_for_mayor;
rename cc418a run_for_office;
rename cc418bx_11 run_for_other;
rename cc418b_t run_for_other_text;
rename cc418bx_1 run_for_schoolboard;
rename cc418bx_7 run_for_stateleg;
rename cc418bx_8 run_for_statewide;
rename cc416r sales_vs_incometaxes;
rename cc405 same_day_reg;
rename starttime_post start_post;
rename starttime start_pre;
rename inputstate_post state_post;
rename inputstate state_pre;
rename investor stock_invest;
rename cc415r tax_vs_spendingcuts;
rename cc316 turnout_08;
rename cc401 turnout_12;
rename unionhh union_household;
rename cc413a vote_attygen;
rename cc411 vote_gov;
rename cc411_t vote_gov_text;
rename cc412 vote_house;
rename cc412a vote_house_hypothetical1;
rename cc412b vote_house_hypothetical2;
rename cc412_t vote_house_text;
rename cc354 vote_intent_general;
rename cc356b vote_intent_gov;
rename cc356b_t vote_intent_gov_text;
rename cc390b vote_intent_house;
rename cc390b_t vote_intent_house_text;
rename cc354c vote_intent_pres_12;
rename cc354c_t vote_intent_pres_12_text;
rename cc355b vote_intent_senate;
rename cc355b_t vote_intent_senate_text;
rename cc403 vote_method;
rename cc317 vote_pres_08;
rename cc317_t vote_pres_08_text;
rename cc410a vote_pres_12;
rename cc410a_t vote_pres_12_text;
rename cc406a vote_problem;
rename cc406c vote_problem_allowed;
rename cc406b_1 vote_problem_id;
rename cc406b_2 vote_problem_registration;
rename cc406b_3 vote_problem_wrong_location;
rename cc413b vote_secstate;
rename cc410b vote_sen;
rename cc410b_t vote_sen_text;
rename cc413d vote_staterep;
rename cc413c vote_statesen;
rename v103 weight;
rename lookupzip_post zip_post;
drop inputzip;
rename lookupzip zip_pre;

gen year = 2012;

save "2012/cces_12_for_cumulative.dta", replace;





#delimit;
append using "2011/cces_11_for_cumulative.dta";
append using "2010/cces_10_for_cumulative.dta";
append using "2009/cces_09_for_cumulative.dta";
append using "2008/cces_08_for_cumulative.dta";
append using "2007/cces_07_for_cumulative.dta";
append using "2006/cces_06_for_cumulative.dta";


#delimit;

drop state_fips_pre;

replace state_pre = 1 if state_pre_06 == "AL";
replace state_pre = 2 if state_pre_06 == "AK";
replace state_pre = 4 if state_pre_06 == "AZ";
replace state_pre = 5 if state_pre_06 == "AR";
replace state_pre = 6 if state_pre_06 == "CA";
replace state_pre = 8 if state_pre_06 == "CO";
replace state_pre = 9 if state_pre_06 == "CT";
replace state_pre = 10 if state_pre_06 == "DE";
replace state_pre = 11 if state_pre_06 == "DC";
replace state_pre = 12 if state_pre_06 == "FL";
replace state_pre = 13 if state_pre_06 == "GA";
replace state_pre = 15 if state_pre_06 == "HI";
replace state_pre = 16 if state_pre_06 == "ID";
replace state_pre = 17 if state_pre_06 == "IL";
replace state_pre = 18 if state_pre_06 == "IN";
replace state_pre = 19 if state_pre_06 == "IA";
replace state_pre = 20 if state_pre_06 == "KS";
replace state_pre = 21 if state_pre_06 == "KY";
replace state_pre = 22 if state_pre_06 == "LA";
replace state_pre = 23 if state_pre_06 == "ME";
replace state_pre = 24 if state_pre_06 == "MD";
replace state_pre = 25 if state_pre_06 == "MA";
replace state_pre = 26 if state_pre_06 == "MI";
replace state_pre = 27 if state_pre_06 == "MN";
replace state_pre = 28 if state_pre_06 == "MS";
replace state_pre = 29 if state_pre_06 == "MO";
replace state_pre = 30 if state_pre_06 == "MT";
replace state_pre = 31 if state_pre_06 == "NE";
replace state_pre = 32 if state_pre_06 == "NV";
replace state_pre = 33 if state_pre_06 == "NH";
replace state_pre = 34 if state_pre_06 == "NJ";
replace state_pre = 35 if state_pre_06 == "NM";
replace state_pre = 36 if state_pre_06 == "NY";
replace state_pre = 37 if state_pre_06 == "NC";
replace state_pre = 38 if state_pre_06 == "ND";
replace state_pre = 39 if state_pre_06 == "OH";
replace state_pre = 40 if state_pre_06 == "OK";
replace state_pre = 41 if state_pre_06 == "OR";
replace state_pre = 42 if state_pre_06 == "PA";
replace state_pre = 44 if state_pre_06 == "RI";
replace state_pre = 45 if state_pre_06 == "SC";
replace state_pre = 46 if state_pre_06 == "SD";
replace state_pre = 47 if state_pre_06 == "TN";
replace state_pre = 48 if state_pre_06 == "TX";
replace state_pre = 49 if state_pre_06 == "UT";
replace state_pre = 50 if state_pre_06 == "VT";
replace state_pre = 51 if state_pre_06 == "VA";
replace state_pre = 53 if state_pre_06 == "WA";
replace state_pre = 54 if state_pre_06 == "WV";
replace state_pre = 55 if state_pre_06 == "WI";
replace state_pre = 56 if state_pre_06 == "WY";

drop state_pre_06 state_fips_post;

destring congdist_pre, replace;
destring congdist_post, replace;
replace congdist_pre = congdist_pre_06 if congdist_pre_06 != .;
drop congdist_pre_06;
destring congdist_redist_pre, replace;
destring congdist_redist_post, replace;

destring county_fips_pre, replace;
destring county_fips_post, replace;
replace county_fips_pre = county_fips_pre_06 if county_fips_pre_06 != .;
drop county_fips_pre_06;

replace media_market_area_pre = media_market_area_pre_06 if media_market_area_pre_06 != .;
drop media_market_area_pre_06;



gen holder = zip_pre_06;
tostring holder, replace;
replace zip_pre = holder if zip_pre == "";
drop holder;
gen holder = zip_post_06;
tostring holder, replace;
replace zip_post = holder if zip_post == "";
drop holder zip_pre_06 zip_post_06;



#delimit;
replace age = year-birthyr;
tostring age, gen(holder);
destring holder, replace;
drop age;
rename holder age;
replace age_group = 1 if age < 35;
replace age_group = 2 if age > 34 & age < 55;
replace age_group = 3 if age > 54;
label define ages 1 "18-34" 2 "35-54" 3 "55+";
label values age_group ages;

label define unions 1 "Current member in household" 
2 "Former member in household" 
3 "No union members in household"
4 "Not sure";
label values union_household unions;

label define ownrent 1 "I rent"
2 "I own my apt or home"
3 "I live with someone else, but don't rent"
4 "I live in institutional housing (dorm/nursing home)";
label values home_owner_08 ownrent;

label values pid7_post PID7;

drop run_for_office_needscleaned;


order
year
caseid
weight
start_pre
end_pre
start_post
end_post
survey_complete
state_pre
state_post
congdist_pre
congdist_post
congdist_redist_pre
congdist_redist_post
zip_pre
zip_post
reg_zip_same_pre
reg_zip_pre
reg_zip_same_post
reg_zip_post
county_fips_pre
county_name_pre
county_fips_post
county_name_post
census_region
media_market_area_pre
media_market_name_pre
media_market_area_post
media_market_name_post
residence_address_months
residence_address_months_06
residence_address_years
residence_address_years_06
residence_city_months
residence_city_months_06
residence_city_years
residence_city_years_06
birthyr
age
age_group
gender
race
race_text
hispanic
educ
marriage_status
children
children_number
immigration_status
military_serving
military_served
military_family
military_family_served
military_none
union
union_08
union_household
employ
employ_text
employ_past_five_yrs
employer_category
occupation_category
industry_class
family_income
family_income_06
home_owner
home_owner_08
home_owner_text
stock_invest
computer_type
internet_home
internet_work
phone
health_ins_work
health_ins_govt
health_ins_school
health_ins_self
health_ins_notsure
health_ins_none
church_attendence
church_attendence_06_07
prayer_freq
prayer_freq_06
born_again
relig_importance
relig_importance_06
religion
religion_text
religion_06
relig_other_06
relig_protestant
relig_protestant_06
relig_protestant_text
relig_advent
relig_advent_text
relig_baptist
relig_baptist_text
relig_buddhist
relig_buddhist_text
relig_catholic
relig_catholic_text
relig_christian
relig_christian_06
relig_christian_text
relig_congreg
relig_congreg_text
relig_episcopal
relig_episcopal_text
relig_hindu
relig_hindu_text
relig_holiness
relig_holiness_text
relig_jewish
relig_jewish_text
relig_lutheran
relig_lutheran_text
relig_methodist
relig_methodist_text
relig_mormon
relig_mormon_text
relig_muslim
relig_muslim_text
relig_nondenom
relig_nondenom_text
relig_orthodox
relig_orthodox_text
relig_pentecost
relig_pentecost_text
relig_presbyterian
relig_presbyterian_text
relig_reformed
relig_reformed_text
pid3
pid3_other
pid3_post
pid3_post_other
pid7
pid7_post
ideo5
ideo7
ideo100
racial_resent_special_favors
racial_resent_slavery
ideo7_dems
ideo7_gop
ideo7_obama
ideo7_gov
ideo7_rep
ideo7_housedemcand
ideo7_houserepcand
ideo7_sen1
ideo7_sen2
ideo7_sencand1
ideo7_sencand2
ideo7_scotus
ideo7_teaparty
ideo100_dems
ideo100_gop
ideo100_gwbush
ideo100_obama
ideo100_mccain
ideo100_gov
ideo100_govcand1
ideo100_govcand2
ideo100_housecand1
ideo100_housecand2
ideo100_sen1
ideo100_sen2
ideo100_sencand1
ideo100_sencand2
approval_pres
approval_cong
approval_gov
approval_rep
approval_sen1
approval_sen2
approval_scotus
approval_stateleg
house_inc_competence
house_inc_integrity
news_interest
politics_interest
economy_retrospective
economy_retrospective_09
economy_prospective
fiscal_pref
fiscal_pref_least
tax_vs_spendingcuts
sales_vs_incometaxes
jobs_environment
soc_sec_private
immig_legal_status
immig_guest_worker
immig_fine_businesses
immig_border_patrol
immig_auto_citizenship
immig_police_question
immig_border_wall
immig_hosp_school
immig_none
mil_use_allies
mil_use_democracy
mil_use_genocide
mil_use_intl_law
mil_use_oil
mil_use_terrorist
mil_use_none
mistake_iraq
mistake_iraq_08
mistake_afghanistan
global_warming
global_warming_06
abortion
affirm_action
affirm_action_06
gay_marriage_amendment
gay_marriage_amendment_06
gun_control
stem_cell_research
opinion_cafta
opinion_extend_capgains
opinion_immig_citizenship
opinion_iraqwithdraw
opinion_minwage
opinion_partial_birth
opinion_stemcell
opinion_schip
opinion_surveillance
opinion_tarp
opinion_stimulus
opinion_affordablecareact
opinion_captrade
opinion_dadt_repeal
opinion_tea_party
opinion_tea_party_11
projects_to_dist
project_description
knowledge_house_maj
knowledge_sen_maj
knowledge_rep_party_name
knowledge_rep_party_06_07
knowledge_rep_race
knowledge_rep_race_text
knowledge_house_demcand_race
knowledge_house_demcand_race_t
knowledge_house_repcand_race
knowledge_house_repcand_race_t
knowledge_sen1_party_name
knowledge_sen1_party_06_07
knowledge_sen2_party_name
knowledge_sen2_party_06_07
knowledge_governor_party_name
knowledge_gov_party_06_07
knowledge_state_house_maj
knowledge_state_sen_maj
knowledge_sen1_cafta
knowledge_sen2_cafta
knowledge_sen1_immig
knowledge_sen2_immig
knowledge_sen1_iraqwithdraw
knowledge_sen2_iraqwithdraw
knowledge_house_iraqwithdraw
knowledge_sen1_minwage
knowledge_sen2_minwage
knowledge_sen1_abortion
knowledge_sen2_abortion
knowledge_sen1_stemcell
knowledge_sen2_stemcell
knowledge_sen1_schip
knowledge_sen2_schip
knowledge_house_schip
knowledge_sen1_surveillance
knowledge_sen2_surveillance
knowledge_house_surveillance
knowledge_sen1_capgains
knowledge_sen2_capgains
knowledge_sen1_debtceiling
knowledge_sen2_debtceiling
knowledge_house_debtceiling
registered_pre
registered_post
turnout_06
turnout_08
turnout_10
turnout_12
turnout_intent_primary
turnout_primary_08
no_vote_reason1
no_vote_reason1_text
no_vote_reason2
no_vote_reason2_text
vote_method
same_day_reg
line_length
line_length_text
show_voter_id
voter_id_allowed_vote
vote_problem
vote_problem_registration
vote_problem_wrong_location
vote_problem_id
vote_problem_allowed
media_24hrs_newspaper
media_24hrs_newspaper_type
media_24hrs_tv_news
media_24hrs_tv_type
media_24hrs_radio
media_24hrs_blog
media_24hrs_none
past_yr_local_pol_meetings
past_yr_polit_sign
past_yr_work_for_campaign
past_yr_donate_money
past_yr_donate_blood
past_yr_none
donation_amount
donation_pres
donation_house_mine
donation_house_other
donation_sen_mine
donation_sen_other
donation_stateoffice
donation_party
donation_politicalgroup
donation_pac
run_for_office
run_for_schoolboard
run_for_localboard
run_for_citycoun
run_for_mayor
run_for_distatty
run_for_countywide
run_for_judge
run_for_stateleg
run_for_statewide
run_for_federal
run_for_other
run_for_other_text
contact_house
contact_house_satisfied
campaign_contact
campaign_contact_inperson
campaign_contact_phone
campaign_contact_mail
campaign_contact_email
vote_intent_general
vote_intent_pres_08
vote_intent_pres_08_text
vote_intent_pres_12
vote_intent_pres_12_text
vote_intent_house
vote_intent_house_text
vote_intent_senate
vote_intent_senate_text
vote_intent_gov
vote_intent_gov_text
vote_pres_08
vote_pres_08_text
vote_pres_12
vote_pres_12_text
vote_house
vote_house_text
vote_sen
vote_sen_text
vote_sen2
vote_gov
vote_gov_text
vote_attygen
vote_attygen_06
vote_secstate
vote_staterep
vote_statesen
vote_house_hypothetical1
vote_house_hypothetical2
reg_validation
gen_validated
prim_validated;



compress;

label var year "year";
label var caseid "case ID number";
label var weight "case weight";
label var start_pre "start time - pre";
label var end_pre "end time - pre";
label var start_post "start time - post";
label var end_post "end time - post";
label var survey_complete "R completed survey";
label var state_pre "state - pre";
label var state_post "state - post";
label var congdist_pre "congressional district number - pre";
label var congdist_post "congressional district number - post";
label var congdist_redist_pre "congressional district (post redistricting) - pre";
label var congdist_redist_post "congressional district (post redistricting) - post";
label var zip_pre "zip code - pre";
label var zip_post "zip code - post";
label var reg_zip_same_pre "registered in same zip as residence - pre";
label var reg_zip_pre "registration zip code - pre";
label var reg_zip_same_post "registered in same zip as residence - post";
label var county_fips_pre "county fipscode - pre";
label var county_name_pre "county name - pre";
label var county_fips_post "county fipscode - post";
label var county_name_post "county name - post";
label var census_region "census region";
label var media_market_area_pre "media market area code - pre";
label var media_market_name_pre "media market name - pre";
label var media_market_area_post "media market area code - post";
label var media_market_name_post "media market name - post";
label var residence_address_months "months lived at current address";
label var residence_address_months_06 "months lived at current address - 06";
label var residence_address_years "years lived at current address";
label var residence_address_years_06 "years lived at current address - 06";
label var residence_city_months "months lived in current city";
label var residence_city_months_06 "months lived in current city - 06";
label var residence_city_years "years lived in current city";
label var residence_city_years_06 "years lived in current city - 06";
label var birthyr "birth year";
label var age "age";
label var age_group "age, by category";
label var gender "gender";
label var race "race";
label var race_text "race - text";
label var hispanic "hispanic";
label var educ "education level";
label var marriage_status "marriage status";
label var children "R has children?";
label var children_number "number of children";
label var immigration_status "immigration status";
label var military_served "R has served in military";
label var military_serving "R is serving in military";
label var military_family "R's family member is serving in military";
label var military_family_served "R's family member has served in military";
label var military_none "R and no family members in military";
label var union "union member";
label var union_08 "union member - 08";
label var union_household "union member in household";
label var employ "employment status";
label var employ_text "employment status - text";
label var employ_past_five_yrs "employment status in last 5 years";
label var employer_category "employer category";
label var occupation_category "occupation category";
label var industry_class "industry class";
label var family_income "family income";
label var family_income_06 "family income - 06-10";
label var home_owner "home owner";
label var home_owner_08 "home owner - 08";
label var home_owner_text "home owner - text";
label var stock_invest "R owns stocks";
label var computer_type "computer type";
label var internet_home "internet access at home";
label var internet_work "internet access at work";
label var phone "phone service";
label var health_ins_work "health insurance from work";
label var health_ins_govt "health insurance from govt (Medicare/Medicaid)";
label var health_ins_school "health insurance from school";
label var health_ins_self "health insurance purchased by R";
label var health_ins_notsure "health insurance - not sure";
label var health_ins_none "no health insurance";
label var church_attendence "church attendence";
label var church_attendence_06_07 "church attendence - 06-07";
label var prayer_freq "frequency of prayer";
label var prayer_freq_06 "frequency of prayer - 06";
label var religion "religion";
label var religion_06 "religion - 06";
label var religion_text "religion - text";
label var born_again "R is born again evangelical";
label var relig_advent "religion - adventist";
label var relig_advent_text "religion - adventist - text";
label var relig_baptist "religion - baptist";
label var relig_baptist_text "religion - baptist - text";
label var relig_buddhist "religion - buddhist";
label var relig_buddhist_text "religion - buddhist - text";
label var relig_catholic "religion - catholic";
label var relig_catholic_text "religion - catholic - text";
label var relig_christian "religion - christian";
label var relig_christian_06 "religion - christian - 06";
label var relig_christian_text "religion - christian - text";
label var relig_congreg "religion - congregationalist";
label var relig_congreg_text "religion - congregationalist - text";
label var relig_episcopal "religion - episcopal";
label var relig_episcopal_text "religion - episcopal - text";
label var relig_hindu "religion - hindu";
label var relig_hindu_text "religion - hindu - text";
label var relig_holiness "religion - holiness";
label var relig_holiness_text "religion - holiness - text";
label var relig_importance "importance of religion in R's life";
label var relig_importance_06 "importance of religion in R's life - 06";
label var relig_jewish "religion - jewish";
label var relig_jewish_text "religion - jewish - text";
label var relig_lutheran "religion - lutheran";
label var relig_lutheran_text "religion - lutheran - text";
label var relig_methodist "religion - methodist";
label var relig_methodist_text "religion - methodist - text";
label var relig_mormon "religion - mormon";
label var relig_mormon_text "religion - mormon - text";
label var relig_muslim "religion - muslim";
label var relig_muslim_text "religion - muslim - text";
label var relig_nondenom "religion - nondenominational christian";
label var relig_nondenom_text "religion - nondenominational christian - text";
label var relig_orthodox "religion - orthodox christian";
label var relig_orthodox_text "religion - orthodox christian - text";
label var relig_other_06 "religion - 06 - text";
label var relig_pentecost "religion - pentecostal";
label var relig_pentecost_text "religion - pentecostal - text";
label var relig_presbyterian "religion - presbyterian";
label var relig_presbyterian_text "religion - presbyterian - text";
label var relig_protestant "religion - protestant";
label var relig_protestant_06 "religion - protestant - 06";
label var relig_protestant_text "religion - protestant - text";
label var relig_reformed "religion - reformed";
label var relig_reformed_text "religion - reformed - text";
label var pid3 "3 point party ID - pre";
label var pid3_other "3 point party ID - text - pre";
label var pid3_post "3 point party ID - post";
label var pid3_post_other "3 point party ID - text - post";
label var pid7 "7 point party ID - pre";
label var pid7_post "7 point party ID - post";
label var ideo5 "5 point ideology";
label var ideo7 "7 point ideology";
label var ideo100 "100 point ideology";
label var racial_resent_special_favors "racial resentment - special favors";
label var racial_resent_slavery "racial resentment - slavery";
label var ideo7_dems "democrats - 7 point ideological placement";
label var ideo7_gop "republicans - 7 point ideological placement";
label var ideo7_obama "obama - 7 point ideological placement";
label var ideo7_gov "governor - 7 point ideological placement";
label var ideo7_rep "house incumbent - 7 point ideological placement";
label var ideo7_housedemcand "house dem candidate - 7 point ideological placement";
label var ideo7_houserepcand "house gop candidate - 7 point ideological placement";
label var ideo7_sen1 "senator 1 - 7 point ideological placement";
label var ideo7_sen2 "senator 2 - 7 point ideological placement";
label var ideo7_sencand1 "senate candidate 1 - 7 point ideological placement";
label var ideo7_sencand2 "senate candidate 2 - 7 point ideological placement";
label var ideo7_scotus "supreme court - 7 point ideological placement";
label var ideo7_teaparty "tea party - 7 point ideological placement";
label var ideo100_dems "democrats - 100 point ideological placement";
label var ideo100_gop "republicans - 100 point ideological placement";
label var ideo100_gwbush "bush - 100 point ideological placement";
label var ideo100_obama "obama - 100 point ideological placement";
label var ideo100_mccain "mccain - 100 point ideological placement";
label var ideo100_gov "governor - 100 point ideological placement";
label var ideo100_govcand1 "governor candidate 1 - 100 point ideological placement";
label var ideo100_govcand2 "governor candidate 2 - 100 point ideological placement";
label var ideo100_housecand1 "house candidate 1 - 100 point ideological placement";
label var ideo100_housecand2 "house candidate 2 - 100 point ideological placement";
label var ideo100_sen1 "senator 1 - 100 point ideological placement";
label var ideo100_sen2 "senator 2 - 100 point ideological placement";
label var ideo100_sencand1 "senate candidate 1 - 100 point ideological placement";
label var ideo100_sencand2 "senate candidate 2 - 100 point ideological placement";
label var approval_pres "president - job approval";
label var approval_cong "congress - job approval";
label var approval_gov "governor - job approval";
label var approval_rep "house incumbent - job approval";
label var approval_sen1 "senator 1 - job approval";
label var approval_sen2 "senator 2 - job approval";
label var approval_scotus "supreme court - job approval";
label var approval_stateleg "state legislature - job approval";
label var house_inc_competence "competence of house incumbent";
label var house_inc_integrity "integrity of house incumbent";
label var news_interest "interest in the news";
label var politics_interest "interest in politics";
label var economy_retrospective "economic evaluation - retrospective";
label var economy_retrospective_09 "economic evaluation - retrospective - 09";
label var economy_prospective "economic evaluation - prospective";
label var fiscal_pref "how to balance budget preference";
label var fiscal_pref_least "how to balance budge - least preferred";
label var tax_vs_spendingcuts "tax versus spending cuts preference";
label var sales_vs_incometaxes "sales versus income tax preference";
label var jobs_environment "jobs-environment tradeoff preference";
label var soc_sec_private "social security privitization preference";
label var immig_legal_status "immigration reform - legal status to immigrants";
label var immig_guest_worker "immigration reform - guest worker program";
label var immig_fine_businesses "immigration reform - fine businesses";
label var immig_border_patrol "immigration reform - more border patrols";
label var immig_auto_citizenship "immigration reform - eliminate automatic citizenship";
label var immig_police_question "immigration reform - allow police to stop and question";
label var immig_border_wall "immigration reform - build border wall";
label var immig_hosp_school "immigration reform - ban from hospitals and schools";
label var immig_none "immigration reform - none of the above";
label var mil_use_allies "legitimate military use - protect allies";
label var mil_use_democracy "legitimate military use - promote democracy";
label var mil_use_genocide "legitimate military use - stop genocide";
label var mil_use_intl_law "legitimate military use - protect international law";
label var mil_use_oil "legitimate military use - secure oil reserves";
label var mil_use_terrorist "legitimate military use - stop terrorists";
label var mil_use_none "legitimate military use - none of above";
label var mistake_iraq "mistake to invade iraq";
label var mistake_iraq_08 "mistake to invade iraq - 08";
label var mistake_afghanistan "mistake to invade afghanistan";
label var global_warming "global warming opinion";
label var global_warming_06 "global warming opinion - 06";
label var abortion "abortion opinion";
label var affirm_action "affirmative action opinion";
label var affirm_action_06 "affirmative action opinion - 06";
label var gay_marriage_amendment "gay marriage ban amendment opinion";
label var gay_marriage_amendment_06 "gay marriage ban amendment opinion - 06";
label var gun_control "gun control opinion";
label var stem_cell_research "stem cell research opinion";
label var opinion_cafta "cafta opinion";
label var opinion_extend_capgains "extend capital gains opinion";
label var opinion_immig_citizenship "citizenship for immigrants opinion";
label var opinion_iraqwithdraw "iraq withdrawal opinion";
label var opinion_minwage "increase minimum wage opinion";
label var opinion_partial_birth "partial birth abortion ban opinion";
label var opinion_stemcell "stem cell research opinion";
label var opinion_schip "schip opinion";
label var opinion_surveillance "fisa surveillance opinion";
label var opinion_tarp "tarp opinion";
label var opinion_stimulus "stimulus opinion";
label var opinion_affordablecareact "afforable care act - obamacare opinion";
label var opinion_captrade "cap and trade opinion";
label var opinion_dadt_repeal "don't ask don't tell repeal opinion";
label var opinion_tea_party "tea party opinion";
label var opinion_tea_party_11 "tea party opinion - 11";
label var projects_to_dist "did house incumbent get projects for district";
label var project_description "description of projects for district";
label var knowledge_house_maj "house majority - knowledge";
label var knowledge_sen_maj "senate majority - knowledge";
label var knowledge_rep_party_name "house incumbent party and name - knowledge";
label var knowledge_rep_party_06_07 "house incumbent party - knowledge - 06-07";
label var knowledge_rep_race "house incumbent race - knowledge";
label var knowledge_rep_race_text "house incumbent race - knowledge - text";
label var knowledge_house_demcand_race "house dem candidate race - knowledge";
label var knowledge_house_demcand_race_t "house dem candidate race - knowledge - text";
label var knowledge_house_repcand_race "house gop candidate race - knowledge";
label var knowledge_house_repcand_race_t "house gop candidate race - knowledge - text";
label var knowledge_sen1_party_name "senator 1 party and name - knowledge";
label var knowledge_sen1_party_06_07 "senator 1 party - knowledge - 06-07";
label var knowledge_sen2_party_name "senator 2 party and name - knowledge";
label var knowledge_sen2_party_06_07 "senator 2 party - knowledge - 06-07";
label var knowledge_governor_party_name "governor party and name - knowledge";
label var knowledge_gov_party_06_07 "governor party - knowledge - 06-07";
label var knowledge_state_house_maj "state house majority - knowledge";
label var knowledge_state_sen_maj "state senate majority - knowledge";
label var knowledge_sen1_cafta "senator 1 - cafta vote - knowledge";
label var knowledge_sen2_cafta "senator 2 - cafta vote - knowledge";
label var knowledge_sen1_immig "senator 1 - immigration reform vote - knowledge";
label var knowledge_sen2_immig "senator 2 - immigration reform vote - knowledge";
label var knowledge_sen1_iraqwithdraw "senator 1 - iraq withdrawal vote - knowledge";
label var knowledge_sen2_iraqwithdraw "senator 2 - iraq withdrawal vote - knowledge";
label var knowledge_house_iraqwithdraw "house incumbent - iraq withdrawal vote - knowledge";
label var knowledge_sen1_minwage "senator 1 - minimum wage increase vote - knowledge";
label var knowledge_sen2_minwage "senator 2 - minimum wage increase vote - knowledge";
label var knowledge_sen1_abortion "senator 1 - partial birth abortion vote - knowledge";
label var knowledge_sen2_abortion "senator 2 - partial birth abortion vote - knowledge";
label var knowledge_sen1_stemcell "senator 1 - stem cell research vote - knowledge";
label var knowledge_sen2_stemcell "senator 2 - stem cell research vote - knowledge";
label var knowledge_sen1_schip "senator 1 - schip renewal vote - knowledge";
label var knowledge_sen2_schip "senator 2 - schip renewal vote - knowledge";
label var knowledge_house_schip "house incumbent - schip renewal vote - knowledge";
label var knowledge_sen1_surveillance "senator 1 - fisa surveillance vote - knowledge";
label var knowledge_sen2_surveillance "senator 2 - fisa surveillance vote - knowledge";
label var knowledge_house_surveillance "house incumbent - fisa surveillance vote - knowledge";
label var knowledge_sen1_capgains "senator 1 - capital gains vote - knowledge";
label var knowledge_sen2_capgains "senator 2 - capital gains vote - knowledge";
label var knowledge_sen1_debtceiling "senator 1 - increase debt ceiling vote - knowledge";
label var knowledge_sen2_debtceiling "senator 2 - increase debt ceiling vote - knowledge";
label var knowledge_house_debtceiling "house incumbent - increase debt ceiling vote - knowledge";
label var registered_pre "registered to vote - pre";
label var registered_post "registered to vote - post";
label var turnout_06 "turnout in 2006 election";
label var turnout_08 "turnout in 2008 election";
label var turnout_10 "turnout in 2010 election";
label var turnout_12 "turnout in 2012 election";
label var turnout_intent_primary "intends to turnout in primary";
label var turnout_primary_08 "turnout in 2008 presidential primary";
label var no_vote_reason1 "reason for not voting 1";
label var no_vote_reason1_text "reason for not voting 1 - text";
label var no_vote_reason2 "reason for not voting 2";
label var no_vote_reason2_text "reason for not voting 2 - text";
label var vote_method "vote method (in person, early, mail)";
label var same_day_reg "registered on election day";
label var line_length "time spent in line to vote";
label var line_length_text "time spend in line to vote - text";
label var show_voter_id "asked to show voter identification";
label var voter_id_allowed_vote "allowed to vote after showing voter ID";
label var vote_problem "problem with registration or ID when voting";
label var vote_problem_registration "problem with registration when voting";
label var vote_problem_wrong_location "problem with polling location when voting";
label var vote_problem_id "problem with voter ID when voting";
label var vote_problem_allowed "allowed to vote after having problem";
label var media_24hrs_newspaper "read newspaper - past 24 hours";
label var media_24hrs_newspaper_type "type of newspaper read - past 24 hours";
label var media_24hrs_tv_news "watched TV news - past 24 hours";
label var media_24hrs_tv_type "type of TV news watched - past 24 hours";
label var media_24hrs_radio "listened to talk radio - past 24 hours";
label var media_24hrs_blog "read a blog - past 24 hours";
label var media_24hrs_none "none of above - past 24 hours";
label var past_yr_local_pol_meetings "attended local political meetings - past year";
label var past_yr_polit_sign "put up a political sign or bumper sticker - past year";
label var past_yr_work_for_campaign "worked for a campaign - past year";
label var past_yr_donate_money "donated money to campaign - past year";
label var past_yr_donate_blood "donated blood - past year";
label var past_yr_none "none of above - past year";
label var donation_amount "amount of money donated to all campaigns";
label var donation_pres "donated money to presidential candidate";
label var donation_house_mine "donated money to R's house candidate";
label var donation_house_other "donated money to a different house candidate";
label var donation_sen_mine "donated money to R's senate candidate";
label var donation_sen_other "donated money to a different senate candidate";
label var donation_stateoffice "donated money to candidate for statewide office";
label var donation_party "donated money to dem or rep party";
label var donation_politicalgroup "donated money to political group";
label var donation_pac "donated money to corporate or union PAC";
label var run_for_office "R has run for office before";
label var run_for_schoolboard "run for school board position";
label var run_for_localboard "run for local board position";
label var run_for_citycoun "run for city council";
label var run_for_mayor "run for city mayor";
label var run_for_distatty "run for city or district attorney";
label var run_for_countywide "run for countywide office";
label var run_for_judge "run for judge position";
label var run_for_stateleg "run for state legislature";
label var run_for_statewide "run for statewide office";
label var run_for_federal "run for federal office";
label var run_for_other "run for other office";
label var run_for_other_text "run for other office - text";
label var contact_house "contacted house incumbent";
label var contact_house_satisfied "satisfied with contact house incumbent";
label var campaign_contact "contacted by a campaign";
label var campaign_contact_inperson "contacted by a campaign - in person";
label var campaign_contact_phone "contacted by a campaign - by phone";
label var campaign_contact_mail "contacted by a campaign - by mail";
label var campaign_contact_email "contacted by a campaign - by email";
label var vote_intent_general "generic ballot - vote intent";
label var vote_intent_pres_08 "president 08 - vote intent";
label var vote_intent_pres_08_text "president 08 - vote intent - text";
label var vote_intent_pres_12 "president 12 - vote intent";
label var vote_intent_pres_12_text "president 12 - vote intent - text";
label var vote_intent_house "house - vote intent";
label var vote_intent_house_text "house - vote intent - text";
label var vote_intent_senate "senate - vote intent";
label var vote_intent_senate_text "senate - vote intent - text";
label var vote_intent_gov "governor - vote intent";
label var vote_intent_gov_text "governor - vote intent - text";
label var vote_pres_08 "president 08 - vote";
label var vote_pres_08_text "president 08 - vote - text";
label var vote_pres_12 "president 12 - vote";
label var vote_pres_12_text "president 12 - vote - text";
label var vote_house "house - vote";
label var vote_house_text "house - vote - text";
label var vote_sen "senate - vote";
label var vote_sen_text "senate - vote - text";
label var vote_sen2 "senate race 2 - vote";
label var vote_gov "governor - vote";
label var vote_gov_text "governor - vote - text";
label var vote_attygen "attorney general - vote";
label var vote_secstate "secretary of state - vote";
label var vote_staterep "state rep - vote";
label var vote_statesen "state senator - vote";
label var vote_house_hypothetical1 "who would you vote for in this hypothetical house race?";
label var vote_house_hypothetical2 "who would you vote for in this hypothetical house race?";
label var reg_validation "validated voter registration status";
label var gen_validated "validated vote history for most recent general election";
label var prim_validated "validated vote history for most recent primary election";
label var reg_zip_post "registration zip code - post";
#delimit;
preserve;






































//Now break the file up into smaller, more manageable ones

keep caseid
year
weight
vote_pres_08_text
vote_intent_pres_12_text
vote_intent_senate_text
vote_intent_gov_text
vote_intent_house_text
employ_text
pid3_other
religion_text
relig_protestant_text
relig_baptist_text
relig_methodist_text
relig_nondenom_text
relig_lutheran_text
relig_presbyterian_text
relig_pentecost_text
relig_episcopal_text
relig_christian_text
relig_congreg_text
relig_holiness_text
relig_reformed_text
relig_advent_text
relig_catholic_text
relig_mormon_text
relig_orthodox_text
relig_jewish_text
relig_muslim_text
relig_buddhist_text
relig_hindu_text
home_owner_text
no_vote_reason1_text
no_vote_reason2_text
line_length_text
vote_pres_12_text
vote_sen_text
vote_gov_text
vote_house_text
run_for_other_text
knowledge_house_demcand_race_t
knowledge_house_repcand_race_t
knowledge_rep_race_text
race_text
project_description
vote_intent_pres_08_text
pid3_post_other
relig_protestant_06
relig_christian_06
relig_other_06;

save "Cumulative File/cumulative_textquestions.dta", replace;

clear;








restore;

drop 
vote_pres_08_text
vote_intent_pres_12_text
vote_intent_senate_text
vote_intent_gov_text
vote_intent_house_text
employ_text
pid3_other
religion_text
relig_protestant_text
relig_baptist_text
relig_methodist_text
relig_nondenom_text
relig_lutheran_text
relig_presbyterian_text
relig_pentecost_text
relig_episcopal_text
relig_christian_text
relig_congreg_text
relig_holiness_text
relig_reformed_text
relig_advent_text
relig_catholic_text
relig_mormon_text
relig_orthodox_text
relig_jewish_text
relig_muslim_text
relig_buddhist_text
relig_hindu_text
home_owner_text
no_vote_reason1_text
no_vote_reason2_text
line_length_text
vote_pres_12_text
vote_sen_text
vote_gov_text
vote_house_text
run_for_other_text
knowledge_house_demcand_race_t
knowledge_house_repcand_race_t
knowledge_rep_race_text
race_text
project_description
vote_intent_pres_08_text
pid3_post_other
relig_protestant_06
relig_christian_06
relig_other_06;

label define votes
1 "Democratic candidate"
2 "Republican candidate"
3 "Other, third-party candidate"
9 "I did not vote"
10 "Not sure"
11 "There wasn't a race for this office";

replace vote_house = 3 if vote_house == 7 & year == 2012;
replace vote_house = 9 if vote_house == 8 & year == 2012;
replace vote_house = 9 if vote_house == 4 & year == 2011;
replace vote_house = 10 if vote_house == 5 & year == 2011;
replace vote_house = 3 if vote_house == 4 & year == 2010;
replace vote_house = 3 if vote_house == 7 & year == 2010;
replace vote_house = 9 if vote_house == 8 & year == 2010;
replace vote_house = 9 if vote_house == 4 & year == 2009;
replace vote_house = 10 if vote_house == 5 & year == 2009;
replace vote_house = 9 if vote_house == 4 & year == 2008;
replace vote_house = 10 if vote_house == 5 & year == 2008;
replace vote_house = 9 if vote_house == 3 & year == 2006;
replace vote_house = 9 if vote_house == 4 & year == 2006;
replace vote_house = 10 if vote_house == 5 & year == 2006;
replace vote_house = 3 if vote_house == 6 & year == 2006;
replace vote_house = 3 if vote_house == 7 & year == 2006;
label values vote_house votes;

replace vote_gov = 3 if vote_gov == 7 & year == 2012;
replace vote_gov = 9 if vote_gov == 8 & year == 2012;
replace vote_gov = 3 if vote_gov == 4 & year == 2010;
replace vote_gov = 3 if vote_gov == 7 & year == 2010;
replace vote_gov = 9 if vote_gov == 8 & year == 2010;
replace vote_gov = 9 if vote_gov == 4 & year == 2008;
replace vote_gov = 10 if vote_gov == 5 & year == 2008;
replace vote_gov = 9 if vote_gov == 3 & year == 2006;
replace vote_gov = 9 if vote_gov == 4 & year == 2006;
replace vote_gov = 10 if vote_gov == 5 & year == 2006;
replace vote_gov = 3 if vote_gov == 6 & year == 2006;
replace vote_gov = 3 if vote_gov == 7 & year == 2006;
label values vote_gov votes;

replace vote_sen = 3 if vote_sen == 4 & year == 2012;
replace vote_sen = 9 if vote_sen == 5 & year == 2012;
replace vote_sen = 9 if vote_sen == 6 & year == 2012;
replace vote_sen = 10 if vote_sen == 7 & year == 2012;
replace vote_sen = 3 if vote_sen == 4 & year == 2010;
replace vote_sen = 8 if vote_sen == 5 & year == 2010;
replace vote_sen = 8 if vote_sen == 6 & year == 2010;
replace vote_sen = 10 if vote_sen == 7 & year == 2010;
replace vote_sen = 9 if vote_sen == 4 & year == 2009;
replace vote_sen = 10 if vote_sen == 5 & year == 2009;
replace vote_sen = 9 if vote_sen == 4 & year == 2008;
replace vote_sen = 10 if vote_sen == 5 & year == 2008;
replace vote_sen = 10 if vote_sen == 5 & year == 2006;
replace vote_sen = 2 if vote_sen == 9 & year == 2006;
replace vote_sen = 9 if vote_sen == 3 & year == 2006;
replace vote_sen = 9 if vote_sen == 4 & year == 2006;
replace vote_sen = 1 if vote_sen == 7 & year == 2006;
replace vote_sen = 3 if vote_sen == 6 & year == 2006;
replace vote_sen = 3 if vote_sen == 8 & year == 2006;
label values vote_sen votes;

replace vote_sen2 = 3 if vote_sen2 == 4 & year == 2010;
replace vote_sen2 = 8 if vote_sen2 == 5 & year == 2010;
replace vote_sen2 = 8 if vote_sen2 == 6 & year == 2010;
replace vote_sen2 = 10 if vote_sen2 == 7 & year == 2010;
replace vote_sen2 = 9 if vote_sen2 == 4 & year == 2009;
replace vote_sen2 = 10 if vote_sen2 == 5 & year == 2009;
replace vote_sen2 = 9 if vote_sen2 == 4 & year == 2008;
replace vote_sen2 = 10 if vote_sen2 == 5 & year == 2008;
label values vote_sen2 votes;

#delimit;
replace vote_attygen = 9 if vote_attygen == 4;
replace vote_attygen = 11 if vote_attygen == 5;
replace vote_attygen_06 = 9 if vote_attygen_06 == 1;
replace vote_attygen_06 = 1 if vote_attygen_06 == 2;
replace vote_attygen_06 = 2 if vote_attygen_06 == 3;
replace vote_attygen_06 = 3 if vote_attygen_06 == 4;
replace vote_attygen_06 = 10 if vote_attygen_06 == 5;
replace vote_attygen = vote_attygen_06 if year == 2006;
drop vote_attygen_06;
label values vote_attygen votes;

replace vote_secstate = 9 if vote_secstate == 4 & year != 2006;
replace vote_secstate = 11 if vote_secstate == 5 & year != 2006;
replace vote_secstate = 9 if vote_secstate == 1 & year == 2006;
replace vote_secstate = 1 if vote_secstate == 2 & year == 2006;
replace vote_secstate = 2 if vote_secstate == 3 & year == 2006;
replace vote_secstate = 3 if vote_secstate == 4 & year == 2006;
replace vote_secstate = 10 if vote_secstate == 5 & year == 2006;
label values vote_secstate votes;

replace vote_staterep = 9 if vote_staterep == 4 & year == 2012;
replace vote_staterep = 11 if vote_staterep == 5 & year == 2012;
replace vote_staterep = 9 if vote_staterep == 4 & year == 2010;
replace vote_staterep = 11 if vote_staterep == 5 & year == 2010;
replace vote_staterep = 9 if vote_staterep == 5 & year == 2008;
replace vote_staterep = 10 if vote_staterep == 6 & year == 2008;
replace vote_staterep = 9 if vote_staterep == 1 & year == 2006;
replace vote_staterep = 1 if vote_staterep == 2 & year == 2006;
replace vote_staterep = 2 if vote_staterep == 3 & year == 2006;
replace vote_staterep = 3 if vote_staterep == 4 & year == 2006;
replace vote_staterep = 10 if vote_staterep == 5 & year == 2006;
label values vote_staterep votes;

replace vote_statesen = 9 if vote_statesen == 4 & year == 2012;
replace vote_statesen = 11 if vote_statesen == 5 & year == 2012;
replace vote_statesen = 9 if vote_statesen == 4 & year == 2010;
replace vote_statesen = 11 if vote_statesen == 5 & year == 2010;
replace vote_statesen = 9 if vote_statesen == 5 & year == 2008;
replace vote_statesen = 10 if vote_statesen == 6 & year == 2008;
replace vote_statesen = 9 if vote_statesen == 1 & year == 2006;
replace vote_statesen = 1 if vote_statesen == 2 & year == 2006;
replace vote_statesen = 2 if vote_statesen == 3 & year == 2006;
replace vote_statesen = 3 if vote_statesen == 4 & year == 2006;
replace vote_statesen = 10 if vote_statesen == 5 & year == 2006;
label values vote_statesen votes;

label define househyp1
1 "House incumbent"
2 "Democratic candidate"
3 "Not sure"
4 "Someone else"
5 "I wouldn't vote";
label values  vote_house_hypothetical1 househyp1;
label define househyp2
1 "House incumbent"
2 "Republican candidate"
3 "Not sure"
4 "Someone else"
5 "I wouldn't vote";
label values  vote_house_hypothetical1 househyp2;

drop vote_intent_general;

label define pres08
1 "John McCain"
2 "Barack Obama"
3 "Ron Paul"
4 "Ralph Nader"
5 "Bob Barr"
6 "Cynthia McKinney"
7 "Other"
8 "I won't vote in this election"
9 "I'm not sure";
label values vote_intent_pres_08 pres08;

label define vote_intent
1 "Democratic candidate"
2 "Republican candidate"
3 "Other, third-party candidate"
8 "I'm not sure"
9 "No one"
10 "I won't vote in this election";

replace vote_intent_house = 3 if vote_intent_house == 7 & year == 2012;
replace vote_intent_house = 3 if vote_intent_house == 4 & year == 2010;
replace vote_intent_house = 3 if vote_intent_house == 7 & year == 2010;
replace vote_intent_house = 3 if vote_intent_house == 4 & year == 2008;
replace vote_intent_house = 8 if vote_intent_house == 11 & year == 2008;
replace vote_intent_house = 10 if vote_intent_house == 4 & year == 2006;
replace vote_intent_house = 8 if vote_intent_house == 5 & year == 2006;
label values vote_intent_house vote_intent;

replace vote_intent_gov = 3 if vote_intent_gov == 7 & year == 2012;
replace vote_intent_gov = 3 if vote_intent_gov == 4 & year == 2010;
replace vote_intent_gov = 3 if vote_intent_gov == 7 & year == 2010;
replace vote_intent_gov = 3 if vote_intent_gov == 4 & year == 2008;
replace vote_intent_gov = 8 if vote_intent_gov == 6 & year == 2008;
replace vote_intent_gov = 10 if vote_intent_gov == 5 & year == 2008;
replace vote_intent_gov = 10 if vote_intent_gov == 4 & year == 2006;
replace vote_intent_gov = 8 if vote_intent_gov == 5 & year == 2006;
label values vote_intent_gov vote_intent;

replace vote_intent_sen = 3 if vote_intent_sen == 7 & year == 2012;
replace vote_intent_sen = 3 if vote_intent_sen == 7 & year == 2010;
replace vote_intent_sen = 3 if vote_intent_sen == 4 & year == 2008;
replace vote_intent_sen = 8 if vote_intent_sen == 6 & year == 2008;
replace vote_intent_sen = 10 if vote_intent_sen == 5 & year == 2008;
replace vote_intent_sen = 10 if vote_intent_sen == 4 & year == 2006;
replace vote_intent_sen = 8 if vote_intent_sen == 5 & year == 2006;
label values vote_intent_sen vote_intent;

replace survey_complete = 2 if survey_complete != 1;
label define completeness 1 "completed post election wave"
2 "didn't complete post election wave";
label values survey_complete completeness;

label define nothing -55555 "nothing";
label values residence_address_months nothing;
label values residence_address_years nothing;
label values residence_city_months nothing;
label values residence_city_years nothing;

rename family_income_06 family_income_old;

replace phone = 0 if phone == 3 & year == 2009;
replace phone = 3 if phone == 1 & year == 2009;
replace phone = 1 if phone == 0 & year == 2009;

label values ideo100 nothing;
label values ideo100_dems nothing;
label values ideo100_gop nothing;
label values ideo100_gwbush nothing;
label values ideo100_obama nothing;
label values ideo100_mccain nothing;
label values ideo100_gov nothing;
label values ideo100_govcand1 nothing;
label values ideo100_govcand2 nothing;
label values ideo100_housecand1 nothing;
label values ideo100_housecand2 nothing;
label values ideo100_sen1 nothing;
label values ideo100_sen2 nothing;
label values ideo100_sencand1 nothing;
label values ideo100_sencand2 nothing;

label define approval_
1 "Strongly approve"
2 "Somewhat approve"
3 "Somewhat disapprove"
4 "Strongly disapprove"
5 "Not sure"
6 "Neither approve nor disapprove";

label values approval_pres approval_;
label values approval_cong approval_;
label values approval_gov approval_;
label values approval_rep approval_;
label values approval_sen1 approval_;
label values approval_sen2 approval_;
label values approval_scotus approval_;
label values approval_stateleg approval_;

label define evaluation
1 "Extremely strong"
2 "Strong"
3 "Somewhat strong"
4 "Fair"
5 "Somewhat weak"
6 "Weak"
7 "Extremely weak"
8 "Not sure"
9 "I have never heard of this person";
label values house_inc_competence evaluation;
label values house_inc_integrity evaluation;

replace house_inc_competence = house_inc_competence + 1 if house_inc_competence > 3 & year == 2009;
replace house_inc_competence = 9 if house_inc_competence == 8 & year == 2009;
replace house_inc_integrity = house_inc_integrity + 1 if house_inc_integrity > 3 & year == 2009;

label define fiscalpref
1 "Cut defense spending"
2 "Cut domestic spending"
3 "Raise taxes"
4 "Borrow";
label values fiscal_pref fiscalpref;
label values fiscal_pref_least fiscalpref;

label define jobsenv
1 "Much more important to protect environment even if it loses jobs"
2 "Environment somewhat more important"
3 "About the same"
4 "Economy somewhat more important"
5 "Much more important to protect jobs even if environment is worse"
6 "Haven't thought much about this";
label values jobs_environment jobsenv;

label define ssr
1 "Strongly favor"
2 "Somewhat favor"
3 "Neither favor nor oppose"
4 "Somewhat oppose"
5 "Strongly oppose";
label values soc_sec_private ssr;

label define yesno
1 "Yes"
2 "No";
label values immig_guest_worker yesno;
label values immig_none yesno;

label define mistake
1 "Mistake from the beginning"
2 "Mistakes worth the cost"
3 "Right thing, mistakes made it too costly"
4 "Right thing, worth it despite mistakes"
5 "Right thing, no mistakes";
label values mistake_iraq mistake;

label values gay_marriage_amendment yesno;

label define favoroppose
1 "Favor"
2 "Oppose"
3 "Don't know";
label values stem_cell_research favoroppose;
label values opinion_cafta favoroppose;
label values opinion_iraqwithdraw favoroppose;
label values opinion_minwage favoroppose;
label values opinion_partial_birth favoroppose;
label values opinion_stemcell favoroppose;
label values opinion_schip favoroppose;
label values opinion_surveillance favoroppose;
label values opinion_tarp favoroppose;
label values opinion_stimulus favoroppose;
label values opinion_affordablecareact favoroppose;
label values opinion_captrade favoroppose;
label values opinion_dadt_repeal favoroppose;

replace knowledge_house_maj = 0 if knowledge_house_maj == 2 & year == 2009;
replace knowledge_house_maj = 2 if knowledge_house_maj == 1 & year == 2009;
replace knowledge_house_maj = 1 if knowledge_house_maj == 0 & year == 2009;
replace knowledge_house_maj = 0 if knowledge_house_maj == 2 & year == 2007;
replace knowledge_house_maj = 2 if knowledge_house_maj == 1 & year == 2007;
replace knowledge_house_maj = 1 if knowledge_house_maj == 0 & year == 2007;
replace knowledge_house_maj = 0 if knowledge_house_maj == 2 & year == 2006;
replace knowledge_house_maj = 2 if knowledge_house_maj == 1 & year == 2006;
replace knowledge_house_maj = 1 if knowledge_house_maj == 0 & year == 2006;

replace knowledge_sen_maj = 0 if knowledge_sen_maj == 2 & year == 2009;
replace knowledge_sen_maj = 2 if knowledge_sen_maj == 1 & year == 2009;
replace knowledge_sen_maj = 1 if knowledge_sen_maj == 0 & year == 2009;
replace knowledge_sen_maj = 0 if knowledge_sen_maj == 2 & year == 2007;
replace knowledge_sen_maj = 2 if knowledge_sen_maj == 1 & year == 2007;
replace knowledge_sen_maj = 1 if knowledge_sen_maj == 0 & year == 2007;
replace knowledge_sen_maj = 0 if knowledge_sen_maj == 2 & year == 2006;
replace knowledge_sen_maj = 2 if knowledge_sen_maj == 1 & year == 2006;
replace knowledge_sen_maj = 1 if knowledge_sen_maj == 0 & year == 2006;

replace knowledge_state_house_maj = 0 if knowledge_state_house_maj == 2 & year == 2007;
replace knowledge_state_house_maj = 2 if knowledge_state_house_maj == 1 & year == 2007;
replace knowledge_state_house_maj = 1 if knowledge_state_house_maj == 0 & year == 2007;

replace knowledge_state_sen_maj = 0 if knowledge_state_sen_maj == 2 & year == 2007;
replace knowledge_state_sen_maj = 2 if knowledge_state_sen_maj == 1 & year == 2007;
replace knowledge_state_sen_maj = 1 if knowledge_state_sen_maj == 0 & year == 2007;

label define knowledge_party
1 "Democrat"
2 "Republican"
3 "Independent"
4 "Don't know";
label values knowledge_rep_party_06_07 knowledge_party;
label values knowledge_sen1_party_06_07 knowledge_party;
label values knowledge_sen2_party_06_07 knowledge_party;
label values knowledge_gov_party_06_07 knowledge_party;

replace knowledge_rep_race = 6 if knowledge_rep_race == 5 & year == 2008;
replace knowledge_rep_race = . if knowledge_rep_race == 6 & year == 2008;

label define knowledge_vote
1 "For this bill"
2 "Against this bill"
3 "Not sure";
label values knowledge_sen1_debtceiling knowledge_vote;
label values knowledge_sen2_debtceiling knowledge_vote;
label values knowledge_house_debtceiling knowledge_vote;

replace turnout_08 = . if turnout_08 == 4 & year == 2009;
replace turnout_08 = 4 if turnout_08 == 3 & year == 2009;

replace turnout_08 = 7 if turnout_08 == 4 & year == 2008;
replace turnout_08 = 6 if turnout_08 == 2 & year == 2008;
replace turnout_08 = 2 if turnout_08 == 3 & year == 2008;
replace turnout_08 = 4 if turnout_08 == 5 & year == 2008;
replace turnout_08 = 5 if turnout_08 == 7 & year == 2008;

label define turnout
1 "I did not vote in this election"
2 "I thought about voting but didn't"
3 "I am not sure"
4 "I definitely voted in this election"
5 "I attempted to vote but didn't or couldn't"
6 "Not sure";
label values turnout_08 turnout;

label define votemethod
1 "In person on election day"
2 "In person before election day (early)"
3 "Voted by mail (or absentee)"
4 "Don't know"
5 "I did not vote";
label values vote_method votemethod;

label values media_24hrs_newspaper yesno;
label values media_24hrs_tv_news yesno;
label values media_24hrs_radio yesno;
label values media_24hrs_blog yesno;
label values media_24hrs_none yesno;

replace run_for_schoolboard = 2 if run_for_schoolboard == 0;
replace run_for_localboard = 2 if run_for_localboard == 0;
replace run_for_citycoun = 2 if run_for_citycoun == 0;
replace run_for_mayor = 2 if run_for_mayor == 0;
replace run_for_distatty = 2 if run_for_distatty == 0;
replace run_for_countywide = 2 if run_for_countywide == 0;
replace run_for_judge = 2 if run_for_judge == 0;
replace run_for_stateleg = 2 if run_for_stateleg == 0;
replace run_for_statewide = 2 if run_for_statewide == 0;
replace run_for_federal = 2 if run_for_federal == 0;
replace run_for_other = 2 if run_for_other == 0;

label define satisfied
1 "Very satisfied"
2 "Somewhat satisfied"
3 "Not very satisfied"
4 "Not at all satisfied"
5 "Yes"
6 "No";

label drop hist;

label define hist
1 "validated record of voting in general election"
2 "did not vote-verified record of being unregistered"
3 "did not vote-non-citizen"
4 "did not vote-R said they're note registered to vote"
5 "did not vote-R said they didn't vote"
6 "did not vote-R has verified registration file, but not vote history file"
7 "no evidence on whether the R voted or not"
8 "Virginia doesn't maintain vote history files"
9 "did not vote - verified record of not voting";

label values prim_validated hist;
label values gen_validated hist;

replace congdist_pre = 0 if
state_pre == 2 | 
state_pre == 10 | 
state_pre == 30 | 
state_pre == 38 | 
state_pre == 46 | 
state_pre == 50 | 
state_pre == 56; 


replace congdist_post = 0 if
state_post == 2 | 
state_post == 10 | 
state_post == 30 | 
state_post == 38 | 
state_post == 46 | 
state_post == 50 | 
state_post == 56;

replace congdist_redist_pre = 0 if
state_pre == 2 | 
state_pre == 10 | 
state_pre == 30 | 
state_pre == 38 | 
state_pre == 46 | 
state_pre == 50 | 
state_pre == 56; 


replace congdist_redist_post = 0 if
state_post == 2 | 
state_post == 10 | 
state_post == 30 | 
state_post == 38 | 
state_post == 46 | 
state_post == 50 | 
state_post == 56;

label define atlarge 0 "At-large";

label values congdist_pre atlarge;
label values congdist_post atlarge;
label values congdist_redist_pre atlarge;
label values congdist_redist_post atlarge;

label values ideo7_* LABJ;
label define cc09_20 0 "not sure", add;

label define income_old
1 "less than $10,000"
2 "$10k-14,999"
3 "$15k-19,999"
4 "$20k-24,999"
5 "$25k-29,999"
6 "$30k-39,999"
7 "$40k-49,999"
8 "$50k-59,999"
9 "$60k-69,999"
10 "$70k-79,999"
11 "$80k-99,999"
12 "$100k-199,999"
13 "$120k-149,999"
14 "$150k+"
15 "prefer not to say";
label values family_income_old income_old;

label define CC324 5 "Other" 6 "Not sure", add;

replace mistake_iraq = mistake_iraq + 1 if year == 2007;
replace mistake_iraq = 1 if mistake_iraq == 3 & year == 2007;
replace mistake_iraq = 3 if mistake_iraq == 10 & year == 2007;

label define iraqmistake
1 "mistake"
2 "not a mistake"
3 "not sure";
label values mistake_iraq iraqmistake;

replace vote_sen2 = 3 if vote_sen2 == 7;
replace vote_sen2 = 10 if vote_sen2 == 8;
replace vote_sen = 3 if vote_sen == 7;
replace vote_sen = 10 if vote_sen == 8;

replace vote_pres_08 = 3 if vote_pres_08 > 2 & vote_pres_08 < 8 & year == 2008;
replace vote_pres_08 = 4 if vote_pres_08 > 7;
replace vote_pres_08 = 99 if vote_pres_08 == 2 & year == 2008;
replace vote_pres_08 = 2 if vote_pres_08 == 1 & year == 2008;
replace vote_pres_08 = 1 if vote_pres_08 == 99 & year == 2008;

replace relig_lutheran = 4 if relig_lutheran == 90;

label define affirmaxn 
1 "strongly support" 
2 "somewhat support" 
3 "lean toward supporting" 
4 "in the middle" 
5 "lean toward opposing" 
6 "somewhat oppose" 
7 "strongly oppose";

label values affirm_action_06 affirmaxn;

/*drop if year == 2012;

drop 
vote_pres_12 
vote_intent_pres_12
congdist_redist_pre
congdist_redist_post
occupation_category
economy_prospective
immig_auto_citizenship
immig_hosp_school
turnout_12;*/

save "Cumulative File/cces_common_cumulative.dta", replace;


do "cumulative file/add incumbency variables.do";