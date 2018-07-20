# data challenge for 20 july 2018
# actions under the antiquities act

require(tidyverse)
require(cowplot)
require(lubridate)
require(datapasta)
require(stringr)

dat = read_csv("data-challenge/20180720_antiquities/actions_under_antiquities_act.csv")
head(dat)
str(dat)


# clean up numbers with commas (incomplete)----
dat %>% 
  mutate(area_type = ifelse(str_detect(acres_affected, "sq. miles") |
                              str_detect(acres_affected, "square miles"),
                            T, F)) %>% 
  arrange(-area_type) %>% 
  select(acres_affected, area_type) 
         
    acres_num = str_replace_all(acres_affected, ",", ""),
         acres_num = as.numeric(acres_num)) %>% 
  select(acres_num)



# clean up action types ----

# I think I'd like to explore something about actions taken
# but the action data is kind of a mess
table(dat$action)

# I picked out a few most common general actions
common_actions = c("Enlarged", "Established", "Incorporated", "Redesignated",
                   "Transferred", "Diminished")

# this function will find those words in the action column
classify_action = function(x){
  num = NA
  if(length(which(str_detect(x, common_actions))) > 0){
    num = which(str_detect(x, common_actions))
  }
  type = ifelse(is.na(num), NA, common_actions[num])
  return(type)
}

# use the above function with map to classify actions
# I used map_chr because I know I'm expecting character data
# otherwise map will return a list than will require an additional step to make it usable
dat %>% 
  mutate(action_type = map_chr(action, classify_action)) -> dat_clean

table(dat_clean$action_type)

# how many are still unclassified?
length(which(is.na(dat_clean$action_type)))
table(dat_clean$action[is.na(dat_clean$action_type)])

# I'm just going to leave those NA for now


# political parties ----

# I'm interested in the types of actions taken when controlled by each party
# I couldn't find a csv, but I found this table online: 
# https://www.infoplease.com/history-and-government/us-government/composition-congress-political-party-1855-2017
# and can use the amazing datapasta package to turn it into usable data!
# (all of the praise hands emojis)
# sidenote: tribble is a row-wise way to create a tibble, just makes it easier for humans to read.

parties = tibble::tribble(
            ~Congress,       ~Years, ~Total, ~Dems, ~Reps, ~Others, ~Vacant, ~Total.1, ~Dems.1, ~Reps.1, ~Others.1, ~Vacant.1,
               "34th",  "1855-1857",    62,   42L,   15L,     "5",     "-",     234L,     83L,    108L,      "43",       "-",
               "35th",  "1857-1859",    64L,   39L,   20L,     "5",     "-",     237L,    131L,     92L,      "14",       "-",
               "36th",  "1859-1861",    66L,   38L,   26L,     "2",     "-",     237L,    101L,    113L,      "23",       "-",
               "37th",  "1861-1863",    50L,   11L,   31L,     "7",     "1",     178L,     42L,    106L,      "28",       "2",
               "38th",  "1863-1865",    51L,   12L,   39L,     "-",     "-",     183L,     80L,    103L,       "-",       "-",
               "39th",  "1865-1867",    52L,   10L,   42L,     "-",     "-",     191L,     46L,    145L,       "-",       "-",
               "40th",  "1867-1869",    53L,   11L,   42L,     "-",     "-",     193L,     49L,    143L,       "-",       "1",
               "41st",  "1869-1871",    74L,   11L,   61L,     "-",     "2",     243L,     73L,    170L,       "-",       "-",
               "42nd",  "1871-1873",    74L,   17L,   57L,     "-",     "-",     243L,    104L,    139L,       "-",       "-",
               "43rd",  "1873-1875",    74L,   19L,   54L,     "-",     "1",     293L,     88L,    203L,       "-",       "2",
               "44th",  "1875-1877",    76L,   29L,   46L,     "-",     "1",     293L,    181L,    107L,       "3",       "2",
               "45th",  "1877-1879",    76L,   36L,   39L,     "1",     "-",     293L,    156L,    137L,       "-",       "-",
               "46th",  "1879-1881",    76L,   43L,   33L,     "-",     "-",     293L,    150L,    128L,      "14",       "1",
               "47th",  "1881-1883",    76L,   37L,   37L,     "2",     "-",     293L,    130L,    152L,      "11",       "-",
               "48th",  "1883-1885",    76L,   36L,   40L,     "-",     "-",     325L,    200L,    119L,       "6",       "-",
               "49th",  "1885-1887",    76L,   34L,   41L,     "-",     "1",     325L,    182L,    140L,       "2",       "1",
               "50th",  "1887-1889",    76L,   37L,   39L,     "-",     "-",     325L,    170L,    151L,       "4",       "-",
               "51st",  "1889-1891",    84L,   37L,   47L,     "-",     "-",     330L,    156L,    173L,       "1",       "-",
               "52nd",  "1891-1893",    88L,   39L,   47L,     "2",     "-",     333L,    231L,     88L,      "14",       "-",
               "53rd",  "1893-1895",    88L,   44L,   38L,     "3",     "3",     356L,    220L,    126L,      "10",       "-",
               "54th",  "1895-1897",    88L,   39L,   44L,     "5",     "-",     357L,    104L,    246L,       "7",       "-",
               "55th",  "1897-1899",    90L,   34L,   46L,    "10",     "-",     357L,    134L,    206L,      "16",       "1",
               "56th",  "1899-1901",    90L,   26L,   53L,    "11",     "-",     357L,    163L,    185L,       "9",       "-",
               "57th",  "1901-1903",    90L,   29L,   56L,     "3",     "2",     357L,    153L,    198L,       "5",       "1",
               "58th",  "1903-1905",    90L,   32L,   58L,     "-",     "-",     386L,    178L,    207L,       "-",       "1",
               "59th",  "1905-1907",    90L,   32L,   58L,     "-",     "-",     386L,    136L,    250L,       "-",       "-",
               "60th",  "1907-1909",    92L,   29L,   61L,     "-",     "2",     386L,    164L,    222L,       "-",       "-",
               "61st",  "1909-1911",    92L,   32L,   59L,     "-",     "1",     391L,    172L,    219L,       "-",       "-",
               "62nd",  "1911-1913",    92L,   42L,   49L,     "-",     "1",     391L,    228L,    162L,       "1",       "-",
               "63rd",  "1913-1915",    96L,   51L,   44L,     "1",     "-",     435L,    290L,    127L,      "18",       "-",
               "64th",  "1915-1917",    96L,   56L,   39L,     "1",     "-",     435L,    231L,    193L,       "8",       "3",
               "65th",  "1917-1919",    96L,   53L,   42L,     "1",     "-",     435L,   2101L,    216L,       "9",       "-",
               "66th",  "1919-1921",    96L,   47L,   48L,     "1",     "-",     435L,    191L,    237L,       "7",       "-",
               "67th",  "1921-1923",    96L,   37L,   59L,     "-",     "-",     435L,    132L,    300L,       "1",       "2",
               "68th",  "1923-1925",    96L,   43L,   51L,     "2",     "-",     435L,    207L,    225L,       "3",       "-",
               "69th",  "1925-1927",    96L,   40L,   54L,     "1",     "1",     435L,    183L,    247L,       "5",       "-",
               "70th",  "1927-1929",    96L,   47L,   48L,     "1",     "-",     435L,    195L,    237L,       "3",       "-",
               "71st",  "1929-1931",    96L,   39L,   56L,     "1",     "-",     435L,    163L,    267L,       "1",       "4",
               "72nd",  "1931-1933",    96L,   47L,   48L,     "1",     "-",     435L,   2162L,    218L,       "1",       "-",
               "73rd",  "1933-1935",    96L,   59L,   36L,     "1",     "-",     435L,    313L,    117L,       "5",       "-",
               "74th",  "1935-1937",    96L,   69L,   25L,     "2",     "-",     435L,    322L,    103L,      "10",       "-",
               "75th",  "1937-1939",    96L,   75L,   17L,     "4",     "-",     435L,    333L,     89L,      "13",       "-",
               "76th",  "1939-1941",    96L,   69L,   23L,     "4",     "-",     435L,    262L,    169L,       "4",       "-",
               "77th",  "1941-1943",    96L,   66L,   28L,     "2",     "-",     435L,    267L,    162L,       "6",       "-",
               "78th",  "1943-1945",    96L,   57L,   38L,     "1",     "-",     435L,    222L,    209L,       "4",       "-",
               "79th",  "1945-1947",    96L,   57L,   38L,     "1",     "-",     435L,    243L,    190L,       "2",       "-",
               "80th",  "1947-1949",    96L,   45L,   51L,     "-",     "-",     435L,    188L,    246L,       "1",       "-",
               "81st",  "1949-1951",    96L,   54L,   42L,     "-",     "-",     435L,    263L,    171L,       "1",       "-",
               "82nd",  "1951-1953",    96L,   48L,   47L,     "1",     "-",     435L,    234L,    199L,       "2",       "-",
               "83rd",  "1953-1955",    96L,   46L,   48L,     "2",     "-",     435L,    213L,    221L,       "1",       "-",
               "84th",  "1955-1957",    96L,   48L,   47L,     "1",     "-",     435L,    232L,    203L,       "-",       "-",
               "85th",  "1957-1959",    96L,   49L,   47L,     "-",     "-",     435L,    234L,    201L,       "-",       "-",
               "86th",  "1959-1961",    98L,   64L,   34L,     "-",     "-",    4363L,    283L,    153L,       "-",       "-",
               "87th",  "1961-1963",   100L,   64L,   36L,     "-",     "-",    4374L,    262L,    175L,       "-",       "-",
               "88th",  "1963-1965",   100L,   67L,   33L,     "-",     "-",     435L,    258L,    176L,       "-",       "1",
               "89th",  "1965-1967",   100L,   68L,   32L,     "-",     "-",     435L,    295L,    140L,       "-",       "-",
               "90th",  "1967-1969",   100L,   64L,   36L,     "-",     "-",     435L,    248L,    187L,       "-",       "-",
               "91st",  "1969-1971",   100L,   58L,   42L,     "-",     "-",     435L,    243L,    192L,       "-",       "-",
               "92nd",  "1971-1973",   100L,   54L,   44L,     "2",     "-",     435L,    255L,    180L,       "-",       "-",
               "93rd",  "1973-1975",   100L,   56L,   42L,     "2",     "-",     435L,    242L,    192L,       "1",       "-",
               "94th",  "1975-1977",   100L,   61L,   37L,     "2",     "-",     435L,    291L,    144L,       "-",       "-",
               "95th",  "1977-1979",   100L,   61L,   38L,     "1",     "-",     435L,    292L,    143L,       "-",       "-",
               "96th",  "1979-1981",   100L,   58L,   41L,     "1",     "-",     435L,    277L,    158L,       "-",       "-",
               "97th",  "1981-1983",   100L,   46L,   53L,     "1",     "-",     435L,    242L,    192L,       "1",       "-",
               "98th",  "1983-1985",   100L,   46L,   54L,     "-",     "-",     435L,    269L,    166L,       "-",       "-",
               "99th",  "1985-1987",   100L,   47L,   53L,     "-",     "-",     435L,    253L,    182L,       "-",       "-",
              "100th",  "1987-1989",   100L,   55L,   45L,     "-",     "-",     435L,    258L,    177L,       "-",       "-",
              "101st",  "1989-1991",   100L,   55L,   45L,     "-",     "-",     435L,    260L,    175L,       "-",       "-",
              "102nd",  "1991-1993",   100L,   56L,   44L,     "-",     "-",     435L,    267L,    167L,       "1",       "-",
              "103rd",  "1993-1995",   100L,   57L,   43L,     "-",     "-",     435L,    258L,    176L,       "1",       "-",
              "104th",  "1995-1997",   100L,   48L,   52L,     "-",     "-",     435L,    204L,    230L,       "1",       "-",
              "105th",  "1997-1999",   100L,   45L,   55L,     "-",     "-",     435L,    207L,    226L,       "2",       "-",
              "106th",  "1999-2001",   100L,   45L,   55L,     "-",     "-",     435L,    211L,    223L,       "1",       "-",
              "107th",  "2001-2003",   100L,   50L,   50L,     "-",     "-",     435L,    212L,    221L,       "2",       "-",
              "108th",  "2003-2005",   100L,   48L,   51L,     "1",     "-",     435L,    205L,    229L,       "1",       "-",
              "109th",  "2005-2007",   100L,   44L,   55L,     "1",     "-",     435L,    202L,    231L,       "1",       "1",
              "110th",  "2007-2009",   100L,   49L,   49L,     "2",     "-",     435L,    233L,    198L,       "-",       "4",
              "111th",  "2009-2011",   100L,   57L,   41L,     "2",     "2",     435L,    256L,    178L,       "-",       "1",
              "112th",  "2011-2013",   100L,   51L,   47L,     "2",     "—",     435L,    193L,    242L,       "-",       "-",
              "113th",  "2013-2015",   100L,   54L,   45L,     "1",     "—",     435L,    201L,    234L,       "—",       "-",
              "114th",  "2015–2017",   100L,   44L,   54L,     "2",     "—",     435L,    188L,    246L,       "—",       "1"
            )


# it's not perfect, and thought that some of the numeric data were characters because of the dashes
# need to clean that up and then reorganize a little
parties %>% 
  mutate(Others = as.integer(Others),
         Vacant = as.integer(Vacant)) %>% 
  select(1:7) %>% 
  mutate(chamber = "senate") -> senate

parties %>% 
  mutate(Others = as.integer(Others),
         Vacant = as.integer(Vacant)) %>% 
  select(1:7) %>% 
  mutate(chamber = "house") %>% 
  full_join(senate) -> parties_clean
parties_clean


# now I want to summarize so that for each congress I have the majority party
parties_clean %>% 
  group_by(Congress, Years) %>% 
  summarize(prop_dem = sum(Dems)/sum(Total),
            prop_rep = sum(Reps)/sum(Total),
            prop_other = sum(Others, na.rm = T)/sum(Total)) %>% 
  mutate(year = as.numeric(substr(Years, 1, 4))) %>% 
  gather(type, prop, 3:5)  %>% 
  ggplot(aes(x = year, y = prop, col = type, group = type)) +
  geom_line(lwd = 2, alpha = 0.7) +
  scale_color_manual(values = c("dodgerblue4", 
                                "palegreen4",
                                "red3"),
                     labels = c("Democrat",
                               "Other",
                               "Republican"),
                     name = "Party") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  xlab("First year of session") +
  ylab("Proportion of seats")
  

# so that was a bit of a detour into how control of congress has changed over time
# back to the point:
parties_clean %>% 
  group_by(Congress, Years) %>% 
  summarize(prop_dem = sum(Dems)/sum(Total),
            prop_rep = sum(Reps)/sum(Total),
            prop_other = sum(Others, na.rm = T)/sum(Total)) %>% 
  mutate(congress_party = ifelse(prop_dem > prop_rep, "dem", "rep")) -> parties_sum
parties_sum  

# and party info for presidents, from here:
# https://gist.github.com/namuol/2657233
# also pasted with datapasta

pres = tibble::tribble(
         ~Presidency,                     ~President,                                       ~Wikipedia.Entry,  ~Took.office,  ~Left.office,                                       ~Party,                   ~Portrait,                       ~Thumbnail,      ~Home.State,
                  1L,            "George Washington",       "http://en.wikipedia.org/wiki/George_Washington",  "30/04/1789",   "4/03/1797",                                "Independent",      "GeorgeWashington.jpg",      "thmb_GeorgeWashington.jpg",       "Virginia",
                  2L,                   "John Adams",              "http://en.wikipedia.org/wiki/John_Adams",   "4/03/1797",   "4/03/1801",                                 "Federalist",             "JohnAdams.jpg",             "thmb_JohnAdams.jpg",  "Massachusetts",
                  3L,             "Thomas Jefferson",        "http://en.wikipedia.org/wiki/Thomas_Jefferson",   "4/03/1801",   "4/03/1809",                      "Democratic-Republican",       "Thomasjefferson.gif",       "thmb_Thomasjefferson.gif",       "Virginia",
                  4L,                "James Madison",           "http://en.wikipedia.org/wiki/James_Madison",   "4/03/1809",   "4/03/1817",                      "Democratic-Republican",          "JamesMadison.gif",          "thmb_JamesMadison.gif",       "Virginia",
                  5L,                 "James Monroe",            "http://en.wikipedia.org/wiki/James_Monroe",   "4/03/1817",   "4/03/1825",                      "Democratic-Republican",           "JamesMonroe.gif",           "thmb_JamesMonroe.gif",       "Virginia",
                  6L,            "John Quincy Adams",       "http://en.wikipedia.org/wiki/John_Quincy_Adams",   "4/03/1825",   "4/03/1829",  "Democratic-Republican/National Republican",       "JohnQuincyAdams.gif",       "thmb_JohnQuincyAdams.gif",  "Massachusetts",
                  7L,               "Andrew Jackson",          "http://en.wikipedia.org/wiki/Andrew_Jackson",   "4/03/1829",   "4/03/1837",                                 "Democratic",   "Andrew_jackson_head.gif",   "thmb_Andrew_jackson_head.gif",      "Tennessee",
                  8L,             "Martin Van Buren",        "http://en.wikipedia.org/wiki/Martin_Van_Buren",   "4/03/1837",   "4/03/1841",                                 "Democratic",        "MartinVanBuren.gif",        "thmb_MartinVanBuren.gif",       "New York",
                  9L,       "William Henry Harrison",  "http://en.wikipedia.org/wiki/William_Henry_Harrison",   "4/03/1841",   "4/04/1841",                                       "Whig",  "WilliamHenryHarrison.gif",  "thmb_WilliamHenryHarrison.gif",           "Ohio",
                 10L,                   "John Tyler",              "http://en.wikipedia.org/wiki/John_Tyler",   "4/04/1841",   "4/03/1845",                                       "Whig",             "JohnTyler.jpg",             "thmb_JohnTyler.jpg",       "Virginia",
                 11L,                "James K. Polk",           "http://en.wikipedia.org/wiki/James_K._Polk",   "4/03/1845",   "4/03/1849",                                 "Democratic",            "JamesKPolk.gif",            "thmb_JamesKPolk.gif",      "Tennessee",
                 12L,               "Zachary Taylor",          "http://en.wikipedia.org/wiki/Zachary_Taylor",   "4/03/1849",   "9/07/1850",                                       "Whig",         "ZacharyTaylor.jpg",         "thmb_ZacharyTaylor.jpg",      "Louisiana",
                 13L,             "Millard Fillmore",        "http://en.wikipedia.org/wiki/Millard_Fillmore",   "9/07/1850",   "4/03/1853",                                       "Whig",       "MillardFillmore.png",       "thmb_MillardFillmore.png",       "New York",
                 14L,              "Franklin Pierce",         "http://en.wikipedia.org/wiki/Franklin_Pierce",   "4/03/1853",   "4/03/1857",                                 "Democratic",        "FranklinPierce.gif",        "thmb_FranklinPierce.gif",  "New Hampshire",
                 15L,               "James Buchanan",          "http://en.wikipedia.org/wiki/James_Buchanan",   "4/03/1857",   "4/03/1861",                                 "Democratic",         "JamesBuchanan.gif",         "thmb_JamesBuchanan.gif",   "Pennsylvania",
                 16L,              "Abraham Lincoln",         "http://en.wikipedia.org/wiki/Abraham_Lincoln",   "4/03/1861",  "15/04/1865",                  "Republican/National Union",        "AbrahamLincoln.jpg",        "thmb_AbrahamLincoln.jpg",       "Illinois",
                 17L,               "Andrew Johnson",          "http://en.wikipedia.org/wiki/Andrew_Johnson",  "15/04/1865",   "4/03/1869",                  "Democratic/National Union",         "AndrewJohnson.gif",         "thmb_AndrewJohnson.gif",      "Tennessee",
                 18L,             "Ulysses S. Grant",        "http://en.wikipedia.org/wiki/Ulysses_S._Grant",   "4/03/1869",   "4/03/1877",                                 "Republican",         "UlyssesSGrant.gif",         "thmb_UlyssesSGrant.gif",           "Ohio",
                 19L,          "Rutherford B. Hayes",     "http://en.wikipedia.org/wiki/Rutherford_B._Hayes",   "4/03/1877",   "4/03/1881",                                 "Republican",      "RutherfordBHayes.png",      "thmb_RutherfordBHayes.png",           "Ohio",
                 20L,            "James A. Garfield",       "http://en.wikipedia.org/wiki/James_A._Garfield",   "4/03/1881",  "19/09/1881",                                 "Republican",        "James_Garfield.jpg",        "thmb_James_Garfield.jpg",           "Ohio",
                 21L,            "Chester A. Arthur",       "http://en.wikipedia.org/wiki/Chester_A._Arthur",  "19/09/1881",   "4/03/1885",                                 "Republican",        "ChesterAArthur.gif",        "thmb_ChesterAArthur.gif",       "New York",
                 22L,             "Grover Cleveland",        "http://en.wikipedia.org/wiki/Grover_Cleveland",   "4/03/1885",   "4/03/1889",                                 "Democratic",    "Grover_Cleveland_2.jpg",    "thmb_Grover_Cleveland_2.jpg",       "New York",
                 23L,            "Benjamin Harrison",       "http://en.wikipedia.org/wiki/Benjamin_Harrison",   "4/03/1889",   "4/03/1893",                                 "Republican",      "BenjaminHarrison.gif",      "thmb_BenjaminHarrison.gif",        "Indiana",
                 24L,  "Grover Cleveland (2nd term)",        "http://en.wikipedia.org/wiki/Grover_Cleveland",   "4/03/1893",   "4/03/1897",                                 "Democratic",      "Grover_Cleveland.jpg",      "thmb_Grover_Cleveland.jpg",       "New York",
                 25L,             "William McKinley",        "http://en.wikipedia.org/wiki/William_McKinley",   "4/03/1897",   "14/9/1901",                                 "Republican",       "WilliamMcKinley.gif",       "thmb_WilliamMcKinley.gif",           "Ohio",
                 26L,           "Theodore Roosevelt",      "http://en.wikipedia.org/wiki/Theodore_Roosevelt",   "14/9/1901",    "4/3/1909",                                 "Republican",     "TheodoreRoosevelt.jpg",     "thmb_TheodoreRoosevelt.jpg",       "New York",
                 27L,          "William Howard Taft",     "http://en.wikipedia.org/wiki/William_Howard_Taft",    "4/3/1909",   "4/03/1913",                                 "Republican",     "WilliamHowardTaft.jpg",     "thmb_WilliamHowardTaft.jpg",           "Ohio",
                 28L,               "Woodrow Wilson",          "http://en.wikipedia.org/wiki/Woodrow_Wilson",   "4/03/1913",   "4/03/1921",                                 "Democratic",         "WoodrowWilson.gif",         "thmb_WoodrowWilson.gif",     "New Jersey",
                 29L,            "Warren G. Harding",       "http://en.wikipedia.org/wiki/Warren_G._Harding",   "4/03/1921",    "2/8/1923",                                 "Republican",        "WarrenGHarding.gif",        "thmb_WarrenGHarding.gif",           "Ohio",
                 30L,              "Calvin Coolidge",         "http://en.wikipedia.org/wiki/Calvin_Coolidge",    "2/8/1923",   "4/03/1929",                                 "Republican",    "CoolidgeWHPortrait.gif",    "thmb_CoolidgeWHPortrait.gif",  "Massachusetts",
                 31L,               "Herbert Hoover",          "http://en.wikipedia.org/wiki/Herbert_Hoover",   "4/03/1929",   "4/03/1933",                                 "Republican",          "HerbertHover.gif",          "thmb_HerbertHover.gif",           "Iowa",
                 32L,        "Franklin D. Roosevelt",   "http://en.wikipedia.org/wiki/Franklin_D._Roosevelt",   "4/03/1933",   "12/4/1945",                                 "Democratic",    "FranklinDRoosevelt.gif",    "thmb_FranklinDRoosevelt.gif",       "New York",
                 33L,              "Harry S. Truman",         "http://en.wikipedia.org/wiki/Harry_S._Truman",   "12/4/1945",  "20/01/1953",                                 "Democratic",           "HarryTruman.jpg",           "thmb_HarryTruman.jpg",       "Missouri",
                 34L,         "Dwight D. Eisenhower",    "http://en.wikipedia.org/wiki/Dwight_D._Eisenhower",  "20/01/1953",  "20/01/1961",                                 "Republican",   "Dwight_D_Eisenhower.jpg",   "thmb_Dwight_D_Eisenhower.jpg",          "Texas",
                 35L,              "John F. Kennedy",         "http://en.wikipedia.org/wiki/John_F._Kennedy",  "20/01/1961",  "22/11/1963",                                 "Democratic",        "John_F_Kennedy.jpg",        "thmb_John_F_Kennedy.jpg",  "Massachusetts",
                 36L,            "Lyndon B. Johnson",       "http://en.wikipedia.org/wiki/Lyndon_B._Johnson",  "22/11/1963",   "20/1/1969",                                 "Democratic",      "Lyndon_B_Johnson.gif",      "thmb_Lyndon_B_Johnson.gif",          "Texas",
                 37L,                "Richard Nixon",           "http://en.wikipedia.org/wiki/Richard_Nixon",   "20/1/1969",    "9/8/1974",                                 "Republican",          "RichardNixon.gif",          "thmb_RichardNixon.gif",     "California",
                 38L,                  "Gerald Ford",             "http://en.wikipedia.org/wiki/Gerald_Ford",    "9/8/1974",  "20/01/1977",                                 "Republican",         "Gerald_R_Ford.jpg",         "thmb_Gerald_R_Ford.jpg",       "Michigan",
                 39L,                 "Jimmy Carter",            "http://en.wikipedia.org/wiki/Jimmy_Carter",  "20/01/1977",  "20/01/1981",                                 "Democratic",        "James_E_Carter.gif",        "thmb_James_E_Carter.gif",        "Georgia",
                 40L,                "Ronald Reagan",           "http://en.wikipedia.org/wiki/Ronald_Reagan",  "20/01/1981",  "20/01/1989",                                 "Republican",              "ReaganWH.jpg",              "thmb_ReaganWH.jpg",     "California",
                 41L,            "George H. W. Bush",       "http://en.wikipedia.org/wiki/George_H._W._Bush",  "20/01/1989",  "20/01/1993",                                 "Republican",       "George_H_W_Bush.gif",       "thmb_George_H_W_Bush.gif",          "Texas",
                 42L,                 "Bill Clinton",            "http://en.wikipedia.org/wiki/Bill_Clinton",  "20/01/1993",  "20/01/2001",                                 "Democratic",               "Clinton.jpg",               "thmb_Clinton.jpg",       "Arkansas",
                 43L,               "George W. Bush",          "http://en.wikipedia.org/wiki/George_W._Bush",  "20/01/2001",  "20/01/2009",                                 "Republican",         "George_W_Bush.jpg",         "thmb_George_W_Bush.jpg",          "Texas",
                 44L,                 "Barack Obama",            "http://en.wikipedia.org/wiki/Barack_Obama",  "20/01/2009",   "Incumbent",                                 "Democratic",          "Barack_Obama.jpg",          "thmb_Barack_Obama.jpg",       "Illinois"
         )

# again I'm using a function with map to extract the last name
extract_last_name = function(x){
  spaces = str_locate_all(x, "\\s")
  start = spaces[[1]][max(nrow(spaces[[1]])),2]+1
  end = nchar(x)
  substr(x, start, end)
}

# from looking at the data I see that the Roosevelts are the only ones we need to worry about
pres %>% 
  select(President, Took.office, Left.office, Party) %>% 
  mutate(first.initial = substr(President, 1, 1),
         last.name = map_chr(President, extract_last_name),
         last.name = ifelse(President == "Theodore Roosevelt", 
                            "T. Roosevelt", last.name),
         pres_party = ifelse(Party == "Democratic", "dem",
                        ifelse(Party == "Republican", "rep", "other"))) %>% 
  select(last.name, pres_party) -> pres_clean
pres_clean 

# now to join that up with the antiquities act data
dat_clean %>% 
  mutate(congress = ifelse(str_detect(pres_or_congress, "Congress"),
                           gsub(" ", "", substr(pres_or_congress, 1, 5)), NA),
         pres = ifelse(str_detect(pres_or_congress, "Congress"), NA,
                                  map_chr(pres_or_congress, extract_last_name)),
         pres = ifelse(pres_or_congress == "T. Roosevelt", "T. Roosevelt", pres)) %>% 
  full_join(pres_clean, by = c("pres" = "last.name")) %>% 
  full_join(parties_sum[,c(1,6)], by = c("congress" = "Congress")) %>% 
  mutate(party = ifelse(is.na(congress_party), pres_party, 
                              congress_party)) %>% 
  select(-pres_or_congress, -pres_party, -congress_party) -> dat_all
dat_all  


# change in total acres protected over time? ----
dat_all %>% 
  mutate(acres_affected = as.numeric(acres_affected),
         acre_change = ifelse(action_type == "Diminished", -1*acres_affected,
                              ifelse(action_type %in% c("Redesignated", "Transferred"), 0, acres_affected))) %>% 
  group_by(year, party) %>% 
  summarize(yr_change = sum(acre_change, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(year) %>% 
  mutate(total_acres = cumsum(yr_change)) %>% 
  ggplot(aes(x = year, y = total_acres)) +
  geom_line(lwd = 2, alpha = 0.7, col = "gray80") +
  geom_point(aes(col = party), size = 3, alpha = 0.9) +
  xlab("Year") +
  ylab("Total acres protected")


# change within years, not cumulative ----
dat_all %>% 
  mutate(acres_affected = as.numeric(acres_affected),
         acre_change = ifelse(action_type == "Diminished", -1*acres_affected,
                              ifelse(action_type %in% c("Redesignated", "Transferred"), 0, acres_affected))) %>% 
  group_by(year, party) %>% 
  summarize(yr_change = sum(acre_change, na.rm = T)) %>% 
  filter(!is.na(year)) %>%
  ggplot(aes(x = as.character(year), y = yr_change)) +
  geom_linerange(lwd = 2, alpha = 0.25, ymin = 0, 
                 aes(ymax = yr_change, col = party)) +
  geom_point(aes(col = party), size = 3, alpha = 0.9) +
  xlab("Year") +
  ylab("Change in protected area (acres)") +
  scale_color_manual(values = c("dodgerblue4", 
                                "palegreen4",
                                "red3"),
                     labels = c("Democrat",
                                "Other",
                                "Republican"),
                     name = "Party in power") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90, hjust = 1,
                                   size = 10)) 


# net change by members of each party? ----
dat_all %>% 
  mutate(acres_affected = as.numeric(acres_affected),
         acre_change = ifelse(action_type == "Diminished", -1*acres_affected,
                              ifelse(action_type %in% c("Redesignated", "Transferred"), 0, acres_affected))) %>% 
  filter(party %in% c("dem", "rep")) %>% 
  mutate(party = c("dem" = "Democrats", "rep" = "Republicans")[party]) %>% 
  group_by(party) %>% 
  summarize(tot_acres = sum(acre_change, na.rm = T)) %>% 
  ggplot(aes(x = party, y = tot_acres)) +
  geom_bar(stat= "identity", width = 0.5, fill = "dodgerblue4", alpha = 0.6) +
  xlab("Political party in power") +
  ylab("Net acres added") 


# which decade 

  
  
