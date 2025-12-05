data clean
================
Huiyi Zhu
2025-12-05

## prepare

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

## data clean

``` r
parkevent = read.csv("Parks_Special_Events_20251204.csv") |>
  janitor::clean_names() |>
  mutate(
    datetime = mdy_hms(date_and_time),
    year  = year(datetime),
    month = month(datetime, label = TRUE, abbr = TRUE),
    day = day(datetime),
    time = format(datetime, "%H:%M:%S"),
    season = case_when(
      month %in% c("Mar","Apr","May") ~ "Spring",
      month %in% c("Jun","Jul","Aug") ~ "Summer",
      month %in% c("Sep","Oct","Nov") ~ "Fall",
      month %in% c("Dec","Jan","Feb") ~ "Winter")) |> 
  rename(type = event_type) |> 
  select(
    -unit,
    -group_name_partner,
    -date_and_time,
    -location_type,
    -event_name,
    -classification,
    -source) |> 
  select(
    datetime, year, month, day, time, season,
    everything())
```

\#check

``` r
parkevent |> 
  count(audience)
```

    ##                                                                         audience
    ## 1                                                                               
    ## 2                                                                       Adaptive
    ## 3                                                              Adaptive, Seniors
    ## 4                  Adaptive, Seniors, General Public, Adults, Young Adult, Teens
    ## 5        Adaptive, Teens, Adults, Young Adult, General Public, Seniors, Children
    ## 6                  Adaptive;#Seniors;#General Public;#Adults;#Young Adult;#Teens
    ## 7        Adaptive;#Teens;#Adults;#Young Adult;#General Public;#Seniors;#Children
    ## 8                                                                         Adults
    ## 9                                                               Adults, Adaptive
    ## 10                                                              Adults, Children
    ## 11                                              Adults, Children, General Public
    ## 12                                                     Adults, Children, Seniors
    ## 13                                                       Adults, Children, Teens
    ## 14                                       Adults, Children, Teens, General Public
    ## 15                            Adults, Children, Teens, Young Adult, Tot, Seniors
    ## 16                                         Adults, Children, Tot, General Public
    ## 17                                                 Adults, Children, Young Adult
    ## 18                                          Adults, Children, Young Adult, Teens
    ## 19                                                        Adults, General Public
    ## 20                                               Adults, General Public, Seniors
    ## 21                                  Adults, General Public, Seniors, Young Adult
    ## 22                                           Adults, General Public, Young Adult
    ## 23                                                               Adults, Seniors
    ## 24                                                     Adults, Seniors, Adaptive
    ## 25                                               Adults, Seniors, General Public
    ## 26                                     Adults, Seniors, General Public, Adaptive
    ## 27                                     Adults, Seniors, General Public, Children
    ## 28                        Adults, Seniors, General Public, Young Adult, Adaptive
    ## 29                                                        Adults, Seniors, Teens
    ## 30                                                  Adults, Seniors, Young Adult
    ## 31                                                                 Adults, Teens
    ## 32                                              Adults, Teens, Adaptive, Seniors
    ## 33                              Adults, Teens, Adaptive, Seniors, General Public
    ## 34                 Adults, Teens, Adaptive, Seniors, General Public, Young Adult
    ## 35                                                       Adults, Teens, Children
    ## 36                                                 Adults, Teens, General Public
    ## 37  Adults, Teens, General Public, Children, Young Adult, Tot, Seniors, Adaptive
    ## 38                                        Adults, Teens, General Public, Seniors
    ## 39                                                        Adults, Teens, Seniors
    ## 40                                        Adults, Teens, Seniors, General Public
    ## 41                         Adults, Teens, Seniors, General Public, Tot, Children
    ## 42                                     Adults, Teens, Tot, Children, Young Adult
    ## 43                                                    Adults, Teens, Young Adult
    ## 44                                          Adults, Teens, Young Adult, Children
    ## 45                                    Adults, Teens, Young Adult, General Public
    ## 46                           Adults, Teens, Young Adult, Seniors, General Public
    ## 47                                                                   Adults, Tot
    ## 48                                                           Adults, Young Adult
    ## 49       Adults, Young Adult, Children, Teens, Adaptive, General Public, Seniors
    ## 50                          Adults, Young Adult, Children, Teens, General Public
    ## 51                                           Adults, Young Adult, General Public
    ## 52                                 Adults, Young Adult, General Public, Children
    ## 53                                                  Adults, Young Adult, Seniors
    ## 54                                        Adults, Young Adult, Seniors, Adaptive
    ## 55                                  Adults, Young Adult, Seniors, General Public
    ## 56                 Adults, Young Adult, Seniors, General Public, Adaptive, Teens
    ## 57                                           Adults, Young Adult, Seniors, Teens
    ## 58                                          Adults, Young Adult, Teens, Children
    ## 59                 Adults, Young Adult, Teens, Children, General Public, Seniors
    ## 60                                    Adults, Young Adult, Teens, General Public
    ## 61                                           Adults, Young Adult, Teens, Seniors
    ## 62                           Adults, Young Adult, Teens, Seniors, General Public
    ## 63                                                              Adults;#Children
    ## 64                                              Adults;#Children;#General Public
    ## 65                                                     Adults;#Children;#Seniors
    ## 66                                                       Adults;#Children;#Teens
    ## 67                                       Adults;#Children;#Teens;#General Public
    ## 68                            Adults;#Children;#Teens;#Young Adult;#Tot;#Seniors
    ## 69                                         Adults;#Children;#Tot;#General Public
    ## 70                                          Adults;#Children;#Young Adult;#Teens
    ## 71                                                        Adults;#General Public
    ## 72                                               Adults;#General Public;#Seniors
    ## 73                                  Adults;#General Public;#Seniors;#Young Adult
    ## 74                                           Adults;#General Public;#Young Adult
    ## 75                                                               Adults;#Seniors
    ## 76                                                     Adults;#Seniors;#Adaptive
    ## 77                                               Adults;#Seniors;#General Public
    ## 78                                     Adults;#Seniors;#General Public;#Adaptive
    ## 79                                     Adults;#Seniors;#General Public;#Children
    ## 80                        Adults;#Seniors;#General Public;#Young Adult;#Adaptive
    ## 81                                                  Adults;#Seniors;#Young Adult
    ## 82                                                                 Adults;#Teens
    ## 83                                              Adults;#Teens;#Adaptive;#Seniors
    ## 84                              Adults;#Teens;#Adaptive;#Seniors;#General Public
    ## 85                 Adults;#Teens;#Adaptive;#Seniors;#General Public;#Young Adult
    ## 86                                                       Adults;#Teens;#Children
    ## 87                                                 Adults;#Teens;#General Public
    ## 88                                        Adults;#Teens;#General Public;#Seniors
    ## 89                                                        Adults;#Teens;#Seniors
    ## 90                                        Adults;#Teens;#Seniors;#General Public
    ## 91                                     Adults;#Teens;#Tot;#Children;#Young Adult
    ## 92                                                    Adults;#Teens;#Young Adult
    ## 93                                          Adults;#Teens;#Young Adult;#Children
    ## 94                                    Adults;#Teens;#Young Adult;#General Public
    ## 95                                                                   Adults;#Tot
    ## 96                                                           Adults;#Young Adult
    ## 97       Adults;#Young Adult;#Children;#Teens;#Adaptive;#General Public;#Seniors
    ## 98                                           Adults;#Young Adult;#General Public
    ## 99                                 Adults;#Young Adult;#General Public;#Children
    ## 100                                                 Adults;#Young Adult;#Seniors
    ## 101                                       Adults;#Young Adult;#Seniors;#Adaptive
    ## 102                                 Adults;#Young Adult;#Seniors;#General Public
    ## 103                Adults;#Young Adult;#Seniors;#General Public;#Adaptive;#Teens
    ## 104                                          Adults;#Young Adult;#Seniors;#Teens
    ## 105                                         Adults;#Young Adult;#Teens;#Children
    ## 106                Adults;#Young Adult;#Teens;#Children;#General Public;#Seniors
    ## 107                                   Adults;#Young Adult;#Teens;#General Public
    ## 108                                          Adults;#Young Adult;#Teens;#Seniors
    ## 109                                                                     Children
    ## 110                                                           Children, Adaptive
    ## 111                                                             Children, Adults
    ## 112                                             Children, Adults, General Public
    ## 113                                                    Children, Adults, Seniors
    ## 114                                    Children, Adults, Seniors, General Public
    ## 115      Children, Adults, Seniors, Young Adult, Adaptive, Teens, General Public
    ## 116                                                      Children, Adults, Teens
    ## 117                                      Children, Adults, Teens, General Public
    ## 118                             Children, Adults, Teens, Seniors, General Public
    ## 119                                         Children, Adults, Teens, Young Adult
    ## 120                Children, Adults, Teens, Young Adult, Seniors, General Public
    ## 121                                    Children, Adults, Teens, Young Adult, Tot
    ## 122                                                        Children, Adults, Tot
    ## 123                                        Children, Adults, Tot, General Public
    ## 124                                    Children, Adults, Tot, Young Adult, Teens
    ## 125 Children, Adults, Tot, Young Adult, Teens, General Public, Seniors, Adaptive
    ## 126                                                Children, Adults, Young Adult
    ## 127                                         Children, Adults, Young Adult, Teens
    ## 128      Children, Adults, Young Adult, Teens, Adaptive, Seniors, General Public
    ## 129                Children, Adults, Young Adult, Teens, General Public, Seniors
    ## 130      Children, Adults, Young Adult, Teens, General Public, Seniors, Adaptive
    ## 131                                Children, Adults, Young Adult, Teens, Seniors
    ## 132                Children, Adults, Young Adult, Teens, Seniors, General Public
    ## 133                           Children, Adults, Young Adult, Teens, Tot, Seniors
    ## 134                                           Children, Adults, Young Adult, Tot
    ## 135                                                     Children, General Public
    ## 136                                             Children, General Public, Adults
    ## 137           Children, General Public, Seniors, Teens, Adults, Young Adult, Tot
    ## 138                                                Children, General Public, Tot
    ## 139                                                  Children, Seniors, Adaptive
    ## 140                                                              Children, Teens
    ## 141                                                    Children, Teens, Adaptive
    ## 142                                                      Children, Teens, Adults
    ## 143                                                 Children, Teens, Adults, Tot
    ## 144                                              Children, Teens, General Public
    ## 145                                         Children, Teens, General Public, Tot
    ## 146                                                     Children, Teens, Seniors
    ## 147                           Children, Teens, Seniors, Adaptive, General Public
    ## 148                                     Children, Teens, Seniors, General Public
    ## 149                                                         Children, Teens, Tot
    ## 150                                                 Children, Teens, Young Adult
    ## 151                Children, Teens, Young Adult, Adults, General Public, Seniors
    ## 152      Children, Teens, Young Adult, Adults, Seniors, Adaptive, General Public
    ## 153                                 Children, Teens, Young Adult, General Public
    ## 154                                                                Children, Tot
    ## 155                                                      Children, Tot, Adaptive
    ## 156                                                        Children, Tot, Adults
    ## 157                                        Children, Tot, Adults, General Public
    ## 158                               Children, Tot, Adults, Seniors, General Public
    ## 159                                        Children, Tot, Adults, Teens, Seniors
    ## 160                                    Children, Tot, Adults, Teens, Young Adult
    ## 161                                    Children, Tot, Adults, Young Adult, Teens
    ## 162                    Children, Tot, Adults, Young Adult, Teens, General Public
    ## 163 Children, Tot, Adults, Young Adult, Teens, General Public, Adaptive, Seniors
    ## 164                                                Children, Tot, General Public
    ## 165                                        Children, Tot, General Public, Adults
    ## 166                                                         Children, Tot, Teens
    ## 167                                    Children, Tot, Teens, Adults, Young Adult
    ## 168                                         Children, Tot, Teens, General Public
    ## 169                    Children, Tot, Teens, General Public, Young Adult, Adults
    ## 170                                                   Children, Tot, Young Adult
    ## 171                                           Children, Tot, Young Adult, Adults
    ## 172                                    Children, Tot, Young Adult, Adults, Teens
    ## 173 Children, Tot, Young Adult, Adults, Teens, Adaptive, General Public, Seniors
    ## 174 Children, Tot, Young Adult, Adults, Teens, Adaptive, Seniors, General Public
    ## 175                    Children, Tot, Young Adult, Adults, Teens, General Public
    ## 176 Children, Tot, Young Adult, Adults, Teens, Seniors, Adaptive, General Public
    ## 177           Children, Tot, Young Adult, Adults, Teens, Seniors, General Public
    ## 178                                   Children, Tot, Young Adult, General Public
    ## 179                                    Children, Tot, Young Adult, Teens, Adults
    ## 180                                                        Children, Young Adult
    ## 181                                                Children, Young Adult, Adults
    ## 182                                Children, Young Adult, Adults, General Public
    ## 183                         Children, Young Adult, Adults, General Public, Teens
    ## 184                                       Children, Young Adult, Adults, Seniors
    ## 185                       Children, Young Adult, Adults, Seniors, General Public
    ## 186                Children, Young Adult, Adults, Seniors, General Public, Teens
    ## 187                                         Children, Young Adult, Adults, Teens
    ## 188                               Children, Young Adult, Adults, Teens, Adaptive
    ## 189               Children, Young Adult, Adults, Teens, Adaptive, General Public
    ## 190      Children, Young Adult, Adults, Teens, Adaptive, General Public, Seniors
    ## 191                      Children, Young Adult, Adults, Teens, Adaptive, Seniors
    ## 192      Children, Young Adult, Adults, Teens, Adaptive, Seniors, General Public
    ## 193                         Children, Young Adult, Adults, Teens, General Public
    ## 194                Children, Young Adult, Adults, Teens, General Public, Seniors
    ## 195      Children, Young Adult, Adults, Teens, General Public, Seniors, Adaptive
    ## 196                                Children, Young Adult, Adults, Teens, Seniors
    ## 197      Children, Young Adult, Adults, Teens, Seniors, Adaptive, General Public
    ## 198                Children, Young Adult, Adults, Teens, Seniors, General Public
    ## 199      Children, Young Adult, Adults, Teens, Seniors, General Public, Adaptive
    ## 200           Children, Young Adult, Adults, Teens, Seniors, General Public, Tot
    ## 201                                    Children, Young Adult, Adults, Teens, Tot
    ## 202           Children, Young Adult, Adults, Teens, Tot, Seniors, General Public
    ## 203 Children, Young Adult, Adults, Tot, Teens, Adaptive, Seniors, General Public
    ## 204           Children, Young Adult, Adults, Tot, Teens, Seniors, General Public
    ## 205                                        Children, Young Adult, General Public
    ## 206                                                 Children, Young Adult, Teens
    ## 207                                       Children, Young Adult, Teens, Adaptive
    ## 208                       Children, Young Adult, Teens, Adaptive, General Public
    ## 209                                         Children, Young Adult, Teens, Adults
    ## 210      Children, Young Adult, Teens, Adults, Adaptive, General Public, Seniors
    ## 211                         Children, Young Adult, Teens, Adults, General Public
    ## 212                Children, Young Adult, Teens, Adults, Seniors, General Public
    ## 213                                 Children, Young Adult, Teens, General Public
    ## 214                                            Children, Young Adult, Teens, Tot
    ## 215                                                   Children, Young Adult, Tot
    ## 216                                    Children, Young Adult, Tot, Adults, Teens
    ## 217                                            Children, Young Adult, Tot, Teens
    ## 218                                                           Children;#Adaptive
    ## 219                                                             Children;#Adults
    ## 220                                             Children;#Adults;#General Public
    ## 221                                                    Children;#Adults;#Seniors
    ## 222                                    Children;#Adults;#Seniors;#General Public
    ## 223      Children;#Adults;#Seniors;#Young Adult;#Adaptive;#Teens;#General Public
    ## 224                                                      Children;#Adults;#Teens
    ## 225                                      Children;#Adults;#Teens;#General Public
    ## 226                             Children;#Adults;#Teens;#Seniors;#General Public
    ## 227                                         Children;#Adults;#Teens;#Young Adult
    ## 228                Children;#Adults;#Teens;#Young Adult;#Seniors;#General Public
    ## 229                                    Children;#Adults;#Teens;#Young Adult;#Tot
    ## 230                                                        Children;#Adults;#Tot
    ## 231                                        Children;#Adults;#Tot;#General Public
    ## 232                                    Children;#Adults;#Tot;#Young Adult;#Teens
    ## 233                                                Children;#Adults;#Young Adult
    ## 234                                         Children;#Adults;#Young Adult;#Teens
    ## 235      Children;#Adults;#Young Adult;#Teens;#Adaptive;#Seniors;#General Public
    ## 236      Children;#Adults;#Young Adult;#Teens;#General Public;#Seniors;#Adaptive
    ## 237                                Children;#Adults;#Young Adult;#Teens;#Seniors
    ## 238                Children;#Adults;#Young Adult;#Teens;#Seniors;#General Public
    ## 239                           Children;#Adults;#Young Adult;#Teens;#Tot;#Seniors
    ## 240                                           Children;#Adults;#Young Adult;#Tot
    ## 241                                                     Children;#General Public
    ## 242                                             Children;#General Public;#Adults
    ## 243                                                Children;#General Public;#Tot
    ## 244                                                  Children;#Seniors;#Adaptive
    ## 245                                                              Children;#Teens
    ## 246                                                    Children;#Teens;#Adaptive
    ## 247                                                      Children;#Teens;#Adults
    ## 248                                              Children;#Teens;#General Public
    ## 249                                         Children;#Teens;#General Public;#Tot
    ## 250                                                     Children;#Teens;#Seniors
    ## 251                           Children;#Teens;#Seniors;#Adaptive;#General Public
    ## 252                                     Children;#Teens;#Seniors;#General Public
    ## 253                                                         Children;#Teens;#Tot
    ## 254                                                 Children;#Teens;#Young Adult
    ## 255                Children;#Teens;#Young Adult;#Adults;#General Public;#Seniors
    ## 256      Children;#Teens;#Young Adult;#Adults;#Seniors;#Adaptive;#General Public
    ## 257                                 Children;#Teens;#Young Adult;#General Public
    ## 258                                                                Children;#Tot
    ## 259                                                      Children;#Tot;#Adaptive
    ## 260                                                        Children;#Tot;#Adults
    ## 261                                        Children;#Tot;#Adults;#General Public
    ## 262                               Children;#Tot;#Adults;#Seniors;#General Public
    ## 263                                        Children;#Tot;#Adults;#Teens;#Seniors
    ## 264                                    Children;#Tot;#Adults;#Teens;#Young Adult
    ## 265                                    Children;#Tot;#Adults;#Young Adult;#Teens
    ## 266 Children;#Tot;#Adults;#Young Adult;#Teens;#General Public;#Adaptive;#Seniors
    ## 267                                                Children;#Tot;#General Public
    ## 268                                        Children;#Tot;#General Public;#Adults
    ## 269                                                         Children;#Tot;#Teens
    ## 270                                    Children;#Tot;#Teens;#Adults;#Young Adult
    ## 271                                         Children;#Tot;#Teens;#General Public
    ## 272                    Children;#Tot;#Teens;#General Public;#Young Adult;#Adults
    ## 273                                                   Children;#Tot;#Young Adult
    ## 274                                           Children;#Tot;#Young Adult;#Adults
    ## 275                                    Children;#Tot;#Young Adult;#Adults;#Teens
    ## 276 Children;#Tot;#Young Adult;#Adults;#Teens;#Adaptive;#Seniors;#General Public
    ## 277                    Children;#Tot;#Young Adult;#Adults;#Teens;#General Public
    ## 278 Children;#Tot;#Young Adult;#Adults;#Teens;#Seniors;#Adaptive;#General Public
    ## 279           Children;#Tot;#Young Adult;#Adults;#Teens;#Seniors;#General Public
    ## 280                                   Children;#Tot;#Young Adult;#General Public
    ## 281                                    Children;#Tot;#Young Adult;#Teens;#Adults
    ## 282                                                        Children;#Young Adult
    ## 283                                                Children;#Young Adult;#Adults
    ## 284                                Children;#Young Adult;#Adults;#General Public
    ## 285                                       Children;#Young Adult;#Adults;#Seniors
    ## 286                       Children;#Young Adult;#Adults;#Seniors;#General Public
    ## 287                Children;#Young Adult;#Adults;#Seniors;#General Public;#Teens
    ## 288                                         Children;#Young Adult;#Adults;#Teens
    ## 289                               Children;#Young Adult;#Adults;#Teens;#Adaptive
    ## 290               Children;#Young Adult;#Adults;#Teens;#Adaptive;#General Public
    ## 291      Children;#Young Adult;#Adults;#Teens;#Adaptive;#General Public;#Seniors
    ## 292                      Children;#Young Adult;#Adults;#Teens;#Adaptive;#Seniors
    ## 293      Children;#Young Adult;#Adults;#Teens;#Adaptive;#Seniors;#General Public
    ## 294                         Children;#Young Adult;#Adults;#Teens;#General Public
    ## 295                Children;#Young Adult;#Adults;#Teens;#General Public;#Seniors
    ## 296      Children;#Young Adult;#Adults;#Teens;#General Public;#Seniors;#Adaptive
    ## 297      Children;#Young Adult;#Adults;#Teens;#Seniors;#Adaptive;#General Public
    ## 298                Children;#Young Adult;#Adults;#Teens;#Seniors;#General Public
    ## 299      Children;#Young Adult;#Adults;#Teens;#Seniors;#General Public;#Adaptive
    ## 300           Children;#Young Adult;#Adults;#Teens;#Seniors;#General Public;#Tot
    ## 301                                    Children;#Young Adult;#Adults;#Teens;#Tot
    ## 302 Children;#Young Adult;#Adults;#Tot;#Teens;#Adaptive;#Seniors;#General Public
    ## 303           Children;#Young Adult;#Adults;#Tot;#Teens;#Seniors;#General Public
    ## 304                                        Children;#Young Adult;#General Public
    ## 305                                                 Children;#Young Adult;#Teens
    ## 306                       Children;#Young Adult;#Teens;#Adaptive;#General Public
    ## 307                                         Children;#Young Adult;#Teens;#Adults
    ## 308      Children;#Young Adult;#Teens;#Adults;#Adaptive;#General Public;#Seniors
    ## 309                         Children;#Young Adult;#Teens;#Adults;#General Public
    ## 310                Children;#Young Adult;#Teens;#Adults;#Seniors;#General Public
    ## 311                                 Children;#Young Adult;#Teens;#General Public
    ## 312                                            Children;#Young Adult;#Teens;#Tot
    ## 313                                                   Children;#Young Adult;#Tot
    ## 314                                            Children;#Young Adult;#Tot;#Teens
    ## 315                                                               General Public
    ## 316                General Public, Adaptive, Adults, Seniors, Teens, Young Adult
    ## 317                General Public, Adaptive, Adults, Teens, Seniors, Young Adult
    ## 318 General Public, Adaptive, Adults, Teens, Seniors, Young Adult, Tot, Children
    ## 319 General Public, Adaptive, Teens, Seniors, Adults, Young Adult, Children, Tot
    ## 320 General Public, Adaptive, Tot, Children, Young Adult, Adults, Teens, Seniors
    ## 321                                                       General Public, Adults
    ## 322                                             General Public, Adults, Children
    ## 323                                 General Public, Adults, Seniors, Young Adult
    ## 324                             General Public, Adults, Teens, Adaptive, Seniors
    ## 325           General Public, Adults, Teens, Children, Tot, Young Adult, Seniors
    ## 326                                          General Public, Adults, Young Adult
    ## 327                                General Public, Adults, Young Adult, Children
    ## 328                                 General Public, Adults, Young Adult, Seniors
    ## 329                                                     General Public, Children
    ## 330                                             General Public, Children, Adults
    ## 331                                              General Public, Children, Teens
    ## 332                                                General Public, Children, Tot
    ## 333                                        General Public, Children, Tot, Adults
    ## 334                                         General Public, Children, Tot, Teens
    ## 335                                 General Public, Children, Tot, Teens, Adults
    ## 336                                        General Public, Children, Young Adult
    ## 337                General Public, Children, Young Adult, Adults, Teens, Seniors
    ## 338                                                      General Public, Seniors
    ## 339      General Public, Seniors, Adaptive, Teens, Adults, Young Adult, Children
    ## 340 General Public, Seniors, Adaptive, Teens, Adults, Young Adult, Children, Tot
    ## 341                                              General Public, Seniors, Adults
    ## 342                                 General Public, Seniors, Adults, Young Adult
    ## 343                             General Public, Seniors, Teens, Adults, Adaptive
    ## 344 General Public, Seniors, Teens, Adults, Adaptive, Young Adult, Children, Tot
    ## 345                        General Public, Seniors, Teens, Adults, Children, Tot
    ## 346           General Public, Seniors, Teens, Adults, Young Adult, Tot, Children
    ## 347                                                        General Public, Teens
    ## 348 General Public, Teens, Adaptive, Seniors, Adults, Young Adult, Children, Tot
    ## 349                                                General Public, Teens, Adults
    ## 350                        General Public, Teens, Adults, Children, Tot, Seniors
    ## 351           General Public, Teens, Adults, Seniors, Children, Tot, Young Adult
    ## 352                         General Public, Teens, Adults, Young Adult, Children
    ## 353                    General Public, Teens, Adults, Young Adult, Children, Tot
    ## 354                                         General Public, Teens, Children, Tot
    ## 355                            General Public, Teens, Children, Tot, Young Adult
    ## 356           General Public, Teens, Seniors, Adults, Children, Tot, Young Adult
    ## 357                         General Public, Teens, Young Adult, Children, Adults
    ## 358                                                General Public, Tot, Children
    ## 359                                         General Public, Tot, Children, Teens
    ## 360           General Public, Tot, Children, Young Adult, Adults, Teens, Seniors
    ## 361 General Public, Tot, Children, Young Adult, Adults, Teens, Seniors, Adaptive
    ## 362                                          General Public, Young Adult, Adults
    ## 363                            General Public, Young Adult, Children, Tot, Teens
    ## 364                                           General Public, Young Adult, Teens
    ## 365                General Public;#Adaptive;#Adults;#Seniors;#Teens;#Young Adult
    ## 366                General Public;#Adaptive;#Adults;#Teens;#Seniors;#Young Adult
    ## 367 General Public;#Adaptive;#Tot;#Children;#Young Adult;#Adults;#Teens;#Seniors
    ## 368                                                       General Public;#Adults
    ## 369                                             General Public;#Adults;#Children
    ## 370                                 General Public;#Adults;#Seniors;#Young Adult
    ## 371                             General Public;#Adults;#Teens;#Adaptive;#Seniors
    ## 372                                          General Public;#Adults;#Young Adult
    ## 373                                General Public;#Adults;#Young Adult;#Children
    ## 374                                 General Public;#Adults;#Young Adult;#Seniors
    ## 375                                                     General Public;#Children
    ## 376                                              General Public;#Children;#Teens
    ## 377                                                General Public;#Children;#Tot
    ## 378                                        General Public;#Children;#Tot;#Adults
    ## 379                                         General Public;#Children;#Tot;#Teens
    ## 380                                        General Public;#Children;#Young Adult
    ## 381                General Public;#Children;#Young Adult;#Adults;#Teens;#Seniors
    ## 382                                                      General Public;#Seniors
    ## 383      General Public;#Seniors;#Adaptive;#Teens;#Adults;#Young Adult;#Children
    ## 384                                              General Public;#Seniors;#Adults
    ## 385                                 General Public;#Seniors;#Adults;#Young Adult
    ## 386                             General Public;#Seniors;#Teens;#Adults;#Adaptive
    ## 387 General Public;#Seniors;#Teens;#Adults;#Adaptive;#Young Adult;#Children;#Tot
    ## 388                        General Public;#Seniors;#Teens;#Adults;#Children;#Tot
    ## 389           General Public;#Seniors;#Teens;#Adults;#Young Adult;#Tot;#Children
    ## 390                                                        General Public;#Teens
    ## 391                                                General Public;#Teens;#Adults
    ## 392                         General Public;#Teens;#Adults;#Young Adult;#Children
    ## 393                                         General Public;#Teens;#Children;#Tot
    ## 394                            General Public;#Teens;#Children;#Tot;#Young Adult
    ## 395                         General Public;#Teens;#Young Adult;#Children;#Adults
    ## 396                                                General Public;#Tot;#Children
    ## 397                                         General Public;#Tot;#Children;#Teens
    ## 398                                          General Public;#Young Adult;#Adults
    ## 399                            General Public;#Young Adult;#Children;#Tot;#Teens
    ## 400                                           General Public;#Young Adult;#Teens
    ## 401                                                                      Seniors
    ## 402                                                              Seniors, Adults
    ## 403                                              Seniors, Adults, General Public
    ## 404                                                       Seniors, Adults, Teens
    ## 405                                                 Seniors, Adults, Young Adult
    ## 406                                          Seniors, Adults, Young Adult, Teens
    ## 407                                               Seniors, Children, Young Adult
    ## 408                                                      Seniors, General Public
    ## 409 Seniors, General Public, Adaptive, Teens, Adults, Young Adult, Children, Tot
    ## 410           Seniors, General Public, Teens, Adults, Children, Young Adult, Tot
    ## 411                                                              Seniors;#Adults
    ## 412                                              Seniors;#Adults;#General Public
    ## 413                                                       Seniors;#Adults;#Teens
    ## 414                                                 Seniors;#Adults;#Young Adult
    ## 415                                          Seniors;#Adults;#Young Adult;#Teens
    ## 416                                               Seniors;#Children;#Young Adult
    ## 417                                                      Seniors;#General Public
    ## 418 Seniors;#General Public;#Adaptive;#Teens;#Adults;#Young Adult;#Children;#Tot
    ## 419                                                                        Teens
    ## 420                                    Teens, Adaptive, Children, General Public
    ## 421                                                                Teens, Adults
    ## 422                    Teens, Adults, General Public, Young Adult, Children, Tot
    ## 423                                                       Teens, Adults, Seniors
    ## 424                                          Teens, Adults, Seniors, Young Adult
    ## 425                                                   Teens, Adults, Young Adult
    ## 426                                         Teens, Adults, Young Adult, Children
    ## 427                                   Teens, Adults, Young Adult, General Public
    ## 428                                Teens, Adults, Young Adult, Seniors, Adaptive
    ## 429                          Teens, Adults, Young Adult, Seniors, General Public
    ## 430                                                              Teens, Children
    ## 431                                                      Teens, Children, Adults
    ## 432                                              Teens, Children, General Public
    ## 433                                                         Teens, Children, Tot
    ## 434                                                        Teens, General Public
    ## 435                           Teens, Seniors, Adults, Young Adult, Children, Tot
    ## 436                                       Teens, Seniors, General Public, Adults
    ## 437                                                         Teens, Tot, Children
    ## 438                                    Teens, Tot, Children, Adults, Young Adult
    ## 439                                                           Teens, Young Adult
    ## 440                                                 Teens, Young Adult, Adaptive
    ## 441                                                   Teens, Young Adult, Adults
    ## 442                                         Teens, Young Adult, Adults, Children
    ## 443                                   Teens, Young Adult, Adults, General Public
    ## 444                                          Teens, Young Adult, Adults, Seniors
    ## 445                                                 Teens, Young Adult, Children
    ## 446                Teens, Young Adult, Children, Adults, Seniors, General Public
    ## 447                                            Teens, Young Adult, Children, Tot
    ## 448                                           Teens, Young Adult, General Public
    ## 449 Teens, Young Adult, Tot, Children, Adults, General Public, Adaptive, Seniors
    ## 450                                    Teens;#Adaptive;#Children;#General Public
    ## 451                                                                Teens;#Adults
    ## 452                                                       Teens;#Adults;#Seniors
    ## 453                                          Teens;#Adults;#Seniors;#Young Adult
    ## 454                                                   Teens;#Adults;#Young Adult
    ## 455                                   Teens;#Adults;#Young Adult;#General Public
    ## 456                                Teens;#Adults;#Young Adult;#Seniors;#Adaptive
    ## 457                          Teens;#Adults;#Young Adult;#Seniors;#General Public
    ## 458                                                              Teens;#Children
    ## 459                                                      Teens;#Children;#Adults
    ## 460                                              Teens;#Children;#General Public
    ## 461                                                         Teens;#Children;#Tot
    ## 462                           Teens;#Seniors;#Adults;#Young Adult;#Children;#Tot
    ## 463                                       Teens;#Seniors;#General Public;#Adults
    ## 464                                                         Teens;#Tot;#Children
    ## 465                                    Teens;#Tot;#Children;#Adults;#Young Adult
    ## 466                                                           Teens;#Young Adult
    ## 467                                                   Teens;#Young Adult;#Adults
    ## 468                                         Teens;#Young Adult;#Adults;#Children
    ## 469                                   Teens;#Young Adult;#Adults;#General Public
    ## 470                                          Teens;#Young Adult;#Adults;#Seniors
    ## 471                                                 Teens;#Young Adult;#Children
    ## 472                Teens;#Young Adult;#Children;#Adults;#Seniors;#General Public
    ## 473                                            Teens;#Young Adult;#Children;#Tot
    ## 474                                           Teens;#Young Adult;#General Public
    ## 475 Teens;#Young Adult;#Tot;#Children;#Adults;#General Public;#Adaptive;#Seniors
    ## 476                                                                          Tot
    ## 477                                                                  Tot, Adults
    ## 478                                    Tot, Adults, Children, Teens, Young Adult
    ## 479 Tot, Adults, Children, Young Adult, Teens, Adaptive, Seniors, General Public
    ## 480           Tot, Adults, Children, Young Adult, Teens, General Public, Seniors
    ## 481 Tot, Adults, Teens, Children, Young Adult, Adaptive, Seniors, General Public
    ## 482                                                     Tot, Adults, Young Adult
    ## 483                                                                Tot, Children
    ## 484                                                      Tot, Children, Adaptive
    ## 485                                                        Tot, Children, Adults
    ## 486                                        Tot, Children, Adults, General Public
    ## 487                                                 Tot, Children, Adults, Teens
    ## 488                                       Tot, Children, Adults, Teens, Adaptive
    ## 489                                 Tot, Children, Adults, Teens, General Public
    ## 490                        Tot, Children, Adults, Teens, Seniors, General Public
    ## 491           Tot, Children, Adults, Teens, Seniors, General Public, Young Adult
    ## 492                                    Tot, Children, Adults, Teens, Young Adult
    ## 493                                           Tot, Children, Adults, Young Adult
    ## 494                           Tot, Children, Adults, Young Adult, General Public
    ## 495                                    Tot, Children, Adults, Young Adult, Teens
    ## 496 Tot, Children, Adults, Young Adult, Teens, Adaptive, General Public, Seniors
    ## 497 Tot, Children, Adults, Young Adult, Teens, Adaptive, Seniors, General Public
    ## 498                    Tot, Children, Adults, Young Adult, Teens, General Public
    ## 499 Tot, Children, Adults, Young Adult, Teens, General Public, Adaptive, Seniors
    ## 500 Tot, Children, Adults, Young Adult, Teens, Seniors, Adaptive, General Public
    ## 501                                                Tot, Children, General Public
    ## 502                                        Tot, Children, General Public, Adults
    ## 503                                         Tot, Children, General Public, Teens
    ## 504                               Tot, Children, Seniors, General Public, Adults
    ## 505                                                         Tot, Children, Teens
    ## 506                                                 Tot, Children, Teens, Adults
    ## 507                          Tot, Children, Teens, Adults, Adaptive, Young Adult
    ## 508 Tot, Children, Teens, Adults, Young Adult, General Public, Seniors, Adaptive
    ## 509                                         Tot, Children, Teens, General Public
    ## 510                                            Tot, Children, Teens, Young Adult
    ## 511                                                   Tot, Children, Young Adult
    ## 512                                           Tot, Children, Young Adult, Adults
    ## 513                 Tot, Children, Young Adult, Adults, Adaptive, Teens, Seniors
    ## 514                           Tot, Children, Young Adult, Adults, General Public
    ## 515                                    Tot, Children, Young Adult, Adults, Teens
    ## 516          Tot, Children, Young Adult, Adults, Teens, Adaptive, General Public
    ## 517 Tot, Children, Young Adult, Adults, Teens, Adaptive, General Public, Seniors
    ## 518                 Tot, Children, Young Adult, Adults, Teens, Adaptive, Seniors
    ## 519 Tot, Children, Young Adult, Adults, Teens, Adaptive, Seniors, General Public
    ## 520                    Tot, Children, Young Adult, Adults, Teens, General Public
    ## 521 Tot, Children, Young Adult, Adults, Teens, General Public, Adaptive, Seniors
    ## 522           Tot, Children, Young Adult, Adults, Teens, General Public, Seniors
    ## 523 Tot, Children, Young Adult, Adults, Teens, General Public, Seniors, Adaptive
    ## 524                           Tot, Children, Young Adult, Adults, Teens, Seniors
    ## 525 Tot, Children, Young Adult, Adults, Teens, Seniors, Adaptive, General Public
    ## 526           Tot, Children, Young Adult, Adults, Teens, Seniors, General Public
    ## 527 Tot, Children, Young Adult, Adults, Teens, Seniors, General Public, Adaptive
    ## 528                                   Tot, Children, Young Adult, General Public
    ## 529                                            Tot, Children, Young Adult, Teens
    ## 530 Tot, Children, Young Adult, Teens, Adaptive, Adults, Seniors, General Public
    ## 531                                    Tot, Children, Young Adult, Teens, Adults
    ## 532 Tot, Children, Young Adult, Teens, Adults, Adaptive, Seniors, General Public
    ## 533                    Tot, Children, Young Adult, Teens, Adults, General Public
    ## 534 Tot, Children, Young Adult, Teens, Adults, General Public, Adaptive, Seniors
    ## 535                            Tot, Children, Young Adult, Teens, General Public
    ## 536                                                          Tot, General Public
    ## 537                                 Tot, General Public, Adults, Teens, Children
    ## 538                                                Tot, General Public, Children
    ## 539                                                                   Tot, Teens
    ## 540                                                         Tot, Teens, Children
    ## 541                                         Tot, Teens, Children, General Public
    ## 542                                                   Tot, Teens, General Public
    ## 543                                     Tot, Teens, Young Adult, Adults, Seniors
    ## 544                    Tot, Young Adult, Adults, Children, Teens, General Public
    ## 545                                    Tot, Young Adult, Adults, Teens, Children
    ## 546                                                   Tot, Young Adult, Children
    ## 547                                           Tot, Young Adult, Children, Adults
    ## 548 Tot, Young Adult, Children, Adults, Teens, Adaptive, Seniors, General Public
    ## 549 Tot, Young Adult, Children, Adults, Teens, General Public, Seniors, Adaptive
    ## 550 Tot, Young Adult, Children, Teens, Adults, Adaptive, Seniors, General Public
    ## 551           Tot, Young Adult, Children, Teens, Adults, General Public, Seniors
    ## 552                                                      Tot, Young Adult, Teens
    ## 553                                    Tot, Young Adult, Teens, Children, Adults
    ## 554                            Tot, Young Adult, Teens, Children, General Public
    ## 555                                                                  Tot;#Adults
    ## 556                                    Tot;#Adults;#Children;#Teens;#Young Adult
    ## 557 Tot;#Adults;#Teens;#Children;#Young Adult;#Adaptive;#Seniors;#General Public
    ## 558                                                                Tot;#Children
    ## 559                                                      Tot;#Children;#Adaptive
    ## 560                                                        Tot;#Children;#Adults
    ## 561                                        Tot;#Children;#Adults;#General Public
    ## 562                                                 Tot;#Children;#Adults;#Teens
    ## 563                                       Tot;#Children;#Adults;#Teens;#Adaptive
    ## 564                                 Tot;#Children;#Adults;#Teens;#General Public
    ## 565                        Tot;#Children;#Adults;#Teens;#Seniors;#General Public
    ## 566           Tot;#Children;#Adults;#Teens;#Seniors;#General Public;#Young Adult
    ## 567                                    Tot;#Children;#Adults;#Teens;#Young Adult
    ## 568                                           Tot;#Children;#Adults;#Young Adult
    ## 569                                    Tot;#Children;#Adults;#Young Adult;#Teens
    ## 570 Tot;#Children;#Adults;#Young Adult;#Teens;#Adaptive;#General Public;#Seniors
    ## 571                    Tot;#Children;#Adults;#Young Adult;#Teens;#General Public
    ## 572                                                Tot;#Children;#General Public
    ## 573                                        Tot;#Children;#General Public;#Adults
    ## 574                                         Tot;#Children;#General Public;#Teens
    ## 575                               Tot;#Children;#Seniors;#General Public;#Adults
    ## 576                                                         Tot;#Children;#Teens
    ## 577                                                 Tot;#Children;#Teens;#Adults
    ## 578                          Tot;#Children;#Teens;#Adults;#Adaptive;#Young Adult
    ## 579                                         Tot;#Children;#Teens;#General Public
    ## 580                                            Tot;#Children;#Teens;#Young Adult
    ## 581                                                   Tot;#Children;#Young Adult
    ## 582                                           Tot;#Children;#Young Adult;#Adults
    ## 583                           Tot;#Children;#Young Adult;#Adults;#General Public
    ## 584                                    Tot;#Children;#Young Adult;#Adults;#Teens
    ## 585                 Tot;#Children;#Young Adult;#Adults;#Teens;#Adaptive;#Seniors
    ## 586 Tot;#Children;#Young Adult;#Adults;#Teens;#Adaptive;#Seniors;#General Public
    ## 587                    Tot;#Children;#Young Adult;#Adults;#Teens;#General Public
    ## 588 Tot;#Children;#Young Adult;#Adults;#Teens;#General Public;#Adaptive;#Seniors
    ## 589 Tot;#Children;#Young Adult;#Adults;#Teens;#General Public;#Seniors;#Adaptive
    ## 590                           Tot;#Children;#Young Adult;#Adults;#Teens;#Seniors
    ## 591 Tot;#Children;#Young Adult;#Adults;#Teens;#Seniors;#Adaptive;#General Public
    ## 592           Tot;#Children;#Young Adult;#Adults;#Teens;#Seniors;#General Public
    ## 593                                            Tot;#Children;#Young Adult;#Teens
    ## 594 Tot;#Children;#Young Adult;#Teens;#Adults;#Adaptive;#Seniors;#General Public
    ## 595 Tot;#Children;#Young Adult;#Teens;#Adults;#General Public;#Adaptive;#Seniors
    ## 596                            Tot;#Children;#Young Adult;#Teens;#General Public
    ## 597                                                          Tot;#General Public
    ## 598                                 Tot;#General Public;#Adults;#Teens;#Children
    ## 599                                                Tot;#General Public;#Children
    ## 600                                                                   Tot;#Teens
    ## 601                                                         Tot;#Teens;#Children
    ## 602                                         Tot;#Teens;#Children;#General Public
    ## 603                                                   Tot;#Teens;#General Public
    ## 604                                     Tot;#Teens;#Young Adult;#Adults;#Seniors
    ## 605                                    Tot;#Young Adult;#Adults;#Teens;#Children
    ## 606                                                   Tot;#Young Adult;#Children
    ## 607                                           Tot;#Young Adult;#Children;#Adults
    ## 608 Tot;#Young Adult;#Children;#Adults;#Teens;#Adaptive;#Seniors;#General Public
    ## 609 Tot;#Young Adult;#Children;#Adults;#Teens;#General Public;#Seniors;#Adaptive
    ## 610                                                      Tot;#Young Adult;#Teens
    ## 611                                    Tot;#Young Adult;#Teens;#Children;#Adults
    ## 612                            Tot;#Young Adult;#Teens;#Children;#General Public
    ## 613                                                                  Young Adult
    ## 614                               Young Adult, Adaptive, General Public, Seniors
    ## 615                                                          Young Adult, Adults
    ## 616                                       Young Adult, Adults, Adaptive, Seniors
    ## 617                       Young Adult, Adults, Adaptive, Seniors, General Public
    ## 618                                                Young Adult, Adults, Children
    ## 619                                Young Adult, Adults, Children, General Public
    ## 620                      Young Adult, Adults, Children, Teens, Adaptive, Seniors
    ## 621      Young Adult, Adults, Children, Teens, Seniors, Adaptive, General Public
    ## 622      Young Adult, Adults, Children, Teens, Seniors, General Public, Adaptive
    ## 623                                          Young Adult, Adults, General Public
    ## 624                                 Young Adult, Adults, General Public, Seniors
    ## 625                Young Adult, Adults, General Public, Teens, Adaptive, Seniors
    ## 626                                                 Young Adult, Adults, Seniors
    ## 627                                Young Adult, Adults, Seniors, Adaptive, Teens
    ## 628                                 Young Adult, Adults, Seniors, General Public
    ## 629                Young Adult, Adults, Seniors, General Public, Teens, Children
    ## 630                                          Young Adult, Adults, Seniors, Teens
    ## 631                Young Adult, Adults, Seniors, Teens, General Public, Adaptive
    ## 632                                                   Young Adult, Adults, Teens
    ## 633                         Young Adult, Adults, Teens, Adaptive, General Public
    ## 634                Young Adult, Adults, Teens, Adaptive, General Public, Seniors
    ## 635                                Young Adult, Adults, Teens, Adaptive, Seniors
    ## 636                Young Adult, Adults, Teens, Adaptive, Seniors, General Public
    ## 637      Young Adult, Adults, Teens, Adaptive, Seniors, General Public, Children
    ## 638                                         Young Adult, Adults, Teens, Children
    ## 639               Young Adult, Adults, Teens, Children, Adaptive, General Public
    ## 640                      Young Adult, Adults, Teens, Children, Adaptive, Seniors
    ## 641 Young Adult, Adults, Teens, Children, General Public, Seniors, Adaptive, Tot
    ## 642                Young Adult, Adults, Teens, Children, Seniors, General Public
    ## 643                                   Young Adult, Adults, Teens, General Public
    ## 644                Young Adult, Adults, Teens, General Public, Adaptive, Seniors
    ## 645                          Young Adult, Adults, Teens, General Public, Seniors
    ## 646                Young Adult, Adults, Teens, General Public, Seniors, Adaptive
    ## 647                                          Young Adult, Adults, Teens, Seniors
    ## 648                                Young Adult, Adults, Teens, Seniors, Adaptive
    ## 649                Young Adult, Adults, Teens, Seniors, Adaptive, General Public
    ## 650                          Young Adult, Adults, Teens, Seniors, General Public
    ## 651                                                        Young Adult, Children
    ## 652                                                Young Adult, Children, Adults
    ## 653                                         Young Adult, Children, Adults, Teens
    ## 654               Young Adult, Children, Adults, Teens, Adaptive, General Public
    ## 655      Young Adult, Children, Adults, Teens, Adaptive, General Public, Seniors
    ## 656                      Young Adult, Children, Adults, Teens, Adaptive, Seniors
    ## 657      Young Adult, Children, Adults, Teens, Adaptive, Seniors, General Public
    ## 658                         Young Adult, Children, Adults, Teens, General Public
    ## 659                Young Adult, Children, Adults, Teens, General Public, Seniors
    ## 660      Young Adult, Children, Adults, Teens, Seniors, Adaptive, General Public
    ## 661                                        Young Adult, Children, General Public
    ## 662                                                 Young Adult, Children, Teens
    ## 663                Young Adult, Children, Teens, Adults, Seniors, General Public
    ## 664                      Young Adult, Children, Teens, Seniors, Adaptive, Adults
    ## 665                                   Young Adult, Children, Tot, General Public
    ## 666                                            Young Adult, Children, Tot, Teens
    ## 667                                                         Young Adult, Seniors
    ## 668                                                 Young Adult, Seniors, Adults
    ## 669                                                           Young Adult, Teens
    ## 670                                                   Young Adult, Teens, Adults
    ## 671                Young Adult, Teens, Adults, Adaptive, General Public, Seniors
    ## 672                                   Young Adult, Teens, Adults, General Public
    ## 673                                Young Adult, Teens, Adults, Seniors, Adaptive
    ## 674                                           Young Adult, Teens, General Public
    ## 675                                  Young Adult, Teens, Seniors, General Public
    ## 676                                                   Young Adult, Tot, Children
    ## 677                                    Young Adult, Tot, Children, Adults, Teens
    ## 678           Young Adult, Tot, Children, Adults, Teens, General Public, Seniors
    ## 679                               Young Adult;#Adaptive;#General Public;#Seniors
    ## 680                                                          Young Adult;#Adults
    ## 681                                       Young Adult;#Adults;#Adaptive;#Seniors
    ## 682                       Young Adult;#Adults;#Adaptive;#Seniors;#General Public
    ## 683                      Young Adult;#Adults;#Children;#Teens;#Adaptive;#Seniors
    ## 684      Young Adult;#Adults;#Children;#Teens;#Seniors;#Adaptive;#General Public
    ## 685      Young Adult;#Adults;#Children;#Teens;#Seniors;#General Public;#Adaptive
    ## 686                                          Young Adult;#Adults;#General Public
    ## 687                                 Young Adult;#Adults;#General Public;#Seniors
    ## 688                Young Adult;#Adults;#General Public;#Teens;#Adaptive;#Seniors
    ## 689                                                 Young Adult;#Adults;#Seniors
    ## 690                                Young Adult;#Adults;#Seniors;#Adaptive;#Teens
    ## 691                                 Young Adult;#Adults;#Seniors;#General Public
    ## 692                Young Adult;#Adults;#Seniors;#General Public;#Teens;#Children
    ## 693                                          Young Adult;#Adults;#Seniors;#Teens
    ## 694                Young Adult;#Adults;#Seniors;#Teens;#General Public;#Adaptive
    ## 695                                                   Young Adult;#Adults;#Teens
    ## 696                         Young Adult;#Adults;#Teens;#Adaptive;#General Public
    ## 697                                Young Adult;#Adults;#Teens;#Adaptive;#Seniors
    ## 698                Young Adult;#Adults;#Teens;#Adaptive;#Seniors;#General Public
    ## 699      Young Adult;#Adults;#Teens;#Adaptive;#Seniors;#General Public;#Children
    ## 700                                         Young Adult;#Adults;#Teens;#Children
    ## 701               Young Adult;#Adults;#Teens;#Children;#Adaptive;#General Public
    ## 702                      Young Adult;#Adults;#Teens;#Children;#Adaptive;#Seniors
    ## 703 Young Adult;#Adults;#Teens;#Children;#General Public;#Seniors;#Adaptive;#Tot
    ## 704                                   Young Adult;#Adults;#Teens;#General Public
    ## 705                          Young Adult;#Adults;#Teens;#General Public;#Seniors
    ## 706                Young Adult;#Adults;#Teens;#General Public;#Seniors;#Adaptive
    ## 707                                          Young Adult;#Adults;#Teens;#Seniors
    ## 708                                Young Adult;#Adults;#Teens;#Seniors;#Adaptive
    ## 709                Young Adult;#Adults;#Teens;#Seniors;#Adaptive;#General Public
    ## 710                          Young Adult;#Adults;#Teens;#Seniors;#General Public
    ## 711                                                        Young Adult;#Children
    ## 712                                                Young Adult;#Children;#Adults
    ## 713                                         Young Adult;#Children;#Adults;#Teens
    ## 714               Young Adult;#Children;#Adults;#Teens;#Adaptive;#General Public
    ## 715                      Young Adult;#Children;#Adults;#Teens;#Adaptive;#Seniors
    ## 716      Young Adult;#Children;#Adults;#Teens;#Adaptive;#Seniors;#General Public
    ## 717                         Young Adult;#Children;#Adults;#Teens;#General Public
    ## 718                Young Adult;#Children;#Adults;#Teens;#General Public;#Seniors
    ## 719      Young Adult;#Children;#Adults;#Teens;#Seniors;#Adaptive;#General Public
    ## 720                                        Young Adult;#Children;#General Public
    ## 721                                                 Young Adult;#Children;#Teens
    ## 722                      Young Adult;#Children;#Teens;#Seniors;#Adaptive;#Adults
    ## 723                                   Young Adult;#Children;#Tot;#General Public
    ## 724                                            Young Adult;#Children;#Tot;#Teens
    ## 725                                                         Young Adult;#Seniors
    ## 726                                                 Young Adult;#Seniors;#Adults
    ## 727                                                           Young Adult;#Teens
    ## 728                                                   Young Adult;#Teens;#Adults
    ## 729                Young Adult;#Teens;#Adults;#Adaptive;#General Public;#Seniors
    ## 730                                           Young Adult;#Teens;#General Public
    ## 731                                  Young Adult;#Teens;#Seniors;#General Public
    ## 732                                                   Young Adult;#Tot;#Children
    ## 733                                    Young Adult;#Tot;#Children;#Adults;#Teens
    ## 734           Young Adult;#Tot;#Children;#Adults;#Teens;#General Public;#Seniors
    ##        n
    ## 1   9082
    ## 2     98
    ## 3      1
    ## 4      1
    ## 5      1
    ## 6      1
    ## 7      1
    ## 8    338
    ## 9      1
    ## 10     6
    ## 11     1
    ## 12     1
    ## 13     1
    ## 14     1
    ## 15     1
    ## 16     1
    ## 17     1
    ## 18     1
    ## 19    14
    ## 20     3
    ## 21     1
    ## 22     3
    ## 23    66
    ## 24     2
    ## 25    16
    ## 26     1
    ## 27     1
    ## 28     1
    ## 29     1
    ## 30     3
    ## 31     5
    ## 32     1
    ## 33     2
    ## 34     1
    ## 35     1
    ## 36     2
    ## 37     1
    ## 38     1
    ## 39     1
    ## 40     1
    ## 41     1
    ## 42     1
    ## 43     1
    ## 44     2
    ## 45     3
    ## 46     1
    ## 47     2
    ## 48    32
    ## 49     1
    ## 50     1
    ## 51     2
    ## 52     1
    ## 53    18
    ## 54     1
    ## 55     6
    ## 56     1
    ## 57     1
    ## 58     1
    ## 59     2
    ## 60     1
    ## 61     2
    ## 62     1
    ## 63     6
    ## 64     1
    ## 65     1
    ## 66     1
    ## 67     1
    ## 68     1
    ## 69     1
    ## 70     1
    ## 71    10
    ## 72     2
    ## 73     1
    ## 74     3
    ## 75    61
    ## 76     2
    ## 77    16
    ## 78     1
    ## 79     1
    ## 80     1
    ## 81     2
    ## 82     3
    ## 83     1
    ## 84     2
    ## 85     1
    ## 86     1
    ## 87     1
    ## 88     1
    ## 89     1
    ## 90     1
    ## 91     1
    ## 92     1
    ## 93     1
    ## 94     3
    ## 95     2
    ## 96    24
    ## 97     1
    ## 98     2
    ## 99     1
    ## 100   16
    ## 101    1
    ## 102    6
    ## 103    1
    ## 104    1
    ## 105    1
    ## 106    1
    ## 107    1
    ## 108    2
    ## 109  680
    ## 110    4
    ## 111   28
    ## 112    4
    ## 113    2
    ## 114    2
    ## 115    1
    ## 116    7
    ## 117    3
    ## 118    1
    ## 119    2
    ## 120    1
    ## 121    2
    ## 122    1
    ## 123    3
    ## 124    1
    ## 125    1
    ## 126    2
    ## 127    2
    ## 128    2
    ## 129    1
    ## 130    1
    ## 131    1
    ## 132    2
    ## 133    1
    ## 134    1
    ## 135   73
    ## 136    2
    ## 137    1
    ## 138    4
    ## 139    1
    ## 140  157
    ## 141    1
    ## 142    2
    ## 143    1
    ## 144   29
    ## 145    2
    ## 146    1
    ## 147    1
    ## 148    1
    ## 149   12
    ## 150    5
    ## 151    1
    ## 152    1
    ## 153    1
    ## 154   43
    ## 155    1
    ## 156    3
    ## 157    1
    ## 158    1
    ## 159    1
    ## 160    1
    ## 161    1
    ## 162    1
    ## 163    1
    ## 164   27
    ## 165    1
    ## 166   35
    ## 167    2
    ## 168    6
    ## 169    1
    ## 170    1
    ## 171    4
    ## 172    7
    ## 173    1
    ## 174    2
    ## 175    3
    ## 176    2
    ## 177    4
    ## 178    1
    ## 179    1
    ## 180   16
    ## 181   18
    ## 182    3
    ## 183    1
    ## 184    1
    ## 185    1
    ## 186    1
    ## 187   23
    ## 188    1
    ## 189    4
    ## 190    2
    ## 191   30
    ## 192   60
    ## 193   16
    ## 194    5
    ## 195    6
    ## 196    1
    ## 197    4
    ## 198   15
    ## 199    2
    ## 200    1
    ## 201    7
    ## 202    1
    ## 203    1
    ## 204    2
    ## 205    6
    ## 206   24
    ## 207    1
    ## 208    1
    ## 209    3
    ## 210    1
    ## 211    2
    ## 212    2
    ## 213    1
    ## 214    2
    ## 215    1
    ## 216    1
    ## 217    2
    ## 218    4
    ## 219   20
    ## 220    2
    ## 221    2
    ## 222    2
    ## 223    1
    ## 224    5
    ## 225    1
    ## 226    1
    ## 227    2
    ## 228    1
    ## 229    2
    ## 230    1
    ## 231    2
    ## 232    1
    ## 233    2
    ## 234    1
    ## 235    2
    ## 236    1
    ## 237    1
    ## 238    2
    ## 239    1
    ## 240    1
    ## 241   70
    ## 242    2
    ## 243    4
    ## 244    1
    ## 245  151
    ## 246    1
    ## 247    2
    ## 248   28
    ## 249    2
    ## 250    1
    ## 251    1
    ## 252    1
    ## 253   11
    ## 254    5
    ## 255    1
    ## 256    1
    ## 257    1
    ## 258   42
    ## 259    1
    ## 260    2
    ## 261    1
    ## 262    1
    ## 263    1
    ## 264    1
    ## 265    1
    ## 266    1
    ## 267   27
    ## 268    1
    ## 269   33
    ## 270    2
    ## 271    6
    ## 272    1
    ## 273    1
    ## 274    2
    ## 275    3
    ## 276    1
    ## 277    2
    ## 278    1
    ## 279    2
    ## 280    1
    ## 281    1
    ## 282   13
    ## 283   14
    ## 284    3
    ## 285    1
    ## 286    1
    ## 287    1
    ## 288   12
    ## 289    1
    ## 290    4
    ## 291    2
    ## 292   29
    ## 293   21
    ## 294    6
    ## 295    4
    ## 296    5
    ## 297    4
    ## 298    9
    ## 299    1
    ## 300    1
    ## 301    4
    ## 302    1
    ## 303    1
    ## 304    6
    ## 305   23
    ## 306    1
    ## 307    3
    ## 308    1
    ## 309    2
    ## 310    2
    ## 311    1
    ## 312    2
    ## 313    1
    ## 314    2
    ## 315 6026
    ## 316    1
    ## 317    1
    ## 318    1
    ## 319    1
    ## 320    1
    ## 321    1
    ## 322    3
    ## 323    2
    ## 324    1
    ## 325    1
    ## 326    1
    ## 327    1
    ## 328    1
    ## 329   15
    ## 330    1
    ## 331    3
    ## 332    1
    ## 333    1
    ## 334    2
    ## 335    1
    ## 336    1
    ## 337    2
    ## 338    6
    ## 339    2
    ## 340    2
    ## 341    3
    ## 342    1
    ## 343    1
    ## 344    1
    ## 345    1
    ## 346    1
    ## 347    2
    ## 348    1
    ## 349    3
    ## 350    1
    ## 351    1
    ## 352    1
    ## 353    1
    ## 354    3
    ## 355    1
    ## 356    1
    ## 357    1
    ## 358    4
    ## 359    1
    ## 360    2
    ## 361    1
    ## 362    1
    ## 363    1
    ## 364    1
    ## 365    1
    ## 366    1
    ## 367    1
    ## 368    1
    ## 369    3
    ## 370    2
    ## 371    1
    ## 372    1
    ## 373    1
    ## 374    1
    ## 375   15
    ## 376    3
    ## 377    1
    ## 378    1
    ## 379    2
    ## 380    1
    ## 381    2
    ## 382    6
    ## 383    1
    ## 384    2
    ## 385    1
    ## 386    1
    ## 387    1
    ## 388    1
    ## 389    1
    ## 390    1
    ## 391    3
    ## 392    1
    ## 393    3
    ## 394    1
    ## 395    1
    ## 396    4
    ## 397    1
    ## 398    1
    ## 399    1
    ## 400    1
    ## 401  233
    ## 402   14
    ## 403    4
    ## 404    1
    ## 405    1
    ## 406    1
    ## 407    1
    ## 408    2
    ## 409    1
    ## 410    1
    ## 411   14
    ## 412    4
    ## 413    1
    ## 414    1
    ## 415    1
    ## 416    1
    ## 417    1
    ## 418    1
    ## 419  146
    ## 420    1
    ## 421    7
    ## 422    1
    ## 423    2
    ## 424    2
    ## 425    2
    ## 426    2
    ## 427    1
    ## 428    1
    ## 429    1
    ## 430   29
    ## 431    1
    ## 432    1
    ## 433    4
    ## 434    3
    ## 435    1
    ## 436    1
    ## 437    1
    ## 438    1
    ## 439    5
    ## 440    1
    ## 441    2
    ## 442    2
    ## 443    1
    ## 444    3
    ## 445    5
    ## 446    2
    ## 447    2
    ## 448    1
    ## 449    2
    ## 450    1
    ## 451    2
    ## 452    2
    ## 453    2
    ## 454    1
    ## 455    1
    ## 456    1
    ## 457    1
    ## 458   29
    ## 459    1
    ## 460    1
    ## 461    4
    ## 462    1
    ## 463    1
    ## 464    1
    ## 465    1
    ## 466    5
    ## 467    2
    ## 468    1
    ## 469    1
    ## 470    3
    ## 471    5
    ## 472    2
    ## 473    2
    ## 474    1
    ## 475    1
    ## 476   92
    ## 477    3
    ## 478    1
    ## 479    1
    ## 480    1
    ## 481    1
    ## 482    1
    ## 483  146
    ## 484    1
    ## 485   11
    ## 486   10
    ## 487    2
    ## 488    1
    ## 489    1
    ## 490    1
    ## 491    1
    ## 492    1
    ## 493    1
    ## 494    1
    ## 495    1
    ## 496    1
    ## 497    3
    ## 498    2
    ## 499    1
    ## 500    1
    ## 501  124
    ## 502    2
    ## 503    1
    ## 504    1
    ## 505  195
    ## 506    8
    ## 507    1
    ## 508    1
    ## 509   11
    ## 510    1
    ## 511   16
    ## 512    5
    ## 513    1
    ## 514    3
    ## 515   31
    ## 516    1
    ## 517    1
    ## 518    8
    ## 519   29
    ## 520   11
    ## 521    1
    ## 522    5
    ## 523    4
    ## 524    2
    ## 525    3
    ## 526   10
    ## 527    2
    ## 528    1
    ## 529    4
    ## 530    1
    ## 531    2
    ## 532    1
    ## 533    2
    ## 534    1
    ## 535    2
    ## 536    4
    ## 537    1
    ## 538   40
    ## 539    3
    ## 540   17
    ## 541    1
    ## 542    2
    ## 543    1
    ## 544    2
    ## 545    2
    ## 546    5
    ## 547    1
    ## 548    3
    ## 549    1
    ## 550    1
    ## 551    1
    ## 552    1
    ## 553    1
    ## 554    1
    ## 555    2
    ## 556    1
    ## 557    1
    ## 558  139
    ## 559    1
    ## 560   10
    ## 561    9
    ## 562    2
    ## 563    1
    ## 564    1
    ## 565    1
    ## 566    1
    ## 567    1
    ## 568    1
    ## 569    1
    ## 570    1
    ## 571    1
    ## 572  122
    ## 573    2
    ## 574    1
    ## 575    1
    ## 576  195
    ## 577    8
    ## 578    1
    ## 579   11
    ## 580    1
    ## 581   16
    ## 582    2
    ## 583    1
    ## 584   18
    ## 585    7
    ## 586   23
    ## 587    7
    ## 588    1
    ## 589    3
    ## 590    2
    ## 591    2
    ## 592    3
    ## 593    4
    ## 594    1
    ## 595    1
    ## 596    1
    ## 597    4
    ## 598    1
    ## 599   40
    ## 600    3
    ## 601   17
    ## 602    1
    ## 603    2
    ## 604    1
    ## 605    1
    ## 606    5
    ## 607    1
    ## 608    1
    ## 609    1
    ## 610    1
    ## 611    1
    ## 612    1
    ## 613   23
    ## 614    1
    ## 615   60
    ## 616    1
    ## 617    7
    ## 618    1
    ## 619    1
    ## 620    1
    ## 621    1
    ## 622    1
    ## 623    5
    ## 624    1
    ## 625    1
    ## 626   54
    ## 627    1
    ## 628   18
    ## 629    1
    ## 630    1
    ## 631    1
    ## 632   13
    ## 633    1
    ## 634    2
    ## 635   23
    ## 636   36
    ## 637    1
    ## 638    3
    ## 639    1
    ## 640    1
    ## 641    1
    ## 642    1
    ## 643    6
    ## 644    1
    ## 645    2
    ## 646    2
    ## 647    9
    ## 648    1
    ## 649    2
    ## 650    5
    ## 651    3
    ## 652    3
    ## 653    4
    ## 654    1
    ## 655    1
    ## 656    2
    ## 657    3
    ## 658    1
    ## 659    1
    ## 660    1
    ## 661    1
    ## 662    1
    ## 663    1
    ## 664    1
    ## 665    1
    ## 666    1
    ## 667    1
    ## 668    1
    ## 669    9
    ## 670    2
    ## 671    1
    ## 672    1
    ## 673    1
    ## 674    1
    ## 675    1
    ## 676    1
    ## 677    1
    ## 678    1
    ## 679    1
    ## 680   54
    ## 681    1
    ## 682    7
    ## 683    1
    ## 684    1
    ## 685    1
    ## 686    5
    ## 687    1
    ## 688    1
    ## 689   52
    ## 690    1
    ## 691   18
    ## 692    1
    ## 693    1
    ## 694    1
    ## 695    6
    ## 696    1
    ## 697   22
    ## 698   15
    ## 699    1
    ## 700    3
    ## 701    1
    ## 702    1
    ## 703    1
    ## 704    1
    ## 705    2
    ## 706    2
    ## 707    8
    ## 708    1
    ## 709    1
    ## 710    4
    ## 711    2
    ## 712    2
    ## 713    1
    ## 714    1
    ## 715    2
    ## 716    2
    ## 717    1
    ## 718    1
    ## 719    1
    ## 720    1
    ## 721    1
    ## 722    1
    ## 723    1
    ## 724    1
    ## 725    1
    ## 726    1
    ## 727    6
    ## 728    2
    ## 729    1
    ## 730    1
    ## 731    1
    ## 732    1
    ## 733    1
    ## 734    1

``` r
parkevent |> 
  count(location)
```

    ##                                                                                       location
    ## 1                                                                    100 North Portland Avenue
    ## 2                                                           100 Street Lexington & Park Avenue
    ## 3                                                                              100% Playground
    ## 4                                                             1003 Madison Street Brooklyn, NY
    ## 5                                            106th Street Bet. 3rd Avenue and Lexington Avenue
    ## 6                                                                          108-35 167th street
    ## 7                                                                     109-20 Union Hall Street
    ## 8                                                                     117th Street Morningside
    ## 9                                                                                 120 Precinct
    ## 10                                                                                121 Precinct
    ## 11                                                                        123 Morningside Park
    ## 12                                                                      123 Street Morningside
    ## 13                                                               123 Street Morningside Avenue
    ## 14                                                                  1230 Zerega Ave, Bronx, NY
    ## 15                                                                123rd St. & Morningside Ave.
    ## 16                                                               123rd Street Lexington Avenue
    ## 17                                                                            123rd Street Mor
    ## 18                                                                    123rd Street Morningside
    ## 19                                                             123rd Street Morningside Avenue
    ## 20                                                           123rd Street bet. 3rd & Lexington
    ## 21                                                                       125th To 118th Street
    ## 22                                                           130 Avenue C bet. 9th & 10th Ave.
    ## 23                                                           1317 Fulton Avenue Bronx New York
    ## 24                                                                          1377 Jerome Avenue
    ## 25                                                                        139th & 141st Street
    ## 26                                                                  139th St. & Amsterdam Ave.
    ## 27                                                                  139th and Amsterdam Avenue
    ## 28                                                                          139th-140th Street
    ## 29                                                                   140th Street Lenox Avenue
    ## 30                                                                          145-21 Liberty Ave
    ## 31                                                                          14590 179th Street
    ## 32                                               147th Rd between 257th street and Weller Lane
    ## 33                                                                   153-90 Rockaway Boulevard
    ## 34                                          156th Street Between Melrose Ave and Courtland Ave
    ## 35                                                                    158 St & Grand Concourse
    ## 36                                                                       160 Beach 29th street
    ## 37                                                  160th Street betwn Melrose and Elton Aves.
    ## 38                                                                    161 street & Melrose ave
    ## 39                                                                        1619 East 174 Street
    ## 40                                                           167-02 Baisley Blvd/Bedell Street
    ## 41                                                                    172 st & Bathgate Avenue
    ## 42                                                                       172nd St & 3rd Avenue
    ## 43                                                                       1769 East 49th Street
    ## 44                                                           182 between Amsterdam and Audubon
    ## 45                                                              184th - 187th St & Audobon Ave
    ## 46                                                                          1958 Fulton Street
    ## 47                                                                     200 West Tremont Avenue
    ## 48                                                                         203-06 109th avenue
    ## 49                                                                 2347 Lafayette Ave & Zerega
    ## 50                                                                          2376 Watson Avenue
    ## 51                                                                            2551 Linden Blvd
    ## 52                                               26--68 Decatur Ave Between 194 St and 195 St.
    ## 53                                                                        27 Huntington Street
    ## 54                                                                        27 West 115th Street
    ## 55                                                                           301 W Tremont Ave
    ## 56                                                                          301 W Tremont Ave.
    ## 57                                                                      306A West 128th Street
    ## 58                                                                     306A West 2128th Street
    ## 59                                                                              340 Bay Street
    ## 60                                                                        386 Marlborough Road
    ## 61                                                                        3920 Paulding Avenue
    ## 62                                                                      39th ave and Bell Blvd
    ## 63                                                                        409 East 95th Street
    ## 64                                                                       4411 Arthur Kill Road
    ## 65                                                       4550 Carpenter Avenue, Bronx NY 10470
    ## 66                                                                      4750 White Plains Road
    ## 67                                                                    484 Knickerbocker Street
    ## 68                                                                       489 East 169th Street
    ## 69                                                                                48th Precint
    ## 70                                                                           50 Belmont Avenue
    ## 71                                                                                  50 Kent St
    ## 72                                                                      500 Weest 159th Street
    ## 73                                                                       535 East 141st Street
    ## 74                                       759 Jennings Street (Between Union & Prospect Avenue)
    ## 75                                                                         771 Fairmount Place
    ## 76                                                               97th Street Block Association
    ## 77                                                                                  A.R.R.O.W.
    ## 78                                                                      A.R.R.O.W. Field House
    ## 79                                                                  Abraham Lincoln Playground
    ## 80                                                             Academy Street & Sherman Avenue
    ## 81                                                                Addabbo Park ( Tudor Field )
    ## 82                                                                          Addabbo Playground
    ## 83                                                                    Agnes Haywood Playground
    ## 84                                                                         Al Mauro Playground
    ## 85                                                                Al Oerter  Recreation Center
    ## 86                                                                 Al Oerter Recreation Center
    ## 87                                                                    Al Oerter Soccer field 8
    ## 88                                                                      Al Quinones Playground
    ## 89                                                               Alexander Hamilton Playground
    ## 90                                                                             Alfred E. Smith
    ## 91                                                                  Alfred E. Smith Playground
    ## 92                                                           Alfred E. Smith Recreation Center
    ## 93                                                                 Allen F Kivlehan Playground
    ## 94                                                                         Allerton Playground
    ## 95                                                                 Alley Pond Adventure Course
    ## 96                                                                             Alley Pond Park
    ## 97                                                                           Allison Pond Park
    ## 98                                                                           Almeda Playground
    ## 99                                                                             Ambrosini Field
    ## 100                                                                         Amelia Gorman Park
    ## 101                                                         American Museum of Natural History
    ## 102                                                                        American Playground
    ## 103                                                            American Veterans Memorial Pier
    ## 104                                                                             Amersfort Park
    ## 105                                                               Amistad Dual Language School
    ## 106                                                                  Andrew Haswell Green Park
    ## 107                                                                                   Annadale
    ## 108                                                                     Anne Loftus Playground
    ## 109                                                                          Annunciation Park
    ## 110                                                                    Annunciation Playground
    ## 111                                                                                   Aquaduct
    ## 112                                                                        Aqueduct Playground
    ## 113                                                                              Aqueduct Walk
    ## 114                                                              Archbishop Molloy High School
    ## 115                                                                        Archie Spigner Park
    ## 116                                                                        Arrochar Playground
    ## 117                                                                                    Arsenal
    ## 118                                                                       Arsenal Central Park
    ## 119                                                               Arverne East Nature Preserve
    ## 120                                                                         Arverne Playground
    ## 121                                                                      Asser Levy Playground
    ## 122                                                               Asser Levy Recreation Center
    ## 123                                                                  Astoria Health Playground
    ## 124                                                                            Astoria Heights
    ## 125                                                                       Astoria Heights Park
    ## 126                                                                 Astoria Heights Playground
    ## 127                                                                               Astoria Park
    ## 128                                                                               Astoria Pool
    ## 129                                                                              Athens Square
    ## 130                                                            Augustus St. Gaudens Playground
    ## 131                                                              Austin J. McDonald Playground
    ## 132                                                   Austin J. McDonald Playground Fieldhouse
    ## 133                                                                          Bailey Playground
    ## 134                                                                     Baisley Park Extension
    ## 135                                                                               Baisley Pond
    ## 136                                                                          Baisley Pond Park
    ## 137                                                                            Barnard College
    ## 138                                                           Barnes Av between E 216 & 217 St
    ## 139                                                                               Barrett Park
    ## 140                                                                        Barretto Point Park
    ## 141                                                                             Baruch College
    ## 142                                                                          Baruch Playground
    ## 143                                                                            Basketball City
    ## 144                                                                        Bathgate Playground
    ## 145                                                                          Battery Park City
    ## 146                                                                     Bay Terrace Playground
    ## 147                                                                             Bayside Fields
    ## 148                                                                         Bayside Playground
    ## 149                                                                             Bayswater Park
    ## 150                                                             Beach 108th street Hocley Rink
    ## 151                                                                        Beach 17 Playground
    ## 152                                                                          Beach 17th Street
    ## 153                                                               Beach 30th Street Playground
    ## 154                                                                          Beach 32nd Street
    ## 155                                                          Beach 32nd Street Football Fields
    ## 156                                                                   Beach 59th St Playground
    ## 157                                                                          Beach 59th Street
    ## 158                                                                          Beach 82nd Street
    ## 159                                                                             Beach 94 Plaza
    ## 160                                                                              Beach 94th St
    ## 161                                                                          Beach 94th Street
    ## 162                                                                        Beach Channel Drive
    ## 163                                                                         Beach Channel Park
    ## 164                                                                       Beanstalk Playground
    ## 165                                                            Bed Stuy New Beginnings Charter
    ## 166                                                                         Behagen Playground
    ## 167                                                                           Bella Abzug Park
    ## 168                                                                        Bellevue South Park
    ## 169                                                                              Belmont Field
    ## 170                                                                                 Ben Abrams
    ## 171                                                                      Ben Abrams Playground
    ## 172                                                                               Bennett Park
    ## 173                                                                           Bensonhurst Park
    ## 174                                                                           Betsey Head Park
    ## 175                                                                            Betsy Head Park
    ## 176                                                                 Bicentennial Memorial Park
    ## 177                                                            Big Egg Marsh Beach 88th Street
    ## 178                                                         Bill Bojangles Robinson Playground
    ## 179                                                                      Bill Brown Playground
    ## 180                                                                                Bill Rainey
    ## 181                                                                           Bill Rainey Park
    ## 182                                                                     Blake Hobbs Playground
    ## 183                                                                          Bloomingdale Park
    ## 184                                                                    Bloomingdale Playground
    ## 185                                                                   Blue Heron Nature Center
    ## 186                                                                            Blue Heron Park
    ## 187                                                                       Booker T. Washington
    ## 188                                                            Booker T. Washington Playground
    ## 189                                                                               Borough Hall
    ## 190                                                                                 Bowne Park
    ## 191                                                                           Bowne Playground
    ## 192                                                                           Brady Playground
    ## 193                                                                            Breininger Park
    ## 194                                                                        Brevoort Playground
    ## 195                                                 Brigadier General Charles Young Playground
    ## 196                                                                        Brighton Playground
    ## 197                                                                              Broad Channel
    ## 198                                                                Broad Channel American Park
    ## 199                                                                         Broad Channel Park
    ## 200                                                                   Broad Channel Playground
    ## 201                                                       Broadway, Times Square, Union Square
    ## 202                                                             Bronx Borough Office - Ranaqua
    ## 203                                                                    Bronx Children's Museum
    ## 204                                                                    Bronx Community College
    ## 205                                                                                 Bronx Park
    ## 206                                                                 Bronx Park East Playground
    ## 207                                                                        Bronx River Parkway
    ## 208                                                                      Bronxdale High School
    ## 209                                                  Brook Avenue, between 137th St & 138th St
    ## 210                                                                                 Brook Park
    ## 211                                                                            Brookfield Park
    ## 212                                             Brooklyn Bears Carlton Avenue Community Garden
    ## 213                                                                    Brooklyn Botanic Garden
    ## 214                                                                       Brooklyn Bridge Park
    ## 215                                                                 Brooklyn Children's Museum
    ## 216                                                                           Brooklyn College
    ## 217                                                                    Brooklyn Public Library
    ## 218                                                  Brooklyn Public Library - Paedegat Branch
    ## 219                                                             Brooklyn Technical High School
    ## 220                                                                            Brookville Park
    ## 221                                                                                Brower Park
    ## 222                                                                     Brownsville Playground
    ## 223                                                              Brownsville Recreation Center
    ## 224                                                                                     Bufano
    ## 225                                                                                Bufano Park
    ## 226                                                                                Buono Beach
    ## 227                                                                           Burns Playground
    ## 228                                                 Burnside Ave between Morris Av & Walton Av
    ## 229                                                                         Bush Terminal Park
    ## 230                                                                    Bush-Clinton Playground
    ## 231                                                                        Bushwick Inlet Park
    ## 232                                                                              Bushwick Pool
    ## 233                                                                                        CBM
    ## 234                                                                      CBM Recreation Center
    ## 235                                                                         CPL. Thompson Park
    ## 236                                                              CUNY City College of New York
    ## 237                                                                       CUNY Graduate Center
    ## 238                                                                       CUNY: Lehman College
    ## 239                                                                          Cadman Plaza Park
    ## 240                                                                          Calvert Vaux Park
    ## 241                                                                            Cambria Heights
    ## 242                                                                         Cambria Playground
    ## 243                                                                              Canarsie Park
    ## 244                                                                  Captain Rivera Playground
    ## 245                                                                         Captain Tilly Park
    ## 246                                                                         Cardozo Playground
    ## 247                                                                           Carl Schurz Park
    ## 248                                                                          Carmansville Park
    ## 249                                                                    Carmansville Playground
    ## 250                                                                               Carroll Park
    ## 251                                                                         Caserta Playground
    ## 252                                                                     Castle Hill Playground
    ## 253                                                                           Castle Hill YMCA
    ## 254                                                                           Castle hill YMCA
    ## 255                                                                                Cedar Grove
    ## 256                                                                         Cedar Grove Meadow
    ## 257                                                                           Cedar Grove Park
    ## 258                                                                     Cedar Grove Playground
    ## 259                                                                           Cedar Playground
    ## 260                                                                               Central Park
    ## 261                                                                 Central Park: Davis Center
    ## 262                                                                     Centreville Playground
    ## 263                                                                         Century Playground
    ## 264                                                                              Chelsea Green
    ## 265                                                                               Chelsea Park
    ## 266                                                                  Chelsea Recreation Center
    ## 267                                                                           Cherry Tree Park
    ## 268                                           Chinatown, Division St between Bowery and Market
    ## 269                                                                                  Claremont
    ## 270                                                                             Claremont Park
    ## 271                                                                  Claremont Park Playground
    ## 272                                                                          Clason Point Park
    ## 273                                                                         Clawson Playground
    ## 274                                                                       Cleopatra Playground
    ## 275                                                                           Clove Lakes Park
    ## 276                                                                     Clove Lakes Playground
    ## 277                                                            Co-Op City (Section 1 Greenway)
    ## 278                                                                           Co-op City Field
    ## 279                                                                                Coffey Park
    ## 280                                                                      Col. Young Playground
    ## 281                                                                          Colden Playground
    ## 282                                                                         Coleman Playground
    ## 283                                                                          Collect Pond Park
    ## 284                                                                   College of Staten Island
    ## 285                                                             Colonel Charles Young Triangle
    ## 286                                                            Colonel David Marcus Playground
    ## 287                                                                         Colucci Playground
    ## 288                                      Columbia University - Jerome L. Greene Science Center
    ## 289                                                                             Columbus  Park
    ## 290                                                                              Columbus Park
    ## 291                                                                         Columbus and 104th
    ## 292                                                                            Commodore Barry
    ## 293                                                                       Commodore Barry Park
    ## 294                                                                            Commodore barry
    ## 295                                                                           Community Center
    ## 296                                                                           Conch Playground
    ## 297                                                         Concrete Park/Bronx River Alliance
    ## 298                                                                        Concrete Plant Park
    ## 299                                                             Coney Island Beach & Boardwalk
    ## 300                                                         Coney Island Beach &amp; Boardwalk
    ## 301                                                                    Coney Island Creek Park
    ## 302                                                                      Conference House Park
    ## 303                                                   Constance Baker Motley Recreation Center
    ## 304                                                                                Cooper Park
    ## 305                                                                                    Cordozo
    ## 306                                                                         Corlears Hook Park
    ## 307                                                                   Corlears Hook Playground
    ## 308                                                                                Corona Golf
    ## 309                                                                     Corona Golf Playground
    ## 310                                                      Corporal John A. Seravalli Playground
    ## 311                                                                     Corporal Thompson Park
    ## 312                                                              Courtney Callender Playground
    ## 313                                                                   Cpl. Thompson Playground
    ## 314                                                                 Crispus Attucks Playground
    ## 315                                                                             Crocheron Park
    ## 316                                                                      Crotona Nature Center
    ## 317                                                                               Crotona Park
    ## 318                                                                 Crotona Park Nature Center
    ## 319                                                                               Crotona Pool
    ## 320                                                                            Cunningham Park
    ## 321                                                                   Cypress Hills Playground
    ## 322                                                             Daniel M. O'Connell Playground
    ## 323                                                                        Davidson Playground
    ## 324                                                                          Dawson Playground
    ## 325                                                                              De Matti Park
    ## 326                                                                        De Matti Playground
    ## 327                                                                       De Witt Clinton Park
    ## 328                                                                        DeKovats Playground
    ## 329                                                                    DeMatti Park Fieldhouse
    ## 330                                                                         DeMatti Playground
    ## 331                                                               Delphin H. Greene Playground
    ## 332                                                                     Det. Russel Timoshenko
    ## 333                                                        Det. Russel Timoshenko Soccer Field
    ## 334                                                            Detective Keith L Williams Park
    ## 335                                                                   Detective Keith Williams
    ## 336                                                   Detective Russel Timoshenko Soccer Field
    ## 337                                                                                 Devoe Park
    ## 338                                                                                Division St
    ## 339                                                                       Division St & Bowery
    ## 340                                                                         Douglas and Degraw
    ## 341                                                                   Douglass and DeGraw Pool
    ## 342                                                                  Downing Street Playground
    ## 343                                                                   Dr. Charles R. Drew Park
    ## 344                                                                       Dr. Green Playground
    ## 345                                                            Dr. Ronald E. McNair Playground
    ## 346                                                                            Drew Playground
    ## 347                                                                                   Dry Dock
    ## 348                                                                        Dry Dock Playground
    ## 349                                                                          Dunbar Playground
    ## 350                                                                     Dutch Kills Playground
    ## 351                                                                       Dyckman House Museum
    ## 352                                                                           Dyker Beach Park
    ## 353                                                             Dyker Beach Park -bad stations
    ## 354                                           East 172nd STreet, btwn Third & Bathgate Avenues
    ## 355                                                                   East Elmhurst Playground
    ## 356                                                                   East End Ave. & 86th St.
    ## 357                                                                       East River Esplanade
    ## 358                                                                            East River Park
    ## 359                                                                      East River Playground
    ## 360                                                            East River Waterfront Esplanade
    ## 361                                                                     Eastchester Playground
    ## 362                                                                  Edenwald Community Center
    ## 363                                                                        Edenwald Playground
    ## 364                                                                               Edward Byrne
    ## 365                                                               Ehrenreich-Austin Playground
    ## 366                                                               Ehrenreich/Austin Playground
    ## 367                                                                        Electric Playground
    ## 368                                                                         Ellis Prep Academy
    ## 369                                                                                   Elmhurst
    ## 370                                                                              Elmhurst Park
    ## 371                                                                          Equity Playground
    ## 372                                                                        Eugene McCabe Field
    ## 373                                                                             Evergreen Park
    ## 374                                                                                  Ewen Park
    ## 375                                                          FDNY-EMT Yadira Arroyo Playground
    ## 376                                                               Faber Park Recreation Center
    ## 377                                                                                 Faber Pool
    ## 378                                                                        Faber Pool and Park
    ## 379                                                                              Fairview Park
    ## 380                                                       Fannie Lou Hamer Freedom High School
    ## 381                                                                   Far Rockaway High School
    ## 382                                                                     Far Rockaway, Beach 17
    ## 383                                                                              Father Macris
    ## 384                                                                         Father Macris Park
    ## 385                                                                           Ferry Point Park
    ## 386                                                                  Fidler-Wyckoff House Park
    ## 387                                                                                 First Park
    ## 388                                                                                Fisher Pool
    ## 389                                                                        Floyd Bennett Field
    ## 390                                                                            Flushing Fields
    ## 391                                                               Flushing Meadows Corona Park
    ## 392                                               Flushing Meadows Corona Park Aquatics Center
    ## 393                                                   Flushing Meadows Corona Park Pool & Rink
    ## 394                                                                           Flynn Playground
    ## 395                                                                              Fordham Plaza
    ## 396                                                                   Forest Hills High School
    ## 397                                                                 Forest Hills Jewish Center
    ## 398                                                                                Forest Park
    ## 399                                                                      Forest Park Bandshell
    ## 400                                                                         Fort #4 Playground
    ## 401                                                                           Fort Greene Park
    ## 402                                                                 Fort Greene Visitor Center
    ## 403                                                               Fort Hamilton Athletic Field
    ## 404                                                            Fort Hamilton Recreation Center
    ## 405                                                                Fort Hamilton Senior Center
    ## 406                                                     Fort Hamilton Senior Recreation Center
    ## 407                                                               Fort Independence Playground
    ## 408                                                               Fort Independence playground
    ## 409                                                                                Fort Totten
    ## 410                                                                           Fort Totten Park
    ## 411                                                                            Fort Tryon Park
    ## 412                                                                       Fort Washington Park
    ## 413                                                               Fountain Of Youth Playground
    ## 414                                                                                   Fox Park
    ## 415                                                                             Fox Playground
    ## 416                                                                         Francis Lewis Park
    ## 417                                                                     Francis Martin Library
    ## 418                                                               Frank D. O'Connor Playground
    ## 419                                                                         Frank Frisch Field
    ## 420                                                                        Frank Principe Park
    ## 421                                                            Franklin D. Roosevelt Boardwalk
    ## 422                                                  Franklin D. Roosevelt Boardwalk and Beach
    ## 423                                     Franklin D. Roosevelt Boardwalk and Beach, South Beach
    ## 424                                                                    Franz Siegel Playground
    ## 425                                                                           Franz Sigel Park
    ## 426                                                                     Fred Samuel Playground
    ## 427                                                              Frederick B. Judge Playground
    ## 428                                                                 Frederick Douglass Academy
    ## 429                                                  Frederick Douglass Academy VI High School
    ## 430                                                              Frederick Douglass Playground
    ## 431                                                                     Frederick Johnson Park
    ## 432                                                               Frederick Johnson Playground
    ## 433                                                    Fredrick Douglas Academy VI High School
    ## 434                                                                         Fresh Meadows Park
    ## 435                                                                            Freshkills Park
    ## 436                                                                               Frisch Field
    ## 437                                                                         Galileo Playground
    ## 438                                                                          Garden by the Bay
    ## 439                                                               Gateway NPS Great Kills Park
    ## 440                                                Gateway National Rec Area: Great Kills Park
    ## 441                                                                 Gerard P. Dugan Playground
    ## 442                                                                           Gertrude Ederele
    ## 443                                                                 Gertrude Ederle Playground
    ## 444                                                          Gertrude Ederle Recreation Center
    ## 445                                                                 Gertrude Ederle Turf Field
    ## 446                                                                        Glendale Playground
    ## 447                                                                        Glenwood Playground
    ## 448                                                                                     Gorman
    ## 449                                                                                Gorman Park
    ## 450                                                                          Gorman Playground
    ## 451                                                    Grace Episcopal Church /Rufus King Park
    ## 452                                                                             Gracie Mansion
    ## 453                                                                       Graduate Center CUNY
    ## 454                                                                           Grady Playground
    ## 455                                                                           Grand Ferry Park
    ## 456                                                                           Grand Playground
    ## 457                                                                                 Grant Park
    ## 458                                                                           Great Kills Park
    ## 459                                                                    Greenbelt Nature Center
    ## 460                                                                Greenbelt Recreation Center
    ## 461                                                                      Greencroft Playground
    ## 462                                                                  Grenada Place and E 231st
    ## 463                                                                           Grover Cleveland
    ## 464                                                                      Grover Cleveland Park
    ## 465                                                                Grover Cleveland Playground
    ## 466                                                                            Gun Hill Meadow
    ## 467                                                                       Gutenberg Playground
    ## 468                                                                                     Haffen
    ## 469                                                                                Haffen Park
    ## 470                                                                                Haffen Pool
    ## 471                                                                     Half-Nelson Playground
    ## 472                                                                    Hallets Cove Playground
    ## 473                                                                         Hamilton Fish Park
    ## 474                                                                   Hamilton Fish Playground
    ## 475                                                                         Hamilton Fish Pool
    ## 476                                                            Hamilton Fish Recreation Center
    ## 477                                                                    Hamilton Heights School
    ## 478                                                                          Hammel Playground
    ## 479                                                                               Hancock Park
    ## 480                                                              Hansborough Recreation Center
    ## 481                                                                   Happy Warrior Playground
    ## 482                                                                               Harding Park
    ## 483                                                                            Harlem Art Park
    ## 484                                                             Harlem Open Streets West 127th
    ## 485  Harlem Open Streets: 127th street, between Adam Clayton Powell and Fredrick Douglas Blvds
    ## 486                                                                          Harlem River Park
    ## 487                                                             Harold Schneiderman Playground
    ## 488                                                                 Harriet Tubman School Yard
    ## 489                                                                       Harris Brothers Park
    ## 490                                                                                Harris Park
    ## 491                                                                      Harry Maze Playground
    ## 492                                                                  Hattie Carthan Playground
    ## 493                                                                                Healy Field
    ## 494                                                                       Heckscher Playground
    ## 495                                                                  Helen Marshall Playground
    ## 496                                                                        Hell's Kitchen Park
    ## 497                                                                     Hendrick I. Lott House
    ## 498                                                                          Henry Hudson Park
    ## 499                                                                Henry M. Jackson Playground
    ## 500                                                             Herbal Garden of East New York
    ## 501                                                      Herbert Von King Cultural Arts Center
    ## 502                                                                      Herbert Von King Park
    ## 503                                                                   Herman Dolgon Playground
    ## 504                                                                             High Rock Park
    ## 505                                                                            Highbridge Park
    ## 506                                                                    Highbridge Park (Bronx)
    ## 507                                                                       Highbridge Pool Deck
    ## 508                                                               Highbridge Recreation Center
    ## 509                                                                              Highland Park
    ## 510                                                            Highland Park Children's Garden
    ## 511                                                                         Hilltop Playground
    ## 512                                                                                Hinton Park
    ## 513                                                                               Hoffman Park
    ## 514                                                                       Holcombe Rucker Park
    ## 515                                                                       Homecrest Playground
    ## 516                                                                            Hook Creek Park
    ## 517                                                                Hoover - Manton Playgrounds
    ## 518                                                                   Hoover Manton Playground
    ## 519                                                                  Horace Harding Playground
    ## 520                                                                  Howard Bennett Playground
    ## 521                                                                                Howard Pool
    ## 522                                                                            Hoyt Playground
    ## 523                                                                                Hudson Park
    ## 524                                                                          Hudson River Park
    ## 525                                          Huns Point, Williamsbridge Oval & St. Mary's Park
    ## 526                                                                  Hunter's Point South Park
    ## 527                                                                   Hunters Point Park South
    ## 528                                                                   Hunters Point South Park
    ## 529                                                                     Hunts Point Playground
    ## 530                                                              Hunts Point Recreation Center
    ## 531                                      Hunts Point Recreation Center/Julio Ca baseball field
    ## 532                                                                 Hunts Point Riverside Park
    ## 533                                                                                  I-Am-Park
    ## 534                                                                                   I.S 126Q
    ## 535                                                                                    I.S. 62
    ## 536                                                                                    IS 126Q
    ## 537                                                                            IS 141 Steinway
    ## 538                                                                                      IS 49
    ## 539                                                                              Idlewild Park
    ## 540                                                                     Imagination Playground
    ## 541                                                       Imagination Playground at Betsy Head
    ## 542                                                                            In-School Bronx
    ## 543                                                                           In-School Queens
    ## 544                                                                          Inspiration Plaza
    ## 545                                              International High School for Health Sciences
    ## 546                                                                  Inwood Hill Nature Center
    ## 547                                                                           Inwood Hill Park
    ## 548                                                                         Irving Square Park
    ## 549                                                                        J. Hood Wright Park
    ## 550                                                           J. Hood Wright Recreation Center
    ## 551                                                                   JCC Gerard Carter Center
    ## 552                                                                            Jackie Robinson
    ## 553                                                                  Jackie Robinson Bandshell
    ## 554                                                                       Jackie Robinson Park
    ## 555                                                          Jackie Robinson Recreation Center
    ## 556                                                                 Jacob H. Schiff Playground
    ## 557                                                                Jamaica Bay Wildlife Refuge
    ## 558                                                                         Jamaica Playground
    ## 559                                                      James Baldwin Outdoor Learning Center
    ## 560                                                                     James Burke Ballfields
    ## 561                                                                        James J Walker Park
    ## 562                                                            James Weldon Johnson Playground
    ## 563                                                                        Jennifer Playground
    ## 564                                                                      Jennifer's Playground
    ## 565                                                            Jeroma L. Greene Science Center
    ## 566                                                             Jerome L Greene Science Center
    ## 567                                                            Jerome L. Greene Science Center
    ## 568                                                                                Jerome Park
    ## 569                                                                      Jersey St. & Pauw St.
    ## 570                                                                     Jesse Owens Playground
    ## 571                                                          Jewish Community Center- Bernikow
    ## 572                                                                              John Bowne HS
    ## 573                                                                     John Bowne High School
    ## 574                                                            John Ericsson Middle School 126
    ## 575                                                                                John Golden
    ## 576                                                                           John Golden Park
    ## 577                                                                    John Hancock Playground
    ## 578                                                                          John J Carty Park
    ## 579                                                                              John Jay Park
    ## 580                                                                              John Jay Pool
    ## 581                                                                       John Paul Jones Park
    ## 582                                                            John V. Lindsay East River Park
    ## 583                                                                           Jones Woods Park
    ## 584                                                                   Joseph Austin Playground
    ## 585                                                              Josephine Caminiti Playground
    ## 586                                                                          Joyce Kilmer Park
    ## 587                                                                       Juan Caraballo Field
    ## 588                                                           Judge Moses Weinstein Playground
    ## 589                                                                             Julio Carballo
    ## 590                                                                      Julio Carballo Fields
    ## 591                                                                        Junction Playground
    ## 592                                                                        Juniper Valley Park
    ## 593                                                                                Kaiser Park
    ## 594                                                                                 Kelly Park
    ## 595                                                                      Kelly Park Playground
    ## 596                                                                            Kett Playground
    ## 597                                                                           Kissena Corridor
    ## 598                                                                      Kissena Corridor Park
    ## 599                                                                               Kissena Park
    ## 600                                                                   Kissena Park Field House
    ## 601                                                                    Kissena Park Playground
    ## 602                                                                         Kissena Playground
    ## 603                                                                            Kosciuszko Pool
    ## 604                                                               Kwama Ture Recreation Center
    ## 605                                                               Kwame Ture Recreation Center
    ## 606                                                      L/CPL Thomas P. Noonan Jr. Playground
    ## 607                                                                          LORETO PLAYGROUND
    ## 608                                                                LaGuardia Community College
    ## 609                                                              LaTourette Park & Golf Course
    ## 610                                                             Lafayette Ave & Edgewater Road
    ## 611                                                                 Langston Hughes Playground
    ## 612                                                                       Laurelton Playground
    ## 613                                                              Lawrence Virgililo Playground
    ## 614                                                               Lawrence Virgilio Playground
    ## 615                                                                             Lehman College
    ## 616                                                           Lehman Highschool Campus Library
    ## 617                                                                          Leif Ericson Park
    ## 618                                                                           Lemon Creek Park
    ## 619                                                                                  Levy Park
    ## 620                                                                            Levy Playground
    ## 621                                                                                    Liberty
    ## 622                                                    Lieutenant John H. Martinson Playground
    ## 623                                                                  Lillian D Wald Playground
    ## 624                                                                           Lincoln Hospital
    ## 625                                                    Lincoln Terrace / Arthur S. Somers Park
    ## 626                                                                                Linden Park
    ## 627                                                                          Linden Playground
    ## 628                                                                            Little Bay Park
    ## 629                                                                Little Claremont Playground
    ## 630                                                                   Little Flower Playground
    ## 631                                          Locke Arts and Engineering School (20 W 112th st)
    ## 632                                                                London Planetree Playground
    ## 633                                                                      Long Island City YMCA
    ## 634                                                                                     Loreto
    ## 635                                                                                Loreto Park
    ## 636                                                                          Loreto Playground
    ## 637                                                                        Lost Battalion Hall
    ## 638                                                      Lost Battalion Hall Recreation Center
    ## 639                                                                        Louis C. Moser Park
    ## 640                                                                         Louis Pasteur Park
    ## 641                                                                          Lozada Playground
    ## 642                                                                  Lt. Joseph Petrosino Park
    ## 643                                                                         Lt. Lia Playground
    ## 644                                                                         Luther Gulick Park
    ## 645                                                                                 Lyons Pool
    ## 646                                                                    Lyons Recreation Center
    ## 647                                                                          Lyons Square Park
    ## 648                                                                                     MS 354
    ## 649                                                                   Mabel Hampton Playground
    ## 650                                                                             MacDonald Park
    ## 651                                                                               Macneil Park
    ## 652                                                                           Macombs Dam Park
    ## 653                                                                     Macombs Dam Playground
    ## 654                                                   Madison Square Garden Hotel Pennsylvania
    ## 655                                                                        Madison Square Park
    ## 656                                                                       Mae Grant Playground
    ## 657                                                                          Mafera Playground
    ## 658                                                                               Mahoney Park
    ## 659                                                                         Mahoney Playground
    ## 660                                                                       Manhattan Beach Park
    ## 661                                                                Manhattan Beach Parking Lot
    ## 662                                                     Manhattan Borough Office -Arsenal West
    ## 663                                                                                      Mapes
    ## 664                                                                Marc And Jason's Playground
    ## 665                                                                               Marconi Park
    ## 666                                                                    Marcus Garvey Ampthater
    ## 667                                                                         Marcus Garvey Park
    ## 668                                                                           Marcy Playground
    ## 669                                                                            Maria Hernandez
    ## 670                                                                       Maria Hernandez Park
    ## 671                                                                     Marie Curie Playground
    ## 672                                                                                Marine Park
    ## 673                                                                Marion Hopkinson Playground
    ## 674                                                                Martin Luther King Jr. Park
    ## 675                                                          Martin Luther King Jr. Playground
    ## 676                                                                    Martin Luther King Plgd
    ## 677                                                         Martin Luther King, Jr. Playground
    ## 678                                                          Martin Luther Kings JR Playground
    ## 679                                                                Martin Van Buren Playground
    ## 680                                                                  Marx Brother's Playground
    ## 681                                                                   Marx Brothers Playground
    ## 682                                                                  Mathews-Palmer Playground
    ## 683                                                              Matthew P. Sapolin Playground
    ## 684                                                               Matthews - Palmer Playground
    ## 685                                                                Matthews Muliner Playground
    ## 686                                                            Maurice A FitzGerald Playground
    ## 687                                                                                      Mauro
    ## 688                                                                                 Mauro Park
    ## 689                                                                           Mauro Playground
    ## 690                                                                          Mazzei Playground
    ## 691                                                Mc Nair Playground - E. 123rd  & 3rd Avenue
    ## 692                                                                       McCaffrey Playground
    ## 693                                                                              McCarren Park
    ## 694                                                                         McCarren Park Pool
    ## 695                                                                       McCarren Play Center
    ## 696                                                                              McCarren Pool
    ## 697                                                                         McCarren Pool Deck
    ## 698                                                                        McDonald Playground
    ## 699                                                                             McGolrick Park
    ## 700                                                                              McKinley Park
    ## 701                                                                        McKinley Playground
    ## 702                                                                       Medger Evers College
    ## 703                                                                         Mellett Playground
    ## 704                                                                       Melrose Commons Park
    ## 705                                                                         Melrose Playground
    ## 706                                                                         Merriam Playground
    ## 707                                                                         Metcalf Playground
    ## 708                                                      Metropolitan Pool & Recreation Center
    ## 709                                                             Metropolitan Recreation Center
    ## 710                                                                   Metropolotin High School
    ## 711                                                                              Midland Beach
    ## 712                                                                              Midland Field
    ## 713                                                                             Mill Pond Park
    ## 714                                                                       Millbrook Playground
    ## 715                                                                         Minetta Playground
    ## 716                                                                           Montbellier Park
    ## 717                                                                          Montefiore Square
    ## 718                                                                 Moore Homestead Playground
    ## 719                                                                           Moore Playground
    ## 720                                                                           Morningside Park
    ## 721                                                                          Morton Playground
    ## 722                                                                            Mosholu Parkway
    ## 723                                                                            Mott Playground
    ## 724                                                                      Mount Hope Playground
    ## 725                                                                       Msgr. McGolrick Park
    ## 726                                                                                    Mullaly
    ## 727                                                                               Mullaly Park
    ## 728                                                                          Mullaly Park Pool
    ## 729                                                                               Mullaly Pool
    ## 730                                                                       Mullaly's Skate Park
    ## 731                                                                              Mullalys Pool
    ## 732                                                              Murphy's Brother's Playground
    ## 733                                                                          Murray Playground
    ## 734                                                     Museo Del Barrio (104th and Fifth Ave)
    ## 735                                                                  Musuem of Natural History
    ## 736                                                                          Naples Playground
    ## 737                                                                   Nathan Straus Playground
    ## 738                                                                        Nautilus Playground
    ## 739                                                                          Nelson Playground
    ## 740                                                      Nest + m School : 111 Columbia Street
    ## 741                                                         New Heights Academy Charter School
    ## 742                                                                        New York University
    ## 743                                                                         Newtown Playground
    ## 744                                                                           Noble Playground
    ## 745                                                                  North Rochdale Playground
    ## 746                                                                      North Shore Esplanade
    ## 747                                                                        Northern Playground
    ## 748                                                                        Nostrand Playground
    ## 749                                                                    O'Donahue/Beach 17th St
    ## 750                                                                             O'Donohue Park
    ## 751                                                              ODonahue Park -Rockaway beach
    ## 752                                                                               Oakland Lake
    ## 753                                                              Ocean Breeze Athletic Complex
    ## 754                                              Ocean Breeze Athletic Complex Parking Lot # 5
    ## 755                                                                          Ocean Breeze Park
    ## 756                                                Ocean Breeze Track & Field Athletic Complex
    ## 757                                                                               Ochard Beach
    ## 758                                                                          Offsite Locations
    ## 759                                                                    Olmsted-Beil House Park
    ## 760                                                          One Whitehall Street : NAC Office
    ## 761                                                                              Orchard Beach
    ## 762                                                                Orchard Beach Nature Center
    ## 763                                                                          Osborn Playground
    ## 764                                                               Owen Dolen Recreation Center
    ## 765                                                                            Owl's Head Park
    ## 766                                                                          Owlâ€™s Head Park
    ## 767                                                                            Owl’s Head Park
    ## 768                                                                                  P. S. 92Q
    ## 769                                                           P.O. Reinaldo Salgado Playground
    ## 770                                                                         P.O. Serrano  Park
    ## 771                                                                    P.O. Serrano Playground
    ## 772                                                                                    P.S 107
    ## 773                                                                                     P.S 18
    ## 774                                                                                    P.S 195
    ## 775                                                                          P.S. 014 Fairview
    ## 776                                                                                   P.S. 145
    ## 777                                                                        P.S. 155 Playground
    ## 778                                                                   P.S. 176 Cambria Heights
    ## 779                                                        P.S. 185 Locke Arts and Engineering
    ## 780                                                                                   P.S. 249
    ## 781                                                                                   P.S. 276
    ## 782                                                                                   P.S. 51X
    ## 783                                                                                   P.S. 55X
    ## 784                                                                                    P.S. 89
    ## 785                                                                              P.S./I.S. 208
    ## 786                                                                               P.S./I.S.323
    ## 787                                                                              P.S./M.S. 42Q
    ## 788                                                                                     P.S.54
    ## 789                                                            PS 055 Benjamin Franklin School
    ## 790                                                                                     PS 145
    ## 791                                                                                     PS 14Q
    ## 792                                                                                    PS 165Q
    ## 793                                                                                    PS 175X
    ## 794                                                                                    PS 178Q
    ## 795                                                                                     PS 192
    ## 796                                                                                     PS 196
    ## 797                                                                                     PS 21X
    ## 798                                                                                     PS 221
    ## 799                                                                                     PS 251
    ## 800                                                                                      PS 29
    ## 801                                                                                     PS 323
    ## 802                                                                                      PS 38
    ## 803                                                                                      PS 45
    ## 804                                                                                      PS 50
    ## 805                                                                                      PS 52
    ## 806                                                                                      PS 54
    ## 807                                                                                     PS 56Q
    ## 808                                                                                      PS 57
    ## 809                                                                                      PS 65
    ## 810                                                                                      PS 90
    ## 811                                                                                     PS 92Q
    ## 812                                                                                      PS 94
    ## 813                                                                        PS/IS 116Q  (T4/T5)
    ## 814                                                                PS/IS 178 Holliswood School
    ## 815                                                                                  PS/IS 49Q
    ## 816                                                                                       PS13
    ## 817                                                                        PSMS 278 Career Day
    ## 818                                                                             Paerdegat Park
    ## 819                                                                              Parade Ground
    ## 820                                                                       Park Of The Americas
    ## 821                                                                       Park of the Americas
    ## 822                                                                                 Parks@Home
    ## 823                                                                        Parkside Playground
    ## 824                                                                           Patterson Houses
    ## 825                                                                       Patterson Playground
    ## 826                                                                              Paul Raimonda
    ## 827                                                                   Paul Raimonda Playground
    ## 828                                                                          Payson Park House
    ## 829                                                                          Payson Playground
    ## 830                                                                        Pearl St Playground
    ## 831                                                                            Pelham Bay Park
    ## 832                                                     Pelham Bay and Split Rock Golf Courses
    ## 833                                                                 Pelham Fritz Marcus Garvey
    ## 834                                                             Pelham Fritz Recreation Center
    ## 835                                                                             Pelham Parkway
    ## 836                                                                          Pena Herrera Park
    ## 837                                                                      Penn South Playground
    ## 838                                                                              People's Park
    ## 839                                                                   People's Park Playground
    ## 840                                                                    Peter Minuit Playground
    ## 841                                                                              Peter's Field
    ## 842                                                                               Peters Field
    ## 843                                                                Phil "Scooter" Rizzuto Park
    ## 844                                                                  Phil Scooter Rizzuto Park
    ## 845                                                                  Phyllis Post Goodman Park
    ## 846                                                                                    Pier 35
    ## 847                                                                                    Pier 42
    ## 848                                                                                 Playground
    ## 849                                                                        Playground 103 CIII
    ## 850                                                                          Playground 52 LII
    ## 851                                                                              Playground 75
    ## 852                                                              Playground Eighty Nine LXXXIX
    ## 853                                                                          Playground Ninety
    ## 854                                                                             Playground One
    ## 855                                                          Playground One Thirty Four CXXXIV
    ## 856                                                            Playground One Twenty Five CXXV
    ## 857                                                                    Playground Seventy Five
    ## 858                                                                Playground for All Children
    ## 859                                                                Playground for all Children
    ## 860                                                          Pleasant Village Community Garden
    ## 861                                                                                   Poe Park
    ## 862                                                                    Poe Park Visitor Center
    ## 863                                                                    Poe Park Visitor center
    ## 864                                                            Police Office Edward Byrne Park
    ## 865                                                           Police Officer Edward Byrne Park
    ## 866                                                      Police Officer Nicholas Demutiis Park
    ## 867                                              Police Officer Russel Timoshenko Soccer Field
    ## 868                                                                         Pomonok Playground
    ## 869                                                                         Ponomok Playground
    ## 870                                                                         Pontiac Playground
    ## 871                                                                  Poor Richard's Playground
    ## 872                                                                     Poppenhusen Playground
    ## 873                                                                         Potomac Playground
    ## 874                                                                              Prospect Park
    ## 875                                                                        Prospect Playground
    ## 876                                    Public School 129  Poppenhusen Ave, College Point 11356
    ## 877                                                                           Public School 48
    ## 878                                                                               Pulaski Park
    ## 879                                                                        Queens Borough Hall
    ## 880                                                 Queens Borough Office -Passerelle Building
    ## 881                                                                    Queens Botanical Garden
    ## 882                                                                         Queens County Farm
    ## 883                                                                           Queens Farm Park
    ## 884                                          Queens HS for Information, Research, & Technology
    ## 885                                                                     Queens Hospital Center
    ## 886                                                                   Queens School of Inquiry
    ## 887                                                                                 Queens Zoo
    ## 888                                                                          Queensbridge Park
    ## 889                                              Rachel Carson High School for Coastal Studies
    ## 890                                                                                  Radnor Rd
    ## 891                                                                                Rainey Park
    ## 892                                                                          Ramon Aponte Park
    ## 893                                                                             Ranaqua Garage
    ## 894                                                                      Randall's Island Park
    ## 895                                                                Raoul Wallemberg Playground
    ## 896                                                                Raoul Wallenberg PLayground
    ## 897                                                                       Rappaport Playground
    ## 898                                                                 Raul Wallemberg Playground
    ## 899                                                                      Ravenswood Playground
    ## 900                                                                    Raymond Bush Playground
    ## 901                                                                       Real Good Playground
    ## 902                                                                       Recreation Center 54
    ## 903                                                                              Red Hook Park
    ## 904                                                                   Red Hook Recreation Area
    ## 905                                                                 Red Hook Recreation Center
    ## 906                                                                         Redfern Playground
    ## 907                                                                           Reiff Playground
    ## 908                                                                     Renaissance Playground
    ## 909                                                                     Rev. Dr. Maggie Howard
    ## 910                                                                 Rev. T Wendell Foster Park
    ## 911                                                                        Richman (Echo) Park
    ## 912                                                                          Rienzi Playground
    ## 913                                                                                 River Park
    ## 914                                                                       Riverdale Playground
    ## 915                                                                  Riverdale Y Sunday Market
    ## 916                                                                             Riverside Park
    ## 917                                                                          Robert Moses Park
    ## 918                                                                    Robert Moses Playground
    ## 919                                                                Roberto Clemente State Park
    ## 920                                                                Roberto clemente state park
    ## 921                                                                              Rochdale Park
    ## 922                                                                             Rockaway Beach
    ## 923                                                                          Rockaway Beach 59
    ## 924                                                  Rockaway Beach 94 Street Performance Area
    ## 925                                                               Rockaway Beach and Boardwalk
    ## 926                                                                    Rockaway Community Park
    ## 927                                                                          Roger Morris Park
    ## 928                                                                        Rory Staunton Field
    ## 929                                                                         Roscoe Brown Plaza
    ## 930                                                                             Rose Hill Park
    ## 931                                                                      Rosemary's Playground
    ## 932                                                                       Rosemarys Playground
    ## 933                                                                                Roy Wilkins
    ## 934                                                                           Roy Wilkins Park
    ## 935                                                              Roy Wilkins Recreation Center
    ## 936                                                                            Rufus King Park
    ## 937                                                                               Ruppert Park
    ## 938                                                                    Russell Sage Playground
    ## 939                                                             SI University Hospital Stadium
    ## 940                                                                        SIUH Community Park
    ## 941                                                                       Sachkerah Playground
    ## 942                                                                                Sakura Park
    ## 943                                                                   Salt Marsh Nature Center
    ## 944                                                         Samuel N. Bennerson 2nd Playground
    ## 945                                                                  Samuel Seabury Playground
    ## 946                                                                     Sara D. Roosevelt Park
    ## 947                                                                    Sara D. Roosevelt Track
    ## 948                                                                              Saratoga Park
    ## 949                                                                        Saw Mill Playground
    ## 950                                                                           Scarangella Park
    ## 951                                                                     Scarangella Playground
    ## 952                                                                                Schmul Park
    ## 953                                                                               Sean's Place
    ## 954                                                               Seaside Wildlife Nature Park
    ## 955                                                                        Sedgwick Playground
    ## 956                                                          Seneca Avenue and Corneila Street
    ## 957                                                                         Serrano Playground
    ## 958                                                         Seth Low Playground/ Bealin Square
    ## 959                                                                           Seton Falls Park
    ## 960                                                                                 Seton Park
    ## 961                                                                                Seward Park
    ## 962                                                                      Sheepshead Playground
    ## 963                                                                            Sheltering Arms
    ## 964                                                                 Sheltering Arms Playground
    ## 965                                                                              Sherman Creek
    ## 966                                                                         Sherman Creek Park
    ## 967                                                                Shirley Chisholm State Park
    ## 968                                                                              Shoelace Park
    ## 969                                                                     Shore Park and Parkway
    ## 970                                                                            Shore Road Park
    ## 971                                                                  Sidney Hillman Playground
    ## 972                                                                           Silver Lake Park
    ## 973                                                                Silver Lake Park Playground
    ## 974                                                                               Simeone Park
    ## 975                                                          Skating Day at Daniel Oâ€™Connell
    ## 976                                                            Skating Day at Daniel O’Connell
    ## 977                                                                         Skyline Playground
    ## 978                                                                        Slattery Playground
    ## 979                                                                                Snug Harbor
    ## 980                                                                Snug Harbor Cultural Center
    ## 981                                             Snug Harbor Cultural Center & Botanical Garden
    ## 982                                                                       Sobelsohn Playground
    ## 983                                                                    Socrates Sculpture Park
    ## 984                                                                       Sol Bloom Playground
    ## 985                                                                        Sol Lain Playground
    ## 986                                                                              Sol Lain Plgd
    ## 987                                                               Sorrentino Recreation Center
    ## 988                                                                                  Soundview
    ## 989                                                                             Soundview Park
    ## 990                                                                       Soundview Playground
    ## 991                                                                 South Bronx Charter School
    ## 992                                                                          South Oxford Park
    ## 993                                                                      Space Time Playground
    ## 994                                                                          Spring Creek Park
    ## 995                                                                           Springfield Park
    ## 996                                                                  Spuyten Duyvil Playground
    ## 997                                                                          St Andrews Church
    ## 998                                                                     St Andrews Summer camp
    ## 999                                                                                 St. Albans
    ## 1000                                                                           St. Albans Park
    ## 1001                                                                   St. Andrew's Playground
    ## 1002                                                                      St. Catherine's Park
    ## 1003                                                                  St. Gregory's Playground
    ## 1004                                                                                 St. James
    ## 1005                                                                            St. James Park
    ## 1006                                                               St. James Recreation Center
    ## 1007                                                                           St. John's Park
    ## 1008                                                              St. John's Recreation Center
    ## 1009                                                St. Julian St between Van Duzer and Bay st
    ## 1010                                                                                St. Mary's
    ## 1011                                                                           St. Mary's Park
    ## 1012                                                                      St. Mary's Park East
    ## 1013                                                                      St. Mary's Park West
    ## 1014                                                                    St. Mary's Parking Lot
    ## 1015                                                              St. Mary's Recreation Center
    ## 1016                                                               St. Mary's park Parking Lot
    ## 1017                                                                      St. Mary's park West
    ## 1018                                                                      St. Marys Playground
    ## 1019                                                                         St. Nicholas Park
    ## 1020                                                             St. Nicholas Playground North
    ## 1021                                                             St. Nicholas Playground South
    ## 1022                                                                           St. Vartan Park
    ## 1023                                                                          St. Vartans Park
    ## 1024                                                                                St. mary's
    ## 1025                                                                    St. mary's Parking lot
    ## 1026                                                                           St. mary's West
    ## 1027                                                                      St. mary's park West
    ## 1028                                                                 Stanley Isaacs Playground
    ## 1029                                                                      Stapleton Playground
    ## 1030                                                                  Star Spangled Playground
    ## 1031                                                                            Starlight Park
    ## 1032                                                  Starlight Park-Starlight Park Playground
    ## 1033                                                                      Starlight Playground
    ## 1034                                                                Stars & Stripes Playground
    ## 1035                                                                Staten Island Borough Hall
    ## 1036                                          Staten Island University Hospital Community Park
    ## 1037                                                                          Story Playground
    ## 1038                                                                         Stroud Playground
    ## 1039                                                                         Stuyvesant Square
    ## 1040                                                                    Stuyvesant Square Park
    ## 1041                                                                          Sunset Cove Park
    ## 1042                                                                               Sunset Park
    ## 1043                                                                          Sunset Park Pool
    ## 1044                                                             Sunset Park Recreation Center
    ## 1045                                                                           Surrogate Court
    ## 1046                                                               Susan E. Wagner High School
    ## 1047                                                                              Sutton Parks
    ## 1048                                                                         Sutton Place Park
    ## 1049                                                                           Swidish Cottage
    ## 1050                                               THE POINT Community Development Corporation
    ## 1051                                                                        Tanahey Playground
    ## 1052                                                                               Tappen Park
    ## 1053                                                                         Taylor Playground
    ## 1054                                                                       Tecumseh Playground
    ## 1055                                                             Tenant Community Meeting Room
    ## 1056                                                                              The Big Park
    ## 1057                                                                  The Painter's Playground
    ## 1058                                                                          The Pearly Gates
    ## 1059                                                                  The Shed in Hudson Yards
    ## 1060                                                                       Thomas Boyland Park
    ## 1061                                                                  Thomas Greene Playground
    ## 1062                                                                     Thomas Jefferson Park
    ## 1063                                                                     Thomas Jefferson Pool
    ## 1064                                                        Thomas Jefferson Recreation Center
    ## 1065                                                                      Thompkin Square Park
    ## 1066                                                                         Tilden Playground
    ## 1067                                                                                Timoshenko
    ## 1068                                                                   Timoshenko Soccer FIeld
    ## 1069                                                                      Tompkins Square Park
    ## 1070                                                                        Tompkinsville Park
    ## 1071                                                           Tony Dapolito Recreation Center
    ## 1072                                                                        Torsney Playground
    ## 1073                                                             Torsney/Lou Lodati Playground
    ## 1074                                                                               Tottenville
    ## 1075                                                                   Tottenville High School
    ## 1076                                                                              Travers Park
    ## 1077                                                             Tree of Life Community Garden
    ## 1078                                                                              Tremont Park
    ## 1079                                                                                Tudor Park
    ## 1080                                                                         Turtle Playground
    ## 1081                                                          Twenty Four Sycamores Playground
    ## 1082                                                                Twenty-Four Sycamores Park
    ## 1083                                               Uncle Vito E. Maranzano Glendale Playground
    ## 1084                                               Uncle Vito F. Maranzano Glendale Playground
    ## 1085                                                                         Union Square Park
    ## 1086                                                                          Unisphere - FMCP
    ## 1087                                                United Federation of Teachers Headquarters
    ## 1088                                                                         Utopia Playground
    ## 1089                                                                       Van Alst Playground
    ## 1090                                                                             Van Cortlandt
    ## 1091                                                               Van Cortlandt Nature Center
    ## 1092                                                                        Van Cortlandt Park
    ## 1093                                                   Van Cortlandt Park-Sachkerah Playground
    ## 1094                                                        Van Cortlandt Park/ Memorial Grove
    ## 1095                                                                        Van Cortlandt Pool
    ## 1096                                                                             Van Courtland
    ## 1097                                                                        Van Courtland Pool
    ## 1098                                                                     Van Courtlandt Park S
    ## 1099                                                     Van Dyke Cornerstone 392 Blake Avenue
    ## 1100                                                                             Van Nest Park
    ## 1101                                                                       Van Nest Playgrpund
    ## 1102                                                                        Vesuvio Playground
    ## 1103                                                                    Vic Hansen Field House
    ## 1104                                                                                Vic Hanson
    ## 1105                                                              Vic Hanson Recreation Center
    ## 1106                                                                              Vidalia Park
    ## 1107                                                                        Vidalia Playground
    ## 1108                                                          Vincent F. Albano Jr. Playground
    ## 1109                                                                      Vinmont Veteran Park
    ## 1110                                                                             Virginia Park
    ## 1111                                                                                   Virtual
    ## 1112                                                                        Virtual Open House
    ## 1113                                                                               Virtual Poe
    ## 1114                                                                          Virtual Poe Park
    ## 1115                                                                       Virtual Poe Park VC
    ## 1116                                                                       Virtual Poe on Zoom
    ## 1117                                                                           Virtual for Poe
    ## 1118                                                         Virtual for Sunset Park community
    ## 1119                                                                       Vito Locascio Field
    ## 1120                                                                          Von Briesen Park
    ## 1121                                                                  Voyager Preparatory H.S.
    ## 1122                                                            Voyages Preparatory Highschool
    ## 1123                                                                   W. 126th St. & 1st Ave.
    ## 1124                                       W. 146th Street Bradhurst Ave. Jackie Robinson Park
    ## 1125                                                          W. 148th Street Bradhurst Avenue
    ## 1126                                       W. 151st Street 7th Avenue - Frederick Johnson Park
    ## 1127                                                                     W. 8th St. & 6th Ave.
    ## 1128                                                                     WNYC Transmitter Park
    ## 1129                                                                                    Wagner
    ## 1130                                                                         Wagner Playground
    ## 1131                                                                      Wakefield Playground
    ## 1132                                                                           Wald Playground
    ## 1133                                                                               Walker Park
    ## 1134                                                                   Walker Park Field House
    ## 1135                                                                       Walter Gladwin Park
    ## 1136                                                                    Walter Ward Playground
    ## 1137                                                                               Walton Park
    ## 1138                                                                         Washington Avenue
    ## 1139                                                      Washington Irving Campus High School
    ## 1140                                                                    Washington Market Park
    ## 1141                                                                           Washington Park
    ## 1142                                                                    Washington Square Park
    ## 1143                                                                 Watson Gleason Playground
    ## 1144                                                                              Wayanda Park
    ## 1145                                                                      Wendell Foster  Park
    ## 1146                                                         Wendell Foster  Recreation Center
    ## 1147                                                                  Wendell Foster Bike Park
    ## 1148                                                      West 152nd and McCombs (Open Street)
    ## 1149                                                                             West 4th Park
    ## 1150                                                                    West 4th Street Courts
    ## 1151                                                                     West 63rd Street YMCA
    ## 1152                                                                             West Brighton
    ## 1153                                                              West Bronx Recreation Center
    ## 1154                                                                           West Playground
    ## 1155                                                                          Westerleigh Park
    ## 1156                                                                               Whalen Park
    ## 1157                                                                                White Park
    ## 1158                                                                          White Playground
    ## 1159                                                                         Whitey Ford Field
    ## 1160                                                                      William F Moore Park
    ## 1161                                                          William F. Passannante Ballfield
    ## 1162                                                                       Williamsbridge Oval
    ## 1163                                                                  Williamsbridge Oval Park
    ## 1164                                                     Williamsbridge Oval Recreation Center
    ## 1165                                                                         Willis Playground
    ## 1166                                                                          Willowbrook Park
    ## 1167                                                                           Windmuller Park
    ## 1168                                                                              Wingate Park
    ## 1169                                                                       Winthrop Playground
    ## 1170                                                                         Wolfe's Pond Park
    ## 1171                                                              Wolfe's Pond Park Playground
    ## 1172                                                                             Woodhull Park
    ## 1173                                                                       Woodlawn Playground
    ## 1174                                                                       Woodtree Playground
    ## 1175                                                                  World Ice Arena ( FMCP )
    ## 1176                                                                Wright Brothers Playground
    ## 1177                                                                          Yellowstone Park
    ## 1178                                                                    Yellowstone Playground
    ## 1179                                                                 Yolanda Garcia Playground
    ## 1180                                                                      Yolanda GarcÃ­a Park
    ## 1181                                                                       Yolanda García Park
    ## 1182                                                                      Zimmerman Playground
    ## 1183                                                        Zimmerman Playground (Field House)
    ## 1184                                                                   beach 17 st far rockway
    ## 1185                                                                               cedar grove
    ## 1186                                                                                   pier 35
    ## 1187                                                                             queens bridge
    ## 1188                                                                              queensbridge
    ##        n
    ## 1      2
    ## 2      2
    ## 3     20
    ## 4      2
    ## 5      2
    ## 6      2
    ## 7      4
    ## 8      2
    ## 9      1
    ## 10     2
    ## 11     8
    ## 12     2
    ## 13     2
    ## 14     1
    ## 15     2
    ## 16     2
    ## 17     2
    ## 18     4
    ## 19     6
    ## 20     2
    ## 21     2
    ## 22     2
    ## 23     2
    ## 24     2
    ## 25     2
    ## 26     2
    ## 27     2
    ## 28     2
    ## 29     2
    ## 30     2
    ## 31     2
    ## 32     2
    ## 33     2
    ## 34     2
    ## 35     2
    ## 36     2
    ## 37     2
    ## 38     2
    ## 39     2
    ## 40     2
    ## 41     2
    ## 42     2
    ## 43     2
    ## 44     1
    ## 45     1
    ## 46     2
    ## 47     2
    ## 48     2
    ## 49     2
    ## 50     2
    ## 51     2
    ## 52     2
    ## 53     2
    ## 54     2
    ## 55     4
    ## 56     2
    ## 57     2
    ## 58     2
    ## 59     1
    ## 60     2
    ## 61     2
    ## 62     2
    ## 63     2
    ## 64     2
    ## 65     2
    ## 66     2
    ## 67     2
    ## 68     2
    ## 69     2
    ## 70     2
    ## 71     6
    ## 72     2
    ## 73     2
    ## 74     2
    ## 75     2
    ## 76     1
    ## 77     2
    ## 78    25
    ## 79     8
    ## 80     1
    ## 81     4
    ## 82     6
    ## 83    21
    ## 84     8
    ## 85    46
    ## 86   227
    ## 87     2
    ## 88     9
    ## 89     4
    ## 90     1
    ## 91    15
    ## 92   333
    ## 93     2
    ## 94     5
    ## 95     2
    ## 96    34
    ## 97     6
    ## 98     4
    ## 99     7
    ## 100    4
    ## 101    3
    ## 102    2
    ## 103   22
    ## 104   15
    ## 105    1
    ## 106    7
    ## 107    2
    ## 108   22
    ## 109    2
    ## 110    3
    ## 111    2
    ## 112    2
    ## 113    4
    ## 114    1
    ## 115   26
    ## 116   16
    ## 117    1
    ## 118   72
    ## 119    4
    ## 120    3
    ## 121   16
    ## 122  109
    ## 123    2
    ## 124    2
    ## 125   14
    ## 126   16
    ## 127   13
    ## 128   57
    ## 129   20
    ## 130    3
    ## 131    8
    ## 132    2
    ## 133    4
    ## 134    2
    ## 135    4
    ## 136   56
    ## 137    1
    ## 138    2
    ## 139   13
    ## 140   33
    ## 141    1
    ## 142    9
    ## 143    2
    ## 144    5
    ## 145    1
    ## 146    1
    ## 147   16
    ## 148    3
    ## 149   76
    ## 150    2
    ## 151    2
    ## 152  112
    ## 153    2
    ## 154    2
    ## 155    2
    ## 156   18
    ## 157    4
    ## 158    4
    ## 159    2
    ## 160    2
    ## 161   18
    ## 162    2
    ## 163    2
    ## 164    2
    ## 165    1
    ## 166    6
    ## 167    3
    ## 168   15
    ## 169    2
    ## 170    2
    ## 171   15
    ## 172   11
    ## 173   25
    ## 174    2
    ## 175   36
    ## 176    4
    ## 177    2
    ## 178    6
    ## 179   10
    ## 180    2
    ## 181    8
    ## 182   15
    ## 183   32
    ## 184   12
    ## 185    3
    ## 186   15
    ## 187    1
    ## 188    8
    ## 189    1
    ## 190   25
    ## 191    8
    ## 192    4
    ## 193    4
    ## 194    6
    ## 195    7
    ## 196    9
    ## 197    2
    ## 198    5
    ## 199   16
    ## 200    4
    ## 201    2
    ## 202   56
    ## 203    1
    ## 204    1
    ## 205   50
    ## 206    2
    ## 207    2
    ## 208    2
    ## 209    2
    ## 210    1
    ## 211    9
    ## 212    2
    ## 213    2
    ## 214    5
    ## 215    1
    ## 216    1
    ## 217    1
    ## 218    1
    ## 219    1
    ## 220   31
    ## 221   27
    ## 222    2
    ## 223  194
    ## 224    2
    ## 225    7
    ## 226    6
    ## 227    2
    ## 228    2
    ## 229   16
    ## 230    2
    ## 231   30
    ## 232   12
    ## 233    2
    ## 234    5
    ## 235   30
    ## 236    1
    ## 237    2
    ## 238    1
    ## 239   32
    ## 240   19
    ## 241    2
    ## 242    6
    ## 243   10
    ## 244    2
    ## 245   32
    ## 246    1
    ## 247   32
    ## 248   26
    ## 249   10
    ## 250   29
    ## 251    8
    ## 252   16
    ## 253    2
    ## 254    2
    ## 255    6
    ## 256    4
    ## 257    4
    ## 258    4
    ## 259   20
    ## 260   83
    ## 261    1
    ## 262    2
    ## 263   22
    ## 264    2
    ## 265   24
    ## 266  427
    ## 267   14
    ## 268    1
    ## 269  102
    ## 270   25
    ## 271    2
    ## 272    4
    ## 273    5
    ## 274    1
    ## 275  134
    ## 276   10
    ## 277    2
    ## 278    6
    ## 279   38
    ## 280    5
    ## 281   13
    ## 282    3
    ## 283    2
    ## 284    4
    ## 285    6
    ## 286   18
    ## 287    6
    ## 288    1
    ## 289    1
    ## 290   46
    ## 291    1
    ## 292    8
    ## 293   39
    ## 294   56
    ## 295    1
    ## 296    2
    ## 297    2
    ## 298   41
    ## 299    1
    ## 300    1
    ## 301    4
    ## 302   62
    ## 303   42
    ## 304   32
    ## 305    2
    ## 306    6
    ## 307    4
    ## 308    2
    ## 309  113
    ## 310   11
    ## 311    1
    ## 312   13
    ## 313    2
    ## 314    4
    ## 315   28
    ## 316    1
    ## 317   67
    ## 318    1
    ## 319    1
    ## 320   52
    ## 321   22
    ## 322    2
    ## 323    2
    ## 324    4
    ## 325   33
    ## 326  250
    ## 327   16
    ## 328    2
    ## 329   21
    ## 330    8
    ## 331    1
    ## 332    2
    ## 333    2
    ## 334   75
    ## 335    6
    ## 336    8
    ## 337   15
    ## 338    1
    ## 339    1
    ## 340   83
    ## 341    2
    ## 342    2
    ## 343    9
    ## 344    2
    ## 345   18
    ## 346    2
    ## 347   78
    ## 348   17
    ## 349    3
    ## 350    6
    ## 351    1
    ## 352   24
    ## 353    1
    ## 354    2
    ## 355    6
    ## 356    2
    ## 357    6
    ## 358    2
    ## 359    6
    ## 360    7
    ## 361    8
    ## 362    2
    ## 363    2
    ## 364    2
    ## 365    7
    ## 366    6
    ## 367    7
    ## 368    1
    ## 369    2
    ## 370   49
    ## 371    3
    ## 372    9
    ## 373    6
    ## 374    5
    ## 375    1
    ## 376  242
    ## 377    1
    ## 378    8
    ## 379    1
    ## 380    1
    ## 381    1
    ## 382    2
    ## 383    4
    ## 384   19
    ## 385   54
    ## 386    2
    ## 387    4
    ## 388   94
    ## 389    1
    ## 390    6
    ## 391   63
    ## 392    3
    ## 393   12
    ## 394    5
    ## 395    1
    ## 396    1
    ## 397    2
    ## 398   76
    ## 399    6
    ## 400    2
    ## 401   37
    ## 402    2
    ## 403    3
    ## 404    4
    ## 405    2
    ## 406    2
    ## 407   34
    ## 408    2
    ## 409    4
    ## 410   26
    ## 411   35
    ## 412   10
    ## 413    4
    ## 414    1
    ## 415   16
    ## 416   17
    ## 417    2
    ## 418   14
    ## 419    6
    ## 420   14
    ## 421    3
    ## 422   29
    ## 423    1
    ## 424    2
    ## 425   19
    ## 426   26
    ## 427    6
    ## 428    1
    ## 429    1
    ## 430   25
    ## 431    4
    ## 432    7
    ## 433    1
    ## 434    2
    ## 435   42
    ## 436    2
    ## 437    4
    ## 438    1
    ## 439    1
    ## 440    1
    ## 441    2
    ## 442   10
    ## 443    2
    ## 444  118
    ## 445    5
    ## 446    2
    ## 447    1
    ## 448    2
    ## 449    2
    ## 450   10
    ## 451    2
    ## 452    1
    ## 453    1
    ## 454    2
    ## 455    2
    ## 456    5
    ## 457    2
    ## 458    8
    ## 459   12
    ## 460   77
    ## 461    2
    ## 462    1
    ## 463    2
    ## 464    2
    ## 465   16
    ## 466    4
    ## 467    6
    ## 468    4
    ## 469   27
    ## 470    7
    ## 471    5
    ## 472    9
    ## 473   48
    ## 474    2
    ## 475   26
    ## 476   61
    ## 477    1
    ## 478    5
    ## 479    2
    ## 480  190
    ## 481   15
    ## 482    4
    ## 483   10
    ## 484    2
    ## 485    2
    ## 486    5
    ## 487    1
    ## 488    2
    ## 489    7
    ## 490    4
    ## 491   19
    ## 492    2
    ## 493    4
    ## 494   24
    ## 495   34
    ## 496    5
    ## 497    1
    ## 498   13
    ## 499   13
    ## 500    2
    ## 501   40
    ## 502   57
    ## 503    4
    ## 504    4
    ## 505  103
    ## 506    2
    ## 507    1
    ## 508  356
    ## 509   62
    ## 510    2
    ## 511   30
    ## 512    3
    ## 513    2
    ## 514    6
    ## 515    5
    ## 516    1
    ## 517   83
    ## 518    4
    ## 519    5
    ## 520    4
    ## 521   10
    ## 522    2
    ## 523    2
    ## 524    1
    ## 525    2
    ## 526   10
    ## 527    2
    ## 528    1
    ## 529   30
    ## 530  603
    ## 531    2
    ## 532    6
    ## 533    3
    ## 534    1
    ## 535    2
    ## 536    1
    ## 537    2
    ## 538    1
    ## 539    2
    ## 540    2
    ## 541    2
    ## 542    5
    ## 543    5
    ## 544    2
    ## 545    2
    ## 546    4
    ## 547  101
    ## 548   26
    ## 549   61
    ## 550   72
    ## 551    1
    ## 552    2
    ## 553    6
    ## 554   33
    ## 555  121
    ## 556   16
    ## 557    3
    ## 558    2
    ## 559    2
    ## 560    2
    ## 561   25
    ## 562    8
    ## 563    2
    ## 564   65
    ## 565    1
    ## 566    1
    ## 567    1
    ## 568    1
    ## 569    1
    ## 570   18
    ## 571    1
    ## 572    1
    ## 573    2
    ## 574    1
    ## 575    2
    ## 576    2
    ## 577   16
    ## 578    1
    ## 579   33
    ## 580    1
    ## 581   22
    ## 582    6
    ## 583    2
    ## 584    4
    ## 585    8
    ## 586   28
    ## 587    2
    ## 588   11
    ## 589    1
    ## 590    6
    ## 591    8
    ## 592   56
    ## 593   16
    ## 594    8
    ## 595    7
    ## 596    2
    ## 597    2
    ## 598   22
    ## 599   44
    ## 600   25
    ## 601    2
    ## 602    2
    ## 603   28
    ## 604    1
    ## 605  530
    ## 606    7
    ## 607    8
    ## 608    1
    ## 609    3
    ## 610    2
    ## 611    3
    ## 612    1
    ## 613    9
    ## 614    4
    ## 615    2
    ## 616    1
    ## 617   52
    ## 618   27
    ## 619    4
    ## 620    5
    ## 621   81
    ## 622   12
    ## 623    3
    ## 624    1
    ## 625    8
    ## 626   50
    ## 627    2
    ## 628    5
    ## 629    2
    ## 630    7
    ## 631    1
    ## 632    2
    ## 633    2
    ## 634    2
    ## 635    2
    ## 636   36
    ## 637    2
    ## 638   44
    ## 639    4
    ## 640    3
    ## 641   16
    ## 642    4
    ## 643    8
    ## 644    8
    ## 645   50
    ## 646  211
    ## 647    2
    ## 648    1
    ## 649    1
    ## 650    5
    ## 651    6
    ## 652   16
    ## 653    2
    ## 654    2
    ## 655    4
    ## 656    7
    ## 657    2
    ## 658    2
    ## 659    4
    ## 660    5
    ## 661    4
    ## 662   23
    ## 663  100
    ## 664    2
    ## 665    4
    ## 666    1
    ## 667  136
    ## 668    2
    ## 669   42
    ## 670   52
    ## 671    1
    ## 672   93
    ## 673    4
    ## 674    6
    ## 675    2
    ## 676   17
    ## 677    5
    ## 678    2
    ## 679    2
    ## 680    3
    ## 681    2
    ## 682    5
    ## 683   12
    ## 684    7
    ## 685    8
    ## 686    1
    ## 687    2
    ## 688    5
    ## 689    2
    ## 690   10
    ## 691    2
    ## 692    3
    ## 693   40
    ## 694   10
    ## 695  213
    ## 696    4
    ## 697    6
    ## 698   16
    ## 699   16
    ## 700  120
    ## 701    8
    ## 702    1
    ## 703    2
    ## 704   18
    ## 705   15
    ## 706    6
    ## 707    4
    ## 708    9
    ## 709    2
    ## 710    1
    ## 711    9
    ## 712   22
    ## 713    2
    ## 714    2
    ## 715    2
    ## 716    3
    ## 717    1
    ## 718    9
    ## 719   14
    ## 720   59
    ## 721    2
    ## 722    2
    ## 723   13
    ## 724    4
    ## 725   28
    ## 726    2
    ## 727   58
    ## 728    2
    ## 729    8
    ## 730    4
    ## 731    2
    ## 732    4
    ## 733    3
    ## 734    1
    ## 735    1
    ## 736   14
    ## 737    8
    ## 738   16
    ## 739   10
    ## 740    1
    ## 741    1
    ## 742    2
    ## 743    4
    ## 744    3
    ## 745   14
    ## 746    4
    ## 747    5
    ## 748    2
    ## 749    2
    ## 750    2
    ## 751    2
    ## 752    3
    ## 753  300
    ## 754    2
    ## 755  441
    ## 756   33
    ## 757    2
    ## 758   22
    ## 759    1
    ## 760    1
    ## 761   22
    ## 762   15
    ## 763    4
    ## 764   34
    ## 765   16
    ## 766    7
    ## 767   16
    ## 768    1
    ## 769    8
    ## 770    2
    ## 771    7
    ## 772    1
    ## 773    2
    ## 774    2
    ## 775    1
    ## 776    1
    ## 777    2
    ## 778    2
    ## 779    1
    ## 780    2
    ## 781    1
    ## 782    1
    ## 783    1
    ## 784    2
    ## 785    1
    ## 786    1
    ## 787    1
    ## 788    2
    ## 789    1
    ## 790    1
    ## 791    1
    ## 792    1
    ## 793    1
    ## 794    1
    ## 795    1
    ## 796    2
    ## 797    1
    ## 798    2
    ## 799    4
    ## 800    1
    ## 801    1
    ## 802    4
    ## 803    1
    ## 804    2
    ## 805    1
    ## 806    2
    ## 807    1
    ## 808    2
    ## 809    1
    ## 810    2
    ## 811    2
    ## 812    2
    ## 813    2
    ## 814    1
    ## 815    1
    ## 816    1
    ## 817    1
    ## 818   57
    ## 819    2
    ## 820   10
    ## 821   21
    ## 822    2
    ## 823    5
    ## 824    2
    ## 825    8
    ## 826    2
    ## 827   27
    ## 828    4
    ## 829    6
    ## 830    3
    ## 831   92
    ## 832    1
    ## 833    2
    ## 834  258
    ## 835    3
    ## 836    4
    ## 837    4
    ## 838    6
    ## 839    2
    ## 840    2
    ## 841    9
    ## 842    2
    ## 843    8
    ## 844   12
    ## 845    1
    ## 846    8
    ## 847    1
    ## 848    2
    ## 849    9
    ## 850   28
    ## 851    2
    ## 852   11
    ## 853   14
    ## 854    2
    ## 855    2
    ## 856    2
    ## 857    8
    ## 858   32
    ## 859    2
    ## 860    2
    ## 861  315
    ## 862  267
    ## 863    4
    ## 864    1
    ## 865    4
    ## 866    5
    ## 867    2
    ## 868   12
    ## 869    2
    ## 870    3
    ## 871    6
    ## 872    1
    ## 873   22
    ## 874   49
    ## 875    2
    ## 876    2
    ## 877    2
    ## 878   18
    ## 879    1
    ## 880   75
    ## 881    4
    ## 882    1
    ## 883    2
    ## 884    1
    ## 885    2
    ## 886    1
    ## 887    1
    ## 888   80
    ## 889    1
    ## 890    1
    ## 891   29
    ## 892    3
    ## 893    4
    ## 894   26
    ## 895    4
    ## 896    2
    ## 897    1
    ## 898    1
    ## 899    5
    ## 900    6
    ## 901    1
    ## 902   52
    ## 903    4
    ## 904    4
    ## 905  189
    ## 906    3
    ## 907    4
    ## 908    3
    ## 909    2
    ## 910    1
    ## 911    1
    ## 912    1
    ## 913    4
    ## 914    1
    ## 915    1
    ## 916   15
    ## 917    2
    ## 918   12
    ## 919    2
    ## 920    2
    ## 921   10
    ## 922   15
    ## 923    2
    ## 924    2
    ## 925   52
    ## 926    1
    ## 927    2
    ## 928    6
    ## 929    1
    ## 930    1
    ## 931    8
    ## 932    2
    ## 933    6
    ## 934   23
    ## 935  216
    ## 936    8
    ## 937   26
    ## 938   17
    ## 939    1
    ## 940    1
    ## 941    2
    ## 942    4
    ## 943    1
    ## 944   14
    ## 945   22
    ## 946   31
    ## 947    2
    ## 948   33
    ## 949   19
    ## 950   10
    ## 951    1
    ## 952   11
    ## 953    2
    ## 954   39
    ## 955    2
    ## 956    2
    ## 957    2
    ## 958    5
    ## 959   12
    ## 960   34
    ## 961   22
    ## 962    8
    ## 963   81
    ## 964    2
    ## 965    1
    ## 966    2
    ## 967    3
    ## 968    6
    ## 969    9
    ## 970    2
    ## 971    5
    ## 972   33
    ## 973    2
    ## 974    1
    ## 975    1
    ## 976    1
    ## 977    2
    ## 978   14
    ## 979    2
    ## 980    2
    ## 981   10
    ## 982    3
    ## 983    2
    ## 984    4
    ## 985    5
    ## 986   13
    ## 987  203
    ## 988    2
    ## 989  155
    ## 990    6
    ## 991    2
    ## 992    2
    ## 993    3
    ## 994    8
    ## 995   26
    ## 996   19
    ## 997    2
    ## 998    2
    ## 999    2
    ## 1000  48
    ## 1001  10
    ## 1002  29
    ## 1003   3
    ## 1004   1
    ## 1005  69
    ## 1006 324
    ## 1007  32
    ## 1008 388
    ## 1009   1
    ## 1010  10
    ## 1011 199
    ## 1012   2
    ## 1013  26
    ## 1014   6
    ## 1015 198
    ## 1016   2
    ## 1017   2
    ## 1018   2
    ## 1019  76
    ## 1020  31
    ## 1021   2
    ## 1022  13
    ## 1023  26
    ## 1024   2
    ## 1025   2
    ## 1026   2
    ## 1027   2
    ## 1028   8
    ## 1029  47
    ## 1030  22
    ## 1031  14
    ## 1032   2
    ## 1033   2
    ## 1034  12
    ## 1035   1
    ## 1036   4
    ## 1037   4
    ## 1038   9
    ## 1039   2
    ## 1040   2
    ## 1041   2
    ## 1042 128
    ## 1043  14
    ## 1044   6
    ## 1045   1
    ## 1046   4
    ## 1047   4
    ## 1048  21
    ## 1049   2
    ## 1050   1
    ## 1051  11
    ## 1052  12
    ## 1053   2
    ## 1054   9
    ## 1055   1
    ## 1056  36
    ## 1057   5
    ## 1058   2
    ## 1059   1
    ## 1060   6
    ## 1061   8
    ## 1062 101
    ## 1063  17
    ## 1064  67
    ## 1065   2
    ## 1066   8
    ## 1067   8
    ## 1068   2
    ## 1069  54
    ## 1070   4
    ## 1071 116
    ## 1072  11
    ## 1073   2
    ## 1074  69
    ## 1075   2
    ## 1076  33
    ## 1077   1
    ## 1078  36
    ## 1079  16
    ## 1080   2
    ## 1081   2
    ## 1082  14
    ## 1083   2
    ## 1084   2
    ## 1085   4
    ## 1086   4
    ## 1087   1
    ## 1088   9
    ## 1089  10
    ## 1090   8
    ## 1091   2
    ## 1092  70
    ## 1093   2
    ## 1094   2
    ## 1095  64
    ## 1096   2
    ## 1097   2
    ## 1098   2
    ## 1099   2
    ## 1100  12
    ## 1101   2
    ## 1102  11
    ## 1103   4
    ## 1104   6
    ## 1105   2
    ## 1106  19
    ## 1107   4
    ## 1108   9
    ## 1109  12
    ## 1110  21
    ## 1111   4
    ## 1112   1
    ## 1113   6
    ## 1114   4
    ## 1115   2
    ## 1116   2
    ## 1117   2
    ## 1118   1
    ## 1119   2
    ## 1120  18
    ## 1121   1
    ## 1122   1
    ## 1123   2
    ## 1124   2
    ## 1125   2
    ## 1126   2
    ## 1127   2
    ## 1128   2
    ## 1129  85
    ## 1130  10
    ## 1131   2
    ## 1132   2
    ## 1133  29
    ## 1134  21
    ## 1135   5
    ## 1136   5
    ## 1137   2
    ## 1138   2
    ## 1139   1
    ## 1140   5
    ## 1141   1
    ## 1142  44
    ## 1143   8
    ## 1144  10
    ## 1145   8
    ## 1146   2
    ## 1147   1
    ## 1148   2
    ## 1149   2
    ## 1150   2
    ## 1151   1
    ## 1152  60
    ## 1153  17
    ## 1154  20
    ## 1155  55
    ## 1156   1
    ## 1157   3
    ## 1158  11
    ## 1159   1
    ## 1160  75
    ## 1161  14
    ## 1162   6
    ## 1163 421
    ## 1164 340
    ## 1165   1
    ## 1166  60
    ## 1167   2
    ## 1168   4
    ## 1169   3
    ## 1170  49
    ## 1171   2
    ## 1172   2
    ## 1173  24
    ## 1174   2
    ## 1175   2
    ## 1176   9
    ## 1177  19
    ## 1178   2
    ## 1179   2
    ## 1180   3
    ## 1181   7
    ## 1182   6
    ## 1183   3
    ## 1184   2
    ## 1185   2
    ## 1186   4
    ## 1187   2
    ## 1188   2

``` r
parkevent |> 
  count(borough)
```

    ##         borough    n
    ## 1         Bronx 5785
    ## 2      Brooklyn 3276
    ## 3     Manhattan 5141
    ## 4        Queens 3761
    ## 5 Staten Island 3025

``` r
parkevent |> 
  count(category)
```

    ##                        category    n
    ## 1                                 34
    ## 2                      --None--  105
    ## 3   Academic/Out of School time  221
    ## 4  Academics/Out of School Time  296
    ## 5                  Arts/Culture 2883
    ## 6               Family Festival 1313
    ## 7                       Fitness 2161
    ## 8                           KIM  421
    ## 9                      Lap Swim    2
    ## 10                  Mobile Unit 6224
    ## 11                       Nature 2428
    ## 12                  Performance  231
    ## 13        Saturday Night Lights 2662
    ## 14                        Sport 2007
