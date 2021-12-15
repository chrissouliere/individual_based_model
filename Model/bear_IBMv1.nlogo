;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;         ENTITIES AND STATE VARIABLES         ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [ gis rnd csv ]

globals
[
;; landscape-related
hour-of-day
day-of-year
month
year
mod-mask ;; shapefile to set envelope
roads
landscape-energy
annual
core-secondary

 ;; bear-related
  age-sex-list ; (e.g. [[5 "male"] [12 "female"]])
  survival-rates ; list of annual bear survival rates
  length-mass-rates ; length and mass parameters derived from von Bertalanffy regressions
  active-prob ; the mean probability that bears are active for each month from April 1st to November 15

 ;; outputs
]

breed [males male]
breed [females female]

turtles-own
[
  age
  sex
  age-class; (i.e., cub, yearling, sub-adult, or adult, with associated age)
  dig-energy-spring
  daily-energy
  body-mass
  spring-body-mass
  body-length
  spring-body-length
  max-dist-timestep ; maximum distance per timestep/time interval
  hours-awake
  hours-resting
  den-emergence
]


males-own[]

females-own
[
  fertile?
  gestating?
  attending-cub?
  attending-yearling?
]

patches-own
[
  dig-energy
  daily-patch-energy
  area-id
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  SETUP FUNCTIONS/MAIN INTERFACE PROCEDURES   ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-landscape
  ca ; clear-all
  ;gis:load-coordinate-system ("data/bma3_250m.prj"); don't think I need this
  ;set mod-mask gis:load-dataset ("data/BMA3.shp")
  set landscape-energy gis:load-dataset ("../../IBMData/NLogo/corsec_ro_50.asc")
  gis:set-world-envelope gis:envelope-of landscape-energy ; need this if bringing in multiple layers
  gis:apply-raster landscape-energy dig-energy
  resize-world 0 gis:width-of landscape-energy - 1 0 gis:height-of landscape-energy - 1; [gis:width-of landscape-energy - 1] removes the last column because the first column in NetLogo starts at 0 and not 1, which ArcMap does

  set core-secondary gis:load-dataset ("../../IBMData/NLogo/gb_bma3_dissolve.shp")

  let min-dig-energy min [dig-energy] of patches with [dig-energy <= 0 or dig-energy >= 0]
  let max-dig-energy max [dig-energy] of patches with [dig-energy <= 0 or dig-energy >= 0]

    foreach gis:feature-list-of core-secondary [
    a -> ask patches gis:intersecting a [ ; two or more arguments require brackets, i.e., [x y] -> ask patches...
      set area-id gis:property-value a "HABITAT"
    ]
  ]

  ask patches [

  ifelse (dig-energy <= 0) or (dig-energy >= 0)
    [ set pcolor scale-color green dig-energy (min-dig-energy - 10000) (max-dig-energy - 10000) ]
    [ set pcolor black ]

  if (dig-energy = 1) or (dig-energy = 2) [ ; 1 refers to core area and 2 refers to secondary area
      ifelse (dig-energy = 1)
      [ set pcolor blue ]
      [ set pcolor yellow ]
    ]

    if (dig-energy = 1) and (area-id = "S") [ set area-id "C" ] ; set area-id that overlap the boundary line between the core and secondary areas to the proper area-id - dig-energy used as references
    if (dig-energy = 2) and (area-id = "C") [ set area-id "S" ] ; ditto

    ;set dig-energy 0 ; use this command later in the program before pressing go

  ]

  reset-ticks
end

to setup-agents
  clear-ticks
  reset-ticks
  clear-turtles
  clear-all-plots
  clear-output
  set hour-of-day 0
  set day-of-year 97 ; approximately mean julian day of den emergence of adult and sub-adult in west-central Alberta (Graham and Stenhouse, 2014)
  set month 4 ; april
  set year 1
  set age-sex-list[]
  set survival-rates [0.56 0.88 0.74 0.67 0.95 0.84 0.75] ; cubs-of-the-years (< 1), yearlings(>= 1 and < 2), sub-adult female (>= 2 and < 5), sub-adult male (>= 2 and < 5), adult female (>= 5 and < 20), adult male (>= 5 < 20), senescent adult (> = 20)
  sex-age-dist
  if males-on? = TRUE [ setup-males ]
  if females-on? = TRUE [ setup-females ]
  assign-age-class
  if(file-exists? "../output/sex-age-distribution_simulation.csv")
  [
    carefully
    [file-delete "../output/sex-age-distribution_simulation.csv"]
    [print error-message]
  ]
  set length-mass-rates [180 0.402 -1.420 158 0.647 -1.190 231 0.297 -1.518 106 0.582 -1.578 ] ; male length; male length growth rate, male age at size 0 for length; female length; female length growth rate; female age at 0 size for length
                                                                                                ; male mass, male mass growth rate, male age at 0 size for mass; female mass, femlae mass growth rate, female age at 0 size for length
  set active-prob [0.1875 0.3225 0.4975 0.592 0.59 0.555 0.454 0.215]; April, May, June, July, August, September, November (first two weeks)
  output-print (word "Core area has " item 0 area-check " patches and secondary area has " item 1 area-check " patches\ncategorized as secondary area and core area respectively,\nafter taking into account road patches that overlap boundaries.")
  output-plots
  update-outputs
end

to setup-males
  create-males sex-ratio * initial-num-bears
  [
    set shape "default"
    set color yellow
    set size 50
    let p random length age-sex-list
    set age item 0 item p age-sex-list
    set sex "male"
    move-to one-of patches with [ pcolor != blue and pcolor != yellow and pcolor != black ] ; "green" string doesn't cover all patches with green shade per se, so exclude other colors to get generate bear on "green" shaded patches
  ]
end

to setup-females
  create-females abs(sex-ratio - 1) * initial-num-bears
  [
    set shape "default"
    set color white
    set size 50
    let p random length age-sex-list
    set age item 0 item p age-sex-list
    set sex "female"

    move-to one-of patches with [ pcolor != blue and pcolor != yellow and pcolor != black  ] ; same for females
  ]
end

to initialize-bear-param
ask turtles [
    determine-spring-length-mass
    determine-max-dist-timestep
    set hours-awake random 24
    set daily-energy 0
]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;                RUN FUNCTIONS                 ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  record-time
  walk

  if year > 10 [stop]
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;              BEAR PROCEDURES                 ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; set age classes according to assigned age
to assign-age-class
  ask turtles [
    ifelse (age < 5)
    [set age-class "sub-adult"]
    [
      ifelse(age >= 5 and age < 20)
      [set age-class "adult"]
      [set age-class "older-adult"]
    ]
  ]
end

to sex-age-dist
  repeat 10000 [
    let z list random 25 random-sex
    set age-sex-list lput z age-sex-list
  ]
  repeat 1000 [
    set age-sex-list map [i -> ifelse-value(last i = "female")
    [ ;female true
      ifelse-value(first i >= 5 and first i < 20)
      [ ;adult true
        ifelse-value (random-float 1 > item 4 survival-rates)
        [list (random (4 - 3 + 1) + 3) (random-sex)]
        [list (first i + 1) (last i)]
      ] ;adult true
      [ ;adult false
        ifelse-value(first i >= 2 and first i <= 4)
        [ ;sub-adult true
          ifelse-value (random-float 1 > item 2 survival-rates)
          [list (random (4 - 3 + 1) + 3) (random-sex)]
          [list (first i + 1) (last i)]
        ];sub-adult true
        [ ;sub-adult false
          ifelse-value (first i >= 20 and first i < 35)
          [ ;older adult  true
            ifelse-value (random-float 1 > item 6 survival-rates)
            [list (random (4 - 3 + 1) + 3) (random-sex)]
            [list (first i + 1) (last i)]
          ] ;older adult true
          [ ;over 35
            ifelse-value (first i >= 35)
            [list (random (4 - 3 + 1) + 3) (random-sex)]
            [list (first i + 1) last(i)]
          ] ;over 35
        ] ;sub-adult false
      ] ;adult false
    ] ;female true

    [ ;female false ;male true
      ifelse-value(first i >= 5 and first i < 20)
      [ ;prime adult true
        ifelse-value (random-float 1 > item 5 survival-rates)
        [list (random (4 - 3 + 1) + 3) (random-sex)]
        [list (first i + 1) (last i)]
      ] ;prime adult true
      [ ;prime adult false
        ifelse-value(first i >= 2 and first i <= 4)
        [ ;sub-adult true
          ifelse-value (random-float 1 > item 3 survival-rates)
          [list (random (4 - 3 + 1) + 3) (random-sex)]
          [list (first i + 1) (last i)]
        ];sub-adult true
        [ ;sub-adult false
          ifelse-value (first i >= 20 and first i < 35)
          [ ;older adult true
            ifelse-value (random-float 1 > item 6 survival-rates)
            [list (random (4 - 3 + 1) + 3) (random-sex)]
            [list (first i + 1) (last i)]
          ] ;older adult true
          [ ;over 35
            ifelse-value (first i >= 35)
            [list (random (4 - 3 + 1) + 3) (random-sex)]
            [list (first i + 1) last(i)]
          ] ;over 35
        ] ;sub-adult false
      ] ;adult false
    ] ;female false ;male true
] age-sex-list]
end

to determine-spring-length-mass
  ifelse (sex = "male")
  [
    set body-length (item 0 length-mass-rates * (1 - exp(1) ^ (- item 1 length-mass-rates * (age - item 2 length-mass-rates))))     ; von Bertalanffy model growth for male length
    set body-length random-normal body-length 11                                                                                    ; random variation introduce to male length value
    set body-mass (item 6 length-mass-rates * (1 - exp(1) ^ (- item 7 length-mass-rates * (age - item 8 length-mass-rates))) ^ 3)   ; von Bertalanffy model growth for male mass
    set body-mass random-normal body-mass 43.2                                                                                      ; random variation introduce to male mass value
  ]
  [
    set body-length (item 3 length-mass-rates * (1 - exp(1) ^ (- item 4 length-mass-rates * (age - item 5 length-mass-rates))))     ; von Bertalanffy model growth for female length
    set body-length random-normal body-length 8.1                                                                                   ; random variation introduce to female length value
    set body-mass (item 9 length-mass-rates * (1 - exp(1) ^ (- item 10 length-mass-rates * (age - item 11 length-mass-rates))) ^ 3) ; von Bertalanffy model growth for female mass
    set body-mass random-normal body-mass 17.6                                                                                      ; random variation introduce to female mass value
  ]

  if(sex = "male" and (body-length < 135 or body-length > 206)) [determine-spring-length-mass]   ; min/max posssible value for male length due to the stochastic nature of introducing random variation producing unrealistic values
  if(sex = "female" and (body-length < 126 or body-length > 176)) [determine-spring-length-mass] ; min/max posssible value for male mass due to the stochastic nature of introducing random variation producing unrealistic values
  if(sex = "male" and (body-mass < 49 or body-mass > 311)) [determine-spring-length-mass]        ; min/max posssible value for female length due to the stochastic nature of introducing random variation producing unrealistic values
  if(sex = "female" and (body-length < 35 or body-mass > 163)) [determine-spring-length-mass]    ; min/max posssible value for female mass due to the stochastic nature of introducing random variation producing unrealistic values

  set spring-body-length body-length
  set spring-body-mass body-mass

  set hours-awake random round(24 * item 0 active-prob) ; retrieve a random number between 0 and [24 hours * active-prob for April (=5)]
  set hours-resting 0
end

to determine-max-dist-timestep
  if (sex = "male" and age >= 5) [set max-dist-timestep 34]
  if (sex = "male" and age >= 2 and age < 5) [set max-dist-timestep 27]
  if (sex = "female" and age >= 5) [set max-dist-timestep 24]
  if (sex = "female" and age >= 2 and age < 5) [set max-dist-timestep 27]
  if (sex = "female" and age >= 1 and age < 2) [set max-dist-timestep 24] ; will need to adjust the if condition for attending-yearling
  if (sex = "female" and age < 1) [set max-dist-timestep 16] ; will need to adjust the if condition for attending-cub
end

to daily-energy-to-mass
  set body-mass (body-mass + daily-energy)
  set daily-energy 0
end

to walk
  ask turtles [
  if (pcolor = black) [right 180]

  let step-length 0
  while [any? other turtles-here or step-length < max-dist-timestep] ; while no turtles on patch and step-length is less than max-dist-timestep, then fd 1 to a patch and set  new step-length
    [
    fd 1
    set step-length step-length + 1
    ]
  ]
end

to active

end


to rest
  set hours-resting hours-resting + 1
  while[month = 4] [if(hours-awake > round(24 * item 0 active-prob)) [set hours-awake 0 set hours-resting 0]]
end

to forage
end

to search-for-energy
end

to memory-informed-search
end



to-report n-random-select [ n ]
  let pairs [ [ "cub" 0.187] [ "yearling" 0.13] [ "sub-adult" 0.251] [ "adult" 0.432 ] ] ; list of list
  report map first rnd:weighted-n-of-list-with-repeats n pairs [ [i] -> last i ] ; map (given reporter is run for each item of the given list) reporter (first; reports first list in item, here "cub" or "yearling", etc...) list (rnd...last i)
end                                                                              ; rnd:weighted-n-of-list-with-repeats reports a list of a given size (here n) followed by the inputed list (pairs), which is choosen randomly with repeats
                                                                                 ; the anonymous-reporter [i...last i] takes the proportional weight of each item and uses it to pick that item according to the weight assigned to it
                                                                                 ; we assign the last item in the list (e.g. 0.13) to the anonymous-procedure and report the associated first item (e.g. yearling) in the final list


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;         LANDSCAPE/PATCH PROCEDURES           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to record-time
    ifelse (hour-of-day < 23)
  [set hour-of-day hour-of-day + 1]
  [set hour-of-day 0]

  if (hour-of-day = 0) [set day-of-year day-of-year + 1]

  if(day-of-year >= 97 and day-of-year <= 120)  [set month 4]
  if(day-of-year >= 121 and day-of-year <= 151) [set month 5]
  if(day-of-year >= 152 and day-of-year <= 181) [set month 6]
  if(day-of-year >= 182 and day-of-year <= 212) [set month 7]
  if(day-of-year >= 213 and day-of-year <= 243) [set month 8]
  if(day-of-year >= 244 and day-of-year <= 273) [set month 9]
  if(day-of-year >= 274 and day-of-year <= 304) [set month 10]
  if(day-of-year >= 305)                        [set month 11]

  if (day-of-year > 319) ; approximately mean julian day of den entry of adult and sub-adult in west-central Alberta (Graham and Stenhouse, 2014)
  [
    set year year + 1
    set month 4
    set hour-of-day 0
    set day-of-year 97
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;        MODEL OUTPUTS AND REPORTERS           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-outputs ; opens on pressing go and writing variables and closes on pressing stop - can do this as many times as needed
  file-open "../output/sex-age-distribution_simulation.csv"
  ;file-print  word(map [[i] -> first i] sex-age-dist, "," map [[i] -> last i] sex-age-dist)
  ;file-print  map [[i] -> last i] sex-age-dist
  ;csv:to-file "sex-age-distribution.csv" sex-age-dist
  csv:to-file "../output/sex-age-distribution_simulation.csv" age-sex-list
  ;csv:to-file "sex-age-distribution_simulation.csv" map [i -> first i] sex-age-dist
  file-close
end

to output-plots
   ; histogram of turtles age
  set-current-plot "Age Distribution of Bears"
  set-histogram-num-bars 35
  histogram [age] of turtles
end

to-report area-check
  let c count patches with [area-id = "C" and dig-energy = 2] ; patches with area-id = core and dig-energy = 2 (secondary); should equal zero after running setup-landscape
  let s count patches with [area-id = "S" and dig-energy = 1] ; patches with area-id = secondary and dig-energy = 1 (core); should equal zero after running setup-landscape
  report list s c
end

to-report random-sex
  ifelse random-float 1 < 0.5
  [report "female"]
  [report "male"]
end
@#$#@#$#@
GRAPHICS-WINDOW
641
10
4199
3362
-1
-1
0.00703
1
10
1
1
1
0
0
0
1
0
3557
0
3342
1
1
1
ticks
30.0

BUTTON
4
10
130
43
setup-landscape
setup-landscape\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
385
10
448
43
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
3
64
114
97
males-on?
males-on?
0
1
-1000

SWITCH
3
104
126
137
females-on?
females-on?
0
1
-1000

BUTTON
133
10
238
43
NIL
setup-agents
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
130
67
302
100
initial-num-bears
initial-num-bears
0
500
100.0
1
1
NIL
HORIZONTAL

MONITOR
473
61
553
106
NIL
day-of-year
17
1
11

MONITOR
555
61
635
106
NIL
hour-of-day
17
1
11

MONITOR
355
61
412
106
NIL
year
17
1
11

SLIDER
130
106
302
139
sex-ratio
sex-ratio
0
1
0.52
0.01
1
NIL
HORIZONTAL

OUTPUT
3
156
478
276
11

MONITOR
506
10
563
55
# males
count males
17
1
11

MONITOR
566
10
635
55
# females
count females
17
1
11

PLOT
373
659
573
809
Histogram of bear classes
Classes
Frequency
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram map [x -> position x [\"cub\" \"yearling\" \"sub-adult\" \"adult\"]] age-sex-class"

PLOT
7
287
325
540
Age Distribution of Bears
Age
Count
0.0
40.0
0.0
25.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

BUTTON
240
10
383
43
NIL
initialize-bear-param
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
414
61
471
106
NIL
month
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
