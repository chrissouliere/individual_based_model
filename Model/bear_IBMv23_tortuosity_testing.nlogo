;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;         ENTITIES AND STATE VARIABLES         ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [ gis rnd csv profiler ]

globals
[
  ;; landscape-related or patch-related
  fruit-dataset
  forb-dataset
  horsetail-dataset
  sweetvetch-dataset
  ant-dataset
  ungulate-road-dataset
  lat-dataset
  long-dataset
  hour-of-day
  day-of-year
  day-of-month
  month
  year
  mod-mask ;; shapefile to set envelope
  roads
  landscape-energy
  annual
  core-secondary
  dig-energy-patches
  ;sensing-radius; global switch
  ;active-coeff; global switch
  ;minimium-energy-hourly-coeff; global switch when calibrating coefficient
  ;draw-watersheds? ; global switch


; turtle-label-on? global variable used as slider

 ;; bear-related
  age-sex-list ; (e.g. [[5 "male"] [12 "female"]])
  survival-rates ; list of annual bear survival rates
  length-mass-rates ; length and mass parameters derived from von Bertalanffy regressions
  active-prob ; the mean probability that bears are active for each month from April 1st to November 15
  cub-cost ; cost of lactation in cubs-of-the-year

;; patch-related
  energy-weights
  potential-foraging-patches
  this-patch
  watershed-dataset

 ; end of simulation outputs
  bear-who
  bear-age
  bear-sex
  bear-age-class
  bear-spring-body-mass
  bear-body-mass
  bear-survival-rates
  bear-survive
  bear-road-freq
  bear-road-dens
  bear-total-dist
  bear-current-bci
  bear-dig-energy
  bear-cub
  bear-yearling
  ;bear-yearling-age

  ; hourly (tick) location outputs
  location-who
  location-xcor
  location-ycor
  location-spring-body-mass
  location-body-mass
  location-current-bci
]

breed [males male]
breed [females female]

turtles-own
[
  age
  sex
  age-class; (i.e., cub, yearling, sub-adult, or adult, with associated age)
  dig-energy-spring
  max-energy-hourly
  min-energy-hourly
  hourly-energy
  body-mass
  spring-body-mass
  body-length
  spring-body-length
  mean-dist-timestep ; mean distance per timestep/time interval
  sd-dist-timestep ; standard deviation of mean distance per timestep/timeinterval
  state
  substate
  should-I-rest?
  should-I-active?
  should-I-forage?
  I-am-localized?
  I-am-moving?
  hours-awake
  hours-resting
  current-step-dist
  previous-step-dist
  road-freq
  total-dist
  current-bci
  previous-bci
  diff-bci
  bear-watershed-id
  bear-watershed-dens
  bear-survival-rate
  bear-survive?
  bear-centroid-x
  bear-centroid-y
]


males-own[]

females-own
[
  attendant-cub?
  attendant-yearling?
  yearling-age
]

patches-own
[
  fruit-energy
  forb-energy
  horsetail-energy
  sweetvetch-energy
  ant-energy
  ungulate-energy
  dig-energy; digestible energy of every single patch as layed out by GIS pixel values
  latitude-value
  longitude-value
  I-am-out-of-bounds? ; boolean vavlue - only patches off-map area (black)
  I-am-road? ; boolean value stating road
  weights-applied? ; has the patch been updated this time period with weights
  current-tick ; the tick number/value that last updated the patches + 1
  ;hourly-patch-energy ; patch energy adjusted for per hour with old framework
  weighted-patch-energy ; available energy weighted to phenology. This is calculated only when bear occupies patch which is needed for computational efficiency
  available-patch-energy; available energy after substracting energy consumption from previous bear
  energy-consumed ; energy consumed by bears
  patch-consumed? ; has the patch energy been previously consumed by a bear
  freq-consumed ; frequency of patch consumption by bears
  will-forage?
  area-id
  watershed-dens
  watershed-id
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  SETUP FUNCTIONS/MAIN INTERFACE PROCEDURES   ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-landscape
  ca ; clear-all
  set core-secondary gis:load-dataset        ("../../IBMData/NLogo/gb_uf_veg_Dissolve_core_sec.shp")
  set watershed-dataset gis:load-dataset     ("../../IBMData/NLogo/wts_corsec4MP.shp")
  set fruit-dataset gis:load-dataset         ("../../IBMData/NLogo/fruit_int_60.asc")
  set forb-dataset gis:load-dataset          ("../../IBMData/NLogo/forb_int_60.asc")
  set horsetail-dataset gis:load-dataset     ("../../IBMData/NLogo/equ_int_60b.asc")
  set sweetvetch-dataset gis:load-dataset    ("../../IBMData/NLogo/hed_int_60b.asc")
  set ant-dataset gis:load-dataset           ("../../IBMData/NLogo/ant_int_60b.asc")
  set ungulate-road-dataset gis:load-dataset ("../../IBMData/NLogo/ung_ro_int_60b.asc")
  set lat-dataset gis:load-dataset           ("../../IBMData/NLogo/latitude.asc")
  set long-dataset gis:load-dataset          ("../../IBMData/NLogo/longitude.asc")

  ;set core-secondary gis:load-dataset        ("../../IBMData/NLogo/gb_uf_veg_Dissolve_core_sec.shp")

  gis:set-world-envelope (gis:envelope-union-of (gis:envelope-of fruit-dataset) ; set world-envelope to raster datasets
                                                (gis:envelope-of forb-dataset)
                                                (gis:envelope-of horsetail-dataset)
                                                (gis:envelope-of sweetvetch-dataset)
                                                (gis:envelope-of ant-dataset)
                                                (gis:envelope-of ungulate-road-dataset)
                                                (gis:envelope-of lat-dataset)
                                                (gis:envelope-of long-dataset))

  gis:apply-raster fruit-dataset fruit-energy
  gis:apply-raster forb-dataset forb-energy
  gis:apply-raster horsetail-dataset horsetail-energy
  gis:apply-raster sweetvetch-dataset sweetvetch-energy
  gis:apply-raster ant-dataset ant-energy
  gis:apply-raster ungulate-road-dataset ungulate-energy
  gis:apply-raster lat-dataset latitude-value
  gis:apply-raster long-dataset longitude-value

      foreach gis:feature-list-of core-secondary [
    a -> ask patches gis:intersecting a [ ; two or more arguments require brackets, i.e., [x y] -> ask patches...
      set area-id gis:property-value a "HABITAT"
    ]
  ]

        foreach gis:feature-list-of watershed-dataset [vector-feature ->
    ask patches gis:intersecting vector-feature [ ; two or more arguments require brackets, i.e., [x y] -> ask patches...
      set watershed-dens gis:property-value vector-feature "km_km2"
      ;set watershed-dens precision watershed-dens 4
      set watershed-id gis:property-value vector-feature "id"
    ]
  ]



  ;resize-world 0 gis:width-of ungulate-dataset - 1 0 gis:height-of ungulate-dataset - 1; [gis:width-of landscape-energy - 1] removes the last column because the first column in NetLogo starts at 0 and not 1, which ArcMap does

   ask patches [

    ; set patches to proper boundary and color identifier
    ifelse (isNAN(fruit-energy))
    [
      set I-am-out-of-bounds? TRUE
      set dig-energy 0 set fruit-energy 0 set forb-energy 0 set horsetail-energy 0 set sweetvetch-energy 0 set ant-energy 0 set ungulate-energy 0 set latitude-value -9999 set longitude-value -9999

      set pcolor black
    ]
    [
      set I-am-out-of-bounds? FALSE
      set dig-energy (fruit-energy + forb-energy + horsetail-energy + sweetvetch-energy + ant-energy + ungulate-energy)
      set pcolor green
    ]
    if (dig-energy = 0 and I-am-out-of-bounds? = FALSE) [set pcolor black set I-am-out-of-bounds? TRUE] ; rectifies the last column on the right
  ]

  let min-dig-energy min [dig-energy] of patches with [pcolor = green]
  let max-dig-energy max [dig-energy] of patches with [pcolor = green]

  ask patches [

    ; color road patches gray
    if (ungulate-energy = 1) or (ungulate-energy = 2) [ ; 1 refers to core area and 2 refers to secondary area
      ifelse (ungulate-energy = 1)
      [ set pcolor 4 ] ; medium gray
      [ set pcolor 7 ] ; light gray
    ]

    ; green patches (energy) get rescaled according to their dig-energy value
    if (pcolor = green) [set pcolor scale-color green dig-energy (min-dig-energy - 10000) (max-dig-energy + 10000)]

    if (dig-energy = 1) and (area-id = "S") [ set area-id "C" ] ; set area-id that overlap the boundary line between the core and secondary areas to the proper area-id - dig-energy used as references
    if (dig-energy = 2) and (area-id = "C") [ set area-id "S" ] ; ditto

    ; substract ungulate energy from dig-energy for patches that are considered roads
    if (ungulate-energy = 1) or (ungulate-energy = 2) [ ; 1 refers to core area and 2 refers to secondary area
      ifelse (ungulate-energy = 1)
      [ set dig-energy dig-energy - 1] ; medium gray
      [ set dig-energy dig-energy - 2] ; light gray
    ]

    ; make sure to out-of-bounds areas have the correct identification
    ifelse (pcolor = black)
    [set dig-energy 0 set I-am-out-of-bounds? TRUE]
    [set I-am-out-of-bounds? FALSE] ;after adjusting patch color and area-id, set non-green patches (roads and black area [formely dig-energy NaN]) dig-energy to 0

    ; if road, set road TRUE and all energy layers to 0
    ifelse (pcolor = 4 or pcolor = 7)
    [set I-am-road? TRUE set dig-energy 0 set fruit-energy 0 set forb-energy 0 set horsetail-energy 0 set sweetvetch-energy 0 set ant-energy 0 set ungulate-energy 0]
    [set I-am-road? FALSE]

   ;set patch-with-energy patches with [dig-energy > 0]
    ;set hourly-patch-energy (dig-energy / ((319 + 1) - 97) / 24) ; hourly patch energy
    set weighted-patch-energy 0
    set patch-consumed? FALSE ;0


  ]

   set energy-weights
  [
    [0.11 0.03 0.06 0.08 0.11 0.00 0.48 0.67 0.70 1.00 0.44 0.16] ; fruit
    [0.53 0.00 0.13 0.17 0.73 1.00 1.00 0.60 0.43 0.10 0.20 0.00] ; forb
    [0.14 1.00 0.86 0.71 0.43 0.43 0.00 0.00 0.00 0.00 0.00 0.00] ; horsetail
    [0.68 0.99 0.67 0.14 0.03 0.01 0.00 0.14 0.30 0.06 0.58 1.00] ; sweetvetch
    [0.00 0.00 0.17 0.00 0.50 0.83 1.00 0.50 0.67 0.00 0.00 0.00] ; ant
    [0.31 0.20 0.43 1.00 0.55 0.41 0.29 0.24 0.16 0.16 0.22 0.16] ; ungulate
  ]

  ask patches
  [ ; these are adjusted for peak weight. i.e. values in patch represent the month with max energy (1.00 of energy weights rows). This is done so that when use apply weights, energy is distributed according phenology and patch total energy.
    set fruit-energy fruit-energy / sum item 0 energy-weights
    set forb-energy forb-energy / sum item 1 energy-weights
    set horsetail-energy horsetail-energy / sum item 2 energy-weights
    set sweetvetch-energy sweetvetch-energy / sum item 3 energy-weights
    set ant-energy ant-energy / sum item 4 energy-weights
    set ungulate-energy ungulate-energy / sum item 5 energy-weights
    set weights-applied? FALSE
  ]

  reset-ticks
end

to setup-agents
  clear-ticks
  reset-ticks
  clear-turtles
  clear-all-plots
  clear-output
  clear-drawing
  set turtle-label-on? TRUE
  set hour-of-day 0
  set day-of-year 105 ; April 15
  set day-of-month 15 ; April 15
  set month 4 ; april
  set year 1
  set age-sex-list[]
  set survival-rates [0.56 0.88 0.74 0.67 0.95 0.84 0.75] ; cubs-of-the-years (< 1), yearlings(>= 1 and < 2), sub-adult female (>= 2 and < 5), sub-adult male (>= 2 and < 5), adult female (>= 5 and < 20), adult male (>= 5 < 20), senescent adult (> = 20)
  sex-age-dist
  if (males-on? = TRUE) [ setup-males ] ; global switch
  if (females-on? = TRUE) [ setup-females ] ; global switch

  assign-age-class

  ifelse(file-exists? (word "../output/sex-age-distribution_simulation_" behaviorspace-run-number ".csv"))
  [
    carefully
    [
      file-delete (word "../output/sex-age-distribution_simulation_" behaviorspace-run-number ".csv")
    ]
    [print error-message]
  ]
  [
    file-open (word "../output/sex-age-distribution_simulation_" behaviorspace-run-number ".csv")
    file-close
  ]

  if(behaviorspace-run-number = 1)
  [
    ifelse(file-exists? (word "../output/nonBehaSpace_results.csv"))
  [
    carefully
    [
      file-delete (word "../output/nonBehaSpace_results.csv")
      file-open (word "../output/nonBehaSpace_results.csv")
      file-print csv:to-row (list "run number" "ticks" "bear-who" "bear-age" "bear-sex" "bear-age-class" "bear-spring-body-mass" "bear-body-mass" "bear-survival-rates" "bear-survive" "bear-road-freq" "bear-road-dens" "bear-total-dist" "bear-current-bci" "bear-cub" "bear-yearling")
      file-close
    ]
    [print error-message]
  ]
  [
    file-open (word "../output/nonBehaSpace_results.csv")
    file-print csv:to-row (list "run number" "ticks" "bear-who" "bear-age" "bear-sex" "bear-age-class" "bear-spring-body-mass" "bear-body-mass" "bear-survival-rates" "bear-survive" "bear-road-freq" "bear-road-dens" "bear-total-dist" "bear-current-bci" "bear-cub" "bear-yearling")
    file-close
  ]
  ]

  ifelse(file-exists? (word "../output/location_results_" behaviorspace-run-number ".csv"))
  [
    carefully
    [
      file-delete (word "../output/location_results_" behaviorspace-run-number ".csv")
      file-open (word "../output/location_results_" behaviorspace-run-number ".csv")
      file-print csv:to-row (list "run number" "ticks" "location-who" "location-xcor" "location-ycor" "location-spring-body-mass" "location-body-mass" "location-current-bci")
      file-close
    ]
    [print error-message]
  ]
  [
    file-open (word "../output/location_results_" behaviorspace-run-number ".csv")
    file-print csv:to-row (list "run number" "ticks" "location-who" "location-xcor" "location-ycor" "location-spring-body-mass" "location-body-mass" "location-current-bci")
    file-close
  ]

  set length-mass-rates [180 0.402 -1.420 158 0.647 -1.190 231 0.297 -1.518 106 0.582 -1.578 ] ; male length; male length growth rate, male age at size 0 for length; female length; female length growth rate; female age at 0 size for length
                                                                                                ; male mass, male mass growth rate, male age at 0 size for mass; female mass, femlae mass growth rate, female age at 0 size for length
  set active-prob [[4 0.1875] [5 0.3225] [6 0.4975] [7 0.592] [8 0.59] [9 0.555] [10 0.454] [11 0.215]]; April, May, June, July, August, September, November (first two weeks)
  ;set active-prob [[4 0.1875] [5 0.3225] [6 0.4975] [7 0.9] [8 0.9] [9 0.555] [10 0.454] [11 0.215]];
  set cub-cost [[4 2200] [5 3200] [6 3500] [7 3500] [8 3200] [9 2200] [10 400]]
  set bear-who [] set bear-age [] set bear-sex [] set bear-age-class [] set bear-spring-body-mass [] set bear-body-mass [] set bear-survival-rates []
  set bear-survive [] set bear-road-freq [] set bear-road-dens [] set bear-total-dist [] set bear-current-bci [] set bear-cub[]

  output-print (word "Core area has " item 0 area-check " patches and secondary area has " item 1 area-check " patches\ncategorized as secondary area and core area respectively,\nafter taking into account road patches that overlap boundaries.")
  output-plot
  update-outputs
end

to setup-males
  create-males sex-ratio * initial-num-bears
  [
    set shape "default"
    set color blue
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
    set color cyan
    set size 50
    let p random length age-sex-list
    set age item 0 item p age-sex-list
    set sex "female"
    move-to one-of patches with [ pcolor != blue and pcolor != yellow and pcolor != black  ] ; same for females
    set attendant-cub? FALSE set attendant-yearling? FALSE
  ]
end

to initialize-bear-param
ask turtles [
    determine-spring-length-mass
    determine-dist-timestep
    determine-min-energy-hourly
    set hours-awake 0
    set hours-resting random 24
    set hourly-energy 0
    set max-energy-hourly (240 * spring-body-mass) / 13 ; Average maximum daily intake in kcal DE of bears * BM (kg) divided by the mean hours spent foraging per day
    if (max-energy-hourly < min-energy-hourly)[set max-energy-hourly min-energy-hourly]; Minimum energy consumed by any bear over an hour
    ;set max-energy-hourly 3000
    set I-am-localized? TRUE
    set total-dist 0
    set current-step-dist 0
    set previous-step-dist 0
    set current-bci 0
    set previous-bci 0
    set diff-bci 0
    set bear-centroid-x xcor
    set bear-centroid-y ycor
]
end

to draw-watersheds
  ifelse (draw-watersheds? = TRUE) ; global switch
  [
    gis:set-drawing-color white
    gis:draw watershed-dataset 1
  ]
  [clear-drawing]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;                RUN FUNCTIONS                 ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  record-time
  ask turtles [
    update-patch-energy
    update-state
    determine-status
    if (turtle-label-on? = FALSE) [set label ""]
    if (hour-of-day = 0)
    [
      daily-energy-to-mass
      update-body-condition
    ]
  ]

  if year > 1 [stop]
    ask turtles [
      set bear-survival-rate survival-rate (road-freq)
      ifelse (random-float 1 < bear-survival-rate)
      [set bear-survive? TRUE]
      [set bear-survive? FALSE]
    ]
  record-location ; don't need to ask all turtles, else this will repeat printing location to file and slow simulation down
  update-plot
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;              BEAR PROCEDURES                 ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; set age classes according to assigned age and whether females have attendant-cub? or attendant-yearling?
to assign-age-class
  ask turtles [
    ifelse (age < 5)
    [set age-class "sub-adult"]
    [
      ifelse(age >= 5 and age < 20)
      [set age-class "adult"]
      [set age-class "older-adult"]
    ]
    if (sex = "female" and age-class = "adult" and age >= 5); might want to change this to >= 6, currently old-adult age-class cannot have attendant cubs or yearlings
    [
      ifelse (random-float 1 < 0.333) ; 0.333 prob of female being assigned cub, 0.33 prob of female being assigned yearling, 0.33 prob of female being assigned no young
      [set attendant-cub? TRUE]
      [if (random-float 1 < 0.5) [
        set attendant-yearling? TRUE
        ifelse (random-float 1 < 0.5) [set yearling-age 1] [set yearling-age 2]
        ]
      ]
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
        [list (random (4 - 3 + 1) + 3) (random-sex)] ; forces random number to be ages 3 or 4
        [list (first i + 1) (last i)] ; aging, same for others
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
  if(sex = "female" and (body-length < 126 or body-length > 176)) [determine-spring-length-mass] ; min/max posssible value for female length due to the stochastic nature of introducing random variation producing unrealistic values
  if(sex = "male" and (body-mass < 49 or body-mass > 311)) [determine-spring-length-mass]        ; min/max posssible value for male mass due to the stochastic nature of introducing random variation producing unrealistic values
  if(sex = "female" and (body-mass < 35 or body-mass > 163)) [determine-spring-length-mass]      ; min/max posssible value for female mass due to the stochastic nature of introducing random variation producing unrealistic values

  set spring-body-length body-length
  set spring-body-mass body-mass

  ;set hours-awake random round(24 * item 1 item 0 active-prob) ; retrieve a random number between 0 and [24 hours * active-prob for April (=5)]
  ;set hours-awake 0
  ;set hours-resting random 24
  ;ifelse(hours-awake <= round(24 * item 1 item 0 active-prob)) [set should-I-forage? TRUE][set should-I-forage? FALSE] ; if hours awake is less than or equal to 5 hours, set should-I-forage to TRUE
end

to determine-min-energy-hourly
      if (sex = "male" and age >= 5) [set min-energy-hourly 3000]
      if (sex = "male" and age < 5) [set min-energy-hourly 2400]
      if (sex = "female" and age >= 5) [set min-energy-hourly 2700]
      if (sex = "female" and age < 5) [set min-energy-hourly 2200]
end

;to determine-dist-timestep ; mean movement rate per hour / ; 60 m
;  ifelse (sex = "male")
;  [
;    if (age >= 5) [set mean-dist-timestep (408 + 2 * 649.7) set sd-dist-timestep (408 + 3 * 649.7)] ;34
;    if (age >= 2 and age < 5) [set mean-dist-timestep (342 + 2 * 522.3) set sd-dist-timestep (342 + 3 * 522.3) ] ;27
;  ]
;  [
;    if (attendant-cub? = TRUE) [set mean-dist-timestep (191 + 2 * 317.8) set sd-dist-timestep (191 + 3 * 317.8)] ;16 also set survival rates for following year
;    if (attendant-yearling? = TRUE) [set mean-dist-timestep (297 + 2 * 449.6) set sd-dist-timestep (297 + 3 * 449.6)] ;24 also set surival rates for following year
;    if (age >= 5 and attendant-cub? = FALSE and attendant-yearling? = FALSE) [set mean-dist-timestep (292 + 2 * 459.6) set sd-dist-timestep (292 + 3 * 459.6)] ;24
;    if (age >= 2 and age < 5 and attendant-cub? = FALSE and attendant-yearling? = FALSE) [set mean-dist-timestep (366 + 2 * 506.4) set sd-dist-timestep (366 + 3 * 506.4)] ;27
;  ]
;end

to determine-dist-timestep ; mean movement rate per hour / ; 60 m
  ifelse (sex = "male")
  [
    if (age >= 5) [set mean-dist-timestep 4200 set sd-dist-timestep 100] ;34 ; 4000
    if (age >= 2 and age < 5) [set mean-dist-timestep 3940 set sd-dist-timestep 100] ;27 ; 3353
  ]
  [
    if (attendant-cub? = TRUE) [set mean-dist-timestep 2200 set sd-dist-timestep 100] ;16 also set survival rates for following year
    if (attendant-yearling? = TRUE) [set mean-dist-timestep 3000 set sd-dist-timestep 100] ;24 also set surival rates for following year
    if (age >= 5 and attendant-cub? = FALSE and attendant-yearling? = FALSE) [set mean-dist-timestep 3300 set sd-dist-timestep 100] ;24 ; 2862
    if (age >= 2 and age < 5 and attendant-cub? = FALSE and attendant-yearling? = FALSE) [set mean-dist-timestep 4040 set sd-dist-timestep 100] ;27 ; 3588
  ]
end

to check-out-of-bounds
  if (I-am-out-of-bounds? = TRUE) [right 180 fd (current-step-dist + 10)]
end

to check-rest
  ifelse(hours-resting < 24 - (round(24 * activity-pattern (active-prob-coeff (active-prob))))) [set should-I-rest? TRUE][set should-I-rest? FALSE]
end

to check-active
  ifelse(hours-awake < round(24 * activity-pattern (active-prob-coeff (active-prob))))
  [set should-I-active? TRUE]
  [set should-I-active? FALSE set should-I-rest? TRUE]
end

to check-stop-for-forage
  ifelse (diff-bci < 0)
  [
    ifelse (random-float 1 < 0.95)
    [set should-I-forage? TRUE]
    [set should-I-forage? FALSE]
  ]
  [
    ifelse (random-float 1 < 0.85) [
    ifelse (mean-radius-energy > active-energy-cost)
    [set should-I-forage? TRUE]
    [set should-I-forage? FALSE]
  ]
  [
    set should-I-forage? FALSE
  ]
  ]
end

to update-state
  check-out-of-bounds
  check-rest
  ifelse (should-I-rest? = TRUE)
  [set state "rest"]
  [check-active
    ifelse(should-I-active? = TRUE)
    [check-stop-for-forage
      ifelse(should-I-forage? = TRUE)
      [set state "active-foraging"]
      [set state "active-moving"]
    ]
    [set state "rest"]
  ]
end

to determine-status
  if (state = "rest")            [rest set label "resting"]
  if (state = "active-foraging") [active forage set label "active-foraging"]
  if (state = "active-moving")   [active move set label "active-moving"]
end

to rest
  ;if (I-am-out-of-bounds? = TRUE) [right 180 fd 100]
  set hours-resting hours-resting + 1 ; make bear rest and count time
  set hourly-energy (hourly-energy - resting-energy-cost)
  if(hours-resting > 24 - (round(24 * activity-pattern (active-prob-coeff (active-prob))))) [set hours-awake 0 set hours-resting 1]; if resting time exceeds threshold, set hours-awake to 0 and hours-resting to 1
  set bear-watershed-id [watershed-id] of patch-here
  set bear-watershed-dens [watershed-dens] of patch-here
end

to active
  set hours-awake hours-awake + 1
  ;ifelse (hours-awake = 1) [set I-am-localized? TRUE][set I-am-localized? FALSE]
end

to forage
  ;if (I-am-out-of-bounds? = TRUE) [right 180 fd 100]
  road-decision
  set previous-step-dist current-step-dist
  set current-step-dist 1
  set total-dist (total-dist + 0.06) ; patch size in KM

  ifelse (random-float 1.0 < 0.9)
  [
    uphill available-patch-energy ; move deterministically to higher neighoring patch
  ]
  [
    move-to one-of neighbors ; move randomly to neighboring patches with prob 0.1

  ]

  ifelse ([available-patch-energy] of patch-here < max-energy-hourly)
  [
    set bear-watershed-id [watershed-id] of patch-here
    set bear-watershed-dens [watershed-dens] of patch-here
    set hourly-energy (hourly-energy + available-patch-energy - active-energy-cost)
    set energy-consumed (energy-consumed + available-patch-energy)
    set pcolor white
    set patch-consumed? TRUE
    set freq-consumed (freq-consumed + 1)
  ]
  [
    set bear-watershed-id [watershed-id] of patch-here
    set bear-watershed-dens [watershed-dens] of patch-here
    set hourly-energy (max-energy-hourly - active-energy-cost)
    set energy-consumed (energy-consumed + max-energy-hourly)
    set pcolor white
    set patch-consumed? TRUE
    set freq-consumed (freq-consumed + 1)
  ]
end

to move
  set previous-step-dist current-step-dist
;  if (I-am-out-of-bounds? = TRUE)
;  [
;    right 180
;    fd (previous-step-dist + 10) ; fd extra 10 will cover any agent who makes a big step into out-of-bounds to come back into in-bound region
;    set total-dist (total-dist + (previous-step-dist + 10) * 0.06) ; KM
;  ]
  set hourly-energy (hourly-energy - active-energy-cost)
  set current-step-dist step-dist
  set road-freq (road-freq + frequency (true) ([i-am-road?] of patches-ahead (step-dist))) ; check how many patches ahead are roads and count them, to detect how many roads were crossed by taking large movement steps between ticks
  facexy bear-centroid-x bear-centroid-y ; orient agent heading towards starting xcor, ycor
  set heading (heading - (0.4 * 360) + random-float (0.4 * 360)) ; adjust heading towards starting xcor, ycor by 50%; need to make sure -20 for example becomes + 340, probably need ifelse statement; doesn't work need some type of learning memory model
  if(heading < 0) [set heading heading + 360]
  fd current-step-dist
  set total-dist (total-dist + (current-step-dist * 0.06)) ; KM

    while [([I-am-road?] of patch-here)] ; if on road, cross it
  [
    ifelse (patch-ahead 1 = nobody) ; if on the edge of world and patch-ahead is nobody, then turn around and move forward 1
    [right 180 forward 1]
    [forward 1] ; if not on edge world (i.e. in watershed-home-id), move forward until across road
  ]

  if(any? other turtles-here) [move-to one-of neighbors with [I-am-out-of-bounds? = FALSE or I-am-road? = FALSE]]

  set bear-watershed-id [watershed-id] of patch-here
  set bear-watershed-dens [watershed-dens] of patch-here
end

to road-decision
  if (I-am-road?) [set road-freq road-freq + 1] ; count if hit road
  while [([I-am-road?] of patch-here)] ; if on road, cross it
  [
    ifelse (patch-ahead 1 = nobody) ; if on the edge of world and patch-ahead is nobody, then turn around and move forward 1
    [right 180 forward 1]
    [forward 1] ; if not on edge world (i.e. in watershed-home-id), move forward until across road
  ]
end

to daily-energy-to-mass
  let daily-intake (hourly-energy / (body-mass ^ 0.75))
  let daily-energy ((0.106 * daily-intake) - 10.8)
  let daily-mass ((daily-energy * (body-mass ^ 0.75)) / 1000)
  set body-mass (body-mass + daily-mass)
  if (body-mass <= 0) [set body-mass 0.01]
  set hourly-energy 0
end

to update-body-condition
  set previous-bci current-bci
  set current-bci body-condition-index (body-mass) (body-length)
  set diff-bci (current-bci - previous-bci)
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;         LANDSCAPE/PATCH PROCEDURES           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to record-time
  ifelse (hour-of-day < 23)
  [set hour-of-day hour-of-day + 1]
  [set hour-of-day 0]

  if (hour-of-day = 0) [set day-of-year day-of-year + 1 set day-of-month day-of-month + 1]

  if (day-of-year >= 105 and day-of-year <= 120) [set month 4]
  if (day-of-year >= 121 and day-of-year <= 151) [set month 5 if (day-of-year = 121) [set day-of-month 1]]
  if (day-of-year >= 152 and day-of-year <= 181) [set month 6 if (day-of-year = 152) [set day-of-month 1]]
  if (day-of-year >= 182 and day-of-year <= 212) [set month 7 if (day-of-year = 182) [set day-of-month 1]]
  if (day-of-year >= 213 and day-of-year <= 243) [set month 8 if (day-of-year = 213) [set day-of-month 1]]
  if (day-of-year >= 244 and day-of-year <= 273) [set month 9 if (day-of-year = 244) [set day-of-month 1]]
  if (day-of-year >= 274 and day-of-year <= 304) [set month 10 if (day-of-year = 274) [set day-of-month 1]]
  if (day-of-year >= 305)                        [set month 11 if (day-of-year = 305) [set day-of-month 1]]

  ;if (day-of-year >= 105 and day-of-year <= 120) [set day-of-month

  if (day-of-year > 288) ; greater than October 15
  [
    record-results
    set year year + 1
    set month 4
    set hour-of-day 0
    set day-of-year 97
  ]
end

to apply-weights
  let z [[4 15] [5 1] [5 15] [6 1] [6 15] [7 1] [7 15] [8 1] [8 15] [9 1] [9 15] [10 1]]
  foreach z [x ->
    ifelse (item 1 x = 15)
    [
      if (month = item 0 x and day-of-month >= 15) [
        if (weights-applied? = FALSE) [
          set fruit-energy fruit-energy * item 0 weights energy-weights
          set forb-energy forb-energy * item 1 weights energy-weights
          set horsetail-energy horsetail-energy * item 2 weights energy-weights
          set sweetvetch-energy sweetvetch-energy * item 3 weights energy-weights
          set ant-energy ant-energy * item 4 weights energy-weights
          set ungulate-energy ungulate-energy * item 5 weights energy-weights
          set weights-applied? TRUE
          ;set patch-consumed? FALSE
        ]
      ]
    ]
    [
      if (month = item 0 x and day-of-month >= 1 and day-of-month < 15)
      [
        set weights-applied?  FALSE
        if (weights-applied? = FALSE) [
          set fruit-energy fruit-energy * item 0 weights energy-weights
          set forb-energy forb-energy * item 1 weights energy-weights
          set horsetail-energy horsetail-energy * item 2 weights energy-weights
          set sweetvetch-energy sweetvetch-energy * item 3 weights energy-weights
          set ant-energy ant-energy * item 4 weights energy-weights
          set ungulate-energy ungulate-energy * item 5 weights energy-weights
          set weights-applied? TRUE
          ;set patch-consumed? FALSE
        ]
      ]
    ]
  ]
end

to update-patch-energy
      ; may need to adjust for roads and out-of-bounds patches, though they will never sample there because there is no available-patch-energy
  let potential-patches [patches in-radius sensing-radius] of patch-here ; patches in radius of 2-patches from the patch in which the turtle is currently on
  let ideal-patch max-one-of potential-patches [weighted-patch-energy] ; of potential-patches, patch with maximum weighted-patch-energy value

  ask potential-patches
  [
    apply-weights
    set current-tick (ticks + 1)
    set weighted-patch-energy (fruit-energy + forb-energy + horsetail-energy + sweetvetch-energy + ant-energy + ungulate-energy)
    set available-patch-energy weighted-patch-energy
    if (patch-consumed? = TRUE) [set available-patch-energy (available-patch-energy - energy-consumed) ] ; substracts energy that has already been consumed by previous bears which visited the patch
    if (available-patch-energy < 0) [set available-patch-energy 0]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;        MODEL OUTPUTS AND REPORTERS           ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; end of year results
to record-results
  if(results-on? = TRUE)
  [
    ; globals outputs used in BehaviorSpace; data is sorted by turtle id
    set bear-who map[x -> [who] of x] sort turtles
    set bear-age map [x -> [age] of x] sort turtles
    set bear-sex map [x -> [sex] of x] sort turtles
    set bear-age-class map [x -> [age-class] of x] sort turtles
    set bear-spring-body-mass map [x -> [spring-body-mass] of x] sort turtles
    set bear-body-mass map [x -> [body-mass] of x] sort turtles
    set bear-survival-rates map [x -> [bear-survival-rate] of x] sort turtles
    set bear-survive map [x -> [bear-survive?] of x] sort turtles
    set bear-road-freq map [x -> [road-freq] of x] sort turtles
    set bear-road-dens map [x -> precision ([0.04268 + 0.005212 * road-freq] of x) 7] sort turtles
    set bear-road-dens intercept-zero (0.04268) (0) (bear-road-dens)
    set bear-total-dist map [x -> [total-dist] of x] sort turtles
    set bear-current-bci map [x -> [current-bci] of x] sort turtles
    let m n-values (count turtles with [sex = "male"]) [false]
    let fc map [x -> [attendant-cub?] of x] sort turtles with [sex = "female"]
    set bear-cub sentence m fc
    let fy map [x -> [attendant-yearling?] of x] sort turtles with [sex = "female"]
    set bear-yearling sentence m fy
  ;bear-dig-energy

    ; this is to double check results of BehaviorSpace; which uses sorted results from above
    file-open (word "../output/nonBehaSpace_results.csv")
    file-print csv:to-row (list behaviorspace-run-number ticks bear-who bear-age bear-sex bear-age-class bear-spring-body-mass bear-body-mass bear-survival-rates bear-survive bear-road-freq bear-road-dens bear-total-dist bear-current-bci bear-cub bear-yearling)
    file-close
  ]
end

to record-location
  if(location-results-on? = TRUE)
  [
    ; this is for behavior space, results are sorted by turtles
  set location-who map[x -> [who] of x] sort turtles
  set location-xcor map[x -> [longitude-value] of x] sort turtles
  set location-ycor map[x -> [latitude-value] of x] sort turtles
  set location-spring-body-mass map[x -> [spring-body-mass] of x] sort turtles
  set location-body-mass map[x -> [body-mass] of x] sort turtles
  set location-current-bci map[x -> [current-bci] of x] sort turtles
  ; this is to double check results of behavior space; which uses sorted results from above
  file-open (word "../output/location_results_" behaviorspace-run-number ".csv")
  file-print csv:to-row (list behaviorspace-run-number ticks location-who location-xcor location-ycor location-spring-body-mass location-body-mass location-current-bci)
  file-close
  ]
end


to update-outputs ; opens on pressing go and writing variables and closes on pressing stop - can do this as many times as needed
  file-open (word "../output/sex-age-distribution_simulation_" behaviorspace-run-number ".csv")
  ;file-print  word(map [[i] -> first i] sex-age-dist, "," map [[i] -> last i] sex-age-dist)
  ;file-print  map [[i] -> last i] sex-age-dist
  ;csv:to-file "sex-age-distribution.csv" sex-age-dist
  csv:to-file (word "../output/sex-age-distribution_simulation_" behaviorspace-run-number ".csv") age-sex-list
  ;csv:to-file "sex-age-distribution_simulation.csv" map [i -> first i] sex-age-dist
  file-close
end

to output-plot
   ; histogram of bear age
  set-current-plot "Age Distribution of Bears"
  set-histogram-num-bars 35
  histogram [age] of turtles
end

to update-plot
  ; plot of mean body-mass by sex and age-class
  set-current-plot "Mean Body Mass of Bears"
  set-current-plot-pen "adult male"
  plot mean [body-mass] of turtles with [sex = "male" and age >= 5]
  set-current-plot-pen "sub-adult male"
  plot mean [body-mass] of turtles with [sex = "male" and age < 5]
  set-current-plot-pen "adult female"
  plot mean [body-mass] of turtles with [sex = "female" and age >= 5 and attendant-cub? = FALSE and attendant-yearling? = FALSE]
  set-current-plot-pen "adult female with cub"
  plot mean [body-mass] of turtles with [sex = "female" and age >= 5 and attendant-cub? = TRUE]
  set-current-plot-pen "adult female with yearling"
  plot mean [body-mass] of turtles with [sex = "female" and age >= 5 and attendant-yearling? = TRUE]
  set-current-plot-pen "sub-adult female"
  plot mean [body-mass] of turtles with [sex = "female" and age < 5]

  ; plot mean distance traveled of sex and age-class in KM
  set-current-plot "Distance Traveled by Bears"
  set-current-plot-pen "adult male"
  plot mean [total-dist] of turtles with [sex = "male" and age >= 5]
  set-current-plot-pen "sub-adult male"
  plot mean [total-dist] of turtles with [sex = "male" and age < 5]
  set-current-plot-pen "adult female"
  plot mean [total-dist] of turtles with [sex = "female" and age >= 5 and attendant-cub? = FALSE and attendant-yearling? = FALSE]
  set-current-plot-pen "adult female with cub"
  plot mean [total-dist] of turtles with [sex = "female" and age >= 5 and attendant-cub? = TRUE]
  set-current-plot-pen "adult female with yearling"
  plot mean [total-dist] of turtles with [sex = "female" and age >= 5 and attendant-yearling? = TRUE]
  set-current-plot-pen "sub-adult female"
  plot mean [total-dist] of turtles with [sex = "female" and age < 5]

  ; plot of mean road crossing frequency by sex and age-class
  set-current-plot "Road Crossing Frequency"
  set-current-plot-pen "adult male"
  plot mean [road-freq] of turtles with [sex = "male" and age >= 5]
  set-current-plot-pen "sub-adult male"
  plot mean [road-freq] of turtles with [sex = "male" and age < 5]
  set-current-plot-pen "adult female"
  plot mean [road-freq] of turtles with [sex = "female" and age >= 5 and attendant-cub? = FALSE and attendant-yearling? = FALSE]
  set-current-plot-pen "adult female with cub"
  plot mean [road-freq] of turtles with [sex = "female" and age >= 5 and attendant-cub? = TRUE]
  set-current-plot-pen "adult female with yearling"
  plot mean [road-freq] of turtles with [sex = "female" and age >= 5 and attendant-yearling? = TRUE]
  set-current-plot-pen "sub-adult female"
  plot mean [road-freq] of turtles with [sex = "female" and age < 5]

  ; plot percentage of bears that survive (true) each day by sex and age-class
  set-current-plot "Percentage of Bears that Survive Each Day"
  if (hour-of-day = 0)
  [
  set-current-plot-pen "adult male"
  plot (count turtles with [sex = "male" and age >= 5 and bear-survive? = TRUE] / count turtles with [sex = "male" and age >= 5]) * 100
  set-current-plot-pen "sub-adult male"
  plot (count turtles with [sex = "male" and age < 5 and bear-survive? = TRUE] / count turtles with [sex = "male" and age < 5]) * 100
  set-current-plot-pen "adult female w/wo yearlings 2+"
  plot (count turtles with [(sex = "female" and age >= 5 and attendant-yearling? = FALSE and attendant-cub? = FALSE and bear-survive? = TRUE) or (sex = "female" and age >= 5 and attendant-yearling? = TRUE and yearling-age = 2 and bear-survive? = TRUE)] /
        count turtles with [(sex = "female" and age >= 5 and attendant-yearling? = FALSE and attendant-cub? = FALSE) or (sex = "female" and age >= 5 and attendant-yearling? = TRUE and yearling-age = 2)]) * 100
  set-current-plot-pen "sub-adult female"
  plot (count turtles with [sex = "female" and age < 5 and bear-survive? = TRUE] / count turtles with [sex = "female" and age < 5]) * 100
  set-current-plot-pen "adult female w/ coy or yearlings 1"
  plot (count turtles with [(sex = "female" and age >= 5 and attendant-cub? = TRUE and bear-survive? = TRUE) or (sex = "female" and age >= 5 and attendant-yearling? = TRUE and yearling-age = 1 and bear-survive? = TRUE)] /
        count turtles with [(sex = "female" and age >= 5 and attendant-cub? = TRUE) or (sex = "female" and age >= 5 and attendant-yearling? = TRUE and yearling-age = 1)]) * 100
  ]

  ; plot mean survival-rate of bears by sex and age-class
  set-current-plot "Mean Survival Rate of Bears"
  set-current-plot-pen "adult male"
  plot mean [bear-survival-rate] of turtles with [sex = "male" and age >= 5]
  set-current-plot-pen "sub-adult male"
  plot mean [bear-survival-rate] of turtles with [sex = "male" and age < 5]
  set-current-plot-pen "adult female w/wo yearlings 2+"
  plot mean [bear-survival-rate] of turtles with [(sex = "female" and age >= 5 and attendant-yearling? = FALSE and attendant-cub? = FALSE) or (sex = "female" and age >= 5 and attendant-yearling? = TRUE and yearling-age = 2)]
  set-current-plot-pen "sub-adult female"
  plot mean [bear-survival-rate] of turtles with [sex = "female" and age < 5]
  set-current-plot-pen "adult female w/ coy or yearlings 1"
  plot mean [bear-survival-rate] of turtles with [(sex = "female" and age >= 5 and attendant-cub? = TRUE) or (sex = "female" and age >= 5 and attendant-yearling? = TRUE and yearling-age = 1)]
end

to-report isNaN [z]
  report not ( z > 0 or z < 0 or z = 0 )
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

to-report weights [a]
  let z [[0 4 15] [1 5 1] [2 5 15] [3 6 1] [4 6 15] [5 7 1] [6 7 15] [7 8 1] [8 8 15] [9 9 1] [10 9 15] [11 10 1]]
  foreach z [x ->
    ifelse (item 2 x = 15)
    [
      if (month = item 1 x and day-of-month >= 15) [report map [i -> item (item 0 x) i] a]
    ]
    [
      if (month = item 1 x and day-of-month >= 1 and day-of-month < 15) [report map [i -> item (item 0 x) i] a]
    ]
  ]
end

to-report active-prob-coeff[a] ; multiplies active-coeff of slider button by active-prob (item 1 i) for calibration purposes
  report map [i -> list (item 0 i)(active-coeff * item 1 i)] a
end

to-report activity-pattern [a]
  foreach a [x -> if(item 0 x = month) [report item 1 x]]
  ;report item 1 item 3 a
end

to-report resting-energy-cost
  let bm body-mass
  if (bm < 0) [set bm 0]
  let basal-metab-rate (61.9 * (bm) ^ 0.77) / 24 ; kcal/hour
  ifelse (sex = "female" and attendant-cub? = TRUE)
  [report basal-metab-rate + (cost-of-cub cub-cost / 24)]
  [report basal-metab-rate]
end

to-report active-energy-cost
  let active-metab-rate (2.57 * (body-mass) - 0.316) * 0.341 ; kcal/hour ; 0.341 km is the mean hourly movement rate (sub-adults and adults, table 6, Graham, 2014)
  ifelse (sex = "female") [
    ifelse (attendant-yearling? = TRUE)
    [ ; yearling TRUE
      ifelse (yearling-age = 1)
      [report active-metab-rate + ((2.57 * (60) - 0.316) * 0.341)] ; yearling-age 1
      [report active-metab-rate + ((2.57 * (91) - 0.316) * 0.341)] ; yearling-age 2
    ] ; yearling TRUE
    [ ; yearling FALSE
      ifelse (attendant-cub? = TRUE)
      [report active-metab-rate + (cost-of-cub cub-cost / 24)] ; cub TRUE
      [report active-metab-rate]; no cub, no yearling
    ]
  ]
  [
    report active-metab-rate ; male
  ]
end

to-report cost-of-cub [a]
  foreach a[x -> if (item 0 x = month) [report item 1 x]]
end

to-report foraging-patches
  let b [patches in-radius 1] of patch-here
  report b
end

to-report mean-radius-energy
  let b [patches in-radius sensing-radius] of patch-here
  let c sum [available-patch-energy] of b / count b ; average weighted-patch-energy over 13 patches if radius is 2 (includes 2 patches from patch-here on vertical and horizontal planes (8), and 1 patch from patch-here on diagonal planes (4), plus patch-here (1))
  report c
end

to-report step-dist
  let z random-normal mean-dist-timestep sd-dist-timestep
  while [z < 0] [set z random-normal mean-dist-timestep sd-dist-timestep]
  report ceiling(z / 60) ; cell size of 60 meters, MUST change according to pixel cell size
end

; reports the patches ahead by a certain step/distance, instead of 1 with just the primitive patch-ahead
to-report patches-ahead [ step ]
  report patch-set map patch-ahead n-values step [i -> i + 1]
end

; filter a list by an item and reports the total length, true is used here with "an-item" in the to-move submodel
to-report frequency [an-item a-list]
    report length (filter [i -> i = an-item] a-list)
end

to-report body-condition-index [m l]
  let z ((ln(m) - 3.21 * ln(l) + 11.64) / (0.29 - 0.017 * ln(l)))
  report z
end

; replace 0.04268 (intercept of road-freq crossing to road density rate formula) with 0
to-report intercept-zero [interc new the-list]
  report map [x -> ifelse-value (x = interc) [new][x]] the-list
end

to-report survival-rate [rf]
  ; road-freq crossing rate converted to road-density
  let rd 0.04268 + 0.005212 * rf

  ; adult male
  if (sex = "male" and age >= 5) [
    ifelse (rd <= 3.7)
    [report (0.946014 - (0.042284 * rd) - (0.031795 * rd ^ 2) - (0.006340 * rd ^ 3))]
    [report 0]
  ]

  ; sub-adult male
  if (sex = "male" and age < 5) [
    ifelse (rd <= 3)
    [report (0.942445 + (0.080035 * rd) - (0.353526 * rd ^ 2) + (0.075524 * rd ^ 3))]
    [report 0]
  ]

    ; adult female alone or adult female with yearlings 2+
  if ((sex = "female" and age >= 5 and attendant-yearling? = FALSE and attendant-cub? = FALSE) or (sex = "female" and age >= 5 and attendant-yearling? = TRUE and yearling-age = 2)) [
    ifelse (rd <= 4.6)
    [report (0.984809 - (0.009914 * rd) - (0.042663 * rd ^ 2))]
    [report 0]
  ]

      ; adult female with cub-of-year or with one year-old yearlings
  if ((sex = "female" and age >= 5 and attendant-cub? = TRUE) or (sex = "female" and age >= 5 and attendant-yearling? = TRUE and yearling-age = 1)) [
    ifelse (rd <= 2.1)
    [report (0.97954 - (0.11163 * rd) + (0.14217 * rd ^ 2) - (0.14685 * rd ^ 3))]
    [report 0]
  ]

      ; sub-adult female
  if (sex = "female" and age < 5 ) [
    ifelse (rd <= 3)
    [report (0.96643 + (0.11602 * rd) - (0.21249 * rd ^ 2) + (0.02300 * rd ^ 3))]
    [report 0]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
675
10
3657
2805
-1
-1
1.0
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
2973
0
2785
1
1
1
ticks
30.0

BUTTON
3
10
106
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
382
10
445
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
86
114
119
males-on?
males-on?
0
1
-1000

SWITCH
3
126
114
159
females-on?
females-on?
0
1
-1000

BUTTON
109
10
194
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
119
88
291
121
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
490
61
561
106
NIL
day-of-year
17
1
11

MONITOR
563
61
635
106
NIL
hour-of-day
17
1
11

MONITOR
295
61
352
106
NIL
year
17
1
11

SLIDER
120
128
292
161
sex-ratio
sex-ratio
0
1
0.5
0.01
1
NIL
HORIZONTAL

OUTPUT
1
165
476
285
11

MONITOR
512
10
569
55
# males
count males
17
1
11

MONITOR
572
10
641
55
# females
count females
17
1
11

PLOT
1
287
319
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
196
10
316
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
353
61
410
106
NIL
month
17
1
11

SWITCH
546
128
666
161
turtle-label-on?
turtle-label-on?
0
1
-1000

BUTTON
505
167
576
200
Profiler
setup-agents\nprofiler:start\nrepeat 30 [go]\nprofiler:stop\nprint profiler:report\nprofiler:reset
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
446
10
509
43
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1
543
640
931
Mean Body Mass of Bears
Ticks
Mass (kg)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"adult male" 1.0 0 -13345367 true "" ""
"sub-adult male" 1.0 0 -16777216 true "" ""
"adult female" 1.0 0 -2064490 true "" ""
"sub-adult female" 1.0 0 -2758414 true "" ""
"adult female with cub" 1.0 0 -1184463 true "" ""
"adult female with yearling" 1.0 0 -955883 true "" ""

MONITOR
411
61
488
106
NIL
day-of-month
17
1
11

PLOT
2
936
639
1326
Distance Traveled by Bears
Ticks
Distance (km)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"adult male" 1.0 0 -13345367 true "" ""
"sub-adult male" 1.0 0 -16777216 true "" ""
"adult female" 1.0 0 -2064490 true "" ""
"sub-adult female" 1.0 0 -2758414 true "" ""
"adult female with cub" 1.0 0 -1184463 true "" ""
"adult female with yearling" 1.0 0 -955883 true "" ""

PLOT
2
1332
639
1713
Road Crossing Frequency
Ticks
Number of Crossings
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"adult male" 1.0 0 -13345367 true "" ""
"sub-adult male" 1.0 0 -16777216 true "" ""
"adult female" 1.0 0 -2064490 true "" ""
"sub-adult female" 1.0 0 -2758414 true "" ""
"adult female with cub" 1.0 0 -1184463 true "" ""
"adult female with yearling" 1.0 0 -955883 true "" ""

BUTTON
5
47
107
80
NIL
draw-watersheds
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
111
47
269
80
draw-watersheds?
draw-watersheds?
0
1
-1000

PLOT
2
1716
639
2056
Percentage of Bears that Survive Each Day
Day - Hour 0
Percentage (%)
0.0
10.0
50.0
100.0
true
true
"" ""
PENS
"adult male" 1.0 0 -13345367 true "" ""
"sub-adult male" 1.0 0 -16777216 true "" ""
"adult female w/wo yearlings 2+" 1.0 0 -2064490 true "" ""
"sub-adult female" 1.0 0 -2758414 true "" ""
"adult female w/ coy or yearlings 1" 1.0 0 -1184463 true "" ""

PLOT
3
2064
641
2413
Mean Survival Rate of Bears
NIL
NIL
0.0
10.0
0.5
1.0
true
true
"" ""
PENS
"adult male" 1.0 0 -13345367 true "" ""
"sub-adult male" 1.0 0 -16777216 true "" ""
"adult female w/wo yearlings 2+" 1.0 0 -2064490 true "" ""
"sub-adult female" 1.0 0 -2758414 true "" ""
"adult female w/ coy or yearlings 1" 1.0 0 -1184463 true "" ""

SWITCH
296
128
397
161
results-on?
results-on?
0
1
-1000

SWITCH
399
128
542
161
location-results-on?
location-results-on?
0
1
-1000

SLIDER
480
207
652
240
sensing-radius
sensing-radius
0
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
480
250
653
283
active-coeff
active-coeff
0.6
1.6
1.2
0.2
1
NIL
HORIZONTAL

SLIDER
464
291
656
324
minimum-hourly-energy-coeff
minimum-hourly-energy-coeff
2000
3000
2700.0
100
1
NIL
HORIZONTAL

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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Resultsv24" repetitions="5" runMetricsEveryStep="false">
    <setup>setup-landscape
setup-agents
initialize-bear-param</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>bear-who</metric>
    <metric>bear-age</metric>
    <metric>bear-sex</metric>
    <metric>bear-age-class</metric>
    <metric>bear-spring-body-mass</metric>
    <metric>bear-body-mass</metric>
    <metric>bear-survival-rates</metric>
    <metric>bear-survive</metric>
    <metric>bear-road-freq</metric>
    <metric>bear-road-dens</metric>
    <metric>bear-total-dist</metric>
    <metric>bear-current-bci</metric>
    <metric>bear-cub</metric>
    <metric>bear-yearling</metric>
    <enumeratedValueSet variable="results-on?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="location-results-on?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
