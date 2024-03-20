extensions [ nw
             rnd
             stats
]


; we have two different kinds of link breeds, one directed and one undirected, just
; to show what the different networks look like with directed vs. undirected links
directed-link-breed [ directed-edges directed-edge ]
undirected-link-breed [ undirected-edges undirected-edge ]

turtles-own [
  state            ;; current internal state (ranges from 0 to 1)
  state-cont       ;; current internal state control (ranges from 0 to 1)
  language-value   ;; each person's value of language at instant t
  new-language-value ;; each person's new value of language after it has heard utterance. Save them in this variable in case of synchronous = FALSE
  language-value-cont ;; each person's value of language at instant t (control)
  orig-state       ;; each person's initially assigned state
  orig-cont-state  ;; each person's initially assigned state (control)
  lang-state       ;; each person's initially assigned lang value
  lang-cont-state  ;; each person's initially assigned lang value (control)
  spoken-state     ;; output of person's speech (0 or 1)
  alpha            ;; value of the bias for reward algorithm
  central          ;; each person's value of centrality
  initial-centrality
  bias             ;; value of the bias for probabilistic algorithm
  similarity-lang-val ;; when rewiring friends, affec a similarity value for language between focus turtles and all turtles
  value-distance   ;; compute distance in network between focus turtle and other turtles
  overall-proba    ;; overall proba of being rewired with
  amIinfluent      ;; tells if the node is influent or not (true/false)
  learning-acceptance ;; the strength of the bias for bayesian algorithms
  alp              ;; alpha value of beta distribution for language value
  bet              ;; beta value of beta distribution for language value
  my-proba
  family           ;; in a language contact network, which family they belong to
  vector           ;; set vector of alpha for each turtles
  vector-mode      ;; show the vector with the modes of all alpha
  bias-strength    ;; show the bias strength (namely highest value in the language value) for MAP dirichlet
  langval          ;; show the language value for MAP-dirichlet
  langval-norm     ;; normalize value of language for MAP-dirichlet for plotting
  rewired          ;; when creating the assortativity network, to know whether extra links have already been added to the turtle or not
  mymean           ;; mean of the prior in the case of continuous bayesian prior
  mysd             ;; sd of the prior in the case of continuous bayesian prior
  newmean          ;; when synchronous = True, then keep the new mean value (for continuous algo) until everyone has communicated
  newsd            ;; when synchronous = True, then keep the new std value (for continuous algo) until everyone has communicated
  newvector        ;; when synchronous = True, then keep the new vector value (for dirichlet algo) until everyone has communicated
  my-avg-pl        ;; the average path length for each turtle
  loc-ass          ;; the local assortativity using the first method
  loc-ass-bis      ;; the local assortativity using the second method
  myidentity
]


globals [
  highlighted-node                ; used for the "highlight mode" buttons to keep track of the currently highlighted node
  highlight-bicomponents-on       ; indicates that highlight-bicomponents mode is active
  stop-highlight-bicomponents     ; indicates that highlight-bicomponents mode needs to stop
  highlight-maximal-cliques-on    ; indicates highlight-maximal-cliques mode is active
  stop-highlight-maximal-cliques  ; indicates highlight-maximal-cliques mode needs to stop
  counter                         ; used inside a loop
  bias-baseline                   ; value of the bias for probabilistic algo
  focus-turtle                    ; focus turtle for computing rewiring
  max-similarity-lang-val ;; when rewiring friends, affec a similarity value for language between focus turtles and all turtles
  max-value-distance   ;; compute distance in network between focus turtle and other turtles
  max-centrality                  ; check centrality as a measure for rewiring friends
  list-proba                      ; probability of rewiring with other turtles for each focus turtle
  turtle-neighbors                ; neighbours of focus turtle
  turtle-unfriend                 ; turtles to unfriend when rewiring
  new-friend                      ; turtles to be friend with when rewiring
  upperuncert                     ; used for the function that compute alpha and beta (not used at the moment)
  loweruncert                     ; used for the function that compute alpha and beta (not used at the moment)
  meancentral1                    ; used for the function to control influencers biased, but with ratio of centrality
  meancentral1cont                ; same as before, for control
  meancentral2                    ; used for the function to control influencers biased, but with ratio of centrality
  meancentral2cont                ; same as before, for control
  typenetwork                     ; typenetwork used
  year                            ; equivalent of tick
  nb-neighb                       ; used to compute con-prob (connection probability)
  N-influent                      ; number of influencers (not a proportion here, but an actual number of turtles)
  langvall                        ; used to compute the 4 previous lists
  list-communities-mean           ; list with the mean language value of each community
  list-communities-std            ; list with the std language value of each community
  list-communities-nbnodes        ; list with the number of turtles inside each community
  mean-com                        ; used to compute the list-communities-mean
  std-com                         ; used to compute the list-communities-std
  nb-nodes                        ; used to compute the list-communities-nbnodes
  id-community                    ; used to compute the 3 previous values
  agentsetlouv                    ; the detected louvain communities
  turtle-biased  ;; in order to compute the pairwaise comparison
  turtle-control ;; in order to compute the pairwaise comparison
  diff           ;; in order to compute the pairwaise comparison
  list-diff-biased     ;; in order to compute the pairwaise comparison
  mean-pairwaise-comp  ;; in order to compute the pairwaise comparison - variable written
  std-pairwaise-comp   ;; in order to compute the pairwaise comparison - variable written
  list-spa-aut-focus-1  ;; list for focus turtles
  langval-neighbors      ;; list with neighbors : just used for further computations
  list-spa-aut-neighbors-1  ;; list with mean of language for neighboring turtles (link <=1)
  list-spa-aut-neighbors-2  ;; list with mean of language for neighboring turtles (link <=2)
  list-spa-aut-neighbors-3  ;; list with mean of language for neighboring turtles (link <=3)
  corr_1                    ;; this variables and the following ones are used for computing the spatial autocorrelation for neighbors <=1, <= 2, and <= 3
  corr_2
  corr_3
  newlist_1
  newlist_2
  newlist_3
  sum_all
  list-degrees       ;; list of degrees of all nodes
  my-ratio-cen       ;; report ratio of biased nodes with high centrality
  my-con-prob        ;; report connection probability
  list-values        ;; report list of utterances produced by each agent: agent number - utterance value
  mynumber           ;; internal value
  init-mean-mode     ;; initial mean language-value of turtles, before they start speaking
  init-med-mode      ;; same but with median
  my-ran-num         ;; a random float number used for MAP-dirichlet
  totalcount         ;; an internal number used for MAP-dirichlet
  mymax
  turtle-lonely
  total
  totall
  value
  fake-vector
  new-vec
  vector-to-add
  ran-num
  me
  mylinks
  list_exc_deg
  list_deg
  freq_deg
  remain_deg
  my_exc_deg
  mymin

  ;;; these are to compute assortativity
  X-or-Y             ;; to compute assortativity: variable useful just internally
  total-X            ;; total of X, in order to compute the mean of X (X is first turtle in a link)
  total-Y            ;; total of Y, in order to compute the mean of Y (Y is second turtle in a link)
  XavgX
  YavgY
  avgX
  avgY
  total-up
  total-down
  subtotal1
  subtotal2
  subtotal3
  subtotal4
  turtlenum1
  turtlenum2
  sumsquaredX
  sumsquaredY
  assortativity
  list-modularity
  joinset
]

to setup
  clear-all
  set year 0
  ask patches [ set pcolor gray ]
  set-current-plot "Degree distribution"
  set-current-plot "Value state distribution"
  set-default-shape turtles "circle"
  reset-ticks
  ;r:clearLocal
  ;r:clear
end

;; Reports the link set corresponding to the value of the links-to-use combo box
to-report get-links-to-use
  report ifelse-value (links-to-use = "directed")
    [ directed-edges ]
    [ undirected-edges ]
end


;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; NETWORK VARIOUS FUNCTIONS

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layouts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to layout-turtles
  if layout = "radial" and count turtles > 1 [
    let root-agent max-one-of turtles [ count my-links ]
    layout-radial turtles links root-agent
  ]
  if layout = "spring" [
    let factor sqrt count turtles
    if factor = 0 [ set factor 1 ]
    layout-spring turtles links (1 / factor) (14 / factor) (1.5 / factor)
  ]
  if layout = "circle" [
    layout-circle sort turtles max-pxcor * 0.9
  ]
  if layout = "tutte" [
    layout-circle sort turtles max-pxcor * 0.9
    layout-tutte max-n-of (count turtles * 0.5) turtles [ count my-links ] links 12
  ]
  display
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clusterers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Colorizes each node according to which component it is part of
to weak-component
  nw:set-context turtles get-links-to-use
  color-clusters nw:weak-component-clusters
end

; Colorizes each node according to the community it is part of
to community-detection
  nw:set-context turtles get-links-to-use
  color-clusters nw:louvain-communities
end

; Allows the user to mouse over and highlight all bicomponents
to highlight-bicomponents

  if stop-highlight-bicomponents = true [
    ; we're asked to stop - do so
    set stop-highlight-bicomponents false
    set highlight-bicomponents-on false
    stop
  ]
  set highlight-bicomponents-on true ; we're on!
  if highlight-maximal-cliques-on = true [
    ; if the other guy is on, he needs to stop
    set stop-highlight-maximal-cliques true
  ]

  if mouse-inside? [
    nw:set-context turtles get-links-to-use
    highlight-clusters nw:bicomponent-clusters
  ]
  display
end

; Allows the user to mouse over and highlight all maximal cliques
to highlight-maximal-cliques
  if (links-to-use != "undirected") [
    user-message "Maximal cliques only work with undirected links."
    stop
  ]
  if stop-highlight-maximal-cliques = true [
    ; we're asked to stop - do so
    set stop-highlight-maximal-cliques false
    set highlight-maximal-cliques-on false
    stop
  ]
  set highlight-maximal-cliques-on true ; we're on!
  if highlight-bicomponents-on = true [
    ; if the other guy is on, he needs to stop
    set stop-highlight-bicomponents true
  ]

  if mouse-inside? [
    nw:set-context turtles undirected-edges
    highlight-clusters nw:maximal-cliques
  ]
  display
end

; Colorizes the biggest maximal clique in the graph, or a random one if there is more than one
to find-biggest-cliques
  if links-to-use != "undirected" [
    user-message "Maximal cliques only work with undirected links."
    stop
  ]
  nw:set-context turtles undirected-edges
  color-clusters nw:biggest-maximal-cliques
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting and coloring of clusters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Allows the user to mouse over different nodes and
; highlight all the clusters that this node is a part of
to highlight-clusters [ clusters ]
  ; get the node with neighbors that is closest to the mouse
  let node min-one-of turtles [ distancexy mouse-xcor mouse-ycor ]
  if node != nobody and node != highlighted-node [
    set highlighted-node node
    ; find all clusters the node is in and assign them different colors
    color-clusters filter [ cluster -> member? node cluster ] clusters
    ; highlight target node
    ask node [ set color red + 2 ]
  ]
end

to color-clusters [ clusters ]
  ; reset all colors
  ask turtles [ update-color ]
  ask links [ set color white ]
  let n length clusters
  let colors ifelse-value (n <= 12)
    [ n-of n remove gray remove white base-colors ] ;; choose base colors other than white and gray
    [ n-values n [ approximate-hsb (random 255) (255) (100 + random 100) ] ] ; too many colors - pick random ones
    ; loop through the clusters and colors zipped together
    (foreach clusters colors [ [cluster cluster-color] ->
      ask cluster [ ; for each node in the cluster
        ; give the node the color of its cluster
        set color cluster-color - 3
        ;set color blue + 1
        ; colorize the links from the node to other nodes in the same cluster
        ; link color is slightly darker...
        ask my-undirected-edges [ if member? other-end cluster [ set color cluster-color - 1 ] ]
        ask my-in-directed-edges [ if member? other-end cluster [ set color cluster-color - 1 ] ]
        ask my-out-directed-edges [ if member? other-end cluster [ set color cluster-color - 1 ] ]
      ]
    ])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Centrality Measures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

to betweenness
  centrality [ -> nw:betweenness-centrality ]
end

to eigenvector
  centrality [ -> nw:eigenvector-centrality ]
end

to closeness
  centrality [ -> nw:closeness-centrality ]
end

to clustering
  centrality [ -> nw:clustering-coefficient ]
end

to assortativity-1
  centrality [ -> loc-ass ]
end

to assortativity-2
  centrality [ -> loc-ass-bis ]
end

to pathlength
  centrality [ -> my-avg-pl ]
end

; Takes a centrality measure as a reporter task, runs it for all nodes
; and set labels, sizes and colors of turtles to illustrate result
to centrality [ measure ]
  nw:set-context turtles get-links-to-use
  ask turtles [
    let res (runresult measure) ; run the task for the turtle
    ifelse is-number? res [
      set label precision res 2
      set size res ; this will be normalized later
    ]
    [ ; if the result is not a number, it is because eigenvector returned false (in the case of disconnected graphs
      set label res
      set size 1
    ]
  ]
  normalize-sizes-and-colors
end

; We want the size of the turtles to reflect their centrality, but different measures
; give different ranges of size, so we normalize the sizes according to the formula
; below. We then use the normalized sizes to pick an appropriate color.
to normalize-sizes-and-colors
  if count turtles > 0 [
    let sizes sort [ size ] of turtles ; initial sizes in increasing order
    let delta last sizes - first sizes ; difference between biggest and smallest
    ifelse delta = 0 [ ; if they are all the same size
      ask turtles [ set size 1 ]
    ]
    [ ; remap the size to a range between 0.5 and 2.5
      ask turtles [ set size ((size - first sizes) / delta) * 2 + 0.5 ]
    ]
    ask turtles [ set color scale-color red size 0 5 ] ; using a higher range max not to get too white...
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving and loading of network files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to save-matrix
  nw:set-context turtles get-links-to-use
  nw:save-matrix "matrix.txt"
end

to load-matrix
  generate [ -> nw:load-matrix "matrix.txt" turtles get-links-to-use ]
end

to save-graphml
  nw:set-context turtles get-links-to-use
  nw:save-graphml "demo.graphml"
end

to load-graphml
  nw:set-context turtles get-links-to-use
  nw:load-graphml "demo.graphml"
end



;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; NETWORK GENERATORS

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

to generate [ generator-task ]
  if clear-before-generating? [ setup ]
  ; we have a general "generate" procedure that basically just takes a task
  ; parameter and runs it, but takes care of calling layout and update plots
  run generator-task
  layout-turtles
  update-plots
end

to preferential-attachment
  setup
  generate [ -> nw:generate-preferential-attachment turtles get-links-to-use num-nodes 1 [setxy  random-xcor random-ycor ]
    set typenetwork "scalefree"
    add-initial-value-turtle]

end

to ring
  setup
  generate [ -> nw:generate-ring turtles get-links-to-use num-nodes [setxy  random-xcor random-ycor ]
    set typenetwork "ring"
    add-initial-value-turtle ]
end

to assort-network
  setup
  generate [ -> nw:generate-preferential-attachment turtles get-links-to-use num-nodes 1 [setxy  random-xcor random-ycor ]
    set typenetwork "assortnet" ]

  ask undirected-edges [ set color white ]
  ask turtles [ set central nw:eigenvector-centrality ]
  layout-turtles
  ask turtles [ set rewired False ]
  ask turtles [ set amIinfluent False ]

  ifelse high-assort = False [
    ;; low assort, namely link lonely with popular
    foreach (range 0 nb-additional-link) [

      ;; select the turtle with minimum centrality
      ask turtles with [ central = min [ central ] of turtles with [ rewired = False] ]  [ set turtle-lonely turtle who ]
      ask turtle-lonely [ set rewired True ]

      ;; find 10% most influential turtles
      ask max-n-of ( round num-nodes / 10 ) turtles [ central ] [ set amIinfluent True ]

      ;; ask turtle with minimum centrality to create link with one of the influential turtle
      ask turtle-lonely [ create-undirected-edge-with one-of turtles with [ amIinfluent = True ] ]

      ;; recompute centrality
      ask turtles [ set central nw:eigenvector-centrality ]

    ]
  ]
  [;; high assort, namely link lonely with lonely and popular with popular

    ;; count initially the number of links
    set total count undirected-edges
    show total

    ;; find 10% most influential turtles and connect them with each other
    ask max-n-of ( round num-nodes / 10 ) turtles [ central ] [ set amIinfluent True ]
    ask turtles with [ amIinfluent = True ] [ create-undirected-edges-with other turtles with [ amIinfluent = True ] ]

    ;; check the new number of edges
    set totall count undirected-edges

    ;; do until the number of edges si reached
    while [ totall < (nb-additional-link + total) ] [

      ;; recheck centrality
      ask turtles [ set central nw:betweenness-centrality ]
      ask turtles [ set rewired False ]

      ;; select two turtles with minimum centrality and rewire them together
      ask min-n-of 2 turtles [ central ] [ set rewired True ]
      ask turtles with [ rewired = True ] [ create-undirected-edges-with other turtles with [ rewired = True ] ]

      ;; count new number of edges
      set totall count undirected-edges

    ]
  ]

  ask undirected-edges [ set color white ]
  add-initial-value-turtle

end
;;; add modularity index !

to lang-contact

  if clear-before-generating? [ setup ]

  ;get-links-to-use
  ;; create first family of turtles
  create-turtles family-size [ set family 1 ]
  ask turtles [ create-links-with other turtles]

  ;; create other family
  set counter 1
  while [ count turtles < nb-family * family-size ] [
    set counter counter + 1
    create-turtles family-size [ set family counter ]
    ask turtles with [family = counter ] [ create-links-with other turtles with [ family = counter ] ]
  ]

  ;; create some extra link to link the families together
  set counter 1
  while [ counter <= max [ family ] of turtles ] [
      let my-fam counter
      foreach ( range 0 (nb-link-extra - 1) ) [
         ask one-of turtles with [ family = my-fam ] [ create-link-with one-of other turtles with [ family != my-fam ] ]
    ]
    set counter counter + 1
  ]

  ;; add other values
  layout-turtles
  update-plots
  ask turtles [setxy  random-xcor random-ycor ]
  set typenetwork "lang-contact"
  add-initial-value-turtle

  set list-modularity list (turtles with [ family = 1 ]) (turtles with [ family = 2 ])

  set counter 3
  while [ counter <= max [ family ] of turtles ]
    [
      set list-modularity insert-item 0 list-modularity (turtles with [ family = counter ])
      set counter counter + 1
  ]


end

to star
  setup
  generate [ -> nw:generate-star turtles get-links-to-use num-nodes[setxy  random-xcor random-ycor ]
    set typenetwork "star"
    add-initial-value-turtle ]
end

to wheel
  setup

  ifelse links-to-use = "directed" [
    ifelse spokes-direction = "inward" [
      generate [ -> nw:generate-wheel-inward turtles get-links-to-use num-nodes [setxy  random-xcor random-ycor ]
        set typenetwork "wheel"
        add-initial-value-turtle]
    ]
    [ ; if it's not inward, it's outward
      generate [ -> nw:generate-wheel-outward turtles get-links-to-use num-nodes[setxy  random-xcor random-ycor ]
        set typenetwork "wheel"
        add-initial-value-turtle ]
    ]
  ]
  [ ; for an undirected network, we don't care about spokes
    generate [ -> nw:generate-wheel turtles get-links-to-use num-nodes[setxy  random-xcor random-ycor ]
      set typenetwork "wheel"
      add-initial-value-turtle]
  ]

end

to lattice-2d
  setup
  generate [ -> nw:generate-lattice-2d turtles get-links-to-use nb-rows nb-cols wrap [setxy  random-xcor random-ycor ]
    set typenetwork "lattice-2d"
    add-initial-value-turtle ]

end

to small-world-ring
  setup
  generate [ -> nw:generate-watts-strogatz turtles get-links-to-use num-nodes neighborhood-size rewire-prob [setxy  random-xcor random-ycor ]
    set typenetwork "small-world-ring"
    add-initial-value-turtle ]
end


to small-world-lattice
  setup
  generate [ -> nw:generate-small-world turtles get-links-to-use nb-rows nb-cols clustering-exponent wrap [setxy  random-xcor random-ycor ]
    set typenetwork "small-world-lattice"
    add-initial-value-turtle ]

end

;to generate-random
;  setup
;  generate  [ -> nw:generate-random turtles get-links-to-use num-nodes connection-prob [setxy  random-xcor random-ycor ]
;    set typenetwork "random"
;    add-initial-value-turtle ]
;
;end

to generate-random2
  while [ nw:mean-path-length = FALSE ] [
    setup
    generate  [ -> nw:generate-random turtles get-links-to-use num-nodes connection-prob [setxy  random-xcor random-ycor ]
      ask turtles [
        if ( count my-links = 0 ) [
          create-link-with one-of other turtles
        ]
      ]
      set typenetwork "random"
      add-initial-value-turtle ]

  ]
end

to generate-random
  ifelse (num-nodes < 100) [
  if clear-before-generating? [ setup ]

  while [ nw:mean-path-length = FALSE ] [
    if clear-before-generating? [ setup ]
    ;; Make a circle of turtles

    create-turtles num-nodes
    ;; Now give each pair of turtles an equal chance of creating a link
    ask turtles [
      ;; we use "self > myself" here so that each pair of turtles is only considered once
      create-links-with turtles with [self > myself and
        random-float 1.0 < connection-prob ]
    ]

    ask turtles [
      if ( count my-links = 0 ) [
        create-link-with one-of other turtles
      ]
    ]
  ]
  set typenetwork "random"
  add-initial-value-turtle
  layout-turtles
  update-plots
  ]
  [
    setup
    generate  [ -> nw:generate-random turtles get-links-to-use num-nodes connection-prob [setxy  random-xcor random-ycor ]
    set typenetwork "random"
    add-initial-value-turtle
    ]
  ]

end



;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; ADDING INITIAL VALUES TO NETWORK

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global function gathering everything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to add-initial-value-turtle

  set list-values []
  ;; change color links
  if typenetwork != "assortnet" [
    ask links [ set color white ]
  ]

  ;; affect each turtle a value for centrality (that could change with time if dynamic network) and
  ;; the initial value of centrality (fixed)
  ask turtles [
    set central nw:eigenvector-centrality
    set initial-centrality nw:eigenvector-centrality
  ]

  ;; distribute binary and continuous states
  ifelse binary-states
    [ distribute-states-binary-specific ]
    [ distribute-states-continuous ]

  ;; distribution language-value: totally random (normal law) or according to the previous state
  distribute-language-value

  ask turtles [

    ;; if states are binary
    ifelse binary-states
    [
      ; assign different shapes and different bias to the two types of states
      ifelse (state = init-langval-0)
      [
        set shape "circle"
        set alpha 0                                       ; set value of the bias for reward algorithm
        set bias value-bias-for-state-0                   ; set value of the bias for probabilistic algorithm
        set learning-acceptance learning-acceptance0      ; set value of the bias for Bayesian algorithm
      ]
      [ set shape "triangle"
        set alpha value-bias
        set bias value-bias-for-state-1
        set learning-acceptance learning-acceptance1
      ]
    ]

    ;; if state are continuous
    [ set size state
      set alpha state * value-bias
      set bias state * (max-value-bias - min-value-bias) + min-value-bias
      set learning-acceptance 0.1
    ]
  ]

  ;; find alpha and beta for Bayesian algorithm
  if (update-algorithm = "SAM") or (update-algorithm = "MAP") [
    ask turtles [ find-initial-alpha-beta-bayesian ]

    ;; and add initial value of the language, if selected
    if total-numb-utt > 0 [ change-langval-start-bayes ]

    ;; and if the network update in a synchronous way, update the language value after hearing the initial value of the language
    if ( synchronous = TRUE ) and ( total-numb-utt > 0)  [ ask turtles [ set language-value new-language-value ] ]
  ]


  ;; find basic value of the bias for both continuous and binary states
  ifelse binary-states
  [ set bias-baseline value-bias-for-state-0 ]
  [ set bias-baseline mean [ max-value-bias ] of turtles ]

  ask turtles [
    if (update-algorithm = "dirichlet" )
    ;; update nodes color according to language-value
    [ set vector [ 1 ]
      foreach ( range 0 (nb-of-categories - 1) ) [ set vector insert-item 0 vector 1 ]
      ;r:put "categ" nb-of-categories
      ;r:eval "list_el <- rep(1, categ)"
      ;set vector r:get "list_el"
      ;set vector [ 1 1 1 ]
      set bias-strength 1
      set langval 0
      ifelse vec-dirichlet = False [
        if synchronous = True [ set newvector vector ]
        foreach ( range 0 init-dirichlet ) [ listen-bay-dirichlet value-init-dirichlet  ]
        if synchronous = True [ set vector newvector ]
      ] [
        set vector-to-add [ 0 ]
        foreach ( range 0 (nb-of-categories - 1) ) [ set vector-to-add insert-item 0 vector-to-add 0 ]
        set vector-to-add replace-item ( value-init-dirichlet - 1 ) vector-to-add init-dirichlet
        listen-bay-dirichlet vector-to-add
      ]
    ]
  ]

  ask turtles [
    set mymean mean-continuous-init
    set mysd sd-continuous-init
  ]

  ;; detect communities
  set agentsetlouv nw:louvain-communities

  ;; initialize variables
  ;detect-communities
  ;pairwaise-comparison
  ;spatial-autocor

  ;; count number of links
  set list-degrees [ count my-links ] of turtles

  ;; find initial value of mean and median
  set init-mean-mode mean [ state ] of turtles
  set init-med-mode median [ state ] of turtles

  ;; compute global assortativity
  ifelse ( typenetwork = "lang-contact" or typenetwork = "random" )
    [ compute-assort-link ]
    [ compute-assort-edge ]

  ;; compute local assortativity
  compute-local-assor
  compute-local-assor-bis

  ;; compute local pathlength
  compute-pathlength-by-turtle

  ;; initialize value
  if bias-additionnal != "none" [
    init-ind-with-value
  ]

  ;; find the bias strength and value of language for better visualization
  if (update-algorithm = "dirichlet") [
    ask turtles [
      ;; take the highest value in vector
      set bias-strength max vector
      set langval ( ( position bias-strength vector ) + 1 )

    ]
  ]
  ask turtles [ update-color ]
  ;show [ mysd ] of turtles
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE EVOLUTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to distribute-language-value

  ask turtles [

    ifelse random-initial-lang-value

    ;; affect random language value if this parameter was selected
    [
      set language-value random-normal initial-value-language standard-dev                       ; assign initial value of language
      set lang-state language-value                                                              ; assign a value for initial value of language (in order to reset language value if needed)
      set language-value-cont random-normal initial-value-language-control standard-dev-control  ; assign initial value of language for control
      set lang-cont-state language-value-cont                                                    ; assign a value for initial value of language control (in order to reset if needed)
    ]

    ;; affect the value of the state for language value if this parameter is selected
    [
      set language-value precision state 2
      set lang-state precision language-value 2
      set language-value-cont precision state 2
      set lang-cont-state precision language-value-cont 2
    ]
  ]

end

to change-langval-start-bayes
    ask turtles [
      foreach ( range 0 number-utt-heard-start ) [
        listen-bayesian 1
      ]
      foreach ( range 0 (total-numb-utt - number-utt-heard-start ) ) [
        listen-bayesian 0
      ]
    ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE DISTRIBUTION - paper Brussels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to compute-pathlength-by-turtle

  ask turtles [
    set me who
    set mylinks 0
    set counter 0
    ask other turtles [
      set mylinks ( mylinks + 1 )
      set counter counter + ( nw:distance-to turtle me )
    ]
    set my-avg-pl counter / mylinks
  ]


end

to compute-local-assor-bis

  ask turtles [
    ;let me_agentset turtle who
    let me_num who
    set joinset turtles with [ nw:distance-to turtle me_num = 1 ]
    ;set joinset ( turtle-set me_agentset others )

      ;------------ 1.  COMPUTE AVERAGE of X and Y -------------;

    set total-Y 0
    set total-X 0

    ;; FIRST, compute total for directed-edges going from SMALL to BIG
    ask joinset [
      ask my-links [

        set X-or-Y "xxx"
        ask both-ends [
          ifelse X-or-Y = "xxx" [
            set subtotal1 count my-links
            set turtlenum1 turtle who
            set X-or-Y "yyy"
          ]
          [
            set subtotal2 count my-links
            set turtlenum2 turtle who
          ]
        ]
        ifelse turtlenum1 < turtlenum2 [
          set total-X ( total-X + subtotal1 )
          set total-Y ( total-Y + subtotal2 )
        ]
        [
          set total-X ( total-X + subtotal2 )
          set total-Y ( total-Y + subtotal1 )
        ]
      ]
    ]

    ;; SECOND, compute total for directed-edges going from BIG to SMALL
    ask joinset [
      ask my-links [
        set X-or-Y "xxx"
        ask both-ends [
          ifelse X-or-Y = "xxx" [
            set subtotal1 count my-links
            set turtlenum1 turtle who
            set X-or-Y "yyy"
          ]
          [
            set subtotal2 count my-links
            set turtlenum2 turtle who
          ]
        ]
        ifelse turtlenum1 > turtlenum2 [
          set total-X ( total-X + subtotal1 )
          set total-Y ( total-Y + subtotal2 )
        ]
        [
          set total-X ( total-X + subtotal2 )
          set total-Y ( total-Y + subtotal1 )
        ]
      ]
    ]
    set avgX total-X / ( (count links) * 2)
    set avgY total-Y / ( (count links) * 2)


    ;------------ 2.  COMPUTE ASSORTATIVITY -------------;
    set total-up 0
    set sumsquaredX 0
    set sumsquaredY 0

    ;; FIRST, compute total for directed-edges going from SMALL to BIG
    ask joinset [
      ask my-links [
        set X-or-Y "xxx"
        ask both-ends [
          ifelse X-or-Y = "xxx" [
            set turtlenum1 turtle who
            set subtotal1 ( count my-links - avgX )
            set subtotal2 ( count my-links - avgY )
            set X-or-Y "yyy"
          ]
          [ set turtlenum2 turtle who
            set subtotal3 ( count my-links - avgX )
            set subtotal4 ( count my-links - avgY )
          ]
        ]
        ifelse turtlenum1 < turtlenum2 [
          set XavgX subtotal1
          set YavgY subtotal4
        ] [
          set XavgX subtotal3
          set YavgY subtotal2
        ]

        set sumsquaredX sumsquaredX + (XavgX ^ 2 )
        set sumsquaredY sumsquaredY + (YavgY ^ 2 )
        set total-up ( total-up + ( XavgX * YavgY ) )
      ]
    ]

    ;; SECOND, compute total for directed-edges going from BIG to SMALL
    ask joinset [
      ask my-links [
        set X-or-Y "xxx"
        ask both-ends [
          ifelse X-or-Y = "xxx" [
            set turtlenum1 turtle who
            set subtotal1 ( count my-links - avgX )
            set subtotal2 ( count my-links - avgY )
            set X-or-Y "yyy"
          ]
          [ set turtlenum2 turtle who
            set subtotal3 ( count my-links - avgX )
            set subtotal4 ( count my-links - avgY )
          ]
        ]
        ifelse turtlenum1 > turtlenum2 [
          set XavgX subtotal1
          set YavgY subtotal4
        ] [
          set XavgX subtotal3
          set YavgY subtotal2
        ]

        set sumsquaredX sumsquaredX + (XavgX ^ 2 )
        set sumsquaredY sumsquaredY + (YavgY ^ 2 )
        set total-up ( total-up + ( XavgX * YavgY ) )
      ]
    ]
    set total-down ( ( sqrt sumsquaredX ) * (sqrt sumsquaredY ) )
    set loc-ass-bis total-up / total-down

  ]

end

to compute-local-assor
  set list_exc_deg [ ]

  ;ask turtles 1 [
  ask turtles [
    set me who

    ;; compute the excess degree of the selected node
    set my_exc_deg ( count my-links - 1 )

    ;; compute the excess degree of the neighbors of the selected node
    set list_exc_deg [ ]
    ask turtles with [ nw:distance-to turtle me = 1 ] [ set list_exc_deg insert-item 0 list_exc_deg (count my-links - 1) ]
    let other_exc_deg mean list_exc_deg

    ;; create a vector for with the possible value of average degree (x axis on histogram distribution of avg neighbors)
    set list_deg (range 0 (max [ count my-links ] of turtles + 1 ) )
    ;set list_deg remove-item 0 list_deg

    ;; create a vector with the value of the histogram for each value of list_deg
    set freq_deg []
    foreach list_deg [
      x -> let mynum (count turtles with [ count my-links = x ])
      set freq_deg insert-item ( length freq_deg ) freq_deg mynum
    ]

    ;; normalize this vector to get the probability of getting each number
    let sumvec sum freq_deg
    set freq_deg map [ i -> i / sumvec ] freq_deg

    ;; find mean degree
    let mean_degree mean [ count my-links ] of turtles

    ;; compute the excess distribution
    set remain_deg [ ]
    let list_deg1 remove-item (length list_deg - 1 ) list_deg ;; remove the last value because in the formula we compute k+1 for each k
    foreach list_deg1 [
      x -> let to_add  ( ( ( x + 1 )  / mean_degree ) * (item ( x + 1 ) freq_deg) )
      set remain_deg insert-item ( length remain_deg ) remain_deg  to_add
    ]
    ;set remain_deg insert-item ( length remain_deg ) remain_deg 0

    ;; find mean and standard deviation of the excess degree distribution
    let mean_remain_deg mean remain_deg
    let std_remain_deg standard-deviation remain_deg

    ;print word "turtle:" who
    ;print word "My excess degree: " my_exc_deg
    ;print word "Other mean excess degree: " other_exc_deg
    ;print word "Mean excess distrib: " mean_remain_deg
    ;print word "Std excess distrib: " std_remain_deg

    ;; calculate the local assortativity based on all these values
    set loc-ass ( my_exc_deg * ( my_exc_deg + 1 ) * ( other_exc_deg - mean_remain_deg) ) / ( 2 * ( count links ) * ( std_remain_deg ) )
    ;print word "Local assort: " loc-ass
  ]

end


to init-ind-with-value

  ask turtles [ set myidentity "unbiased" ]

  ;; find number of nodes that will be biased in proportion with the total size of the network
  let nb-biased ( count turtles * ( percent-bias / 100 ) )

  ifelse bias-additionnal = "random" [
      set mymax n-of nb-biased turtles
  ]
  [
    if type-ind-biased = "cluscoeff" [
      set mymax max-n-of nb-biased turtles [ nw:clustering-coefficient ]
      set mymin min-n-of nb-biased turtles [ nw:clustering-coefficient ]
    ]

    if type-ind-biased = "degree" [
      set mymax max-n-of nb-biased turtles [ count my-links ]
      set mymin min-n-of nb-biased turtles [ count my-links ]
    ]

    if type-ind-biased = "pathlen" [
      ;show nb-biased
      ;show max-n-of nb-biased turtles [ my-avg-pl ]
      set mymin max-n-of nb-biased turtles [ my-avg-pl ]
      set mymax min-n-of nb-biased turtles [ my-avg-pl ]
    ]

    if type-ind-biased = "centbet" [
      set mymax max-n-of nb-biased turtles [ nw:betweenness-centrality ]
      set mymin min-n-of nb-biased turtles [ nw:betweenness-centrality ]
    ]

    if type-ind-biased = "centeigen" [
      set mymax max-n-of nb-biased turtles [ nw:eigenvector-centrality ]
      set mymin min-n-of nb-biased turtles [ nw:eigenvector-centrality ]
    ]

    if type-ind-biased = "centclos" [
      set mymax max-n-of nb-biased turtles [ nw:closeness-centrality ]
      set mymin min-n-of nb-biased turtles [ nw:closeness-centrality ]
    ]

    if type-ind-biased = "assor" [
      set mymax max-n-of nb-biased turtles [ loc-ass-bis ]
      set mymin min-n-of nb-biased turtles [ loc-ass-bis ]
    ]
  ]


  if bias-additionnal = "highest" or bias-additionnal = "random" [
    ask mymax [
      set myidentity "biased"
      if (update-algorithm = "dirichlet" )
      [
        ifelse vec-dirichlet = False [
          if synchronous = True [ set newvector vector ]
          foreach ( range 0 init-dirichlet-add ) [ listen-bay-dirichlet value-init-dirichlet-add  ]
          if synchronous = True [ set vector newvector ]

        ] [
          set vector-to-add [ 0 ]
          foreach ( range 0 (nb-of-categories - 1) ) [ set vector-to-add insert-item 0 vector-to-add 0 ]
          set vector-to-add replace-item ( value-init-dirichlet-add - 1 ) vector-to-add init-dirichlet-add
          listen-bay-dirichlet vector-to-add
        ]
      ]
      if update-algorithm = "continuous-MAP" or update-algorithm = "continuous-SAM" [
        if synchronous = True [
          set newmean mymean
          set newsd mysd
        ]
        foreach ( range 0 init-continuous-add ) [ listen-bay-continuous mean-continuous-init-add  ]
        if synchronous = True [
          set mymean newmean
          set mysd newsd
        ]
      ]
    ]
  ]



  if bias-additionnal = "lowest" [
    ask mymin [
      set myidentity "biased"
      if (update-algorithm = "dirichlet" ) [
        ifelse vec-dirichlet = False [
          if synchronous = True [ set newvector vector ]
          foreach ( range 0 init-dirichlet-add ) [ listen-bay-dirichlet value-init-dirichlet-add  ]
          if synchronous = True [ set vector newvector ]

        ] [
          set vector-to-add [ 0 ]
          foreach ( range 0 (nb-of-categories - 1) ) [ set vector-to-add insert-item 0 vector-to-add 0 ]
          set vector-to-add replace-item ( value-init-dirichlet-add - 1 ) vector-to-add init-dirichlet-add
          listen-bay-dirichlet vector-to-add
        ]
      ]
      if update-algorithm = "continuous-MAP" or update-algorithm = "continuous-SAM" [
        if synchronous = True [
          set newmean mymean
          set newsd mysd
        ]
        foreach ( range 0 init-continuous-add ) [ listen-bay-continuous mean-continuous-init-add  ]
        if synchronous = True [
          set mymean newmean
          set mysd newsd
        ]
      ]
    ]
  ]


end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE DISTRIBUTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to distribute-states-binary-specific

  ;; initiate all turtles with the value of not biased people
  ask turtles [
    set state init-langval-0
    set state-cont init-langval-0 ; for control
    set amIinfluent FALSE
  ]

  ;; if choose-N-influent option has been selected:
  if choose-N-influent [

    ;; if people wants more biased influencers than there are biased nodes in the network...
    if percent-influent >= percent-state-1 [ set percent-influent percent-state-1 ]

    ;; from the percentage of biased nodes, find the number of nodes
    set N-influent ( percent-influent / 100 ) * num-nodes

    ;; 1- affect state = init-langval-1 to N most influential node
    ask max-n-of N-influent turtles with [state = init-langval-0] [initial-centrality]
    [ set state init-langval-1
      set amIinfluent TRUE]

    ;; 2 - ask a random selection of the rest to have state = 1
    ask n-of (((percent-state-1 / 100) * num-nodes) - N-influent) turtles with [ amIinfluent = FALSE]
    [ set state init-langval-1 ]

    ;; 3 - do the same for state control
     ask max-n-of N-influent-control turtles with [state-cont = init-langval-0] [initial-centrality]; for control
    [ set state-cont init-langval-1 ]

    ask n-of (((percent-state-control-1 / 100) * num-nodes) - N-influent-control) turtles
    [ set state-cont init-langval-1 ]

    ; set a value of original state to all turtles: it can be used for 'reset-states' state if needed
    ask turtles [
      set orig-state state
      set orig-cont-state state-cont
    ]
  ]

 ;; if choose-N-influent option has NOT been selected, meaning that the user want to have a ratio of centrality instead:
 if (choose-N-influent = FALSE) and (typenetwork = "scalefree") [

    ;; compute the mean centrality for turtles with state = init-langval-0
    set meancentral2 mean [ initial-centrality ] of turtles with [ state = init-langval-0 ]
    set meancentral1 0

    ;; test different combinations of states until the program finds the one with the good ratio (very fast usually)
    ;; here the ratio is a bit flexible (not exactly ratio time higher, but ratio approximately higher... Here we chose 0.008 but it can be lower
    while [ ( (ratio-centrality-scalefree * meancentral2) >  (meancentral1 + 0.01) ) or  ( (ratio-centrality-scalefree * meancentral2) <  (meancentral1 - 0.01) ) ]
    [
      ;; try a random number of N-influent node selected
      let Ninf random ((percent-state-1 / 100) * num-nodes)

      ;; initialize all notes with non-biased state
      ask turtles [
        set state init-langval-0
        set amIinfluent FALSE ]

      ;; 1- affect state = init-langval-1 to N most influential node
      ask max-n-of Ninf turtles with [state = init-langval-0] [initial-centrality]
      [ set state init-langval-1
        set amIinfluent TRUE]

      ;; 2 - ask a random selection of the rest to have state = 1
      ask n-of (((percent-state-1 / 100) * num-nodes) - Ninf) turtles with [ amIinfluent = FALSE]
      [ set state init-langval-1 ]

      ;; 3 - compute the mean centrality for both states, and if there are not OK according to the ratio, the program will run it again
      set meancentral2 mean [ initial-centrality ] of turtles with [ state = init-langval-0 ]
      set meancentral1 mean [ initial-centrality ] of turtles with [ state = init-langval-1 ]
    ]

    ;; 4 - do the same for state control
    while [ ( (ratio-centrality-control * meancentral2cont) >  (meancentral1cont + 0.005) ) or  ( (ratio-centrality-control * meancentral2cont) <  (meancentral1cont - 0.005) ) ]
    [
      let Ninf random ((percent-state-control-1 / 100) * num-nodes)
      ask turtles [
        set state-cont init-langval-0
        set amIinfluent FALSE ]
      ask max-n-of Ninf turtles with [state-cont = init-langval-0] [initial-centrality]; for control
      [ set state-cont init-langval-1 ]
      ask n-of (((percent-state-control-1 / 100) * num-nodes) - Ninf) turtles
      [ set state-cont init-langval-1 ]

      set meancentral2cont mean [ initial-centrality ] of turtles with [ state-cont = init-langval-0 ]
      set meancentral1cont mean [ initial-centrality ] of turtles with [ state-cont = init-langval-1 ]
    ]
  ]

end

to distribute-states-continuous

  ;; very short because no option to select centrality yet.
  ask turtles [

    ifelse is-state-random  [
      set state random-float 1
      ifelse exactly-same-for-control [ set state-cont state ] [ set state-cont random-float 1 ]
    ]
    [ ; if distribution not random
      if distribution-state = "exponential" [
        set state random-exponential mean-if-exponential
        ifelse exactly-same-for-control  [ set state-cont state ] [ set state-cont random-exponential mean-if-exponential ]
      ]
      if distribution-state  = "gamma" [
        set state random-gamma alpha-if-gamma lambda-if-gamma
        ifelse exactly-same-for-control [ set state-cont state ] [ set state-cont random-gamma alpha-if-gamma lambda-if-gamma ]
      ]
      if distribution-state  = "normal" [
        set state random-normal mean-if-normal std-if-normal
        ifelse exactly-same-for-control [ set state-cont state ] [ set state-cont random-normal mean-if-normal std-if-normal ]
      ]
      ;; if a value is going below 0 or above 1, change it:
      if state <= 0.01 [ set state 0.01 ]
      if state >= 0.99 [ set state 0.99 ]

      if distribution-state  = "poisson" [
        set state random-poisson mean-if-poisson
        ifelse exactly-same-for-control [ set state-cont state ] [ set state-cont random-poisson mean-if-poisson ]
      ]
    ]
  ]

  ; normalize
  ;ask turtles [
  ;  set state state / max [ state ] of turtles
  ;]

  ; set a value of original state to all turtles: it can be used for 'reset-states' state if needed
  ask turtles [
    set orig-state state
    set orig-cont-state state-cont
  ]
end

to find-initial-alpha-beta-bayesian

  ;; this is possible to use a function (written below) that finds those values. However, as it is using R, it crashes often.
  ;; for this reason, I copy pasted all values for mu=0.1 here.
  if (learning-acceptance = 0.1) and (language-value = 0.01)
  [ set alp 1.56017552549526
    set bet 56.4573770240307]
  if (learning-acceptance = 0.1) and (language-value = 0.02)
  [ set alp 2.2564736921743
    set bet 62.5672109165405]
  if (learning-acceptance = 0.1) and (language-value = 0.03)
  [ set alp 3.08942009027869
    set bet 68.5579162523443]
  if (learning-acceptance = 0.1) and (language-value = 0.04)
  [ set alp 4.05088056208576
    set bet 74.2211334900583]
  if (learning-acceptance = 0.1) and (language-value = 0.05)
  [ set alp 5.12002144859363
    set bet 79.280407523279]
  if (learning-acceptance = 0.1) and (language-value = 0.06)
  [ set alp 6.26601648668885
    set bet 83.5009249581253]
  if (learning-acceptance = 0.1) and (language-value = 0.07)
  [ set alp 7.45388951338564
    set bet 86.7445321064092]
  if (learning-acceptance = 0.1) and (language-value = 0.08)
  [ set alp 8.64997543377075
    set bet 88.9747174883636]
  if (learning-acceptance = 0.1) and (language-value = 0.09)
  [ set alp 9.82553800528592
    set bet 90.2359953867798]
  if (learning-acceptance = 0.1) and (language-value = 0.1)
  [ set alp 10.9581894636844
    set bet 90.6237051731596]
  if (learning-acceptance = 0.1) and (language-value = 0.11)
  [ set alp 12.5968395640574
    set bet 94.8289746546464]
  if (learning-acceptance = 0.1) and (language-value = 0.12)
  [ set alp 14.3932885646176
    set bet 99.2174494738627]
  if (learning-acceptance = 0.1) and (language-value = 0.13)
  [ set alp 16.358589020322
    set bet 103.784403443693]
  if (learning-acceptance = 0.1) and (language-value = 0.14)
  [ set alp 18.5035905437804
    set bet 108.522056197508]
  if (learning-acceptance = 0.1) and (language-value = 0.15)
  [ set alp 20.8390580017497
    set bet 113.421328676582]
  if (learning-acceptance = 0.1) and (language-value = 0.16)
  [ set alp 23.3755837771827
    set bet 118.471814830209]
  if (learning-acceptance = 0.1) and (language-value = 0.17)
  [ set alp 26.1238102320905
    set bet 123.663308780207]
  if (learning-acceptance = 0.1) and (language-value = 0.18)
  [ set alp 29.0942749210134
    set bet 128.985030195728]
  if (learning-acceptance = 0.1) and (language-value = 0.19)
  [ set alp 32.2976623156564
    set bet 134.426876187799]
  if (learning-acceptance = 0.1) and (language-value = 0.2)
  [ set alp 35.7447434284337
    set bet 139.978973713735]
  if (learning-acceptance = 0.1) and (language-value = 0.21)
  [ set alp 39.4464548018753
    set bet 145.631901397531]
  if (learning-acceptance = 0.1) and (language-value = 0.22)
  [ set alp 43.4140754855169
    set bet 151.377176721378]
  if (learning-acceptance = 0.1) and (language-value = 0.23)
  [ set alp 47.6591747602067
    set bet 157.206802458083]
  if (learning-acceptance = 0.1) and (language-value = 0.24)
  [ set alp 52.1936753000255
    set bet 163.113305116747]
  if (learning-acceptance = 0.1) and (language-value = 0.25)
  [ set alp 57.0300445266135
    set bet 169.090133579841]
  if (learning-acceptance = 0.1) and (language-value = 0.26)
  [ set alp 62.1811681256259
    set bet 175.131016972935]
  if (learning-acceptance = 0.1) and (language-value = 0.27)
  [ set alp 67.6605902945909
    set bet 181.23048487056]
  if (learning-acceptance = 0.1) and (language-value = 0.28)
  [ set alp 73.4824670885732
    set bet 187.383486799188]
  if (learning-acceptance = 0.1) and (language-value = 0.29)
  [ set alp 79.6616846115774
    set bet 193.585503704207]
  if (learning-acceptance = 0.1) and (language-value = 0.3)
  [ set alp 86.2138663233185
    set bet 199.83235475441]
  if (learning-acceptance = 0.1) and (language-value = 0.31)
  [ set alp 93.1555286097008
    set bet 206.12037013127]
  if (learning-acceptance = 0.1) and (language-value = 0.32)
  [ set alp 100.504041249079
    set bet 212.446087654292]
  if (learning-acceptance = 0.1) and (language-value = 0.33)
  [ set alp 108.277859738314
    set bet 218.806563711123]
  if (learning-acceptance = 0.1) and (language-value = 0.34)
  [ set alp 116.496439714735
    set bet 225.198971210955]
  if (learning-acceptance = 0.1) and (language-value = 0.35)
  [ set alp 125.180468510881
    set bet 231.620870091637]
  if (learning-acceptance = 0.1) and (language-value = 0.36)
  [ set alp 134.351826685728
    set bet 238.069914107961]
  if (learning-acceptance = 0.1) and (language-value = 0.37)
  [ set alp 144.033868435378
    set bet 244.544154362941]
  if (learning-acceptance = 0.1) and (language-value = 0.38)
  [ set alp 154.251339353805
    set bet 251.041658945681]
  if (learning-acceptance = 0.1) and (language-value = 0.39)
  [ set alp 165.030576804708
    set bet 257.560645771467]
  if (learning-acceptance = 0.1) and (language-value = 0.4)
  [ set alp 176.399846257005
    set bet 264.099769385507]
  if (learning-acceptance = 0.1) and (language-value = 0.41)
  [ set alp 188.389051338251
    set bet 270.65741534041]
  if (learning-acceptance = 0.1) and (language-value = 0.42)
  [ set alp 201.030363423181
    set bet 277.232406632011]
  if (learning-acceptance = 0.1) and (language-value = 0.43)
  [ set alp 214.358107664787
    set bet 283.823538067275]
  if (learning-acceptance = 0.1) and (language-value = 0.44)
  [ set alp 228.409079385948
    set bet 290.429737400298]
  if (learning-acceptance = 0.1) and (language-value = 0.45)
  [ set alp 243.2227050243
    set bet 297.049972807478]
  if (learning-acceptance = 0.1) and (language-value = 0.46)
  [ set alp 258.841435173696
    set bet 303.683423899556]
  if (learning-acceptance = 0.1) and (language-value = 0.47)
  [ set alp 275.31080327722
    set bet 310.329203695589]
  if (learning-acceptance = 0.1) and (language-value = 0.48)
  [ set alp 292.679820723319
    set bet 316.986472450263]
  if (learning-acceptance = 0.1) and (language-value = 0.49)
  [ set alp 311.001535971075
    set bet 323.654659888262]
  if (learning-acceptance = 0.1) and (language-value = 0.5)
  [ set alp 330.333022301653
    set bet 330.333022301653]
  if (learning-acceptance = 0.1) and (language-value = 0.51)
  [ set alp 323.654659419556
    set bet 311.00153552075]
  if (learning-acceptance = 0.1) and (language-value = 0.52)
  [ set alp 316.986493866481
    set bet 292.679840492136]
  if (learning-acceptance = 0.1) and (language-value = 0.53)
  [ set alp 310.329179264057
    set bet 275.310781611522]
  if (learning-acceptance = 0.1) and (language-value = 0.54)
  [ set alp 303.683419410998
    set bet 258.841431350109]
  if (learning-acceptance = 0.1) and (language-value = 0.55)
  [ set alp 297.050006927386
    set bet 243.222732940588]
  if (learning-acceptance = 0.1) and (language-value = 0.56)
  [ set alp 290.429748595988
    set bet 228.409088182562]
  if (learning-acceptance = 0.1) and (language-value = 0.57)
  [ set alp 283.823537569948
    set bet 214.35810728961]
  if (learning-acceptance = 0.1) and (language-value = 0.58)
  [ set alp 277.232412485144
    set bet 201.030367661656]
  if (learning-acceptance = 0.1) and (language-value = 0.59)
  [ set alp 270.65741586255
    set bet 188.389051701094]
  if (learning-acceptance = 0.1) and (language-value = 0.6)
  [ set alp 264.099725852419
    set bet 176.399817234946]
  if (learning-acceptance = 0.1) and (language-value = 0.61)
  [ set alp 257.560676140439
    set bet 165.030596220936]
  if (learning-acceptance = 0.1) and (language-value = 0.62)
  [ set alp 251.041650788275
    set bet 154.251334354104]
  if (learning-acceptance = 0.1) and (language-value = 0.63)
  [ set alp 244.544141966875
    set bet 144.033861155149]
  if (learning-acceptance = 0.1) and (language-value = 0.64)
  [ set alp 238.069941728892
    set bet 134.351842222502]
  if (learning-acceptance = 0.1) and (language-value = 0.65)
  [ set alp 231.620868880137
    set bet 125.180467858535]
  if (learning-acceptance = 0.1) and (language-value = 0.66)
  [ set alp 225.19898527197
    set bet 116.496446958288]
  if (learning-acceptance = 0.1) and (language-value = 0.67)
  [ set alp 218.806569665583
    set bet 108.277862671108]
  if (learning-acceptance = 0.1) and (language-value = 0.68)
  [ set alp 212.446095734519
    set bet 100.504045051538]
  if (learning-acceptance = 0.1) and (language-value = 0.69)
  [ set alp 206.12034444474
    set bet 93.1555170693761]
  if (learning-acceptance = 0.1) and (language-value = 0.7)
  [ set alp 199.832357424943
    set bet 86.2138674678326]
  if (learning-acceptance = 0.1) and (language-value = 0.71)
  [ set alp 193.585504293494
    set bet 79.6616848522723]
  if (learning-acceptance = 0.1) and (language-value = 0.72)
  [ set alp 187.383508735059
    set bet 73.4824756191897]
  if (learning-acceptance = 0.1) and (language-value = 0.73)
  [ set alp 181.230480021863
    set bet 67.6605885012372]
  if (learning-acceptance = 0.1) and (language-value = 0.74)
  [ set alp 175.131025288424
    set bet 62.1811710472842]
  if (learning-acceptance = 0.1) and (language-value = 0.75)
  [ set alp 169.090107605145
    set bet 57.0300358683816]
  if (learning-acceptance = 0.1) and (language-value = 0.76)
  [ set alp 163.113333089632
    set bet 52.193684133568]
  if (learning-acceptance = 0.1) and (language-value = 0.77)
  [ set alp 157.206782294999
    set bet 47.6591687374673]
  if (learning-acceptance = 0.1) and (language-value = 0.78)
  [ set alp 151.377196744413
    set bet 43.4140811330396]
  if (learning-acceptance = 0.1) and (language-value = 0.79)
  [ set alp 145.631938450775
    set bet 39.4464646514718]
  if (learning-acceptance = 0.1) and (language-value = 0.8)
  [ set alp 139.978946238734
    set bet 35.7447365596834]
  if (learning-acceptance = 0.1) and (language-value = 0.81)
  [ set alp 134.426894072739
    set bet 32.2976665108893]
  if (learning-acceptance = 0.1) and (language-value = 0.82)
  [ set alp 128.985090352547
    set bet 29.0942881261689]
  if (learning-acceptance = 0.1) and (language-value = 0.83)
  [ set alp 123.663323407629
    set bet 26.1238132280686]
  if (learning-acceptance = 0.1) and (language-value = 0.84)
  [ set alp 118.471884599581
    set bet 23.3755970665869]
  if (learning-acceptance = 0.1) and (language-value = 0.85)
  [ set alp 113.421319682658
    set bet 20.8390564145867]
  if (learning-acceptance = 0.1) and (language-value = 0.86)
  [ set alp 108.522130663341
    set bet 18.5036026661254]
  if (learning-acceptance = 0.1) and (language-value = 0.87)
  [ set alp 103.784479163195
    set bet 16.3586003347303]
  if (learning-acceptance = 0.1) and (language-value = 0.88)
  [ set alp 99.2175506238225
    set bet 14.393302357794]
  if (learning-acceptance = 0.1) and (language-value = 0.89)
  [ set alp 94.8289601647515
    set bet 12.5968377731715]
  if (learning-acceptance = 0.1) and (language-value = 0.9)
  [ set alp 90.6237850040874
    set bet 10.9581983337875]
  if (learning-acceptance = 0.1) and (language-value = 0.91)
  [ set alp 90.2360249253183
    set bet 9.82554092667983]
  if (learning-acceptance = 0.1) and (language-value = 0.92)
  [ set alp 88.9746809298878
    set bet 8.64997225477284]
  if (learning-acceptance = 0.1) and (language-value = 0.93)
  [ set alp 86.7447029027167
    set bet 7.45390236902169]
  if (learning-acceptance = 0.1) and (language-value = 0.94)
  [ set alp 83.5011455042449
    set bet 6.26603056410073]
  if (learning-acceptance = 0.1) and (language-value = 0.95)
  [ set alp 79.2801102163958
    set bet 5.12000580086293]
  if (learning-acceptance = 0.1) and (language-value = 0.96)
  [ set alp 74.2210291824358
    set bet 4.05087621593482]
  if (learning-acceptance = 0.1) and (language-value = 0.97)
  [ set alp 68.5585385499958
    set bet 3.08943933659781]
  if (learning-acceptance = 0.1) and (language-value = 0.98)
  [ set alp 62.566314148102
    set bet 2.25645539077759]
  if (learning-acceptance = 0.1) and (language-value = 0.99)
  [ set alp 56.4575277155028
    set bet 1.56017704763134]
  ;; for fixed biases (in the reality language-value = 0.01 and learning-acceptance = 0.00001 ; this is just to make the writing simpler and use Behhavior space without problems)
  ;if (language-value = 0.1) and (learning-acceptance = 0)
  ;[ set alp 1000
  ;  set bet 5000000 ]

  if (language-value = 0.1) and (learning-acceptance = 0)
  [ set alp 300000000
    set bet 300000000 * 9 ]

  ;; strongly biased toward 0.9 : this was not used in this analysis
  if (language-value = 0.9) and (learning-acceptance = 0.1)
  [ set alp 90.6237
    set bet 10.9582 ]

  ;; weakly biased toward 0.9 : this was not used in this analysis
  if (language-value = 0.9) and (learning-acceptance = 0.6)
  [ set alp 6.1858
    set bet 1.5762 ]

  ;; unbiased
  if (language-value = 0.5) and (learning-acceptance = 0.9)
  [ set alp 2.1997
    set bet 2.1997 ]

  ;; strongly biased toward 0.1
  if (language-value = 0.1) and (learning-acceptance = 0.1)
  [ set alp 10.96
    set bet 90.62 ]

  ;; weakly biased toward 0.1
  if (language-value = 0.1) and (learning-acceptance = 0.6)
  [ set alp 1.58
    set bet 6.19 ]

  ;; totally flat : not used in this analysis
  if (learning-acceptance = 1)
  [ set alp 1
    set bet 1 ]

end

;; here is the function that compute the previous numbers automatically.
;to find-initial-alpha-beta-bayesian-complete
;
;  ;; download function
;  r:eval "betaExpert <- function(best, lower, upper, p = 0.95, method = 'mode'){f_mode <- function(x, mode, p, target){return(sum((qbeta(p = p, shape1 = x, shape2 = (x * (1 - mode) + 2 * mode - 1) / mode) -target) ^ 2))}; a <- optimize(f_mode, c(0, 1000), mode = best, p = (c(0, p) + (1 - p) / 2), target = c(lower, upper))$minimum; b <- (a * (1 - best) + 2 * best - 1) / best; out <- list(alpha = a, beta = b); return(out)}"
;
;  ifelse learning-acceptance <= 0.99
;  [ if language-value < 0.50
;    [ let therange (1 - language-value) * learning-acceptance
;      set upperuncert language-value + therange
;      ifelse therange > language-value [ set loweruncert 0.01 ] [ set loweruncert language-value - therange ]
;    ]
;    if language-value > 0.50
;    [ let therange language-value * learning-acceptance
;      set loweruncert language-value - therange
;      ifelse therange > (1 - language-value) [ let setupperuncert 0.99 ] [ set upperuncert language-value + therange ]
;    ]
;    if language-value = 0.50
;    [ let therange language-value * learning-acceptance
;      set loweruncert language-value - therange
;      set upperuncert language-value + therange
;    ]
;
;    r:put "loweruncert" loweruncert
;    r:put "upperuncert" upperuncert
;    r:put "mu" language-value
;    r:eval "params <- betaExpert(best = mu, lower = loweruncert, upper = upperuncert, p = 0.99)"
;    set alp r:get "params$alpha"
;    set bet r:get "params$beta"
;  ]
;   ;; if learning-acceptance = 1 (totally flat prior)
;  [ set alp 1
;    set bet 1 ]
;
;end


to update-color

  if (update-algorithm = "dirichlet" )
  [
    ifelse (max [ langval ] of turtles = 0 )
    [ set mymax 0.5 ]
    [ set mymax max [ langval ] of turtles ]
    set langval-norm langval / mymax
    set color scale-color red langval-norm 0 1
    ;set size 0.5
    ;set color red
  ]
  if (update-algorithm = "continuous-SAM" or update-algorithm = "continuous-MAP" )
  [
    set color scale-color red mymean 0 1
    set size 1
  ]
  if (update-algorithm != "dirichlet" and  update-algorithm != "continuous-SAM" and  update-algorithm != "continuous-MAP")
  [
    set color scale-color red language-value 0 1
    set size language-value
  ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RESET DISTRIBUTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reset the network with the same state as the previous one
to reset-nodes
  clear-all-plots
  ask turtles [
    set state orig-state
    set state-cont orig-cont-state
    update-color
  ]
  reset-ticks
end

;; reset the network with new states
to redistribute-states
  clear-all-plots
  ifelse binary-states = TRUE
  [ distribute-states-binary-specific ]
  [ distribute-states-continuous ]
  reset-ticks
end

;; reset the network with the same language value as the previous one
to reset-lang-value
  clear-all-plots
  ask turtles [
    set language-value lang-state
    set language-value-cont lang-cont-state
    update-color
  ]
  reset-ticks
end

;; reset the network with new language values
to redistribute-lang-value
  clear-all-plots
  ask turtles [
    set language-value random-normal initial-value-language standard-dev
    set language-value-cont random-normal initial-value-language-control standard-dev-control
    update-color]
  ask turtles [
    set state orig-state
    update-color
  ]
  reset-ticks
end



;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; DYNAMIC NETWORK

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;


to rewire-friends
  ask turtles [
    if random-float 1.0 <= proba-rewire [
      set focus-turtle who

      ;; set values to all turtles except focus turtles
      ask turtles with [ who != focus-turtle ] [
        ifelse nw:distance-to turtle focus-turtle = FALSE
          [ set value-distance 0 ]
          [ set value-distance nw:distance-to turtle focus-turtle ]
        set similarity-lang-val  abs ( ( [ language-value ] of turtle focus-turtle ) - ( [ language-value ] of turtle who ) )
        set central nw:betweenness-centrality
      ]

      ;; normalize centralize so all measures have the same weight
      set max-value-distance (max [ value-distance ] of turtles with [ who != focus-turtle ] )
      set max-similarity-lang-val (max [ similarity-lang-val ] of turtles with [ who != focus-turtle ] )
      set max-centrality (max [ central ] of turtles with [ who != focus-turtle ] )
      ask turtles with [ who != focus-turtle ] [
        ifelse (max-value-distance != 0)
        [ set value-distance ( value-distance / max-value-distance ) ]
        [ set value-distance 0 ]
        set similarity-lang-val  ( similarity-lang-val / max-similarity-lang-val )
        set central  ( central / max-centrality )

        ;; set overal proba: give a general probability value of being rewired with to all turtles (except focus)
        set overall-proba ( ( (central * import-central) + (similarity-lang-val * import-lang) + ( value-distance * import-dist ) ) / (import-central + import-lang + import-dist) )
      ]

      ;; find turtle neighbours, so that it does not rewire with one of its preexisting neighbors
      set turtle-neighbors []
      ask link-neighbors [ set turtle-neighbors insert-item 0 turtle-neighbors who ]

      ;; create a list with turtle id according to their probability of being selected as the new friend
      set list-proba []
      let maximum-proba (max [ overall-proba ] of turtles with [ who != focus-turtle ] )
      ;print maximum-proba
      ask turtles with [ (who != focus-turtle) and (not member? who turtle-neighbors) ] [
        ; scale between 0 and 1
        ifelse (maximum-proba != 0)
        [ set overall-proba  ( overall-proba / maximum-proba ) ]
        [ set overall-proba 0 ]
        foreach ( range 0 round(overall-proba * 100)) [
          set list-proba insert-item 0 list-proba who
        ]
      ]

      ;; select a random item from the list
      ifelse not empty? list-proba
        [ set new-friend one-of list-proba ]
        [ ask one-of turtles with [ who != focus-turtle ] [ set new-friend who ] ]

      ;; delete a random link and create a new link to the new friend selected
      if any? link-neighbors [
        set turtle-unfriend [ who ] of ( one-of link-neighbors )
        ask undirected-edge focus-turtle turtle-unfriend [ die
                                                           set color gray]
      ]
      ask turtle focus-turtle [ create-undirected-edge-with turtle new-friend ]
    ]
  ]
end



;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; GO !

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; here, it's is possible to comment stuff for the program to go faster
;; for example, if the language value, alpha and beta of all nodes is not needed, comment the lines starting with set list-all-langval [], list-all-alpha, list-all-beta

to go

  ;; uncomment this if you want to record all utterances pronounced by the agents (you will also need to uncomment text below)
  ;set list-values []

  ;; if rewire-friends>0, then dynamic network
  rewire-friends

  ;; if synchronous, then affect to the new mean the older mean (otherwise it does not successfully update
  if synchronous = TRUE [
     if ( update-algorithm = "continuous-SAM" or update-algorithm = "continuous-MAP") [
      ask turtles [ set newmean mymean ]
      ask turtles [ set newsd mysd ]
    ]
    if ( update-algorithm = "dirichlet" ) [
      ask turtles [ set newvector vector ]
    ]
  ]

  ;; make people communicate via the selected communication algorithm
  ask turtles [ communicate-via update-algorithm ]

  ;; each agent produce two utterances at each round instead of one utterance
  if listen-twice [ ask turtles [ communicate-via update-algorithm ] ]

  ;; if the network is synchronous, affect the new value of language at the same time for all turtles
  if synchronous = TRUE [
    if ( update-algorithm = "bayesianSAM" or update-algorithm ="bayesianMAP") [
      ask turtles [ set language-value new-language-value ]
    ]
    if ( update-algorithm = "continuous-SAM" or update-algorithm = "continuous-MAP") [
      ask turtles [ set mymean newmean
                    set mysd newsd ]
    ]
    if ( update-algorithm = "dirichlet" ) [
      ask turtles [ set vector newvector ]
    ]
  ]


  ;; find the bias strength and value of language for better visualization
  if (update-algorithm = "dirichlet") [
    ask turtles [
      ;; take the highest value in vector
      set bias-strength max vector-mode
      set langval ( ( position bias-strength vector-mode ) + 1 )

    ]
  ]

  ;; update their color
  ask turtles [ update-color ]

  ;; this is in case you want to find other information: communities detection, pairwaise comparison and spatial autocorrelation
  ;; as this is super long to compute for each year, we just print it for year=0 and year = 999
  ;; please note that there are also computed for the very first moment, when the network is just created : if you want to compute it, please go to setup and uncomment these lines
  ;; but if you want to compute it for every year, you can comment the following line
   ifelse (year = 0) or (year = 4999) [
    ;; additionnal measures to retrieve: feel free to comment the one you do not want to compute
    ;detect-communities
    ;pairwaise-comparison
    ;spatial-autocor
  ]
  ;; when the year is not 0 or 999, the program automatically affect the value "NA" or empty list to these variables
  ;; also comment the variables you do not want to use
  [ ;set corr_1 "NA"
    ;set corr_2 "NA"
    ;set corr_3 "NA"
    ;set mean-pairwaise-comp "NA"
    ;set std-pairwaise-comp "NA"
    ;set list-communities-mean []
    ;set list-communities-std []
    ;set list-communities-nbnodes []
  ]

  set list-degrees []

  ;; add year
  set year year + 1
  tick

end


to detect-communities
  ;; variable to add in order to retrieve communities values
  set list-communities-mean []
  set list-communities-std []
  set list-communities-nbnodes []
  foreach ( range 0 length( agentsetlouv)) [
    x -> set id-community x
    set mean-com mean [ language-value ] of item id-community agentsetlouv

    ;; special case if communities with only one agent
    ifelse ( count item id-community agentsetlouv <= 1) [ set std-com "NA" ]
    [ set std-com standard-deviation [ language-value ] of item id-community agentsetlouv ]

    set nb-nodes count item id-community agentsetlouv
    set list-communities-mean insert-item 0 list-communities-mean   ( ( round ( mean-com * 10000 ) ) / 10000 )
    set list-communities-std insert-item 0 list-communities-std ( ( round ( std-com * 10000 ) ) / 10000 )
    set list-communities-nbnodes insert-item 0 list-communities-nbnodes nb-nodes
  ]
end

to spatial-autocor

  ;; There is probably an easier way to do it. But I didn't find a function that could concatenate the in-link-neighbors of in-link-neighbors inside one only agenset.
  ;; Because of this, I needed to use several tricks to be able to record the neighboring environment of a turtle...

  ;; initialize lists
  set list-spa-aut-focus-1 []  ;; list for focus turtles
  set list-spa-aut-neighbors-1 [] ;; list with mean of language for neighboring turtles (link <=1)
  set list-spa-aut-neighbors-2 [] ;; list with mean of language for neighboring turtles (link <=2)
  set list-spa-aut-neighbors-3 [] ;; list with mean of language for neighboring turtles (link <=3)

   ask turtles [

    ;; compute spatial correlation only for turtles who are not isolated
    if (count in-link-neighbors != 0)
    [
      ;; put focus turtles in a list
      set list-spa-aut-focus-1 insert-item 0 list-spa-aut-focus-1   ( [ language-value ] of turtle who )

      ;; create a list with the mean language value of neighbors (link<=1) for each focus turtle
      set langval-neighbors []                                                            ;; list with neighbors : just used for further computations
      ask in-link-neighbors [ set langval-neighbors insert-item 0 langval-neighbors who ] ;; add the neighboring (for link <= 1) turtle's personal number
      set langval-neighbors remove who langval-neighbors                                  ;; remove the focus turtle from this tlist
      set sum_all 0
      foreach langval-neighbors [ x -> set sum_all (( [ language-value ] of turtle x ) + sum_all ) ]   ;; compute the sum of language value of neighboring turtle
      set list-spa-aut-neighbors-1 insert-item 0 list-spa-aut-neighbors-1 ( sum_all / length langval-neighbors ) ;; divide this by the number of neighboring turtle, and store this number in a vector

      ;; repeat the same process, except that here link <=2 and duplicates are removed
      ask in-link-neighbors [
        ask in-link-neighbors [ set langval-neighbors insert-item 0 langval-neighbors who ] ;; for link = 2
      ]
      set langval-neighbors remove-duplicates langval-neighbors
      set langval-neighbors remove who langval-neighbors
      set sum_all 0
      foreach langval-neighbors [ x -> set sum_all (( [ language-value ] of turtle x ) + sum_all ) ]
      set list-spa-aut-neighbors-2 insert-item 0 list-spa-aut-neighbors-2  ( sum_all / length langval-neighbors )

      ;; repeat the same process, except that here link <=3 and duplicates are removed
      ask in-link-neighbors [
        ask in-link-neighbors [
          ask in-link-neighbors [set langval-neighbors insert-item 0 langval-neighbors who ] ;; for link = 3
        ]
      ]
      set langval-neighbors remove-duplicates langval-neighbors
      set langval-neighbors remove who langval-neighbors
      set sum_all 0
      foreach langval-neighbors [ x -> set sum_all (( [ language-value ] of turtle x ) + sum_all ) ]
      set list-spa-aut-neighbors-3 insert-item 0 list-spa-aut-neighbors-3  ( sum_all / length langval-neighbors )
    ]
  ]



  if ( length list-spa-aut-focus-1 ) != ( length list-spa-aut-neighbors-1 )
  [ print("problem with list 1")
    print(list-spa-aut-focus-1)
    print(list-spa-aut-neighbors-1)
    print(langval-neighbors)]

  if ( length list-spa-aut-focus-1 ) != ( length list-spa-aut-neighbors-2 )
  [ print("problem with list 2")
    print(list-spa-aut-focus-1)
    print(list-spa-aut-neighbors-2)
    print(langval-neighbors) ]

  if ( length list-spa-aut-focus-1 ) != ( length list-spa-aut-neighbors-3 )
  [ print("problem with list 3")
    print(list-spa-aut-focus-1)
    print(list-spa-aut-neighbors-3)
    print(langval-neighbors) ]

  ;; this method does not work well on Behaviour Space, sometimes R crashes...
;  ;; compute correlation coefficient
;  r:put "list_focus" list-spa-aut-focus-1
;  r:put "list_neigh1" list-spa-aut-neighbors-1
;  r:put "list_neigh2" list-spa-aut-neighbors-2
;  r:put "list_neigh3" list-spa-aut-neighbors-3
;
;  r:eval "corr_1 <- cor.test(list_focus, list_neigh1, method='spearman')$estimate"
;  r:eval "corr_2 <- cor.test(list_focus, list_neigh2, method='spearman')$estimate"
;  r:eval "corr_3 <- cor.test(list_focus, list_neigh3, method='spearman')$estimate"
;
;  set corr_1 r:get "corr_1"
;  set corr_2 r:get "corr_2"
;  set corr_3 r:get "corr_3"

  ;;... so we use an other method using stats extension :(

  ;; initialize lists
  set newlist_1 []
  set newlist_2 []
  set newlist_3 []

  ;; transform the format into another format used by stats extension
  ;; because the stats extension do not crash
  foreach (range 1 length list-spa-aut-focus-1 1 )
    [x -> let incr x
     let mysublist1 list ( item incr list-spa-aut-focus-1 ) ( item incr list-spa-aut-neighbors-1 )
     set newlist_1 insert-item 0 newlist_1 mysublist1
     let mysublist2 list ( item incr list-spa-aut-focus-1 ) ( item incr list-spa-aut-neighbors-2 )
     set newlist_2 insert-item 0 newlist_2 mysublist2
     let mysublist3 list ( item incr list-spa-aut-focus-1 ) ( item incr list-spa-aut-neighbors-3 )
      set newlist_3 insert-item 0 newlist_3 mysublist3 ]

  ; Create stats tables
  let tbl1 stats:newtable-from-row-list newlist_1
  let tbl2 stats:newtable-from-row-list newlist_2
  let tbl3 stats:newtable-from-row-list newlist_3

  ;; compute correlation
  set corr_1 item 0 item 1 stats:correlation tbl1
  set corr_2 item 0 item 1 stats:correlation tbl2
  set corr_3 item 0 item 1 stats:correlation tbl3

end

;; compute the difference between biased and unbiased nodes using a pairwaise comparison --> takes a lot of time
to pairwaise-comparison
  set list-diff-biased []
  ask turtles with [state = init-langval-1][
    set turtle-biased who
    ask turtles with [state = init-langval-0][
      set turtle-control who
      set diff ( [language-value] of turtle turtle-biased ) - ( [language-value] of turtle turtle-control )
      set list-diff-biased insert-item 0 list-diff-biased diff
    ]
  ]
  ifelse not empty? list-diff-biased
  [ set mean-pairwaise-comp mean list-diff-biased
    set std-pairwaise-comp standard-deviation list-diff-biased ]
  [ set mean-pairwaise-comp "NA"
    set std-pairwaise-comp  "NA" ]



end


;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; COMMUNICATION ALGORITHM

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic algorithm calling other functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to communicate-via [ algorithm ] ;; node procedure


  ;; Discrete Grammar ;;
  if (algorithm = "threshold")
  [ listen-threshold ]

  if (algorithm = "individual")
    [ listen-individual ]

  if (algorithm = "reward")
      [ speak-reward
        ask link-neighbors
        [ listen-reward [spoken-state] of myself ]

        speak-reward-control
        ask link-neighbors
        [ listen-reward-control [spoken-state] of myself ]
   ]

   if (algorithm = "probabilistic")
      [ speak-probabilistic
        ask link-neighbors
        [ listen-probabilistic [spoken-state] of myself ]

        speak-probabilistic-control
        ask link-neighbors
        [ listen-probabilistic-control [spoken-state] of myself ]
   ]


  if (  ( algorithm = "SAM" ) or ( algorithm = "MAP" )) [

    ;; speak according to Bayesian algorithm
    ifelse (algorithm = "SAM")
    [ speak-bayesianSAM ]
    [ speak-bayesianMAP ]

    ;; hear (same bayesian algorithm for SAM and MAP)
    ask link-neighbors [ listen-bayesian [spoken-state] of myself ]

    ;; record the values of each utterances: uncomment this if you want to measure it
    ;if (who = 1) [ set mynumber 101 ]
    ;if (who = 0) [ set mynumber 102 ]
    ;if (who != 0) and (who != 1) [ set mynumber who]
    ;set list-values insert-item 0 list-values spoken-state
    ;set list-values insert-item 0 list-values mynumber


    ;; value for control (not coded yet)
    ;speak-bayesianSAM-control
    ;ask link-neighbors
    ;[ listen-bayesian-control [spoken-state] of myself ]
  ]

  if ( algorithm = "dirichlet" ) [
    speak-dirichlet
    ask link-neighbors [ listen-bay-dirichlet [ spoken-state ] of myself ]
  ]

  if (  ( algorithm = "continuous-SAM" ) or ( algorithm = "continuous-MAP" )) [

    ;; speak according to Bayesian algorithm
    ifelse (algorithm = "continuous-SAM")
    [ speak-continuous-SAM ]
    [ speak-continuous-MAP ]

    ;; hear (same bayesian algorithm for SAM and MAP)
    ask link-neighbors [ listen-bay-continuous [spoken-state] of myself ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Threshold algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to listen-threshold ;; node procedure
  let grammar-one-sum sum [language-value] of link-neighbors
  ifelse grammar-one-sum >= (count link-neighbors * threshold-val)
  [ ifelse synchronous = FALSE
   [set language-value 1
    set language-value-cont 1 ]
   [set new-language-value 1   ;; same thing here except that it's new-language-value for synchronicity
    set language-value-cont 1 ]
    ]
  [ ;; if there are not enough neighbors with grammar 1,
    ;; and 1 is not a sink state, then change to 0
    if not sink-state-1 [ ifelse synchronous = FALSE
                          [set language-value 0
                          set language-value-cont 0]
                          [set new-language-value 0
                          set language-value-cont 0]
                         ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Individual algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to listen-individual
  if synchronous = FALSE [
    set language-value [language-value] of one-of link-neighbors
    set language-value-cont [language-value-cont] of one-of link-neighbors
  ]

  if synchronous = TRUE [
    set new-language-value [language-value] of one-of link-neighbors
    set language-value-cont [language-value-cont] of one-of link-neighbors

  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reward algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to speak-reward-control

  ;; for control language value:
  if logistic
  [ let filter-val 1 / (1 + exp (- (2 * language-value-cont - 1) * 5))
    ifelse random-float 1.0 <= filter-val
    [ set spoken-state 1 ]
    [ set spoken-state 0 ]
  ]

if not logistic
  [ ;; the slope needs to be greater than 1, so we arbitrarily set to 1.5
    ;; when state is >= 2/3, the biased-val would be greater than or equal to 1
    let biased-val 1.5 * language-value
    if biased-val >= 1 [ set biased-val 1 ]
    ifelse random-float 1.0 <= biased-val
    [ set spoken-state 1 ]
    [ set spoken-state 0 ]
  ]

end

to speak-reward ;; node procedure
  ;; alpha is the level of bias in favor of grammar 1, and is constant for all nodes
  ;; the alpha value of 0.025 works best with the logistic function, it is adjusted so that it takes input values [0,1] and output to [0,1]

  if logistic
  [ let gain (alpha + 0.1) * 20
    let filter-val 1 / (1 + exp (- (gain * language-value - 1) * 5))
    ifelse random-float 1.0 <= filter-val
    [ set spoken-state 1 ]
    [ set spoken-state 0 ]
  ]

  ;; for probabilistic learners who only have bias for grammar 1, no preference for discrete grammars (i.e., no logistic)
  if not logistic
  [ ;; the slope needs to be greater than 1, so we arbitrarily set to 1.5
    ;; when state is >= 2/3, the biased-val would be greater than or equal to 1
    let biased-val 1.5 * language-value
    if biased-val >= 1 [ set biased-val 1 ]
    ifelse random-float 1.0 <= biased-val
    [ set spoken-state 1 ]
    [ set spoken-state 0 ]
  ]
end


to listen-reward [heard-state]
  let gamma 0.01 ;; for now, gamma is the same for all nodes

  if synchronous = FALSE [
    ;; choose a grammar state to be in
    ifelse random-float 1.0 <= language-value
    [
      ifelse heard-state = 1 ; if grammar 1 was heard
      [ set language-value language-value + (gamma * (1 - language-value)) ]
      [ set language-value (1 - gamma) * language-value ]
    ][
      ;; if grammar 0 was heard
      ifelse heard-state = 0
      [ set language-value language-value * (1 - gamma) ]
      [ set language-value gamma + language-value * (1 - gamma) ]
    ]
  ]

  if synchronous = TRUE [
    ;; choose a grammar state to be in
    ifelse random-float 1.0 <= language-value
    [
      ifelse heard-state = 1 ; if grammar 1 was heard
      [ set new-language-value language-value + (gamma * (1 - language-value)) ]
      [ set new-language-value (1 - gamma) * language-value ]
    ][
      ;; if grammar 0 was heard
      ifelse heard-state = 0
      [ set new-language-value language-value * (1 - gamma) ]
      [ set new-language-value gamma + language-value * (1 - gamma) ]
    ]
  ]
end

to listen-reward-control [heard-state]
  let gamma 0.01 ;; for now, gamma is the same for all nodes
  ;; choose a grammar state to be in
  ifelse random-float 1.0 <= language-value-cont
  [
    ifelse heard-state = 1 ; if grammar 1 was heard
    [ set language-value-cont language-value-cont + (gamma * (1 - language-value-cont)) ]
    [ set language-value-cont (1 - gamma) * language-value-cont ]
  ][
    ;; if grammar 0 was heard
    ifelse heard-state = 0
    [ set language-value-cont language-value-cont * (1 - gamma) ]
    [ set language-value-cont gamma + language-value-cont * (1 - gamma) ]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Probabilistic algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to record-value [heard-state]

end

to speak-probabilistic
  ; produce 30 utterances according to value language
  set counter 0
  repeat 30 [
     ; increment counter if the turtle produced an utterance F1* according to its prior p probability (value language)
      if random 101 <= (language-value * 100) [set counter counter + 1]
  ]

  ;compute the frequency of utterances heard by the focus turtle
  set spoken-state counter / 30

end

to speak-probabilistic-control
  ; produce 30 utterances according to value language
  set counter 0
  repeat 30 [
     ; increment counter if the turtle produced an utterance F1* according to its prior p probability (value language)
      if random 101 <= (language-value-cont * 100) [set counter counter + 1]
  ]

  ;compute the frequency of utterances heard by the focus turtle
  set spoken-state counter / 30
end

to listen-probabilistic [heard-state]
  if (synchronous = FALSE) [
    ifelse heard-state >= language-value
    [ set language-value ( language-value + ( abs(language-value - heard-state) * bias-baseline ) ) ]
    [ set language-value ( language-value - ( abs(language-value - heard-state) * bias ) ) ]
  ]

  if (synchronous = TRUE) [
    ifelse heard-state >= language-value
    [ set new-language-value ( language-value + ( abs(language-value - heard-state) * bias-baseline ) ) ]
    [ set new-language-value ( language-value - ( abs(language-value - heard-state) * bias ) ) ]
  ]
end

to listen-probabilistic-control [heard-state]

  ifelse heard-state >= language-value-cont
     [ set language-value-cont ( language-value-cont + ( abs(language-value-cont - heard-state) * bias-baseline ) ) ]
     [ set language-value-cont ( language-value-cont - ( abs(language-value-cont - heard-state) * bias-baseline ) ) ]


end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bayesian algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; function to create a Beta distribution from a gamma distribution (R function did not work in Behaviour Search... ;( )
to-report random-beta [ #alpha #beta ]
  let XX random-gamma #alpha 1
  let YY random-gamma #beta 1
  report XX / (XX + YY)
end


to speak-bayesianSAM

  ;; previous R code that worked in Netlogo but not in Behavior Space...
;  r:put "a" alp
;  r:put "b" bet
;  r:eval "random_values <- rbeta(1, a, b)"
;  r:eval "utterance <- rbinom(n=1, size=1, prob=random_values)"
;  set spoken-state r:get "utterance"

  ifelse (alp != 1) and (bet != 1) [
    ;; select random number from the Beta distribution
    set my-proba random-beta alp bet

    ;; extract from this probability from a binomial distribution
    ifelse (my-proba > random-float 1) [set spoken-state 1] [set spoken-state 0]
  ]
  ;; if flat prior, than choose a random utterance between 0 and 1
  [ set spoken-state random 2 ]


end


to speak-bayesianMAP

  ifelse (alp != 1) and (bet != 1)
  [
    ;; previous R code that worked in Netlogo but not in Behavior Space...
    ; r put "mode" ((alp - 1) / (alp + bet - 2))
    ;r:eval "utterance <-rbinom(n=1, size=1, prob = mode)"
    ;set spoken-state r:get "utterance"

    ;; extract the mode of the Beta distribution
    set my-proba ((alp - 1) / (alp + bet - 2))

    ;; extract the utterance value based on the probability of the mode
    ifelse (my-proba > random-float 1) [set spoken-state 1] [set spoken-state 0]
  ]
  ;; if flat prior, than choose a random utterance between 0 and 1
  [ set spoken-state random 2 ]

end

to listen-bayesian [heard-state]

  ;; the exact change in Bayesian formula is newalpha = alpha + X and newbeta = beta + N * X, where X is the number of utterances = 1 and N the total number of utterances
  ;; in our case agents always receive only one utterance at the time, so N always = 1
  set alp alp + heard-state
  set bet bet + 1 - heard-state

  ifelse (alp != 1) and (bet != 1) and (synchronous = FALSE)
  [ set language-value (alp - 1) / (alp + bet - 2) ]
  [ set language-value 0.5 ] ;; arbitrary decision: flat priors do not have mode, but we assume that 0.5 is the best way to represent the agent's belief

  ifelse (alp != 1) and (bet != 1) and (synchronous = TRUE)
  [ set new-language-value (alp - 1) / (alp + bet - 2) ]
  [ set new-language-value 0.5 ] ;; arbitrary decision: flat priors do not have mode, but we assume that 0.5 is the best way to represent the agent's belief
end


to speak-dirichlet
 ;; find vector of mode
  let sumvec sum vector
  set vector-mode map [ i -> i / sumvec ] vector

  ifelse vec-dirichlet = False
  [ ;; draw samples from this multinomial distribution
    set fake-vector (range 1 11)
    set new-vec vector-mode
    let pairs (map list fake-vector new-vec)
    set spoken-state first rnd:weighted-one-of-list pairs [ [p] -> last p ]
  ]
  [ ;; sample directly from the dirichlet distribution
    set fake-vector map [ i -> random-gamma i 1 ] vector
    let sumvec2 sum fake-vector
    set spoken-state map [ i -> i / sumvec2 ] fake-vector
  ]

end

to listen-bay-dirichlet [ heard-state ]

  ifelse synchronous = False [
    ifelse vec-dirichlet = False [
      ;; hear item and add + 1 in the corresponding category
      let hearditem heard-state - 1
      set vector replace-item hearditem vector (item hearditem vector + 1 )
    ] [
      ;; hear vector of item and add this new vector to the previous vector
      set new-vec [ ]
      (foreach vector heard-state  [ [a b] -> set new-vec insert-item ( length new-vec) new-vec (a + b) ])
      set vector new-vec
    ]
  ]
  [
    ifelse vec-dirichlet = False [
      ;; hear item and add + 1 in the corresponding category
      let hearditem heard-state - 1
      set newvector replace-item hearditem newvector ( item hearditem newvector + 1 )
    ] [
      ;; hear vector of item and add this new vector to the previous vector
      set new-vec [ ]
      (foreach newvector heard-state  [ [a b] -> set new-vec insert-item ( length new-vec) new-vec (a + b) ])
      set newvector new-vec
    ]
  ]


end


to speak-continuous-MAP

 ;; the spoken utterance is the value of the mode of the normal distribution
 ;; .. which is equivalent to the mean
 set spoken-state mymean

end

to speak-continuous-SAM

  ;; extract a random number from the normal distribution
  ;; repeat the process until the extracted number is between 0 and 1
  set ran-num -1
  while [ ran-num < 0 or ran-num > 1 ] [
    set ran-num random-normal mymean mysd
  ]

  ;; the chosen random number is the utterance spoken by the individual
  set spoken-state ran-num
  ;print(spoken-state)

end

to listen-bay-continuous [ heard-state ]

  ifelse (synchronous = FALSE)
  [
    ;; if this is not synchronous, hear the utterance said and update immediately
    let new-mean ( 1 / ( ( 1 / mysd ) + ( 1 / fixed-value-sd ) ) ) * ( ( mymean / mysd ) + ( heard-state / fixed-value-sd ) )
    let new-sd ( (1 / mysd ) + ( 1 / fixed-value-sd ) ) ^ (-1)
    set mymean new-mean
    set mysd new-sd
  ]
  [
    ;; if the network is synchronous, hear the utterance and keep it in the 2 variables newmean and newsd
    set newmean ( 1 / ( ( 1 / newsd ) + ( 1 / fixed-value-sd ) ) ) * ( ( newmean / newsd ) + ( heard-state / fixed-value-sd ) )
    set newsd ( (1 / newsd ) + ( 1 / fixed-value-sd ) ) ^ (-1)
  ]

end

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; ASSORTATIVITY

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;


to compute-assort-link

  ;------------ 1.  COMPUTE AVERAGE of X and Y -------------;

  set total-Y 0
  set total-X 0

  ;; FIRST, compute total for directed-edges going from SMALL to BIG
  ask links [
    set X-or-Y "xxx"
    ask both-ends [
      ifelse X-or-Y = "xxx" [
        set subtotal1 count my-links
        set turtlenum1 turtle who
        set X-or-Y "yyy"
      ]
      [
        set subtotal2 count my-links
        set turtlenum2 turtle who
      ]
    ]
    ifelse turtlenum1 < turtlenum2 [
      set total-X ( total-X + subtotal1 )
      set total-Y ( total-Y + subtotal2 )
    ]
    [
      set total-X ( total-X + subtotal2 )
      set total-Y ( total-Y + subtotal1 )
    ]

  ]

  ;; SECOND, compute total for directed-edges going from BIG to SMALL
  ask links [
    set X-or-Y "xxx"
    ask both-ends [
      ifelse X-or-Y = "xxx" [
        set subtotal1 count my-links
        set turtlenum1 turtle who
        set X-or-Y "yyy"
      ]
      [
        set subtotal2 count my-links
        set turtlenum2 turtle who
      ]
    ]
    ifelse turtlenum1 > turtlenum2 [
      set total-X ( total-X + subtotal1 )
      set total-Y ( total-Y + subtotal2 )
    ]
    [
      set total-X ( total-X + subtotal2 )
      set total-Y ( total-Y + subtotal1 )
    ]

  ]
  set avgX total-X / ( (count links) * 2)
  set avgY total-Y / ( (count links) * 2)


  ;------------ 2.  COMPUTE ASSORTATIVITY -------------;
  set total-up 0
  set sumsquaredX 0
  set sumsquaredY 0

  ;; FIRST, compute total for directed-edges going from SMALL to BIG
  ask links [
    set X-or-Y "xxx"
    ask both-ends [
      ifelse X-or-Y = "xxx" [
        set turtlenum1 turtle who
        set subtotal1 ( count my-links - avgX )
        set subtotal2 ( count my-links - avgY )
        set X-or-Y "yyy"
      ]
      [ set turtlenum2 turtle who
        set subtotal3 ( count my-links - avgX )
        set subtotal4 ( count my-links - avgY )
      ]
    ]
    ifelse turtlenum1 < turtlenum2 [
      set XavgX subtotal1
      set YavgY subtotal4
    ] [
      set XavgX subtotal3
      set YavgY subtotal2
    ]

    set sumsquaredX sumsquaredX + (XavgX ^ 2 )
    set sumsquaredY sumsquaredY + (YavgY ^ 2 )
    set total-up ( total-up + ( XavgX * YavgY ) )

  ]

  ;; SECOND, compute total for directed-edges going from BIG to SMALL
  ask links [
    set X-or-Y "xxx"
    ask both-ends [
      ifelse X-or-Y = "xxx" [
        set turtlenum1 turtle who
        set subtotal1 ( count my-links - avgX )
        set subtotal2 ( count my-links - avgY )
        set X-or-Y "yyy"
      ]
      [ set turtlenum2 turtle who
        set subtotal3 ( count my-links - avgX )
        set subtotal4 ( count my-links - avgY )
      ]
    ]
    ifelse turtlenum1 > turtlenum2 [
      set XavgX subtotal1
      set YavgY subtotal4
    ] [
      set XavgX subtotal3
      set YavgY subtotal2
    ]

    set sumsquaredX sumsquaredX + (XavgX ^ 2 )
    set sumsquaredY sumsquaredY + (YavgY ^ 2 )

    set total-up ( total-up + ( XavgX * YavgY ) )

  ]

  set total-down ( ( sqrt sumsquaredX ) * (sqrt sumsquaredY ) )
  set assortativity total-up / total-down


end

to compute-assort-edge

  ;------------ 1.  COMPUTE AVERAGE of X and Y -------------;

  set total-Y 0
  set total-X 0

  ;; FIRST, compute total for directed-edges going from SMALL to BIG
  ask undirected-edges [
    set X-or-Y "xxx"
    ask both-ends [
      ifelse X-or-Y = "xxx" [
        set subtotal1 count my-links
        set turtlenum1 turtle who
        set X-or-Y "yyy"
      ]
      [
        set subtotal2 count my-links
        set turtlenum2 turtle who
      ]
    ]
    ifelse turtlenum1 < turtlenum2 [
      set total-X ( total-X + subtotal1 )
      set total-Y ( total-Y + subtotal2 )
    ]
    [
      set total-X ( total-X + subtotal2 )
      set total-Y ( total-Y + subtotal1 )
    ]

  ]

  ;; SECOND, compute total for directed-edges going from BIG to SMALL
  ask undirected-edges [
    set X-or-Y "xxx"
    ask both-ends [
      ifelse X-or-Y = "xxx" [
        set subtotal1 count my-links
        set turtlenum1 turtle who
        set X-or-Y "yyy"
      ]
      [
        set subtotal2 count my-links
        set turtlenum2 turtle who
      ]
    ]
    ifelse turtlenum1 > turtlenum2 [
      set total-X ( total-X + subtotal1 )
      set total-Y ( total-Y + subtotal2 )
    ]
    [
      set total-X ( total-X + subtotal2 )
      set total-Y ( total-Y + subtotal1 )
    ]

  ]
  set avgX total-X / ( (count undirected-edges) * 2)
  set avgY total-Y / ( (count undirected-edges) * 2)


  ;------------ 2.  COMPUTE ASSORTATIVITY -------------;
  set total-up 0
  set sumsquaredX 0
  set sumsquaredY 0

  ;; FIRST, compute total for directed-edges going from SMALL to BIG
  ask undirected-edges [
    set X-or-Y "xxx"
    ask both-ends [
      ifelse X-or-Y = "xxx" [
        set turtlenum1 turtle who
        set subtotal1 ( count my-links - avgX )
        set subtotal2 ( count my-links - avgY )
        set X-or-Y "yyy"
      ]
      [ set turtlenum2 turtle who
        set subtotal3 ( count my-links - avgX )
        set subtotal4 ( count my-links - avgY )
      ]
    ]
    ifelse turtlenum1 < turtlenum2 [
      set XavgX subtotal1
      set YavgY subtotal4
    ] [
      set XavgX subtotal3
      set YavgY subtotal2
    ]

    set sumsquaredX sumsquaredX + (XavgX ^ 2 )
    set sumsquaredY sumsquaredY + (YavgY ^ 2 )
    set total-up ( total-up + ( XavgX * YavgY ) )

  ]

  ;; SECOND, compute total for directed-edges going from BIG to SMALL
  ask undirected-edges [
    set X-or-Y "xxx"
    ask both-ends [
      ifelse X-or-Y = "xxx" [
        set turtlenum1 turtle who
        set subtotal1 ( count my-links - avgX )
        set subtotal2 ( count my-links - avgY )
        set X-or-Y "yyy"
      ]
      [ set turtlenum2 turtle who
        set subtotal3 ( count my-links - avgX )
        set subtotal4 ( count my-links - avgY )
      ]
    ]
    ifelse turtlenum1 > turtlenum2 [
      set XavgX subtotal1
      set YavgY subtotal4
    ] [
      set XavgX subtotal3
      set YavgY subtotal2
    ]

    set sumsquaredX sumsquaredX + (XavgX ^ 2 )
    set sumsquaredY sumsquaredY + (YavgY ^ 2 )

    set total-up ( total-up + ( XavgX * YavgY ) )

  ]

  set total-down ( ( sqrt sumsquaredX ) * (sqrt sumsquaredY ) )
  set assortativity total-up / total-down


end

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; TO REPORT

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; please note that some of the variables may not be used.


; the mean language value of control nodes
to-report mean-langval-cont
  report mean [ language-value-cont ] of turtles
end


; useful in previous program, not used now
to-report find-partner
  let pick random-float sum [count link-neighbors] of (turtles with [any? link-neighbors])
  let partner nobody
  ask turtles
  [ ;; if there's no winner yet
    if partner = nobody
    [ ifelse count link-neighbors > pick
      [ set partner self]
      [ set pick pick - (count link-neighbors)]
    ]
  ]
  report partner
end

; number of influencers... not used anymore
to-report max-value-for-influencers
  report ((percent-state-1 / 100) * num-nodes)
end

;; number of influencers in the control situation... not used anymore
to-report max-value-for-influencers-control
  report ((percent-state-control-1 / 100) * num-nodes)
end

;; reports a string of the agent's initial grammar... not used anymore
to-report orig-grammar-string
  report ifelse-value orig-state = 1.0 ["1"] ["0"]
end

; the ratio of centrality of turtles having the bias : for example, biased turtles are N times for influential than unbiased turtles
to-report ratio-cen
  report my-ratio-cen
end

;; the connection probability
to-report con-prob
  report my-con-prob
end


;; the mean language value of turtle
to-report mean-langval
 report ( round ( ( mean [ language-value ] of turtles ) * 10000 ) / 10000 )
end

;; the mean language value of biased turtles
to-report mean-langval-1
 ifelse any? turtles with [state = init-langval-1]
  [ report ( round ( ( mean [ language-value ] of turtles with [ state = init-langval-1 ] ) * 10000 ) / 10000 ) ]
  [ report "NA" ]
end

;; the mean language value of unbiased turtles
to-report mean-langval-0
  ifelse any? turtles with [state = init-langval-0]
  [ report ( round ( ( mean [ language-value ] of turtles with [ state = init-langval-0 ] ) * 10000 ) / 10000 ) ]
  [ report "NA" ]
end

;; the mean alpha of unbiased turtles
to-report alp-0
  ifelse any? turtles with [state = init-langval-0]
  [ report mean [ alp ] of turtles with [ state = init-langval-0 ] ]
  [ report "NA" ]
end

;; the mean beta of unbiased turtles
to-report bet-0
  ifelse any? turtles with [state = init-langval-0]
  [ report mean [ bet ] of turtles with [ state = init-langval-0 ] ]
  [ report "NA" ]
end

;; the mean alpha of biased turtles
to-report alp-1
  ifelse any? turtles with [state = init-langval-1]
  [ report mean [ alp ] of turtles with [ state = init-langval-1 ] ]
  [ report "NA" ]
end

;; the mean beta of biased turtles
to-report bet-1
  ifelse any? turtles with [state = init-langval-1]
  [ report mean [ bet ] of turtles with [ state = init-langval-1 ] ]
  [ report "NA" ]
end

;; report list with the language values of all turtles
to-report all-lang-val
  report [ language-value ] of turtles
end

;; report list with the language values of biased turtles
to-report all-langval-1
  report [ language-value ] of turtles with [ state = init-langval-1 ]
end

;; report list with the language values of unbiased turtles
to-report all-langval-0
  report [ language-value ] of turtles with [ state = init-langval-0 ]
end

;; report list with the alpha of all turtles
to-report all-alpha
 report [ alp ] of turtles
end

;; report list with the beta of all turtles
to-report all-beta
 report [ bet ] of turtles
end

;; report list with the beta of all turtles
to-report langval-report
 report [ langval ] of turtles
end

to-report bias-strength-report
 report max [ vector ] of turtles
end

to-report vector-report
 report [ vector ] of turtles
end

to-report mean-report
 report [ mymean ] of turtles
end

to-report sd-report
 report [ mysd ] of turtles
end

;; report list with the mean language of each community
to-report communities-mean
 report list-communities-mean
end

;; report list with the std language of each community
to-report communities-std
 report list-communities-std
end

;; report list with the number of nodes of each community
to-report communities-nbnodes
 report list-communities-nbnodes
end

;; report spatial autocorrelation for Neighbors <=1
to-report spatial-corr-1
 report corr_1
end

;; report spatial autocorrelation for Neighbors <=2
to-report spatial-corr-2
 report corr_2
end

;; report spatial autocorrelation for Neighbors <=3
to-report spatial-corr-3
 report corr_3
end

;; report mean pairwaise comparison between unbiased and biased nodes
to-report mean-pairwaise-com
 report mean-pairwaise-comp
end

;; report std pairwaise comparison between unbiased and biased nodes
to-report std-pairwaise-com
 report std-pairwaise-comp
end

;; report list of degrees of all nodes
to-report degrees
  report list-degrees
end

to-report init-average-mode
  report init-mean-mode
end

to-report init-median-mode
  report init-med-mode
end

to-report mean-pathlength
  report nw:mean-path-length
end

to-report clust-coeff
  report mean [nw:clustering-coefficient] of turtles
end

to-report assort-coeff
  report assortativity
end

to-report random-num
  report my-ran-num
end

to-report modularity
  report nw:modularity list-modularity
end

to-report avg-degree
  report mean [ count my-links ] of turtles

end

to-report degree-all
  report [ count my-links ] of turtles
end

to-report bet-all
  report [ nw:betweenness-centrality ] of turtles
end


to-report clo-all
  report [ nw:closeness-centrality ] of turtles
end


to-report eig-all
  report  [ precision nw:eigenvector-centrality 3 ] of turtles
end

to-report clus-all
  report [ nw:clustering-coefficient ] of turtles
end
;;;


to-report centbet-biased
  report mean [ nw:betweenness-centrality ] of turtles with [ myidentity = "biased" ]
end

to-report centbet-unbiased
  report mean [ nw:betweenness-centrality ] of turtles with [ myidentity = "unbiased" ]
end


to-report cluscoeff-biased
  report mean [ nw:clustering-coefficient ] of turtles with [ myidentity = "biased" ]
end

to-report cluscoeff-unbiased
  report mean [ nw:clustering-coefficient ] of turtles with [ myidentity = "unbiased" ]
end

to-report centeigen-biased
  report mean [ nw:eigenvector-centrality ] of turtles with [ myidentity = "biased" ]
end

to-report centeigen-unbiased
  report mean [ nw:eigenvector-centrality ] of turtles with [ myidentity = "unbiased" ]
end

to-report centclos-biased
  report mean [ nw:closeness-centrality ] of turtles with [ myidentity = "biased" ]
end

to-report centclos-unbiased
  report mean [ nw:closeness-centrality ] of turtles with [ myidentity = "unbiased" ]
end

to-report assor-biased
  report mean [ loc-ass-bis ] of turtles with [ myidentity = "biased" ]
end

to-report assor-unbiased
  report mean [ loc-ass-bis ] of turtles with [ myidentity = "unbiased" ]
end

to-report deg-biased
  report mean [ count my-links ] of turtles with [ myidentity = "biased" ]
end

to-report deg-unbiased
  report mean [ count my-links ] of turtles with [ myidentity = "unbiased" ]
end

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; CODE BORROWING

;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------------------------------------------------------------------------------------;;

;; This code was borrowed from:
;;    - Language Change model library, which get inspired of Lottery Example. Available in the Model library.
;;    - NW General Examples.nlogo, a file powered by Netlogo showing all the possible features of the Network extension. Available at http://ccl.northwestern.edu/nw-ext-applet/nw-demo.html.




; Copyright 2007 Uri Wilensky.
; See Info tab for full copyright and license.

; Public Domain:
; To the extent possible under law, Uri Wilensky has waived all
; copyright and related or neighboring rights to this model.
@#$#@#$#@
GRAPHICS-WINDOW
600
10
1103
514
-1
-1
15.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
475
45
585
78
NIL
betweenness
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

TEXTBOX
310
30
480
56
Clusters
12
0.0
1

SLIDER
5
45
285
78
num-nodes
num-nodes
0
1000
300.0
10
1
NIL
HORIZONTAL

TEXTBOX
475
30
625
48
Centrality
12
0.0
1

BUTTON
5
85
90
130
setup/clear
setup
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
5
315
165
348
preferential attachment
preferential-attachment
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
5
575
110
608
lattice 2D
lattice-2d
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
115
540
215
573
nb-rows
nb-rows
6
20
7.0
1
1
NIL
HORIZONTAL

SLIDER
115
575
215
608
nb-cols
nb-cols
6
20
6.0
1
1
NIL
HORIZONTAL

SWITCH
5
540
110
573
wrap
wrap
0
1
-1000

TEXTBOX
55
10
205
28
Generate networks
16
0.0
1

BUTTON
475
85
585
118
NIL
eigenvector
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
5
190
90
223
random
generate-random
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
95
190
285
223
connection-prob
connection-prob
0
1
0.5
0.001
1
NIL
HORIZONTAL

BUTTON
5
610
110
643
kleinberg
small-world-lattice
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
115
610
215
643
clustering-exponent
clustering-exponent
0
10
0.1
0.1
1
NIL
HORIZONTAL

BUTTON
475
120
585
153
NIL
closeness
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
230
565
420
598
weak component clusters
weak-component
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

CHOOSER
5
695
205
740
links-to-use
links-to-use
"undirected" "directed"
0

PLOT
300
225
585
385
Degree distribution
Degrees
Nb nodes
0.0
30.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ count my-links ] of turtles"

BUTTON
5
900
90
933
save matrix
save-matrix
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
5
935
90
968
load matrix
load-matrix
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
385
390
470
435
NIL
count turtles
17
1
11

MONITOR
300
390
380
435
NIL
count links
17
1
11

BUTTON
230
670
420
703
biggest maximal cliques
find-biggest-cliques
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
175
315
230
348
NIL
ring
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
5
490
90
535
NIL
wheel
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
235
315
290
348
NIL
star
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
475
390
585
435
Mean path length
nw:mean-path-length
3
1
11

CHOOSER
5
135
105
180
layout
layout
"spring" "circle" "radial" "tutte"
0

BUTTON
120
135
205
180
layout once
layout-turtles
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
120
85
205
130
layout
layout-turtles
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
230
600
420
633
highlight bicomponents
highlight-bicomponents
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
230
635
420
668
highlight maximal cliques
highlight-maximal-cliques
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

CHOOSER
95
490
215
535
spokes-direction
spokes-direction
"inward" "outward"
0

BUTTON
95
900
195
933
save GraphML
save-graphml
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
95
935
195
968
load GraphML
load-graphml
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
305
50
455
83
detect communities
community-detection
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
5
270
90
303
small world
small-world-ring
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
5
235
285
268
neighborhood-size
neighborhood-size
0
floor ((num-nodes - 1) / 2)
2.0
1
1
NIL
HORIZONTAL

SLIDER
95
270
285
303
rewire-prob
rewire-prob
0
1
0.7
0.01
1
NIL
HORIZONTAL

SWITCH
5
745
200
778
clear-before-generating?
clear-before-generating?
0
1
-1000

MONITOR
305
90
455
135
clustering coefficient
mean [nw:clustering-coefficient] of turtles
5
1
11

TEXTBOX
335
10
645
40
Observe network properties
16
0.0
1

TEXTBOX
440
435
455
965
...................................................................................................................................................................................
11
0.0
1

SLIDER
660
805
840
838
percent-state-1
percent-state-1
0
100
30.0
10
1
%
HORIZONTAL

BUTTON
790
1335
945
1368
redistribute states
redistribute-states
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
790
1375
945
1408
reset states
reset-nodes
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
1260
635
1385
668
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

CHOOSER
1155
45
1370
90
update-algorithm
update-algorithm
"individual" "threshold" "reward" "probabilistic" "SAM" "MAP" "dirichlet" "continuous-SAM" "continuous-MAP"
6

SLIDER
1155
115
1370
148
threshold-val
threshold-val
0
1
0.3
0.05
1
NIL
HORIZONTAL

SWITCH
1155
155
1297
188
sink-state-1
sink-state-1
0
1
-1000

SWITCH
1155
255
1275
288
logistic
logistic
1
1
-1000

SLIDER
1155
220
1375
253
value-bias
value-bias
0
0.05
0.025
0.005
1
NIL
HORIZONTAL

PLOT
1140
740
1495
940
Mean State of language users in the network
Time
State
0.0
100.0
0.0
1.0
true
true
"" ""
PENS
"lang val" 1.0 0 -16777216 true "" "plot mean [language-value] of turtles"
"lang val control" 1.0 0 -13791810 true "" "plot mean [language-value-cont] of turtles"

BUTTON
1255
680
1390
713
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

TEXTBOX
230
540
445
576
Highlight links and nodes
16
0.0
1

TEXTBOX
10
870
220
906
Save files and graph
16
0.0
1

TEXTBOX
10
665
160
683
Other parameters
16
0.0
1

TEXTBOX
620
520
1105
556
Choose internal properties (initial states) for Binomial/Beta only !
16
0.0
1

TEXTBOX
1130
95
1405
121
With threshold algorithm:
12
0.0
1

SLIDER
20
1335
255
1368
initial-value-language
initial-value-language
0
1
0.54
0.01
1
NIL
HORIZONTAL

SLIDER
660
1100
845
1133
percent-influent
percent-influent
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
20
1375
255
1408
standard-dev
standard-dev
0
1
0.5
0.05
1
NIL
HORIZONTAL

TEXTBOX
660
620
715
646
If binary:
12
0.0
1

TEXTBOX
1135
15
1450
51
Choose communication algorithm
16
0.0
1

BUTTON
20
1500
255
1533
redistribute language values
redistribute-lang-value
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
20
1540
255
1573
reset language value
reset-lang-value
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1130
200
1380
231
With reward and continuous algorithm:
12
0.0
1

SLIDER
20
1415
255
1448
initial-value-language-control
initial-value-language-control
0
1
0.45
0.01
1
NIL
HORIZONTAL

SLIDER
20
1455
255
1488
standard-dev-control
standard-dev-control
0
1
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
660
845
840
878
percent-state-control-1
percent-state-control-1
0
100
66.0
1
1
%
HORIZONTAL

SLIDER
660
1140
845
1173
N-influent-control
N-influent-control
0
max-value-for-influencers-control
0.0
1
1
people
HORIZONTAL

SWITCH
765
585
912
618
binary-states
binary-states
1
1
-1000

TEXTBOX
885
620
1070
656
If continuous (still in the case of binomial bias), just the dispersion is continuous:
10
0.0
1

TEXTBOX
855
635
870
1285
...........................................................................................................................................................................................................................................................................................
12
0.0
1

TEXTBOX
1140
365
1325
391
-------------- Binary ---------------
10
124.0
1

SLIDER
1320
425
1492
458
max-value-bias
max-value-bias
0
1
0.57
0.01
1
NIL
HORIZONTAL

SWITCH
885
650
1035
683
is-state-random
is-state-random
1
1
-1000

MONITOR
825
1285
907
1330
Mean state
mean [ state ] of turtles
3
1
11

TEXTBOX
885
700
1100
726
if continuous but not random:
11
0.0
1

CHOOSER
885
720
1032
765
distribution-state
distribution-state
"exponential" "gamma" "normal" "poisson"
2

SLIDER
885
890
1082
923
mean-if-exponential
mean-if-exponential
0
0.3
0.03
0.01
1
NIL
HORIZONTAL

SLIDER
885
930
1080
963
alpha-if-gamma
alpha-if-gamma
0
1
0.92
0.01
1
NIL
HORIZONTAL

SLIDER
885
970
1080
1003
lambda-if-gamma
lambda-if-gamma
0
1
0.88
0.01
1
NIL
HORIZONTAL

SLIDER
885
1010
1080
1043
mean-if-normal
mean-if-normal
0
1
0.57
0.01
1
NIL
HORIZONTAL

SLIDER
885
1050
1080
1083
std-if-normal
std-if-normal
0
0.3
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
885
1090
1080
1123
mean-if-poisson
mean-if-poisson
0
1
0.87
0.01
1
NIL
HORIZONTAL

TEXTBOX
885
775
1090
910
Select parameters according to the distribution-state selected. \nPlease note that values are then normalized to fit inside a [0,1] range. So the values you select do not always correspond to the reality (a too high STD for normal distribution will change the value of mean selected)
10
0.0
1

TEXTBOX
885
1135
1145
1236
Do you want the values for state-control to be absolutely exactly the same as for state? (\"on\")\nOr do you want to use the same distribution state and values, but have different values for each nodes? (\"off)\nNote: \"off\" condition has not been coded yet :)\n
10
0.0
1

SWITCH
885
1220
1112
1253
exactly-same-for-control
exactly-same-for-control
0
1
-1000

PLOT
950
1280
1155
1420
Value state distribution
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.05 1 -16777216 true "" "histogram [ state ] of turtles"

MONITOR
230
925
350
970
Mean lang val
mean [ language-value ] of turtles
3
1
11

MONITOR
1280
955
1400
1000
Mean lang val cont
mean [language-value-cont] of turtles
3
1
11

SLIDER
1320
385
1492
418
min-value-bias
min-value-bias
0
1
0.29
0.01
1
NIL
HORIZONTAL

MONITOR
1290
280
1367
325
Mean bias
mean [ bias ] of turtles
3
1
11

SLIDER
230
760
402
793
proba-rewire
proba-rewire
0
1
0.0
0.001
1
NIL
HORIZONTAL

TEXTBOX
235
715
415
765
Dynamic network parameters
16
0.0
1

SLIDER
230
800
402
833
import-central
import-central
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
230
840
402
873
import-lang
import-lang
0
20
1.0
1
1
NIL
HORIZONTAL

SLIDER
230
880
402
913
import-dist
import-dist
0
20
9.0
1
1
NIL
HORIZONTAL

MONITOR
660
1295
782
1340
state 1 centrality
mean [ initial-centrality ] of turtles with [ (state = 1) or (state = init-langval-1) ]
3
1
11

MONITOR
660
1345
782
1390
state 0 centrality
mean [initial-centrality] of turtles with [ (state = 0) or (state = init-langval-0) ]
3
1
11

TEXTBOX
1130
475
1305
501
With Bayesian inference
12
0.0
1

SLIDER
1120
495
1315
528
learning-acceptance1
learning-acceptance1
0
1
0.08
0.01
1
NIL
HORIZONTAL

SLIDER
660
700
840
733
init-langval-1
init-langval-1
0
1
0.1
0.10
1
NIL
HORIZONTAL

SLIDER
660
740
840
773
init-langval-0
init-langval-0
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
1120
535
1315
568
learning-acceptance0
learning-acceptance0
0
1
0.9
0.10
1
NIL
HORIZONTAL

SLIDER
660
1205
845
1238
ratio-centrality-scalefree
ratio-centrality-scalefree
1
6
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
735
1175
855
1230
choose either N-influent either ratio-centrality
10
123.0
1

SWITCH
660
1035
840
1068
choose-N-influent
choose-N-influent
0
1
-1000

SWITCH
20
1250
225
1283
random-initial-lang-value
random-initial-lang-value
1
1
-1000

TEXTBOX
20
995
245
1031
Language initial values
16
0.0
1

TEXTBOX
20
1205
225
1256
Do you want language values to be the same as the state values? Highly recommended for Bayesian algorithms.
10
0.0
1

TEXTBOX
20
1290
240
1341
If you answered \"On\" to the previous question, set the parameters for language value.
10
0.0
1

TEXTBOX
210
640
225
960
..............................................................................................
12
0.0
1

TEXTBOX
660
645
830
696
Initial values of states. Choose 0 and 1 if you want a \"real\" binary feature.\nNote that langval-1 needs to be the biased state.
10
0.0
1

TEXTBOX
660
790
860
816
% of biased people in the population.
10
0.0
1

TEXTBOX
660
886
845
1041
Do you want to affect states according to centrality?\nIf no, click \"On\" and affect \"0\" N-influent people.\nIf yes, you have 2 options:\n- affect bias to the N most influent people\n- affect bias so that the mean centrality of biased people is RATIO times higher than the mean centrality of non-biased people\nONLY WORKS FOR SCALE-FREE NETWORKS.
10
0.0
1

TEXTBOX
660
1080
810
1098
If on:
12
0.0
1

TEXTBOX
665
1185
815
1203
If off:
12
0.0
1

SLIDER
660
1245
845
1278
ratio-centrality-control
ratio-centrality-control
1
5
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
1250
605
1400
623
GO and see graph
16
0.0
1

SLIDER
15
1115
237
1148
number-utt-heard-start
number-utt-heard-start
0
4
0.0
2
1
NIL
HORIZONTAL

TEXTBOX
20
1060
260
1111
For Bayesian: number of utterances=1 heard before it starts out of the total number of utterances heard. Affect 0 to both sliders if you do not want any initial value of language.
10
0.0
1

SLIDER
15
1155
235
1188
total-numb-utt
total-numb-utt
0
4
0.0
4
1
NIL
HORIZONTAL

MONITOR
355
925
427
970
langval-1
mean [ language-value ] of turtles with [ state = init-langval-1 ]
3
1
11

MONITOR
435
925
512
970
lang-val-0
mean [ language-value ] of turtles with [ state = init-langval-0 ]
3
1
11

MONITOR
475
585
555
630
NIL
con-prob
6
1
11

MONITOR
475
635
555
680
NIL
ratio-cen
17
1
11

TEXTBOX
1320
365
1500
391
-------------- Continuous --------------- 
10
125.0
1

SLIDER
1115
425
1310
458
value-bias-for-state-0
value-bias-for-state-0
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
1115
385
1310
418
value-bias-for-state-1
value-bias-for-state-1
0
1
0.31
0.01
1
NIL
HORIZONTAL

TEXTBOX
1120
340
1490
381
For non-Bayesian algorithms, set the strength of the bias:
12
0.0
1

SWITCH
5
785
200
818
synchronous
synchronous
0
1
-1000

MONITOR
305
140
455
185
Mean degree
mean [ count my-links ] of turtles
2
1
11

MONITOR
475
480
592
525
Global assortativity
assort-coeff
5
1
11

SWITCH
5
825
200
858
listen-twice
listen-twice
1
1
-1000

BUTTON
5
360
140
393
language contact
lang-contact
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
145
360
290
393
family-size
family-size
0
num-nodes / 3
2.0
1
1
NIL
HORIZONTAL

SLIDER
5
400
140
433
nb-link-extra
nb-link-extra
0
20
7.0
1
1
NIL
HORIZONTAL

SLIDER
145
400
290
433
nb-family
nb-family
0
40
32.0
1
1
NIL
HORIZONTAL

MONITOR
475
430
585
475
modularity
nw:modularity list-modularity
5
1
11

SLIDER
280
1075
440
1108
nb-of-categories
nb-of-categories
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
280
1210
440
1243
init-dirichlet
init-dirichlet
0
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
280
1250
440
1283
value-init-dirichlet
value-init-dirichlet
0
nb-of-categories
4.0
1
1
NIL
HORIZONTAL

BUTTON
5
440
140
473
assort scalefree
assort-network
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
145
440
290
473
nb-additional-link
nb-additional-link
0
100
20.0
1
1
NIL
HORIZONTAL

SWITCH
295
440
412
473
high-assort
high-assort
0
1
-1000

TEXTBOX
1400
95
1550
113
With dirichlet:
12
0.0
1

SWITCH
1390
115
1535
148
vec-dirichlet
vec-dirichlet
1
1
-1000

SLIDER
475
1075
625
1108
fixed-value-sd
fixed-value-sd
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
475
1115
625
1148
mean-continuous-init
mean-continuous-init
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
475
1155
625
1188
sd-continuous-init
sd-continuous-init
0
1
0.1
0.01
1
NIL
HORIZONTAL

BUTTON
360
190
472
223
NIL
assortativity-1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
20
1030
170
1048
Binomial/Beta
13
123.0
1

TEXTBOX
305
1035
455
1053
Multinomial/Dirichlet
13
123.0
1

TEXTBOX
25
975
570
1001
.....................................................................................................................................
11
0.0
1

CHOOSER
380
1370
550
1415
type-ind-biased
type-ind-biased
"cluscoeff" "pathlen" "centbet" "centeigen" "centclos" "assor" "degree"
6

BUTTON
475
155
585
188
NIL
clustering
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
475
530
590
575
Sum local assortativity
sum [ loc-ass ] of turtles
17
1
11

BUTTON
475
190
585
223
NIL
assortativity-2
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
295
190
387
223
NIL
pathlength
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
380
1320
550
1365
bias-additionnal
bias-additionnal
"none" "random" "highest" "lowest"
1

SLIDER
290
1470
460
1503
init-dirichlet-add
init-dirichlet-add
0
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
290
1510
460
1543
value-init-dirichlet-add
value-init-dirichlet-add
0
nb-of-categories
4.0
1
1
NIL
HORIZONTAL

SLIDER
465
1470
637
1503
init-continuous-add
init-continuous-add
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
465
1510
635
1543
mean-continuous-init-add
mean-continuous-init-add
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
380
1420
552
1453
percent-bias
percent-bias
0
100
0.0
1
1
NIL
HORIZONTAL

TEXTBOX
495
1035
645
1053
Continuous/Normal
13
123.0
1

TEXTBOX
390
1300
615
1326
For heterogenous populations
11
0.0
1

TEXTBOX
620
550
1090
631
This is relevant *only* in the context of the first paper (individuals are intrinsically biased toward a feature (THIS PART), and then they also possess a language value (PART ON THE LEFT)
11
0.0
1

TEXTBOX
480
1215
635
1280
There is no option for adding an initial value of language here. Indeed, we did not find a good way to simulate the initial language value...
9
0.0
1

TEXTBOX
390
1195
540
1213
Adding initial language value
11
0.0
1

TEXTBOX
420
1055
570
1073
Initial parameters
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model demonstrates various features of the Netlogo NW extension: it allows you to generate various kinds of networks, lay them out on screen, and get information about them.

## HOW TO USE IT

The SETUP/CLEAR button initializes the model and clears any existing networks.

The LINKS-TO-USE chooser allow you to specify whether you want to use directed or undirected links: all the generators use the kind of links selected in the chooser. You can generate different networks with different kinds of links without clearing everything in between.

The value of LINKS-TO-USE is used by the different clusterers and measures as well. Be careful to use the right value for the network you are interested in. For example, if you ask for betweenness centrality with "directed links" selected in the chooser, but the network on the screen is undirected, the betweenness centrality values will all be zero, because the algorithm only takes directed links into account.

There is another chooser called LAYOUT. NetLogo currently offers four different kinds of layouts (this is not new in the NW extension - they were all available before):

- [circle](http://ccl.northwestern.edu/netlogo/docs/dictionary.html#layout-circle)

- [radial](http://ccl.northwestern.edu/netlogo/docs/dictionary.html#layout-radial)

- [spring](http://ccl.northwestern.edu/netlogo/docs/dictionary.html#layout-spring)

- [tutte](http://ccl.northwestern.edu/netlogo/docs/dictionary.html#layout-tutte)

Clicking the LAYOUT forever button will ensure that the chosen layout is applied continuously. This is especially useful in the case of the spring layout. For other layouts, you can also use the LAYOUT ONCE button.

### Generators

The first thing that you will see in the **Generators** section of the model is a slider labeled NB-NODES. It allows you to specify the number of nodes you want to have in your network. The first six generator buttons (PREFERENTIAL ATTACHMENT, RING, STAR, WHEEL, RANDOM, and SMALL WORLD) will take the value of that slider into account.

By default, using one of the generators will clear the current network first. You can disable this using the CLEAR-BEFORE-GENERATING? switch. Note that at any time, you can press the SETUP/CLEAR button to erase everything and start over.

Here is a description of each of generator.

PREFERENTIAL ATTACHMENT

Generates a new network using the [Barabási–Albert](https://en.wikipedia.org/wiki/Barab%C3%A1si%E2%80%93Albert_model) algorithm. This network will have the property of being "scale free": the distribution of degrees (i.e. the number of links for each turtle) should follow a power law.

Turtles are added, one by one, each forming one link to a previously added turtle, until _nb-nodes_ is reached. The more links a turtle already has, the greater the probability that new turtles form links with it when they are added.

RING

Generates a [ring network](https://en.wikipedia.org/wiki/Ring_network) of NB-NODES turtles, in which each turtle is connected to exactly two other turtles.

STAR

Generates a [star network](https://en.wikipedia.org/wiki/Star_graph) in which there is one central turtle and every other turtle is connected only to this central node. The number of turtles can be as low as one, but it won't look much like a star.

WHEEL

Generates a [wheel network](https://en.wikipedia.org/wiki/Wheel_graph), which is basically a [ring network](https://en.wikipedia.org/wiki/Ring_network) with an additional "central" turtle that is connected to every other turtle. The number of nodes must be at least four.

On the right side of the WHEEL button, you will see a chooser allowing you the select either "inward" or "outward". This will allow to specify if the "spokes" of the wheel point toward the central turtle (inward) or away from it (outward). This is, of course, meaningful only in the case of a directed network.

RANDOM

Generates a new random network of NB-NODES turtles in which each one has a  connection probability (between 0 and 1) of being connected to each other turtles (this is specified through the CONNECTION-PROB slider). The algorithm uses the [Erdős–Rényi model](https://en.wikipedia.org/wiki/Erd%C5%91s%E2%80%93R%C3%A9nyi_model).

SMALL WORLD

Generates a new [Watts-Strogatz small-world network](https://en.wikipedia.org/wiki/Watts_and_Strogatz_model). The algorithm begins by creating a ring of nodes, where each node is connected to NEIGHBORHOOD-SIZE nodes on either side. Then, each link is rewired with probability REWIRE-PROB.

LATTICE 2D

Generates a new 2D [lattice network](https://en.wikipedia.org/wiki/Lattice_graph) (basically, a grid) of NB-ROWS rows and NB-COLS columns. The grid will wrap around itself if the WRAP switch is set to "on".

KLEINBERG

Generates a new [small-world network](https://en.wikipedia.org/wiki/Small-world_network) using the [Kleinberg Model](https://en.wikipedia.org/wiki/Small_world_routing#The_Kleinberg_Model).

The generator uses the same sliders and switch as the lattice 2D generator, namely, NB-ROWS, NO-COLS and WRAP. The algorithm proceeds by generating a lattice of the given number of rows and columns (the lattice will wrap around itself if WRAP is "on"). The "small world effect" is created by adding additional links between the nodes in the lattice. The higher the CLUSTERING-EXPONENT, the more the algorithm will favor already close-by nodes when adding new links. A clustering exponent of `2.0` is typically used.

### Clusters and cliques

Now that you have generated one or more networks, there are things that you might want to know about them.

WEAK COMPONENT CLUSTERS

This button will assign a different color to all the "weakly" [connected components](https://en.wikipedia.org/wiki/Connected_component_%28graph_theory%29) in the current network. A weakly connected component is simply a group of nodes where there is a path from each node to every other node. A "strongly" connected component would be one where there is a _directed_ path from each node to every other. The extension does not support the identification of strongly connected components at the moment.

DETECT COMMUNITIES

Detects community structure present in the network. It does this by maximizing modularity using the [Louvain method](https://en.wikipedia.org/wiki/Louvain_Modularity).

HIGHLIGHT BICOMPONENTS

Clicking on this button will put you in a mode where you use your mouse to highlight the different [bicomponent clusters](https://en.wikipedia.org/wiki/Biconnected_component) in the current network. A bicomponent (also known as a maximal biconnected subgraph) is a part of a network that cannot be disconnected by removing only one node (i.e. you need to remove at least two to disconnect it).

Note that one turtle can be a member of more than one bicomponent at once. If it is the case, all the bicomponents that the target turtle is part of will be highlighted when you move your mouse pointer near it, but they will be of different color.

HIGHLIGHT MAXIMAL CLIQUES

The general usage for this is the same as for the **highlight bicomponents** mode. Note you should not try to use both highlight modes at the same time.

A [clique](https://en.wikipedia.org/wiki/Clique_%28graph_theory%29) is a subset of a network in which every node has a direct link to every other node. A maximal clique is a clique that is not, itself, contained in a bigger clique.

BIGGEST MAXIMAL CLIQUES

This simply highlights the biggest of all the maximal cliques in the networks. If there are multiple cliques that are equally big (as is often the case), it will highlight them with different colors.

### Centrality measures

Besides all the clusterers and the clique finder, you can also calculate some centrality measures on your networks. All the centrality measures will label the nodes will the result of the calculation and adjust their size and color to reflect that result.

BETWEENNESS

To calculate the [betweenness centrality](https://en.wikipedia.org/wiki/Betweenness_centrality) of a turtle, you take every other possible pairs of turtles and, for each pair, you calculate the proportion of shortest paths between members of the pair that passes through the current turtle. The betweenness centrality of a turtle is the sum of these.

EIGENVECTOR

The [Eigenvector centrality](https://en.wikipedia.org/wiki/Centrality#Eigenvector_centrality) of a node can be thought of as the proportion of its time that an agent forever "walking" at random on the network would spend on this node. In practice, turtles that are connected to a lot of other turtles that are themselves well-connected (and so) get a higher Eigenvector centrality score.

Eigenvector centrality is only defined for connected networks, and will report `false` for disconnected graphs.

CLOSENESS

The [closeness centrality](https://en.wikipedia.org/wiki/Centrality#Closeness_centrality) of a turtle is defined as the inverse of the sum of it's distances to all other turtles.

Note that this primitive reports the _intra-component_ closeness of a turtle, that is, it takes into account only the distances to the turtles that are part of the same [component](https://en.wikipedia.org/wiki/Connected_component_%28graph_theory%29) as the current turtle, since distance to turtles in other components is undefined. The closeness centrality of an isolated turtle is defined to be zero.

### Files

LOAD / SAVE MATRIX

Finally, you can save and load your networks. This can be done through the use of simple text files containing an [adjacency matrix](https://en.wikipedia.org/wiki/Adjacency_matrix).

The model currently always save the network to your NetLogo directory in a file called `matrix.txt` when you click the SAVE MATRIX button. When you click the LOAD MATRIX button, it reads from the same location and creates a new network from the file.

LOAD / SAVE GRAPHML

You can also save and load GraphML files. Please see the [extension's documentation](http://ccl.northwestern.edu/netlogo/docs/nw.html#save-graphml) for more detail on handling GraphML files. The demo simply saves the current network to (and can load from) the file `demo.graphml` in your default directory.

## THINGS TO NOTICE

- When you generate preferential attachment networks, notice the distribution of node degrees in the histogram. What does it look like? What happens if you generate a network with more nodes, or multiple preferential attachment networks?

- When you generate a small world network, what is the MEAN PATH LENGTH value that you can see on the monitor? How does it compare the a random network with the same number of nodes?

## THINGS TO TRY

- In general, different layouts work best for different kind of graphs. Can you try every combination of graph/layout? Which layout do you prefer for each kind of graph? Why?

- Try the spring layout with a lattice 2D network, with WRAP set to off. How does it look? Now try it with WRAP set to on. Can you explain the difference?

- Generate a small world network with a low clustering exponent (e.g., 0.1). What is the size of the biggest maximal clique? Now try it with a big exponent (e.g. 10.0). What is the size? Try it multiple times. Do you see a pattern? What if you crank up the number of rows and columns?

## EXTENDING THE MODEL

The current version of the demo does not take link weights into account. You can add a "weight" variable to each link breed. Can you add a button assigning random weights to the links? Can you make it so that link thickness reflects the "weight" of the link? Look at the extensions documentation for primitive that take weights into account. Can you integrate those in the demo?

## NETLOGO FEATURES

This model demonstrates the `nw` extension primitives.

But aside from that, notice the interesting use it makes of tasks for the centrality buttons. We have only one `centrality` procedure in the code that does all the hard work, and the other procedures call it with a `measure` reporter task as a parameter, that the `centrality` primitive then runs with `runresult`. This removes a lot of code duplication.

Another nice tidbit is how the `foreach` command is used in the `color-clusters` primitive. Notice how it loops over both the `clusters` list and the `colors` and then uses the `cluster` and `cluster-color` arguments to access members of each pair of cluster/color.

## RELATED MODELS

A couple of models already in the model library, namely the "Giant Component" model and the "Small World" model could be build much more easily by using the primitives in the network extension. Such versions of these two models are included in the "demo" folder of the extension, but trying to make the modifications yourself would be an excellent exercise.

<!-- 2012 -->
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
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
Polygon -7500403 true true 135 285 195 285 270 90 30 90 105 285
Polygon -7500403 true true 270 90 225 15 180 90
Polygon -7500403 true true 30 90 75 15 120 90
Circle -1 true false 183 138 24
Circle -1 true false 93 138 24

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
setup
nw:generate-watts-strogatz
  turtles undirected-edges 200 4 0.1
community-detection
set layout "circle"
layout-turtles
set layout "spring"
repeat 300 [ layout-turtles ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="D1_cont_scalefree" repetitions="20000" runMetricsEveryStep="false">
    <setup>preferential-attachment</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>degree-all</metric>
    <metric>mean-report</metric>
    <metric>sd-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;continuous-SAM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-influent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-continuous-init">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-continuous-add">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-continuous-init-add">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed-value-sd">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-continuous-init">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;cluscoeff&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D0_dir_timeline" repetitions="5" runMetricsEveryStep="true">
    <setup>preferential-attachment</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="50"/>
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-influent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="family-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-family">
      <value value="32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-link-extra">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D1_dir_scalefree" repetitions="20000" runMetricsEveryStep="false">
    <setup>preferential-attachment</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>degree-all</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-influent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D1_dir_random" repetitions="20000" runMetricsEveryStep="false">
    <setup>generate-random</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-influent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.06"/>
      <value value="0.24"/>
      <value value="0.25"/>
      <value value="0.26"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D2_dir_random" repetitions="10" runMetricsEveryStep="false">
    <setup>generate-random</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="50"/>
      <value value="150"/>
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-influent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.05"/>
      <value value="0.15"/>
      <value value="0.25"/>
      <value value="0.35"/>
      <value value="0.45"/>
      <value value="0.55"/>
      <value value="0.65"/>
      <value value="0.75"/>
      <value value="0.85"/>
      <value value="0.95"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D2_dir_smallworld" repetitions="5" runMetricsEveryStep="false">
    <setup>small-world-ring</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.7"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="50"/>
      <value value="150"/>
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-influent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.95"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D2_dir_scalefree" repetitions="100" runMetricsEveryStep="false">
    <setup>preferential-attachment</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>degree-all</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="50"/>
      <value value="150"/>
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-influent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.95"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3_dir_random" repetitions="100" runMetricsEveryStep="false">
    <setup>generate-random</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>centbet-biased</metric>
    <metric>centbet-unbiased</metric>
    <metric>cluscoeff-biased</metric>
    <metric>cluscoeff-unbiased</metric>
    <metric>centeigen-biased</metric>
    <metric>centeigen-unbiased</metric>
    <metric>centclos-biased</metric>
    <metric>centclos-unbiased</metric>
    <metric>assor-biased</metric>
    <metric>assor-unbiased</metric>
    <metric>deg-biased</metric>
    <metric>deg-unbiased</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;highest&quot;"/>
      <value value="&quot;lowest&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
      <value value="&quot;cluscoeff&quot;"/>
      <value value="&quot;centbet&quot;"/>
      <value value="&quot;centeigen&quot;"/>
      <value value="&quot;centclos&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3_dir_random_ran" repetitions="100" runMetricsEveryStep="false">
    <setup>generate-random</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>centbet-biased</metric>
    <metric>centbet-unbiased</metric>
    <metric>cluscoeff-biased</metric>
    <metric>cluscoeff-unbiased</metric>
    <metric>centeigen-biased</metric>
    <metric>centeigen-unbiased</metric>
    <metric>centclos-biased</metric>
    <metric>centclos-unbiased</metric>
    <metric>assor-biased</metric>
    <metric>assor-unbiased</metric>
    <metric>deg-biased</metric>
    <metric>deg-unbiased</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3_dir_random_none" repetitions="100" runMetricsEveryStep="false">
    <setup>generate-random</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>centbet-biased</metric>
    <metric>centbet-unbiased</metric>
    <metric>cluscoeff-biased</metric>
    <metric>cluscoeff-unbiased</metric>
    <metric>centeigen-biased</metric>
    <metric>centeigen-unbiased</metric>
    <metric>centclos-biased</metric>
    <metric>centclos-unbiased</metric>
    <metric>assor-biased</metric>
    <metric>assor-unbiased</metric>
    <metric>deg-biased</metric>
    <metric>deg-unbiased</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D1_dir_smallworld" repetitions="100" runMetricsEveryStep="false">
    <setup>small-world-ring</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-influent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D1_cont_random" repetitions="20000" runMetricsEveryStep="false">
    <setup>generate-random</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>degree-all</metric>
    <metric>mean-report</metric>
    <metric>sd-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;continuous-SAM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-influent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-continuous-init">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-continuous-add">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-continuous-init-add">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed-value-sd">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-continuous-init">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;cluscoeff&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.06"/>
      <value value="0.24"/>
      <value value="0.25"/>
      <value value="0.26"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D1_cont_smallworld" repetitions="100" runMetricsEveryStep="false">
    <setup>small-world-ring</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>degree-all</metric>
    <metric>mean-report</metric>
    <metric>sd-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;continuous-SAM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-influent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-continuous-init">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3_dir_scalefree" repetitions="100" runMetricsEveryStep="false">
    <setup>preferential-attachment</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>centbet-biased</metric>
    <metric>centbet-unbiased</metric>
    <metric>cluscoeff-biased</metric>
    <metric>cluscoeff-unbiased</metric>
    <metric>centeigen-biased</metric>
    <metric>centeigen-unbiased</metric>
    <metric>centclos-biased</metric>
    <metric>centclos-unbiased</metric>
    <metric>assor-biased</metric>
    <metric>assor-unbiased</metric>
    <metric>deg-biased</metric>
    <metric>deg-unbiased</metric>
    <metric>degree-all</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;highest&quot;"/>
      <value value="&quot;lowest&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
      <value value="&quot;cluscoeff&quot;"/>
      <value value="&quot;centbet&quot;"/>
      <value value="&quot;centeigen&quot;"/>
      <value value="&quot;centclos&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3_dir_scalefree_none" repetitions="100" runMetricsEveryStep="false">
    <setup>preferential-attachment</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>centbet-biased</metric>
    <metric>centbet-unbiased</metric>
    <metric>cluscoeff-biased</metric>
    <metric>cluscoeff-unbiased</metric>
    <metric>centeigen-biased</metric>
    <metric>centeigen-unbiased</metric>
    <metric>centclos-biased</metric>
    <metric>centclos-unbiased</metric>
    <metric>assor-biased</metric>
    <metric>assor-unbiased</metric>
    <metric>deg-biased</metric>
    <metric>deg-unbiased</metric>
    <metric>degree-all</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3_dir_scalefree_ran" repetitions="100" runMetricsEveryStep="false">
    <setup>preferential-attachment</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>centbet-biased</metric>
    <metric>centbet-unbiased</metric>
    <metric>cluscoeff-biased</metric>
    <metric>cluscoeff-unbiased</metric>
    <metric>centeigen-biased</metric>
    <metric>centeigen-unbiased</metric>
    <metric>centclos-biased</metric>
    <metric>centclos-unbiased</metric>
    <metric>assor-biased</metric>
    <metric>assor-unbiased</metric>
    <metric>deg-biased</metric>
    <metric>deg-unbiased</metric>
    <metric>degree-all</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3_dir_smallworld" repetitions="100" runMetricsEveryStep="false">
    <setup>small-world-ring</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>centbet-biased</metric>
    <metric>centbet-unbiased</metric>
    <metric>cluscoeff-biased</metric>
    <metric>cluscoeff-unbiased</metric>
    <metric>centeigen-biased</metric>
    <metric>centeigen-unbiased</metric>
    <metric>centclos-biased</metric>
    <metric>centclos-unbiased</metric>
    <metric>assor-biased</metric>
    <metric>assor-unbiased</metric>
    <metric>deg-biased</metric>
    <metric>deg-unbiased</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;highest&quot;"/>
      <value value="&quot;lowest&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
      <value value="&quot;cluscoeff&quot;"/>
      <value value="&quot;centbet&quot;"/>
      <value value="&quot;centeigen&quot;"/>
      <value value="&quot;centclos&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3_dir_smallworld_ran" repetitions="100" runMetricsEveryStep="false">
    <setup>small-world-ring</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>centbet-biased</metric>
    <metric>centbet-unbiased</metric>
    <metric>cluscoeff-biased</metric>
    <metric>cluscoeff-unbiased</metric>
    <metric>centeigen-biased</metric>
    <metric>centeigen-unbiased</metric>
    <metric>centclos-biased</metric>
    <metric>centclos-unbiased</metric>
    <metric>assor-biased</metric>
    <metric>assor-unbiased</metric>
    <metric>deg-biased</metric>
    <metric>deg-unbiased</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3_dir_smallworld_none" repetitions="100" runMetricsEveryStep="false">
    <setup>small-world-ring</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>centbet-biased</metric>
    <metric>centbet-unbiased</metric>
    <metric>cluscoeff-biased</metric>
    <metric>cluscoeff-unbiased</metric>
    <metric>centeigen-biased</metric>
    <metric>centeigen-unbiased</metric>
    <metric>centclos-biased</metric>
    <metric>centclos-unbiased</metric>
    <metric>assor-biased</metric>
    <metric>assor-unbiased</metric>
    <metric>deg-biased</metric>
    <metric>deg-unbiased</metric>
    <metric>vector-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D0_cont_timeline" repetitions="5" runMetricsEveryStep="true">
    <setup>preferential-attachment</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>clust-coeff</metric>
    <metric>mean-pathlength</metric>
    <metric>init-average-mode</metric>
    <metric>assort-coeff</metric>
    <metric>avg-degree</metric>
    <metric>mean-report</metric>
    <metric>sd-report</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;continuous-SAM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="50"/>
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-influent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-continuous-init">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-continuous-add">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-continuous-init-add">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed-value-sd">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-continuous-init">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;cluscoeff&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3sup_dir_scalefree_none" repetitions="50" runMetricsEveryStep="false">
    <setup>preferential-attachment</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>degree-all</metric>
    <metric>bet-all</metric>
    <metric>clo-all</metric>
    <metric>eig-all</metric>
    <metric>clus-all</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3sup_dir_random_none" repetitions="50" runMetricsEveryStep="false">
    <setup>generate-random</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>degree-all</metric>
    <metric>bet-all</metric>
    <metric>clo-all</metric>
    <metric>eig-all</metric>
    <metric>clus-all</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D3sup_dir_smallworld_none" repetitions="50" runMetricsEveryStep="false">
    <setup>small-world-ring</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>degree-all</metric>
    <metric>bet-all</metric>
    <metric>clo-all</metric>
    <metric>eig-all</metric>
    <metric>clus-all</metric>
    <enumeratedValueSet variable="synchronous">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-additional-link">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-assort">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="binary-states">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="is-state-random">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-state">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choose-N-influent">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;dirichlet&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-utt-heard-start">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-numb-utt">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listen-twice">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vec-dirichlet">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-bias">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bias-additionnal">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-dirichlet-add">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-init-dirichlet-add">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-ind-biased">
      <value value="&quot;degree&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
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
1
@#$#@#$#@
