#lang racket/gui











(require graph)


(define ns (variable-reference->namespace (#%variable-reference)))

(define (string->procedure s)
  (define sym (string->symbol s))
  (eval sym ns))

  (define dcheck (λ (s d) (cond
                            ((not (andmap (λ (x)  (has-vertex? tubeg x)) (list s d))) (send fr set-value "There is no route found"))
                            ((not (empty? (filter-map (λ (y) (andmap (λ (x)  (has-vertex? (string->procedure y) x)) (list s d))) tube))) (send fr set-value (string-join (append (cond
                                                                                                                                                                                   ((equal? (length (filter (λ (y) (andmap (λ (x)  (has-vertex? (string->procedure y) x)) (list s d))) tube)) 1) (filter (λ (y) (andmap (λ (x)  (has-vertex? (string->procedure y) x)) (list s d))) tube))
                                                                                                                                                                                   (#t (list (string-append (string-join (filter (λ (y) (andmap (λ (x)  (has-vertex? (string->procedure y) x)) (list s d))) tube) " or ") " lines" ))))
                                                                                                                                                                                    (list (string-join (fewest-vertices-path tubeg s d) "->"))) " line:   "))) 
                            (not #f (fewest-vertices-path tubeg s d) (send fr set-value (string-join (fewest-vertices-path tubeg s d) "->")))
                            (#t "No route has been found"))))


(define tube (list "Northen" "Bakerloo" "Victoria"))

(define tubeg (unweighted-graph/undirected '(                 
("Elephant and Castle" "Lambeth North")
("Lambeth North" "Waterloo")
("Waterloo" "Embankment")
("Embankment" "Charing Cross")
("Charing Cross" "Piccadilly Circus")
("Piccadilly Circus" "Oxford Circus")
("Oxford Circus" "Regent's Park")
("Regent's Park" "Baker Street")
("Baker Street" "Marylebone")
("Marylebone" "Edgware Road")
("Edgware Road" "Paddington")
("Paddington" "Warwick Avenue")
("Warwick Avenue" "Maida Vale")
("Maida Vale" "Kilburn Park")
("Kilburn Park" "Queens Park")
("Queens Park" "Kensal Green")
("Kensal Green" "Wilesden Juction")
("Wilesden Juction" "Harlesden")
("Harlesden" "Stonebridge Park")
("Stonebridge Park" "Wembley Central")
("Wembley Central" "North Wembley")
("North Wembley" "South Kenton")
("South Kenton" "Kenton")
("Kenton" "Harrow and Wealdstone")
("Walthamstow Central" "Blackhorse Road")
("Blackhorse Road" "Tottenham Hale")
("Tottenham Hale" "Seven Sisters")
("Seven Sisters" "Finsbury Park")
("Finsbury Park" "Highbury & Islington")
("Highbury & Islington" "King's Cross St Pancras")
("King's Cross St Pancras" "Euston")
("Euston" "Warren Street")
("Warren Street" "Oxford Circus")
("Oxford Circus" "Green Park")
("Green Park" "Victoria")
("Victoria" "Pimlico")
("Pimlico" "Vauxhall")
("Vauxhall" "Stockwell")
("Stockwell" "Brixton")
("Morden" "South Wimbledon")
("South Wimbledon" "Colliers Wood")
("Colliers Wood" "Tooting Broadway")
("Tooting Broadway" "Tooting Bec")
("Tooting Bec" "Balham")
("Balham" "Clapham South")
("Clapham South" "Clapham Common")
("Clapham Common" "Clapham North")
("Clapham North" "Stockwell")
("Stockwell" "Oval")
("Oval" "Kennington")
("Kennington" "Nine Elms")
("Nine Elms" "Battersea Power Station")
("Kennington" "Waterloo")
("Waterloo" "Embankment")
("Embankment" "Charing Cross")
("Charing Cross" "Leicester Square")
("Leicester Square" "Tottenham Court Road")
("Tottenham Court Road" "Goodge Street")
("Goodge Street" "Warren Street")
("Warren Street" "Euston")
("Euston" "Mornington Crescent")
("Mornington Crescent" "Camden Town")
("Camden Town" "Kentish Town") 
("Kentish Town" "Tuffnell Park")
("Tuffnell Park" "Archway")
("Archway" "Highgate")
("Highgate" "East Finchley")
("East Finchley" "Finchley Central")
("Finchley Central" "West Finchley")
("West Finchley" "Woodside Park")
("Woodside Park" "Totteridge & Whetstone")
("Totteridge & Whetstone" "High Barnet")
("Kennington" "Elephant & Castle")
("Elephant & Castle" "Borough")
("Borough" "London Bridge")
("London Bridge" "Bank")
("Bank" "Moorgate")
("Moorgate" "Old Street")
("Old Street" "Angel")
("Angel" "King's Cross St Pancras")
("King's Cross St Pancras" "Euston")
("Euston" "Camden Town")
("Camden Town" "Chalk Farm")
("Chalk Farm" "Belsize Park")
("Belsize Park" "Hampstead")
("Hampstead" "Golders Green")
("Golders Green" "Brent Cross")
("Brent Cross" "Hendon Central")
("Hendon Central" "Colindale")
("Colindale" "Burnt Oak")
("Burnt Oak" "Edgware")
                                             )))


(define Bakerloo (unweighted-graph/undirected '(
("Elephant and Castle" "Lambeth North")
("Lambeth North" "Waterloo")
("Waterloo" "Embankment")
("Embankment" "Charing Cross")
("Charing Cross" "Piccadilly Circus")
("Piccadilly Circus" "Oxford Circus")
("Oxford Circus" "Regent's Park")
("Regent's Park" "Baker Street")
("Baker Street" "Marylebone")
("Marylebone" "Edgware Road")
("Edgware Road" "Paddington")
("Paddington" "Warwick Avenue")
("Warwick Avenue" "Maida Vale")
("Maida Vale" "Kilburn Park")
("Kilburn Park" "Queens Park")
("Queens Park" "Kensal Green")
("Kensal Green" "Wilesden Juction")
("Wilesden Juction" "Harlesden")
("Harlesden" "Stonebridge Park")
("Stonebridge Park" "Wembley Central")
("Wembley Central" "North Wembley")
("North Wembley" "South Kenton")
("South Kenton" "Kenton")
("Kenton" "Harrow and Wealdstone")

)   ))


(define Victoria (unweighted-graph/undirected '(("Walthamstow Central" "Blackhorse Road")
("Blackhorse Road" "Tottenham Hale")
("Tottenham Hale" "Seven Sisters")
("Seven Sisters" "Finsbury Park")
("Finsbury Park" "Highbury & Islington")
("Highbury & Islington" "King's Cross St Pancras")
("King's Cross St Pancras" "Euston")
("Euston" "Warren Street")
("Warren Street" "Oxford Circus")
("Oxford Circus" "Green Park")
("Green Park" "Victoria")
("Victoria" "Pimlico")
("Pimlico" "Vauxhall")
("Vauxhall" "Stockwell")
("Stockwell" "Brixton"))))

(define Northen (unweighted-graph/undirected '(
("Morden" "South Wimbledon")
("South Wimbledon" "Colliers Wood")
("Colliers Wood" "Tooting Broadway")
("Tooting Broadway" "Tooting Bec")
("Tooting Bec" "Balham")
("Balham" "Clapham South")
("Clapham South" "Clapham Common")
("Clapham Common" "Clapham North")
("Clapham North" "Stockwell")
("Stockwell" "Oval")
("Oval" "Kennington")
("Kennington" "Nine Elms")
("Nine Elms" "Battersea Power Station")
("Kennington" "Waterloo")
("Waterloo" "Embankment")
("Embankment" "Charing Cross")
("Charing Cross" "Leicester Square")
("Leicester Square" "Tottenham Court Road")
("Tottenham Court Road" "Goodge Street")
("Goodge Street" "Warren Street")
("Warren Street" "Euston")
("Euston" "Mornington Crescent")
("Mornington Crescent" "Camden Town")
("Camden Town" "Kentish Town") 
("Kentish Town" "Tuffnell Park")
("Tuffnell Park" "Archway")
("Archway" "Highgate")
("Highgate" "East Finchley")
("East Finchley" "Finchley Central")
("Finchley Central" "West Finchley")
("West Finchley" "Woodside Park")
("Woodside Park" "Totteridge & Whetstone")
("Totteridge & Whetstone" "High Barnet")
("Kennington" "Elephant & Castle")
("Elephant & Castle" "Borough")
("Borough" "London Bridge")
("London Bridge" "Bank")
("Bank" "Moorgate")
("Moorgate" "Old Street")
("Old Street" "Angel")
("Angel" "King's Cross St Pancras")
("King's Cross St Pancras" "Euston")
("Euston" "Camden Town")
("Camden Town" "Chalk Farm")
("Chalk Farm" "Belsize Park")
("Belsize Park" "Hampstead")
("Hampstead" "Golders Green")
("Golders Green" "Brent Cross")
("Brent Cross" "Hendon Central")
("Hendon Central" "Colindale")
("Colindale" "Burnt Oak")
("Burnt Oak" "Edgware"))))


                                                

;(for ([i (map string->procedure tube)]) ( graph-union! tubeg  i ))






  (define mapper (new frame%
                       [label "MyMapper"]
                       [width 1920]
                       [height 1080]))
(define start (new text-field%
                    [parent mapper]
                    [label "From:"]))


(define switch (new button%
                          [parent mapper]
                          [label "Switch"]
                          [callback (λ (o e)
                                      (let ([x (list (send start get-value) (send dest get-value))])
                                      (send dest set-value (first x))
                                        (send start set-value (second x))
                                        (dcheck (send start get-value) (send dest get-value))
                                        )
                                      )]))

(define dest (new text-field%
                   [label "To:"]
                   [parent mapper]))

(define find (new button%
                  [parent mapper]
                  [label "Find Route"]
                  [callback (λ (o e) (dcheck (send start get-value) (send dest get-value)))]))

(define fr (new text-field%
                     [label "Route:"]
                     [parent mapper]
                     
                     [enabled #f]))


(send mapper show #t)