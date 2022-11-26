;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "2018-fall-reader.rkt" "csc104")((modname Project-2) (compthink-settings #hash((prefix-types? . #f))))
; CSC104 2018 Fall : Project 2
; ============================
; Make some database functions, and a spreadsheet application that relies on them.

; Read the comments in order.
; Write the check-expects, and define (fix) the functions, as indicated by ‘★’s.
; You MUST uncomment the existing check-expects for each function you define:
;  if not, that function will not be marked.
; You MUST click in the Definitions area and click the Format button before
;  submitting this file.
; You MUST run the file before submitting it, and it must pass the check-expects
;  that are not still commented-out.

; DO NOT paste any literal images into this file.

; MAKE SURE the file "test-images.rkt" is in the same folder/directory as this file
;  while you work on the project: that file imports images used by some of the
;  test/documentation check-expects. The images are in a separate file so that this
;  file's text can be displayed on MarkUs.
(require "test-images.rkt")

; Completing the following two sections is worth 60% of the mark:
;   Viewing A Number or Text
;   Columns
; Completing the following section is worth 5% of the mark:
;   CSV Files
; Completing the following section is worth 5% of the mark:
;   Handling Numbers vs Texts
; The following sections can be completed in any order, and are worth the amount shown:
;   Aesthetics : 5%
;   Search a Column : 5%
;   Sort by a Column : 10%
;   Histogram : 10%

; Small Sample Table
; ==================
; A table is represented as a list of equal-length lists, with each of the inner lists
;  containing only numbers and text. The inner lists represent the rows. For an index i
;  less than the length of the inner lists, the ith elements from the lists form a column.
;  For a given column, all the elements are texts, or all the elements are numbers.

; Here's a small (and opinionated) table to play with:
(define DW-table (list (list "clara" 11 2012 "great")
                       (list "amy" 11 2010 "great")
                       (list "rory" 11 2010 "great")
                       (list "rose" 10 2006 "ok")
                       (list "clara" 12 2014 "great")
                       (list "donna" 10 2008 "ok")
                       (list "martha" 10 2009 "ok")
                       (list "bill" 12 2017 "ok")
                       (list "rose" 9 2005 "good")))

; In particular, you can enter the following into the Interactions each time you complete
;  a function and pass its tests, to see your progress:
#;(spreadsheet DW-table)
; You can also try the TTC subway delay data, by selecting its csv file:
#;(spreadsheet #false)

; Launch the Application
; ======================
; spreadsheet : table-or-false → image, with side-effect of interaction
; ---------------------------------------------------------------------
; Launches a spreadsheet for a table, or if called with #false open a file chooser for the user
;  to select a CSV data file on their computer.
; Produce an image of the spreadsheet when the big-bang window is closed.

; This function definition is correct, and delegates most of the work to functions defined later.
; Understanding the details is not necessary for completion of the project.

(define (spreadsheet maybe-table)
  (local [(define table (if [(same? maybe-table #false) (get-csv-data convert-numerics)]
                            [else maybe-table]))
          (define (handle-mouse state x y event-type)
            (if [(same? event-type "button-down")
                 (handle-select state (interval x (table-intervals table)) table)]
                [else state]))
          (define (draw state) (spreadsheet-image table state))]
    (draw (big-bang (list 0 0 "") [to-draw draw] [on-mouse handle-mouse] [on-key handle-typed]))))

; Model
; =====
; The state of the application is a list of three elements:
;   • the index of the column of data for the histogram view
;   • the index of the column to sort, and to search
;   • the search text typed so far

; The function definitions below are correct.
; They are fairly simple, but you may skip them: none of the functions you define will call
;  these functions directly.

; Representation-independence: “abstracts” the access and update of each part of the state,
;  providing more meaningful names than referring to the arbitrary order in which the parts
;  are stored in the list.

(define index:histogram   first)
(define index:sort-search second)
(define typed third)

(define (with-index:histogram index state)
  (list index
        (index:sort-search state)
        (typed state)))

(define (with-index:sort-search index state)
  (list (index:histogram state)
        index
        (typed state)))

(define (with-typed a-text state)
  (list (index:histogram state)
        (index:sort-search state)
        a-text))

; Controllers
; ===========
; These functions handle relevant keyboard and mouse events, updating the state.
; In particular, the big-bang expression in ‘spreadsheet’ pre-processes the mouse events to
;  determine whether they're relevant, then calls handle-select with a pre-processed argument
;  that allows handle-select to be written and tested without knowledge of mouse event handling.

; The definitions are correct.
; Understanding their details is not necessary for completion of the project.

(define (handle-typed state key)
  (local [(define current (typed state))]
    (with-typed (if [(same? key "\b") (sub-text current 0 (maximum 0 (dec (text-length current))))]
                    [else (text-join current key)])
      state)))

(define (handle-select state selected table)
  (local [(define valid-selected (maximum 0 (minimum selected (dec (length (first table))))))]
    (if [(same? valid-selected (index:histogram state)) (with-index:sort-search valid-selected state)]
        [else (with-index:histogram valid-selected state)])))

; View
; ====
; These function definitions are correct.
; Understanding their details is not necessary for completion of the project.

; spreadsheet-image : table state → image
; ---------------------------------------
; Produces an image of a table, ordered and sifted based on a selected highlighted column
;  and current search text, with an accompanying histogram based on a selected highlighted
;  column of the sifted table.
(define (spreadsheet-image table state)
  (local [(define background (invisible-width (* 2 (width (columns-image table)))))
          (define processed-table (process-table table state))
          (define table-image (columns-image processed-table))]
    (top-left-overlay
     (top-left-beside
      (top-left-overlay (highlights (table-intervals processed-table)
                                    (index:histogram state)
                                    (index:sort-search state)
                                    (height table-image))
                        table-image)
      (top-left-above (frame (imagine (text-join "search: " (typed state))))
                      (frame (histogram (column processed-table (index:histogram state))))))
     background)))

(define (process-table table state)
  (local [(define (match? entry) (text-match? (typed state) (textify entry)))
          (define sifted (sift:column match? table (index:sort-search state)))]
    (table-sort (if [(empty? sifted) table]
                    [else sifted])
                (index:sort-search state))))

(define (table-intervals table)
  (map width (map column-image (columns table))))

(define color:search-and-sort (list 100 0 0 25))
(define color:histogram (list 0 100 0 25))

(define (highlights widths index-1 index-2 the-height)
  (local [(define (highlight-interval index color)
            (beside (invisible-width (absolute widths index))
                    (rectangle (element widths index) the-height "solid" color)))]
    (top-left-overlay (highlight-interval index-1 color:histogram)
                      (highlight-interval index-2 color:search-and-sort))))

(define (absolute widths i)
  (apply + (sub-list widths 0 i)))

; Image Alignment
; ===============
; Much of the image placement for the spreadsheet aligns images by their top-left corners,
;  rather than using the default centering. Using the following functions makes it clearer
;  that this alignment is common, in particular because align-beside/above only mention one
;  of the alignments explicitly.
; These definitions are correct.
(define (top-left-beside  image-1 image-2) (align-beside         "top" image-1 image-2))
(define (top-left-overlay image-1 image-2) (align-overlay "left" "top" image-1 image-2))
(define (top-left-above   image-1 image-2) (align-above   "left"       image-1 image-2))

; Intervals
; =========
; Columns are positioned by “pasting” them together, rather than being placed individually at
;  particular co-ordinates. For a number and a list of widths, the function ‘interval’ finds
;  the index of the width containing the number. This is used to determine which column the
;  mouse is over.

; interval : list-of-positive-numbers → natural-number
; ----------------------------------------------------
(define (interval n widths)
  (if [(or (empty? widths) (< n (first widths))) 0]
      [else (inc (interval (- n (first widths)) (rest widths)))]))

; That function definition is correct.
; You should be comfortable stepping it at this point in the course.

(check-expect (interval 20 (list)) 0)
(check-expect (interval 20 (list 21 10)) 0)
(check-expect (interval 20 (list 20 10)) 1)
(check-expect (interval 20 (list 15 10)) 1)
(check-expect (interval 20 (list 10 10)) 2)
(check-expect (interval 20 (list 10 5 10)) 2)
(check-expect (interval 20 (list 5 5 5)) 3)

; Viewing A Number or Text
; ========================

; The text size and color for viewing the data:
(define text-size 12)
(define text-color "darkblue")

; textify : text-or-number → text
; -------------------------------
; For a number, produce its textual representation, and for a text just produce the text.
(check-expect (textify 104) "104")
(check-expect (textify "howdy") "howdy")

; Note: our language has a function
;   number->text : number → text
;  that produces a textual representation of a number.

; ★ Define textify:
(define (textify a-text-or-number)
  (if [(number? a-text-or-number) (number->text a-text-or-number)]
      [else a-text-or-number]))

; invisible-width : non-negative-number → image
; ---------------------------------------------
; For a given width, produce an invisible image with that width, and zero height.
(check-expect (width  (invisible-width 123)) 123)
(check-expect (height (invisible-width 123)) 0)

; ★ Define invisible-width:
(define (invisible-width the-width)
  (rectangle the-width 0 "solid" "white"))

; imagine : text-or-number → image
; --------------------------------
; For a text or number, produce an image of it with size ‘text-size’ and color ‘text-color’,
;  padded on the left and right by ‘text-size’ pixels each.
(check-expect (imagine "howdy") imagine:test-image-1)
(check-expect (imagine 104) imagine:test-image-2)



; ★ Define imagine:
(define (imagine a-text-or-number)
  (beside (invisible-width text-size) (text->image (textify a-text-or-number) text-size text-color) (invisible-width text-size)))

; Columns
; =======

; column : table natural-number → list
; ------------------------------------
; For a table and index i, a list of the ith element of each list in the table.
(check-expect (column (list (list "clara" 11) (list "rose" 9) (list "amy" 11) (list "donna" 10)) 1)
              (list 11 9 11 10))

; ★ Write at least one more check-expect for the function, so that the set of check-expects
;    varies each argument at least once:

(check-expect (column (list (list "clara" 11) (list "rose" 9) (list "amy" 11) (list "donna" 10)) 0)
              (list  "clara"  "rose"   "amy"  "donna"))

(check-expect (column (list (list "1" 11) (list "2" 9) (list "3" 11) (list "4" 10)) 0)
              (list  "1"  "2"   "3"  "4"))

; ★ Write a Design check-expect for the function.
; Hint: decide whether one (or more) of the general list transformations is happening: mapping,
;  applying, sifting, or sorting. If so, decide the contract and behaviour of the function
;  you would like to give to map, apply, sift, or sort. If that function doesn't exist in our
;  language, define it. If that function depends on the argument(s) given to ‘column’, make the
;  function local so its body can use those argument(s) as well as its own.

(check-expect (column (list (list "clara" 11) (list "rose" 9) (list "amy" 11) (list "donna" 10)) 0)
              (local [(define (get l)
                        (element l 0))]
                (map get (list (list "clara" 11) (list "rose" 9) (list "amy" 11) (list "donna" 10)))))

; ★ Define ‘column’:
(define (column table i)
  (local [(define (get l)
            (element l i))]
    (map get table)))
      
  

; columns : table → list-of-lists
; -------------------------------
; For a table, thinking of its elements as rows, produce a list of its columns.
(check-expect (columns (list (list "clara" 11)
                             (list "rose" 9)
                             (list "amy" 11)
                             (list "donna" 10)))
              (list (list "clara" "rose" "amy" "donna")
                    (list 11 9 11 10)))
(check-expect (columns (list (list "clara" 11)
                             (list "rose" 9)
                             (list "amy" 11)
                             (list "donna" 10)))
              (list (column (list (list "clara" 11)
                                  (list "rose" 9)
                                  (list "amy" 11)
                                  (list "donna" 10))
                            0)
                    (column (list (list "clara" 11)
                                  (list "rose" 9)
                                  (list "amy" 11)
                                  (list "donna" 10))
                            1)))


; ★ Write at least one more check-expect for the function, so that the set of check-expects
;    varies each argument at least once:
(check-expect (columns (list (list "x" 0)))
              (list (list "x") (list 0)))

; ★ Write a Design check-expect for the function. Hint: see the hint for ‘column’.
(check-expect (columns (list (list "clara" 11)
                             (list "rose" 9)
                             (list "amy" 11)
                             (list "donna" 10)))
              (adjoin (column (list (list "clara" 11)
                                    (list "rose" 9)
                                    (list "amy" 11)
                                    (list "donna" 10)) 0) (columns (map rest (list (list "clara" 11)
                                                                                   (list "rose" 9)
                                                                                   (list "amy" 11)
                                                                                   (list "donna" 10))))))
              

; ★ Define ‘columns’:
(define (columns table)
  (if ((empty? (first table)) (list))
      (else (adjoin (column table 0) (columns (map rest table))))))

; Column Images
; =============
; These functions use some of the previous functions you wrote.
; They are already correct, but you should make sure you can understand their implementations
;  at this point in the course.

; The check-expects show the results that would be produced if you also implement Aesthetics.

; elements-image : list → image
; -----------------------------
; For a list of numbers or text, produce an image of its elements stacked vertically,
;  with alignment corresponding to whethewr the elements are numeric or text.
(define (elements-image a-list)
  (apply align-above (adjoin (alignment a-list) (map imagine a-list))))
#;(check-expect (elements-image (list "once" "upon" "a" "time")) elements-image:test-image-1)
#;(check-expect (elements-image (list 10 4 104)) elements-image:test-image-2)

; column-image : list → image
; ---------------------------
; For a list of numbers or text, produce a framed image of its elements.
(define (column-image entries) (frame (elements-image entries)))
#;(check-expect (column-image (list "old pond" "frog leaps in" "water's sound"))
                column-image:test-image)

; columns-image : table → image
; -----------------------------
; For a table, produce a framed image of its columns.
(define (columns-image table)
  (apply beside (map column-image (columns table))))

#;(check-expect (columns-image (list (list "clara" 11)
                                     (list "rose" 9)
                                     (list "amy" 11)
                                     (list "donna" 10)))
                columns-image:test-image)

; CSV Files
; =========
; A CSV file contains a table of data, in a particular syntax (format): “Comma-Separated-Values”.
; The default datatype is often text, and an application or user might have to decide when to
;  treat some of the text as representing other types of data. We'll use a function from a
;  Racket library that reads csv file and produces a list of lists of texts from it. Then we'll
;  convert any text that could be a number into a number.

; convert-numeric : text → text-or-number
; ---------------------------------------
; For a text, produce the corresponding number if the text is the textual representation of a number,
;  otherwise just produce the text.

#;(check-expect (convert-numeric "10.4") 10.4)
#;(check-expect (convert-numeric "ten point four") "ten point four")

; Our language has a function
;   text->number : text → number-or-false
;  that produces #false for a text not representing a number, otherwise it produces the number:
#;(check-expect (text->number "10.4") 10.4)
#;(check-expect (text->number "ten point four") #false)

; ★ Change the following check-expects, by changing the first expression in each one
;     to an expression that distinguishes the first case above from the second case:
#;(check-expect (text->number "10.4") #true)
#;(check-expect (text->number "ten point four") #false)
; Do not delete any part of each first expression that is already there, and the code
;  you add should be identical in both cases.


; ★ Define convert-numeric:
(define (convert-numeric a-text)
  a-text)

; convert-numerics : list-of-texts → list-of-texts-or-numbers
; ----------------------------------------------------------
; For a list of texts, produce the list with all texts that represent numbers converted to numbers.
#;(check-expect (convert-numerics (list "10.4" "ten point four")) (list 10.4 "ten point four"))

; ★ Define convert-numerics:
(define (convert-numerics texts)
  texts)

; get-csv-data : unary-function → table
; -------------------------------------
; Given a unary function that accepts a text as argument, ask the user to choose a CSV data file
;  from their computer, and process the elements using that function.

; Get some functions from the racket libraries:
(require (only-in 2htdp/batch-io read-csv-file/rows))
(require (only-in racket/gui get-file path->string))

; Those new functions are only used in get-csv-data, which is already correct, and you may skip
;  reading its definition.

(define (get-csv-data converter)
  (read-csv-file/rows (path->string (get-file "Select a CSV Data File"))
                      converter))

; Handling Numbers vs Texts
; =========================
; if-numeric-list : list any any → any
; ------------------------------------
; For a list, if it has a first element and that element is a number, produce the second argument,
;  otherwise produce the third argument.
#;(check-expect (if-numeric-list (list 123 45 678) "numbers" "something else") "numbers")
#;(check-expect (if-numeric-list (list "once" "upon" "a" "time") 123 456) 456)

; ★ Write two more check-expects, one that illustrates and tests the most extreme case,
;    and one that illustrates and tests that only the first element matters:


; ★ Define ‘if-numeric-list’:
(define (if-numeric-list entries choice-for-numeric other-choice)
  other-choice)

; Aesthetics
; ==========
; These functions fine tune the look of the spreadsheet.
; They do not affect the functionality (search, sort, and histogram).

; alignment : list → text
; ------------------------
; For a list, produce "right" if it's considered numeric according to the earlier function
;  ‘if-numeric-list’, otherwise produce "left".
#;(check-expect (alignment (list 123 45 678)) "right")
#;(check-expect (alignment (list "once" "upon" "a" "time")) "left")

; ★ Define ‘alignment’:
(define (alignment a-list)
  "left")

; frame : image → image
; ---------------------
; For an image, produce the image of it framed with a border enlarged that is larger than the
;  image by a factor of ‘frame-proportion’.
(define frame-proportion 1.1)
#;(check-expect (frame (star 25 "solid" "green")) frame:test-image)

; ★ Define ‘frame’:
(define (frame image)
  image)

; Search a Column
; ===============
; These functions add the search feature.
; They do not affect the sort nor histogram features.

; sift:column : unary-predicate table natural-number → table
; ----------------------------------------------------------
; Sift a table based on the entries in a particular column, producing only the rows
;  where that column entry satisfies a predicate.
#;(check-expect (sift:column even?
                             (list (list 9 "eccelston") (list 10 "Tennant"))
                             0)
                (list (list 10 "Tennant")))
#;(check-expect (sift:column positive?
                             (list (list 9 "eccelston") (list 10 "Tennant"))
                             0)
                (list (list 9 "eccelston") (list 10 "Tennant")))
#;(check-expect (sift:column lower-case?
                             (list (list 9 "eccelston") (list 10 "Tennant"))
                             1)
                (list (list 9 "eccelston")))

; ★ Define ‘sift:column’:
(define (sift:column p? table index)
  table)

; text-match? : text text → boolean
; ---------------------------------
; For two texts, is the first one a sub-text of the second, ignoring upper versus lower case?
#;(check-expect (text-match? "it" "kitten") #true)
#;(check-expect (text-match? "It" "kitten") #true)
#;(check-expect (text-match? "bit" "kitten") #false)
#;(check-expect (text-match? "Troll" "UTRoll") #true)

; Note these two functions that are in our language:
;   sub-text? : text text → text
;   lower-case : text → text

; ★ Write Partial or Full Design versions of each of the previous four check-expects:


; ★ Define ‘text-match?’:
(define (text-match? part text)
  (same? part text))

; Sort by a Column
; ================
; These functions add the sort feature.
; They do not affect the search nor histogram features.

; ordering : list → binary-predicate
; ----------------------------------
; For a list, produce the function ‘<’ if the list is considered numeric, otherwise produce
;  the function ‘alphabet-order?’.

; For reasons we'll discuss at the end of the course, check-expect refuses to compare functions,
;  so these check-expects test indirectly:
#;(check-expect (local [(define in-order? (ordering (list 123 45 678)))]
                  (list (in-order? 104 401) (in-order? 104 104) (in-order? 401 104)))
                (list #true #false #false))
#;(check-expect (local [(define in-order? (ordering (list "once" "upon" "a" "time")))]
                  (sort in-order? (list "once" "upon" "a" "time")))
                (list "a" "once" "time" "upon"))

; ★ Define ‘ordering’:
(define (ordering entries)
  same?)

; table-sort : table → table
; --------------------------
; For a table and an index, produce a version of the table where its rows have been sorted
;  based on the column at the index. The column is assumed to have only numbers, or only texts.
#;(check-expect (table-sort (list (list "clara" 11)
                                  (list "rose" 9)
                                  (list "amy" 11)
                                  (list "donna" 10))
                            0)
                (list (list "amy" 11)
                      (list "clara" 11)
                      (list "donna" 10)
                      (list "rose" 9)))
#;(check-expect (table-sort (list (list "clara" 11)
                                  (list "rose" 9)
                                  (list "amy" 11)
                                  (list "donna" 10))
                            1)
                (list (list "rose" 9)
                      (list "donna" 10)
                      (list "clara" 11)
                      (list "amy" 11)))

; ★ Define ‘table-sort’:
(define (table-sort table index)
  (local [(define ordered? (ordering (column table index)))]
    (sort ordered? table)))

; Histogram
; =========
; These functions add the histogram feature.
; They do not affect the search nor sort features.

(define bar-color "darkgreen")

; histogram : list → image
; ------------------------
; For a list, produce a bar chart of the number of times each value occurs in the list.
(define (histogram a-list)
  (local [(define counted (counts a-list))]
    (beside (elements-image (map first counted))
            (apply align-above (adjoin "left" (map bar (map second counted)))))))

; Uncomment this after you have defined the rest of the functions in this section:
#;(check-expect (histogram (list "ob" "la" "di" "ob" "la" "da")) histogram:test-image)

; That function definition is correct.
; It relies on the functions you will write in the rest of this section.

; bar : non-negative-number → image
; ---------------------------------
; For a given amount, produce a solid rectangle with color ‘bar-color’ on a transparent background.
; The height of the background is ‘text-size’, and the width is ‘text-width’ times half the amount.
; The the visible bar is centered inside the background, with the same width but half the height.

#;(check-expect (bar 10) bar:test-image)
#;(check-expect (width  (bar 10)) (* 10 (/ text-size 2)))
#;(check-expect (height (bar 10)) (*  2 (/ text-size 2)))

; ★ Define ‘bar’:
(define (bar amount)
  (square 10 "solid" "purple"))

; sift:same? : any list → list
; ----------------------------
; For a list and a value, produce all elements of the list that are the same as the value.

#;(check-expect (sift:same? "was" (list "it" "was" "the" "best" "of" "times" "it" "was"))
                (list "was" "was"))

; ★ Define ‘sift:same?’:
(define (sift:same? c a-list)
  a-list)

; unique : list → list
; --------------------
; For a list, produce the version with all duplicate elements removed,
;  keeping the first occurrence of each element.
#;(check-expect (unique (list "a" "b" "a" "c" "b" "b" "d")) (list "a" "b" "c" "d"))

; ★ Write a Full Design check-expect for the case of the empty list:


; ★ Write fuller or Full Design versions of the following check-expects, which are
;    for the case that the list is not empty:
#;(check-expect (unique (list "b" "a" "c" "b" "b" "d")) (adjoin "b" (list "a" "c" "d")))
#;(check-expect (unique (list "a" "c" "b" "b" "d")) (adjoin "a" (list "c" "b" "d")))
; Hints:
;   • use ‘unique’ recursively
;   • there is a function  remove : any list → list
;     in our language that removes a value completely from a list
(check-expect (remove 1 (list 2 3 1 4 1 2)) (list 2 3 4 2))


; ★ Define ‘unique’:
(define (unique a-list)
  a-list)

; counts : list → list-of-lists
; -----------------------------
; For a list, produce a list of two-element lists, one for each value that occurs in the list,
;  each value paired up with how many times the value occurs in the list.
#;(check-expect (counts (list "ob" "la" "di" "ob" "la" "da")) (list (list "ob" 2)
                                                                    (list "la" 2)
                                                                    (list "di" 1)
                                                                    (list "da" 1)))
; Document some general properties of the function:
#;(check-expect (map first (counts (list "ob" "la" "di" "ob" "la" "da")))
                (unique (list "ob" "la" "di" "ob" "la" "da")))
#;(check-expect (apply = (adjoin 2 (map length (counts (list "ob" "la" "di" "ob" "la" "da")))))
                #true)
#;(check-expect (apply + (map second (counts (list "ob" "la" "di" "ob" "la" "da"))))
                (length (list "ob" "la" "di" "ob" "la" "da")))

; ★ Define ‘counts’:
(define (counts entries)
  (list))
