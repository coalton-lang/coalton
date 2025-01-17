(defpackage #:coalton-benchmarking/printing
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:iter  #:coalton-library/iterator)
   (#:vec   #:coalton-library/vector)
   (#:math  #:coalton-library/math)
   (#:str   #:coalton-library/string)
   (#:list  #:coalton-library/list)
   (#:cell  #:coalton-library/cell)
   (#:state #:coalton-library/monad/state)
   (#:seq   #:coalton-library/seq))
  (:export
   #:render

   #:BoxChar
   #:Horizontal
   #:Vertical
   #:TopLeft
   #:TopRight
   #:TopDown
   #:BottomLeft
   #:BottomRight
   #:BottomUp
   #:LeftCross
   #:RightCross
   #:Cross
   #:Newline

   #:Alignment
   #:Right
   #:Left
   #:Center
   #:TableCell
   #:TableComponent
   #:TopEdge
   #:TopInternalEdge
   #:InternalEdge
   #:BottomEdge

   #:TableHeader
   #:TableRow
   #:TopTableRow

   #:TableState
   #:Header
   #:SecondaryHeader
   #:Row
   #:TopRow
   #:Bottom
   #:coalton-table))

(in-package #:coalton-benchmarking/printing)

(coalton-toplevel

  (define-class (Render :a)
    "Class for rendering portions of tables."
    (render
     "Renders a portion of a table in string form."
     (:a -> String))))

(coalton-toplevel

  (define-type BoxChar
    "Coalton table printing atoms."
    Horizontal
    Vertical
    TopLeft
    TopRight
    TopDown
    BottomLeft
    BottomRight
    BottomUp
    LeftCross
    RightCross
    Cross
    Newline)

  (define-type TableComponent
    "Coalton table printing combinations."
    (TopEdge UFix)
    "The top edge of a table (above the header)."
    (TopInternalEdge UFix UFix)
    "The top edge of the top row."
    (InternalEdge UFix UFix)
    "An internal edge between rows."
    (BottomEdge UFix UFix)
    "The bottom edge of the table.")

  (declare %column-spacing (UFix -> UFix -> (Tuple UFix UFix)))
  (define (%column-spacing width columns)
    "Evenly divides the width by the number of columns. Returns the size for each column plus the remainder."
    (if (== columns 1)
        (Tuple width 0)
        (let ((size (math:floor/ (into width) (into columns)))
              (remainder (- (into width) (* size (into columns)))))
          (Tuple (math:1- (fromint size)) (fromint remainder)))))

  (declare %write-component (UFix -> UFix -> BoxChar -> BoxChar -> BoxChar -> String))
  (define (%write-component width columns start-char break-char end-char)
    "Writes a component (edge) as a string."
    (let ((spacing (%column-spacing width columns))
          (out (the (vec:Vector BoxChar) (vec:new))))
      (vec:push! start-char out)
      (for i in (iter:up-to (+ (fst spacing) (snd spacing)))
        (vec:push! Horizontal out))
      (for j in (iter:up-to (math:1- columns))
        (vec:push! break-char out)
        (for i in (iter:up-to (fst spacing))
          (vec:push! Horizontal out)))
      (vec:push! end-char out)
      (vec:push! NewLine out)
      (render out)))

  (declare %top-edge (UFix -> String))
  (define (%top-edge width)
    "Generates the top edge of a table."
    (%write-component width 1 TopLeft Horizontal TopRight))

  (declare %top-internal-edge (UFix -> UFix -> String))
  (define (%top-internal-edge width columns)
    "Generates the top-edge of a row of width `width` divided evenly into `columns` columns"
    (%write-component width columns LeftCross TopDown RightCross))

  (declare %internal-edge (UFix -> UFix -> String))
  (define (%internal-edge width columns)
    "Generates the top-edge of a row of width `width` divided evenly into `columns` columns"
    (%write-component width columns LeftCross Cross RightCross))

  (declare %bottom-edge (UFix -> UFix -> String))
  (define (%bottom-edge width columns)
    "Generates the top-edge of a row of width `width` divided evenly into `columns` columns"
    (%write-component width columns BottomLeft BottomUp BottomRight))

  ;;
  ;; Writing text, cells, headers
  ;;

  (define-type Alignment
    "The direction the cell is aligned."
    Right
    Left
    Center)

  (define-struct TableCell
    "A table cell with text and alignment."
    (text String)
    (alignment Alignment))

  (declare %whitespace (UFix -> String))
  (define (%whitespace width)
    "Generates whitespace with a given width."
    (mconcat (vec:with-initial-element width " ")))

  (declare %right-cell (String -> UFix -> String))
  (define (%right-cell text blank)
    "Writes text as if to a cell, with appropriate whitespace"
    (mconcat (vec:make (%whitespace (1- blank)) text " ")))

  (declare %left-cell (String -> UFix -> String))
  (define (%left-cell text blank)
    "Writes text as if to a cell, with appropriate whitespace"
    (mconcat (vec:make " " text (%whitespace (1- blank)))))

  (declare %center-cell (String -> UFix -> String))
  (define (%center-cell text blank)
    "Writes text as if to a cell, with appropriate whitespace"
    (let ((offsets (Tuple (%whitespace (fromint (math:floor/ (into blank) 2)))
                          (%whitespace (fromint (math:ceiling/ (into blank) 2))))))
      (mconcat (vec:make (fst offsets) text (snd offsets)))))

  (declare %write-cell (TableCell -> UFix -> String))
  (define (%write-cell (TableCell cell-text alignment) width)
    "Writes text as if to a cell, with appropriate whitespace"
    ;; this handles text too long for a table cell
    (let ((text (if (>= (str:length cell-text) width)
                    (str:substring cell-text 0 (- width 2))
                    cell-text))
          (blank (- width (str:length text))))
      (match alignment
        ((Right)
         (%right-cell text blank))
        ((Left)
         (%left-cell text blank))
        ((Center)
         (%center-cell text blank)))))

  ;;
  ;;
  ;;

  (declare %write-row-component (UFix -> (seq:Seq TableCell) -> TableComponent -> String))
  (define (%write-row-component width cells top-edge)
    "Writes a full table row of width `width` containing `cells`."
    (let ((columns (seq:size cells))
          (spacing (%column-spacing width columns))
          (out (the (vec:Vector String) (vec:new))))
      (vec:push! (render top-edge) out)
      (vec:push! (render Vertical) out)
      (vec:push! (%whitespace (snd spacing)) out)
      (for cell in cells
        (vec:push! (%write-cell cell (fst spacing)) out)
        (vec:push! (render Vertical) out))
      (vec:push! (render NewLine) out)
      (mconcat out)))

  (declare %write-top-row (UFix -> (seq:Seq TableCell) -> String))
  (define (%write-top-row width cells)
    "Writes the top-row of a table- has no lines crossing above the top."
    (%write-row-component width cells (TopInternalEdge width (seq:size cells))))

  (declare %write-row (UFix -> (seq:Seq TableCell) -> String))
  (define (%write-row width cells)
    "Writes a row of a table."
    (%write-row-component width cells (InternalEdge width (seq:size cells))))

  (define-instance (Render TableComponent)
    (define (render tc)
      (match tc
        ((TopEdge width)
         (%top-edge width))
        ((TopInternalEdge width columns)
         (%top-internal-edge width columns))
        ((InternalEdge width columns)
         (%internal-edge width columns))
        ((BottomEdge width columns)
         (%bottom-edge width columns)))))

  (define-instance (Render BoxChar)
    (define (render bc)
      (match bc
        ((Horizontal)  "─")
        ((Vertical)    "│")
        ((TopLeft)     "┌")
        ((TopRight)    "┐")
        ((TopDown)     "┬")
        ((BottomLeft)  "└")
        ((BottomRight) "┘")
        ((BottomUp)    "┴")
        ((Cross)       "┼")
        ((LeftCross)   "├")
        ((RightCross)  "┤")
        ((Newline)     "
"))))

  (define-instance (Render (List BoxChar))
    (define (render bcs)
      (mconcat (map render bcs))))

  (define-instance (Render (vec:Vector BoxChar))
    (define (render bcs)
      (mconcat (map render bcs)))))

(coalton-toplevel

  (define-struct TableHeader
    (width "The width of the table" UFix)
    (text  "The text of the table"  String))

  (define-struct TableRow
    "A struct that can be used to generate a printed table row."
    (width           "The width of the table row." UFix)
    (cells "A vector of column contents." (seq:Seq TableCell)))

  (define-struct TopTableRow
    "A struct that can be used to generate a printed table row with no row above."
    (width           UFix)
    (column-contents (seq:Seq TableCell)))

  (define-instance (Render TableRow)
    (define (render (TableRow width contents))
      (%write-row width contents)))

  (define-instance (Render TopTableRow)
    (define (render (TopTableRow width contents))
      (%write-top-row width contents)))

  (define-instance (Render TableHeader)
    (define (render (TableHeader width text))
      (mconcat
       (vec:make
        (%top-edge (math:1- width))
        (render Vertical)
        (%write-cell (TableCell text Center) (math:1- width))
        (render Vertical)
        (render NewLine))))))

;;;
;;; Monadic table building
;;;

(coalton-toplevel

  (declare %add-component ((Render :a) => :a -> (state:ST TableState Unit)))
  (define (%add-component component)
    "Adds a rendered component to the table printout."
    (do
     (table <- state:get)
     (pure (cell:update! (fn (s)
                           (str:concat s (render component)))
                         (.printout table)))
      (state:put table)))

  (define-struct TableState
    (printout "The table string being rendered." (Cell String))
    (width    "The width of the table" UFix))

  (define-instance (Into TableState String)
    (define (into (TableState printout width))
      (cell:read printout)))

  (define-instance (Default TableState)
    (define (default)
      (TableState
       (cell:new "")
       90)))

  (declare Header (String -> (state:ST TableState Unit)))
  (define (Header text)
    "Add a header to the table printout."
    (do
     (table <- state:get)
     (%add-component (TableHeader (.width table) text))))

  (define (SecondaryHeader text)
    "Adds a header below the first header."
    (do
     (table <- state:get)
     (%add-component (TableRow (1- (.width table)) (seq:make text)))))

  (declare Row ((seq:Seq TableCell) -> (state:ST TableState Unit)))
  (define (Row cells)
    "Add a row to the table printout."
    (do
     (table <- state:get)
     (%add-component (TableRow (.width table) cells))))

  (declare TopRow ((seq:Seq TableCell) -> (state:ST TableState Unit)))
  (define (TopRow cells)
    "Add a top row to the table printout (no upward cross characters)."
    (do
     (table <- state:get)
     (%add-component (TopTableRow (.width table) cells))))

  (declare Bottom (UFix -> (state:ST TableState Unit)))
  (define (Bottom columns)
    "Add the bottom edge to the table printout."
    (do
     (table <- state:get)
     (%add-component (BottomEdge (.width table) columns)))))

(cl:defmacro coalton-table (width cl:&rest forms)
  "Can be used for building tables or portions of tables.
Forms should be provided with the understanding that they are embedded in a `do` form."
  (cl:let ((forms (cl:append '(do) forms)))
    `(cell:read (.printout (fst (state:run ,forms (TableState (cell:new "") ,width)))))))
