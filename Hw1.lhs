---
title: Homework #1, Due Friday, January 20th
---

Preliminaries
-------------

Before starting this assignment:

1. Read chapters 1 -- 3 of The Haskell School of Expression.
2. Download and install the [Glasgow Haskell Compiler (GHC)](http://www.haskell.org/ghc/).
3. Download the SOE code bundle from
   [the Haskell School of Expression page](/static/SOE.tar.gz).
4. Verify that it works by changing into the `SOE/src` directory and
   running `ghci Draw.lhs`, then typing `main0` at the prompt:
 
~~~
cd SOE/src
ghci Draw.lhs
*Draw> main0
~~~

  You should see a window with some shapes in it.

**NOTE:** If you have trouble installing SOE, [see this page](soe-instructions.html)

5. Download the required files for this assignment: [hw1.tar.gz](/static/hw1.tar.gz).
   Unpack the files and make sure that you can successfully run the main program (in `Main.hs`).
   We've provided a `Makefile`, which you can use if you like. You should see this output:

~~~
Main: Define me!
~~~

Haskell Formalities
-------------------

We declare that this is the Hw1 module and import some libraries:

> module Hw1 where
> import SOE
> import Play
> import XMLTypes

Part 1: Defining and Manipulating Shapes
----------------------------------------

You will write all of your code in the `hw1.lhs` file, in the spaces
indicated. Do not alter the type annotations --- your code must
typecheck with these types to be accepted.

The following are the definitions of shapes from Chapter 2 of SOE:

> data Shape = Rectangle Side Side
>            | Ellipse Radius Radius
>            | RtTriangle Side Side
>            | Polygon [Vertex]
>            deriving Show
> 
> type Radius = Float 
> type Side   = Float
> type Vertex = (Float, Float)

1. Below, define functions `rectangle` and `rtTriangle` as suggested
   at the end of Section 2.1 (Exercise 2.1). Each should return a Shape
   built with the Polygon constructor.

> rectangle       :: Side -> Side -> Shape
> rectangle s1 s2  = Polygon [(0,0),(s1,0),(s1,s2),(0,s2)] 
> rtTriangle        :: Side -> Side -> Shape
> rtTriangle s1 s2  = Polygon [(0,0),(s1,0),(0,s2)]

2. Define a function

> sides :: Shape -> Int

> sides s = 
>  case s of Rectangle _ _ -> 4
>            Ellipse _ _   -> 42
>            RtTriangle _ _-> 3
>            Polygon []    -> 0
>            Polygon (_:_:_:vs) -> length vs + 3
>            _ -> 0

  which returns the number of sides a given shape has.
  For the purposes of this exercise, an ellipse has 42 sides,
  and empty polygons, single points, and lines have zero sides.

3. Define a function

> bigger :: Shape -> Float -> Shape
> bigger s e = 
>     case s of Rectangle a b -> Rectangle (factor*a) (factor*b)
>               Ellipse a b -> Ellipse (factor*a) (factor*b)
>               RtTriangle a b -> RtTriangle (factor*a) (factor*b)
>               Polygon vs -> Polygon (map (\(a,b) -> ((factor*a),(factor*b))) vs)
>               where factor = sqrt(e)

  that takes a shape `s` and expansion factor `e` and returns
  a shape which is the same as (i.e., similar to in the geometric sense)
  `s` but whose area is `e` times the area of `s`.

4. The Towers of Hanoi is a puzzle where you are given three pegs,
   on one of which are stacked $n$ discs in increasing order of size.
   To solve the puzzle, you must move all the discs from the starting peg
   to another by moving only one disc at a time and never stacking
   a larger disc on top of a smaller one.
   
   To move $n$ discs from peg $a$ to peg $b$ using peg $c$ as temporary storage:
   
   1. Move $n - 1$ discs from peg $a$ to peg $c$.
   2. Move the remaining disc from peg $a$ to peg $b$.
   3. Move $n - 1$ discs from peg $c$ to peg $b$.
   
   Write a function
   
> hanoi :: Int -> String -> String -> String -> IO ()
> hanoi 1 r1 r2 r3 = do
>                 putStrLn( "Move disk from " ++ r1 ++ " to "++r2)
> hanoi n r1 r2 r3 = do
>                 hanoi(n-1)r1 r3 r2
>                 putStrLn( "Move disk from " ++ r1 ++ " to "++r2)
>                 hanoi(n-1) r3 r2 r1

  that, given the number of discs $n$ and peg names $a$, $b$, and $c$,
  where a is the starting peg,
  emits the series of moves required to solve the puzzle.
  For example, running `hanoi 2 "a" "b" "c"`

  should emit the text

~~~  
move disc from a to c
move disc from a to b
move disc from c to b
~~~

Part 2: Drawing Fractals
------------------------
 
1. The Sierpinski Carpet is a recursive figure with a structure similar to
   the Sierpinski Triangle discussed in Chapter 3:
 
![Sierpinski Carpet](/static/scarpet.png)
 
Write a function `sierpinskiCarpet` that displays this figure on the
screen:
 
> colors = [Magenta, Blue, Green, Red, Yellow, Black]
 
> colorSquare w x y s c = drawInWindow w (withColor (colors !! c)(polygon [(x,y),(x+s,y),(x+s,y+s),(x,y+s)]))
 
> colorCircle w x y s c = drawInWindow w (withColor (colors !! c)(ellipse (x,y) ((x+s),y+s)))
 
> sierpinskiSquare w x y s = do
>               let rs = s `div` 3
>               if (s < 3) then colorSquare w x y s 1
>                       else do
>                               sierpinskiSquare w x y rs
>                               sierpinskiSquare w (x+rs) y rs
>                               sierpinskiSquare w x (y+rs) rs
>                               sierpinskiSquare w (x+2*rs) y rs
>                               sierpinskiSquare w x (y+2*rs) rs
>                               sierpinskiSquare w (x+2*rs) (y+rs) rs
>                               sierpinskiSquare w (x+rs) (y+2*rs) rs
>                               sierpinskiSquare w (x+2*rs) (y+2*rs) rs
                                                               
                                                                       
> sierpinskiCarpet :: IO ()
> sierpinskiCarpet = runGraphics $ do
>                               w <- openWindow "Sierpinski Square" (500,500)
>                               sierpinskiSquare w 0 0 500
>                               k <- getKey w
>                               closeWindow w
 
Note that you either need to run your program in `SOE/src` or add this
path to GHC's search path via `-i/path/to/SOE/src/`.
Also, the organization of SOE has changed a bit, so that now you use
`import SOE` instead of `import SOEGraphics`.
 
2. Write a function `myFractal` which draws a fractal pattern of your
   own design.  Be creative!  The only constraint is that it shows some
   pattern of recursive self-similarity.
 
> myFractalSquare w x y s c = do
>               let c1 = (c+1) `mod` (length colors)
>               let c2 = (c+1) `mod` (length colors)
>               let rs = s `div` 2
>               if (s < 2) then colorCircle w x y s c
>               else       do
>                               colorSquare w x y s c
>                               colorCircle w x y rs c1
>                               colorCircle w (x+rs) y rs c1
>                               colorCircle w x (y+rs) rs c1
>                               colorCircle w (x+rs) (y+rs) rs c1
>                               myFractalSquare w (x + (rs `div` 2)) (y + (rs`div` 2)) rs c2


> myFractal :: IO ()
> myFractal = runGraphics $ do
>               w <- openWindow "My Fractal" (500,500)
>               myFractalSquare w 0 0 500 1
>               k <- getKey w
>               closeWindow w

Part 3: Transforming XML Documents
----------------------------------

First, a warmup:

1. Read chapters 5 and 7 of SOE.

2. Do problems 5.3, 5.5, 5.6, 7.1, and 7.2 from SOE, and turn them
   is as part of the source code you create below.

   Your `maxList` and `minList` functions may assume that the lists
   they are passed contain at least one element.

> lengthNonRecrusive :: [a] -> Int
> lengthNonRecrusive xs = foldr (\_ x -> x+1) 1 xs

> doubleEach       :: [Int] -> [Int]
> doubleEach []     = []
> doubleEach (x:xs) = 2*x:(doubleEach xs)

> doubleEachNonRecursive :: [Int] -> [Int]
> doubleEachNonRecursive xs = map (\x -> x*2) xs

> pairAndOne :: [Int] -> [(Int, Int)]
> pairAndOne [] = []
> pairAndOne (x:xs) = (x, x+1):pairAndOne(xs)

> pairAndOneNonRecursive :: [Int] -> [(Int, Int)]
> pairAndOneNonRecursive xs = map (\x -> (x, x+1)) xs

> addEachPair :: [(Int, Int)] -> [Int]
> addEachPair [] = []
> addEachPair ((a,b):xs) = (a+b):addEachPair(xs)

> addEachPairNonRecursive :: [(Int, Int)] -> [Int]
> addEachPairNonRecursive xs = map (\(a,b) -> a+b) xs

> minList :: [Int] -> Int
> minList [] = maxBound :: Int
> minList (x:xs) 
>                  | x <= minTail = x
>                  | otherwise = minTail
>                    where minTail = minList(xs)

> minListNonRecursive :: [Int] -> Int
> minListNonRecursive xs = foldl (min) maxBound xs

> maxList :: [Int] -> Int
> maxList [] = minBound :: Int
> maxList (x:xs) 
>                  | x >= maxTail = x
>                  | otherwise = maxTail
>                    where maxTail = maxList(xs)

> maxListNonRecursive :: [Int] -> Int
> maxListNonRecursive xs = foldl (max) minBound xs

> data Tree a = Leaf a | Branch (Tree a) (Tree a)
>               deriving (Show, Eq)

> leaf = Leaf 1
> branch = Branch (Leaf 1) (Leaf 2)
> tree = Branch (branch) (branch)

> fringe :: Tree a -> [a]
> fringe a = 
>     case a of Leaf b -> [b]
>               Branch b c -> fringe(b)++fringe(c)


> treeSize :: Tree a -> Int
> treeSize a = length (fringe a) 

> treeHeight :: Tree a -> Int
> treeHeight a = 
>     case a of Leaf b-> 0
>               Branch b c-> 1+(max (treeHeight b) (treeHeight c))

> data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a)
>                       deriving (Show, Eq)

> ileaf = ILeaf
> iTree = IBranch 3 ileaf ileaf
> takeTree :: Int -> InternalTree a -> InternalTree a
> takeTree a t
>  | a==0 = ILeaf
>  | otherwise = case t of ILeaf -> ILeaf
>                          IBranch b c d -> IBranch b (takeTree (a-1) c) (takeTree (a-1) d)


> takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
> takeTreeWhile p t =
>           case t of ILeaf -> ILeaf
>                     IBranch a b c
>                        |  (p a)==True ->IBranch a (takeTreeWhile p b) (takeTreeWhile p c)
>                        |otherwise -> ILeaf

Write the function map in terms of foldr:

> myMap :: (a -> b) -> [a] -> [b]
> myMap f xs = foldr (\a b -> (f a):b) [] xs

The rest of this assignment involves transforming XML documents.
To keep things simple, we will not deal with the full generality of XML,
or with issues of parsing. Instead, we will represent XML documents as
instances of the following simpliﬁed type:

~~~~


That is, a `SimpleXML` value is either a `PCDATA` ("parsed character
data") node containing a string or else an `Element` node containing a
tag and a list of sub-nodes.

The file `Play.hs` contains a sample XML value. To avoid getting into
details of parsing actual XML concrete syntax, we'll work with just
this one value for purposes of this assignment. The XML value in
`Play.hs` has the following structure (in standard XML syntax):

~~~
<PLAY>
  <TITLE>TITLE OF THE PLAY</TITLE>
  <PERSONAE>
    <PERSONA> PERSON1 </PERSONA>
    <PERSONA> PERSON2 </PERSONA>
    ... -- MORE PERSONAE
    </PERSONAE>
  <ACT>
    <TITLE>TITLE OF FIRST ACT</TITLE>
    <SCENE>
      <TITLE>TITLE OF FIRST SCENE</TITLE>
      <SPEECH>
        <SPEAKER> PERSON1 </SPEAKER>
        <LINE>LINE1</LINE>
        <LINE>LINE2</LINE>
        ... -- MORE LINES
      </SPEECH>
      ... -- MORE SPEECHES
    </SCENE>
    ... -- MORE SCENES
  </ACT>
  ... -- MORE ACTS
</PLAY>
~~~

* `sample.html` contains a (very basic) HTML rendition of the same
  information as `Play.hs`. You may want to have a look at it in your
  favorite browser.  The HTML in `sample.html` has the following structure
  (with whitespace added for readability):
  
~~~
<html>
  <body>
    <h1>TITLE OF THE PLAY</h1>
    <h2>Dramatis Personae</h2>
    PERSON1<br/>
    PERSON2<br/>
    ...
    <h2>TITLE OF THE FIRST ACT</h2>
    <h3>TITLE OF THE FIRST SCENE</h3>
    <b>PERSON1</b><br/>
    LINE1<br/>
    LINE2<br/>
    ...
    <b>PERSON2</b><br/>
    LINE1<br/>
    LINE2<br/>
    ...
    <h3>TITLE OF THE SECOND SCENE</h3>
    <b>PERSON3</b><br/>
    LINE1<br/>
    LINE2<br/>
    ...
  </body>
</html>
~~~

You will write a function `formatPlay` that converts an XML structure
representing a play to another XML structure that, when printed,
yields the HTML speciﬁed above (but with no whitespace except what's
in the textual data in the original XML).

data SimpleXML =
   PCDATA String
 | Element ElementName [SimpleXML]
 deriving Show

type ElementName = String
~~~~


> formatPlay :: SimpleXML -> SimpleXML 
> formatPlay xml = 
>   head (transform 0 xml)
>     where

transformchangesthegivenSimpleXMLinplaytosample. TheresultoftransformhastobealistofSimpleXMLsincesometimes oneelementbecomestwoafterexchange.

>       transform :: Int -> SimpleXML -> [SimpleXML] 
>       transform level old =
>         let 
>           br = Element "br" []

       formTitle makes the title element with given level of header size.

>           formTitle :: Int -> String -> SimpleXML
>           formTitle level title = Element ('h':(show level)) [PCDATA title]

formLististhecoroutineoftransform,changesalistofSimpleXMLfromplay tosample.

>           formList :: Int -> [SimpleXML] -> [SimpleXML]
>           formList level [] = []
>           formList level (head : tail) = (transform level head) ++ (formList level tail)
>         in case old of
>           Element "PLAY" body -> [Element "html" [Element "body" (formList (level+1) body)]]
>           Element "TITLE" [PCDATA title] -> [formTitle level title]
>           Element "PERSONAE" body -> (formTitle (level+1) "Dramatis Personae"):(formList (level+1) body)
>           Element "PERSONA" person -> person ++ [br]
>           Element "LINE" line -> line ++ [br]
>           Element "SPEAKER" person -> [Element "b" person , br]
>           Element _ body -> formList (level+1) body
>           _ -> [old]

The main action that we've provided below will use your function to
generate a ﬁle `dream.html` from the sample play. The contents of this
ﬁle after your program runs must be character-for-character identical
to `sample.html`.

> mainXML = do writeFile "dream.html" $ xml2string $ formatPlay play
>              testResults "dream.html" "sample.html"
>
> firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
> firstDiff [] [] = Nothing
> firstDiff (c:cs) (d:ds) 
>      | c==d = firstDiff cs ds 
>      | otherwise = Just (c:cs, d:ds)
> firstDiff cs ds = Just (cs,ds)
> 
> testResults :: String -> String -> IO ()
> testResults file1 file2 = do 
>   f1 <- readFile file1
>   f2 <- readFile file2
>   case firstDiff f1 f2 of
>     Nothing -> do
>       putStr "Success!\n"
>     Just (cs,ds) -> do
>       putStr "Results differ: '"
>       putStr (take 20 cs)
>       putStr "' vs '"
>       putStr (take 20 ds)
>       putStr "'\n"

Important: The purpose of this assignment is not just to "get the job
done" --- i.e., to produce the right HTML. A more important goal is to
think about what is a good way to do this job, and jobs like it. To
this end, your solution should be organized into two parts:

1. a collection of generic functions for transforming XML structures
   that have nothing to do with plays, plus

2. a short piece of code (a single deﬁnition or a collection of short
   deﬁnitions) that uses the generic functions to do the particular
   job of transforming a play into HTML.

Obviously, there are many ways to do the ﬁrst part. The main challenge
of the assignment is to ﬁnd a clean design that matches the needs of
the second part.

You will be graded not only on correctness (producing the required
output), but also on the elegance of your solution and the clarity and
readability of your code and documentation.  Style counts.  It is
strongly recommended that you rewrite this part of the assignment a
couple of times: get something working, then step back and see if
there is anything you can abstract out or generalize, rewrite it, then
leave it alone for a few hours or overnight and rewrite it again. Try
to use some of the higher-order programming techniques we've been
discussing in class.

Submission Instructions
-----------------------

* If working with a partner, you should both submit your assignments
  individually.
* Make sure your `hw1.lhs` is accepted by GHC without errors or warnings.
* Attach your `hw1.hs` file in an email to `cse230@goto.ucsd.edu` with the
  subject "HW1" (minus the quotes).
  *This address is unmonitored!*

Credits
-------

This homework is essentially Homeworks 1 & 2 from
<a href="http://www.cis.upenn.edu/~bcpierce/courses/552-2008/index.html">UPenn's CIS 552</a>.
