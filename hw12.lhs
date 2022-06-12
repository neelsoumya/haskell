\documentclass[11pt]{article}
%% Literate Haskell script intended for lhs2TeX.

%include polycode.fmt
%format alpha = "\alpha"

\usepackage{fullpage}
\usepackage{mathpazo}
\usepackage{graphicx}
\usepackage[centertags]{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{amsthm}


\title{CS 557 Homework 1 Problem 1.2}
\author{Soumya Banerjee}
\date{october 6th, 2008}
\begin{document}
\thispagestyle{empty}
\maketitle

\tableofcontents
\section{Introduction}


\begin{code}
module Main where
main = do
	if (elem' 12 [12,23]) then putStr "Yes\n" else putStr "no"
\end{code}

\section{Types}

\subsection{Types for generalized circuits}




\begin{code}
elem'          :: (Eq a) => a -> [a] -> Bool
elem' m []     = False
elem' m (n:ns) = if m == n then True else elem' m ns 
\end{code}

...

\section{Set Equal}



\begin{code}
setEqual           :: [Integer] -> [Integer] -> Bool
setEqual [] [] = True
setEqual [] x = False
setEqual x [] = False
setEqual (m:ms) (n:ns) = (m == n) && (setEqual ms ns)

\end{code}
...

\section{Set Difference}

\begin{code}
setDiff [] x = []
setDiff x [] = x
setDiff (m:ms) (n:ns) = if elem' m (n:ns) then setDiff ms (n:ns) else m:(setDiff ms (n:ns))
 
\end{code}

\section{Set Intersection}

\begin{code}
setIntersection  :: [Integer] -> [Integer] -> [Integer]
setIntersection [] x = []
setIntersection (m:ms) (n:ns) = if elem' m (n:ns) then m:(setIntersection ms (n:ns)) else setIntersection ms (n:ns)
 
\end{code}

\section{Set Union}

\begin{code}
setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion xs ys = (setDiff xs ys) ++ ys
 
\end{code}


\section{Power Set}

\begin{code}
compl :: Integer -> [[Integer]] -> [[Integer]]
compl y yss = [[y] ++ ys | ys <- yss]

powerSet  :: [Integer] -> [[Integer]]
powerSet [] = [[]]
powerSet (x:xs) =  (powerSet xs) ++ (compl x (powerSet xs))
 
\end{code}
\end{document}
