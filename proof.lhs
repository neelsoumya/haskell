\documentclass[twocolumn]{revtex4}
\usepackage[latin1]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{times}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{algorithm}
\usepackage{algorithmic}
\usepackage{times}
\usepackage{rotating}
\usepackage{multirow}
\usepackage{url}
\usepackage[small,compact]{titlesec}
\usepackage[small,it]{caption}
\pagestyle{empty} 
\newcommand{\argmax}{\ensuremath{\displaystyle\operatornamewithlimits{arg\:max}}}
\newcommand{\argmin}{\ensuremath{\displaystyle\operatornamewithlimits{arg\:min}}}
\renewcommand\floatpagefraction{.9}
\renewcommand\topfraction{.9}
\renewcommand\bottomfraction{.9}
\renewcommand\textfraction{.1}   
\setcounter{totalnumber}{50}
\setcounter{topnumber}{50}
\setcounter{bottomnumber}{50}
\addtolength{\floatsep}{-0.5ex}
\addtolength{\textfloatsep}{-1ex}
\addtolength{\textfloatsep}{-1ex}
\addtolength{\abovecaptionskip}{-1ex}
\addtolength{\belowcaptionskip}{-1ex}
\begin{document}
\title{CS 558 HW2}
\author{Soumya Banerjee\\
\\}

\maketitle

\section{HW 2.3 (Graphs)}
\subsection{1. Are all directed graphs representable in this fashion?}

No, all directed graphs cannot be represented in this fashion. The edges are represented in the form [(Int,Int)] and one would run out of numbers to enumerate the vertices if the number of vertices exceeded $2^{31} - 1$.

\section{HW 2.6 (Programs)}


foldr f e xs = foldl (flip f) e (reverse xs)

axioms:


foldr f v [] = v					 (Eq. 1)

foldr f v (x:xs) = f x (foldr f v xs)		 (Eq. 2)

foldl f v [] = v					 (Eq. 3)

foldl f v (x:xs) = foldl f (f v x) xs		 (Eq. 4)


flip f x y = f y x					 (Eq. 5)

reverse [] = []					 (Eq. 6)

reverse (x:xs) = (reverse xs) ++ [x]	 (Eq. 7)

[] ++ ys = ys						(Eq. 8)

(x:xs) ++ ys = x:(xs ++ ys)			(Eq. 9)

Proof by calculation


Let xs = [$x_{0}$, $x_{1}$, $x_{2}$, ......., $x_{n}$]       (Eq. 10)

LHS = foldr f e xs

	= \{ by Eq. 10 \}

	foldr f e [$x_{0}$, $x_{1}$, $x_{2}$, ......., $x_{n}$] 	
	
	= \{ by Eq. 2 definition of foldr \}
	
	f $x_{0}$ (foldr f v [$x_{1}$, $x_{2}$, ......., $x_{n}$] ) 
	
	= \{ by Eq. 2 definition of foldr \}
	
	f $x_{0}$ ( f $x_{1}$ (foldr f v [$x_{2}$,  ......., $x_{n}$] )) 

	= \{ by Eq. 2 definition of foldr \}
	
	f $x_{0}$ ( f $x_{1}$ (f $x_{2}$ (foldr f v [$x_{3}$,  ......., $x_{n}$] ))) 
	
	= \{ after applying Eq. 2 $n - 2$ times\}
	
	f $x_{0}$ ( f $x_{1}$ (f $x_{2}$ (.... (f $x_{n}$ (foldr f v [] ))... ))) 

	= \{ by Eq. 1 base case definition of foldr\}
	
	f $x_{0}$ ( f $x_{1}$ (f $x_{2}$ (.... (f $x_{n}$ e)... ))) 
	
	
	

RHS = foldl (flip f) e (reverse xs)

	 = \{ by Eq. 10 \}
	 
	 foldl (flip f) e (reverse [$x_{0}$, $x_{1}$, $x_{2}$, ......., $x_{n}$])	

	 = \{ by Eq. 7 definition of reverse \}
	 
	 foldl (flip f) e ((reverse [$x_{1}$, $x_{2}$, ......., $x_{n}$]) ++ [$x_{0}$] )
	 
	 = \{ by Eq. 7 definition of reverse \}
	 
	 foldl (flip f) e ( ((reverse [$x_{2}$, ......., $x_{n}$]) ++ [$x_{1}$]) ++ [$x_{0}$] )
	 
	 = \{ by Eq. 7 definition of reverse \}
	 
	 foldl (flip f) e ( ( ((reverse [$x_{3}$, ......., $x_{n}$]) ++ [$x_{2}$]) ++ [$x_{1}$]) ++ [$x_{0}$] )
	
	 = \{ after applying Eq. 7 $n - 2$ times \}
	 
	 foldl (flip f) e ( ( ( ( ...... ((reverse [ ]) ++ [$x_{n}$]) ++  .... ) ++ [$x_{2}$]) ++ [$x_{1}$]) ++ [$x_{0}$] )

	 = \{ after applying Eq. 6 base case of reverse \}
	 
	 foldl (flip f) e ( ( ( ( ...... ([ ] ++ [$x_{n}$]) ++  .... ) ++ [$x_{2}$]) ++ [$x_{1}$]) ++ [$x_{0}$] )

	 = \{ after applying Eq. 8 base case of ++ \}
	 
	 foldl (flip f) e ( ( ( ( ...... ([$x_{n}$] ++ [$x_{n-1}$]) .... ) ++ [$x_{2}$]) ++ [$x_{1}$]) ++ [$x_{0}$] )

	 = \{ after applying Eq. 9 definition of ++ \}
	 
	 foldl (flip f) e ( ( ( ( ...... ([$x_{n}$, $x_{n-1}$]) .... ) ++ [$x_{2}$]) ++ [$x_{1}$]) ++ [$x_{0}$] )

	 = \{ after applying Eq. 9 $n - 1$ times \}
	 
	 foldl (flip f) e ([$x_{n}$, $x_{n-1}$, ....., $x_{2}$, $x_{1}$, $x_{0}$])
	 
	 = \{ after applying Eq. 4 definition of foldl \}
	 
	 foldl (flip f) (flip f e $x_{n}$) ([$x_{n-1}$, ....., $x_{2}$, $x_{1}$, $x_{0}$])
	 
	 = \{ after applying Eq. 5 definition of flip \}
	 
	 foldl (flip f) (f $x_{n}$ e) ([$x_{n-1}$, ....., $x_{2}$, $x_{1}$, $x_{0}$])
	 
	 = \{ after applying Eq. 4 definition of foldl \}
	 
	 foldl (flip f) (flip f (f $x_{n}$ e) $x_{n-1}$) ([$x_{n-2}$, ....., $x_{2}$, $x_{1}$, $x_{0}$])
	 
	 = \{ after applying Eq. 5 definition of flip \}
	 
	 foldl (flip f) (f $x_{n-1}$ (f $x_{n}$ e)) ([$x_{n-2}$, ....., $x_{2}$, $x_{1}$, $x_{0}$])
	 
	 = \{ after applying Eq. 5 definition of flip and Eq. 4 definition of foldl $n - 4$ times \}
	 
	 foldl (flip f) (f $x_{3}$ (.... (f $x_{n-1}$ (f $x_{n}$ e)) ....) ) ([$x_{2}$, $x_{1}$, $x_{0}$])

	 = \{ after applying Eq. 4 definition of foldl \}
	 
	 foldl (flip f) (flip f (f $x_{3}$ (.... (f $x_{n-1}$ (f $x_{n}$ e)) ....) ) $x_{2}$) ([$x_{1}$, $x_{0}$])

	 = \{ after applying Eq. 5 definition of flip \}
	 
	 foldl (flip f) (f $x_{2}$ (f $x_{3}$ (.... (f $x_{n-1}$ (f $x_{n}$ e)) ....) ) ) ([$x_{1}$, $x_{0}$])

	 = \{ after applying Eq. 4 definition of foldl \}
	 
	 foldl (flip f) (flip f (f $x_{2}$ (f $x_{3}$ (.... (f $x_{n-1}$ (f $x_{n}$ e)) ....) ) ) $x_{1}$) ([$x_{1}$, $x_{0}$])

	 = \{ after applying Eq. 5 definition of flip \}
	 
	 foldl (flip f) (f $x_{1}$ (f $x_{2}$ (f $x_{3}$ (.... (f $x_{n-1}$ (f $x_{n}$ e)) ....) ) ) ) ([$x_{0}$])

	 = \{ after applying Eq. 4 definition of foldl \}
	 
	 foldl (flip f) (flip f (f $x_{1}$ (f $x_{2}$ (f $x_{3}$ (.... (f $x_{n-1}$ (f $x_{n}$ e)) ....) ) ) ) $x_{0}$) ([])

	 = \{ after applying Eq. 5 definition of flip \}
	 
	 foldl (flip f) (f $x_{0}$ (f $x_{1}$ (f $x_{2}$ (f $x_{3}$ (.... (f $x_{n-1}$ (f $x_{n}$ e)) ....) ) ) ) ) ([])

	 = \{ after applying Eq. 3 base case of foldl \}
	 
	 f $x_{0}$ (f $x_{1}$ (f $x_{2}$ (f $x_{3}$ (.... (f $x_{n-1}$ (f $x_{n}$ e)) ....) ) ) )
	 
	 
	
	
	Hence LHS = RHS by the calculational proof method and foldr f e xs = foldl (flip f) e (reverse xs).

	
	


{
\footnotesize
\bibliographystyle{abbrv}
\addtolength{\itemsep}{-2mm}
\bibliography{mbsearch}
}
\end{document}
\bibliographystyle{plain}
\end{document}
