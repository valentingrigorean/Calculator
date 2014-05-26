module Main where
import Graphics.UI.WX
import Calc

buttonSize = (Size 30 25)
marginSize = 10
columnSize = 10
rowSize    = 10

setAnswer :: Textual w => w -> IO()	
setAnswer w = do
	val <- get w text	
	valid <- validate w ""
	if valid == 0
	then do
		ans <- calcUI val
		set w [text := ans]
	else return ()

remove1 :: Textual w => w -> IO()	
remove1 w = do
	val <- get w text	
	valid <- validate w ""
	if valid == 0
	then set w [text := take (length val -1) val]	
	else return()

validate :: Textual w => w -> String -> IO Int
validate w s = do
	val <- get w text
	if (length val) == 0 && s == ""
	then return 1
	else if val == "" && s == "."
	then return 1
	else if (length $ filter (== '.') val) == 1 && s == "."
	then return 1
	else if (val == "0" && s == "0")
	then return 1
	else return 0

isDigit :: [Char] -> Int
isDigit (x:xs) = if fromEnum x - 48 >= 0 && fromEnum x - 48 <= 9
	then 0
	else 1

setText :: Textual w => w -> String -> IO()	
setText w t = do
	val <- get w text
	valid <- validate w t
	if valid == 0
	then
		if val == "0" && isDigit t == 0
		then set w [text := t]
		else set w [text :~ (++t)]	
	else return()	
	
initButton p w s  = do
	val <- button p [text:=s,clientSize := buttonSize]
	set val [on command := setText w s]
	return val
	
main :: IO()
main = start gui

gui :: IO ()
gui = do
	f <- frameFixed [ text := "Calculator"]
	p <- panel f [] 
	te <- textEntry p [alignment := AlignRight,enabled := False]
	b0 				<- initButton p te "0"
	b1 				<- initButton p te "1"
	b2 				<- initButton p te "2"
	b3 				<- initButton p te "3"
	b4 				<- initButton p te "4"
	b5 				<- initButton p te "5"
	b6 				<- initButton p te "6"
	b7 				<- initButton p te "7"
	b8 				<- initButton p te "8"
	b9 				<- initButton p te "9"
	bpleft 			<- initButton p te "("
	bpright 		<- initButton p te ")"
	baddition 		<- initButton p te "+"
	bsubtraction 	<- initButton p te "-"
	bmultiplication	<- initButton p te "*"
	bdivision 		<- initButton p te "/"
	bmodulus 		<- initButton p te "%"
	bsin 			<- initButton p te "sin"
	bcos 			<- initButton p te "cos"
	bsqrt 			<- initButton p te "sqrt"
	bpow 			<- initButton p te "^"	
	bfraction 		<- initButton p te "."
	bequal 			<- button p [text := "=", clientSize := buttonSize, on command := setAnswer te]
	bclearall 		<- button p [text := "C", clientSize := buttonSize, on command := set te [text := ""]]
	bclear1 		<- button p [text := "←", clientSize := buttonSize, on command := remove1 te]
	
	set p 	[ layout := 
				margin 10(
				column 10 
				[ 			
					widget te,
					row 10
					[
						widget bclear1,
						widget bclearall,
						widget bpleft,
						widget bpright,
						widget bsqrt
					],
					row 10
					[
						widget b7,
						widget b8,
						widget b9,
						widget bdivision,
						widget bmodulus
					],
					row 10
					[
						widget b4,
						widget b5,
						widget b6,
						widget bmultiplication,
						widget bpow
					],
					row 10
					[
						widget b1,
						widget b2,
						widget b3,
						widget bsubtraction,
						widget bsin
					],
					row 10
					[
						widget bfraction,
						widget b0,	
						widget bequal,						
						widget baddition,
						widget bcos					
					]
				])		
			]	
	set f [layout := widget p]	
	set te [clientSize := (Size 185 25)]
	set p
		[on (charKey '0') := set te [text := "0"]
		,on (charKey '1') := set te [text := "1"]
		,on (charKey '2') := set te [text := "2"]
		,on (charKey '3') := set te [text := "3"]
		,on (charKey '4') := set te [text := "4"]
		,on (charKey '5') := set te [text := "5"]
		,on (charKey '6') := set te [text := "6"]
		,on (charKey '7') := set te [text := "7"]
		,on (charKey '8') := set te [text := "8"]
		,on (charKey '9') := set te [text := "9"]
		]          
	set f
		[on (charKey '0') := set te [text := "0"]
		,on (charKey '1') := set te [text := "1"]
		,on (charKey '2') := set te [text := "2"]
		,on (charKey '3') := set te [text := "3"]
		,on (charKey '4') := set te [text := "4"]
		,on (charKey '5') := set te [text := "5"]
		,on (charKey '6') := set te [text := "6"]
		,on (charKey '7') := set te [text := "7"]
		,on (charKey '8') := set te [text := "8"]
		,on (charKey '9') := set te [text := "9"]
		] 	
	return ()