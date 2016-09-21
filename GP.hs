import Data.Char

cripa::String->String
cripa [] =[]
cripa (a:b) | ((ord a)+1)> (ord 'z') = [chr ((ord a)-25)] ++ cripa b
			| a == ' ' = [a] ++ cripa b
			| otherwise = [chr ((ord a)+1)] ++ cripa b

cripateste1 :: Bool
cripateste1 | cripa "abcd" == "bcde" = True
			| otherwise = False
			
cripateste2 :: Bool
cripateste2 | cripa "zaap" == "abbq" = True
			| otherwise = False

cripateste3 :: Bool
cripateste3 | cripa "eu amo" == "fv bnp" = True
			| otherwise = False

cripateste4 :: Bool
cripateste4 | cripa "eu hoje estou com fome" == "fv ipkf ftupv dpn gpnf" = True
			| otherwise = False
			
porcentagemdeacerto::Float
porcentagemdeacerto = 100*contaacerto[cripateste1,cripateste2,cripateste3,cripateste4]/4


contaacerto [] = 0
contaacerto (a:b) | a == True = contaacerto b +1
				  | otherwise = contaacerto b