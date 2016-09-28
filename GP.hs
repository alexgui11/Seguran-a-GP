import Data.Char

--os testes 1 são o teste mais básico, no quarl passo uma palavra, e coloco o retorno correto, comparando o acerto, caso esteja certo Retorna True, Caso não False
--os testes 2 são para testa como lida com a condição do z, tendo em vista que voltaria para o começo da lista, caso esteja certo Retorna True, Caso não False
--os testes 2 são para testar se a condição do " " está correta,  caso esteja certo Retorna True, Caso não False
--os testes 4 é algo maior para comprovar que está correto caso esteja certo Retorna True, Caso não False
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
		
------------------------------------------------------------------------------
cripbteste1 :: Bool
cripbteste1 | cripb "abcd" == "cdef" = True
			| otherwise = False

cripbteste2 :: Bool
cripbteste2 | cripb "zaap" == "bccr" = True
			| otherwise = False

cripbteste3 :: Bool
cripbteste3 | cripb "eu amo" == "gw coq" = True
			| otherwise = False

cripbteste4 :: Bool
cripbteste4 | cripb "eu hoje estou com fome" == "gw jqlg guvqw eqo hqog" = True
			| otherwise = False
---------------------------------------
cripcteste1 :: Bool
cripcteste1 | cripc "abcd" == "defg" = True
			| otherwise = False

cripcteste2 :: Bool
cripcteste2 | cripc "zaap" == "cdds" = True
			| otherwise = False

cripcteste3 :: Bool
cripcteste3 | cripc "eu amo" == "hx dpr" = True
			| otherwise = False

cripcteste4 :: Bool
cripcteste4 | cripc "eu hoje estou com fome" == "hx krmh hvwrx frp irph" = True
			| otherwise = False

---------------------------------------------
cripdteste1 :: Bool
cripdteste1 | cripd "abcd" == "efgh" = True
			| otherwise = False

cripdteste2 :: Bool
cripdteste2 | cripd "zaap" == "deet" = True
			| otherwise = False

cripdteste3 :: Bool
cripdteste3 | cripd "eu amo" == "iy eqs" = True
			| otherwise = False

cripdteste4 :: Bool
cripdteste4 | cripd "eu hoje estou com fome" == "iy lsni iwxsy gsq jsqi" = True
			| otherwise = False
------------------------------------------
cripeteste1 :: Bool
cripeteste1 | cripe "abcd" == "fghi" = True
			| otherwise = False

cripeteste2 :: Bool
cripeteste2 | cripe "zaap" == "effu" = True
			| otherwise = False

cripeteste3 :: Bool
cripeteste3 | cripe "eu amo" == "jz frt" = True
			| otherwise = False

cripeteste4 :: Bool
cripeteste4 | cripe "eu hoje estou com fome" == "jz mtoj jxytz htr ktrj" = True
			| otherwise = False
------------------------------------------
cripfteste1 :: Bool
cripfteste1 | cripf "abcd" == "ghij" = True
			| otherwise = False

cripfteste2 :: Bool
cripfteste2 | cripf "zaap" == "fggv" = True
			| otherwise = False

cripfteste3 :: Bool
cripfteste3 | cripf "eu amo" == "ka gsu" = True
			| otherwise = False

cripfteste4 :: Bool
cripfteste4 | cripf "eu hoje estou com fome" == "ka nupk kyzua ius lusk" = True
			| otherwise = False
-------------------------------------------
cripgteste1 :: Bool
cripgteste1 | cripg "abcd" == "hijk" = True
			| otherwise = False

cripgteste2 :: Bool
cripgteste2 | cripg "zaap" == "ghhw" = True
			| otherwise = False

cripgteste3 :: Bool
cripgteste3 | cripg "eu amo" == "lb htv" = True
			| otherwise = False

cripgteste4 :: Bool
cripgteste4 | cripg "eu hoje estou com fome" == "lb ovql lzavb jvt mvtl" = True
			| otherwise = False
-----------------------------------------
criphteste1 :: Bool
criphteste1 | criph "abcd" == "ijkl" = True
			| otherwise = False

criphteste2 :: Bool
criphteste2 | criph "zaap" == "hiix" = True
			| otherwise = False

criphteste3 :: Bool
criphteste3 | criph "eu amo" == "mc iuw" = True
			| otherwise = False

criphteste4 :: Bool
criphteste4 | criph "eu hoje estou com fome" == "mc pwrm mabwc kwu nwum" = True
			| otherwise = False
-------------------------------------------		
cripiteste1 :: Bool
cripiteste1 | cripi "abcd" == "jklm" = True
			| otherwise = False

cripiteste2 :: Bool
cripiteste2 | cripi "zaap" == "ijjy" = True
			| otherwise = False

cripiteste3 :: Bool
cripiteste3 | cripi "eu amo" == "nd jvx" = True
			| otherwise = False

cripiteste4 :: Bool
cripiteste4 | cripi "eu hoje estou com fome" == "nd qxsn nbcxd lxv oxvn" = True
			| otherwise = False	
-

				  
cripa::String->String
cripa [] =[]
cripa (a:b) | ((ord a)+1)> (ord 'z') = [chr ((ord a)-25)] ++ cripa b
			| a == ' ' = [a] ++ cripa b
			| otherwise = [chr ((ord a)+1)] ++ cripa b

				  
cripb::String->String
cripb [] =[]
cripb (a:b) | ((ord a)+2)> (ord 'z') = [chr ((ord a)-24)] ++ cripb b
			| a == ' ' = [a] ++ cripb b
			| otherwise = [chr ((ord a)+2)] ++ cripb b
			
cripc::String->String
cripc [] =[]
cripc (a:b) | ((ord a)+3)> (ord 'z') = [chr ((ord a)-23)] ++ cripc b
			| a == ' ' = [a] ++ cripc b
			| otherwise = [chr ((ord a)+3)] ++ cripc b
			
cripd::String->String
cripd [] =[]
cripd (a:b) | ((ord a)+4)> (ord 'z') = [chr ((ord a)-22)] ++ cripd b
			| a == ' ' = [a] ++ cripd b
			| otherwise = [chr ((ord a)+4)] ++ cripd b
			
cripe::String->String
cripe [] =[]
cripe (a:b) | ((ord a)+5)> (ord 'z') = [chr ((ord a)-21)] ++ cripe b
			| a == ' ' = [a] ++ cripe b
			| otherwise = [chr ((ord a)+5)] ++ cripe b

cripf::String->String
cripf [] =[]
cripf (a:b) | ((ord a)+6)> (ord 'z') = [chr ((ord a)-20)] ++ cripf b
			| a == ' ' = [a] ++ cripf b
			| otherwise = [chr ((ord a)+6)] ++ cripf b
			
cripg::String->String
cripg [] =[]
cripg (a:b) | ((ord a)+7)> (ord 'z') = [chr ((ord a)-19)] ++ cripg b
			| a == ' ' = [a] ++ cripg b
			| otherwise = [chr ((ord a)+7)] ++ cripg b

criph::String->String
criph [] =[]
criph (a:b) | ((ord a)+8)> (ord 'z') = [chr ((ord a)-18)] ++ criph b
			| a == ' ' = [a] ++ criph b
			| otherwise = [chr ((ord a)+8)] ++ criph b

cripi::String->String
cripi [] =[]
cripi (a:b) | ((ord a)+9)> (ord 'z') = [chr ((ord a)-17)] ++ cripi b
			| a == ' ' = [a] ++ cripi b
			| otherwise = [chr ((ord a)+9)] ++ cripi b
