import Data.Char

--os testes 1 são o teste mais básico, no quarl passo uma palavra, e coloco o retorno correto, comparando o acerto, caso esteja certo Retorna True, Caso não False
--os testes 2 são para testa como lida com a condição do z, tendo em vista que voltaria para o começo da lista, caso esteja certo Retorna True, Caso não False
--os testes 2 são para testar se a condição do " " está correta,  caso esteja certo Retorna True, Caso não False
--os testes 4 é algo maior para comprovar que está correto caso esteja certo Retorna True, Caso não False
unitteste = [cripjteste, cripkteste, criplteste, cripmteste, cripnteste, cripoteste, crippteste, cripqteste]

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
-------------------------------------------
cripjteste1 :: Bool
cripjteste1 | cripj "abcd" == "klmn" = True
			| otherwise = False

cripjteste2 :: Bool
cripjteste2 | cripj "zaap" == "jkkz" = True
			| otherwise = False

cripjteste3 :: Bool
cripjteste3 | cripj "eu amo" == "oe kwy" = True
			| otherwise = False

cripjteste4 :: Bool
cripjteste4 | cripj "eu hoje estou com fome" == "oe ryto ocdye myw pywo" = True
			| otherwise = False	

cripjteste :: Bool
cripjteste | [cripjteste1, cripjteste2, cripjteste3,cripjteste4] == [True,True,True,True] = True
		   | otherwise = False 
----------------------------------------------
cripkteste1 :: Bool
cripkteste1 | cripk "abcd" == "lmno" = True
			| otherwise = False

cripkteste2 :: Bool
cripkteste2 | cripk "zaap" == "klla" = True
			| otherwise = False

cripkteste3 :: Bool
cripkteste3 | cripk "eu amo" == "pf lxz" = True
			| otherwise = False

cripkteste4 :: Bool
cripkteste4 | cripk "eu hoje estou com fome" == "pf szup pdezf nzx qzxp" = True
			| otherwise = False	
cripkteste :: Bool
cripkteste | [cripkteste1, cripkteste2, cripkteste3,cripkteste4] == [True,True,True,True] = True
		   | otherwise = False

-----------------------------------------------
criplteste1 :: Bool
criplteste1 | cripl "abcd" == "mnop" = True
			| otherwise = False

criplteste2 :: Bool
criplteste2 | cripl "zaap" == "lmmb" = True
			| otherwise = False

criplteste3 :: Bool
criplteste3 | cripl "eu amo" == "qg mya" = True
			| otherwise = False

criplteste4 :: Bool
criplteste4 | cripl "eu hoje estou com fome" == "qg tavq qefag oay rayq" = True
			| otherwise = False	

criplteste :: Bool
criplteste | [criplteste1, criplteste2, criplteste3,criplteste4] == [True,True,True,True] = True
		   | otherwise = False
			
------------------------------------------------
cripmteste1 :: Bool
cripmteste1 | cripm "abcd" == "nopq" = True
			| otherwise = False

cripmteste2 :: Bool
cripmteste2 | cripm "zaap" == "mnnc" = True
			| otherwise = False

cripmteste3 :: Bool
cripmteste3 | cripm "eu amo" == "rh nzb" = True
			| otherwise = False

cripmteste4 :: Bool
cripmteste4 | cripm "eu hoje estou com fome" == "rh ubwr rfgbh pbz sbzr" = True
			| otherwise = False	

cripmteste :: Bool
cripmteste | [cripmteste1, cripmteste2, cripmteste3,cripmteste4] == [True,True,True,True] = True
		   | otherwise = False
-----------------------------------------------
cripnteste1 :: Bool
cripnteste1 | cripn "abcd" == "opqr" = True
			| otherwise = False

cripnteste2 :: Bool
cripnteste2 | cripn "zaap" == "nood" = True
			| otherwise = False

cripnteste3 :: Bool
cripnteste3 | cripn "eu amo" == "si oac" = True
			| otherwise = False

cripnteste4 :: Bool
cripnteste4 | cripn "eu hoje estou com fome" == "si vcxs sghci qca tcas" = True
			| otherwise = False	
cripnteste :: Bool
cripnteste | [cripnteste1, cripnteste2, cripnteste3,cripnteste4] == [True,True,True,True] = True
		   | otherwise = False
			
-------------------------------------------------
cripoteste1 :: Bool
cripoteste1 | cripo "abcd" == "pqrs" = True
			| otherwise = False

cripoteste2 :: Bool
cripoteste2 | cripo "zaap" == "oppe" = True
			| otherwise = False

cripoteste3 :: Bool
cripoteste3 | cripo "eu amo" == "tj pbd" = True
			| otherwise = False

cripoteste4 :: Bool
cripoteste4 | cripo "eu hoje estou com fome" == "tj wdyt thidj rdb udbt" = True
			| otherwise = False	

cripoteste :: Bool
cripoteste | [cripoteste1, cripoteste2, cripoteste3,cripoteste4] == [True,True,True,True] = True
		   | otherwise = False
			
-------------------------------------------------
crippteste1 :: Bool
crippteste1 | cripp "abcd" == "qrst" = True
			| otherwise = False

crippteste2 :: Bool
crippteste2 | cripp "zaap" == "pqqf" = True
			| otherwise = False

crippteste3 :: Bool
crippteste3 | cripp "eu amo" == "uk qce" = True
			| otherwise = False

crippteste4 :: Bool
crippteste4 | cripp "eu hoje estou com fome" == "uk xezu uijek sec vecu" = True
			| otherwise = False	
			
crippteste :: Bool
crippteste | [crippteste1, crippteste2, crippteste3,crippteste4] == [True,True,True,True] = True
		   | otherwise = False
			
-------------------------------------------------
cripqteste1 :: Bool
cripqteste1 | cripq "abcd" == "rstu" = True
			| otherwise = False

cripqteste2 :: Bool
cripqteste2 | cripq "zaap" == "qrrg" = True
			| otherwise = False

cripqteste3 :: Bool
cripqteste3 | cripq "eu amo" == "vl rdf" = True
			| otherwise = False

cripqteste4 :: Bool
cripqteste4 | cripq "eu hoje estou com fome" == "vl yfav vjkfl tfd wfdv" = True
			| otherwise = False	
			
cripqteste :: Bool
cripqteste | [cripqteste1, cripqteste2, cripqteste3,cripqteste4] == [True,True,True,True] = True
		   | otherwise = False			

				  
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

cripj::String->String
cripj [] =[]
cripj (a:b) | ((ord a)+10)> (ord 'z') = [chr ((ord a)-16)] ++ cripj b
			| a == ' ' = [a] ++ cripj b
			| otherwise = [chr ((ord a)+10)] ++ cripj b
	
cripk::String->String
cripk [] =[]
cripk (a:b) | ((ord a)+11)> (ord 'z') = [chr ((ord a)-15)] ++ cripk b
			| a == ' ' = [a] ++ cripk b
			| otherwise = [chr ((ord a)+11)] ++ cripk b
			
cripl::String->String
cripl [] =[]
cripl (a:b) | ((ord a)+12)> (ord 'z') = [chr ((ord a)-14)] ++ cripl b
			| a == ' ' = [a] ++ cripl b
			| otherwise = [chr ((ord a)+12)] ++ cripl b
			
cripm::String->String
cripm [] =[]
cripm (a:b) | ((ord a)+13)> (ord 'z') = [chr ((ord a)-13)] ++ cripm b
			| a == ' ' = [a] ++ cripm b
			| otherwise = [chr ((ord a)+13)] ++ cripm b

cripn::String->String
cripn [] =[]
cripn (a:b) | ((ord a)+14)> (ord 'z') = [chr ((ord a)-12)] ++ cripn b
			| a == ' ' = [a] ++ cripn b
			| otherwise = [chr ((ord a)+14)] ++ cripn b
			
cripo::String->String
cripo [] =[]
cripo (a:b) | ((ord a)+15)> (ord 'z') = [chr ((ord a)-11)] ++ cripo b
			| a == ' ' = [a] ++ cripo b
			| otherwise = [chr ((ord a)+15)] ++ cripo b
			
cripp::String->String
cripp [] =[]
cripp (a:b) | ((ord a)+16)> (ord 'z') = [chr ((ord a)-10)] ++ cripp b
			| a == ' ' = [a] ++ cripp b
			| otherwise = [chr ((ord a)+16)] ++ cripp b
			
cripq::String->String
cripq [] =[]
cripq (a:b) | ((ord a)+17)> (ord 'z') = [chr ((ord a)-9)] ++ cripq b
			| a == ' ' = [a] ++ cripq b
			| otherwise = [chr ((ord a)+17)] ++ cripq b

