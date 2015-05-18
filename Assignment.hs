{-# LANGUAGE OverloadedStrings , DeriveGeneric #-} 
module Main where

import Happstack.Server(Method(GET),dir,seeOther,toResponse,ok,simpleHTTP,nullConf,ServerPart,Response,method)
import Control.Monad (msum) 
import Control.Applicative ((<$>))
import 	Data.Aeson as W
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Blaze.Html5 as H  hiding (map) 
import Text.Blaze.Html5.Attributes as A hiding (dir, method, map)
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import System.IO
import Data.Monoid
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html5 hiding (html,param)
--import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Renderer.Utf8(renderHtml)

import Snap.Core (MonadSnap (..), addHeader, modifyResponse, writeLBS)
import Control.Monad.Identity (Identity(runIdentity))
import Data.String (IsString(fromString))
import Data.Text  (Text)
import Text.Blaze.Html5 ((!))
import Prelude as R
import qualified Data.Text.Lazy as T 
import qualified Data.Map as G
--import Clay as N


{--
Example from lecture and labs. Get the idea to prsent on a blazer
main :: IO ()
main = simpleHTTP nullConf $  msum [ dir "students" $ ok getlist
									,dir "hello" $ ok "hello"
									,seeOther ("/students"::String) getlist
		

	]

--}

main :: IO()
main = do 
	d<- (eitherDecode <$> getJSON):: IO(Either String Temperatures) --gets the json from the URL

	case d of
		Left err -> putStrLn "ERROR"
		

		Right ps -> do
					hdl <- openFile "/Users/michalis/Desktop/haskell/mytemp.txt" WriteMode
					hPutStr hdl getDetailList

					hClose hdl
					

					--print $ fileSize "/Users/michalis/Desktop/haskell/mytemp.txt"
					--(Reding File)contents <- readFile "/Users/michalis/Desktop/haskell/mytemp.txt" 
					--putStr contents 

					

					--Print on the termilan the list of the blaze


					h <- print $ renderHtml $ blaze (getChar1" ") "My blog" ["Test1","Test2","Test3"]
					--simpleHTTP nullConf $  msum [ dir "Temperatures" $ ok getDetailList
					--								,dir "Specify" $ ok "hello"
					--							, seeOther ("/Temperatures"::String) getDetailList]
					--	

			
			
				
				--	withFile "/Users/michalis/Desktop/haskell/mytemp.txt" ReadWriteMode $ \handle -> do
				--		content <- hGetContents handle
				--	let	content_lines = T.lines content

				

				-- Create direction and print the lists

					simpleHTTP nullConf $ msum [
											 dir "temparature" $  helloBlaze,
											 dir "talo" $ ok $ getText,
											dir "single" $ ok$  getText2,--Should bring back the integer of the list

												seeOther ("/temparature"::String)  getText  --(IsString problem) follow crash course happstack details and its doens't work
											 ]
												
					

					--simpleHTTP nullConf $ helloBlaze							
					print ps 
	

getString :: String -> [String]
getString [] = []
getString  x = [x] 

{-put in a local file-}
putFile :: [String]-> IO()
putFile x = writeFile "Desktop/haskell/mytemp.txt" $ unwords $ getString "hello"




getDetails :: Details -> String
getDetails k  = show k



{-download the information from url and make to ByteString-}

getJSON :: IO C.ByteString
getJSON  = simpleHttp url

{-Specify the url . Could be done in getJson but split it into 2 functions-}

url :: String
url = "http://www.phoric.eu/temperature.json"



{-get the list and bring back the second element of the list. I can able to find the average of teh Temperatures-}
getTemp :: [Details] -> Integer
getTemp [(Details _ t)] = fromIntegral t



{-- Trying to get json from my file



gets :: IO(Maybe[Students])
gets = do
	json<- Data.ByteString.Lazy.readFile "Desktop/haskell/students.json"
	let students = decode json
	return students
--}
{--
main :: IO ()
main = simpleHTTP nullConf $ msum 
		[dir "student" $ do method 
					ok $ getlist
		
		]

--dir "message" $ do method GET
						-- ok "goodbye"



--}


{-Temperatures Data which has to split into list so we can have the date and the Temperature-}
data Temperatures = Temperatures {temperatures :: [Details]

								}deriving (Generic,Show)

data Details = Details { date :: String , temperature :: Int
						
						} deriving (Generic, Show)

getDetailsExample :: [Details]
getDetailsExample = [(Details "2015-02-28T20:16:12+00:00" 0),
						(Details "2015-01-01T21:46:55+00:00" 2),
						(Details "2015-04-08T21:46:53+00:00" 3),
						(Details "2015-04-09T21:46:01+00:00" 4),
						(Details "2015-04-10T21:46:40+00:00" 5),
						(Details "2015-04-11T21:46:36+00:00" 6),
						(Details "2015-04-12T20:36:25+00:00" 7)]				





instance ToJSON Details	where
			toJSON (Details m k) = W.object ["Date".= m,
											"temper".=k]
instance ToJSON Temperatures
instance FromJSON Details					
instance FromJSON Temperatures



{-Get the actual data from the list-}
getDetailList :: String
getDetailList = (C.unpack (encode getDetailsExample))

{-Give a forma of our blaze. return in into Html and actual print into the screen-}

blaze:: String -> Html -> [Html] -> Html
blaze title headers body = 
	H.html $ do
		H.head $
			H.title (H.toHtml headers)
		
		H.body $ do
				
					H.p $ do 
							(H.b "Welcome to My Page Temperatures")
							(H.p "")
							(H.toHtml title) 
							--(H.toHtml body)

							(H.p "")
						
						--	(H.input ! A.type_ "text")
						--	(inputSumbit "hello")
							
							
					
							(H.a "Single Temperature"!A.href "http://localhost:8000/talo")




blaze2:: String -> Html -> [Html] -> Html
blaze2 title headers body = 
	H.html $ do
		H.head $
			H.title (H.toHtml headers)
		
		H.body $ do
				
					H.p $ do 
							(H.toHtml title)



blaze3:: String -> Integer -> Html -> [Html] -> Html
blaze3 title k headers body = 
	H.html $ do
		H.head $
			H.title (H.toHtml headers)
		
		H.body $ do
				
					H.p $ do 
							(H.toHtml title)
							(H.toHtml k)							 
							





{-Give a Response to the file that localhost can be accept the Details-}


helloBlaze :: ServerPart Response
helloBlaze =
	ok $ toResponse $
	 blaze (getChar1 " ") "My Temp" ["Michalis","test1","Test2"]	


getText :: Response
getText = 
	toResponse $ 
				blaze2 (getChar2" ") "My Temp" ["Michalis","bar","fizz"]	


getText2 :: Response
getText2 =
	toResponse $
				
	blaze3 (getChar2 " " ) (getTemp getDetailsExample) "My Temp"  ["Michalis","bar","fizz"]	

changeToChar2 :: [Details] -> [Char]
changeToChar2 [] = []
changeToChar2 p@(k:ks)= show k++ "\n"++ changeToChar2 ks

{-Getting information from the file and return it to ByteStrings
-}


{-Getting the list Details(Temperatures) and return everything as a character
-}

changeToChar :: [Details] -> [Char]
changeToChar [] = []
changeToChar (k:ks)= "Temperature is  : " ++"\n"++ C.unpack(encode k)
	
{-
Geting from the list as character. now be able to return on the blaze
-}	
getChar2 :: [Char] -> [Char]
getChar2 [] = [] 
getChar2 l = changeToChar getDetailsExample
{-
Geting from the list as character now ad be able to return on the blaze in different directory
-}	

getChar1 :: [Char]-> [Char]
getChar1 [] = []
getChar1 k = changeToChar2 getDetailsExample


--aver:: [Details] -> Int
--aver (x:xs) =  length x
{-
Find the size of the File
-}

fileSize :: FilePath -> IO Integer
fileSize path = withFile path ReadMode hFileSize








{--
inputSumbit :: Text -> H.Html
inputSumbit value = H.input
	!A.type_ "sumbit"
	!A.value (H.toValue value)

-}

{--


helloexample :: ServerPart IO XML
hello = unHSPT $ unXMLGenT
	<html>
	<head>
		<title> Hello,HSP! </title>
	</head>
		<body>
			<h1> Hello Michali </h1>
		</body>
	</html>
	where
			getMethod :: XMLGenT (HSPT XML (ServerPart IO)) String
			getMethod = show. rqMethod <$> askRq


--}





{-

instance Applicative (P s) where
  pure = return
  (<*>) = ap



instance Alternative (P s) where
      empty = mzero
      (<|>) = mplus
-}


