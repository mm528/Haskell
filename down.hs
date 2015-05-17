{-# LANGUAGE OverloadedStrings , DeriveGeneric #-} 
module Main where

import Happstack.Server(Method(GET),dir,toResponse,ok,simpleHTTP,nullConf,ServerPart,Response,method,seeOther)
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
main :: IO ()
main = simpleHTTP nullConf $  msum [ dir "students" $ ok getlist
									,dir "hello" $ ok "hello"
									,seeOther ("/students"::String) getlist
		

	]

--}

main :: IO()
main = do 
	d<- (eitherDecode <$> getJSON):: IO(Either String Temperatures)

	case d of
		Left err -> putStrLn "ERROR"
		

		Right ps -> do
					hdl <- openFile "/Users/michalis/Desktop/haskell/mytemp.txt" WriteMode
					hPutStr hdl getDetailList

					hClose hdl
					

					--print $ fileSize "/Users/michalis/Desktop/haskell/mytemp.txt"
					--(Reding File)contents <- readFile "/Users/michalis/Desktop/haskell/mytemp.txt" 
					--putStr contents 
					h <- print $ renderHtml $ gen (pisw" ") "My blog" ["Test1","Test2","Test3"]
					--simpleHTTP nullConf $  msum [ dir "Temperatures" $ ok getDetailList
					--								,dir "Specify" $ ok "hello"
					--							, seeOther ("/Temperatures"::String) getDetailList]
					--	

					--putCss $ 
					--		N.body ?
					--			do background re


				--	withFile "/Users/michalis/Desktop/haskell/mytemp.txt" ReadWriteMode $ \handle -> do
				--		content <- hGetContents handle
				--	let	content_lines = T.lines content

				


					simpleHTTP nullConf $ msum [
											 dir "temparature" $  helloBlaze,
											 dir "talo" $ getText

											-- seeOther ("/talo"::String) getText
											 ]
												
					

					--simpleHTTP nullConf $ helloBlaze							
					print ps 
	

getString :: String -> [String]
getString [] = []
getString  x = [x] 


putFile :: [String]-> IO()
putFile x = writeFile "Desktop/haskell/mytemp.txt" $ unwords $ getString "hello"




getDetails :: Details -> String
getDetails k  = show k




getJSON :: IO C.ByteString
getJSON  = simpleHttp url


url :: String
url = "http://www.phoric.eu/temperature.json"
{--
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


--dir Students : localhost::8000/Student
{--
listStudents :: ServerPart Response
listStudents :: encodeJSONArray $ ok $mkIndex $ toResponse
--}
{--
main:: IO ()
main = putStr $ show json
	where json = encode getStudents

--}
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




getlist :: String
getlist = (C.unpack (encode getStudents))

getDetailList :: String
getDetailList = (C.unpack (encode getDetailsExample))



getStudents :: [Students]
getStudents = [(Students "Michalis Motis" "mjk" "mm528"),(Students "Andrew Jackson" "akjs" "adik2")]
	
instance ToJSON Students where
	toJSON (Students n s l) = W.object ["name" .= n,
					 "sid" .= s,
					 "login".=l ]


data Students = Students {
		name :: String,
		sid  :: String,
	 	login :: String
}deriving (Generic,Show)




gen:: String -> Html -> [Html] -> Html
gen title headers body = 
	H.html $ do
		H.head $
			H.title (H.toHtml headers)
		
		H.body $ do
				
					H.p $ do 

							(H.toHtml title) 
							--(H.toHtml body)
							(H.p "")
							(H.button "click "! A.href "http://localhost:8000/talo")
						--	(H.input ! A.type_ "text")
						--	(inputSumbit "hello")
							(option "www.google.com")
							
							(input ! type_ "Text" ! A.href "www.goog")
							(H.b "hello"!A.href "http://localhost:8000/talo")




gen2:: String -> Html -> [Html] -> Html
gen2 title headers body = 
	H.html $ do
		H.head $
			H.title (H.toHtml headers)
		
		H.body $ do
				
					H.p $ do 
							(H.toHtml title)

							
							--(inputSumbit "hello")









helloBlaze :: ServerPart Response
helloBlaze =
	ok $ toResponse $
	gen (pisw" ") "My Temp" ["Michalis","bar","fizz"]	


getText :: ServerPart Response
getText = 
	ok $ toResponse $ 
				gen2 (pisw2" ") "My Temp" ["Michalis","bar","fizz"]	

{--
malakia2 :: ServerPart Response
malakia2 = 
	ok $ toResponse $
		malakia [(Details "2015-02-28T20:16:12+00:00" 0),
						(Details "2015-01-01T21:46:55+00:00" 2),
						(Details "2015-04-08T21:46:53+00:00" 3),
						(Details "2015-04-09T21:46:01+00:00" 4),
						(Details "2015-04-10T21:46:40+00:00" 5),
						(Details "2015-04-11T21:46:36+00:00" 6),
						(Details "2015-04-12T20:36:25+00:00" 7)]	

		--}			

malakia :: [Details] -> [Char]
malakia [] = []
malakia p@(k:ks)= show k++ "\n"++ malakia ks

{-Getting information from the file and return it to ByteStrings
-}


{-Getting the list Details(Temperatures) and return everything as a character
-}

malakia2 :: [Details] -> [Char]
malakia2 [] = []
malakia2 (k:ks)= "Temperature is  : " ++"\n"++ C.unpack(encode k)
	
{-
Geting from the list as character now ad be able to return on the blaze
-}	
pisw2 :: [Char] -> [Char]
pisw2 [] = [] 
pisw2 l = malakia2 getDetailsExample
{-
Geting from the list as character now ad be able to return on the blaze in different directory
-}	

pisw :: [Char]-> [Char]
pisw [] = []
pisw k = malakia getDetailsExample


aver:: [Details] -> Details
aver (k:y:ys) =  y
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





{--
helloBlaze :: ServerPart Response
helloBlaze = 
	ok $ toResponse $
		gen "Michalis Motis"
				H.p "Hello"
						[H.p $ do "hello,"
								H.b "blaze-html"]



--}
{--
data lists = lists{
	stud :: [Students]
}deriving (Eq,Show)

instance FromJSON lists where
	parseJSON(Object o) = do
		stud <- parseJSON =<< (o .:"stud")
	return $ Students stud
	parseJSON _ = mzero

instance FromJSON Students where
	parseJSON (Object v) = Students <$>
				v.:"name"<*>
				v.:"sid"<*>
				v.:"login"<*>

mkIndex :: H.Html
mkIndex = H.docTypeHtml $ do
		H.head $ do
			H.title "Michael"
		H.body $ do 
			"evil mixing of business"++ "miaouu"



toResponse :: ToMessage a => a -> Response
instance ToMessage Blaze.Html where
	toContentType _= B.pack
			"text/html; charset=UTF−8"
	toMessage	= Blaze.renderHtml


-}




{-

instance Applicative (P s) where
  pure = return
  (<*>) = ap



instance Alternative (P s) where
      empty = mzero
      (<|>) = mplus
-}


