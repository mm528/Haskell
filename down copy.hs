{-# LANGUAGE OverloadedStrings , DeriveGeneric #-} 
module Main where

import Happstack.Server(Method(GET),dir,toResponse,ok,simpleHTTP,nullConf,ServerPart,Response,method,seeOther)
import Control.Monad (msum) 
import Control.Applicative
import 	Data.Aeson as W
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Blaze.Html5 as H hiding (map, title)
import Text.Blaze.Html5.Attributes as H hiding (dir, method, map)
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)

main :: IO ()
main = simpleHTTP nullConf $  msum [ dir "students" $ ok getlist
									,dir "hello" $ ok "hello"
									,seeOther ("/students"::String) getlist
		

	]

jsonURL :: String
jsonURL = "http:://phoric.eu/temperature.json"


getJSON :: IO C.ByteString
getJSON = simpleHttp jsonURL

{--
gets :: IO(Maybe[Students])
gets = do
	json<- Data.ByteString.Lazy.readFile "Desktop/haskell/students.json"
	let students = decode json
	return students
}
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
data Students = Students {
		name :: String,
		sid  :: String,
	 	login :: String
}deriving (Generic,Show)


getlist :: String
getlist = (C.unpack (encode getStudents))





getStudents :: [Students]
getStudents = [(Students "Michalis Motis" "mjk" "mm528"),(Students "Andrew Jackson" "akjs" "adik2")]
	
instance ToJSON Students where
	toJSON (Students n s l) = W.object ["name" .= n,
					 "sid" .= s,
					 "login".=l ]

{-

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




