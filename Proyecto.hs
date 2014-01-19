import Data.List.Split
import Data.Char(toUpper)
import Data.List
import Data.Char
import System.IO
import System.Exit

data Dispositivo = Dispositivo{ 
					   id :: String,
                       user :: String,
                       fall_back :: String
                     } deriving (Eq,Show,Read)
                                        
data Grupo = Grupo { 
						id :: String
                     } deriving (Eq,Show,Read)

data Capab = Capab { 
						name :: String,
						value :: String
                  } deriving (Eq,Show,Read)
                                        
data XML =XML {
					device :: Device,
                    group :: Group,
					capa:: Capability
               }deriving (Eq,Show,Read)
                                        

leerXML :: FilePath -> IO [String]
leerXML file = do        
                                final <- readFile file
                                return (lines final)
                                

showDevice :: Dispositivo-> String
showDevice d = "id = "++(id d)++" user_agent= "++(user d)++" fall_back = "++(fall_back d)


idDeviceService:: Dispositivo -> String
idDeviceService d = (id d)


getElementByFallBack:: Dispositivo -> String
getElementByFallBack d = (fall_back d)

showGroup :: Grupo-> String
showGroup g= "id del Grupo es = "++(id g)


idGroupService:: Grupo-> String
idGroupService g = (id g)

showCapabilitySys :: Capab -> String
showCapabilitySys c= "name= "++(name c)++" value= "++(value c)

nameCapabilityOrder:: Capab -> String
nameCapabilityOrder c= (name c)

getDeviceGroup :: XML -> Dispositivo
getDeviceGroup xml =do
                        (device xml)
						
getGroup :: XML -> Grupo
getGroup g =do
                        (group g)
                        

getCapability :: XML -> Capab
getCapability g =do
                        (capa g)

shellSpace ::[String]->[String]
shellSpace []=[]
shellSpace (x:xs)= do
        if x==""
        then []++shellSpace xs
        else [x]++shellSpace xs
        

listcapabilityShow[]=[]
listcapabilityShow (x:xs) = do
                if (x=="name") then do
                        [head xs]++listcapabilityShow xs
                else if (x=="value") then do
                        if (xs==[]) then do
                                xs++["none"]
                                else [head xs]++listcapabilityShow xs
                else listcapabilityShow xs


listgroupShow[]=[]
listgroupShow (x:xs) = do
                if (x=="id") then do
                        [head xs]++listgroupShow xs
                else listgroupShow xs


listdeviceShow ::[String] -> [String]        
listdeviceShow[]=[]
listdeviceShow (x:xs) = do
                if (x=="id") then do
                        [head xs]++listdeviceShow xs
                else if (x=="user_agent")then do
                        if(head xs == "fall_back") then do
                                "none": tail xs
                        else [head xs]++listdeviceShow xs
                else if (x=="fall_back") then do
                        if(head xs == "fall_back") then do
                                "none": tail xs
                        else [head xs]++listdeviceShow xs
                else listdeviceShow xs
                        

newdeviceEntry ::[String] -> Dispositivo
newdeviceEntry [] = Dispositivo "" "" ""
newdeviceEntry (x:y:zs) = do
        Dispositivo x y (head zs)


newcapabilityEntry [] = Capab "" ""
newcapabilityEntry (x:ys) = do
        Capab x (head ys)


newgroupEntry [] = Grupo ""
newgroupEntry (x:ys) = do
        Grupo x

newXMLEntry ::Dispositivo->Grupo->Capab->XML
newXMLEntry (Dispositivo "" "" "") (Grupo "") (Capab "" "")= XML (Dispositivo "" "" "") (Grupo "") (Capab "" "")
newXMLEntry device group capa = do
        XML device group capa


capabilityFntnMain :: [String]-> Capab
capabilityFntnMain capaMain = do
                        let lista = listcapabilityShow capaMain
                        if(lista/=[]) then newcapabilityEntry lista
                        else Capab "" ""
groupFntnMain :: [String]-> Grupo
groupFntnMain groupMain = do
                        let lista = listgroupShow groupMain
                        if(lista/=[]) then newgroupEntry lista
                        else Grupo ""

deviceFntnMain::[String] -> Dispositivo
deviceFntnMain   deviceMain = do
                        let list = listdeviceShow deviceMain
                        if(list/=[]) then newdeviceEntry list
                        else Dispositivo "" "" ""
                

parseXML' :: String -> [XMLAST]
parseXML' str =
  f ast where
      ast = parse ((many innerXML)) "" str
      f (Right x) = x
      f (Left x) = [CouldNotParse (show x)]

parseXML :: String -> XMLAST
parseXML str =
  f ast where
      ast = parse (spaces >> xmlParser) "" str
      f (Right x) = x
      f (Left x) = CouldNotParse (show x)
      
xmlParser :: Parser XMLAST
xmlParser =
  try withoutExplictCloseTag <|> withExplicitCloseTag


withExplicitCloseTag :: Parser XMLAST
withExplicitCloseTag =
  do
    (name, attr) <- openTag
    innerXML <- many innerXML
    closeTag name
    return (Element name attr innerXML)

innerXML = comment <|> schema <|> xmlParser <|> parseBody

parseBody = fmap Body $ many1 $ noneOf "<>"

schema :: Parser XMLAST
schema =
  do
    try $ string "<!"
    body <- manyTill anyChar (string ">")
    return (Schema body)

comment :: Parser XMLAST
comment =
  do
    try $ string "<!--"
    body <- manyTill anyChar (string "-->")
    return (Comment body)

openTag :: Parser (String, [(String,String)])
openTag =
  do
    try $ char '<' >> notFollowedBy (char '/')
    tag <- many (letter <|> digit)
    spaces
    a <- try (many keyValue)
    char '>'
    return (tag, a)

closeTag :: String -> Parser ()
closeTag str =
  do
    try $ string "</"
    spaces
    string str
    spaces
    char '>'
    return ()

withoutExplictCloseTag :: Parser XMLAST
withoutExplictCloseTag =
  do
    try $ char '<' >> notFollowedBy (char '/')
    name <- many (letter <|> digit)
    spaces
    a <- try (many keyValue)
    spaces
    string "/>"
    return (Element name a [])

keyValue :: Parser (String, String)
keyValue =
  do
    key <- many1 (letter <|> digit <|> char '-')
    spaces
    char '='
    spaces
    value <- quotedString
    spaces
    return (key, value)

quotedString :: Parser String
quotedString = do
  q <- (try (char '"')) <|> char '\''
  value <- fmap concat $ many
    $ many1 (noneOf ['\\', q])
      <|> try (string ['\\', q])
      <|> try (string "\\")
  char q
  return value

 producter :: [String]->Dispositivo->Grupo->Capab->[XML]
 producter [] _ _ _ = []
 producter (w:ws) de gro cap = do
	let m = splitOneOf(("<>=/ \\\"") x
    let fileWSp= shellSpace m
    let lista=[]
        if (head fileWSp) == "device" then do
                        let dispositivom = deviceFntnMain $ tail fileWSp
                        let xml = newXMLEntry dispositivom gro cap
                        [xml]++producter ws dispositivom gro cap
        else if (head fileWSp) == "group" then do
                        let grop = groupFntnMain $ tail fileWSp
                        let xml = newXMLEntry de grop cap
                        [xml]++producter ws de grop cap
        else if (head fileWSp) == "capability" then do
                        let cab = capabilityFntnMain $ tail fileWSp
                        let xml = newXMLEntry de gro cab
                        [xml]++producter ws de gro cab
                        
        else lista++producter ws de gro cap

getAllElements :: XMLAST -> [(XMLAST, String, XMLAST)]
getAllElements ast = getAllElements' ast "" ast
getAllElements' pe pp element@(Element n a es) = concat $ map (getAllElements' element (pp ++ "/" ++ n)) es
getAllElements' pe pp x = [(pe, pp, x)]

getElementsByName :: String -> XMLAST -> [(XMLAST, String, XMLAST)]
getElementsByName str ast = filter (\e -> f e) (getAllElements ast) where
                  f ((Element n _ _), _, _) = n == str
                  f _ = False

getElementsByPath :: String -> XMLAST -> [(XMLAST, String, XMLAST)]
getElementsByPath str ast = filter (\e -> f e) (getAllElements ast) where
                  f (_ , p, _) = p == str


getAllBodies :: XMLAST -> [(String, String)]
getAllBodies = getAllBodies' "" where
  getAllBodies' :: String -> XMLAST -> [(String, String)]
  getAllBodies' p (Body str) = [(p, str)]
  getAllBodies' p (Element n a es) =
               let v2 = concat $ map (getAllBodies' (fixUp p n)) es
                   fixUp x y = x ++ "/" ++ y
               in v2
  getAllBodies' p _ = []

getBodiesByName :: String -> XMLAST -> [String]
getBodiesByName name xmlast= map snd $ filter (\(n,v) -> n == name) (getAllBodies xmlast)
                


lista :: [String]->[Grupo]
lista (x:xs) = do
                                if isInfixOf "<devices>" x then do
                                        prodt xs (Device "" "" "") (Group "") (Capability "" "")
                                else
                                        lista xs
                                       
main = do
        archivo <- leerXML "wurfl23.xml"
        putStrLn "Bienvenido Usuario"
        opc <- getLine
        buscar opc lista
        