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



funcioncapabi x = do
                        let list = listacapability x
                        if(list/=[]) then createcapability list
                        else Capability "" ""

funciongroup x = do
                        let list = listagroup x
                        if(list/=[]) then creategroup list
                        else Group ""
--Función que crea un Device        
funciondevice::[String]        -> Device        
funciondevice        x = do
                        let list = listadevice x
                        if(list/=[]) then createdevice list
                        else Device "" "" ""
                


                


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
        