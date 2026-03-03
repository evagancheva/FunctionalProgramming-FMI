module XMLParser(Element,processXml,trim) where

import Parser (Parser(),many,label, oneOf,sat,many1,between,but, Parseable,parser,run,ParseError (UnexpectedSymbol))
import Control.Applicative (Alternative(empty))
import qualified Control.Applicative as App
import State
import qualified Data.Map as Map

type Attribute = (String,String)
data Element = Element {
    tag :: String,
    attributes :: [Attribute],
    childrens :: [Element],
    text :: String,
    uid :: String
}
    deriving (Show,Eq)

spaces :: Parser String
spaces = many $ oneOf " \n\t\r"

validChars :: [Char]
validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

getId :: Parser String
getId = many1 $ sat (\c-> c `elem` validChars)

textUntil :: Char -> Parser String
textUntil stop = many (but (== stop))

attribute :: Parser Attribute
attribute = do
    name <- getId
    _ <- spaces
    _ <- sat (== '=')
    _ <- spaces
    value <- between (sat (== '"')) (textUntil '"') (sat (== '"'))
    pure (name, value)

inBrackets :: Parser a -> Parser a
inBrackets p = between (sat (== '<')) p (sat (== '>'))

openTag :: Parser (String, [Attribute])
openTag = inBrackets $ do
    name <- getId
    attrs <- many $ do
        _ <- spaces
        attribute
    _ <- spaces
    pure (name, attrs)

closeTag :: String -> Parser String
closeTag name = inBrackets $ do
    _ <- sat (== '/')
    closingName <- getId
    if closingName == name
       then do
           _ <- spaces
           pure closingName
       else
           label (UnexpectedSymbol closingName) empty

children :: Parser [Element]
children = many (between spaces element spaces)

extractId :: [Attribute] -> (String, [Attribute])
extractId attrs =
    let val = case lookup "id" attrs of
                Just v  -> v
                Nothing -> ""
    in (val, attrs)

element :: Parser Element
element = do
    _ <- spaces
    (name, attrs) <- openTag
    let (extractedUid, cleanAttrs) = extractId attrs

    txtBefore <- textUntil '<'
    childs <- children
    txtAfter <- if not (null childs)
                then textUntil '<'
                else pure ""

    _ <- spaces
    _ <- closeTag name
    _ <- spaces

    let combinedText = trim (txtBefore ++ txtAfter)

    pure $ Element name cleanAttrs childs combinedText extractedUid

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile (`elem` " \n\t\r")

type FullState = (Int, Map.Map String Int)

asInt :: String -> Maybe Int
asInt s =
    if all (`elem` ['0'..'9']) s && not (null s)
       then Just (read s)
       else Nothing

collectIds :: Element -> [String]
collectIds el =
    let own = if uid el /= "" then [uid el] else []
    in own ++ concatMap collectIds (childrens el)

initialCounter :: Element -> Int
initialCounter root =
    let ids = collectIds root
        nums = [ n | Just n <- map asInt ids ]
    in if null nums then 1 else maximum nums + 1

assignUniqueId :: Element -> State FullState Element
assignUniqueId el = do
    (counter, used) <- get
    let original = uid el

    (newUid, newUsed, newCounter) <-
        if original /= "" then
            case Map.lookup original used of
                Nothing ->
                    let counter' =
                            case asInt original of
                                Just n  -> max counter (n + 1)
                                Nothing -> counter
                    in pure (original, Map.insert original 1 used, counter')

                Just n ->
                    let u = original ++ "_" ++ show n
                    in pure (u, Map.insert original (n+1) used, counter)

        else
            let u = show counter
            in pure (u, used, counter + 1)

    put (newCounter, newUsed)

    newChildren <- mapM assignUniqueId (childrens el)

    pure el { uid = newUid, childrens = newChildren }

processXml :: Element -> Element
processXml root =
    let start = initialCounter root
    in evalState (assignUniqueId root) (start, Map.empty)

runFinal :: String -> Either ParseError Element
runFinal input = fmap processXml (run input)

instance Parseable Element where
    parser = element

indent :: Int -> String
indent n = replicate (n * 2) ' '

formatAttrs :: [Attribute] -> String
formatAttrs [] = ""
formatAttrs attrs = concatMap (\(k, v) -> " " ++ k ++ "=\"" ++ v ++ "\"") attrs

toXmlString :: Int -> Element -> String
toXmlString lvl (Element tName attrs kids txt _) = 
    let prefix = indent lvl 
        attrStr = formatAttrs attrs 
        openTag = prefix ++ "<" ++ tName ++ attrStr ++ ">" 
        closeTag = "</" ++ tName ++ ">" 
    in 
        if null kids && null txt 
            then prefix ++ "<" ++ tName ++ attrStr ++ " />" 
        else if null kids 
            then openTag ++ txt ++ closeTag 
        else 
        openTag ++ "\n" ++ 
        (if null txt then "" else indent (lvl + 1) ++ txt ++ "\n") 
        ++ unlines (map (toXmlString (lvl + 1)) kids) 
        ++ prefix ++ closeTag

printXML :: Element -> IO ()
printXML el = putStrLn (toXmlString 0 el)

getAttribute :: String -> Element -> Maybe String
getAttribute key el = lookup key (attributes el)

findByUid :: String -> Element -> Maybe Element
findByUid target el
    | uid el == target = Just el
    | otherwise = searchChildren (childrens el)
  where
    searchChildren [] = Nothing
    searchChildren (x:xs) =
        case findByUid target x of
            Just v  -> Just v
            Nothing -> searchChildren xs


selectAttr :: String -> String -> String -> Either String String
selectAttr uid key xml =
    case runFinal xml of
        Left err -> Left (show err)
        Right root ->
            case findByUid uid root of
                Nothing -> Left ("No element with id=" ++ uid)
                Just el ->
                    case getAttribute key el of
                        Nothing -> Left ("Attribute " ++ key ++ " not found")
                        Just v  -> Right v

setAttr :: String -> String -> [Attribute] -> [Attribute]
setAttr key val [] = [(key, val)]
setAttr key val ((k,v):xs)
    | k == key  = (k, val) : xs
    | otherwise = (k,v) : setAttr key val xs

updateByUid :: String -> (Element -> Element) -> Element -> Element
updateByUid target f el
    | uid el == target = f el
    | otherwise =
        el { childrens = map (updateByUid target f) (childrens el) }

setCommand :: String -> String -> String -> String -> Either String Element
setCommand uid key value xml =
    case runFinal xml of
        Left err -> Left (show err)
        Right root ->
            case findByUid uid root of
                Nothing -> Left ("No element with id=" ++ uid)
                Just _ ->
                    let newRoot =
                            updateByUid uid
                                (\el -> el { attributes = setAttr key value (attributes el) })
                                root
                    in Right newRoot

showAttrs :: [Attribute] -> String
showAttrs [] = "(no attributes)"
showAttrs xs = unwords [ k ++ "=\"" ++ v ++ "\"" | (k,v) <- xs ]

childrenCommand :: String -> String -> Either String [String]
childrenCommand uid xml =
    case runFinal xml of
        Left err -> Left (show err)
        Right root ->
            case findByUid uid root of
                Nothing -> Left ("No element with id=" ++ uid)
                Just el ->
                    Right
                      [ tag c ++ ": " ++ showAttrs (attributes c)
                      | c <- childrens el
                      ]

childCommand :: String -> Int -> String -> Either String Element
childCommand targetUid n xml =
    case runFinal xml of
        Left err -> Left (show err)
        Right root ->
            case findByUid targetUid root of
                Nothing -> Left ("No element with id=" ++ targetUid)
                Just el ->
                    if n <= 0 || n > length (childrens el)
                       then Left ("Element " ++ targetUid ++ " has no child #" ++ show n)
                       else Right (childrens el !! (n - 1))

textCommand :: String -> String -> Either String String
textCommand targetUid xml =
    case runFinal xml of
        Left err -> Left (show err)
        Right root ->
            case findByUid targetUid root of
                Nothing -> Left ("No element with id=" ++ targetUid)
                Just el -> Right (text el)

deleteAttr :: String -> [Attribute] -> [Attribute]
deleteAttr _ [] = []
deleteAttr key ((k,v):xs)
    | k == key  = xs           
    | otherwise = (k,v) : deleteAttr key xs

updateByUidDelete :: String -> (Element -> Element) -> Element -> Element
updateByUidDelete = updateByUid 

deleteCommand :: String -> String -> String -> Either String Element
deleteCommand targetUid key xml =
    case runFinal xml of
        Left err -> Left (show err)
        Right root ->
            case findByUid targetUid root of
                Nothing -> Left ("No element with id=" ++ targetUid)
                Just _ ->
                    let newRoot = updateByUid targetUid
                                    (\el -> el { attributes = deleteAttr key (attributes el) })
                                    root
                    in Right newRoot

newChild :: String -> Element
newChild newUid = Element {
    tag = "new",       
    attributes = [],
    childrens = [],
    text = "",
    uid = newUid
}

newChildCommand :: String -> String -> String -> Either String Element
newChildCommand parentUid newUid xml =
    case runFinal xml of
        Left err -> Left (show err)
        Right root ->
            case findByUid parentUid root of
                Nothing -> Left ("No element with id=" ++ parentUid)
                Just _ ->
                    let newRoot = updateByUid parentUid
                                    (\el -> el { childrens = childrens el ++ [newChild newUid] })
                                    root
                    in Right newRoot

xml = "<root id=\"1\" type = \"r\">\
\  Hello\
\  <person id=\"\">\
\    <name>Ivan</name>\
\    <age>20</age>\
\  </person>\
\  <person>\
\    <name>Maria</name>\
\    <age>22</age>\
\  </person>\
\</root>"

-- >>> runFinal xml
-- Left (UnexpectedSymbol "p")

-- >>>printXML tree
