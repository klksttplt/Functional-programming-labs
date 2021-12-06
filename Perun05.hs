{-# OPTIONS_GHC -Wall #-}
module Perun05 where

import Data.Char(isSpace, isDigit, isLetter) 

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- ������ 1 -----------------------------------------
spaces :: String -> String
spaces = dropWhile (isSpace) 
  
-- ������ 2.a ----------------------------------------- 
manyT :: String -> (String,String)
t:: Char -> Bool

t x = x /= '<' && x /= '>'
manyT s = (takeWhile t s, dropWhile t s) 

-- ������ 2.b ----------------------------------------- 
value :: String -> (String,String)
v:: Char -> Bool

v x = x /= '"'
value s = (takeWhile v s, dropWhile v s)    

-- ������ 2.c ----------------------------------------- 
manyN :: String -> (String,String)
n:: Char -> Bool

n x = x == '.'|| isLetter x ||isDigit x || x == '-'
manyN s = (takeWhile n s, dropWhile n s) 

-- ������ 3.a -----------------------------------------
name :: String ->  Maybe(String,String) 
name [] = Nothing
name (x:xs) | isLetter x = Just(x:fst(manyN xs), snd(manyN xs))
            | otherwise = Nothing

-- ������ 3.b -----------------------------------------
text :: String ->  Maybe(String,String) 
text [] = Nothing
text xs | null (fst(manyT xs)) = Nothing
        | otherwise = Just(fst(manyT xs), snd(manyT xs))

-- ������ 3.c -----------------------------------------
fullValue :: String ->  Maybe(String,String) 
fullValue [] = Nothing
fullValue (x:xs) | x == '"' = let (a, b) = value xs in Just (a, tail b)
                 | otherwise = Nothing

-- ������ 4.a -----------------------------------------
attrib :: String -> Maybe ((String,String),String) 
attrib s = case name s of
    Just (_, "") -> Nothing
    Just (a, at) -> if l /= '=' then Nothing
                            else case fullValue (spaces(lefted)) of
                            Just (val,str) -> Just((a,val), spaces (str))
                            _  -> Nothing
                            where (l:lefted) = spaces at
    _ -> Nothing
    
-- ������ 4.b -----------------------------------------
manyAtt :: String -> Maybe (Attributes,String) 
manyHelp :: String -> (Attributes, String)

manyHelp s = 
  case attrib s of
    Just (ah, ss) ->
      (ah:attr, rest)
      where (attr, rest) = manyHelp ss
    Nothing -> ([], s)

manyAtt str = Just (manyHelp str)

-- ������ 5.a -----------------------------------------
begTag :: String -> Maybe ((String,Attributes),String)

begT :: [Char] -> Bool
begT s = elem '<' s && elem '>' s

begTag [] = Nothing
begTag s@(_:tag)
    | begT s = case name tag of
                Just (nm, afterName) -> case manyAtt  (spaces(afterName)) of
                                        Just (attributes, (_:lefted)) -> Just ((nm, attributes), lefted) 
                                        _ -> Nothing
                _ -> Nothing
    | otherwise = Nothing


-- ������ 5.b -----------------------------------------
endTag :: String -> Maybe (String,String) 
endT :: [Char] -> Bool
endT "" = False
endT [_] = False
endT (a:b:c) = a == '<' && b == '/' && elem '>' c

endTag s
    | endT s = case name $ drop 2 s of 
                   Just(nm, afterName) -> Just(nm, tail $ afterName)
                   _ -> Nothing
    | otherwise = Nothing   

-- ������ 6.a -----------------------------------------
element :: String -> Maybe (XML,String) 
element = undefined

-- ������ 6.b -----------------------------------------
xml :: String -> Maybe (XML,String)
xml = undefined

-- ������ 6.c -----------------------------------------
manyXML :: String -> Maybe ([XML],String)
manyXML = undefined

-- ������ 7 -----------------------------------------
fullXML :: String -> Maybe XML 
fullXML = undefined

-- ������ ���� -------------------------------------------
-- ����� ����� XML-��'���� (��� �������)
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>" 

-- ���������� ������ ���������� XML-��'����
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]



