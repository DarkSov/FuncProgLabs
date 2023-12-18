{-# LANGUAGE OverloadedStrings #-}

import Text.XML
import Text.XML.Writer
import qualified Data.Text.Lazy.IO as TIO

createXML :: Element
createXML =
  Element
    "root"
    []
    [ NodeElement $
        Element
          "person"
          [("id", "1")]
          [ NodeElement $
              Element "name" [] [NodeContent "John"],
            NodeElement $
              Element "age" [] [NodeContent "30"],
            NodeElement $
              Element "city" [] [NodeContent "New York"]
          ],
      NodeElement $
        Element
          "person"
          [("id", "2")]
          [ NodeElement $
              Element "name" [] [NodeContent "Alice"],
            NodeElement $
              Element "age" [] [NodeContent "25"],
            NodeElement $
              Element "city" [] [NodeContent "London"]
          ]
    ]

createDTD :: Document
createDTD =
  Document
    (Prologue [] Nothing [])
    (Doctype "root" (Just "example.dtd"))
    [ NodeElement $
        Element
          "element"
          [("name", "root")]
          [ NodeElement $
              Element
                "element"
                [("name", "person"), ("minOccurs", "0"), ("maxOccurs", "unbounded")]
                [ NodeElement $
                    Element
                      "element"
                      [("name", "name"), ("minOccurs", "1"), ("maxOccurs", "1")]
                      [],
                  NodeElement $
                    Element
                      "element"
                      [("name", "age"), ("minOccurs", "1"), ("maxOccurs", "1")]
                      [],
                  NodeElement $
                    Element
                      "element"
                      [("name", "city"), ("minOccurs", "1"), ("maxOccurs", "1")]
                      []
                ]
          ]
    ]

main :: IO ()
main = do
  let xml = Document (Prologue [] Nothing []) (Element "root" [] [NodeElement createXML]) []
  TIO.writeFile "example.xml" (renderText def xml)

  let dtd = createDTD
  TIO.writeFile "example.dtd" (renderText def dtd)
