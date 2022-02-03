module Parser 
    where
    import System.IO
    import Control.Monad
    import qualified Data.Map as Map
    import Text.ParserCombinators.Parsec
    import Text.ParserCombinators.Parsec.Expr
    import Text.ParserCombinators.Parsec.Language
    import qualified Text.ParserCombinators.Parsec.Token as Token
    import qualified Text.Parsec.String as PS
    import qualified Text.Parsec.Prim   as PP
    import qualified Text.Parsec.Token  as PT
    import qualified Text.Parsec.Expr as PE
    import qualified Text.Parsec.Char as C
    import Text.Parsec.Perm
    import Text.Parsec.Combinator

{-- Grammar
    keywords = follow end info set bibliography style link graph
               source sources <field> <styles> <name>

    sfield = author title publisher language media

    nfield = year

    fieldPair = sfield : String
              | nfield : Number

    styles = MLA Chicago APA

    name = USERDEFINED

    collection = sources
               | list-of <name>

    following = NOTHING
              | follow <name>

    condition = undefined

    exp = <sfield> <name> String
        | <sfield> <collection> String
        | <nfield> <name> Number
        | <nfield> <collection> Number
        | info <collection> <field>
        | set <collection> <condition>
        | link <collection> String
        | graph <name> <following> end
        | style <styles>
        | bibliography <collection>
        | bibliography <name>
        | source <name> map-of <fieldPair>
 --}


    data Exp = Style Styles
             | NewSource String Source
             | Error ParseError
             | Bibliography
             | Set String Source
             | Link String String String
             deriving (Show)

    data Source = Source (Map.Map String String) (Map.Map String String)
                deriving (Show)

    data Styles = MLA | CHICAGO deriving (Show) -- | APA

    lexer :: PT.TokenParser ()
    lexer = PT.makeTokenParser $ emptyDef {
        reservedNames = ["style", "Chicago", "MLA"]
    }

    identifier = Token.identifier lexer -- parses an identifier
    reserved   = Token.reserved   lexer -- parses a reserved name
    reservedOp = Token.reservedOp lexer -- parses an operator
    parens     = Token.parens     lexer -- parses surrounding parenthesis:
    braces     = Token.braces     lexer
                                        --   parens p
                                        -- takes care of the parenthesis and
                                        -- uses p to parse what's inside them
    integer    = Token.integer    lexer -- parses an integer
    colon      = Token.colon      lexer -- parses a colon
    whiteSpace = Token.whiteSpace lexer -- parses whitespace


    (<|||>) :: Parser a -> Parser a -> Parser a
    p1 <|||> p2 = (try p1) <|> p2

    -- Parse many expressions
    whileParser = whiteSpace >> expression 

    -- main expression parser
    expression :: Parser Exp
    expression  =  styleExp 
               <|> sourceExp
               <|> bibExp
               <|> setExp
               <|> linkExp
 

    -- style expression
    styles :: Parser Styles
    styles = let parseC = do
                    string "Chicago"
                    return CHICAGO
                 parseM = do
                    string "MLA"
                    return MLA
             in parseC <|||> parseM

    styleExp :: Parser Exp
    styleExp = do reserved "style"
                  style <- styles
                  return $ Style style

    -- source expression
    sourceExp :: Parser Exp
    sourceExp = do reserved "source"
                   n <- many1 alphaNum
                   whiteSpace
                   c <- braces (sepBy fieldExp (string "," >> whiteSpace))
                   return $ NewSource n (Source (Map.fromList c) (Map.fromList []))

    quote = between (string "'") (string "'")
                 
    labelExp l = do string l
                    whiteSpace
                    colon
                    whiteSpace
                    a <- quote (many1 (alphaNum <|||> oneOf " ,"))
                    whiteSpace
                    return a
    
    fieldExp = let authorExp = do a <- labelExp "author"
                                  return $ ("author", a)
                   titleExp  = do t <- labelExp "title"
                                  return $ ("title", t)
                   pubExp    = do p <- labelExp "publisher"
                                  return $ ("publisher", p)
                   yearExp   = do y <- labelExp "year"
                                  return $ ("year", y)
               in authorExp <|||> titleExp <|||> pubExp <|||> yearExp

    
    setExp = do string "set"
                whiteSpace
                name <- many1 alphaNum
                whiteSpace
                c <- braces (sepBy fieldExp (string "," >> whiteSpace))
                return $ Set name (Source (Map.fromList c) (Map.fromList []))


    -- bibliography expression
    bibExp :: Parser Exp
    bibExp = do reserved "bibliography"
                return $ Bibliography

    
    -- link expression
    linkExp :: Parser Exp
    linkExp = do reserved "link"
                 whiteSpace
                 name1 <- (many1 alphaNum)
                 whiteSpace
                 name2 <- (many1 alphaNum)
                 whiteSpace
                 linker <- quote (many1 alphaNum)
                 return $ Link name1 name2 linker

   
    -- parse line
    parseLine :: String -> Exp
    parseLine l = let p = (parse whileParser "" l)
                  in case p of 
                    Left err  -> (Error err)
                    Right xs  -> xs


    -- parse and run line
    interpretLine :: String -> ProgState -> IO (ProgState, Bool)
    interpretLine b s = do
        putStr $ b
        hFlush stdout
        str <- getLine
        if str == "quit"
        then return (s, False)
        else do s2 <- run (parseLine str) s
                return (s2, True)


    -- program implementation
    -- program state
    data ProgState = ProgState {style   :: (Source -> IO ()),
                                sources :: Map.Map String Source}

    -- initial program state
    initS = ProgState {style = printMLA,
                       sources = Map.fromList []}

    -- Main program run
    run :: Exp -> ProgState -> IO ProgState

    run (Error e)       state = do print e
                                   return state

    run (Style CHICAGO) state = do print "Chicago"
                                   return (state {style = printChicago})

    run (Style MLA)     state = do print "MLA"
                                   return (state {style = printMLA})

    run (NewSource n a) state = do style state a
                                   let newSources = Map.insert n a (sources state)
                                   return state {sources = newSources}

    run (Bibliography)  state = 
        do let bib = Map.foldr (\s output -> (style state) s >> output)
                               (return ())
                               (sources state)
           bib
           return state

    run (Set n (Source info links)) state = 
        case (Map.lookup n (sources state)) of
            Nothing -> do print "Error: source not declared" 
                          return state
            Just (Source i l)  -> 
                do let newFields = Map.union info i
                   let newSource = Source newFields l
                   let newSources = Map.insert n newSource (sources state)
                   print newSource
                   return state {sources = newSources}

    -- TODO: refactor repetitive code
    run (Link n1 n2 l) state = 
        do let s1 = case (Map.lookup n1 (sources state)) of
                         Nothing -> do print ("Error: source " ++ n1 ++ " not declared")
                                       return Nothing
                         Just (Source i1 l1) -> do let newLinks = Map.insert n2 l l1
                                                   let newSource = Source i1 newLinks
                                                   return (Just newSource)
           let s2 = case (Map.lookup n2 (sources state)) of
                         Nothing -> do print ("Error: source " ++ n2 ++ " not decalred")
                                       return Nothing
                         Just (Source i2 l2) -> do let newLinks = Map.insert n1 l l2
                                                   let newSource = Source i2 newLinks
                                                   return (Just newSource)
           source1 <- s1
           source2 <- s2
           let newSources = case source1 of
                               Nothing -> (sources state)
                               Just ns1 -> Map.insert n1 ns1 (sources state)
           let newSources2 = case source2 of
                               Nothing -> (sources state)
                               Just ns2 -> Map.insert n2 ns2 newSources
           return state {sources = newSources2}


    -- printing implementation
    printChicago (Source info links) = 
        if valid (Source info links)
        then let (author, title, pub, year) = fields (Source info links)
             in putStrLn (author ++ ". " ++ title ++ "(" 
                       ++ pub ++ ", " ++ year ++ ")")
        else printInvalid (Source info links)

    printMLA (Source info links) = 
        if valid (Source info links)
        then let (author, title, pub, year) = fields (Source info links)
             in putStrLn (author ++ ". " ++ title ++ ". " 
                          ++ pub ++ ", " ++ year)
        else printInvalid (Source info links)

    -- validate that source has all the necessary fields
    valid (Source info links) = 
        foldr (\n b -> b && Map.member n info) 
              True 
              ["author", "title", "publisher", "year"]    

    -- extract all the fields from source
    fields (Source info links) = let (Just author) = Map.lookup "author" info
                                     (Just title)  = Map.lookup "title" info
                                     (Just pub)    = Map.lookup "publisher" info
                                     (Just year)   = Map.lookup "year" info
                                 in (author, title, pub, year)

    -- safe printing a source with missing fields
    printInvalid (Source info links) = do putStr ("Fields missing from source: ")
                                          print info >> print links


    -- testing
    -- source defronzo {author: 'Defronzo, James', title: 'Revolutions', publisher: 'New York', year: '2014'}
