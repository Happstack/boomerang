{-
instance Functor (Parser tok a) where
    fmap f (Parser p) = Parser $ \tok -> fmap (first (fmap f)) (p tok)

instance Applicative (Parser tok a) where
    pure x = Parser $ \tok -> return (const x, tok)


{-
newtype ParserM tok a = ParserM { runParserM :: tok -> ListT (Either RouteError) (a, tok) }

instance Monad (ParserM tok) where
    return a = ParserM $ \tok -> return (a, tok)
    (ParserM p) >>= f = 
        ParserM $ \tok ->
            do (a, tok') <- p tok
               runParserM (f a) tok

newtype Parser tok a b = Parser { runParser :: tok -> ListT (Either RouteError) (a -> b, tok) }

instance Monad (Parser tok a) where
    (Parser p) >>= k = 
        Parser $ \tok ->
            do (f, tok') <- p tok
               (b, tok'') <- runParser (k (f undefined)) tok'
               return (b, tok'')
-}

{-
   instance Monad ((->) r) where
        return = const
        f >>= k = \ r -> k (f r) r 
-}

{-
newtype Parser tok a b = Parser { runParser :: tok -> Either RouteError [(a -> b, tok)] }

instance Functor (Parser tok a) where
    fmap f (Parser p) = 
        Parser $ \s -> fmap (map (first (fmap f))) $ p s

instance Monad (Parser tok a) where
    return b = Parser $ \tok -> Right [(const b, tok)]
    (Parser p) >>= f = Parser $ \tok ->
        case p tok of
          (Left e) -> Left e
          (Right as) ->
              case partitionEithers [ (runParser (f a) tok')  | (a, tok') <- as ] of
                ([], []) -> Right []
--              partitionEithers (map f as)
  -}  
{-
-- | Build a router for a value given all the ways to parse and serialize it.
val :: (tok ->  Either e [(a, tok)]) -> (a -> [tok -> tok]) -> Boomerang e tok r (a :- r)
val rs ss = Boomerang
    (liftEitherP (fmap (map (first (:-))) . rs))
    (\(a :- r) -> (map (\f -> (f, r))) (ss a))
-}
liftEither :: (Error e) => Either e [a] -> ListT (StateT ErrorPos (Either e)) a
liftEither (Left e)   = ListT $ (StateT $ \s -> Left e)
liftEither (Right as) = ListT (StateT $ \s -> Right (as, s))

liftEitherP :: (tok -> Either RouteError [(a, tok)]) -> Parser RouteError tok a
liftEitherP e = Parser $ \tok -> liftEither (e tok)

{-
      (Left e)   -> Left e
      (Right []) -> Left (strMsg "unparse1: no valid unparsing")
      (Right (s:_)) -> Right s
-}

{-
data Boomerang tok a b = Boomerang
  { prs :: tok -> Either RouteError [(a -> b, tok)]
  , ser :: b -> Either RouteError [(tok -> tok, a)]
  }


routeError :: ErrorMsg -> Either RouteError b
routeError e = Left (RouteError Nothing e)
-}



{-
mapRouteError :: (e -> e') -> Boomerang e a b -> Boomerang e' a b
mapRouteError f (Boomerang pf sf) =
    Boomerang (\a -> either (Left . f) (Right . id) (pf a))
           (\b -> either (Left . f) (Right . id) (sf b))

instance Category (Boomerang tok) where
  id = Boomerang
    (\x -> return [(id, x)])
    (\x -> return [(id, x)])

  ~(Boomerang pf sf) . ~(Boomerang pg sg) = Boomerang 
    (compose (.) pf pg)
    (compose (.) sf sg) 
-} 
{-
    Parser $ \tok ->
        do (f, tok')  <- runParser mf tok
           (g, tok'') <- runParser mg tok'
           return (f `op` g, tok'')
-}


{-
instance Applicative (Parser e tok) where
    pure =         
        Parser $ \tok pos ->
            [Right ((a, tok), pos)]
    (Parser f) >>= (Parser x) =
        Parser $ \tok pos ->
            concat [ (runParser (f a)) tok' pos' | Right ((a, tok'), pos') <- f tok pos ]
-}

    

                         
        
    

-- type P pos e tok a = tok -> pos -> [Either e ((a, tok), pos)]



{-
instance Monad (Parser e tok) where
    return a = Parser $ \tok -> return (a, tok)
    (Parser p) >>= f =
        Parser $ \tok ->
            do (a, tok') <- p tok
               (b, tok'') <- runParser (f a) tok'
               return (b, tok'')

instance MonadPlus (Parser e tok) where
    mzero = Parser $ \tok -> ListT $ return []
    (Parser x) `mplus` (Parser y) =
        Parser $ \tok -> 
            ListT $ StateT $ \st ->
                case runStateT (runListT (x tok)) st of
                  (Left _) -> 
                      runStateT (runListT (y tok)) st
                  (Right (a, st')) ->
                      case runStateT (runListT (y tok)) st of
                        (Left _) -> Right (a, st')
                        (Right (b, st'')) -> Right (a ++ b, st'')

instance Applicative (Parser e tok) where
    pure    = return
    f <*> x = f `ap` x


composeP
  :: (a -> b -> c)
  -> Parser e tok a
  -> Parser e tok b
  -> Parser e tok c
composeP op mf mg = 
    Parser $ \tok ->
        do (f, tok')  <- runParser mf tok
           (g, tok'') <- runParser mg tok'
           return (f `op` g, tok'')

bind :: Either e [a] -> (a -> Either e [b]) -> Either e [b]
bind (Left e) _ = (Left e)
bind (Right l) f =
    case partitionEithers (map f l) of
      ([], []) -> Right []
      (errs,[]) -> Left (last errs)
      (_, succ) -> Right (concat succ)
composeE
  :: (a -> b -> c)
  -> (i -> Either e [(a, j)])
  -> (j -> Either e [(b, k)])
  -> (i -> Either e [(c, k)])
composeE op mf mg = \s ->
  case mf s of
    (Left e) -> (Left e)
    (Right fs) ->
        case partitionEithers [ fmap (map (first (op f))) (mg s') | (f, s') <- fs  ] of
          ([], []) -> Right []
          (errs,[]) -> Left (last errs)
          (_, succs) -> Right (concat succs)


-}{-
    case partitionEithers $ runParser (prs p) s initialPos of
      ([], [])   -> Right []
      (errs, []) -> Left $ errs
      (_, fs)    -> Right [ (f (), tok) | ((f, tok),_) <- fs ]
-}

{-
parse :: (Position (Pos e)) => Boomerang e tok () a -> tok -> Either [e] [(a, tok)]
parse p s = 
    case partitionEithers $ runParser (prs p) s initialPos of
      ([], [])   -> Right []
      (errs, []) -> Left $ errs
      (_, fs)    -> Right [ (f (), tok) | ((f, tok),_) <- fs ]
-}

--      (Left errs)     -> Left errs
--      (Right fs) -> Right [ (f (), tok) | (f, tok) <- fs ]
{-

parse1 :: (Error e, Position (Pos e)) => Boomerang e tok () (a :- ()) -> tok -> Either [e] a
parse1 p s = 
    case parse p s of 
      (Left e) -> (Left e)
      (Right as) -> 
          case map (hhead . fst) as of
            [] -> Left [strMsg "no complete parses."]
            (a:_) -> Right a
-}

{-
-- | @r \`printAs\` s@ uses ther serializer of @r@ to test if serializing succeeds,
--   and if it does, instead serializes as @s@. 
printAs :: Boomerang a b -> String -> Boomerang a b
printAs r s = r { ser = map (first (const (s ++))) . take 1 . ser r }

readEither :: (Read a) => [String] -> StateT ErrorPos (Either RouteError) [(a, [String])]
readEither [] = throwRouteError EOF
readEither (p:ps) = 
          case reads p of
            [] -> throwRouteError (Other $ "readEither failed on " ++ p)
            rs -> Right $ map (\(a,p') -> (a, p':ps)) rs




-- | Routes any value that has a Show and Read instance.
readshow :: (Show a, Read a) => Boomerang [String] r (a :- r)
readshow = val readEither showEither


showEither :: (Show a) => a -> Either e [[String] -> [String]]
showEither a = Right [\(s:ss) -> (shows a s) : ss ]

-- | Routes any integer.
int :: Boomerang [String] r (Int :- r)
int = readshow

-- | Routes any string.
string :: Boomerang [String] r (String :- r)
string = val ps ss 
    where
      ps [] = routeError EOF
      ps (h:t) = return [(h, ("" : t))]
      ss str = return [\(s:ss) -> (str ++ s) : ss]

-- | Routes one string satisfying the given predicate.
satisfy :: (String -> Bool) -> Boomerang [String] r (String :- r)
satisfy p = val ps ss
    where
      ps []    = routeError EOF
      ps (h:t) = if p h
                 then return [(h, t)]
                 else Left $ strMsg ("predicate failed on " ++ h)
      ss s = if p s
             then return [([s] ++)]
             else Left (strMsg $ "predicate failed on " ++ s)
infixr 9 </>
(</>) :: Boomerang [String] b c -> Boomerang [String] a b -> Boomerang [String] a c
f </> g = f . eops . g

eops :: Boomerang [String] r r
eops = Boomerang 
       (\path -> case path of
                   []      -> return   [(id, [])]
                   ("":ps) -> return [(id, ps)]
                   (p:_) -> Left $ strMsg $ "path-segment not entirely consumed: " ++ p)
       (\a -> return [(("" :), a)])


-}
