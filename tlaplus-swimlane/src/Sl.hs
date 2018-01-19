{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, IOException)
import Data.Maybe ( fromMaybe, fromJust ) -- for the getOpt package use
import Data.List as List
import Data.Map  as Map hiding (splitAt)
import Data.Set  as Set hiding (splitAt)
import Debug.Trace as Trace
import System.Console.GetOpt
import GHC.Base  as Prelude hiding (join)
import System.IO
import Lexer (alexScanTokens)
import Parser (Stmt(..), Ident(..), Expr(..), parser, listparser, exprparser)
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex as Regex

import TraceReader
import ExprHelper

import System.Environment
import System.Exit

import SLA (readSLA)

data Output = PsTricks
outputformat = PsTricks

data Flag = Verbose
          | Version
          | Help
          | Output String
          | Density String
          | BlackAndWhite
          | DefaultMsgNotes
          | DefaultStateNotes
          | XUnitMM String
          | YUnitMM String
          | LeftWidth String
          | RightWidth String
          | AnnotationFile String
          | ConfigVariable String
          | DrawBoundingBox
          | Landscape
          | IncludeFile
          | LaneOrder String -- "A,B,C"
          | DefaultNoteFont String
          | UserNoteFont String
          | TLCView String
          | Liberal
          | SLAFile String
            deriving (Show, Eq)

-- FIXME remove -m option throught the code
options :: [OptDescr Flag]
options =
  [Option ['a']     ["annotation"]  (ReqArg AnnotationFile "STRING")
     "name of the annotation file (default is <inputfile>.ann)"
  ,Option ['b']     ["boundingbox"] (NoArg DrawBoundingBox) "draw bounding box (LaTeX \\fobx)"
  ,Option ['B']     ["blackandwhite"] (NoArg BlackAndWhite) "use black and white only"
  ,Option ['c']     ["config"]  (ReqArg ConfigVariable "STRING") "name of sl config record in annotation file (default is sl_config)"
  ,Option ['f']     ["autonotefont"]  (ReqArg DefaultNoteFont "STRING") "font used for default notes (-m, -s), default LaTeX \\tiny."
  ,Option ['h','?'] ["help"]    (NoArg Help)          "usage help"
  ,Option ['d']     ["density"]  (ReqArg Density "compact|all") "layout"
  ,Option ['i']     ["include"] (NoArg IncludeFile)       "pspicture file for LaTeX include"
  ,Option ['l']     ["landscape"] (NoArg Landscape)       "landscape output"
  ,Option ['L']     ["liberal"] (NoArg Liberal)       "do not exit on -w problems."
  ,Option ['m']     ["defmnote"](NoArg DefaultMsgNotes)
     "annotate all trace message interactions with the message content"
  ,Option ['o']     ["output"]  (ReqArg Output "FILE")  "output FILE"
  ,Option ['p']     ["width_l"] (ReqArg LeftWidth "INT") "left hand side label width [mm]"
  ,Option ['q']     ["width_r"] (ReqArg RightWidth "INT") "right hand side label width [mm]"
  ,Option ['s']     ["defsnote"] (NoArg DefaultStateNotes) "display all changes for each state transition"
  ,Option ['t']     [""] (ReqArg SLAFile "STRING") "SLA file"
  ,Option ['u']     ["usernotefont"]  (ReqArg UserNoteFont "STRING") "font used for user notes (annotation), default LaTex \\footnotesize"
  ,Option ['v']     ["verbose"] (NoArg Verbose)       "verbose output"
  ,Option ['V']     ["version"] (NoArg Version)       "show version number"
  ,Option ['w']     ["tlcview"]   (ReqArg TLCView "STRING") "list of variables in view (e.g. -w rs,cs) if VIEW is used in TLC cfg file."
  ,Option ['x']     ["xunit"]   (ReqArg XUnitMM "INT") "swimlane (x) distance [mm]"
  ,Option ['y']     ["yunit"]   (ReqArg YUnitMM "INT") "state (y) distance [mm]"
  ,Option ['z']     ["laneorder"]   (ReqArg LaneOrder "STRING") "specify lane ordering left to right (e.g. -zA,B,X,Y)"
  ]
  where s = "                                             "

outp :: Maybe String -> Flag
outp = Output . fromMaybe "stdout"

getOptions :: [String] -> IO ([Flag], [String])
getOptions argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) ->
              ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: sl [OPTIONS] tracefile"

data DensityOption = All | Compact deriving (Eq, Show)

data Config = Config { verbose           :: Bool,
                       fbox              :: Bool,
                       blackandwhite     :: Bool,
                       includefile       :: Bool,
                       landscape         :: Bool,
                       liberal           :: Bool,
                       input             :: String,
                       output            :: Maybe String,
                       laneorder         :: Maybe [String],
                       tlcview           :: Maybe [String],
                       annotationfile    :: Maybe String,
                       configvar         :: String,
                       density           :: DensityOption,
                       defaultnotefont   :: String,
                       usernotefont      :: Maybe String,
                       gen_msg_labels    :: Bool,
                       gen_state_labels  :: Bool,
                       l_label_w_mm      :: Int,
                       r_label_w_mm      :: Int,
                       xunit_mm          :: Int,
                       yunit_mm          :: Int,
                       slafile           :: Maybe String }
              deriving (Eq, Show)

{-# NOINLINE config #-}
config :: Config
config = unsafePerformIO $
           do args <- getArgs
              (flags, files) <- getOptions args
              i <- getInputfile files
              o <- getOutputfile flags
              l <- getDensity flags
              return Config {
                verbose = getVerbose flags,
                fbox = getDrawBoundingBox flags,
                blackandwhite = getBlackAndWhite flags,
                landscape = getLandscape flags,
                liberal = getLiberal flags,
                input  = i,
                includefile = getIncludeFile flags,
                output = o,
                density = l,
                defaultnotefont = getDefaultNoteFont flags,
                usernotefont = getUserNoteFont flags,
                laneorder = split $ getLaneOrder flags,
                tlcview = split $ getTLCView flags,
                gen_msg_labels = getDefaultMsgNotes flags,
                gen_state_labels = getDefaultStateNotes flags,
                l_label_w_mm = getLeftWidth flags,  -- mm
                r_label_w_mm = getRightWidth flags, -- mm
                annotationfile = getAnnotationFile flags,
                slafile = getSLAFile flags,
                configvar = getConfigVariable flags,
                xunit_mm = getXUnits flags,
                yunit_mm = getYUnits flags }
  where getDensity :: [Flag] -> IO (DensityOption)
        getDensity [] = return All -- default is All
        getDensity ((Density "all"):rest)     = return All
        getDensity ((Density "compact"):rest) = return Compact
        getDensity ((Density ups):rest)       =
            ioError (userError ("unknown density "++(show ups)++
                                ", must be all or compact"))
        getDensity (_:rest) = getDensity rest

        getDefaultMsgNotes :: [Flag] -> Bool
        getDefaultMsgNotes []                  = False
        getDefaultMsgNotes (DefaultMsgNotes:_) = True
        getDefaultMsgNotes (_:r)               = getDefaultMsgNotes r

        getDefaultStateNotes :: [Flag] -> Bool
        getDefaultStateNotes []                    = False
        getDefaultStateNotes (DefaultStateNotes:_) = True
        getDefaultStateNotes (_:r)                 = getDefaultStateNotes r

        getXUnits :: [Flag] -> Int
        getXUnits []                 = 10
        getXUnits (XUnitMM intstr:_) = read intstr
        getXUnits (_:rest)           = getXUnits rest

        getYUnits :: [Flag] -> Int
        getYUnits []                  = 4
        getYUnits (YUnitMM intstr:_)  = read intstr
        getYUnits (_:rest)            = getYUnits rest

        getLeftWidth :: [Flag] -> Int
        getLeftWidth []                   = 50
        getLeftWidth (LeftWidth intstr:_) = read intstr
        getLeftWidth (_:rest)             = getLeftWidth rest

        getRightWidth :: [Flag] -> Int
        getRightWidth []                    = 50
        getRightWidth (RightWidth intstr:_) = read intstr
        getRightWidth (_:rest)              = getRightWidth rest

        getInputfile :: [String] -> IO (String)
        getInputfile []    = ioError (userError ("missing input file name"))
        getInputfile (f:_) = return f -- take first name

        getOutputfile :: [Flag] -> IO (Maybe String)
        getOutputfile []             = return Nothing
        getOutputfile ((Output f):_) = return $ Just f
        getOutputfile (_:rest)       = getOutputfile rest

        getAnnotationFile :: [Flag] -> Maybe String
        getAnnotationFile []                    = Nothing
        getAnnotationFile (AnnotationFile af:_) = Just af
        getAnnotationFile (_:rest)              = getAnnotationFile rest

        getConfigVariable :: [Flag] -> String
        getConfigVariable []                     = "sl_config"
        getConfigVariable (ConfigVariable cfv:_) = cfv
        getConfigVariable (_:rest)               = getConfigVariable rest

        getDefaultNoteFont :: [Flag] -> String
        getDefaultNoteFont []                    = "footnotesize"
        getDefaultNoteFont (DefaultNoteFont f:_) = f
        getDefaultNoteFont (_:rest)              = getDefaultNoteFont rest

        getUserNoteFont :: [Flag] -> Maybe String
        getUserNoteFont []                 = Nothing
        getUserNoteFont (UserNoteFont f:_) = Just f
        getUserNoteFont (_:rest)           = getUserNoteFont rest

        getVerbose :: [Flag] -> Bool
        getVerbose []          = False
        getVerbose (Verbose:_) = True
        getVerbose (_:rest)    = getVerbose rest

        getBlackAndWhite :: [Flag] -> Bool
        getBlackAndWhite []                = False
        getBlackAndWhite (BlackAndWhite:_) = True
        getBlackAndWhite (_:rest)          = getBlackAndWhite rest

        getLaneOrder :: [Flag] -> Maybe String
        getLaneOrder []              = Nothing
        getLaneOrder (LaneOrder s:_) = Just s
        getLaneOrder (_:rest)        = getLaneOrder rest

        getTLCView :: [Flag] -> Maybe String
        getTLCView []            = Nothing
        getTLCView (TLCView s:_) = Just s
        getTLCView (_:rest)      = getTLCView rest

        split :: Maybe String -> Maybe [String]
        split Nothing  = Nothing
        split (Just s) = Just $ Regex.splitRegex (Regex.mkRegex ",") s

        getIncludeFile :: [Flag] -> Bool
        getIncludeFile []              = False
        getIncludeFile (IncludeFile:_) = True
        getIncludeFile (_:rest)        = getIncludeFile rest

        getDrawBoundingBox :: [Flag] -> Bool
        getDrawBoundingBox []                  = False
        getDrawBoundingBox (DrawBoundingBox:_) = True
        getDrawBoundingBox (_:rest)            = getDrawBoundingBox rest

        getLandscape :: [Flag] -> Bool
        getLandscape []            = False
        getLandscape (Landscape:_) = True
        getLandscape (_:rest)      = getLandscape rest

        getLiberal :: [Flag] -> Bool
        getLiberal []          = False
        getLiberal (Liberal:_) = True
        getLiberal (_:rest)    = getLiberal rest

        getSLAFile :: [Flag] -> Maybe String
        getSLAFile []             = Nothing
        getSLAFile (SLAFile af:_) = Just af
        getSLAFile (_:rest)       = getSLAFile rest

version = "Swimlane (sl), version \"0.2a\""
help = "Swimlane (sl) renders a TLC counterexample as a LaTeX/pstricks "++
       "pspicture file.\n" ++ (usageInfo "\nUsage:" options)

data PrintLevel = PNote | PWarning | PError
say :: PrintLevel -> String -> IO ()
say PNote s    | not $ verbose config = return ()
say PNote s    |       verbose config = hPutStrLn stderr $ "[note] "++s
say PWarning s                        = hPutStrLn stderr $ "[warning] "++s
say PError s                          = hPutStrLn stderr $ "[error] "  ++s

note = say PNote
warn = say PWarning
err s = do say PError s
           exitFailure

main :: IO ()
main = do argv <- getArgs
          (flags, files) <- getOptions argv
          if Prelude.elem Version flags
             then (do
                   putStrLn version
                   exitWith ExitSuccess)
             else return ()
          if Prelude.elem Help flags
             then (do
                   putStrLn help
                   exitWith ExitSuccess)
             else return ()
          if includefile config && landscape config
            then err "landscape option (-l) cannot be used in includefile mode (-i)."
            else return ()
          if includefile config && fbox config
            then err "boundingbox option (-b) cannot be used in includefile mode (-i)."
            else return ()
          let annfile = case annotationfile config of
                          Nothing -> input config ++ ".ann"
                          Just af -> af
          anntext <- catch
                       (do f <- readFile annfile
                           note $ "reading annotation file "++annfile
                           return f)
                       (\(e::IOException) -> do
                                 case annotationfile config of
                                   Nothing -> -- default annotation file
                                     note $ "no default annotation file "++
                                            annfile++" found."
                                   Just _ -> -- -a switch present
                                     do err $ "annotation file "++annfile++
                                              " not found."
                                 return "")
          let ann  = case anntext of
                       "" -> []
                       _  -> (listparser . alexScanTokens) anntext
          let sl_config_key = configvar config
          let defaultMsgNotes = gen_msg_labels config
              defaultStateNotes = gen_state_labels config

          (saf, slafun) <-
              case slafile config of
                Nothing -> err "missing .sla file (use -t switch)."
                Just f -> SLA.readSLA f >>= \sla ->
                            case sla of
                              Right (saf, slafun) ->
                                return (saf, slafun)
                              Left errmsg -> err $ errmsg
          readFile (input config) >>= \s ->
           case textToStates s of
           (Just errmsg, _, _, _) ->
             err errmsg
           (Nothing, issue, states, cycle) ->
            let estates = List.map (\(i, bindlist) ->
                            let m = List.map (\(Bind id e) -> (id, e))
                                             bindlist
                             in (i, RecE (Map.fromList m))) states
                mel = fromJust slafun $ estates
                playernames = List.nub $ concat $
                                List.map (\((_,sl), (_,sl'), _) ->
                                  [sl,sl']) mel
                suspectmel = Trace.trace ("Swimlanes = "++show playernames) $
                    List.filter (\((i,sl), (i',sl'), deco) -> sl == sl') mel

                allstateidx' =
                  Prelude.map (\(_s, ievents) ->
                    Prelude.map (\(i,_) -> i) ievents) skeleton
                allstateidx =
                  List.sort $ List.nub (concat allstateidx')

                t_labeledtrace = []
                interactions = toInteractions mel
                grid = playerGrid interactions
                t_skeleton  = toSwimlanes states saf grid playernames
                t_spiderweb = if blackandwhite config
                              then toBlackAndWhite interactions
                              else toInteractions mel

                f_skeleton  = filterstates mel $
                                toSwimlanes states saf grid playernames
                f_spiderweb = if blackandwhite config
                              then toBlackAndWhite interactions
                              else interactions

                (m, skeleton, spiderweb) = case density config of
                  All     -> (length states-1, t_skeleton, t_spiderweb)
                  -- note in Compact density mode the bounding box needs to be
                  -- computed with all state (incl the ones not drawn) counted,
                  -- hence the length of t_labeltrace (T!) being used
                  Compact -> (length allstateidx, f_skeleton, f_spiderweb)
                numlanes = case laneorder config of
                             Nothing -> length skeleton
                             Just lp -> length lp
                out = case outputformat of
                        PsTricks ->
                          let annR = case ann of
                                       [] -> RecE Map.empty
                                       [b] -> let (Bind sl_config_key recE) = b
                                               in recE
                              snotes = mapannIS  "state_label_horizontal" annR
                              vnotes = mapannIS  "state_label_vertical" annR
                              arrowtips = mapannPIS "arrowtip_label" annR
                              abbrev = mapannAbbrev "abbreviation" annR
                              subgraphs =
                                to_pstricks_swimlanes
                                  saf cycle abbrev numlanes
                                  defaultStateNotes m
                                  snotes vnotes allstateidx skeleton
                              hasSaf = case saf of -- missing (Eq) on saf fun
                                         Nothing -> False
                                         Just _ -> True
                           in
                           [to_pstricks_graph_open numlanes m
                              (True) -- FIXME check .sla file
                              ((snotes /= Map.empty) || hasSaf)
                              issue,
                            indent "  "
                            ((to_pstricks_graph_preamble numlanes m)++
                             subgraphs++
                             (to_pstricks_interactions
                                defaultMsgNotes m arrowtips abbrev
                                allstateidx spiderweb)),
                            to_pstricks_graph_close argv]
             in  do let content = unlines $ concat out
                     in case output config of
                          Nothing -> do hPutStrLn stdout content
                                        hFlush stdout
                                        hClose stdout
                          Just f -> do note $ "Writing to file: " ++ f
                                       writeFile f content
                    let lines = List.map
                         (\((i, sl), (i', sl'), deco) ->
                           sl++" "++show i++", "++
                           show i'++": "++ {-ppE m Map.empty-} show "foo")
                         suspectmel
                     in if length lines > 0
                        then do hPutStr stderr $ unlines lines
                                if liberal config
                                 then warn "message exchange on same swimlane."
                                 else err $ "message exchange on same "++
                                            "swimlane (-L ignores this error)."
                        else return ()
          where nodelist [] acc      = List.nub acc
                nodelist ((((i,_),(j,_),_),_):cme) acc = nodelist cme (i:j:acc)
                filterstates :: [MsgExchange] -> [Swimlane] -> [Swimlane]
                filterstates mel lanes =
                    let idxs = concat $ List.map (\((i,sl),(i',sl'),_) ->
                                          [(i,sl), (i',sl')]) mel
                     in List.map (\swimlane -> filterlane idxs swimlane) lanes
                    where filterlane :: [(Int, SwimlaneName)] -> Swimlane
                                     -> Swimlane
                          filterlane idxs lane =
                              let (sl, nodes) = lane
                                  nodes' = List.filter (\(i, EventGrp sl _) ->
                                             List.elem (i,sl) idxs) nodes
                               in (sl, nodes')
                renumber :: LabeledTrace -> LabeledTrace
                renumber lt =
                    let t = List.map (\(i,egrp) -> egrp) lt
                     in numberlist t
                mapannIS :: String -> Expr -> (Map Int String)
                mapannIS name (RecE ann) =
                    case Map.null ann of
                      True  -> Map.empty
                      False -> case Map.member (Ident name) ann of
                                 True  -> let MapE mape = ann!(Ident name)
                                           in mapIS mape
                                 False -> Map.empty
                  where mapIS :: (Map Expr Expr) -> (Map Int String)
                        mapIS map_e =
                            let m'  = Map.mapKeys (\(IntE i) -> i) map_e
                             in Map.mapWithKey (\k (StrE s) -> s) m'
                mapannAbbrev :: String -> Expr -> (Map String String)
                mapannAbbrev name (RecE ann) =
                    case Map.null ann of
                      True  -> Map.empty
                      False -> case Map.member (Ident name) ann of
                                 True  -> let MapE mape = ann!(Ident name)
                                           in mapAB mape
                                 False -> Map.empty
                  where mapAB :: (Map Expr Expr) -> (Map String String)
                        mapAB map_e =
                            let m'  = Map.mapKeys (\(StrE i) -> i) map_e
                             in Map.mapWithKey (\k (StrE s) -> s) m'
                mapannPIS :: String -> Expr -> (Map (Int,Int) String)
                mapannPIS name (RecE ann) =
                    case Map.null ann of
                      True  -> Map.empty
                      False -> case Map.member (Ident name) ann of
                                 True  -> let MapE mape = ann!(Ident name)
                                           in mapPIS mape
                                 False -> Map.empty
                  where mapPIS :: (Map Expr Expr) -> (Map (Int,Int) String)
                        mapPIS map_e =
                            let m'  = Map.mapKeys
                                        (\(SeqE [(IntE i),(IntE j)]) -> (i,j))
                                        map_e
                             in Map.mapWithKey (\k (StrE s) -> s) m'

runSl :: String -> Stmt
runSl = parser . alexScanTokens

type Trace = [EventGrp]
type Node = (Int, EventGrp)
type LabeledTrace = [Node]
type Swimlane = (String, LabeledTrace)

data EventGrp = EventGrp String (Set Event) deriving (Show, Ord, Eq)
type OrigEventRecMap = Map Ident Expr
data Event = NoMessageEvent OrigEventRecMap String
           | SingleSendEvent OrigEventRecMap String Message
           | MultiSendEvent OrigEventRecMap String Message (Set String)
           | MultiRecvEvent OrigEventRecMap String (Set Message)
           deriving (Show, Ord, Eq)
type Message = Map Ident Expr

type MessageNode = (Int, Event)
type MessageExchange = (MessageNode, MessageNode, Message) -- FIXME remove msg
type ColoredMessageExchange = (MessageExchange, (String, String,
                                                 String, String,
                                                 String, String))
type StateNotes = Map Int String
type MsgNotes = Map (Int,Int) String

toTrace :: [Expr] -> Trace
toTrace l = Prelude.map f l
  where f (RecE rec) = let AtomE sl    = rec!(Ident "swimlane")
                           SetE events = rec!(Ident "events")
                        in mkE sl events
mkE :: String -> Set Expr -> EventGrp
mkE swimlane exprset = EventGrp swimlane (Set.map f exprset)
   where f (RecE r) | Map.member (Ident "msg") r =
           case r!(Ident "msg") of
             SeqE msg -> let StrE etype = r!(Ident "etype")
                             SeqE tuple = r!(Ident "msg")
                             RecE msg = head22 tuple
                             SetE destset = (head22.tail) tuple
                             dest = Set.map (\(AtomE d) -> d) destset
                          in MultiSendEvent r etype msg dest
             RecE msg -> let StrE etype = r!(Ident "etype")
                             RecE msg = r!(Ident "msg")
                          in SingleSendEvent r etype msg
         f (RecE r) | Map.member (Ident "msgs") r =
           let StrE etype = r!(Ident "etype")
               SetE msgsrece = r!(Ident "msgs")
               msgs = Set.map (\(RecE msg) -> msg) msgsrece
            in MultiRecvEvent r etype msgs
         f (RecE r) =
           let StrE etype = r!(Ident "etype")
            in NoMessageEvent r etype

numberlist :: Trace -> LabeledTrace
numberlist l           = numberlist0 (1::Int) l
  where numberlist0 i (h:rest) = (i, h):(numberlist0 (i+1) rest)
        numberlist0 i []       = []

swimlanes :: LabeledTrace -> [Swimlane] -- left to right
swimlanes labeledtrace = Prelude.map
                           (filterandshape labeledtrace)
                           (alllanes labeledtrace)
    where alllanes :: LabeledTrace -> [String]
          alllanes labeledtrace =
            List.nub (Prelude.map (\(_, (EventGrp sl _)) -> sl) labeledtrace)
          filterandshape :: LabeledTrace -> String -> Swimlane
          filterandshape trace slname =
            let lt = Prelude.filter
                       (\(_, (EventGrp sl _)) -> sl == slname)
                       trace
             in (slname, lt)

to_pstricks_swimlanes :: (Maybe StateAnnFun) -> Int ->
                         Abbrev -> Int -> Bool -> Int -> StateNotes ->
                         StateNotes -> [Int] -> [Swimlane] -> [String]
to_pstricks_swimlanes saf cycle abbrev numlanes defaultNotes m notes vertnotes
                      allstateidx sl =
    let alllanes = List.sort $ List.nub $ Prelude.map (\(s, _) -> s) sl
     in concat (Prelude.map
                  (to_pstricks_swimlanes0 saf cycle abbrev defaultNotes m
                                          notes vertnotes alllanes
                                                allstateidx)
                  sl)
    where to_pstricks_swimlanes0 ::
            (Maybe StateAnnFun) -> Int ->
            Abbrev -> Bool -> Int -> StateNotes -> StateNotes ->
            [String] -> [Int] -> Swimlane -> [String]
          to_pstricks_swimlanes0
              saf cycle abbrev defaultNotes m notes vertnotes alllanes
              allstateidx (name, ievents) =
            let Just xpos = case laneorder config of
                  Nothing -> List.elemIndex name alllanes
                  Just lp -> List.elemIndex name lp
                nodes = Prelude.map
                          (\(i, egrp) ->
                              let sli = name
                                  Just index = List.elemIndex i allstateidx
                                  ypos = m-(index+1)
                                  (hasnote, note, size) =
                                      case defaultNotes of
                                        True -> defaultNote abbrev "diff" egrp
                                        False ->
                                           stateAnnHor saf (getRec egrp)
                                  (customstyle,
                                   dotstyle) =
                                     -- FIXME this string matching is evil
                                     -- it's not very obvious what
                                     -- "annotations" are supported.
                                     --    - cross
                                     --    - downtriangle
                                     --    - triangle
                                     if hasEv "cross" egrp
                                     then (True,
                                           "[dotstyle=+,dotscale="++
                                           "2,dotangle=45]")
                                     else if hasEv "downtriangle" egrp
                                          then (True,
                                             "[dotstyle=triangle*,dotscale="++
                                             "2,dotangle=180]")
                                          else if hasEv "triangle" egrp
                                               then (True,
                                                    "[dotstyle=triangle*,"++
                                                    "dotscale=2,dotangle=0]")
                                               else (False, "")
                                  l = ["\\rput [tr]("++(show xpos)++",",
                                      (show ypos)++"){",
                                      "\\rnode{dummy_"++(show i)++"}",
                                      "{\\"++(footnotesize "footnotesize")++
                                      " \\hspace{0pt}",
                                      (show i)++" \\hspace{1pt} }} ",
                                      if customstyle
                                         then "\\dotnode"++dotstyle++"("++
                                                  (show xpos)++","++
                                                  (show ypos)++"){"++
                                                  (ref i sli)++"}"
                                         else "\\dotnode ("++(show xpos)++","++
                                              (show ypos)++"){"++
                                              (ref i sli)++"}"
                                      ]
                                  n = ["\\rput [l]("++ -- use lb for bottom
                                      (show numlanes)++",",
                                      (show ypos)++"){",
                                      "\\rnode{descr_"++(show i)++"}",
                                      "{"++(parboxState size $
                                              (show i)++": "++note)++
                                      "}}"++"\\ncline[linestyle=dotted, ",
                                      "linecolor=lightgray]{"++"-}",
                                      "{"++(ref i sli)++"}{descr_"++
                                           (show i)++"}"]
                                  c = if (index == m-1) -- if,then draw box
                                      then let n = cycle
                                               d = 0.5
                                               xboxl = -d
                                               yboxl = fromIntegral ypos-d
                                               xboxu = fromIntegral
                                                         numlanes-1+d
                                               yboxu = yboxl +
                                                         fromIntegral (n-1)+d+d
                                            in if n > 0
                                               then
                                                 ["\\psframe[linewidth=1pt, "++
                                                  "framearc=0.1, "++
                                                  "linestyle=dashed, "++
                                                  "linecolor=lightgray]("++
                                                  show xboxl++","++
                                                  show yboxl++")("++
                                                  show xboxu++","++
                                                  show yboxu++")"]
                                               else []
                                      else []
                              in case hasnote of
                                   True  -> concat $ l++n++c
                                   False -> concat $ l++c)
                          ievents
                symnodes = Prelude.map (\(i, _egrp) -> i) ievents
                labels = label xpos (fromIntegral m-0.5) name
             in nodes++(pairs vertnotes name symnodes)++labels
          stateAnnHor :: (Maybe StateAnnFun) -> Expr
                      -> (Bool, String, String) -- has-label, label, font
          stateAnnHor Nothing _ = (False, "", "")
          stateAnnHor (Just saf) state =
              case saf state of
                Just dl ->
                  let label = case List.find (\e -> case e of
                                                      (Label _) -> True
                                                      otherwise -> False) dl of
                                Just (Label s) -> s
                                Nothing -> ""
                      lfont = case List.find (\e -> case e of
                                                      (LabelFont _) -> True
                                                      otherwise -> False) dl of
                                Just (LabelFont f) -> f
                                Nothing -> footnotesize "footnotesize"
                   in (label /= "", label, lfont)
                Nothing -> (False, "", "")
          pairs :: StateNotes -> String -> [Int] -> [String]
          pairs vertnotes sli [i]    = []
          pairs vertnotes sli (i:is) =
              let j = head22 is
                  l = "\\ncline[linewidth=3pt,"++"linecolor=lightgray]{"++
                      "-}{"++(ref i sli)++"}{"++(ref j sli)++"}"
                  note = case Map.lookup i vertnotes of
                           Nothing -> ""
                           Just t  -> t
                  t = "\\aput[1pt]{:U}{\\"++defaultnotefont config++" "++
                      note++"}"
               in case Map.lookup i vertnotes of
                    Nothing -> l:(pairs vertnotes sli is)
                    Just _  -> (l++t):(pairs vertnotes sli is)
          -- NOTE: needed when -Dcompact yields no msg exchanges (e.g. if
          -- patterns in sla file are not yet written)
          pairs vertnotes sli [] = []
          label xpos ypos name = ["\\rput[b]("++(show xpos)++","++(show ypos)++
                                 "){\\rnode{swimlane_"++(name)++
                                 "}{\\psframebox{\\"++(footnotesize "footnotesize")++" "++name++"}}}"]
          parboxState :: String -> String -> String
          parboxState size s = "\\parbox[t]{"++(show $ r_label_w_mm config)++
                               "mm}{\\"++size++" "++s++"}"
          getRec :: EventGrp -> Expr
          getRec (EventGrp _ eset) =
              let [(NoMessageEvent map _)] = Set.toList eset -- FIXME rename Ty
               in RecE map
          defaultNote :: Abbrev -> String -> EventGrp -> (Bool, String, String)
          defaultNote abbrev f (EventGrp _ eset) =
              let msgs = Set.fold
                           (\e acc ->
                             case Map.lookup (Ident f) (erec e) of
                               Nothing -> acc
                               Just (StrE s) -> (protectS s):acc)
                           [] eset
               in case msgs of
                    [] -> (False, "", "")
                    [""] -> (False, "", "")
                    otherwise -> let s = join ", " msgs
                                  in (True, s, tiny)
          hasEv :: String -> EventGrp -> Bool
          hasEv et (EventGrp _ egrp) =
              List.any (\ev -> case ev of
                                 (NoMessageEvent _ x) -> x == et
                                 _ -> False)
                       (Set.toList egrp)
          erec :: Event -> OrigEventRecMap
          erec (NoMessageEvent  erec _)     = erec
          erec (SingleSendEvent erec _ _)   = erec
          erec (MultiSendEvent  erec _ _ _) = erec
          erec (MultiRecvEvent  erec _ _)   = erec

toBlackAndWhite :: [ColoredMessageExchange] -> [ColoredMessageExchange]
toBlackAndWhite l = List.map (\(me, (_c,s,a,b,c,d)) ->
                      (me, ("black", s, a,b,c,d))) l

type PlayerGrid = Map Int (Set String)

playerGrid :: [ColoredMessageExchange] -> PlayerGrid
playerGrid l =
  let l' = List.map (\(me, _) ->
             let ((i,  (NoMessageEvent _ sl)),
                  (i', (NoMessageEvent _ sl')), _) = me
              in [(i,  Set.fromList [sl ]),     -- i' because 'next' state
                  (i', Set.fromList [sl'])]) l  -- is used to label transit.
   in Map.fromListWith (\a b -> Set.union a b) $ concat l'

to_pstricks_interactions ::
    Bool -> Int -> MsgNotes -> Abbrev -> [Int] ->
    [ColoredMessageExchange] -> [String]
to_pstricks_interactions defaultNotes numy arrowtips
                         abbrev allstateidx l =
    Prelude.map
      (to_dot_message numy arrowtips abbrev (length l) allstateidx)
      l
    where to_dot_message numy arrowtips abbrev
                         m allstateidx (me, (color, style,
                                             label, labelfont,
                                             tiplabel2, tiplabelfont)) =
              -- FIXME pruge all other Event types!!!
              let ((i, NoMessageEvent _ sli),
                   (j, NoMessageEvent _ slj), m) = me -- do grow top down

                  index = case List.elemIndex j allstateidx of
                            Just i -> i
                            Nothing -> Trace.trace
                                         ("j= "++show j++", allstateidx= "++
                                          show allstateidx) 0
                  ypos = numy-(index+1)
                  -- ypos = numy-j
                  StrE typename = m!(Ident "type")
                  (hasTipLabel, tiplabel, tipSize) =
                      (tiplabel2 /= "", tiplabel2, footnotesize tiplabelfont)
                  success = case Map.member (Ident "success") m of
                              True  -> let AtomE boo = m!(Ident "success")
                                        in " "++(shortform boo abbrev)
                              False -> ""
                  tip = if hasTipLabel
                          then "\\"++tipSize++" "++tiplabel++success
                          else ""
                  l = "\\ncarc[linestyle="++(style)++", linecolor="++(color)++
                       "]{"++"->}{"++(ref i sli)++"}{"++
                                     (ref j slj)++"}"++
                        "\\bput[.5pt](.8){ \\rnode{"++(label_msg "m" i j)++
                        "}{"++tip++"}}"
                  (hasnote, note, size) =
                      (label /= "", label, footnotesize labelfont)
                  t = "\\pnode(-1,"++(show ypos)++"){"++
                       (label_msg "c" i j)++"}"++
                       "\\rput[rt](-1,"++(show ypos)++
                       "){\\Rnode{"++(label_msg "dummy" i j)++
                       "}{"++(parboxMsg size $
                                (show i)++"-"++(show j)++": "++note++"}}")++
                       "\\ncline[linestyle=dotted,dotsize=1.5pt 1, "++
                       "linecolor=lightgray]{"++"-*}{"++
                       (label_msg "c" i j)++"}{"++(label_msg "m" i j)++"}"
               in case hasnote of
                    True  -> l++t
                    False -> l
            where label_msg l i j = l++"_"++(show i)++"_"++(show j)
                  parboxMsg :: String -> String -> String
                  parboxMsg size s =
                      "\\parbox[t]{"++(show $ l_label_w_mm config)++"mm}"++
                      "{\\"++size++" "++s++"}"
                  defaultMsgNote :: Message -> String
                  defaultMsgNote m = protectS (ppRec (RecE m) abbrev)

ref stateidx swimlanename = "ref"++show stateidx++swimlanename

tiny :: String
tiny = defaultnotefont config

footnotesize :: String -> String
footnotesize s = case usernotefont config of
                   Nothing -> s
                   Just f -> f

to_pstricks_graph_open :: Int -> Int -> Bool -> Bool -> String -> [String]
to_pstricks_graph_open n m mnotes snotes issue =
    let hasLeftComment = mnotes || gen_msg_labels config
        hasRightComment = snotes || (gen_state_labels config)
        right_comment_width = case hasRightComment of
                                True -> (xunit_mm config + r_label_w_mm config)
                                False -> 2
        left_comment_width = case hasLeftComment of
                                True -> (xunit_mm config + l_label_w_mm config)
                                False -> 2 -- fluff
        xorigin = -left_comment_width
        yorigin = -4 -- flubber
        xupper  = (xunit_mm config * (n-1)) + right_comment_width
        yupper  = (fromIntegral $ yunit_mm config)*(fromIntegral m-0.5)+
                  5 -- flubber
        lscape = if landscape config then " ,landscape" else ""
        h = ["\\documentclass[10pt"++lscape++"]{article}",
             "\\usepackage{pstricks, pst-node}",
             "\\usepackage[usenames]{color}",
             "\\begin{document}",
             "\\begin{center}",
             "{\\footnotesize",
             "{\\bf Error:} \\begin{verbatim}" ++ issue ++ "\\end{verbatim}",
             if fbox config then "\\fbox{" else ""]
     in (if includefile config then [] else h) ++
         ["\\begin{pspicture}"++
         "("++(show xorigin)++"mm, "++(show yorigin)++"mm)",
         "("++(show xupper)++"mm, "++(show yupper)++"mm)",
         ""]

to_pstricks_graph_preamble :: Int -> Int -> [String]
to_pstricks_graph_preamble _ _ =
    ["\\psset{xunit="++(show $ xunit_mm config)++"mm}",
     "\\psset{yunit="++(show $ yunit_mm config)++"mm}",
     ""]

to_pstricks_graph_close :: [String] -> [String]
to_pstricks_graph_close args =
  let n = 3
      psclosing =
          ["\\end{pspicture}"]
      cmdline = join " " args
      e = [if fbox config then "}" else "",
           "}", -- end \footnotesize
           "\\end{center}",
           "\\begin{verbatim}$ ./sl "++cmdline++"\\end{verbatim}",
           "\\end{document}"]
   in psclosing ++ (if includefile config then [] else e)

indent s l = Prelude.map (\line -> s++line) l

protectS :: String -> String
protectS = protectUS
protectUS = List.map (\c -> case c of
                              '_' -> '-'
                              otherwise -> c)

toInteractions :: [MsgExchange] -> [ColoredMessageExchange]
toInteractions mel = List.map convert mel
    where convert :: MsgExchange -> ColoredMessageExchange
          convert me =
              let ((i,sl), (i',sl'), dl) = me
                  msg = Map.empty -- FIXME remove - not used!
                  mn  = mkMessageNode i  sl  msg
                  mn' = mkMessageNode i' sl' msg
                  exchange = (mn, mn', msg)
                  color = case List.find (\e -> case e of
                                            (Color _) -> True
                                            otherwise -> False) dl of
                            Just (Color c) -> c
                            Nothing -> "black"
                  style = case List.find (\e -> case e of
                                            (Style _) -> True
                                            otherwise -> False) dl of
                            Just (Style s) -> s
                            Nothing -> "solid"
                  label = case List.find (\e -> case e of
                                            (Label _) -> True
                                            otherwise -> False) dl of
                            Just (Label s) -> s
                            Nothing -> ""
                  labelfont = case List.find (\e -> case e of
                                            (LabelFont _) -> True
                                            otherwise -> False) dl of
                            Just (LabelFont s) -> s
                            Nothing -> ""
                  tiplabel = case List.find (\e -> case e of
                                            (TipLabel _) -> True
                                            otherwise -> False) dl of
                            Just (TipLabel s) -> s
                            Nothing -> ""
                  tiplabelfont = case List.find (\e -> case e of
                                            (TipLabelFont _) -> True
                                            otherwise -> False) dl of
                            Just (TipLabelFont s) -> s
                            Nothing -> ""
               in (exchange, (color, style,
                              label, labelfont,
                              tiplabel, tiplabelfont))
            where mkMessageNode :: Int -> String -> Map Ident Expr
                                -> MessageNode
                  mkMessageNode i sl msgmap =
                      let event = NoMessageEvent msgmap sl -- fake event
                       in (i, event)

head2 [] = error "!2"
head2 l = head l
head22 [] = error "!22"
head22 l = head l

textToStates :: String -> (Maybe String, String, [State], Int)
textToStates fc =
    let e = Regex.splitRegex (Regex.mkRegex "State ") fc
        issue = concat $ tail $ Regex.splitRegex (Regex.mkRegex "Error: ")
                                                 (head22 e)
        stl = List.map (\s ->
               let lines2 = -- when Assertion is violated, trunc the line sect.
                     takeWhile (\l -> case l of
                                        ("The error occurred when TLC was evaluating the nested") -> False
                                        _ -> True)
                               (lines s)
                   lines' = List.filter (/=[]) $ lines2 -- drop empty lines
                in case lines' of
                     [header, state] -> ([state], Nothing, 0) -- View used
                     (header:statevars) | statevars /= [] -> -- No view
                        let ls = List.map (Regex.splitRegex
                                             (Regex.mkRegex " = "))
                                     statevars
                            exprs = List.foldl (\acc l ->
                                      case l of
                                        [_v,e] -> acc++","++e
                                        [line1] -> acc++line1)
                                      [] ls
                            vars = List.map (\l -> case l of
                                                     [('/':'\\':' ':v), _e] ->
                                                       v)
                                            ls
                         in (["<<"++exprs++">>"], Just vars, 0)
                     [other] -> -- Loop indication for temporal violation
                       case Regex.splitRegex
                              (Regex.mkRegex ": Back to state ")
                              other of
                         [a,b] ->
                           let a' = read a
                               (b',_) = splitAt
                                          (fromJust $ List.elemIndex '.' b)
                                          b
                               b'' = read b'
                            in ([], Nothing, a' - b'') )
              $ tail e
        st = concat $ List.map (\(l, _, _) -> l) stl
        (_, vars, _) = head22 stl -- all vars are the same
        (_, _, cycle) = last stl
        (errtxt, names) = case vars of
          Just names -> (Nothing, names)
          Nothing -> case tlcview config of
                       Nothing -> (Just "The TLC .cfg file includes a VIEW statement. Swimlane needs to know the value of the VIEW to parse the error trace correctly.  Please use the -w option to supply the list of variable names in the order they appear in the VIEW. For example: VIEW = <<ps,as>> (.cfg file), then use -wps,as", [])
                       Just names -> (Nothing, names)
        st' = List.map (\state ->
                -- FIXME kramer@acm.org reto -- handle case where no VIEW is
                -- active in the .cfg file, i.e. if states are not tuples,
                -- but are /\ rs = ... /\ cs = ...
                let res = exprparser $ alexScanTokens state
                 in case res of
                        SeqE values ->
                              let namevaluepairs = zip names values
                               in concat $ List.map (\(n,v) ->
                                     if n=="_"
                                       then []
                                       else [Bind (Ident n) v]) namevaluepairs
              ) st
        states = zip [0 .. length st'-1] st'
     in (errtxt, issue, states, cycle)

-- FIXME which of these many function are really used (the TLA+ one),
-- remove all others
toSwimlanes :: [State] -> (Maybe StateAnnFun) -> PlayerGrid -> [String] -> [Swimlane]
toSwimlanes states saf grid playernames =
    let scl = List.map (\(s, s') -> (s, s', diffSL s s')) $
                zip states (tail states)
     in groupSL $ concat (List.map (convert (length states) saf
                                            grid playernames) scl)
    where convert :: Int -> (Maybe StateAnnFun) ->
                     PlayerGrid -> [String] -> (State, State, StateChange)
                  -> [Swimlane]
          convert n saf grid playernames (s, s', sc) =
              let (oldi, _) = s
                  (newi, _) = s'
                  (i, dgl) = sc
                  -- honor the -z list of players (and ordering) if present
                  -- if a statechange does not involve sending/receiving a
                  -- message, assume all players are involved since we cannot
                  -- be more specific here.
                  -- FIXME, drive this from the output of the state annotation
                  -- function (saf) below! Writing the saf will be a lot of
                  -- work for the user, so this is a very useful default.
                  -- NOTE the -ldense switch will effectively remove the
                  -- states transitions that are not involved in message
                  -- exchanges.
                  affected = (Set.toList $ Map.findWithDefault
                                             (Set.fromList playernames)
                                             (oldi+1) grid)
                  -- List.intersection preserves ordering in playernames
                  sls = List.intersect playernames affected
                  ddl = List.filter
                          (\dd -> not (includesAnyPathRef hidediff dd))
                          (diffdescr sc)
                  value = ppDL ddl
                  l = [(Ident "diff", StrE (join ", " value))
                      ,(Ident "oldstate",    RecE $ stateToRec s )
                      ,(Ident "newstate",    RecE $ stateToRec s')
                      ,(Ident "state",       RecE $ stateToRec s')
                      ,(Ident "oldstateidx", IntE oldi)
                      ,(Ident "newstateidx", IntE newi)]
                  map = Map.fromList l
                  (ann, hidediff) = stateAnn saf $ RecE map
                  ev = NoMessageEvent map ann
               in List.map (\sl ->
                    (sl, [(i, EventGrp sl (Set.singleton ev))])) sls
          stateToRec :: State -> Map Ident Expr
          stateToRec (_, statelist) = Map.fromList $
              List.map (\(Bind ident expr) -> (ident, expr)) statelist
          idxsl :: [MsgExchange] -> [(Int, SwimlaneName)]
          idxsl mel = let l = List.map (\(p,p',_) -> [p, p']) mel
                       in concat l
          groupSL :: [Swimlane] -> [Swimlane]
          groupSL sll =
              let allsl = List.nub $ List.map (\(sl, _) -> sl) sll
               in List.map (\sl -> filter sl sll) allsl
             where filter :: SwimlaneName -> [Swimlane] -> Swimlane
                   filter slname sll =
                       let fl = List.filter (\(sl, _grp) -> sl == slname) sll
                           grps = concat $ List.map (\(i, grpl) -> grpl) fl
                        in (slname, grps)
          diffdescr :: StateChange -> [DiffDescr]
          diffdescr (_i, sc) =
             concat $ List.map (\(_id, dg) -> dg) sc
          stateAnn :: (Maybe StateAnnFun) -> Expr -> (String, [String])
          stateAnn Nothing _ = ("", []) -- don't care
          stateAnn (Just saf) rec =
              case saf rec of
                Just dl ->
                  let look = case List.find (\e -> case e of
                                                      (Style _) -> True
                                                      otherwise -> False) dl of
                                Just (Style s) -> s
                                Nothing -> ""
                      hdiff = case List.find (\e -> case e of
                                                      (HideDiff _) -> True
                                                      otherwise -> False) dl of
                                Just (HideDiff sl) -> sl
                                Nothing -> []
                   in (look, hdiff)
                Nothing -> ("", [])
