
import Data.Char (isDigit)
import Data.String
import Data.List (sort, sortBy, elemIndex, findIndex, intersect)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Text.Parsec
import Data.Char (digitToInt)
import Sound.Tidal.Pattern
import Sound.Tidal.Params (midinote)
import Sound.Tidal.Parse (Parseable,p, parseRhythm)
import Text.Parsec.Token
import Data.Foldable (toList)

type Interval = Int

data Scale = Scale {scaleRoot :: Note,  scaleType :: ScaleType} deriving (Show)
data ScaleType = Ionian | Dorian | Phrygian | Lydian | Myxolidian | Aeolian | Locrian | Major | Minor deriving (Show, Eq)

intervals :: ScaleType -> [Interval]
intervals scale = take 7 $ drop rank $ cycle intervalsIonian
  where
    rank= fromEnum scale
    intervalsIonian = [2,2,1,2,2,2,1]

instance Parseable Chord where
  p = parseRhythm pChord

instance Show Chord where
  show (Chord s o d ci) = show s ++ "-" ++ show o ++ "-" ++ show d

pChord = fmap (pure) parseChord

data Intervals = FixedInterval ChordType | InDegInterval [Int] | Inversion Intervals

data Chord = Chord { scale :: Scale,
                     octa :: Int,
                     deg :: Int,
                     chordInts:: Intervals}

intvs :: Intervals -> ScaleType -> Int -> [Int]
intvs (FixedInterval cT) s deg  = fmap (+offset) (intChord cT)
                                    where offset = intFromRoot s deg
intvs (InDegInterval degTab) s deg =  fmap (+offset) $ 0 : fmap (intFromDegre s deg) degTab
                                          where offset = intFromRoot s deg
intvs (Inversion i) s deg  = inv $ intvs i s deg
                                  where inv [] = []
                                        inv (i:reste) =  reste ++ [i+12]

instance Enum ScaleType where
    fromEnum = fromJust . flip lookup table
    toEnum = fromJust . flip lookup (map swap table)
table = [(Ionian, 0),(Major,0),(Dorian,1),(Phrygian,2),(Lydian,3),(Myxolidian,4),(Aeolian,5),(Minor,5),(Locrian,6)]

data ChordType = Maj | Min | Maj7 | Min7 | Dom7 | Maj6 | Min6 | Five | Sus4 | Sus2 | MajorAdd2 | MinorAdd2 | Dom9 | Dom13 | Min9 | Maj9 | Maj13 | Min11 deriving (Show,Eq,Read)

intChord :: ChordType -> [Interval]
intChord Five=[0,intFromRoot Major 5]
intChord Maj = (intChord Five) ++ [intFromRoot Major 3]
intChord Min = (intChord Five) ++ [(intFromRoot Minor 3)]
intChord Maj7 = (intChord Maj) ++ [(intFromRoot Major 7)]
intChord Min7 = (intChord Min) ++ [(intFromRoot Minor 7)]
intChord Dom7 = (intChord Maj) ++ [(intFromRoot Minor 7)]
intChord Maj6 = (intChord Maj) ++ [(intFromRoot Major 6)]
intChord Min6 = (intChord Min) ++ [(intFromRoot Major 6)]
intChord Sus4 = (intChord Five) ++ [(intFromRoot Major 4)]
intChord Sus2 = (intChord Five) ++ [(intFromRoot Major 2)]
intChord MajorAdd2 = (intChord Maj) ++ [(intFromRoot Major 2)]
intChord MinorAdd2 = (intChord Min) ++ [(intFromRoot Minor 2)]
intChord Dom9 = (intChord Dom7) ++ [(intFromRoot Major 9)]
intChord Dom13 = (intChord Dom7) ++ [(intFromRoot Major 13)]
intChord Min9 = (intChord Min7) ++ [(intFromRoot Minor 9)]
intChord Min11 = (intChord Min7) ++ [(intFromRoot Minor 11)]
intChord Maj9 = (intChord Maj7) ++ [(intFromRoot Major 9)]
intChord Maj13 = (intChord Maj7) ++ [(intFromRoot Major 13)]

genInts :: ScaleType -> Int -> [Int] -> [Interval]
genInts s degre degTab  = fmap (+offset) $ 0 : fmap (intFromDegre s degre) degTab
                          --where offset = maximum [0,(degre-1)]
                          where offset = intFromRoot s degre

intFromRoot :: ScaleType -> Int -> Interval
intFromRoot s degre = intFromDegre s 1 degre

intFromDegre :: ScaleType -> Int -> Int -> Interval
intFromDegre s degInScale degre
           | degre <= 1 = 0
           | otherwise = foldr (+) 0 (take (degre-1) $ drop (degInScale-1) $ (cycle $ intervals s))

type Octave = Int

mkChordFromDegres s o d degs = Chord s o d (InDegInterval degs)
mkChordFromType   s o d cType = Chord s o d (FixedInterval cType)

toPat :: Chord -> Pattern Int
toPat c = mconcat $ fmap (return) (chordToNotes c)

toPatt :: Pattern Chord -> Pattern Int
toPatt (Pattern f)  = Pattern $ \(s,e) -> flatMap g (f (s,e))
                  where g (a1,a2,v) = fmap (\x -> (a1,a2,x)) (chordToNotes v)
                        flatMap f = concatMap (Data.Foldable.toList . f)

midiChord c  = midinote $ toPatt c

chordToNotes :: Chord -> [Int]
chordToNotes c  = fmap (+root) (sort ints)
                          where ints = intvs (chordInts c) (scaleType sc) (deg c)
                                root = (noteV $ scaleRoot sc) + 24 + 12*(octa c)
                                sc = scale c

inversion :: Chord -> Chord
inversion c =  c { chordInts= Inversion (chordInts c) }

-- voisin :: cycle des quinte pour les # / cycle des quartes pour les b
voisin :: Note -> Int -> Note
voisin n i
          | i<0 = Note $ ((noteV n) + quarteJuste*(-i)) `mod` 12
          | i>=0 = Note $ ((noteV n) + quinteJuste*i) `mod` 12
          where
           quinteJuste = intFromRoot Major 5
           quarteJuste = intFromRoot Major 4

-- relative :: fonction retourne la tonalite relative scale 1 -> scale 2
relative :: ScaleType -> ScaleType -> Note -> Note
relative sc1 sc2 n = Note $ ((noteV n)  + intFromRoot sc1 (1+offset)) `mod` 12
                   where
                     offset = (7 + (fromEnum sc2) - (fromEnum sc1)) `mod` 7

majToMin n = relative Major Minor n
minToMaj n = relative Minor Major n

data Degre = Degre Int deriving Show
data Note = Note {noteV :: Int} deriving Eq
instance Show Note where
  show (Note a) = noteNames !! ( a `mod` 12)
    where
      noteNames = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

instance Read Note where
        readsPrec _ s =
          case (lookup s noteStrings) of
            Just i -> [(Note i, "")]
            Nothing -> error ("Invalid note: '" ++ s ++ "'")
          where noteStrings = [("C",0), ("C#",1), ("Db",1), ("D",2), ("D#",3), ("Eb",3), ("E",4), ("F",5), ("F#",6), ("Gb",6), ("G",7), ("G#",8), ("Ab",8), ("A",9), ("A#",10), ("Bb",10), ("B",11)]

instance IsString Note where
 fromString s  = either (\x -> error "invalide Note") id $ parse parseNote "" s

instance IsString Chord where
 fromString s  = either (\x -> error "invalide Chord") id $ parse parseChord "" s

parseScaleOctaveDegre :: Parsec String () (Scale,Int,Int)
parseScaleOctaveDegre = do
                        note <- parseNote
                        o <- digit
                        deg <- ((try (char '°' >> digit)) <|> return '1')
                        sType <- parseScaleType
                        return (Scale {scaleRoot = note, scaleType = sType},digitToInt o,digitToInt deg)


parseChord :: Parsec String () (Chord)
parseChord = do
            (sc,oct,deg) <- parseScaleOctaveDegre
            rel <- parseRelative
            let scRel = maybe sc (relScale sc) rel

            vois <- parseVoisins
            let scFinal = voisinScale scRel vois

            desc <- (try parseChordDesc) <|> return (Right [])
            nbInvs <- (try parseInversions) <|> return 0

            let myChord = case (desc) of
                           Left cType -> mkChordFromType scFinal oct deg cType
                           Right cInChords ->mkChordFromDegres scFinal oct deg cInChords
            return (repInv nbInvs myChord)
            where repInv 0 ch = ch
                  repInv n ch = repInv (n-1) (inversion ch)

voisinScale sc1 i = sc1 { scaleRoot=(voisin (scaleRoot sc1) i ) }
relScale sc1 scType  = sc1 { scaleRoot=(relative (scaleType sc1) scType (scaleRoot sc1)), scaleType=scType }

parseInversions :: Parsec String () Int
parseInversions = length <$> (many1 (char '!'))

parseChordDesc :: Parsec String () (Either ChordType [Int])
parseChordDesc = do
                  char '|'
                  (Left <$> parseChordType) <|> (Right <$> parseChordsInScale)

parseChordsInScale :: Parsec String () [Int]
parseChordsInScale = do
                       (fmap (read)) <$> (many1 digit) `sepBy` (char '-')

parseChordType :: Parsec String () ChordType
parseChordType =  (read) <$> ((try (string "Maj13")) <|>
                              (try (string "Maj13")) <|>
                              (try (string "Maj7")) <|>
                              (try (string "Maj6")) <|>
                              (try (string "Maj")) <|>
                              (try (string "Min11")) <|>
                              (try (string "Min9")) <|>
                              (try (string "Min7")) <|>
                              (try (string "Min6")) <|>
                              (try (string "Min")) <|>
                              (try (string "Dom13")) <|>
                              (try (string "Dom9")) <|>
                              (try (string "Dom7")) <|>
                              (try (string "Sus4")) <|>
                              (try (string "Sus2")) <|>
                              (try (string "MajorAdd2")) <|>
                              (try (string "MinorAdd2")) <|>
                              string "Five")


parseVoisins :: Parsec String () Int
parseVoisins = (try (length <$> (many1 (char '+')))) <|>
                (try ((*(-1)).length <$> (many1 (char '-')))) <|> return 0

parseRelative :: Parsec String () (Maybe ScaleType)
parseRelative =  try (Just <$> (char '>' >> parseScaleType)) <|> return Nothing

parseScaleType :: Parsec String () ScaleType
parseScaleType =  do
                  (try (string "Major") >> return Major) <|>
                   (try (string "Minor") >> return Minor) <|>
                   (try (string "Myxolidian") >> return Myxolidian) <|>
                   (char 'M' >> return Major) <|>
                   (char 'm' >> return Minor) <|>
                   (try (string "Ionian") >> return Ionian) <|>
                   (try (string "Dorian") >> return Dorian) <|>
                   (try (string "Lydian") >> return Lydian) <|>
                   (try (string "Locrian") >> return Locrian) <|>
                   (try (string "Aeolian") >> return Aeolian) <|>
                   (try (string "Phrygian") >> return Phrygian)


parseNote:: Parsec String () Note
parseNote = do
            thenote <- oneOf notes
            alteration <- (try (char 'b') >> return (-1)) <|> (try (char '#') >> return 1) <|> return 0
            return (Note $ noteValue thenote alteration)
            where
              index thenote =  fromJust $ elemIndex thenote notes
              notes = "CDEFGAB"
              noteValue thenote alteration = ((intFromRoot Major ((index thenote) +1)) + alteration) `mod` 12
