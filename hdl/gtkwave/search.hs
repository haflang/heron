import           Data.List
import           Data.Maybe
import           System.Process

{-

We check for a bad qualities, but not all of them

Included:

  + Reads of unallocated addresses before an allocation
  + Freeing of an unallocated address
  + Allocation twice before a feww
  + Updates after a free

Unchecked:
  + If we wrongly free an address, it is likely to be reallocated very quickly. If we then dereference, we can't tell.

To solve the above we can try to dynamically check this in our circuit. Add an
extra memory saying which GC pass a node was allocated after. Pointers also have
this, and the two are compared during dereferencing

-}

type Cycle = Int
type Addr  = Int
data Err = Err Cycle Addr String

instance Show Err where
  show (Err c a msg) = unwords ["cycle", show c, " @", show a, " ", msg]

parse f = readFile f >>= return . map ((\(a,b) -> (read a :: Int, read b :: Int)) . span (/=' ')) . lines

analyse = do
  fs <- parse "/tmp/ts_frees"
  us <- parse "/tmp/ts_updates"
  rs <- parse "/tmp/ts_reads"
  as <- parse "/tmp/ts_allocs"
  print $ checkUpdateAfterFree fs us
  print $ checkReadAfterFree fs rs as

checkReadAfterFree :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [Err]
checkReadAfterFree fs rs as = catMaybes $ map (danger fs rs as) (nub (map snd rs))
  where
    danger :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> Int -> Maybe Err
    danger fs rs as a =
      let fcs = map fst $ filter ((==a) . snd) fs
          rcs = map fst $ filter ((==a) . snd) rs
          acs = map fst $ filter ((==a) . snd) as
      in check a fcs rcs acs
    -- Expecting 1 alloc, optional multiple reads, one free. Repeat
    -- As we consume, check alloc before free
    --              ,       alloc before read (drop all reads up until next free)
    check _ fs [] as = Nothing -- All reads were good
    check x fs _  [] = Just $ Err (head fs) x "Reads after last phase"
    check x fs (r:rs) (a:as)
      | r < a = Just $ Err r x "Read before allocation"
      | a > f = Just $ Err a x "Free before allocation"
      | a' < f = Just $ Err f x "Double allocation"
      | r > f = check x (drop 1 fs) (r:rs) (dropWhile (<f) as) -- Look at next phase
      | otherwise = check x fs rs (a:as)
      where
        f  = case fs of { [] -> maxBound; (x:_) -> x }
        a' = case as of { [] -> maxBound; (x:_) -> x }

checkUpdateAfterFree fs us = filter (danger fs us) (nub (map snd us))
  where
    danger fs us a =
      let fcs = map fst $ filter ((==a) . snd) fs
          ucs = map fst $ filter ((==a) . snd) us
      in check fcs ucs
    check [] ucs = length ucs > 1
    check (c:cs) ucs = length (filter (<=c) ucs) > 1 || check cs (filter (c>) ucs)

main = do
  sequence_ [print (show (x,y)) >> processFile x y >> analyse | x<-[0..0], y<-[0..0]]
  -- sequence_ [print (show (x,y)) >> processFile x y >> analyse | x<-[0..1], y<-[0..3]]

processFile x y = do
  system $ "grep \"RamWrite [0-9]* (Free\" /tmp/heron_core_\\(" ++ show x ++ "," ++ show y ++ "\\).log | sed 's/\\([0-9]*\\).*RamWrite \\([0-9]*\\) (Free.*/\\1 \\2/' > /tmp/ts_frees"
  system $ "grep \"upd   = Just\" /tmp/heron_core_\\(" ++ show x ++ "," ++ show y ++ "\\).log | sed 's/  upd   = Just //' > /tmp/ts_updates"
  system $ "grep \"_top' = Ptr\"  /tmp/heron_core_\\(" ++ show x ++ "," ++ show y ++ "\\).log | sed 's/\\([0-9]*\\).* _top. = Ptr [0-9]* [a-z,A-Z]* \\([0-9]*\\).*/\\1 \\2/' > /tmp/ts_reads"
  system $ "grep -B2 \"gcReq = RAlloc\" /tmp/heron_core_\\(" ++ show x ++ "," ++ show y ++ "\\).log | grep \"RamWrite.*Ram\" | sed 's/\\([0-9]*\\) .* RamWrite \\([0-9]*\\) .* Ram.*/\\1 \\2/' > /tmp/ts_allocs1"
  system $ "grep -B2 \"gcReq = RAlloc\" /tmp/heron_core_\\(" ++ show x ++ "," ++ show y ++ "\\).log | grep \"Ram.*RamWrite\" | sed 's/\\([0-9]*\\) .*Ram.* RamWrite \\([0-9]*\\) .*/\\1 \\2/' > /tmp/ts_allocs2"
  system $ "cat /tmp/ts_allocs1 /tmp/ts_allocs2 | sort -n > /tmp/ts_allocs"
