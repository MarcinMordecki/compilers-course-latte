module RemoveJumpingBlocks where
import CFG
import IR
import qualified Data.Map as M
import System.IO


type MergeBlockLink = (CFGBlock, CFGBlock, CFGBlock) -- removed, inlink, outlink


getJumping :: CFGBMap -> [CFGBlock] -> IO (Maybe MergeBlockLink)
getJumping _ [] = return Nothing
getJumping bmap (b:bt) = do
  case outLinks b of
    Single blink -> do
      case M.lookup blink bmap of
        Just bnext -> do
          mblink <- checkRelay bmap b bnext 
          case mblink of
            Just mergeBlockLink -> return $ Just mergeBlockLink
            Nothing -> getJumping bmap bt
        _ -> fail "impossible branch in getJumping" 
    _ -> getJumping bmap bt
  where
    checkRelay :: CFGBMap -> CFGBlock -> CFGBlock -> IO (Maybe MergeBlockLink)
    checkRelay bmap bold bmid =
      if length (quads bmid) /= 1 || length (inLinks bmid) /= 1 then do
        return Nothing else
        case outLinks bmid of
          Single labelBlast -> case M.lookup labelBlast bmap of
            Just blast -> if length (inLinks blast) == 1 then return $ Just (bmid, bold, blast) else return Nothing
            Nothing -> return Nothing
          _ -> return Nothing

removeJumpingBlocks :: CFGBMap -> IO (Bool, CFGBMap)
removeJumpingBlocks bmap = do
  jump <- getJumping bmap $ map snd $ M.toList bmap
  case jump of
    Nothing -> return (False, bmap)
    Just triplet -> do
      let (removed, first, last) = triplet
      let bmap0 = M.insert (label first) (fixOutlinks triplet) bmap
      let bmap1 = M.insert (label last) (fixInlinks triplet) bmap0
      let bmap2 = M.delete (label removed) bmap1
      if bmap == bmap2 then return (False, bmap) else do
        (_, bmap3) <- removeJumpingBlocks bmap2
        return (True, bmap3)
  where
  fixOutlinks :: MergeBlockLink -> CFGBlock
  fixOutlinks (removed, first, last) =
    case head $ quads first of
      IExpr l (IBr0 a) -> first {quads = IExpr l (IBr0 (label last)) : tail (quads first), outLinks = Single (label last)}
      _ -> first -- Impossible
  changePhis :: Blabel -> Blabel -> IExpr -> IExpr
  changePhis lremoved lfirst (IExpr l r) = IExpr l (case r of
    IPhi l t phiList -> IPhi l t $ map (\(ll, e) -> if ll == lremoved then (lfirst, e) else (ll, e)) phiList
    IAOrPhi phiList -> IAOrPhi $ map (\(ll, e) -> if ll == lremoved then (lfirst, e) else (ll, e)) phiList
    e -> e
    )
  fixInlinks :: MergeBlockLink -> CFGBlock
  fixInlinks (removed, first, last) = last {
    inLinks = [label first],
    quads = map (changePhis (label removed) (label first)) $ quads last
    }


