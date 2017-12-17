module Car where

import System.IO
import Data.List

-- 竞拍者，前一个是身份证号，后一个是出价
type Bidder = (Integer, Integer)

getBidderFromStr :: String -> Bidder
getBidderFromStr str = (f, s)
  where
    f = read $ take 16 str
    s = read $ drop 19 str

-- 读取一个Bidder
readBidder :: Handle -> IO Bidder
readBidder pf = do
  str <- hGetLine pf
  return $ getBidderFromStr str

-- 读取Bidder列表
readBidders :: Handle -> IO [Bidder]
readBidders pf = do
  eof <- hIsEOF pf
  if eof
    then return []
    else do
      b <- readBidder pf
      bs <- readBidders pf
      return $ b:bs

-- 求平均数
ave :: [Bidder] -> Float
ave bs = fromInteger $ sum sndbs `div` toInteger (length bs)
  where sndbs = [y | (_, y) <- bs]

-- 根据给的数据，向另一个文件写信息
writeInfomation :: Handle -> [Bidder] -> IO ()
writeInfomation pf fbs = let bs = take 10 fbs in do
  hPutStrLn pf $ "最高成交价：" ++ (show.snd.head) bs
  hPutStrLn pf $ "最低成交价：" ++ (show.snd.last) bs
  hPutStrLn pf $ "平均成交价：" ++ (show.ave) bs
  hPutStrLn pf $ "总共有" ++ (show.length) fbs ++ "参与竞价"
  writeBidders pf bs

-- 写Bidder列表
writeBidders :: Handle -> [Bidder] -> IO ()
writeBidders _ [] = return ()
writeBidders pf ((h, t):bs) = do
  hPutStrLn pf $ show h ++ "   " ++ show t
  writeBidders pf bs

-- 整个加工过程
processCar :: IO ()
processCar = do
  -- 读取文件
  pfr <- openFile "bids_201711.txt" ReadMode
  bs <- readBidders pfr
  hClose pfr
  -- 写文件
  pfw <- openFile "bidResults1.txt" WriteMode
  writeInfomation pfw $ sortBy (\(_, p1) (_, p2) -> compare p2 p1) bs
  hClose pfw

main :: IO ()
main = putStrLn "HelloWorld!"
