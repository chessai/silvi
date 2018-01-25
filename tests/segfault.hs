{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

type SegfaultLog = '[ 'FieldTimestamp ]

type OffsetLog = '[ 'FieldOffset ]

type DatetimeLog = '[ 'FieldDatetime ]

type DateLog = '[ 'FieldDate ]

type YearLog = '[ 'FieldYear ]

type MonthLog = '[ 'FieldMonth ]

type DayOfMonthLog = '[ 'FieldDayOfMonth ]

type TimeOfDayLog = '[ 'FieldTimeOfDay ]

segfault :: IO ()
segfault = do
  Prelude.putStrLn "Starting..." 
  let r :: Silvi SegfaultLog 
      r = randLog @SegfaultLog
  Prelude.putStrLn "running pprint"
  let t :: Gen (IO ()) 
      t = fmap (Topaz.traverse_ E.print) r
  Prelude.putStrLn "sampling..."
  let k :: IO (IO ()) 
      k = sample t
  join k

msPrint :: String -> IO ()
msPrint x = do
  Prelude.putStrLn "================================"
  replicateM_ ((32 - length x) `div` 2) (Prelude.putStr "=") 
  Prelude.putStr x
  replicateM_ ((32 - length x) `div` 2) (Prelude.putStr "=") 
  Prelude.putStrLn "" 
  Prelude.putStrLn "================================"

success :: String -> IO ()
success x = do
  Prelude.putStrLn ""
  Prelude.putStrLn $ "Succeeded: " ++ x

msOffset :: IO ()
msOffset = do 
  msPrint "Offset" 
  let o :: Silvi OffsetLog 
      o  = randLog @OffsetLog
      ot :: Gen (IO ()) 
      ot = fmap (Topaz.traverse_ E.print) o
      ok :: IO (IO ()) 
      ok = sample ot
  join ok 
  success "Offset"

msDatetime :: IO ()
msDatetime = do
  msPrint "Datetime" 
  let o :: Silvi DatetimeLog 
      o  = randLog @DatetimeLog
      ot :: Gen (IO ()) 
      ot = fmap (Topaz.traverse_ E.print) o
      ok :: IO (IO ()) 
      ok = sample ot
  join ok 
  success "Datetime"

msDate :: IO ()
msDate = do
  msPrint "Date" 
  let o :: Silvi DateLog 
      o  = randLog @DateLog
      ot :: Gen (IO ()) 
      ot = fmap (Topaz.traverse_ E.print) o
      ok :: IO (IO ()) 
      ok = sample ot
  join ok 
  success "Date"

msTimeOfDay :: IO ()
msTimeOfDay = do
  msPrint "TimeOfDay" 
  let o :: Silvi TimeOfDayLog 
      o  = randLog @TimeOfDayLog
      ot :: Gen (IO ()) 
      ot = fmap (Topaz.traverse_ E.print) o
      ok :: IO (IO ()) 
      ok = sample ot
  join ok 
  success "TimeOfDay"

msYear :: IO ()
msYear = do
  msPrint "Year" 
  let o :: Silvi YearLog 
      o  = randLog @YearLog
      ot :: Gen (IO ()) 
      ot = fmap (Topaz.traverse_ E.print) o
      ok :: IO (IO ()) 
      ok = sample ot
  join ok 
  success "Year"

msMonth :: IO ()
msMonth = do
  msPrint "Month"
  let o :: Silvi MonthLog 
      o  = randLog @MonthLog
      ot :: Gen (IO ()) 
      ot = fmap (Topaz.traverse_ E.print) o
      ok :: IO (IO ()) 
      ok = sample ot
  join ok 
  success "Year"

msDayOfMonth :: IO ()
msDayOfMonth = do
  msPrint "DayOfMonth"
  let o :: Silvi DayOfMonthLog 
      o  = randLog @DayOfMonthLog
      ot :: Gen (IO ()) 
      ot = fmap (Topaz.traverse_ E.print) o
      ok :: IO (IO ()) 
      ok = sample ot
  join ok 
  success "DayOfMonth"

maybeSegfault :: IO ()
maybeSegfault = do
  msDayOfMonth
  msMonth
  msYear
  msTimeOfDay
  msDate
  msDatetime

main :: IO ()
main = maybeSegfault
