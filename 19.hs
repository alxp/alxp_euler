
-- Weekday (0-7)

data Weekday = Sunday | Monday | Tuesday | Wednesday
             | Thursday | Friday | Saturday
             deriving (Show, Eq, Enum)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
           deriving (Show, Eq, Enum)

type Year = Int
type Day = Int

data Date = Date Weekday Year Month Day
          deriving (Show)

nextWeekday :: Weekday -> Weekday
nextWeekday Saturday = Sunday
nextWeekday d = succ d

nextMonth :: Month -> Month
nextMonth Dec = Jan
nextMonth m = succ m

lastDayOfMonth :: Date -> Bool
lastDayOfMonth (Date w y m d) = d == lengthOfMonth (Date w y m d)

lastDayOfYear :: Date -> Bool
lastDayOfYear (Date w y m d) = m == Dec && lastDayOfMonth (Date w y m d)

incrementDate :: Date -> Date
incrementDate (Date w y m d) = (Date w' y' m' d')
  where w' = nextWeekday w
        y' = if lastDayOfYear (Date w y m d)
             then y + 1
             else y
        m' = if lastDayOfMonth (Date w y m d)
             then nextMonth m
             else m
        d' = if lastDayOfMonth (Date w y m d)
             then 1
             else d + 1

lengthOfMonth :: Date -> Int
lengthOfMonth (Date  _ y m _)
            | elem m [Jan, Mar, May, Jul, Aug, Oct, Dec] = 31
            | elem m [Apr, Jun, Sep, Nov] = 30
            | (m == Feb) && (y `mod` 4 == 0) && ((y `mod` 100 /= 0) || (y `mod` 400 == 0)) = 29
            | m == Feb = 28

countSundays :: Date -> Int
countSundays (Date w y m d)
  | y >= 2001 = 0
  | y == 1900 = n
  | w == Sunday && d == 1 = 1 + n
  | otherwise = n
  where n = countSundays (incrementDate (Date w y m d))
