import Data.List

largest = 999 * 999


isPalindrome :: Int -> Bool
isPalindrome n = show n == (reverse $ show n)


largestPalindrome :: Int
largestPalindrome = head $ reverse $ sort [m * n| m <- reverse [1..999], n <- reverse[1..999], isPalindrome (m * n)]