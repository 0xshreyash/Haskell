de :: DFA
de
  = prod d e


de_test_accept_cases
  = ["aba", "abbba", "abbba",  "abcbabcbabcba", "abbbabbba",
     "abcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcba"]

de_test_accept :: [String]
de_test_accept
  = ["If anything is listed after this, the DFA failed that test. IF nothing is listed, the DFA passed all tests!"] ++ [input  | input <- de_test_accept_cases,  (run_DFA de input) == Reject]


de_test_reject_cases
  = ["",
     "a", "b", "c",
     "aa", "ab", "ac", "ba", "bb", "bc", "ca", "cb", "cc",
     "aaa", "aab", "aac", "abb", "abc", "baa", "bab", "bac", "bba", "bbb", "bbc", "bca", "bcb", "bcc", "caa", "cab", "cac", "cba", "cbb", "cbc", "cca", "ccb", "ccc",
     "abba", "abbcba", "abbbbba", "abcbabcbabacba", "ababa", "abcbaabcbaabcbaba",
     "abbb","abbc","abcb","abcc","acbb","acbc","accb","accc",
     "babb","babc","bacb","bacc","cabb","cabc","cacb","cacc",
     "bbab","bbac","bcab","bcac","cbab","cbac","ccab","cacc",
     "bbba","bbca","bcba","bcca","cbba","cbca","ccba","ccca",
     "abcbcba", "ababa",
     "abbbabbbab", "abbbabbbabb", "abbbabbbabbb",
     "babbbabbba", "babbbabbbab", "babbbabbbabb", "babbbabbbabbb",
     "bbabbbabbba", "bbabbbabbbab", "bbabbbabbbabb", "bbabbbabbbabbb",
     "abcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbab",
     "abcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabc",
     "abcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcbabcb"]


de_test_reject :: [String]
de_test_reject
  = ["If anything is listed after this, the DFA failed that test. IF nothing is listed, the DFA passed all tests!"] ++ [input  | input <- de_test_reject_cases,  (run_DFA de input) == Accept]
