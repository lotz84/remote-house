module Main where

baseLayer :: Array (Array (Array Int))
baseLayer =
  let g = [0, 4]
      o = [0, 59]
      t = [0, 60]
      l = [0, 61]
      b = [0, 62]
      a = [1, 60]
      c = [1, 61]
      d = [1, 62]
      i = [2, 59]
      e = [2, 60]
      f = [2, 61]
      h = [2, 62]
      j = [2, 49]
   in [ [g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g]
      , [g, o, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, i, g]
      , [g, o, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, i, g]
      , [g, o, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
      , [g, t, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, e, g]
      , [g, l, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, f, g]
      , [g, b, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, h, g]
      , [g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g]
      ]

objectLayer :: Array (Array (Array Int))
objectLayer =
  let o = []
      a = [4, 104]
      b = [5, 104]
      c = [5, 105]
      d = [5, 106]
      e = [6, 104]
      f = [6, 105]
      g = [6, 106]
      h = [7, 104]
      i = [7, 105]
      j = [7, 106]
      k = [7, 129]
      l = [7, 130]
      m = [0, 98]
      n = [0, 99]
      p = [1, 100]
      q = [2, 98]
      s = [1, 123]
      t = [0, 123]
      u = [3, 125]
      v = [5, 125]
      r = [3, 131]
      w = [3, 132]
   in [ [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, k, o, o, m, s, t, t, u, u, u, v, v, s, o, o]
      , [o, o, o, o, o, a, o, a, o, a, o, a, o, a, o, o, o, l, o, o, n, a, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, b, e, e, e, e, e, e, e, e, e, h, o, o, o, o, a, n, o, o, a, o, o, o, a, o, o, o, o]
      , [o, o, o, a, c, f, f, f, f, f, f, f, f, f, i, a, o, k, o, o, p, q, q, q, q, q, q, q, q, q, o, o]
      , [o, o, o, o, c, f, f, f, f, f, f, f, f, f, i, o, o, l, o, o, a, o, a, o, a, o, a, o, a, o, o, o]
      , [o, o, o, a, c, f, f, f, f, f, f, f, f, f, i, a, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, d, g, g, g, g, g, g, g, g, g, j, o, o, k, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, o, a, o, a, o, a, o, a, o, a, o, o, o, l, o, o, a, b, h, o, o, a, b, h, o, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, d, j, a, o, o, d, j, a, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, k, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, a, o, a, o, a, o, a, o, a, o, a, o, o, l, o, o, o, o, a, o, r, o, a, o, o, o, o, o]
      , [o, o, o, o, b, e, e, e, h, o, b, e, e, e, h, o, o, o, o, o, o, o, o, o, w, o, o, o, o, o, o, o]
      , [o, o, o, o, d, g, g, g, j, o, d, g, g, g, j, o, o, k, o, o, o, o, a, o, o, o, a, o, o, o, o, o]
      , [o, o, o, o, a, o, a, o, a, o, a, o, a, o, a, o, o, l, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
      ]

seatMap :: { "0" :: { "0" :: Array Int
         , "1" :: Array Int
         , "10" :: Array Int
         , "11" :: Array Int
         , "12" :: Array Int
         , "13" :: Array Int
         , "2" :: Array Int
         , "3" :: Array Int
         , "4" :: Array Int
         , "5" :: Array Int
         , "6" :: Array Int
         , "7" :: Array Int
         , "8" :: Array Int
         , "9" :: Array Int
         }
, "1" :: { "0" :: Array Int
         , "1" :: Array Int
         , "2" :: Array Int
         , "3" :: Array Int
         , "4" :: Array Int
         , "5" :: Array Int
         }
, "2" :: { "0" :: Array Int
         , "1" :: Array Int
         , "2" :: Array Int
         , "3" :: Array Int
         , "4" :: Array Int
         , "5" :: Array Int
         }
, "3" :: { "0" :: Array Int
         , "1" :: Array Int
         , "2" :: Array Int
         , "3" :: Array Int
         , "4" :: Array Int
         , "5" :: Array Int
         , "6" :: Array Int
         , "7" :: Array Int
         , "8" :: Array Int
         }
, "4" :: { "0" :: Array Int
         , "1" :: Array Int
         }
, "5" :: { "0" :: Array Int
         , "1" :: Array Int
         }
, "6" :: { "0" :: Array Int
         }
, "7" :: { "0" :: Array Int
         }
, "8" :: { "0" :: Array Int
         }
, "9" :: { "0" :: Array Int
         }
}

seatMap =
    {
      "0": {
        "0":  [5,  5],
        "1":  [5,  7],
        "2":  [5,  9],
        "3":  [5,  11],
        "4":  [5,  13],
        "5":  [7,  3],
        "6":  [7,  15],
        "7":  [9,  3],
        "8":  [9,  15],
        "9":  [11, 5],
        "10": [11, 7],
        "11": [11, 9],
        "12": [11, 11],
        "13": [11, 13]
      },
      "1": {
        "0": [15, 4],
        "1": [15, 6],
        "2": [15, 8],
        "3": [18, 4],
        "4": [18, 6],
        "5": [18, 8]
      },
      "2": {
        "0": [15, 10],
        "1": [15, 12],
        "2": [15, 14],
        "3": [18, 10],
        "4": [18, 12],
        "5": [18, 14]
      },
      "3": {
        "0": [5, 21],
        "1": [6, 19],
        "2": [6, 23],
        "3": [6, 27],
        "4": [8, 20],
        "5": [8, 22],
        "6": [8, 24],
        "7": [8, 26],
        "8": [8, 28]
      },
      "4": {
        "0": [11, 20],
        "1": [12, 23]
      },
      "5": {
        "0": [11, 25],
        "1": [12, 28]
      },
      "6": {
        "0": [15, 22]
      },
      "7": {
        "0": [15, 26]
      },
      "8": {
        "0": [17, 22]
      },
      "9": {
        "0": [17, 26]
      }
    }