\version "2.18.2"

global = {
	\time 4/4
	\key c \major
}

\score {
  \relative { \voiceOne \times 2/3 { b'4 b8 } }
  \transpose c c'
  { \time 4/4
    c \grace ges16  <<bes4 g>> c <<a f>> |
    c <<g e>> c \grace ees16 <<a4 f>>
  }
  \layout {}
  \midi { \tempo 4 = 120 }
}