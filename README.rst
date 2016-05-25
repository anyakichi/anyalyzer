===============================
anyalyzer - Shogi Kifu Analyzer
===============================

anyalyzer analyzes a shogi kifu file with an external engine and generate
KIF format output with engine's score and principal variations of each
position as comments.


How to use
==========

Analyze a CSA file with the apery::

  $ anyalizer ~/apery/bin/apery 20160526-211506.csa

Analyze the file with depth 15 and multipv 5::

  $ anyalizer -d 15 -m 5 ~/apery/bin/apery 20160526-211506.csa

By default, a KIFU file is analyzed with depth 13 and multipv 3.
