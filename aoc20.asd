;;;; aoc20.asd

(asdf:defsystem #:aoc20
  :description "Advent of Code 2020"
  :depends-on ("cl-ppcre"
               "arrows"
               "alexandria"
               "anaphora"
               "fiveam"
               "serapeum"
               "bit-smasher")
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "day01")
               (:file "day02")
               (:file "day03")
               (:file "day04")
               (:file "day05")
               (:file "day06")
               (:file "day07")
               (:file "day08")
               (:file "day09")
               (:file "day10")
               (:file "day11")
               (:file "day12")
               (:file "day13")
               (:file "day14")
               (:file "day15")
               (:file "day16")
               (:file "day17")
               (:file "day18")
               (:file "day19")
               (:file "day20")
               (:file "day21")
               (:file "day22")
               (:file "day23")
               (:file "day24")
               (:file "day25")
               (:file "aoc20")))
