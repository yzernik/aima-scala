package edu.uiuc.cs.dicarlo2

import java.io.BufferedReader
import collection.mutable
import edu.uiuc.cs.dicarlo2.alg.Space

object Parser {
  def parse(reader: BufferedReader): IndexedSeq[IndexedSeq[Space]] = {
    val board = mutable.IndexedSeq.fill(5)(mutable.IndexedSeq[Space]())
    while (reader.ready()) {
      reader.readLine().split("\\s+").foldLeft(0)((i, char) => {
        board(i) :+= Space(Integer.valueOf(char), None)
        i + 1
      })
    }
    board
  }
}
