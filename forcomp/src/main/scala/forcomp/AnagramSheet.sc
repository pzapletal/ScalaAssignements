package forcomp

object AnagramSheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  
  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary     //> dictionary  : List[forcomp.AnagramSheet.Word] = List(Aarhus, Aaron, Ababa, a
                                                  //| back, abaft, abandon, abandoned, abandoning, abandonment, abandons, abase, a
                                                  //| based, abasement, abasements, abases, abash, abashed, abashes, abashing, aba
                                                  //| sing, abate, abated, abatement, abatements, abater, abates, abating, Abba, a
                                                  //| bbe, abbey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbrevia
                                                  //| tes, abbreviating, abbreviation, abbreviations, Abby, abdomen, abdomens, abd
                                                  //| ominal, abduct, abducted, abduction, abductions, abductor, abductors, abduct
                                                  //| s, Abe, abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberrat
                                                  //| ion, aberrations, abet, abets, abetted, abetter, abetting, abeyance, abhor, 
                                                  //| abhorred, abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abi
                                                  //| ding, Abidjan, Abigail, Abilene, abilities, ability, abject, abjection, abje
                                                  //| ctions, abjectly, abjectness, abjure, abjured, abjures, abjuring, ablate, ab
                                                  //| lated, ablates, ablating
                                                  //| Output exceeds cutoff limit.

  /** Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.filter(_.isLetter).groupBy(c => c).mapValues(c => c.length).toList.sorted
                                                  //> wordOccurrences: (w: forcomp.AnagramSheet.Word)forcomp.AnagramSheet.Occurre
                                                  //| nces
 
  
  wordOccurrences("petrik je hodnej, milej a inzenyr")
                                                  //> res0: forcomp.AnagramSheet.Occurrences = List((a,1), (d,1), (e,5), (h,1), (
                                                  //| i,3), (j,3), (k,1), (l,1), (m,1), (n,3), (o,1), (p,1), (r,2), (t,1), (y,1),
                                                  //|  (z,1))
                                                  
  val w = "petrik je hodnej, milej a inzenyr"     //> w  : String = petrik je hodnej, milej a inzenyr
  w.toLowerCase.filter(_.isLetter).groupBy(c => c).mapValues(c => c.length).toList.sorted
                                                  //> res1: List[(Char, Int)] = List((a,1), (d,1), (e,5), (h,1), (i,3), (j,3), (k
                                                  //| ,1), (l,1), (m,1), (n,3), (o,1), (p,1), (r,2), (t,1), (y,1), (z,1))
                                                  
                                                  
  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.reduce((a, b) => a + b))
                                                  //> sentenceOccurrences: (s: forcomp.AnagramSheet.Sentence)forcomp.AnagramSheet
                                                  //| .Occurrences
                                                  
  val s: Sentence = List("Petrik", "je", "hodnej", "milej", "a", "inzenyr")
                                                  //> s  : forcomp.AnagramSheet.Sentence = List(Petrik, je, hodnej, milej, a, inz
                                                  //| enyr)
                                                  
	wordOccurrences(s.reduce((a, b) => a + b))//> res2: forcomp.AnagramSheet.Occurrences = List((a,1), (d,1), (e,5), (h,1), (
                                                  //| i,3), (j,3), (k,1), (l,1), (m,1), (n,3), (o,1), (p,1), (r,2), (t,1), (y,1),
                                                  //|  (z,1))
                                                  
  s.mkString                                      //> res3: String = Petrikjehodnejmilejainzenyr
                                                  
                                      
  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))
                                                  //> dictionaryByOccurrences: => Map[forcomp.AnagramSheet.Occurrences,List[forco
                                                  //| mp.AnagramSheet.Word]]
                                                  
                                                  
                                                  

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), Nil)
                                                  //> wordAnagrams: (word: forcomp.AnagramSheet.Word)List[forcomp.AnagramSheet.Wo
                                                  //| rd]
  
  dictionaryByOccurrences.getOrElse(wordOccurrences("eat"), Nil)
                                                  //> res4: List[forcomp.AnagramSheet.Word] = List(ate, eat, tea)

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    occurrences match {
      case Nil => List(List())
      case head :: tail =>
        val combs = combinations(tail)
        combs ::: (for {
          comb <- combs
          i <- 1 to head._2
        } yield (head._1, i) :: comb)
    }
  }                                               //> combinations: (occurrences: forcomp.AnagramSheet.Occurrences)List[forcomp.A
                                                  //| nagramSheet.Occurrences]
  
  
  val occ = sentenceOccurrences(s)                //> occ  : forcomp.AnagramSheet.Occurrences = List((a,1), (d,1), (e,5), (h,1), 
                                                  //| (i,3), (j,3), (k,1), (l,1), (m,1), (n,3), (o,1), (p,1), (r,2), (t,1), (y,1)
                                                  //| , (z,1))
  
  

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    (y.toMap.foldLeft(x.toMap) ((map, tuple) => {
      val newFrequency = map(tuple._1) - tuple._2
      if (newFrequency <= 0) map - tuple._1
      else map.updated(tuple._1, newFrequency)
    })).toList.sorted
  }                                               //> subtract: (x: forcomp.AnagramSheet.Occurrences, y: forcomp.AnagramSheet.Occ
                                                  //| urrences)forcomp.AnagramSheet.Occurrences


  
  
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
                                                  //> lard  : List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,1))
    val r = List(('r', 1))                        //> r  : List[(Char, Int)] = List((r,1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))  //> lad  : List[(Char, Int)] = List((a,1), (d,1), (l,1))
  	subtract(lard, r)                         //> res5: forcomp.AnagramSheet.Occurrences = List((a,1), (d,1), (l,1))
  	
  	
  def sentenceAnagrams(s: Sentence): List[Sentence] = {
    def inner(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty) List(List())
      else
        for {
          combinations <- combinations(occurrences)
          word <- dictionaryByOccurrences.getOrElse(combinations, List())
          sentence <- inner(subtract(occurrences, wordOccurrences(word)))
          if !combinations.isEmpty
        } yield word :: sentence
    }
    inner(sentenceOccurrences(s))
  }
                                                  //> sentenceAnagrams: (s: forcomp.AnagramSheet.Sentence)List[forcomp.AnagramShe
                                                  //| et.Sentence]
                                                    
}