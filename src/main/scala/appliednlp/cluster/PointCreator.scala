package appliednlp.cluster

import nak.cluster._
import nak.util.CollectionUtil._
import chalk.util.SimpleTokenizer

import org.apache.log4j.Logger
import org.apache.log4j.Level

/**
 *  Read data and produce data points and their features.
 *
 *  @param filename the name of the file containing the data
 *  @return a triple, the first element of which is a sequence of id's
 *     (unique for each row / data point), the second element is the sequence
 *     of (known) cluster labels, and the third of which is the sequence of
 *     Points to be clustered.
 */
trait PointCreator extends (String => Iterator[(String,String,Point)])

/**
 * Read data in the standard format for use with k-means.
 */
object DirectCreator extends PointCreator {

 def apply(filename: String) : Iterator[(String,String,Point)] = { 
 

 val DataRE = """(\d+) (\d+) (-?\d+\.?\d*) (-?\d+\.?\d*)\n?""".r
 val data = io.Source.fromFile(filename).mkString.split("\n")
 

 val dataParsed = data.flatMap{
	case DataRE(id, label, pt1, pt2) => 
		Some((id, label, Point(IndexedSeq(pt1.toDouble,pt2.toDouble))))
 	
	case _ => None
}.toIterator

 dataParsed
}
}


/**
 * A standalone object with a main method for converting the achieve.dat rows
 * into a format suitable for input to RunKmeans.
 */
object SchoolsCreator extends PointCreator {

  def apply(filename: String): Iterator[(String,String,Point)] = {
	
	val SchoolRE = """([A-Z ]+)\s+(\d+\.\d+)\s+(\d+\.\d+)\s+(\d+\.\d+)\s+(\d+\.\d+)\s*""".r
	val scores = io.Source.fromFile(filename).mkString.split("\n")
        
        
	val scoresParsed = scores.flatMap{
		case SchoolRE(school, read4, math4, read6, math6) =>
			Some( List((school.trim.replaceAll(" ","_")++"_4", "4", Point(IndexedSeq(read4.toDouble,math4.toDouble))),(school.trim.replaceAll(" ","_") ++ "_6", "6", Point(IndexedSeq(read6.toDouble,math6.toDouble)))))
		case _ => None
	}.flatten.toIterator
  
  scoresParsed
 
  }
}




/**
 * A standalone object with a main method for converting the birth.dat rows
 * into a format suitable for input to RunKmeans.
 */
object CountriesCreator extends PointCreator {

  def apply(filename: String): Iterator[(String,String,Point)] = {
	
	val CountryRE = """([A-Z \.]+)\s+(\d+\.\d+)\s+(\d+\.\d+)\s*""".r

	val countries = io.Source.fromFile(filename).mkString.split("\n")

	val countriesParsed = countries.flatMap{
		case CountryRE(country, birth, death) =>
			Some ((country.trim.replaceAll(" ","_"), "1", Point(IndexedSeq(birth.toDouble,death.toDouble))))
		case _ => None
	}.toIterator
	//countriesParsed.foreach(println)
	countriesParsed
  }
}

/**
 * A class that converts the raw Federalist
 * papers into rows with a format suitable for input to Cluster. As part of
 * this, it must also perform feature extraction, converting the texts into
 * sets of values for each feature (such as a word count or relative
 * frequency).
 */
class FederalistCreator(simple: Boolean = false) extends PointCreator {

  def apply(filename: String) : Iterator[(String,String,Point)] = {

	val articleInfo = FederalistArticleExtractor(filename)
	val ids = {for(article <- articleInfo) yield{ article.getOrElse("id","err")}}.toIndexedSeq
 	val labels = {for (article <- articleInfo) yield{article.getOrElse("author","err")}}.toIndexedSeq
	val textSeq = {for(article <- articleInfo) yield{article.getOrElse("text","err").toLowerCase}}.toIndexedSeq
	
	
	val feds = for (id <- ids) yield
	{
		if(simple){
			(id,labels(ids.indexOf(id)),extractSimple(textSeq)(ids.indexOf(id)))
		}
		else{
		
			(id,labels(ids.indexOf(id)),extractFull(textSeq)(ids.indexOf(id)))
		}
	}
	feds.toIndexedSeq.toIterator
	

  }
  /**
   * Given the text of an article, compute the frequency of "the", "people"
   * and "which" and return a Point per article that has the frequency of
   * "the" as the value of the first dimension, the frequency of "people"
   * for the second, and the frequency of "which" for the third.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */
  def extractSimple(texts: IndexedSeq[String]): IndexedSeq[Point] = {

     val tokenizedTexts = texts.map(w=> SimpleTokenizer(w))
     
     val part5 = List(List("the"),List("which"),List("people"))
         
     val points = {for(text <- tokenizedTexts) yield {
	Point(getWordCount(text,part5))
     }}.toIndexedSeq
    
     points
    
    
  }

  /**
   * Given the text of an article, extract features as best you can to try to
   * get good alignment of the produced clusters with the known authors.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */
  def extractFull(texts: IndexedSeq[String]): IndexedSeq[Point] = {
    	val tokenizedTexts = texts.map(w => SimpleTokenizer(w))

	val pronouns = List("i","you","we","he","they","me","us","her","him","them","my","mine","our","ours","theirs","their","her","hers","his","myself","itself","himself","herself","ourselves","themselves","anything","something","everything","nothing","anyone","everyone","someone")
     	val prepositions = List("about","in","across","inside","into","near","against","along","near","around","of","at","off","behind","onto","beside","besides","over","by","through","despite","to","down","toward","during","with","within","for","from","without")
     	val determiners = List("the","a","an","some","any","this","each","no","that","every","all","half","both","twice","one","two","first","other","second","another","next","last","many","few","much","more","most","several","little","less","least","no","own")
     	val conj = List("and","or","but","so","after","before","when","since","as","while","because","although","though","if","what","which","where","who","whose","how","than")      
     	val modal = List("can","might","may","would","will","should","must","shall","could") 

    	val test1 = List(List("constitution"),List("people"),List("which"))
    	val full = List(List("which"),prepositions,List("constitution"),List("people"))
	val test3 = List(determiners,pronouns,List("constitution"),List("people"))
        val test5 = List(conj,determiners,modal,List("constitution"),List("people"))

     	val points = {for(text <- tokenizedTexts) yield {
		//Point(getWordFreq(text,test3)++getWordCount(text,full))
 		//Point(IndexedSeq(getAvgWordLength(text))++getWordCount(text,test5))
		Point(getWordCount(text,full))
		//Point(getWordCount(text,test5))
		//Point(IndexedSeq(getAvgWordLength(text))++getWordCount(text,full))
		//Point(getWordCount(text,List(List("the"),List("which"),List("people"))))

     }}.toIndexedSeq
    
     points

 }

  def getWordCount(text: IndexedSeq[String], words: List[List[String]]): IndexedSeq[Double] = {
	{for (word <- words) yield {
	   val num = for( wd <- word) yield {
 		 text.count(w=> w == wd).toDouble
		}
	   num.sum.toDouble
  	}}.toIndexedSeq
  }

  def getWordFreq(text: IndexedSeq[String], words: List[List[String]]): IndexedSeq[Double] = {
	{for (word <- words) yield{
	 val num = for ( wd <- word ) yield {
		text.count(w => w == wd).toDouble
	}
	num.sum.toDouble / text.length
	}}.toIndexedSeq

}

  def getAvgWordLength(text: IndexedSeq[String]): Double = {
	val avg = (text.map(w => w.length).sum.toDouble)/text.length
	avg
  }



}

object FederalistArticleExtractor {
  /**
   * A method that takes the raw Federalist papers input and extracts each
   * article into a structured format.
   *
   * @param filename The filename containing the Federalist papers.
   * @return A sequence of Maps (one per article) from attributes (like
   *         "title", "id", and "text") to their values for each article.
   */
  def apply(filename: String): IndexedSeq[Map[String, String]] = {

    // Regex to identify the text portion of a document.
    val JustTextRE = (
      """(?s)\*\*\* START OF THIS PROJECT GUTENBERG.+""" +
      """\*\*\*(.+)\*\*\* END OF THIS PROJECT GUTENBERG""").r

    // Regex to capture different parts of each article.
    val ArticleRE = (
      """(?s)(\d+)\n+""" + // The article number.
      """(.+?)\n+""" + // The title (note non-greedy match).
      """((?:(?:For|From)[^\n]+?)?)\s+""" + // The publication venue (optional).
      """((?:(?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday).+\d\d\d\d\.)?)\n+""" + // The date (optional).
      """((?:MAD|HAM|JAY).+?)\n+""" + // The author(s).
      """(To the [^\n]+)""" + // The addressee.
      """(.+)""" // The text.
      ).r

    val book = io.Source.fromFile(filename).mkString
    val text = JustTextRE.findAllIn(book).matchData.next.group(1)
    val rawArticles = text.split("FEDERALIST.? No. ")

    // Use the regular expression to parse the articles.
    val allArticles = rawArticles.flatMap {
      case ArticleRE(id, title, venue, date, author, addressee, text) =>
        Some(Map("id" -> id.trim,
          "title" -> title.replaceAll("\\n+", " ").trim,
          "venue" -> venue.replaceAll("\\n+", " ").trim,
          "date" -> date.replaceAll("\\n+", " ").trim,
          "author" -> author.replaceAll("\\n+", " ").trim,
          "addressee" -> addressee.trim,
          "text" -> text.trim))

      case _ => None
    }.toIndexedSeq

    // Get rid of article 71, which is a duplicate, and return the rest.
    allArticles.take(70) ++ allArticles.slice(71, allArticles.length)
  }

}
