import scala.io.StdIn

// Custom exception for validating string in the second task
class StringLengthException(message: String) extends Exception(message)
class WordLengthException(message: String) extends Exception(message)
class SymbolsException(message: String) extends Exception(message)

extension(lst: List[Double])

    // Function for the first task
    def getLimitedSum(limit: Double): Double = lst.filter(_ <= limit).sum

@main def main(): Unit = {
    var optionNumber = 0
    var isRun = true
    Printer.getTitle()

    while isRun do
        Printer.getMenu()

        optionNumber = Validator.getInt("Enter the number of the task: ")
        optionNumber match
            case 1 => Tasker.listOperations()
            case 2 => Tasker.stringOperations()
            case 0 => isRun = false
            case _ => println("Incorrect option. Please try again")
}

object Tasker:
    // Task 1
    def listOperations(): Unit = {
        Printer.getListTask()

        val lstDouble = Validator.getListDouble("Enter the list of double: ")
        val resultSum = lstDouble.getLimitedSum(10)

        Printer.getResultListTask(lstDouble, resultSum)
    }

    // Task2
    def stringOperations(): Unit = {
        Printer.getStringTask()

        val stringList = Validator.getSeparatedString(1, 15, 1, 5, "Enter the string: ")
        val reversedList = stringList.reverse
        val countList: List[Int] = stringList.map((str: String) => stringList.count(_ == str))
        val countWords: Map[String, Int] = stringList.zip(countList).distinct.toMap
        val sortedWords = stringList.sorted
        
        Printer.getResultStringTask(stringList, reversedList, countWords, sortedWords)
    }

object Validator:
    def getInt(request: String): Int = {
        var isInput = true
        var input = 0

        while isInput do
            try
                println("===========================================================")
                print(request)
                input = StdIn.readLine().toInt
                isInput = false
            catch
                case n: NumberFormatException => println("Incorrect input. Type only the integer number")

        input
    }

    def getListDouble(request: String): List[Double] = {
        var isInput = true
        var input: List[Double] = List()

        while isInput do
            try
                println("===========================================================")
                print(request)
                input = StdIn.readLine().split(",").map(_.toDouble).toList
                isInput = false
            catch
                case n: NumberFormatException => println("Incorrect input. Type the list of double numbers separated by a comma")

        input
    }

    def getSeparatedString(lowBound: Int, highBound: Int, lowWordBound: Int, highWordBound: Int, request: String): List[String] = {
        var isInput = true
        var input: List[String] = List()

        while isInput do
            try
                println("===========================================================")
                print(request)
                input = StdIn.readLine().toUpperCase().split(",").map((str: String) => str.strip).toList
                input = input.init :+ input.last.replace('.', ' ').strip

                if (input.length < lowBound || input.length > highBound) then 
                    throw StringLengthException("The count of words should be betweeen 1 and 15")

                if input.map((str: String) => str.length).filter(_ >= lowWordBound).filter(_ <= highWordBound).length != input.length then 
                    throw WordLengthException("The length of all the words should be between 1 and 5")

                if input.map(str => str.forall(_.isLetter)).filter(_ == true).length != input.length then 
                    throw SymbolsException("Words should contain only english letters")

                isInput = false

            catch
                case sl: StringLengthException => println(sl.getMessage)
                case wl: WordLengthException => println(wl.getMessage)
                case s: SymbolsException => println(s.getMessage)
        
        input
    }

object Printer:

    def getTitle(): Unit = {
        println("===========================================================")
        println("Mykyta Barkalov, KM-31")
        println("Laboratory work 2")
        println("Variant 1")
    }

    def getMenu(): Unit = {
        println("===========================================================")
        println("Menu:")
        println("1. Operations with lists")
        println("2. Operations with strings")
        println("0. Exit")
    }

    def getListTask(): Unit = {
        println("===========================================================")
        println("Task: create a function which sum elements of the list which are not more than 10")
        println("You need to input a list of double numbers. Don't use brackets. For separating use a comma")
    }

    def getStringTask(): Unit = {
        println("===========================================================")
        println("Task: there is a string of words. The count of words is between 1 and 15. Words include from 1 to 5 uppercase letters")
        println("Words are separated by a comma. The end symbol is the point")
        println("Create an inversed string, count words in the string, sort string by words in an alphabet ascending order")
        println("You need to input the string correctly. All letters will be uppercased. All symbols of words should be english letters")
    }

    def getResultListTask(lstDouble: List[Double], resultSum: Double): Unit = {
        println("===========================================================")
        println(s"Start List: ${lstDouble.mkString(", ")}")
        println(s"Result: $resultSum")
    }

    def getResultStringTask(stringList: List[String], reversedList: List[String], 
                            countWords: Map[String, Int], sortedWords: List[String]): Unit = {
        println("===========================================================")
        println(s"Entered string: ${stringList.mkString(", ")}.")
        println(s"Reversed string: ${reversedList.mkString(", ")}.")
        println(s"Count of words: ${countWords.mkString(", ")}")
        println(s"Sorted words: ${sortedWords.mkString(", ")}.")
    }
