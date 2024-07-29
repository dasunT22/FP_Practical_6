object StudentRecords {
  import scala.io.StdIn.readLine

  // Function to get student info
  def getStudentInfo: (String, Int, Int, Double, Char) = {
    val (name, marks, totalMarks) = getStudentInfoWithRetry
    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = calculateGrade(percentage)
    (name, marks, totalMarks, percentage, grade)
  }

  // Function to print student record
  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Name: $name")
    println(s"Marks: $marks")
    println(s"Total Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f")
    println(s"Grade: $grade")
  }

  // Function to validate input
  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) (false, Some("Name cannot be empty"))
    else if (marks < 0 || marks > totalMarks) (false, Some("Marks must be positive and not exceed total marks"))
    else (true, None)
  }

  // Function to get student info with retry
  def getStudentInfoWithRetry: (String, Int, Int) = {
    var isValid = false
    var name = ""
    var marks = 0
    var totalMarks = 0

    while (!isValid) {
      println("Enter student's name:")
      name = readLine()
      println("Enter student's marks:")
      marks = readLine().toInt
      println("Enter total possible marks:")
      totalMarks = readLine().toInt

      val (valid, errorMessage) = validateInput(name, marks, totalMarks)
      if (!valid) {
        println(s"Error: ${errorMessage.get}")
      }
      isValid = valid
    }
    (name, marks, totalMarks)
  }

  // Function to calculate grade
  def calculateGrade(percentage: Double): Char = {
    if (percentage >= 90) 'A'
    else if (percentage >= 75) 'B'
    else if (percentage >= 50) 'C'
    else 'D'
  }

  // Main function to run the application
  def main(args: Array[String]): Unit = {
    val studentInfo = getStudentInfo
    printStudentRecord(studentInfo)
  }
}

// Call the main method to run the application
object MainApp extends App {
  StudentRecords.main(Array())
}

