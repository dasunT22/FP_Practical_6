import scala.io.StdIn.readLine

case class Product(id: Int, name: String, quantity: Int, price: Double)

object InventorySystem extends App {

var inventory1: Map[Int, Product] = Map(
    101 -> Product(101, "ProductA", 10, 15.0),
    102 -> Product(102, "ProductB", 5, 25.0),
    103 -> Product(103, "ProductC", 8, 12.0)
)

var inventory2: Map[Int, Product] = Map(
    102 -> Product(102, "ProductB", 10, 30.0),
    104 -> Product(104, "ProductD", 7, 20.0)
)

  // 1. Retrieve all product names from inventory1.
def getAllProductNames(inventory: Map[Int, Product]): Seq[String] = {
    inventory.values.map(_.name).toSeq
}

  // 2. Calculate the total value of all products in inventory1.
def calculateTotalValue(inventory: Map[Int, Product]): Double = {
    inventory.values.map(p => p.quantity * p.price).sum
}

  // 3. Check if inventory1 is empty.
def isInventoryEmpty(inventory: Map[Int, Product]): Boolean = {
    inventory.isEmpty
}

  // 4. Merge inventory1 and inventory2, updating quantities and retaining the highest price.
def mergeInventories(inventory1: Map[Int, Product], inventory2: Map[Int, Product]): Map[Int, Product] = {
    inventory2.foldLeft(inventory1) { (merged, entry) =>
        val (id, product) = entry
        merged.get(id) match {
        case Some(existingProduct) =>
            merged + (id -> existingProduct.copy(
            quantity = existingProduct.quantity + product.quantity,
            price = math.max(existingProduct.price, product.price)
        ))
        case None => merged + (id -> product)
        }
    }
}

  // 5. Check if a product with a specific ID exists and print its details.
def checkProductExists(inventory: Map[Int, Product], id: Int): Option[Product] = {
    inventory.get(id)
}

def displayMenu(): Unit = {
    println()
    println("Select an operation:")
    println("1. Retrieve all product names from inventory1")
    println("2. Calculate the total value of all products in inventory1")
    println("3. Check if inventory1 is empty")
    println("4. Merge inventory1 and inventory2")
    println("5. Check if a product with ID 102 exists in inventory1")
    println("6. Exit")
}

def mainMenu(): Unit = {
    var continue = true
    while (continue) {
        displayMenu()
        val choice = readLine("Enter your choice: ").toInt

        choice match {
        case 1 =>
            val productNames = getAllProductNames(inventory1)
            println(s"Product Names: ${productNames.mkString(", ")}")
        case 2 =>
            val totalValue = calculateTotalValue(inventory1)
            println(f"Total Value of Products: $$ $totalValue%.2f")
        case 3 =>
            val isEmpty = isInventoryEmpty(inventory1)
            println(s"Inventory1 is empty: $isEmpty")
        case 4 =>
            inventory1 = mergeInventories(inventory1, inventory2)
            println("Inventories merged.")
        case 5 =>
            checkProductExists(inventory1, 102) match {
                case Some(product) => println(s"Product Details: $product")
                case None => println("Product with ID 102 does not exist in inventory1.")
            }
        case 6 =>
            continue = false
            println("Exiting...")
        case _ =>
            println("Invalid choice. Please select a valid option.")
        }
    }
}
mainMenu()
}