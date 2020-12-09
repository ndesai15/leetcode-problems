package basics.datastructure.TreesProblems

/**
  * @author ndesai on 2020-12-08
  *
  */
trait BSTree {
  def root: Int
  def left: BSTree
  def right: BSTree
  def insert(element: Int): BSTree
  def lookup(element: Int): BSTree
}

object BSEmpty extends BSTree {
  override def root: Nothing = throw new NoSuchElementException("empty value")
  override def left: BSTree =
    throw new NoSuchElementException("empty left tree")
  override def right: BSTree =
    throw new NoSuchElementException("empty right tree")
  override def insert(element: Int): BSTree =
    new BSCons(element, BSEmpty, BSEmpty)
  override def lookup(element: Int): BSTree =
    throw new NoSuchMethodException("looking into empty binary search tree")
}

class BSCons(override val root: Int, val l: BSTree, val r: BSTree)
    extends BSTree {
  override def left: BSTree = l
  override def right: BSTree = r
  override def lookup(element: Int): BSTree = {
      if (element < root) l.lookup(element)
      else if (element > root) r.lookup(element)
      else new BSCons(element, l, r)
  }
  override def insert(element: Int): BSTree =
    if (element < root) new BSCons(root, l.insert(element), r)
    else new BSCons(root, l, r.insert(element))
}

object TreeStructureTest extends App {
  val tree = BSEmpty.insert(28).insert(22).insert(73).insert(44).insert(55).insert(91).insert(2).insert(54)
  println(tree.root) // 28
  println(tree.left.left.root) // 2
  println(tree.right.left.right.root) // 55
  println(tree.right.left.right.left.root) // 54
  println(tree.right.right.root) // 91
  println(tree.lookup(73).left.root == 44)
}
