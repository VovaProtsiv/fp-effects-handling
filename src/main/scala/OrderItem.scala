case class OrderItem(description:String, amount:Double)
case class Order(id:Int,items:List[OrderItem]){
  def total:Double = items.foldLeft(0.0)((acc,item)=>acc+item.amount)
}
