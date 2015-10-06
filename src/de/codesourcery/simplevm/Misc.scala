package de.codesourcery.simplevm

object Misc 
{
  def repeatUntilNone[T]( func : => Option[T]) : Iterable[T] = 
  {
    new Iterable[T]() 
    {
      final override def iterator : Iterator[T] = 
      {
        new Iterator[T]() 
        {
          private[this] var current = func
          
          override def hasNext : Boolean = current.isDefined
      
          override def next : T = 
          {
            val tmp = current.get
            current = func
            tmp
          }
        }
      }
    }  
  }  
}