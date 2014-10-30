
object simple extends App{
  var myArray= Array(10,25,30);
  var result=0;
  	for(i<-myArray){
  	  if(i%2==0){
  	    result=i*2;
  	    println(result);
  	  } else if(i%2==1){
  	    result=i*3;
  	    println(result);
  	  }
  	  
  	}

}
