//test array and struct
import 'stdio.dart';

class A {
    var krunk;
    var next;
}

var tmp;
int total;

void init(int count) {
  A a;
  a = new A();
  a.krunk = count;
  tmp = a;

  a.next = 5;
  a.next = 5;
  a.next = 5;
}

void sum(var a) {
    total = a.krunk;

  a.next = 5;	
  a.next = 5;
  a.next = 5;
}

void main()
{
  var a;
  
  init(10);
  a = tmp;
  sum(a);
  WriteLong(total);
  WriteLine();
}


/*
55
*/
