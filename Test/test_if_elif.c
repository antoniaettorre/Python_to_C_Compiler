#include <stdio.h>
#include <stdlib.h>


int main(void){

\* DECLARATION PART *\
int a;
int c;
int x;


\* CODE PART *\
x=30;
if (x>0){
a=1;
printf("If");
printf("ciao");
printf("hello");
}else if (x<1){
c=1;
printf("elif1");
}else if (x<2){
c=2;
printf("elif2");
}else if (x<3){
c=3;
printf("elif3");
}else if (x<4){
c=4;
printf("elif4");
}else if (x<5){
c=5;
printf("elif5");
}else{
a=2;
printf("else");
}
printf("Ciao mondo");
printf("Seconda fuori");
return 0;
}
