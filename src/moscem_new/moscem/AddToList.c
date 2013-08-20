/*================================ AddToList.c =============================
      AddSeqList: add new points to sequences
      AddCvgList: add convergence values to a list for records

      Yuqiong Liu        March 2003
============================================================================*/


#include "constant.h"
#include "datatype.h"
#include "moscem.h"

void AddSeqList(int i, SeqList_ptr NewPoint) {

     int j;
     if (SeqHead[i] == NULL) {
	 SeqHead[i] = NewPoint;
         SeqHead[i]->Index = 0;
	 SeqTail[i] = SeqHead[i];
     }
     else {
         SeqTail[i]->Next = NewPoint;
	 SeqTail[i] = SeqTail[i]->Next;
     }
     SeqHead[i]->Index++;
}

void AddCvgList(CvgList_ptr NewPoint) {

     int j;
     if (CvgHead == NULL) {
         CvgHead = NewPoint;
         CvgHead->Index = 0;
         CvgTail = CvgHead;
     }
     else {
         CvgTail->Next = NewPoint;
         CvgTail = CvgTail->Next;
     }
     CvgHead->Index++;
}

