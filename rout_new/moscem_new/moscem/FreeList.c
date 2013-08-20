/*==================================================================
    Free the memories of Lists

    Yuqiong Liu         March 2003
==================================================================*/

#include <stdlib.h>
#include <stdio.h>
#include "constant.h"
#include "datatype.h"
#include "utility.h"
#include "moscem.h"

void FreeSeqList(int n) {

    int i;
    SeqList_ptr tmp_ptr1,tmp_ptr2;

    for (i=0;i<n; i++) {
        tmp_ptr1 = SeqHead[i];
	while(tmp_ptr1 != NULL)
        {
	     tmp_ptr2 = tmp_ptr1;
	     tmp_ptr1 = tmp_ptr1->Next;
	     FreeDoubleVector( tmp_ptr2->ObjValue );
	     FreeDoubleVector( tmp_ptr2->ParValue );
	     free( tmp_ptr2 );
	 }
    }

}

void FreeCvgList() {

    CvgList_ptr tmp_ptr1,tmp_ptr2;

    tmp_ptr1 = CvgHead;
    while(tmp_ptr1 != NULL)
    {
        tmp_ptr2 = tmp_ptr1;
        tmp_ptr1 = tmp_ptr1->Next;
        FreeDoubleVector( tmp_ptr2->Convergence );
        free( tmp_ptr2 );
    }

}

