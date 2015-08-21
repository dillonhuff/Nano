#include <immintrin.h>
#include "utils.h"
#include "avx_macros.h"
void op(double* A, double* B, double* C){
	simple_add(8, 8, (A + 0), 1, 8, (B + 0), 8, 1, (C + 0), 1, 8);
}

void transformedOp(double* A, double* B, double* C){
	int i;
	int iz;
	__m256d r_0;
	__m256d r_1;
	__m256d r_2;
	__m256d r_0ByCol0;
	__m256d r_0ByCol1;
	__m256d r_0ByCol2;
	__m256d r_0ByCol3;
	__m256d r_1ByRow0;
	__m256d r_1ByRow1;
	__m256d r_1ByRow2;
	__m256d r_1ByRow3;
	__m256d r_2ByCol0;
	__m256d r_2ByCol1;
	__m256d r_2ByCol2;
	__m256d r_2ByCol3;
	for (i = 0; (i <= 7); i = (i + 4))
	{
		for (iz = 0; (iz <= 7); iz = (iz + 4))
		{
			PACK_DBL_4x4(r_0ByCol0, r_0ByCol1, r_0ByCol2, r_0ByCol3, (A + ((iz * 8) + i)));
			PACK_DBL_4x4(r_1ByRow0, r_1ByRow1, r_1ByRow2, r_1ByRow3, (B + (iz + (i * 8))));
			EADD_DBL_4x4(r_2ByCol0, r_2ByCol1, r_2ByCol2, r_2ByCol3, r_0ByCol0, r_0ByCol1, r_0ByCol2, r_0ByCol3, r_1ByRow0, r_1ByRow1, r_1ByRow2, r_1ByRow3);
			UNPACK_DBL_4x4((C + ((iz * 8) + i)), r_2ByCol0, r_2ByCol1, r_2ByCol2, r_2ByCol3);
		}
	}
}

void sanity_check(FILE* df){
	double* A;
	double* B;
	double* C;
	double* A_ref;
	double* B_ref;
	double* C_ref;
	double* A_test;
	double* B_test;
	double* C_test;
	int A_sc_result;
	int B_sc_result;
	int C_sc_result;
	A = malloc((sizeof(double) * 64));
	B = malloc((sizeof(double) * 64));
	C = malloc((sizeof(double) * 64));
	A_ref = malloc((sizeof(double) * 64));
	B_ref = malloc((sizeof(double) * 64));
	C_ref = malloc((sizeof(double) * 64));
	A_test = malloc((sizeof(double) * 64));
	B_test = malloc((sizeof(double) * 64));
	C_test = malloc((sizeof(double) * 64));
	rand_doubles(64, A);
	rand_doubles(64, B);
	rand_doubles(64, C);
	memcpy(A_ref, A, (sizeof(double) * 64));
	memcpy(B_ref, B, (sizeof(double) * 64));
	memcpy(C_ref, C, (sizeof(double) * 64));
	memcpy(A_test, A, (sizeof(double) * 64));
	memcpy(B_test, B, (sizeof(double) * 64));
	memcpy(C_test, C, (sizeof(double) * 64));
	op(A_ref, B_ref, C_ref);
	transformedOp(A_test, B_test, C_test);
	A_sc_result = test_buffer_diff(64, A_ref, A_test);
	B_sc_result = test_buffer_diff(64, B_ref, B_test);
	C_sc_result = test_buffer_diff(64, C_ref, C_test);
	if ((A_sc_result || (B_sc_result || C_sc_result)))
	{
		fprintf(df, "false\n");
	}
	else
	{
		fprintf(df, "true\n");
	}
	free(A);
	free(B);
	free(C);
}

int main(){
	FILE* data_file;
	data_file = fopen("/Users/dillon/Haskell/HPC/Nano/runData/fuzzTest.txt", "w");
	sanity_check(data_file);
	fclose(data_file);
	return 0;
}
