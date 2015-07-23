#include <immintrin.h>
#include "utils.h"
#include "avx_macros.h"
void op(double* A, double* C, double* alpha){
	double* tr9c9;
	tr9c9 = malloc((sizeof(double) * 81));
	simple_smul(9, 9, (alpha + 0), (A + 0), 1, 9, (A + 0), 1, 9);
	simple_trans(9, 9, (tr9c9 + 0), 1, 9, (A + 0), 1, 9);
	simple_mmul(9, 9, 9, (tr9c9 + 0), 1, 9, (tr9c9 + 0), 1, 9, (C + 0), 9, 1);
	free(tr9c9);
}

void transformedOp(double* A, double* C, double* alpha){
	int i7;
	int i6;
	int i9;
	int i8;
	int i3;
	int i2;
	int i1;
	double r_0;
	double r_1;
	double r_2;
	double r_3;
	double r_4;
	double r_5;
	double r_6;
	double* tr9c9;
	tr9c9 = malloc((sizeof(double) * 81));
	for (i7 = 0; (i7 <= 8); i7 = (i7 + 1))
	{
		for (i6 = 0; (i6 <= 8); i6 = (i6 + 1))
		{
			r_0 = alpha[0];
			r_1 = A[((i6 * 1) + ((i7 * 9) + 0))];
			r_2 = (r_0 * r_1);
			A[((i6 * 1) + ((i7 * 9) + 0))] = r_2;
		}
	}
	for (i9 = 0; (i9 <= 8); i9 = (i9 + 1))
	{
		for (i8 = 0; (i8 <= 8); i8 = (i8 + 1))
		{
			r_3 = A[((i8 * 9) + ((i9 * 1) + 0))];
			tr9c9[((i8 * 1) + ((i9 * 9) + 0))] = r_3;
		}
	}
	for (i3 = 0; (i3 <= 8); i3 = (i3 + 1))
	{
		for (i2 = 0; (i2 <= 8); i2 = (i2 + 1))
		{
			for (i1 = 0; (i1 <= 8); i1 = (i1 + 1))
			{
				r_4 = tr9c9[((i1 * 1) + ((i3 * 9) + 0))];
				r_5 = tr9c9[((i2 * 9) + ((i3 * 1) + 0))];
				r_6 = C[((i1 * 9) + ((i2 * 1) + 0))];
				r_6 = ((r_4 * r_5) + r_6);
				C[((i1 * 9) + ((i2 * 1) + 0))] = r_6;
			}
		}
	}
	free(tr9c9);
}

void sanity_check(FILE* df){
	double* A;
	double* C;
	double* alpha;
	double* A_ref;
	double* C_ref;
	double* alpha_ref;
	double* A_test;
	double* C_test;
	double* alpha_test;
	int A_sc_result;
	int C_sc_result;
	int alpha_sc_result;
	A = malloc((sizeof(double) * 81));
	C = malloc((sizeof(double) * 81));
	alpha = malloc((sizeof(double) * 1));
	A_ref = malloc((sizeof(double) * 81));
	C_ref = malloc((sizeof(double) * 81));
	alpha_ref = malloc((sizeof(double) * 1));
	A_test = malloc((sizeof(double) * 81));
	C_test = malloc((sizeof(double) * 81));
	alpha_test = malloc((sizeof(double) * 1));
	rand_doubles(81, A);
	rand_doubles(81, C);
	rand_doubles(1, alpha);
	memcpy(A_ref, A, (sizeof(double) * 81));
	memcpy(C_ref, C, (sizeof(double) * 81));
	memcpy(alpha_ref, alpha, (sizeof(double) * 1));
	memcpy(A_test, A, (sizeof(double) * 81));
	memcpy(C_test, C, (sizeof(double) * 81));
	memcpy(alpha_test, alpha, (sizeof(double) * 1));
	op(A_ref, C_ref, alpha_ref);
	transformedOp(A_test, C_test, alpha_test);
	A_sc_result = test_buffer_diff(81, A_ref, A_test);
	C_sc_result = test_buffer_diff(81, C_ref, C_test);
	alpha_sc_result = test_buffer_diff(1, alpha_ref, alpha_test);
	if ((A_sc_result || (C_sc_result || alpha_sc_result)))
	{
		fprintf(df, "false\n");
	}
	else
	{
		fprintf(df, "true\n");
	}
	free(A);
	free(C);
	free(alpha);
}

int main(){
	FILE* data_file;
	data_file = fopen("/Users/dillon/Haskell/Nano/runData/fuzzTest.txt", "w");
	sanity_check(data_file);
	fclose(data_file);
	return 0;
}
