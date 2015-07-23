#include <immintrin.h>
#include "utils.h"
#include "avx_macros.h"
void op(double* alpha, double* x, double* y){
	simple_mmul(1, 1, 11, (x + 0), 1, 1, (y + 0), 1, 1, (alpha + 0), 1, 1);
}

void transformedOp(double* alpha, double* x, double* y){
	int d_0;
	__m256d d_1;
	__m256d d_2;
	__m256d d_3;
	__m256d d_4;
	__m256d d_5;
	__m256d r_0;
	__m256d r_1;
	__m256d r_10;
	__m256d r_11;
	__m256d r_12;
	__m256d r_13;
	__m256d r_14;
	__m256d r_15;
	__m256d r_16;
	__m256d r_17;
	__m256d r_18;
	__m256d r_19;
	__m256d r_2;
	__m256d r_3;
	__m256d r_4;
	__m256d r_5;
	__m256d r_6;
	__m256d r_7;
	__m256d r_8;
	__m256d r_9;
	__m256d cgr5;
	__m256d cgr4;
	__m256d cgr3;
	__m256d cgr2;
	__m256d cgr1;
	__m256d cgr0;
	r_0 = _mm256_setzero_pd();
	d_2 = r_0;
	for (d_0 = 0; (d_0 <= 7); d_0 = (d_0 + 4))
	{
		r_1 = _mm256_loadu_pd((x + d_0));
		r_2 = _mm256_loadu_pd((y + d_0));
		r_3 = _mm256_mul_pd(r_1, r_2);
		d_1 = r_3;
		r_4 = d_1;
		r_5 = d_2;
		r_6 = _mm256_add_pd(r_4, r_5);
		d_2 = r_6;
	}
	r_7 = _mm256_maskload_pd((alpha + 0), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1));
	d_3 = r_7;
	r_8 = d_3;
	r_9 = d_2;
	r_10 = d_3;
	ACCUM4(cgr0, cgr1, cgr2, r_8, r_9, r_10);
	d_3 = r_10;
	r_11 = d_3;
	_mm256_maskstore_pd((alpha + 0), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1), r_11);
	r_12 = _mm256_maskload_pd((x + 8), _mm256_set_epi32(0, 0, -1, -1, -1, -1, -1, -1));
	r_13 = _mm256_maskload_pd((y + 8), _mm256_set_epi32(0, 0, -1, -1, -1, -1, -1, -1));
	r_14 = _mm256_mul_pd(r_12, r_13);
	d_4 = r_14;
	r_15 = _mm256_maskload_pd((alpha + 0), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1));
	d_5 = r_15;
	r_16 = d_5;
	r_17 = d_4;
	r_18 = d_5;
	ACCUM4(cgr3, cgr4, cgr5, r_16, r_17, r_18);
	d_5 = r_18;
	r_19 = d_5;
	_mm256_maskstore_pd((alpha + 0), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1), r_19);
}

void sanity_check(FILE* df){
	double* alpha;
	double* x;
	double* y;
	double* alpha_ref;
	double* x_ref;
	double* y_ref;
	double* alpha_test;
	double* x_test;
	double* y_test;
	int alpha_sc_result;
	int x_sc_result;
	int y_sc_result;
	alpha = malloc((sizeof(double) * 1));
	x = malloc((sizeof(double) * 11));
	y = malloc((sizeof(double) * 11));
	alpha_ref = malloc((sizeof(double) * 1));
	x_ref = malloc((sizeof(double) * 11));
	y_ref = malloc((sizeof(double) * 11));
	alpha_test = malloc((sizeof(double) * 1));
	x_test = malloc((sizeof(double) * 11));
	y_test = malloc((sizeof(double) * 11));
	rand_doubles(1, alpha);
	rand_doubles(11, x);
	rand_doubles(11, y);
	memcpy(alpha_ref, alpha, (sizeof(double) * 1));
	memcpy(x_ref, x, (sizeof(double) * 11));
	memcpy(y_ref, y, (sizeof(double) * 11));
	memcpy(alpha_test, alpha, (sizeof(double) * 1));
	memcpy(x_test, x, (sizeof(double) * 11));
	memcpy(y_test, y, (sizeof(double) * 11));
	op(alpha_ref, x_ref, y_ref);
	transformedOp(alpha_test, x_test, y_test);
	alpha_sc_result = test_buffer_diff(1, alpha_ref, alpha_test);
	x_sc_result = test_buffer_diff(11, x_ref, x_test);
	y_sc_result = test_buffer_diff(11, y_ref, y_test);
	if ((alpha_sc_result || (x_sc_result || y_sc_result)))
	{
		fprintf(df, "false\n");
	}
	else
	{
		fprintf(df, "true\n");
	}
	free(alpha);
	free(x);
	free(y);
}

int main(){
	FILE* data_file;
	data_file = fopen("/Users/dillon/Haskell/Nano/runData/fuzzTest.txt", "w");
	sanity_check(data_file);
	fclose(data_file);
	return 0;
}
