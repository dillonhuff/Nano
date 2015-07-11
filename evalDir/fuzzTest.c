#include <immintrin.h>
#include "utils.h"
void op(double* alpha, double* x, double* y){
	double* t;
	t = malloc((sizeof(double) * 16));
	simple_smul(16, 1, (alpha + 0), (x + 0), 1, 1, (t + 0), 1, 1);
	simple_add(16, 1, (t + 0), 1, 1, (y + 0), 1, 1, (y + 0), 1, 1);
	free(t);
}

void transformedOp(double* alpha, double* x, double* y){
	int i2;
	__m256d r_0;
	__m256d r_1;
	__m256d r_2;
	__m256d sm0;
	__m256d t;
	for (i2 = 0; (i2 <= 15); i2 = (i2 + 4))
	{
		sm0 = _mm256_broadcast_sd((alpha + 0));
		sm0 = sm0;
		r_0 = _mm256_loadu_pd((x + i2));
		t = _mm256_mul_pd(sm0, r_0);
		t = t;
		t = t;
		r_1 = _mm256_loadu_pd((y + i2));
		r_2 = _mm256_add_pd(t, r_1);
		_mm256_storeu_pd((y + i2), r_2);
	}
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
	x = malloc((sizeof(double) * 16));
	y = malloc((sizeof(double) * 16));
	alpha_ref = malloc((sizeof(double) * 1));
	x_ref = malloc((sizeof(double) * 16));
	y_ref = malloc((sizeof(double) * 16));
	alpha_test = malloc((sizeof(double) * 1));
	x_test = malloc((sizeof(double) * 16));
	y_test = malloc((sizeof(double) * 16));
	rand_doubles(1, alpha);
	rand_doubles(16, x);
	rand_doubles(16, y);
	memcpy(alpha_ref, alpha, (sizeof(double) * 1));
	memcpy(x_ref, x, (sizeof(double) * 16));
	memcpy(y_ref, y, (sizeof(double) * 16));
	memcpy(alpha_test, alpha, (sizeof(double) * 1));
	memcpy(x_test, x, (sizeof(double) * 16));
	memcpy(y_test, y, (sizeof(double) * 16));
	op(alpha_ref, x_ref, y_ref);
	transformedOp(alpha_test, x_test, y_test);
	alpha_sc_result = test_buffer_diff(1, alpha_ref, alpha_test);
	x_sc_result = test_buffer_diff(16, x_ref, x_test);
	y_sc_result = test_buffer_diff(16, y_ref, y_test);
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
