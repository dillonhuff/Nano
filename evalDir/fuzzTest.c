#include <immintrin.h>
#include "utils.h"
void op(float* A, float* alpha, float* beta, float* x, float* y){
	float* t1;
	float* t2;
	t1 = malloc((sizeof(float) * 4));
	t2 = malloc((sizeof(float) * 4));
	set_to_zero_float((t1 + 0), 4, 1, 1, 1);
	simple_mmul_float(4, 1, 15, (A + 0), 1, 4, (x + 0), 1, 1, (t1 + 0), 1, 1);
	simple_smul_float(4, 1, (alpha + 0), (t1 + 0), 1, 1, (t2 + 0), 1, 1);
	simple_smul_float(4, 1, (beta + 0), (y + 0), 1, 1, (y + 0), 1, 1);
	simple_add_float(4, 1, (t2 + 0), 1, 1, (y + 0), 1, 1, (y + 0), 1, 1);
	free(t1);
	free(t2);
}

void transformedOp(float* A, float* alpha, float* beta, float* x, float* y){
	int b_0;
	__m256d k_0;
	__m256d k_1;
	__m256d k_2;
	__m256d k_3;
	__m256d k_4;
	__m256d k_5;
	__m256d sm0;
	__m256d sm1;
	__m256d sm2;
	__m256d t_1;
	__m256d t_2;
	__m256d t_3;
	k_0 = _mm256_setzero_ps();
	t_1 = k_0;
	for (b_0 = 0; (b_0 <= 14); b_0 = (b_0 + 1))
	{
		sm0 = _mm256_broadcast_ss((x + b_0));
		k_1 = _mm256_maskload_ps((A + (b_0 * 4)), _mm256_set_epi32(0, 0, 0, 0, -1, -1, -1, -1));
		t_2 = _mm256_mul_ps(sm0, k_1);
		t_1 = _mm256_add_ps(t_2, t_1);
	}
	sm1 = _mm256_broadcast_ss((alpha + 0));
	t_3 = _mm256_mul_ps(sm1, t_1);
	sm2 = _mm256_broadcast_ss((beta + 0));
	k_2 = _mm256_maskload_ps((y + 0), _mm256_set_epi32(0, 0, 0, 0, -1, -1, -1, -1));
	k_3 = _mm256_mul_ps(sm2, k_2);
	_mm256_maskstore_ps((y + 0), _mm256_set_epi32(0, 0, 0, 0, -1, -1, -1, -1), k_3);
	k_4 = _mm256_maskload_ps((y + 0), _mm256_set_epi32(0, 0, 0, 0, -1, -1, -1, -1));
	k_5 = _mm256_add_ps(t_3, k_4);
	_mm256_maskstore_ps((y + 0), _mm256_set_epi32(0, 0, 0, 0, -1, -1, -1, -1), k_5);
}

void sanity_check(FILE* df){
	float* A;
	float* alpha;
	float* beta;
	float* x;
	float* y;
	float* A_ref;
	float* alpha_ref;
	float* beta_ref;
	float* x_ref;
	float* y_ref;
	float* A_test;
	float* alpha_test;
	float* beta_test;
	float* x_test;
	float* y_test;
	int A_sc_result;
	int alpha_sc_result;
	int beta_sc_result;
	int x_sc_result;
	int y_sc_result;
	A = malloc((sizeof(float) * 60));
	alpha = malloc((sizeof(float) * 1));
	beta = malloc((sizeof(float) * 1));
	x = malloc((sizeof(float) * 15));
	y = malloc((sizeof(float) * 4));
	A_ref = malloc((sizeof(float) * 60));
	alpha_ref = malloc((sizeof(float) * 1));
	beta_ref = malloc((sizeof(float) * 1));
	x_ref = malloc((sizeof(float) * 15));
	y_ref = malloc((sizeof(float) * 4));
	A_test = malloc((sizeof(float) * 60));
	alpha_test = malloc((sizeof(float) * 1));
	beta_test = malloc((sizeof(float) * 1));
	x_test = malloc((sizeof(float) * 15));
	y_test = malloc((sizeof(float) * 4));
	rand_floats(60, A);
	rand_floats(1, alpha);
	rand_floats(1, beta);
	rand_floats(15, x);
	rand_floats(4, y);
	memcpy(A_ref, A, (sizeof(float) * 60));
	memcpy(alpha_ref, alpha, (sizeof(float) * 1));
	memcpy(beta_ref, beta, (sizeof(float) * 1));
	memcpy(x_ref, x, (sizeof(float) * 15));
	memcpy(y_ref, y, (sizeof(float) * 4));
	memcpy(A_test, A, (sizeof(float) * 60));
	memcpy(alpha_test, alpha, (sizeof(float) * 1));
	memcpy(beta_test, beta, (sizeof(float) * 1));
	memcpy(x_test, x, (sizeof(float) * 15));
	memcpy(y_test, y, (sizeof(float) * 4));
	op(A_ref, alpha_ref, beta_ref, x_ref, y_ref);
	transformedOp(A_test, alpha_test, beta_test, x_test, y_test);
	A_sc_result = test_buffer_diff_float(60, A_ref, A_test);
	alpha_sc_result = test_buffer_diff_float(1, alpha_ref, alpha_test);
	beta_sc_result = test_buffer_diff_float(1, beta_ref, beta_test);
	x_sc_result = test_buffer_diff_float(15, x_ref, x_test);
	y_sc_result = test_buffer_diff_float(4, y_ref, y_test);
	if ((A_sc_result || (alpha_sc_result || (beta_sc_result || (x_sc_result || y_sc_result)))))
	{
		fprintf(df, "false\n");
	}
	else
	{
		fprintf(df, "true\n");
	}
	free(A);
	free(alpha);
	free(beta);
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
