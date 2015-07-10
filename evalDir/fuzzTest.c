#include <immintrin.h>
#include "utils.h"
void op(double* alpha, double* m1, double* m3){
	double* tr9c9;
	tr9c9 = malloc((sizeof(double) * 81));
	simple_smul(9, 9, (alpha + 0), (m3 + 0), 1, 9, (tr9c9 + 0), 1, 9);
	simple_add(9, 9, (tr9c9 + 0), 1, 9, (m1 + 0), 1, 9, (m1 + 0), 1, 9);
	free(tr9c9);
}

void transformedOp(double* alpha, double* m1, double* m3){
	int i2;
	int i4;
	__m256d k_0;
	__m256d k_1;
	__m256d k_2;
	__m256d r_0;
	__m256d r_1;
	__m256d r_2;
	__m256d sm0;
	__m256d sm1;
	__m256d t_1;
	__m256d t_2;
	for (i2 = 0; (i2 <= 5); i2 = (i2 + 4))
	{
		for (i4 = 0; (i4 <= 8); i4 = (i4 + 1))
		{
			sm0 = _mm256_broadcast_sd((alpha + 0));
			sm0 = sm0;
			r_0 = _mm256_loadu_pd((m3 + ((i4 * 9) + i2)));
			t_1 = _mm256_mul_pd(sm0, r_0);
			t_1 = t_1;
			t_1 = t_1;
			r_1 = _mm256_loadu_pd((m1 + ((i4 * 9) + i2)));
			r_2 = _mm256_add_pd(t_1, r_1);
			_mm256_storeu_pd((m1 + ((i4 * 9) + i2)), r_2);
		}
	}
	for (i4 = 0; (i4 <= 8); i4 = (i4 + 1))
	{
		sm1 = _mm256_broadcast_sd((alpha + 0));
		sm1 = sm1;
		k_0 = _mm256_maskload_pd((m3 + ((i4 * 9) + 8)), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1));
		t_2 = _mm256_mul_pd(sm1, k_0);
		t_2 = t_2;
		t_2 = t_2;
		k_1 = _mm256_maskload_pd((m1 + ((i4 * 9) + 8)), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1));
		k_2 = _mm256_add_pd(t_2, k_1);
		_mm256_maskstore_pd((m1 + ((i4 * 9) + 8)), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1), k_2);
	}
}

void sanity_check(FILE* df){
	double* alpha;
	double* m1;
	double* m3;
	double* alpha_ref;
	double* m1_ref;
	double* m3_ref;
	double* alpha_test;
	double* m1_test;
	double* m3_test;
	int alpha_sc_result;
	int m1_sc_result;
	int m3_sc_result;
	alpha = malloc((sizeof(double) * 1));
	m1 = malloc((sizeof(double) * 81));
	m3 = malloc((sizeof(double) * 81));
	alpha_ref = malloc((sizeof(double) * 1));
	m1_ref = malloc((sizeof(double) * 81));
	m3_ref = malloc((sizeof(double) * 81));
	alpha_test = malloc((sizeof(double) * 1));
	m1_test = malloc((sizeof(double) * 81));
	m3_test = malloc((sizeof(double) * 81));
	rand_doubles(1, alpha);
	rand_doubles(81, m1);
	rand_doubles(81, m3);
	memcpy(alpha_ref, alpha, (sizeof(double) * 1));
	memcpy(m1_ref, m1, (sizeof(double) * 81));
	memcpy(m3_ref, m3, (sizeof(double) * 81));
	memcpy(alpha_test, alpha, (sizeof(double) * 1));
	memcpy(m1_test, m1, (sizeof(double) * 81));
	memcpy(m3_test, m3, (sizeof(double) * 81));
	op(alpha_ref, m1_ref, m3_ref);
	transformedOp(alpha_test, m1_test, m3_test);
	alpha_sc_result = test_buffer_diff(1, alpha_ref, alpha_test);
	m1_sc_result = test_buffer_diff(81, m1_ref, m1_test);
	m3_sc_result = test_buffer_diff(81, m3_ref, m3_test);
	if ((alpha_sc_result || (m1_sc_result || m3_sc_result)))
	{
		fprintf(df, "false\n");
	}
	else
	{
		fprintf(df, "true\n");
	}
	free(alpha);
	free(m1);
	free(m3);
}

int main(){
	FILE* data_file;
	data_file = fopen("/Users/dillon/Haskell/Nano/runData/fuzzTest.txt", "w");
	sanity_check(data_file);
	fclose(data_file);
	return 0;
}
