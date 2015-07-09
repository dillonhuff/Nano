#include <immintrin.h>
#include "utils.h"
void op(double* A, double* B, double* C, double* alpha, double* beta){
	double* T1;
	T1 = malloc((sizeof(double) * 255));
	set_to_zero_double((T1 + 0), 15, 17, 17, 1);
	simple_mmul(15, 17, 22, (A + 0), 22, 1, (B + 0), 17, 1, (T1 + 0), 17, 1);
	simple_smul(15, 17, (alpha + 0), (T1 + 0), 17, 1, (T1 + 0), 17, 1);
	simple_smul(15, 17, (beta + 0), (C + 0), 17, 1, (C + 0), 17, 1);
	simple_add(15, 17, (T1 + 0), 17, 1, (C + 0), 17, 1, (C + 0), 17, 1);
	free(T1);
}

void transformedOp(double* A, double* B, double* C, double* alpha, double* beta){
	int b_0;
	int b_5z;
	int b_5;
	int b_5zz;
	__m256 k_0;
	__m256 k_1;
	__m256 k_2;
	__m256 k_3;
	__m256 k_4;
	__m256 k_5;
	__m256 r_0;
	__m256 r_1;
	__m256 r_10;
	__m256 r_2;
	__m256 r_3;
	__m256 r_4;
	__m256 r_5;
	__m256 r_6;
	__m256 r_7;
	__m256 r_8;
	__m256 r_9;
	__m256 sm0;
	__m256 sm1;
	__m256 sm2;
	__m256 sm3;
	__m256 sm4;
	__m256 sm5;
	__m256 t_2;
	__m256 t_3;
	__m256 t_4;
	double* t_1;
	t_1 = malloc((sizeof(double) * 255));
	r_0 = _mm256_setzero_pd();
	k_0 = _mm256_setzero_pd();
	sm2 = _mm256_broadcast_sd((alpha + 0));
	sm3 = _mm256_broadcast_sd((beta + 0));
	sm4 = _mm256_broadcast_sd((alpha + 0));
	sm5 = _mm256_broadcast_sd((beta + 0));
	for (b_0 = 0; (b_0 <= 14); b_0 = (b_0 + 1))
	{
		for (b_5z = 0; (b_5z <= 13); b_5z = (b_5z + 4))
		{
			_mm256_storeu_pd((t_1 + (b_5z + (b_0 * 17))), r_0);
		}
		t_2 = k_0;
		for (b_5 = 0; (b_5 <= 21); b_5 = (b_5 + 1))
		{
			sm0 = _mm256_broadcast_sd((A + (b_5 + (b_0 * 22))));
			for (b_5zz = 0; (b_5zz <= 13); b_5zz = (b_5zz + 4))
			{
				r_1 = _mm256_loadu_pd((B + (b_5zz + (b_5 * 17))));
				t_3 = _mm256_mul_pd(sm0, r_1);
				r_2 = _mm256_loadu_pd((t_1 + (b_5zz + (b_0 * 17))));
				r_3 = _mm256_add_pd(t_3, r_2);
				_mm256_storeu_pd((t_1 + (b_5zz + (b_0 * 17))), r_3);
			}
			sm1 = _mm256_broadcast_sd((A + (b_5 + (b_0 * 22))));
			k_1 = _mm256_maskload_pd((B + (16 + (b_5 * 17))), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1));
			t_4 = _mm256_mul_pd(sm1, k_1);
			t_2 = _mm256_add_pd(t_4, t_2);
		}
		for (b_5zz = 0; (b_5zz <= 13); b_5zz = (b_5zz + 4))
		{
			r_4 = _mm256_loadu_pd((t_1 + (b_5zz + (b_0 * 17))));
			r_5 = _mm256_mul_pd(sm2, r_4);
			_mm256_storeu_pd((t_1 + (b_5zz + (b_0 * 17))), r_5);
			r_6 = _mm256_loadu_pd((C + (b_5zz + (b_0 * 17))));
			r_7 = _mm256_mul_pd(sm3, r_6);
			_mm256_storeu_pd((C + (b_5zz + (b_0 * 17))), r_7);
			r_8 = _mm256_loadu_pd((t_1 + (b_5zz + (b_0 * 17))));
			r_9 = _mm256_loadu_pd((C + (b_5zz + (b_0 * 17))));
			r_10 = _mm256_add_pd(r_8, r_9);
			_mm256_storeu_pd((C + (b_5zz + (b_0 * 17))), r_10);
		}
		t_2 = _mm256_mul_pd(sm4, t_2);
		k_2 = _mm256_maskload_pd((C + (16 + (b_0 * 17))), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1));
		k_3 = _mm256_mul_pd(sm5, k_2);
		_mm256_maskstore_pd((C + (16 + (b_0 * 17))), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1), k_3);
		k_4 = _mm256_maskload_pd((C + (16 + (b_0 * 17))), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1));
		k_5 = _mm256_add_pd(t_2, k_4);
		_mm256_maskstore_pd((C + (16 + (b_0 * 17))), _mm256_set_epi32(0, 0, 0, 0, 0, 0, -1, -1), k_5);
	}
	free(t_1);
}

void sanity_check(FILE* df){
	double* A;
	double* B;
	double* C;
	double* alpha;
	double* beta;
	double* A_ref;
	double* B_ref;
	double* C_ref;
	double* alpha_ref;
	double* beta_ref;
	double* A_test;
	double* B_test;
	double* C_test;
	double* alpha_test;
	double* beta_test;
	int A_sc_result;
	int B_sc_result;
	int C_sc_result;
	int alpha_sc_result;
	int beta_sc_result;
	A = malloc((sizeof(double) * 330));
	B = malloc((sizeof(double) * 374));
	C = malloc((sizeof(double) * 255));
	alpha = malloc((sizeof(double) * 1));
	beta = malloc((sizeof(double) * 1));
	A_ref = malloc((sizeof(double) * 330));
	B_ref = malloc((sizeof(double) * 374));
	C_ref = malloc((sizeof(double) * 255));
	alpha_ref = malloc((sizeof(double) * 1));
	beta_ref = malloc((sizeof(double) * 1));
	A_test = malloc((sizeof(double) * 330));
	B_test = malloc((sizeof(double) * 374));
	C_test = malloc((sizeof(double) * 255));
	alpha_test = malloc((sizeof(double) * 1));
	beta_test = malloc((sizeof(double) * 1));
	rand_doubles(330, A);
	rand_doubles(374, B);
	rand_doubles(255, C);
	rand_doubles(1, alpha);
	rand_doubles(1, beta);
	memcpy(A_ref, A, (sizeof(double) * 330));
	memcpy(B_ref, B, (sizeof(double) * 374));
	memcpy(C_ref, C, (sizeof(double) * 255));
	memcpy(alpha_ref, alpha, (sizeof(double) * 1));
	memcpy(beta_ref, beta, (sizeof(double) * 1));
	memcpy(A_test, A, (sizeof(double) * 330));
	memcpy(B_test, B, (sizeof(double) * 374));
	memcpy(C_test, C, (sizeof(double) * 255));
	memcpy(alpha_test, alpha, (sizeof(double) * 1));
	memcpy(beta_test, beta, (sizeof(double) * 1));
	op(A_ref, B_ref, C_ref, alpha_ref, beta_ref);
	transformedOp(A_test, B_test, C_test, alpha_test, beta_test);
	A_sc_result = test_buffer_diff(330, A_ref, A_test);
	B_sc_result = test_buffer_diff(374, B_ref, B_test);
	C_sc_result = test_buffer_diff(255, C_ref, C_test);
	alpha_sc_result = test_buffer_diff(1, alpha_ref, alpha_test);
	beta_sc_result = test_buffer_diff(1, beta_ref, beta_test);
	if ((A_sc_result || (B_sc_result || (C_sc_result || (alpha_sc_result || beta_sc_result)))))
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
	free(alpha);
	free(beta);
}

int main(){
	FILE* data_file;
	data_file = fopen("/Users/dillon/Haskell/Nano/runData/fuzzTest.txt", "w");
	sanity_check(data_file);
	fclose(data_file);
	return 0;
}
