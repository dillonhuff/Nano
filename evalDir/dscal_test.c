#include "mkl.h"
#include "utils.h"
void time_impl(FILE* df){
	int m;
	double* beta;
	double* x;
	unsigned long long start;
	unsigned long long end;
	unsigned long long total_cycles;
	unsigned long long lvar;
	unsigned long long num_runs;
	double avg_cycles_per_run;
	m = 1200;
	beta = mkl_malloc(sizeof(double), 32);
	x = mkl_malloc((sizeof(double) * 1200), 32);
	rand_doubles(1200, x);
	rand_doubles(1, beta);
	num_runs = 0;
	total_cycles = 0;
	while ((total_cycles <= 100000000))
	{
		start = rdtsc();
		cblas_dscal(m, beta[0], x, 1);
		end = rdtsc();
		total_cycles = (total_cycles + (end - start));
		num_runs = (num_runs + 1);
	}
	start = rdtsc();
	for (lvar = 1; (lvar <= num_runs); lvar = (lvar + 1))
	{
		cblas_dscal(m, beta[0], x, 1);
	}
	end = rdtsc();
	avg_cycles_per_run = ((end - start) / ((double)num_runs));
	fprintf(df, "%f\n", avg_cycles_per_run);
	mkl_free(beta);
	mkl_free(x);
}

int main(){
	FILE* data_file;
	data_file = fopen("/Users/dillon/Haskell/Nano/runData/dscal_test.txt", "w");
	time_impl(data_file);
	fclose(data_file);
	return 0;
}
