#define RRBROADCAST(t0, a, c) {	   \
    t0 = _mm256_unpacklo_pd(a, a); \
    c = _mm256_permute2f128_pd(t0, t0, 0b00100010); \
}

#define ACCUM4(t0, t1, t2, a, b, c) {					\
  t0 = _mm256_hadd_pd(b, _mm256_setzero_pd()); \
  t1 = _mm256_permute4x64_pd(t0, 0b11011000); \
  t2 = _mm256_hadd_pd(t1, _mm256_setzero_pd()); \
  c = _mm256_add_pd(t2, a); \
}
