#define RRBROADCAST(a, c) { \
    c = _mm256_permute2f128_pd(_mm256_unpacklo_pd(a, a), _mm256_unpacklo_pd(a, a), 0b00100010); \
}

#define ACCUM4(a, b, c) { \
  c = _mm256_add_pd(_mm256_hadd_pd(_mm256_permute4x64_pd(_mm256_hadd_pd(b, _mm256_setzero_pd()), 0b11011000), _mm256_setzero_pd()), a); \
}
