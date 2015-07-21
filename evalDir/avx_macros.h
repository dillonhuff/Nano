#define RRBROADCAST(a, c) { \
      c = _mm256_permute2f128_pd(_mm256_unpacklo_pd(a, a), _mm256_unpacklo_pd(a, a), 0b00100010); \
}
