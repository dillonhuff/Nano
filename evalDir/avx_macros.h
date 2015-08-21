#define RRBROADCAST(t0, a, c) {				\
    t0 = _mm256_unpacklo_pd(a, a);			\
    c = _mm256_permute2f128_pd(t0, t0, 0b00100010);	\
  }

#define ACCUM4(t0, t1, t2, a, b, c) {					\
    t0 = _mm256_hadd_pd(b, _mm256_setzero_pd());			\
    t1 = _mm256_permute4x64_pd(t0, 0b11011000);				\
    t2 = _mm256_hadd_pd(t1, _mm256_setzero_pd());			\
    c = _mm256_add_pd(t2, a);						\
  }

#define TRANS(regB0, regB1, regB2, regB3, regA0, regA1, regA2, regA3, r12, r0, r13, r1) { \
    r1 = _mm256_unpacklo_pd( regA0, regA1 );				\
    r13 = _mm256_unpacklo_pd( regA2, regA3 );				\
    r0 = _mm256_unpackhi_pd( regA0, regA1 );				\
    r12 = _mm256_unpackhi_pd( regA2, regA3 );				\
    regB0 = _mm256_permute2f128_pd( r1, r13, 0x20 );			\
    regB1 = _mm256_permute2f128_pd( r0, r12, 0x20 );			\
    regB2 = _mm256_permute2f128_pd( r1, r13, 0x31 );			\
    regB3 = _mm256_permute2f128_pd( r0, r12, 0x31 );			\
  }

#define PACK_DBL_4x4() {}
#define UNPACK_DBL_4x4() {}
#define EADD_DBL_4x4() {}
