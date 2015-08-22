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

#define PACK_DBL_4x4(r0, r1, r2, r3, a, s) { \
    r0 = _mm256_loadu_pd(a + 0 * s);	     \
    r1 = _mm256_loadu_pd(a + 1 * s);	     \
    r2 = _mm256_loadu_pd(a + 2 * s);	     \
    r3 = _mm256_loadu_pd(a + 3 * s);	     \
}

#define UNPACK_DBL_4x4(a, s, r0, r1, r2, r3) { \
    _mm256_storeu_pd(a + 0 * s, r0);	       \
    _mm256_storeu_pd(a + 1 * s, r1);	       \
    _mm256_storeu_pd(a + 2 * s, r2);	       \
    _mm256_storeu_pd(a + 3 * s, r3);	       \
  }

#define EADD_DBL_4x4(c0, c1, c2, c3, a0, a1, a2, a3, b0, b1, b2, b3) { \
    c0 = _mm256_add_pd(a0, b0);					       \
    c1 = _mm256_add_pd(a1, b1);					       \
    c2 = _mm256_add_pd(a2, b2);					       \
    c3 = _mm256_add_pd(a3, b3);					       \
  }
