#ifdef __ARM_NEON
#include <arm_neon.h>
#define cast(v)			\
({					\
    typeof(v) __in = (v);		\
    int32x4_t __out;			\
    asm("" : "=w"(__out) : "0"(__in));	\
     __out;				\
})
#endif /* __ARM_NEON */
#ifdef __ARM_FEATURE_SVE
#include <arm_sve.h>
#endif /* __ARM_FEATURE_SVE */
#ifdef __ARM_FEATURE_SME
#include <arm_sme.h>
#endif