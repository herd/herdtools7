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

#ifdef __ARM_FEATURE_MEMORY_TAGGING
#include <arm_acle.h>
typedef unsigned long tag_t;
#define untagged(p)						\
({								\
    unsigned long __in = (unsigned long)(p);			\
    typeof(p) __out = (typeof(p))(__in & ~(0xful << 56));	\
    __out;							\
})
#define tagged(p,t)					\
({							\
    unsigned long __in = (unsigned long)(untagged(p));	\
    unsigned long __tag = (unsigned long)(t) << 56;	\
    typeof(p) __out = (typeof(p))(__in | __tag);	\
    __out;						\
})
#define tag_of(p)				\
({						\
    unsigned long __in = (unsigned long)(p);	\
    tag_t __out = (__in >> 56) & 0xf;		\
    __out;					\
})
#define get_tag(p)					\
({							\
    unsigned long __in = (unsigned long)(p);		\
    typeof(p) __out;					\
    asm volatile("ldg %0, [%1]"				\
		 : "=r"(__out)				\
		 : "r"(__in)				\
		 :"memory");				\
    __out;						\
})
#define set_tag_range(p,s,t)				\
({		      					\
    unsigned long __in = (unsigned long)(untagged(p));	\
    unsigned long __start = round_down(__in, 16);	\
    unsigned long __end = round_up(__in + s , 16);	\
    for (unsigned long ptr = __start;			\
	 ptr < __end;					\
	 ptr += 16)					\
	    asm volatile("stg %0, [%1]"			\
                         :				\
                         : "r"(tagged(ptr,t)), "r"(ptr)	\
			 : "memory");			\
})
#else
#define untagged(p) (p)
#define tagged(p,t) (p)
#define tag_of(p) (0)
#define get_tag(p) (p)
#define set_tag_range(p,s,t) do {} while(0)
#endif /* __ARM_FEATURE_MEMORY_TAGGING */
