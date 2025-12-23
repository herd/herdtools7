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
#define MTE_GRANULE_SIZE        16UL
#define MTE_GRANULE_MASK        (MTE_GRANULE_SIZE - 1)
#define MTE_TAG_SHIFT           56

typedef unsigned long tag_t;
#define untagged(p)								\
({										\
    unsigned long __in = (unsigned long)(p);					\
    typeof(p) __out = (typeof(p))(__in & ~(MTE_GRANULE_MASK << MTE_TAG_SHIFT));	\
    __out;									\
})
#define tagged(p,t)						\
({								\
    unsigned long __in = (unsigned long)(untagged(p));		\
    unsigned long __tag = (unsigned long)(t) << MTE_TAG_SHIFT;	\
    typeof(p) __out = (typeof(p))(__in | __tag);		\
    __out;							\
})
#define tag_of(p)						\
({								\
    unsigned long __in = (unsigned long)(p);			\
    tag_t __out = (__in >> MTE_TAG_SHIFT) & MTE_GRANULE_MASK;	\
    __out;							\
})
#define get_tag(p)					\
({							\
    typeof(p) __in = p;					\
    typeof(p) __out = __in;				\
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
	    asm volatile("stg %0, [%0]"			\
                         :				\
                         : "r"(tagged(ptr,t))		\
			 : "memory");			\
})
#else
#define untagged(p) (p)
#define tagged(p,t) (p)
#define tag_of(p) (0)
#define get_tag(p) (p)
#define set_tag_range(p,s,t) do {} while(0)
#endif /* __ARM_FEATURE_MEMORY_TAGGING */
